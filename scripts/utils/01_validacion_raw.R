#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
################################################################################
# 01_validacion_raw.R — Diagnóstico sistemático de ficheros RAW
#
# Qué hace:
#  - Enumera CSV en data/raw y los lee como texto con read_raw()
#  - Detecta separador/encoding, filas/columnas y tamaño
#  - Identifica columna de periodo y rango de años (marca fuera de 2010–2023)
#  - Prueba parseo numérico en columnas candidatas y recoge tokens problemáticos
#  - Genera resumen y vistas de columnas/únicos/preview
#
# Entradas:  data/raw/*.csv
# Salidas:
#   diagnostics/resumen_raw.csv
#   diagnostics/raw_numeric_parse_samples.csv
#   diagnostics/raw_sep_encoding.csv
#   diagnostics/cols_<archivo>.csv
#   diagnostics/preview_<archivo>.csv
#   diagnostics/uniques_<archivo>.csv
#
# Proyecto : Delitos e Inmigración en España (2010–2023)
# Autor    : Jesús Castro · JESUSCASTRODATA
# Licencia : MIT (código) · CC BY 4.0 (docs/figuras)
# Fecha    : 2025-09-22
################################################################################

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(here); library(stringr)
  library(tidyr); library(janitor); library(purrr); library(tools)
})

## ==== Configuración ==========================================================
WRITE_DERIVADOS <- as.logical(Sys.getenv("WRITE_DERIVADOS","TRUE"))
STRICT          <- as.logical(Sys.getenv("STRICT","TRUE"))          # TRUE: aborta si faltan CSV
MAX_PREVIEW     <- as.integer(Sys.getenv("MAX_PREVIEW","20"))       # filas preview
MAX_UNIQUES     <- as.integer(Sys.getenv("MAX_UNIQUES","50"))       # muestra de únicos
VERBOSE         <- as.logical(Sys.getenv("VERBOSE","TRUE"))

## ==== Utilidades del proyecto ===============================================
source(here::here("scripts","00_utils_limpieza.R"))
PROJECT_ROOT_HINT <- Sys.getenv("PROJECT_ROOT_HINT", "")
project_hint <- if (nzchar(PROJECT_ROOT_HINT)) PROJECT_ROOT_HINT else NULL
root_init("scripts/01_validacion_raw.R", project_root_hint = project_hint,
          anchor_rel = "scripts/00_paths_config.R")

in_dir  <- here::here("data","raw")
out_dir <- here::here("diagnostics")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(in_dir)) abort("No existe el directorio de RAW: ", in_dir)

## ==== Helpers locales ========================================================
parse_total_es <- function(x) safe_parse_number(x)
human_mb <- function(bytes) round(as.numeric(bytes)/(1024^2), 3)

is_categorical_like <- function(x, n_rows) {
  n_u <- dplyr::n_distinct(x, na.rm = TRUE)
  all_chr <- is.character(x) || is.factor(x)
  (n_u <= min(200, max(10, round(0.1 * n_rows)))) || (all_chr && n_u <= 1000)
}

key_col_pattern <- function(nm) {
  grepl("comun|ccaa|autono|territ|ambito|ámbito|region|prov|municip|sexo|edad|nacional|tipolog|delit|infracc|tipo",
        nm, ignore.case = TRUE)
}

num_candidates_from_names <- function(nms) {
  unique(grep("^(total|valor|numero|n)$|hechos|conocidos|esclarecidos|detencion|detenciones|poblaci|paro|tasa|cantidad|importe|num$",
              nms, ignore.case = TRUE, value = TRUE))
}

preview_write <- function(df, path, n = 15) {
  if (!WRITE_DERIVADOS) { if (VERBOSE) message("[01] DRY-RUN: ", path); return(invisible()) }
  utils::write.table(utils::head(df, n), file = path, sep = ",",
                     row.names = FALSE, col.names = TRUE, na = "")
}

sample_values <- function(x, k = 50) {
  vals <- unique(na.omit(as.character(x)))
  paste(utils::head(vals, k), collapse = "; ")
}

log_line <- function(..., .prefix="[01] ") if (VERBOSE) message(.prefix, paste0(...))

cli_progress <- function(step, file=NULL) {
  if (requireNamespace("cli", quietly = TRUE)) {
    if (is.null(file)) cli::cli_alert_info(step) else cli::cli_alert_info(paste(step, file))
  } else {
    log_line(step, if (!is.null(file)) paste0(" ", file) else "")
  }
}

## ==== Listado de ficheros ====================================================
files <- list.files(in_dir, pattern = "\\.csv$", full.names = TRUE)
if (!length(files)) {
  msg <- paste0("No se encontraron CSV en: ", in_dir)
  if (STRICT) abort(msg) else { warning(msg); }
}

resumen    <- list()
parse_iss  <- list()
file_logs  <- list()

## ==== Bucle principal ========================================================
for (f in files) {
  cli_progress("Procesando", basename(f))
  t0 <- proc.time()
  warn_vec <- character()
  
  withCallingHandlers({
    df <- try(read_raw(f), silent = TRUE)
    if (inherits(df, "try-error")) {
      warn_vec <- c(warn_vec, paste("ERROR al leer:", basename(f)))
      next
    }
    
    sep_detect <- attr(df, "raw_sep")
    enc_detect <- attr(df, "raw_encoding")
    nms <- names(df)
    
    # Esquema de columnas
    if (WRITE_DERIVADOS) {
      write_clean(tibble(columna = nms),
                  file.path(out_dir, paste0("cols_", tools::file_path_sans_ext(basename(f)), ".csv")))
    } else log_line("DRY-RUN: cols_* para ", basename(f))
    
    # Preview
    preview_write(df, file.path(out_dir, paste0("preview_", tools::file_path_sans_ext(basename(f)), ".csv")),
                  n = MAX_PREVIEW)
    
    # Año: detección y estadísticas
    col_year <- detect_year_col(nms)
    y_min <- y_max <- n_year <- anos_fuera <- NA_integer_
    if (!is.na(col_year)) {
      yy <- extract_year(df[[col_year]])
      if (!all(is.na(yy))) {
        y_min <- suppressWarnings(min(yy, na.rm = TRUE))
        y_max <- suppressWarnings(max(yy, na.rm = TRUE))
        n_year <- sum(!is.na(yy))
        anos_fuera <- sum(!is.na(yy) & (yy < 2010 | yy > 2023))
      } else {
        warn_vec <- c(warn_vec, paste0("Columna de año '", col_year, "' sin años parseables"))
      }
    } else {
      warn_vec <- c(warn_vec, "No se detectó columna de periodo/año")
    }
    
    # Parseo numérico en candidatas
    cand_totals <- num_candidates_from_names(nms)
    if (length(cand_totals) == 0L) cand_totals <- intersect(nms, c("total","valor","numero","n"))
    
    for (ct in cand_totals) {
      tot_raw <- df[[ct]]
      if (is.numeric(tot_raw)) next
      parsed <- parse_total_es(tot_raw)
      non_empty <- !is.na(tot_raw) & nzchar(as.character(tot_raw))
      na_new <- sum(is.na(parsed) & non_empty)
      if (na_new > 0) {
        bad_tokens <- unique(na.omit(as.character(tot_raw[is.na(parsed) & non_empty])))
        parse_iss[[length(parse_iss) + 1]] <- tibble(
          archivo = basename(f),
          columna = ct,
          n_na_nuevos = na_new,
          muestra = list(utils::head(bad_tokens, 10))
        )
      }
    }
    
    # Uniques por columna "categórica" o clave
    uniques_df <- tibble(columna = character(), n_unicos = integer(), muestra = character())
    for (nm in nms) {
      vec <- df[[nm]]
      if (key_col_pattern(nm) || is_categorical_like(vec, nrow(df))) {
        uniques_df <- bind_rows(
          uniques_df,
          tibble(columna = nm,
                 n_unicos = dplyr::n_distinct(vec, na.rm = TRUE),
                 muestra = sample_values(vec, k = MAX_UNIQUES))
        )
      }
    }
    if (nrow(uniques_df)) {
      if (WRITE_DERIVADOS) {
        write_clean(uniques_df, file.path(out_dir, paste0("uniques_", tools::file_path_sans_ext(basename(f)), ".csv")))
      } else log_line("DRY-RUN: uniques_* para ", basename(f))
    }
    
    # Resumen por archivo
    f_size <- tryCatch(file.size(f), error = function(e) NA_real_)
    resumen[[length(resumen) + 1]] <- tibble(
      archivo = basename(f),
      filas = nrow(df),
      cols = ncol(df),
      bytes = f_size,
      mb = human_mb(f_size),
      sep = sep_detect,
      encoding = enc_detect,
      col_periodo = ifelse(is.na(col_year), NA_character_, col_year),
      ano_min = y_min, ano_max = y_max,
      n_valores_ano = n_year,
      anos_fuera_2010_2023 = anos_fuera
    )
    
  }, warning = function(w) {
    warn_vec <<- c(warn_vec, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  
  elap <- (proc.time() - t0)[["elapsed"]]
  file_logs[[length(file_logs) + 1]] <- tibble(
    archivo = basename(f),
    segundos = round(elap, 3),
    n_warnings = length(warn_vec),
    warnings = paste(unique(warn_vec), collapse = " || ")
  )
}

## ==== Salidas globales ======================================================
resumen_df <- dplyr::bind_rows(resumen) %>% dplyr::arrange(archivo)
if (WRITE_DERIVADOS) {
  write_clean(resumen_df, file.path(out_dir, "resumen_raw.csv"))
  write_clean(resumen_df %>% dplyr::select(archivo, sep, encoding),
              file.path(out_dir, "raw_sep_encoding.csv"))
} else {
  log_line("DRY-RUN: resumen_raw.csv, raw_sep_encoding.csv")
}

if (length(parse_iss)) {
  parse_iss_df <- dplyr::bind_rows(parse_iss) %>%
    tidyr::unnest_longer(muestra, values_to = "token_muestra")
  if (WRITE_DERIVADOS) {
    write_clean(parse_iss_df, file.path(out_dir, "raw_numeric_parse_samples.csv"))
  } else {
    log_line("DRY-RUN: raw_numeric_parse_samples.csv")
  }
} else {
  log_line("Sin incidencias de parse numérico registradas.")
}

logs_df <- dplyr::bind_rows(file_logs)
if (WRITE_DERIVADOS) write_clean(logs_df, file.path(out_dir, "raw_file_logs.csv"))

## ==== Acciones prioritarias (auto) ==========================================
acciones <- list()

if (nrow(resumen_df)) {
  acciones[[length(acciones)+1]] <- resumen_df %>%
    filter(!is.na(anos_fuera_2010_2023) & anos_fuera_2010_2023 > 0) %>%
    transmute(archivo, issue = "años_fuera_2010_2023",
              detalle = paste0(ano_min, "–", ano_max, " (", anos_fuera_2010_2023, " fuera)"))
  
  acciones[[length(acciones)+1]] <- resumen_df %>%
    filter(is.na(encoding) | encoding != "UTF-8" | !(sep %in% c(";", ","))) %>%
    transmute(archivo, issue = "encoding/sep",
              detalle = paste0("sep=", sep, ", enc=", encoding))
}

if (exists("parse_iss_df") && nrow(parse_iss_df)) {
  acciones[[length(acciones)+1]] <- parse_iss_df %>%
    group_by(archivo) %>%
    summarise(issue = "parse_numeric",
              detalle = paste0("cols=", n_distinct(columna),
                               ", ejemplos=", paste(unique(token_muestra)[1:min(3, n())], collapse=" | ")),
              .groups = "drop")
}

acciones_df <- if (length(acciones)) bind_rows(acciones) %>% arrange(archivo, issue) else tibble()
if (nrow(acciones_df) && WRITE_DERIVADOS) {
  write_clean(acciones_df, file.path(out_dir, "acciones_prioritarias.csv"))
}

## ==== Mensaje final =========================================================
if (STRICT && length(files) == 0) abort("No hay CSV en data/raw y STRICT=TRUE.")
if (STRICT && nrow(resumen_df) == 0) abort("No se pudo generar 'resumen_df'. Revisa lectura.")
message("✅ Diagnóstico RAW completado. Revisa 'diagnostics/'",
        if (!WRITE_DERIVADOS) " (DRY-RUN)" else "")
