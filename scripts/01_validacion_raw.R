#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
###############################################################################
# 01_validacion_raw.R — Diagnóstico sistemático de ficheros RAW
#
# Qué hace (ajustado):
#  - Enumera todos los CSV en data/raw
#  - Lee cada archivo con read_raw() (forzando texto)
#  - Registra: separador/encoding detectados, filas/cols, tamaño bytes/MB
#  - Detecta columna de periodo, rango de años y años fuera 2010–2023
#  - Prueba parseo numérico en múltiples columnas candidatas (total/valor/número/
#    n/hechos/conocidos/esclarecidos/detenciones/poblacion/paro, etc.) y da
#    ejemplos de tokens problemáticos
#  - Exporta:
#      diagnostics/resumen_raw.csv
#      diagnostics/raw_numeric_parse_samples.csv (si aplica)
#      diagnostics/raw_sep_encoding.csv
#      diagnostics/cols_<archivo>.csv (esquema de columnas)
#      diagnostics/preview_<archivo>.csv (primeras filas)
#      diagnostics/uniques_<archivo>.csv (nº de únicos + muestra por columna
#        "categórica" o por columnas clave: region/tipo/sexo/edad/...)
#
# Autor: Jesús Castro (Analista de Datos)
# Última actualización: 2025-08-23
###############################################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(here)
  library(stringr)
  library(tidyr)
  library(janitor)
  library(purrr)
  library(tools)
})

# —— Cargar utils y fijar root ————————————————————————————————
source(here::here("scripts", "00_utils_limpieza.R"))
PROJECT_ROOT_HINT <- "D:/delitos_inmigracion_espana"  # cambia si procede
root_init("scripts/01_validacion_raw.R", project_root_hint = PROJECT_ROOT_HINT)

in_dir  <- here("data", "raw")
out_dir <- here("diagnostics")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(in_dir)) abort("No existe el directorio de RAW: ", in_dir)

# —— Helpers locales ————————————————————————————————————————————————
parse_total_es <- function(x) safe_parse_number(x)

human_mb <- function(bytes) round(as.numeric(bytes) / (1024^2), 3)

is_categorical_like <- function(x, n_rows) {
  # Heurística: pocos únicos o claramente de texto corto
  n_u <- dplyr::n_distinct(x, na.rm = TRUE)
  all_chr <- is.character(x) || is.factor(x)
  n_u <= min(200, max(10, round(0.1 * n_rows))) || (all_chr && n_u <= 1000)
}

key_col_pattern <- function(nm) {
  grepl("comun|ccaa|autono|territ|ambito|ámbito|region|prov|municip|sexo|edad|nacional|tipolog|delit|infracc|tipo", nm, ignore.case = TRUE)
}

num_candidates_from_names <- function(nms) {
  # Priorizamos columnas típicas de cuantitativos
  cand <- grep(
    pattern = "^(total|valor|numero|n)$|hechos|conocidos|esclarecidos|detencion|detenciones|poblaci|paro|tasa|cantidad|importe|numero|num$",
    x = nms, ignore.case = TRUE, value = TRUE
  )
  unique(cand)
}

preview_write <- function(df, path, n = 10) {
  utils::write.table(head(df, n), file = path, sep = ",", row.names = FALSE, col.names = TRUE, na = "")
}

sample_values <- function(x, k = 50) {
  vals <- unique(na.omit(as.character(x)))
  vals <- utils::head(vals, k)
  paste(vals, collapse = "; ")
}

# —— Listado de ficheros ————————————————————————————————————————————
files <- list.files(in_dir, pattern = "\\.csv$", full.names = TRUE)
if (!length(files)) abort("No se encontraron CSV en: ", in_dir)

resumen    <- list()
parse_iss  <- list()

for (f in files) {
  cat("\n────────────────────────────────────────────────────────\n")
  cat("Archivo: ", basename(f), "\n", sep = "")
  
  # Lee como texto, detectando sep/encoding (attrs)
  df <- try(read_raw(f), silent = TRUE)
  if (inherits(df, "try-error")) {
    warning("⚠️  ERROR al leer: ", f)
    next
  }
  
  sep_detect <- attr(df, "raw_sep")
  enc_detect <- attr(df, "raw_encoding")
  nms <- names(df)
  
  # Guarda esquema de columnas
  write_clean(tibble(columna = nms),
              file.path(out_dir, paste0("cols_", tools::file_path_sans_ext(basename(f)), ".csv")))
  
  # Previews (primeras filas)
  preview_write(df, file.path(out_dir, paste0("preview_", tools::file_path_sans_ext(basename(f)), ".csv")), n = 15)
  
  # Año: detección y estadísticas
  col_year <- detect_year_col(nms)
  y_min <- y_max <- n_year <- anos_fuera <- NA_integer_
  if (!is.na(col_year)) {
    yy <- extract_year(df[[col_year]])
    y_min <- suppressWarnings(min(yy, na.rm = TRUE))
    y_max <- suppressWarnings(max(yy, na.rm = TRUE))
    n_year <- sum(!is.na(yy))
    anos_fuera <- sum(!is.na(yy) & (yy < 2010 | yy > 2023))
  }
  
  # Intento de parseo en múltiples columnas candidatas
  cand_totals <- num_candidates_from_names(nms)
  if (length(cand_totals) == 0L) cand_totals <- intersect(nms, c("total","valor","numero","n"))
  
  for (ct in cand_totals) {
    tot_raw <- df[[ct]]
    if (is.numeric(tot_raw)) next
    parsed <- parse_total_es(tot_raw)
    # Contamos nuevos NAs (eran no-vacíos en el bruto y pasan a NA tras parseo)
    non_empty <- !is.na(tot_raw) & nzchar(as.character(tot_raw))
    na_new <- sum(is.na(parsed) & non_empty)
    if (na_new > 0) {
      bad_tokens <- unique(na.omit(as.character(tot_raw[is.na(parsed) & non_empty])))
      parse_iss[[length(parse_iss) + 1]] <- tibble(
        archivo = basename(f),
        columna = ct,
        n_na_nuevos = na_new,
        muestra = utils::head(bad_tokens, 10)
      )
    }
  }
  
  # Uniques por columna "categórica" o clave
  uniques_df <- tibble(
    columna = character(), n_unicos = integer(), muestra = character()
  )
  for (nm in nms) {
    vec <- df[[nm]]
    if (key_col_pattern(nm) || is_categorical_like(vec, nrow(df))) {
      uniques_df <- bind_rows(
        uniques_df,
        tibble(
          columna = nm,
          n_unicos = dplyr::n_distinct(vec, na.rm = TRUE),
          muestra = sample_values(vec, k = 50)
        )
      )
    }
  }
  if (nrow(uniques_df)) {
    write_clean(uniques_df, file.path(out_dir, paste0("uniques_", tools::file_path_sans_ext(basename(f)), ".csv")))
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
    ano_min = y_min,
    ano_max = y_max,
    n_valores_ano = n_year,
    anos_fuera_2010_2023 = anos_fuera
  )
}

# —— Salidas ————————————————————————————————————————————————
resumen_df <- dplyr::bind_rows(resumen) |> dplyr::arrange(archivo)
write_clean(resumen_df, file.path(out_dir, "resumen_raw.csv"))

if (length(parse_iss)) {
  parse_iss_df <- dplyr::bind_rows(parse_iss) |>
    tidyr::unnest_longer(muestra, values_to = "token_muestra")
  write_clean(parse_iss_df, file.path(out_dir, "raw_numeric_parse_samples.csv"))
} else {
  message("Sin incidencias de parse numérico registradas.")
}

write_clean(resumen_df |> dplyr::select(archivo, sep, encoding),
            file.path(out_dir, "raw_sep_encoding.csv"))

message("✅ Diagnóstico RAW completado. Revisa la carpeta 'diagnostics/'.")
