#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
###############################################################################
# 00_utils_limpieza.R
# Utilidades de E/S, parseo y QC para el proyecto
# - Lectura robusta de RAW con detección de separador/encoding (forzando texto)
# - Escritura consistente de CSVs
# - Parseo numérico ES/EN
# - Agregaciones seguras, joins con logging, validaciones y helpers
#
# Autor: Jesús Castro (Analista de Datos)
# Última actualización: 2025-08-22
###############################################################################

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(janitor)
  library(tidyr)
  library(here)
})

# ────────────────────────────────────────────────────────────────────────────
# 0) Helpers genéricos
# ────────────────────────────────────────────────────────────────────────────

abort <- function(...) stop(paste0(...), call. = FALSE)

root_init <- function(caller = NULL, project_root_hint = NULL) {
  # Intenta fijar el WD al root del proyecto si el usuario da una pista.
  if (!is.null(project_root_hint) && dir.exists(project_root_hint)) {
    setwd(project_root_hint)
  }
  msg <- if (is.null(caller)) "ROOT inicializado" else paste("ROOT:", caller)
  message(msg, " -> ", here::here())
  invisible(here::here())
}

assert_infile <- function(path) {
  if (!file.exists(path)) abort("No existe: ", path)
}

first_existing <- function(...) {
  cand <- c(...)
  hit <- cand[file.exists(cand)]
  if (!length(hit)) abort("Ninguno de los ficheros existe:\n", paste(cand, collapse = "\n"))
  hit[[1]]
}

# Escritura consistente de CSV (UTF-8, punto decimal, sin row.names)
write_clean <- function(df, path, na = "") {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  readr::write_csv(df, path, na = na)
  message("Escrito: ", normalizePath(path, winslash = "/"))
  invisible(path)
}

# ────────────────────────────────────────────────────────────────────────────
# 1) Parseo numérico robusto (ES/EN)
# ────────────────────────────────────────────────────────────────────────────

# Devuelve numérico intentando primero convención española, luego inglesa.
safe_parse_number <- function(x) {
  x_chr <- as.character(x)
  # Evita parsear strings vacíos como 0
  x_chr[!nzchar(x_chr)] <- NA_character_
  
  # 1) Español: miles '.' y decimal ','
  v1 <- suppressWarnings(
    readr::parse_number(
      x_chr,
      locale = readr::locale(grouping_mark = ".", decimal_mark = ",")
    )
  )
  if (sum(!is.na(v1)) >= sum(!is.na(x_chr)) * 0.9) return(v1)
  
  # 2) Inglés: miles ',' y decimal '.'
  v2 <- suppressWarnings(
    readr::parse_number(
      x_chr,
      locale = readr::locale(grouping_mark = ",", decimal_mark = ".")
    )
  )
  if (sum(!is.na(v2)) >= sum(!is.na(x_chr)) * 0.9) return(v2)
  
  # 3) Fallback mixto (quita separadores no numéricos salvo signo y coma/punto)
  x_clean <- gsub("(\\s|\\u00A0)", "", x_chr)                   # quita espacios y NBSP
  x_clean <- gsub("(?<=\\d)[,](?=\\d{3}(\\D|$))", "", x_clean, perl = TRUE) # quita miles ','
  x_clean <- gsub("(?<=\\d)[.](?=\\d{3}(\\D|$))", "", x_clean, perl = TRUE) # quita miles '.'
  # Si hay coma y punto, normaliza a punto decimal según último separador
  dual <- grepl(",", x_clean) & grepl("\\.", x_clean)
  x_clean[dual] <- sub(",", "", x_clean[dual]) # asume coma como miles cuando conviven
  x_clean <- gsub(",", ".", x_clean, fixed = TRUE)
  suppressWarnings(as.numeric(x_clean))
}

# ────────────────────────────────────────────────────────────────────────────
# 2) Lectura robusta de CSV RAW (forzando texto, sin parseos prematuros)
# ────────────────────────────────────────────────────────────────────────────

# Intenta combinaciones de separador/encoding y escoge la "mejor"
read_raw <- function(path,
                     delim_try = c(";", ",", "\t"),
                     enc_try   = c("UTF-8", "UTF-8-BOM", "Latin1")) {
  assert_infile(path)
  results <- list()
  scores  <- numeric()
  
  for (d in delim_try) {
    for (e in enc_try) {
      df <- try(
        readr::read_delim(
          file = path,
          delim = d,
          col_types = readr::cols(.default = readr::col_character()),
          na = c("", "NA", "NaN", "null", "Null"),
          locale = readr::locale(encoding = e),
          guess_max = 100000
        ),
        silent = TRUE
      )
      if (inherits(df, "try-error")) next
      # Puntuación: más columnas (>1) y menos NA en cabecera ⇒ mejor
      sc <- ncol(df) + 0.001 * nrow(df)
      if (ncol(df) <= 1) sc <- sc * 0.1
      results[[length(results) + 1]] <- list(df = df, delim = d, enc = e, score = sc)
      scores[length(scores) + 1] <- sc
    }
  }
  
  if (!length(results)) abort("No se pudo leer el fichero: ", path)
  
  best <- results[[which.max(scores)]]
  df   <- janitor::clean_names(best$df)
  
  # Atributos para diagnóstico posterior (los usa 01_validacion_raw.R)
  attr(df, "raw_sep")      <- best$delim
  attr(df, "raw_encoding") <- best$enc
  
  message(sprintf("Leído: %s (sep='%s', enc='%s', %d filas, %d cols)",
                  basename(path), best$delim, best$enc, nrow(df), ncol(df)))
  df
}

# ────────────────────────────────────────────────────────────────────────────
# 3) Agregaciones seguras
# ────────────────────────────────────────────────────────────────────────────

safe_max <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) == 0 || all(is.na(x))) return(NA_real_)
  max(x, na.rm = TRUE)
}

safe_sum <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) == 0 || all(is.na(x))) return(NA_real_)
  sum(x, na.rm = TRUE)
}

# ────────────────────────────────────────────────────────────────────────────
# 4) Joins con logging (para detectar claves/años perdidos)
# ────────────────────────────────────────────────────────────────────────────

join_with_log <- function(x, y, by = "ano", mode = c("left", "inner")) {
  mode <- match.arg(mode)
  miss_in_y <- dplyr::anti_join(x, y, by = by)
  miss_in_x <- dplyr::anti_join(y, x, by = by)
  
  if (nrow(miss_in_y))
    message("[JOIN] claves de X sin match en Y por ", by, ": ",
            paste(utils::head(miss_in_y[[by]], 5), collapse = ", "),
            if (nrow(miss_in_y) > 5) " ...")
  
  if (nrow(miss_in_x))
    message("[JOIN] claves de Y sin match en X por ", by, ": ",
            paste(utils::head(miss_in_x[[by]], 5), collapse = ", "),
            if (nrow(miss_in_x) > 5) " ...")
  
  if (mode == "left") dplyr::left_join(x, y, by = by) else dplyr::inner_join(x, y, by = by)
}

# ────────────────────────────────────────────────────────────────────────────
# 5) Detección/Extracción de año
# ────────────────────────────────────────────────────────────────────────────

detect_year_col <- function(nms) {
  cand <- c("periodo", "período", "ano", "año", "year", "time", "time_period", "TIME_PERIOD")
  ix <- which(tolower(nms) %in% tolower(cand))
  if (length(ix)) nms[ix[1]] else NA_character_
}

extract_year <- function(x) {
  suppressWarnings(as.integer(stringr::str_extract(as.character(x), "\\d{4}")))
}

# ────────────────────────────────────────────────────────────────────────────
# 6) Cargador estándar de series (ano + 1 columna objetivo con coalesce)
# ────────────────────────────────────────────────────────────────────────────

coalesce_any <- function(df, candidates) {
  cols <- intersect(candidates, names(df))
  if (!length(cols)) abort("Ninguna columna candidata existe: ", paste(candidates, collapse = ", "))
  dplyr::coalesce(!!!df[cols])
}

# Uso típico: load_series(fp, "det_tot", c("det_tot","valor","total","numero"))
load_series <- function(fp, out_name, candidates, year_col_candidates = NULL) {
  df <- readr::read_csv(fp, show_col_types = FALSE) |> janitor::clean_names()
  if (is.null(year_col_candidates)) {
    year_col_candidates <- c("ano","año","periodo","período","year","time","time_period")
  }
  col_year <- detect_year_col(names(df))
  if (is.na(col_year)) {
    # intenta algunos alias adicionales
    col_year <- intersect(year_col_candidates, names(df))[1]
  }
  if (is.na(col_year)) abort("Falta columna de año en: ", basename(fp))
  
  y <- extract_year(df[[col_year]])
  tibble::tibble(
    ano = y,
    !!out_name := coalesce_any(df, candidates)
  )
}
