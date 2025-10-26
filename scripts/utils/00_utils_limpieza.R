#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
###############################################################################
# 00_utils_limpieza.R
# Utilidades de E/S, parseo y QC para el proyecto
# - Lectura robusta de RAW con detección de separador/encoding (forzando texto)
# - Escritura consistente de CSVs
# - Parseo numérico ES/EN (con placeholders tratados como NA)
# - Agregaciones seguras, joins con logging, validaciones y helpers
# - LOGGING: registra lecturas y escrituras en diagnostics/log_io.csv
#
# Proyecto: Delitos e Inmigración en España (2010–2023)
# Autor   : Jesús Castro · JESUSCASTRODATA
# Licencia: MIT (código) · CC BY 4.0 (docs/figuras)
# Última actualización: 2025-09-22
###############################################################################

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(janitor)
  library(tidyr)
  library(here)
  library(fs)     # <-- necesario para fs::path_rel()
})

# ────────────────────────────────────────────────────────────────────────────
# 0) Helpers genéricos
# ────────────────────────────────────────────────────────────────────────────

abort <- function(...) stop(paste0(...), call. = FALSE)

# Inicializa raíz del proyecto. Si conoces la ruta relativa de un archivo ancla
# (p. ej., "scripts/00_paths_config.R"), pásala en anchor_rel para i_am().
root_init <- function(caller = NULL, project_root_hint = NULL, anchor_rel = NULL) {
  if (!is.null(project_root_hint) && dir.exists(project_root_hint)) {
    setwd(project_root_hint)
  }
  if (!is.null(anchor_rel)) {
    # Evita errores si ya está anclado
    try(here::i_am(anchor_rel), silent = TRUE)
  }
  msg <- if (is.null(caller)) "ROOT inicializado" else paste("ROOT:", caller)
  message(msg, " -> ", here::here())
  invisible(here::here())
}

assert_infile <- function(path) {
  if (!file.exists(path)) abort("No existe: ", path)
  invisible(path)
}

first_existing <- function(...) {
  cand <- c(...)
  hit <- cand[file.exists(cand)]
  if (!length(hit)) abort("Ninguno de los ficheros existe:\n", paste(cand, collapse = "\n"))
  hit[[1]]
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# ────────────────────────────────────────────────────────────────────────────
# LOGGING (lecturas/escrituras)
# ────────────────────────────────────────────────────────────────────────────

# Carpeta de logs
.log_dir <- tryCatch(here::here("diagnostics"), error = function(...) "diagnostics")
dir.create(.log_dir, showWarnings = FALSE, recursive = TRUE)
.log_path <- file.path(.log_dir, "log_io.csv")

# Escribe una fila en CSV, creando cabecera si no existe
.log_append_row <- function(df_row, path) {
  utils::write.table(
    df_row, file = path, sep = ",",
    row.names = FALSE,
    col.names = !file.exists(path),
    append = TRUE,
    qmethod = "double"
  )
}

# Detecta mejor nombre de script/caller para el log
.log_guess_script <- function() {
  scr <- Sys.getenv("R_SCRIPT", unset = NA_character_)
  if (!is.na(scr) && nzchar(scr)) return(scr)
  cs <- tryCatch(sys.calls(), error = function(...) NULL)
  if (!is.null(cs) && length(cs)) {
    txt <- paste(cs, collapse = " || ")
    hit <- stringr::str_extract(txt, "scripts[/\\\\][^\\)]+?\\.R")
    if (!is.na(hit)) return(basename(gsub("\\\\", "/", hit)))
  }
  NA_character_
}

# Logger principal
.log_writer <- function(kind = c("read","write"), path, extra = list()) {
  kind <- match.arg(kind)
  script_guess <- .log_guess_script()
  caller <- tryCatch(as.character(sys.call(-1))[1], error = function(...) NA_character_)
  df <- tibble::tibble(
    ts      = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    kind    = kind,
    script  = script_guess,
    caller  = caller,
    path    = normalizePath(path, winslash = "/", mustWork = FALSE),
    rows    = extra$rows %||% NA_integer_,
    cols    = extra$cols %||% NA_integer_,
    meta    = extra$meta %||% NA_character_
  )
  .log_append_row(df, .log_path)
  invisible(df)
}

# ────────────────────────────────────────────────────────────────────────────
# 1) Normalización de texto
# ────────────────────────────────────────────────────────────────────────────

normalize_text <- function(x) {
  x <- stringr::str_trim(tolower(as.character(x)))
  y <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x[!is.na(y)] <- y[!is.na(y)]
  stringr::str_squish(x)
}

norm_ascii <- function(x){
  x |>
    tolower() |>
    iconv(from = "", to = "ASCII//TRANSLIT") |>
    stringr::str_squish()
}

# ────────────────────────────────────────────────────────────────────────────
# 2) Parseo numérico robusto (ES/EN) + placeholders seguros
# ────────────────────────────────────────────────────────────────────────────

# Ampliado para cubrir tokens habituales del QA
.NUM_MISS <- c(
  "", "na", "n/a", "sd", "s/d", "nd", "n.d.", "n.d", "null",
  "..", ".", "—", "–", "-", ":", "…", "sin dato", "sin_dato"
)

num_like_cols <- function(nms) {
  grep("total|valor|numero|^n$|^n_|_n$|tasa|porcen", nms,
       ignore.case = TRUE, perl = TRUE, value = TRUE)
}

# ⇩⇩⇩  PARCHEADO
limpia_num <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x[x %in% .NUM_MISS] <- NA
  
  # Normaliza caracteres problemáticos
  x <- chartr("\u2212", "-", x)                    # minus unicode → '-'
  x <- gsub("\u00A0", " ", x, fixed = TRUE)        # NBSP → espacio normal
  
  # Quita símbolos comunes
  x <- gsub("%", "", x)
  
  # Arreglo miles/decimales ES→EN
  x <- gsub("\\.", "", x)                          # quita miles con punto
  x <- gsub(",", ".", x)                           # coma decimal → punto
  
  # Mantén solo dígitos, signo y punto
  x <- gsub("[^0-9\\-\\.]", "", x)
  
  # Quita separadores colgantes que rompen el parseo
  x <- gsub("(?<=\\d)[\\.,]+$", "", x, perl = TRUE)  # "123." → "123"
  x <- gsub("^[\\.,]+(?=\\d)", "", x, perl = TRUE)   # ".123" → "123"
  
  suppressWarnings(as.numeric(x))
}

# ⇩⇩⇩  PARCHEADO — añade normalización extra en fallback mixto
safe_parse_number <- function(x) {
  x_chr <- tolower(trimws(as.character(x)))
  x_chr[x_chr %in% .NUM_MISS] <- NA_character_
  x_chr <- gsub("%", "", x_chr)
  
  # Español
  v1 <- suppressWarnings(
    readr::parse_number(x_chr, locale = readr::locale(grouping_mark = ".", decimal_mark = ","))
  )
  if (sum(!is.na(v1)) >= sum(!is.na(x_chr)) * 0.9) return(v1)
  
  # Inglés
  v2 <- suppressWarnings(
    readr::parse_number(x_chr, locale = readr::locale(grouping_mark = ",", decimal_mark = "."))
  )
  if (sum(!is.na(v2)) >= sum(!is.na(x_chr)) * 0.9) return(v2)
  
  # Fallback mixto
  x_clean <- gsub("(\\s|\\u00A0)", "", x_chr)  # quita espacios y NBSP
  x_clean <- gsub("(?<=\\d)[,](?=\\d{3}(\\D|$))", "", x_clean, perl = TRUE) # quita miles ','
  x_clean <- gsub("(?<=\\d)[.](?=\\d{3}(\\D|$))", "", x_clean, perl = TRUE) # quita miles '.'
  dual <- grepl(",", x_clean) & grepl("\\.", x_clean)
  x_clean[dual] <- sub(",", "", x_clean[dual]) # asume coma miles si conviven
  x_clean <- gsub(",", ".", x_clean, fixed = TRUE)
  
  # Normaliza signo unicode y separadores colgantes
  x_clean <- chartr("\u2212", "-", x_clean)
  x_clean <- gsub("(?<=\\d)[\\.,]+$", "", x_clean, perl = TRUE)
  x_clean <- gsub("^[\\.,]+(?=\\d)", "", x_clean, perl = TRUE)
  
  suppressWarnings(as.numeric(x_clean))
}

# ⇩⇩⇩  endurece: solo tokens con al menos 2 dígitos
find_numeric_parse_issues <- function(df, archivo = NA_character_) {
  cols <- num_like_cols(names(df))
  if (!length(cols)) return(tibble::tibble())
  purrr::map_dfr(cols, function(col) {
    v <- tolower(trimws(as.character(df[[col]])))
    parsed <- limpia_num(v)
    has_digits <- grepl("\\d.*\\d", v)                 # al menos 2 dígitos
    raro <- is.na(parsed) & !(v %in% .NUM_MISS) & nzchar(v) & has_digits
    toks <- unique(v[raro])
    if (!length(toks)) return(tibble::tibble())
    tibble::tibble(
      archivo        = archivo,
      columna        = col,
      n_na_nuevos    = sum(raro, na.rm = TRUE),
      token_muestra  = head(toks, 1)
    )
  })
}

# ────────────────────────────────────────────────────────────────────────────
# 3) Lectura robusta de CSV RAW (todo a texto)
# ────────────────────────────────────────────────────────────────────────────

read_raw <- function(path,
                     delim_try = c(";", ",", "\t"),
                     enc_try   = c("UTF-8", "UTF-8-BOM", "Latin1")) {
  assert_infile(path)
  results <- list(); scores <- numeric()
  for (d in delim_try) {
    for (e in enc_try) {
      df <- try(
        readr::read_delim(
          file = path, delim = d,
          col_types = readr::cols(.default = readr::col_character()),
          na = c("", "NA", "NaN", "null", "Null"),
          locale = readr::locale(encoding = e),
          guess_max = 100000, trim_ws = TRUE,
          show_col_types = FALSE
        ),
        silent = TRUE
      )
      if (inherits(df, "try-error")) next
      sc <- ncol(df) + 0.001 * pmin(nrow(df), 1e6)
      if (ncol(df) <= 1) sc <- sc * 0.05
      results[[length(results) + 1]] <- list(df = df, delim = d, enc = e, score = sc)
      scores[length(scores) + 1] <- sc
    }
  }
  if (!length(results)) abort("No se pudo leer el fichero: ", path)
  
  best <- results[[which.max(scores)]]
  df   <- janitor::clean_names(best$df)
  attr(df, "raw_sep")      <- best$delim
  attr(df, "raw_encoding") <- best$enc
  
  message(sprintf("Leído: %s (sep='%s', enc='%s', %d filas, %d cols)",
                  basename(path), best$delim, best$enc, nrow(df), ncol(df)))
  
  .log_writer("read", path, list(rows = nrow(df), cols = ncol(df),
                                 meta = paste0("sep=", best$delim, ";enc=", best$enc)))
  df
}

# ────────────────────────────────────────────────────────────────────────────
# 4) Escritura consistente de CSV
# ────────────────────────────────────────────────────────────────────────────

write_clean <- function(df, path, na = "") {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  readr::write_csv(df, path, na = na)
  rel <- tryCatch(fs::path_rel(path, start = here::here()), error = function(...) path)
  message("Escrito: ", rel)
  .log_writer("write", path, list(
    rows = tryCatch(nrow(df), error = function(...) NA_integer_),
    cols = tryCatch(ncol(df), error = function(...) NA_integer_)
  ))
  invisible(path)
}

# ────────────────────────────────────────────────────────────────────────────
# 5) Agregaciones seguras
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
# 6) Joins con logging (para detectar claves/años perdidos)
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
# 7) Detección/Extracción de año
# ────────────────────────────────────────────────────────────────────────────

detect_year_col <- function(nms) {
  cand <- c("periodo", "período", "ano", "año", "anio", "year",
            "time", "time_period", "TIME_PERIOD", "fecha")
  ix <- which(tolower(nms) %in% tolower(cand))
  if (length(ix)) nms[ix[1]] else {
    hit <- grep("(period|períod|ano|año|year)", nms, ignore.case = TRUE, value = TRUE)
    if (length(hit)) hit[1] else NA_character_
  }
}

extract_year <- function(x) {
  suppressWarnings(as.integer(stringr::str_extract(as.character(x), "\\d{4}")))
}

# ────────────────────────────────────────────────────────────────────────────
# 8) Cargador estándar de series (ano + 1 columna objetivo con coalesce)
# ────────────────────────────────────────────────────────────────────────────

coalesce_any <- function(df, candidates) {
  cols <- intersect(candidates, names(df))
  if (!length(cols)) abort("Ninguna columna candidata existe: ", paste(candidates, collapse = ", "))
  dplyr::coalesce(!!!df[cols])
}

# Devuelve serie NUMERIZADA con limpia_num(); avisa si detecta años duplicados.
load_series <- function(fp, out_name, candidates, year_col_candidates = NULL) {
  df <- readr::read_csv(fp, show_col_types = FALSE) |> janitor::clean_names()
  
  if (is.null(year_col_candidates)) {
    year_col_candidates <- c("ano","año","anio","periodo","período","year","time","time_period","fecha")
  }
  
  col_year <- detect_year_col(names(df))
  if (is.na(col_year)) {
    col_year <- intersect(year_col_candidates, names(df))[1]
  }
  if (is.na(col_year)) abort("Falta columna de año en: ", basename(fp))
  
  y <- extract_year(df[[col_year]])
  val_chr <- coalesce_any(df, candidates)
  
  out <- tibble::tibble(
    ano = y,
    !!out_name := limpia_num(val_chr)
  )
  
  # Duplicate-year guard (informativo)
  dup <- out |> dplyr::filter(!is.na(ano)) |> dplyr::count(ano) |> dplyr::filter(n > 1)
  if (nrow(dup)) {
    message("[load_series] Años duplicados en ", basename(fp), ": ",
            paste(dup$ano, collapse = ", "), " (me quedo con la primera fila por año)")
    out <- out |> dplyr::distinct(ano, .keep_all = TRUE)
  }
  
  out
}
