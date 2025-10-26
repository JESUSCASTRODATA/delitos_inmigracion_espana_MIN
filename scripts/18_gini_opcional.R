#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
###############################################################################
# 18_gini_opcional.R — Índice de Gini (opcional) para robustez
# IN  : data/raw/gini.csv
# OUT : data/processed/gini_total.csv  (ano, gini)  # gini en 0–100
# QA  : output/tables/18_gini_qa_cols.csv, 18_gini_qa_cobertura.csv, 18_gini_qa_rango.csv
# ENV : YEAR_MIN=2010 YEAR_MAX=2023  GINI_PAD_MISSING=true|false (default true)
###############################################################################

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(janitor); library(here)
  library(stringr); library(tibble); library(fs); library(tidyr); library(tidyselect)
})

msg   <- function(...) message("[18GINI] ", paste0(...))
abort <- function(...) stop(paste0(...), call. = FALSE)

YEAR_MIN <- as.integer(Sys.getenv("YEAR_MIN", "2010"))
YEAR_MAX <- as.integer(Sys.getenv("YEAR_MAX", "2023"))
YEARS_SEQ <- YEAR_MIN:YEAR_MAX
PAD_MISSING <- tolower(Sys.getenv("GINI_PAD_MISSING","true")) %in% c("1","true","t","yes","y","si","sí","s")

fp_in  <- here::here("data","raw","gini.csv")
fp_out <- here::here("data","processed","gini_total.csv")
qa_dir <- here::here("output","tables"); fs::dir_create(qa_dir)

if (!file.exists(fp_in)) abort("No existe data/raw/gini.csv. Aporta el fichero o desactiva este paso.")

# --- autodetección separador / encoding --------------------------------------
detect_delim <- function(path, n = 2000){
  enc <- "UTF-8"
  lines <- tryCatch(readr::read_lines(path, n_max = n, locale = readr::locale(encoding = enc)),
                    error = function(e) readr::read_lines(path, n_max = n, locale = readr::locale(encoding = "Latin1")))
  if (!length(lines)) return(",")
  counts <- c(`;`=sum(stringr::str_count(lines,";")),
              `,`=sum(stringr::str_count(lines,",")),
              `\t`=sum(stringr::str_count(lines,"\t")))
  names(counts)[which.max(counts)]
}
detect_encoding <- function(path){
  ge <- tryCatch(readr::guess_encoding(path, n_max = 50000), error = function(e) NULL)
  if (is.null(ge) || nrow(ge)==0) "UTF-8" else ge$encoding[1]
}
parse_num_dual <- function(x){
  x <- as.character(x)
  x[x %in% c(":", "", "NA", "N/A", "..", "...", "…", "-", "—")] <- NA_character_
  v1 <- suppressWarnings(readr::parse_number(x, locale = readr::locale(decimal_mark = ",", grouping_mark = ".")))
  v2 <- suppressWarnings(readr::parse_number(x, locale = readr::locale(decimal_mark = ".", grouping_mark = ",")))
  nn1 <- sum(!is.na(v1)); nn2 <- sum(!is.na(v2))
  if (nn2 > nn1) return(v2)
  if (nn2 == nn1) {
    r1 <- range(v1, na.rm = TRUE); r2 <- range(v2, na.rm = TRUE)
    s1 <- if (is.finite(r1[2])) (r1[2] <= 1000) else FALSE
    s2 <- if (is.finite(r2[2])) (r2[2] <= 1000) else FALSE
    if (s2 && !s1) return(v2)
  }
  v1
}

delim <- detect_delim(fp_in); enc <- detect_encoding(fp_in)
msg(sprintf("Leyendo gini.csv (sep='%s', enc='%s', ventana=%d-%d) …", delim, enc, YEAR_MIN, YEAR_MAX))
raw <- suppressWarnings(
  readr::read_delim(fp_in, delim = delim,
                    locale = readr::locale(encoding = enc),
                    show_col_types = FALSE, guess_max = 100000, trim_ws = TRUE)
) |> janitor::clean_names()
if (!nrow(raw)) abort("gini.csv vacío o ilegible.")

# --- caso conocido (tu fichero) ----------------------------------------------
build_from_known_format <- function(df) {
  msg("Formato detectado: (nacimiento_residencia, periodo, total). Normalizando escala mixta…")
  df %>%
    filter(tolower(nacimiento_residencia) == "gini") %>%
    transmute(
      ano = suppressWarnings(as.integer(periodo)),
      val = suppressWarnings(as.numeric(total))
    ) %>%
    filter(!is.na(ano), dplyr::between(ano, YEAR_MIN, YEAR_MAX)) %>%
    group_by(ano) %>%
    summarise(
      gini = {
        cand_0_100 <- val[is.finite(val) & val <= 100]
        if (length(cand_0_100) > 0) max(cand_0_100, na.rm = TRUE) else max(val/10, na.rm = TRUE)
      },
      .groups = "drop"
    ) %>% arrange(ano)
}

# --- fallback genérico --------------------------------------------------------
build_generic <- function(df) {
  nms <- names(df)
  cand_year <- nms[grepl("^(ano|año|year|time|periodo|period|time_period)$", nms, ignore.case = TRUE)]
  if (!length(cand_year)) cand_year <- nms[grepl("ano|año|year|time|period", nms, ignore.case = TRUE)]
  cand_val  <- nms[grepl("gini|valor|value|indice|index|total|obs_value|obsvalue|(^|_)v$", nms, ignore.case = TRUE)]
  year_cols <- nms[grepl("^(19|20)[0-9]{2}$", nms)]
  if (!length(cand_val) && length(year_cols) >= 5) {
    df <- tidyr::pivot_longer(df, cols = tidyselect::all_of(year_cols),
                              names_to = "year_wide", values_to = "value_wide") |>
      janitor::clean_names()
    nms <- names(df); cand_year <- "year_wide"; cand_val <- "value_wide"
  }
  if (!length(cand_year) || !length(cand_val))
    abort("No se encuentran columnas de año/valor plausibles en gini.csv.")
  col_ano <- cand_year[[1]]; col_val <- cand_val[[1]]
  
  df |>
    transmute(
      ano  = suppressWarnings(as.integer(stringr::str_extract(.data[[col_ano]], "(19|20)[0-9]{2}"))),
      val  = parse_num_dual(.data[[col_val]]),
      gini = dplyr::case_when(
        val > 1000 ~ val/10,
        val <= 1.5 ~ val*100,
        TRUE       ~ val
      )
    ) |>
    filter(!is.na(ano), dplyr::between(ano, YEAR_MIN, YEAR_MAX)) |>
    group_by(ano) |>
    summarise(gini = mean(gini, na.rm = TRUE), .groups = "drop") |>
    arrange(ano)
}

use_known <- all(c("nacimiento_residencia","periodo","total") %in% names(raw))
gini <- if (use_known) build_from_known_format(raw) else build_generic(raw)

# --- padding opcional ---------------------------------------------------------
if (PAD_MISSING) {
  faltan <- setdiff(YEARS_SEQ, gini$ano)
  if (length(faltan)) {
    msg("Padding de años sin dato (NA) para: ", paste(faltan, collapse=", "))
    gini <- dplyr::bind_rows(gini, tibble(ano = faltan, gini = NA_real_)) |> arrange(ano)
  }
}

# --- QA -----------------------------------------------------------------------
readr::write_csv(tibble(cols_detectadas = names(raw),
                        ruta = if (use_known) "known" else "generic"),
                 file.path(qa_dir, "18_gini_qa_cols.csv"))

qa_cov <- tibble(
  years_min = suppressWarnings(min(gini$ano, na.rm = TRUE)),
  years_max = suppressWarnings(max(gini$ano, na.rm = TRUE)),
  n_years   = dplyr::n_distinct(gini$ano),
  missing   = paste(setdiff(YEARS_SEQ, sort(unique(gini$ano))), collapse = ", ")
)
readr::write_csv(qa_cov, file.path(qa_dir, "18_gini_qa_cobertura.csv"))

qa_rng <- tibble(
  min_gini = suppressWarnings(min(gini$gini, na.rm = TRUE)),
  max_gini = suppressWarnings(max(gini$gini, na.rm = TRUE)),
  nota     = "gini en 0–100; normalización automática aplicada"
)
readr::write_csv(qa_rng, file.path(qa_dir, "18_gini_qa_rango.csv"))

# --- salida -------------------------------------------------------------------
if (!nrow(gini)) abort(sprintf("Sin filas válidas tras limpieza (%d–%d).", YEAR_MIN, YEAR_MAX))
fs::dir_create(fs::path_dir(fp_out))
readr::write_csv(gini, fp_out)
msg(sprintf("Escrito: %s (%d filas, gini en 0–100).", fp_out, nrow(gini)))


