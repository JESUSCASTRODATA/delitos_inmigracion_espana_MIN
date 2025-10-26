#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
###############################################################################
# 18_gini_opcional.R — Índice de Gini (opcional) para robustez (2010–2023)
# Proyecto: Delitos e Inmigración en España (2010–2023)
# Autor: Jesús Castro · JESUSCASTRODATA
#
# IN  : data/raw/gini.csv  (Eurostat/INE u otra; sep ';' o ',')
# OUT : data/processed/gini_total.csv  (ano, gini)  # gini en 0–100
# QA  : output/tables/18_gini_qa_cols.csv
#       output/tables/18_gini_qa_cobertura.csv
#       output/tables/18_gini_qa_rango.csv
#
# ENV : YEAR_MIN=2010 YEAR_MAX=2023
###############################################################################

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(janitor); library(here)
  library(stringr); library(tibble); library(fs); library(tidyr); library(tidyselect)
})

msg   <- function(...) message("[18GINI] ", paste0(...))
abort <- function(...) stop(paste0(...), call. = FALSE)

# --- ventana temporal ---------------------------------------------------------
YEAR_MIN <- as.integer(Sys.getenv("YEAR_MIN", "2010"))
YEAR_MAX <- as.integer(Sys.getenv("YEAR_MAX", "2023"))
YEARS_SEQ <- YEAR_MIN:YEAR_MAX

fp_in  <- here::here("data","raw","gini.csv")
fp_out <- here::here("data","processed","gini_total.csv")
qa_dir <- here::here("output","tables"); fs::dir_create(qa_dir)

if (!file.exists(fp_in)) abort("No existe data/raw/gini.csv. Aporta el fichero o desactiva este paso.")

# --- detección separador/encoding --------------------------------------------
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

# --- parser numérico dual (coma o punto) -------------------------------------
parse_num_dual <- function(x){
  x <- as.character(x)
  x[x %in% c(":", "", "NA", "N/A", "..", "...", "…", "-", "—")] <- NA_character_
  # intento 1: coma decimal
  v1 <- suppressWarnings(readr::parse_number(x, locale = readr::locale(decimal_mark = ",", grouping_mark = ".")))
  # intento 2: punto decimal
  v2 <- suppressWarnings(readr::parse_number(x, locale = readr::locale(decimal_mark = ".", grouping_mark = ",")))
  # elige la versión con más no-NA; si igual, prioriza la que tenga rango plausible (0–1000)
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

# --- lectura -----------------------------------------------------------------
delim <- detect_delim(fp_in); enc <- detect_encoding(fp_in)
msg(sprintf("Leyendo gini.csv (sep='%s', enc='%s', ventana=%d-%d) …", delim, enc, YEAR_MIN, YEAR_MAX))
raw <- suppressWarnings(
  readr::read_delim(fp_in, delim = delim,
                    locale = readr::locale(encoding = enc),
                    show_col_types = FALSE, guess_max = 100000, trim_ws = TRUE)
) |> janitor::clean_names()

if (!nrow(raw)) abort("gini.csv vacío o ilegible.")

# --- detectar columnas de año/valor ------------------------------------------
nms <- names(raw)
cand_year <- nms[grepl("^(ano|año|year|time|periodo|period|time_period)$", nms, ignore.case = TRUE)]
if (!length(cand_year)) cand_year <- nms[grepl("ano|año|year|time|period", nms, ignore.case = TRUE)]
cand_val  <- nms[grepl("gini|valor|value|indice|index|total|obs_value|obsvalue|(^|_)v$", nms, ignore.case = TRUE)]

# formato wide → pivot a largo
year_cols <- nms[grepl("^(19|20)[0-9]{2}$", nms)]
if (!length(cand_val) && length(year_cols) >= 5) {
  raw <- tidyr::pivot_longer(raw, cols = tidyselect::all_of(year_cols),
                             names_to = "year_wide", values_to = "value_wide") |>
    janitor::clean_names()
  nms <- names(raw)
  cand_year <- "year_wide"
  cand_val  <- "value_wide"
}

if (!length(cand_year) || !length(cand_val))
  abort("No se encuentran columnas de año/valor plausibles en gini.csv.")

col_ano <- cand_year[[1]]
col_val <- cand_val[[1]]

# QA columnas detectadas
readr::write_csv(tibble(cols_detectadas = nms, col_ano = col_ano, col_val = col_val),
                 file.path(qa_dir, "18_gini_qa_cols.csv"))

# --- limpieza, recorte, agregación -------------------------------------------
gini <- raw |>
  transmute(
    ano  = suppressWarnings(as.integer(stringr::str_extract(.data[[col_ano]], "(19|20)[0-9]{2}"))),
    gini = parse_num_dual(.data[[col_val]])
  ) |>
  filter(!is.na(ano), !is.na(gini)) |>
  filter(dplyr::between(ano, YEAR_MIN, YEAR_MAX)) |>
  group_by(ano) |>
  summarise(gini = mean(gini, na.rm = TRUE), .groups = "drop") |>
  arrange(ano)

if (!nrow(gini)) abort(sprintf("Sin filas válidas tras limpieza (%d–%d).", YEAR_MIN, YEAR_MAX))

# --- autoescala a 0–100 ------------------------------------------------------
rng <- range(gini$gini, na.rm = TRUE)
escala <- "sin cambios"
if (is.finite(rng[2]) && rng[2] <= 1.5) {
  gini <- gini |> mutate(gini = gini * 100); escala <- "x100 (0–1 → 0–100)"
} else if (is.finite(rng[2]) && rng[2] > 100 && rng[2] <= 1000) {
  gini <- gini |> mutate(gini = gini / 10);  escala <- "/10 (0–1000 → 0–100)"
}
gini <- gini |> mutate(gini = round(gini, 1))

# --- QA cobertura y rangos ---------------------------------------------------
qa_cov <- tibble(
  years_min = min(gini$ano, na.rm = TRUE),
  years_max = max(gini$ano, na.rm = TRUE),
  n_years   = dplyr::n_distinct(gini$ano),
  missing   = paste(setdiff(YEARS_SEQ, sort(unique(gini$ano))), collapse = ", ")
)
readr::write_csv(qa_cov, file.path(qa_dir, "18_gini_qa_cobertura.csv"))

qa_rng <- tibble(
  min_gini = min(gini$gini, na.rm = TRUE),
  max_gini = max(gini$gini, na.rm = TRUE),
  escala_aplicada = escala
)
readr::write_csv(qa_rng, file.path(qa_dir, "18_gini_qa_rango.csv"))

# --- salida ------------------------------------------------------------------
fs::dir_create(fs::path_dir(fp_out))
readr::write_csv(gini, fp_out)
msg(sprintf("Escrito: %s (%d filas, gini en 0–100).", fp_out, nrow(gini)))
