#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
###############################################################################
# 15_limpieza_detenciones_totales.R — Nivel Nacional (2010–2023)
# Proyecto: Delitos e Inmigración en España (2010–2023)
# Autor: Jesús Castro (Analista de Datos)
# Fecha: 2025-08-23 | Versión: 1.0 (alineado con 00_utils_limpieza.R del 2025-08-22)
#
# Entradas
#   - data/raw/detenciones_tipologia.csv
#   - (opcional) config/map_tipologias.csv  ← si existe, se usa para normalizar etiquetas
# Dependencias
#   - R/00_utils_limpieza.R → root_init(), assert_infile(), read_raw(),
#     write_clean(), safe_parse_number(), detect_year_col(), extract_year()
# Salidas
#   - data/processed/detenciones_totales_total_nacional.csv (ano, tipo, valor)
#   - data/processed/detenciones_totales_total.csv (ano, det_tot)
# QA
#   - Usa tipo == "total infracciones penales" si existe; si no, suma tipologías.
#   - Excluye regiones desconocida/en el extranjero al agregar CCAA.
#   - Reporta negativos, duplicados, cobertura, y discrepancias total vs suma.
###############################################################################

suppressPackageStartupMessages({
  library(here)
  library(readr)
  library(dplyr)
  library(tidyr)
  library(janitor)
  library(stringr)
  library(purrr)
})

# ————————————————————————————————————————————————————————————————
# 0) Cargar utilidades comunes
# ————————————————————————————————————————————————————————————————
source_if <- function(path) { if (file.exists(path)) source(path, local = TRUE) }
source_if(here::here("R", "00_utils_limpieza.R"))
source_if(here::here("scripts", "00_utils_limpieza.R"))

# Fallbacks mínimos
if (!exists("abort")) abort <- function(...) stop(paste0(...), call. = FALSE)
if (!exists("root_init")) root_init <- function(caller = NULL, project_root_hint = NULL){
  msg <- if (!is.null(caller)) paste0("ROOT:", caller) else "ROOT:15_limpieza_detenciones_totales"
  message(msg, " -> ", getwd()); invisible(getwd())
}
if (!exists("assert_infile")) assert_infile <- function(path){ if (!file.exists(path)) abort("No existe: ", path) }
if (!exists("write_clean")) write_clean <- function(df, path, na = ""){ dir.create(dirname(path), TRUE, TRUE); readr::write_csv(df, path, na = na); message("Escrito: ", normalizePath(path, winslash = "/")); invisible(path) }
if (!exists("read_raw")) read_raw <- function(path){ readr::read_delim(path, delim = ";", col_types = readr::cols(.default = readr::col_character()), show_col_types = FALSE) |> janitor::clean_names() }
if (!exists("safe_parse_number")) safe_parse_number <- function(x){ readr::parse_number(as.character(x)) }
if (!exists("detect_year_col")) detect_year_col <- function(nms){
  cand <- c("periodo", "período", "ano", "año", "year", "time", "time_period", "TIME_PERIOD")
  ix <- which(tolower(nms) %in% tolower(cand))
  if (length(ix)) nms[ix[1]] else NA_character_
}
if (!exists("extract_year")) extract_year <- function(x){ y <- stringr::str_extract(as.character(x), "[0-9]{4}"); suppressWarnings(as.integer(y)) }

root_init()

YEAR_MIN <- 2010L
YEAR_MAX <- 2023L
YEARS_SEQ <- YEAR_MIN:YEAR_MAX

# ————————————————————————————————————————————————————————————————
# Rutas
# ————————————————————————————————————————————————————————————————
if (requireNamespace("here", quietly = TRUE)) {
  here_path <- function(...) here::here(...)
} else {
  base_root <- getwd()
  message("Paquete 'here' no disponible; usando getwd(): ", base_root)
  here_path <- function(...) file.path(base_root, ...)
}

fp_in   <- here_path("data", "raw", "detenciones_tipologia.csv")
fp_map  <- here_path("config", "map_tipologias.csv")
fp_nat  <- here_path("data", "processed", "detenciones_totales_total_nacional.csv")
fp_tot  <- here_path("data", "processed", "detenciones_totales_total.csv")
qa_dir  <- here_path("output", "tables")
dir.create(qa_dir, recursive = TRUE, showWarnings = FALSE)

assert_infile(fp_in)

# ————————————————————————————————————————————————————————————————
# 1) Lectura y normalización de columnas
# ————————————————————————————————————————————————————————————————
raw <- read_raw(fp_in)
cn <- names(raw)
cn_low <- tolower(cn)

pick_col <- function(patterns, not = NULL) {
  hit <- purrr::map_lgl(cn_low, ~ any(stringr::str_detect(.x, patterns)))
  if (!is.null(not)) hit <- hit & !stringr::str_detect(cn_low, not)
  col <- cn[which(hit)][1]
  ifelse(is.na(col) || length(col)==0, NA_character_, col)
}

col_region  <- pick_col(c("comun", "ccaa", "autono", "territ", "ambito", "ámbito", "region"))
col_tipo    <- pick_col(c("tipolog", "delit", "infracc", "tipo"))
col_periodo <- detect_year_col(cn)
if (is.na(col_periodo)) col_periodo <- pick_col(c("period", "anio", "año", "ano", "fecha", "year", "time"))
col_valor   <- pick_col(c("^total$", "^valor$", "hechos", "conteo", "numero", "n[0-9]*$", "detenc"))

# region puede no existir si el fichero es ya nacional
if (is.na(col_tipo) || is.na(col_periodo) || is.na(col_valor)) {
  abort(paste0(
    "❌ Columnas requeridas no detectadas. Encontradas→ ",
    "region:", ifelse(is.na(col_region), "<NA>", col_region),
    " | tipo:",   ifelse(is.na(col_tipo),   "<NA>", col_tipo),
    " | periodo:",ifelse(is.na(col_periodo),"<NA>", col_periodo),
    " | valor:",  ifelse(is.na(col_valor),  "<NA>", col_valor)
  ))
}

DT <- raw |>
  transmute(
    region  = if (is.na(col_region)) "TOTAL NACIONAL" else .data[[col_region]],
    tipo    = as.character(.data[[col_tipo]]),
    periodo = .data[[col_periodo]],
    valor   = safe_parse_number(.data[[col_valor]])
  ) |>
  mutate(ano = extract_year(periodo)) |>
  select(ano, region, tipo, valor)

# ————————————————————————————————————————————————————————————————
# 2) QA básicos
# ————————————————————————————————————————————————————————————————
qa_anios_fuera <- DT |>
  mutate(flag_fuera = is.na(ano) | ano < YEAR_MIN | ano > YEAR_MAX) |>
  filter(flag_fuera) |>
  arrange(ano)
if (nrow(qa_anios_fuera) > 0) write_clean(qa_anios_fuera, file.path(qa_dir, "qa_det_tot_anios_fuera_rango.csv"))

DT <- DT |>
  filter(!is.na(ano), dplyr::between(ano, YEAR_MIN, YEAR_MAX))

qa_na <- DT |>
  filter(is.na(valor) | is.na(tipo))
if (nrow(qa_na) > 0) write_clean(qa_na, file.path(qa_dir, "qa_det_tot_na.csv"))

# ————————————————————————————————————————————————————————————————
# 3) Normalizar región y agregar a TOTAL NACIONAL
# ————————————————————————————————————————————————————————————————
normalize_text <- function(x) {
  x <- stringr::str_trim(tolower(as.character(x)))
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  x <- stringr::str_squish(x)
  x
}

DT <- DT |> mutate(region_norm = normalize_text(region))
excluir_regiones <- c(
  "desconocida", "no consta", "no especificado", "sin especificar",
  "en el extranjero", "extranjero", "otros territorios", "resto del mundo"
)

has_total_nacional <- any(DT$region_norm %in% c("total nacional", "nacional", "total"), na.rm = TRUE)

nat <- if (has_total_nacional) {
  message("✓ Usando 'TOTAL NACIONAL' presente en detenciones.")
  DT |>
    filter(region_norm %in% c("total nacional", "nacional", "total")) |>
    group_by(ano, tipo) |>
    summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop")
} else {
  message("ℹ No hay 'TOTAL NACIONAL': se suman CCAA válidas (excluye desconocida/extranjero).")
  DT |>
    filter(!region_norm %in% excluir_regiones) |>
    group_by(ano, tipo) |>
    summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop")
}

# ————————————————————————————————————————————————————————————————
# 4) (Opcional) Normalizar tipologías vía config/map_tipologias.csv
# ————————————————————————————————————————————————————————————————
if (file.exists(fp_map)) {
  map_tip <- readr::read_csv(fp_map, show_col_types = FALSE) |> janitor::clean_names()
  if (all(c("raw_tipo","propuesta_tipo") %in% names(map_tip))) {
    nat <- nat |>
      left_join(map_tip |> select(raw_tipo, propuesta_tipo), by = c("tipo" = "raw_tipo")) |>
      mutate(tipo = dplyr::coalesce(propuesta_tipo, tipo)) |>
      select(-propuesta_tipo)
  }
}

# ————————————————————————————————————————————————————————————————
# 5) QA adicionales y cobertura
# ————————————————————————————————————————————————————————————————
qa_neg <- nat |> filter(valor < 0)
if (nrow(qa_neg) > 0) write_clean(qa_neg, file.path(qa_dir, "qa_det_tot_negativos.csv"))

qa_dup <- nat |> count(ano, tipo, name = "n") |> filter(n > 1)
if (nrow(qa_dup) > 0) write_clean(qa_dup, file.path(qa_dir, "qa_det_tot_duplicados.csv"))

present <- sort(unique(nat$ano)); missing <- setdiff(YEARS_SEQ, present)
qa_cov <- tibble::tibble(
  variable = "detenciones_totales",
  years_min = ifelse(length(present) > 0, min(present), NA_integer_),
  years_max = ifelse(length(present) > 0, max(present), NA_integer_),
  n_years = length(present),
  n_missing = length(missing),
  missing_list = paste(missing, collapse = ", ")
)
write_clean(qa_cov, file.path(qa_dir, "qa_det_tot_cobertura.csv"))

# ————————————————————————————————————————————————————————————————
# 6) Selección de TOTAL y consistencia con suma de tipologías
# ————————————————————————————————————————————————————————————————
# Detectar etiqueta de total por patrón robusto
norm_tipo <- normalize_text(nat$tipo)
mask_total <- grepl("^total\\s*infracci", norm_tipo) | grepl("^total$", norm_tipo)

nat_totlabel <- nat |> filter(mask_total) |> transmute(ano, det_tot = valor)

nat_sum <- nat |>
  filter(!mask_total) |>
  group_by(ano) |>
  summarise(det_tot_sum = sum(valor, na.rm = TRUE), .groups = "drop")

# Si hay etiqueta total, úsala; si no, usa la suma
if (nrow(nat_totlabel) > 0) {
  det_tot <- nat_totlabel
  # Comparación con la suma si existe también
  cmp <- det_tot |>
    full_join(nat_sum, by = "ano") |>
    mutate(diff = det_tot - det_tot_sum,
           rel_diff = ifelse(det_tot_sum > 0, diff / det_tot_sum, NA_real_)) |>
    arrange(ano)
  write_clean(cmp, file.path(qa_dir, "qa_det_tot_vs_suma.csv"))
} else {
  det_tot <- nat_sum |> transmute(ano, det_tot = det_tot_sum)
}

# ————————————————————————————————————————————————————————————————
# 7) Escrituras finales
# ————————————————————————————————————————————————————————————————
# a) Serie por tipología (TOTAL NACIONAL)
final_nat <- nat |> arrange(ano, tipo) |> select(ano, tipo, valor)
write_clean(final_nat, fp_nat)

# b) Serie total
final_tot <- det_tot |> arrange(ano) |> select(ano, det_tot)
write_clean(final_tot, fp_tot)

message("✔ Detenciones totales (TOTAL NACIONAL y total) limpios y guardados.")
