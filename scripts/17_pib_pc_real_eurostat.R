#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
###############################################################################
# 17_pib_pc_real_eurostat.R — PIB per cápita real (ES) 2010–2023
# Proyecto: Delitos e Inmigración en España (2010–2023)
# Autor: Jesús Castro (Analista de Datos)
# Fecha: 2025-08-23 | Versión: 1.0 (alineado con 00_utils_limpieza.R del 2025-08-22)
#
# Entradas (data/raw/):
#   - Preferente: pib.csv (SDMX-CSV de Eurostat con columnas unit, na_item, geo, time, value)
#   - Alternativa: estat_nama_10_pc_filtered_en.csv (o similar, ya descargado de Eurostat)
#
# Salidas (data/processed/):
#   - pib_pc_real_es.csv  (ano, pib_pc_real)  — CLV*_EUR_HAB (encadenado, € por habitante)
#
# QA (output/tables/):
#   - qa_pib_filtros.csv (conteo por unit/na_item/geo)
#   - qa_pib_anios_fuera_rango.csv (si aplica)
#   - qa_pib_cobertura.csv
#   - qa_pib_duplicados.csv (si aplica)
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
  msg <- if (!is.null(caller)) paste0("ROOT:", caller) else "ROOT:17_pib_pc_real_eurostat"
  message(msg, " -> ", getwd()); invisible(getwd())
}
if (!exists("assert_infile")) assert_infile <- function(path){ if (!file.exists(path)) abort("No existe: ", path) }
if (!exists("write_clean")) write_clean <- function(df, path, na = ""){ dir.create(dirname(path), TRUE, TRUE); readr::write_csv(df, path, na = na); message("Escrito: ", normalizePath(path, winslash = "/")); invisible(path) }
if (!exists("read_raw")) read_raw <- function(path){ readr::read_delim(path, delim = ",", col_types = readr::cols(.default = readr::col_character()), show_col_types = FALSE) |> janitor::clean_names() }
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
# Rutas (entrada flexible)
# ————————————————————————————————————————————————————————————————
if (requireNamespace("here", quietly = TRUE)) {
  here_path <- function(...) here::here(...)
} else {
  base_root <- getwd()
  message("Paquete 'here' no disponible; usando getwd(): ", base_root)
  here_path <- function(...) file.path(base_root, ...)
}

in_candidates <- c(
  here_path("data","raw","pib.csv"),
  here_path("data","raw","estat_nama_10_pc_filtered_en.csv")
)
# Busca también cualquier nama_10_pc*.csv si existen
more <- list.files(here_path("data","raw"), pattern = "nama_10_pc.*\\.csv$", full.names = TRUE)
if (length(more)) in_candidates <- c(in_candidates, more)

fp_in  <- in_candidates[file.exists(in_candidates)][1]
if (is.na(fp_in) || is.null(fp_in)) abort("No se encontró un fichero de PIB válido en data/raw/ (pib.csv / nama_10_pc*.csv)")

fp_out <- here_path("data","processed","pib_pc_real_es.csv")
qa_dir <- here_path("output","tables")
dir.create(qa_dir, recursive = TRUE, showWarnings = FALSE)

# ————————————————————————————————————————————————————————————————
# 1) Lectura y normalización básica
# ————————————————————————————————————————————————————————————————
raw <- read_raw(fp_in)  # 00_utils::read_raw suele detectar sep/enc; este fallback asume ","
cn <- names(raw)

# Detecta columnas clave
col_unit   <- cn[grepl("^unit$", cn, ignore.case = TRUE)][1]
col_naitem <- cn[grepl("^na_item$", cn, ignore.case = TRUE)][1]
col_geo    <- cn[grepl("^geo$", cn, ignore.case = TRUE)][1]
col_time   <- detect_year_col(cn); if (is.na(col_time)) col_time <- cn[grepl("time", cn, ignore.case = TRUE)][1]

# Columna de valor (Eurostat suele usar value/values/obs_value)
val_candidates <- c("value","values","obs_value","obsvalue","v")
col_value <- intersect(val_candidates, cn)[1]
if (is.na(col_value) || !nzchar(col_value)) {
  # como último recurso, intenta la última columna
  col_value <- tail(cn, 1)
}

if (any(is.na(c(col_unit, col_naitem, col_geo, col_time, col_value)))) {
  abort(paste0(
    "❌ Columnas requeridas no detectadas. Encontradas→ ",
    "unit:",   ifelse(is.na(col_unit),   "<NA>", col_unit),
    " | na_item:", ifelse(is.na(col_naitem), "<NA>", col_naitem),
    " | geo:",    ifelse(is.na(col_geo),    "<NA>", col_geo),
    " | time:",   ifelse(is.na(col_time),   "<NA>", col_time),
    " | value:",  ifelse(is.na(col_value),  "<NA>", col_value)
  ))
}

x <- raw |>
  transmute(
    unit   = .data[[col_unit]],
    na_item= .data[[col_naitem]],
    geo    = .data[[col_geo]],
    periodo= .data[[col_time]],
    valor  = safe_parse_number(.data[[col_value]])
  ) |>
  mutate(
    unit_norm = tolower(as.character(unit)),
    ano = extract_year(periodo)
  )

# ————————————————————————————————————————————————————————————————
# 2) Filtros: geo ES, na_item B1GQ, unit CLV* + HAB
# ————————————————————————————————————————————————————————————————
# QA de combos disponibles
qa_filtros <- x |>
  count(unit, na_item, geo, name = "n") |>
  arrange(desc(n))
write_clean(qa_filtros, file.path(qa_dir, "qa_pib_filtros.csv"))

x <- x |>
  filter(geo %in% c("ES","Spain","ES_TOT","ES00")) |>
  filter(toupper(na_item) == "B1GQ")

units_clv_hab <- unique(x$unit[grepl("CLV", x$unit, ignore.case = TRUE) & grepl("HAB", x$unit, ignore.case = TRUE)])
if (length(units_clv_hab) == 0) abort("No se encontró unidad CLV* con HAB (real per cápita). Revisa el fichero de entrada.")
# Si hay varias, prioriza *_EUR_HAB
pref_ix <- grep("EUR", units_clv_hab)
unit_sel <- if (length(pref_ix)) units_clv_hab[pref_ix[1]] else units_clv_hab[1]
message("Unidad seleccionada: ", unit_sel)

x <- x |>
  filter(unit == unit_sel) |>
  select(ano, valor)

# ————————————————————————————————————————————————————————————————
# 3) QA: años y duplicados
# ————————————————————————————————————————————————————————————————
qa_anios <- x |>
  mutate(flag_fuera = is.na(ano) | ano < YEAR_MIN | ano > YEAR_MAX) |>
  filter(flag_fuera) |>
  arrange(ano)
if (nrow(qa_anios) > 0) write_clean(qa_anios, file.path(qa_dir, "qa_pib_anios_fuera_rango.csv"))

x <- x |> filter(!is.na(ano), dplyr::between(ano, YEAR_MIN, YEAR_MAX))

# Duplicados por año
qa_dup <- x |> count(ano, name = "n") |> filter(n > 1)
if (nrow(qa_dup) > 0) write_clean(qa_dup, file.path(qa_dir, "qa_pib_duplicados.csv"))

# Agrega si quedaran duplicados
x <- x |> group_by(ano) |> summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop")

# Cobertura
present <- sort(unique(x$ano)); missing <- setdiff(YEARS_SEQ, present)
qa_cov <- tibble::tibble(
  variable = "pib_pc_real_es",
  years_min = ifelse(length(present) > 0, min(present), NA_integer_),
  years_max = ifelse(length(present) > 0, max(present), NA_integer_),
  n_years = length(present),
  n_missing = length(missing),
  missing_list = paste(missing, collapse = ", ")
)
write_clean(qa_cov, file.path(qa_dir, "qa_pib_cobertura.csv"))

# ————————————————————————————————————————————————————————————————
# 4) Salida final
# ————————————————————————————————————————————————————————————————
if (nrow(x) == 0) abort("❌ No hay filas tras filtrar PIB per cápita real.")
if (any(is.na(x$valor))) abort("❌ Existen valores NA en 'valor' tras el filtrado.")

out <- x |> arrange(ano) |> transmute(ano, pib_pc_real = valor)
write_clean(out, fp_out)
message("✔ PIB per cápita real (ES) generado: ", fp_out)
