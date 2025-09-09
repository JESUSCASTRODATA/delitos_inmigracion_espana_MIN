#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
###############################################################################
# 13_limpieza_hechos_conocidos.R — Nivel Nacional (2010–2023)
# Proyecto: Delitos e Inmigración en España (2010–2023)
# Autor: Jesús Castro (Analista de Datos)
# Fecha: 2025-08-23 | Versión: 1.2 (alineado con 00_utils_limpieza.R del 2025-08-22)
#
# Entradas
#   - data/raw/hechos_conocidos.csv
# Dependencias
#   - R/00_utils_limpieza.R → root_init(), assert_infile(), read_raw(),
#     write_clean(), safe_parse_number(), detect_year_col(), extract_year()
# Salidas
#   - data/processed/hechos_conocidos_total_nacional.csv (ano, tipo, valor)
#   - output/tables/qa_hc_cobertura.csv
#   - output/tables/qa_hc_cobertura_por_tipo.csv
#   - output/tables/qa_hc_anios_fuera_rango.csv (si aplica)
#   - output/tables/qa_hc_negativos.csv (si aplica)
#   - output/tables/qa_hc_na.csv (si aplica)
#   - output/tables/qa_hc_duplicados.csv (si aplica)
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

# Fallbacks mínimos si no se cargó el utilitario (para no romper durante edición)
if (!exists("abort")) abort <- function(...) stop(paste0(...), call. = FALSE)
if (!exists("root_init")) root_init <- function(caller = NULL, project_root_hint = NULL){
  msg <- if (!is.null(caller)) paste0("ROOT:", caller) else "ROOT:13_limpieza_hechos_conocidos"
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
if (!exists("extract_year")) extract_year <- function(x){
  y <- stringr::str_extract(as.character(x), "[0-9]{4}")
  suppressWarnings(as.integer(y))
}

# Inicializa raíz del proyecto (sin argumentos para evitar problemas de comillas)
root_init()

YEAR_MIN <- 2010L
YEAR_MAX <- 2023L
YEARS_SEQ <- YEAR_MIN:YEAR_MAX

# ————————————————————————————————————————————————————————————————
# Rutas (robustas y sin try/strings conflictivos)
# ————————————————————————————————————————————————————————————————
if (requireNamespace("here", quietly = TRUE)) {
  here_path <- function(...) here::here(...)
} else {
  base_root <- getwd()
  message("Paquete 'here' no disponible; usando getwd(): ", base_root)
  here_path <- function(...) file.path(base_root, ...)
}

fp_in  <- here_path("data", "raw", "hechos_conocidos.csv")
fp_out <- here_path("data", "processed", "hechos_conocidos_total_nacional.csv")
qa_dir <- here_path("output", "tables")
dir.create(qa_dir, recursive = TRUE, showWarnings = FALSE)

assert_infile(fp_in)

# ————————————————————————————————————————————————————————————————
# 1) Lectura y normalización de columnas
# ————————————————————————————————————————————————————————————————
raw <- read_raw(fp_in)  # 00_utils ya hace clean_names()
cn <- names(raw)
cn_low <- tolower(cn)

pick_col <- function(patterns, not = NULL) {
  hit <- map_lgl(cn_low, ~ any(stringr::str_detect(.x, patterns)))
  if (!is.null(not)) hit <- hit & !stringr::str_detect(cn_low, not)
  col <- cn[which(hit)][1]
  ifelse(is.na(col) || length(col)==0, NA_character_, col)
}

col_region  <- pick_col(c("comun", "ccaa", "autono", "territ", "ambito", "ámbito", "region"))
col_tipo    <- pick_col(c("tipolog", "delit", "infracc"))
col_periodo <- detect_year_col(cn)
if (is.na(col_periodo)) col_periodo <- pick_col(c("period", "anio", "año", "ano", "fecha", "year", "time"))
col_valor   <- pick_col(c("^total$", "^valor$", "hechos", "conteo", "numero", "n[0-9]*$"))

if (any(is.na(c(col_region, col_tipo, col_periodo, col_valor)))) {
  abort(paste0(
    "❌ Columnas requeridas no detectadas. Encontradas→ ",
    "region:", ifelse(is.na(col_region), "NA", col_region),
    " | tipo:", ifelse(is.na(col_tipo), "NA", col_tipo),
    " | periodo:", ifelse(is.na(col_periodo), "NA", col_periodo),
    " | valor:", ifelse(is.na(col_valor), "NA", col_valor)
  ))
}

hc <- raw |>
  transmute(
    region  = .data[[col_region]],
    tipo    = as.character(.data[[col_tipo]]),
    periodo = .data[[col_periodo]],
    valor   = safe_parse_number(.data[[col_valor]])
  ) |>
  mutate(
    ano = extract_year(periodo)
  ) |>
  select(ano, region, tipo, valor)

# ————————————————————————————————————————————————————————————————
# 2) QA: años fuera de rango y NAs
# ————————————————————————————————————————————————————————————————
qa_anios_fuera <- hc |>
  mutate(flag_fuera = is.na(ano) | ano < YEAR_MIN | ano > YEAR_MAX) |>
  filter(flag_fuera) |>
  arrange(ano)
if (nrow(qa_anios_fuera) > 0) write_clean(qa_anios_fuera, file.path(qa_dir, "qa_hc_anios_fuera_rango.csv"))

hc <- hc |>
  filter(!is.na(ano), dplyr::between(ano, YEAR_MIN, YEAR_MAX))

qa_na <- hc |>
  filter(is.na(valor) | is.na(tipo))
if (nrow(qa_na) > 0) write_clean(qa_na, file.path(qa_dir, "qa_hc_na.csv"))

# ————————————————————————————————————————————————————————————————
# 3) Agregación a TOTAL NACIONAL
# ————————————————————————————————————————————————————————————————
normalize_text <- function(x) {
  x <- stringr::str_trim(tolower(as.character(x)))
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  x <- stringr::str_squish(x)
  x
}

hc <- hc |> mutate(region_norm = normalize_text(region))

excluir_regiones <- c(
  "desconocida", "no consta", "no especificado", "sin especificar",
  "en el extranjero", "extranjero", "otros territorios", "resto del mundo"
)

has_total_nacional <- any(hc$region_norm %in% c("total nacional", "nacional", "total"), na.rm = TRUE)

nat <- if (has_total_nacional) {
  message("✓ Usando 'TOTAL NACIONAL' presente en el fichero (hechos conocidos).")
  hc |>
    filter(region_norm %in% c("total nacional", "nacional", "total")) |>
    group_by(ano, tipo) |>
    summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop")
} else {
  message("ℹ No hay 'TOTAL NACIONAL': se suman CCAA válidas (excluye desconocida/extranjero).")
  hc |>
    filter(!region_norm %in% excluir_regiones) |>
    group_by(ano, tipo) |>
    summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop")
}

# ————————————————————————————————————————————————————————————————
# 4) QA: negativos, duplicados y cobertura
# ————————————————————————————————————————————————————————————————
qa_neg <- nat |> filter(valor < 0)
if (nrow(qa_neg) > 0) write_clean(qa_neg, file.path(qa_dir, "qa_hc_negativos.csv"))

qa_dup <- nat |> count(ano, tipo, name = "n") |> filter(n > 1)
if (nrow(qa_dup) > 0) write_clean(qa_dup, file.path(qa_dir, "qa_hc_duplicados.csv"))

present <- sort(unique(nat$ano))
missing <- setdiff(YEARS_SEQ, present)
qa_cov <- tibble::tibble(
  variable = "hechos_conocidos",
  years_min = ifelse(length(present) > 0, min(present), NA_integer_),
  years_max = ifelse(length(present) > 0, max(present), NA_integer_),
  n_years = length(present),
  n_missing = length(missing),
  missing_list = paste(missing, collapse = ", ")
)
write_clean(qa_cov, file.path(qa_dir, "qa_hc_cobertura.csv"))

qa_cov_tipo <- nat |>
  group_by(tipo) |>
  summarise(
    n_years = n_distinct(ano),
    complete = as.integer(n_years == length(YEARS_SEQ)),
    missing_list = paste(setdiff(YEARS_SEQ, sort(unique(ano))), collapse = ", ")
  ) |>
  ungroup()
write_clean(qa_cov_tipo, file.path(qa_dir, "qa_hc_cobertura_por_tipo.csv"))

# ————————————————————————————————————————————————————————————————
# 5) Validaciones finales y escritura
# ————————————————————————————————————————————————————————————————
if (nrow(nat) == 0) abort("❌ No hay filas tras la limpieza de hechos conocidos.")
if (any(is.na(nat$valor))) abort("❌ Existen valores NA en 'valor' tras limpieza de hechos conocidos.")

nat <- nat |> arrange(ano, tipo) |> select(ano, tipo, valor)
write_clean(nat, fp_out)
message("✔ Hechos conocidos (TOTAL NACIONAL) limpios y guardados.")
