#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
###############################################################################
# 02_config_diccionarios_y_excepciones.R
# Objetivo: crear/actualizar configuraciones a partir de diagnostics/uniques_*
#
# Salidas requeridas:
#   - config/map_tipologias.csv     (raw_tipo, propuesta_tipo)
#   - config/map_regiones.csv       (raw_region, propuesta_region)
#   - config/qa_excepciones_he_vs_hc.csv  (ano, tipo) ← desde qa_sugerencia_whitelist.csv si existe
#
# QA: formato, duplicados, minúsculas/ASCII en propuestas.
#
# Notas:
#   - Lee diagnostics/uniques_*.csv para identificar columnas candidatas.
#   - Para obtener el universo completo de valores, re-lee el RAW asociado
#     (data/raw/<dataset>.csv) y extrae valores únicos de columnas de región y tipología.
#   - Si ya existen config/*.csv, hace merge incremental y de-duplica.
###############################################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(stringr)
  library(janitor)
  library(purrr)
  library(here)
})

# ————————————————————————————————————————————————————————————————
# Utilidades del proyecto
# ————————————————————————————————————————————————————————————————
source_if <- function(path) { if (file.exists(path)) source(path, local = TRUE) }
source_if(here::here("R", "00_utils_limpieza.R"))
source_if(here::here("scripts", "00_utils_limpieza.R"))

# Fallback mínimos si 00_utils no se cargó
if (!exists("abort")) abort <- function(...) stop(paste0(...), call. = FALSE)
if (!exists("write_clean")) write_clean <- function(df, path, na = ""){ dir.create(dirname(path), TRUE, TRUE); readr::write_csv(df, path, na = na); message("Escrito: ", normalizePath(path, winslash = "/")); invisible(path) }
if (!exists("read_raw")) read_raw <- function(path){ readr::read_delim(path, delim = ";", col_types = readr::cols(.default = readr::col_character()), show_col_types = FALSE) |> janitor::clean_names() }

normalize_text <- function(x) {
  x <- stringr::str_trim(tolower(as.character(x)))
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  x <- stringr::str_squish(x)
  x
}

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

in_diag   <- here_path("diagnostics")
raw_dir   <- here_path("data", "raw")
config_dir<- here_path("config")
qa_dir    <- here_path("output", "tables")

if (!dir.exists(in_diag)) abort("No existe carpeta diagnostics/: ", in_diag)
if (!dir.exists(raw_dir)) abort("No existe carpeta data/raw/: ", raw_dir)

dir.create(config_dir, recursive = TRUE, showWarnings = FALSE)

glob_uniques <- list.files(in_diag, pattern = "^uniques_.*\\.csv$", full.names = TRUE)
if (!length(glob_uniques)) abort("No hay diagnostics/uniques_*.csv. Ejecuta 01_validacion_raw.R primero.")

# ————————————————————————————————————————————————————————————————
# 1) Identificar columnas de región y tipología por dataset
# ————————————————————————————————————————————————————————————————
pattern_region <- "comun|ccaa|autono|territ|ambito|ámbito|region|prov|municip"
pattern_tipo   <- "tipolog|delit|infracc|\\btipo\\b"

# Dado el archivo uniques_X.csv, intentamos leer data/raw/X.csv
raw_fp_from_uniques <- function(uni_path) {
  base <- gsub("^uniques_", "", basename(uni_path))
  base <- gsub("\\.csv$", "", base)
  file.path(raw_dir, paste0(base, ".csv"))
}

collect_uniques <- function(fp_raw) {
  if (!file.exists(fp_raw)) return(list(regiones = character(), tipos = character()))
  df <- read_raw(fp_raw)
  nms <- names(df)
  cols_region <- nms[grepl(pattern_region, nms, ignore.case = TRUE)]
  cols_tipo   <- nms[grepl(pattern_tipo,   nms, ignore.case = TRUE)]
  
  vals_region <- c()
  vals_tipo   <- c()
  if (length(cols_region)) {
    vals_region <- df[, cols_region, drop = FALSE] |>
      tidyr::pivot_longer(everything(), names_to = "col", values_to = "val") |>
      dplyr::pull(val) |> unique() |> na.omit() |> as.character()
  }
  if (length(cols_tipo)) {
    vals_tipo <- df[, cols_tipo, drop = FALSE] |>
      tidyr::pivot_longer(everything(), names_to = "col", values_to = "val") |>
      dplyr::pull(val) |> unique() |> na.omit() |> as.character()
  }
  list(regiones = vals_region, tipos = vals_tipo)
}

all_regions <- character()
all_tipos   <- character()

for (u in glob_uniques) {
  fp_raw <- raw_fp_from_uniques(u)
  out <- try(collect_uniques(fp_raw), silent = TRUE)
  if (inherits(out, "try-error")) next
  all_regions <- c(all_regions, out$regiones)
  all_tipos   <- c(all_tipos,   out$tipos)
}

all_regions <- unique(na.omit(all_regions))
all_tipos   <- unique(na.omit(all_tipos))

# Filtrar ruidos típicos
is_noise <- function(x) {
  x0 <- tolower(trimws(as.character(x)))
  x0 == "" | x0 %in% c("na","n/a","-","—","sd","s/d","desconocido","desconocida")
}
all_regions <- all_regions[!is_noise(all_regions)]
all_tipos   <- all_tipos[!is_noise(all_tipos)]

# ————————————————————————————————————————————————————————————————
# 2) Propuestas de normalización (ASCII/minúsculas)
# ————————————————————————————————————————————————————————————————
map_regiones <- tibble::tibble(
  raw_region = sort(unique(all_regions)),
  propuesta_region = normalize_text(raw_region)
) |>
  dplyr::arrange(raw_region)

map_tipologias <- tibble::tibble(
  raw_tipo = sort(unique(all_tipos)),
  propuesta_tipo = normalize_text(raw_tipo)
) |>
  dplyr::arrange(raw_tipo)

# ————————————————————————————————————————————————————————————————
# 3) Merge con configuraciones existentes (si las hay)
# ————————————————————————————————————————————————————————————————
fp_map_reg <- here_path("config", "map_regiones.csv")
fp_map_tip <- here_path("config", "map_tipologias.csv")

if (file.exists(fp_map_reg)) {
  old_reg <- readr::read_csv(fp_map_reg, show_col_types = FALSE) |> janitor::clean_names()
  # admitir viejos nombres de columnas
  colnames(old_reg) <- sub("^region$", "raw_region", colnames(old_reg))
  colnames(old_reg) <- sub("^propuesta$", "propuesta_region", colnames(old_reg))
  if (!all(c("raw_region","propuesta_region") %in% names(old_reg))) {
    warning("map_regiones.csv existente con columnas inesperadas; se conservarán nuevas columnas estandar.")
  } else {
    map_regiones <- dplyr::bind_rows(old_reg |> dplyr::select(raw_region, propuesta_region), map_regiones) |>
      dplyr::distinct(raw_region, .keep_all = TRUE)
  }
}

if (file.exists(fp_map_tip)) {
  old_tip <- readr::read_csv(fp_map_tip, show_col_types = FALSE) |> janitor::clean_names()
  colnames(old_tip) <- sub("^tipo$", "raw_tipo", colnames(old_tip))
  colnames(old_tip) <- sub("^propuesta$", "propuesta_tipo", colnames(old_tip))
  if (!all(c("raw_tipo","propuesta_tipo") %in% names(old_tip))) {
    warning("map_tipologias.csv existente con columnas inesperadas; se conservarán nuevas columnas estandar.")
  } else {
    map_tipologias <- dplyr::bind_rows(old_tip |> dplyr::select(raw_tipo, propuesta_tipo), map_tipologias) |>
      dplyr::distinct(raw_tipo, .keep_all = TRUE)
  }
}

# ————————————————————————————————————————————————————————————————
# 4) QA de duplicados y normalización
# ————————————————————————————————————————————————————————————————
# Duplicados por propuesta (varios raw → misma propuesta)
qa_dup_reg <- map_regiones |>
  dplyr::count(propuesta_region, name = "n") |>
  dplyr::filter(n > 1)
qa_dup_tip <- map_tipologias |>
  dplyr::count(propuesta_tipo, name = "n") |>
  dplyr::filter(n > 1)

# Guardar QA (informativo)
qa_out_dir <- here_path("output", "tables")
dir.create(qa_out_dir, recursive = TRUE, showWarnings = FALSE)
if (nrow(qa_dup_reg) > 0) write_clean(qa_dup_reg, file.path(qa_out_dir, "qa_map_regiones_duplicados.csv"))
if (nrow(qa_dup_tip) > 0) write_clean(qa_dup_tip, file.path(qa_out_dir, "qa_map_tipologias_duplicados.csv"))

# ————————————————————————————————————————————————————————————————
# 5) Escribir configuraciones
# ————————————————————————————————————————————————————————————————
map_regiones |>
  dplyr::arrange(propuesta_region, raw_region) |>
  write_clean(fp_map_reg)

map_tipologias |>
  dplyr::arrange(propuesta_tipo, raw_tipo) |>
  write_clean(fp_map_tip)

message("✔ map_regiones.csv y map_tipologias.csv actualizados en config/.")

# ————————————————————————————————————————————————————————————————
# 6) Construir qa_excepciones_he_vs_hc.csv a partir de sugerencias
# ————————————————————————————————————————————————————————————————
fp_sug <- here_path("output", "tables", "qa_sugerencia_whitelist.csv")
fp_wl  <- here_path("config", "qa_excepciones_he_vs_hc.csv")

wl_new <- tibble::tibble(ano = integer(), tipo = character())
if (file.exists(fp_sug)) {
  sug <- readr::read_csv(fp_sug, show_col_types = FALSE) |> janitor::clean_names()
  if (all(c("ano","tipo") %in% names(sug))) {
    wl_new <- sug |> dplyr::select(ano, tipo) |> dplyr::mutate(ano = as.integer(ano), tipo = as.character(tipo)) |> dplyr::distinct()
  }
}

if (file.exists(fp_wl)) {
  wl_old <- readr::read_csv(fp_wl, show_col_types = FALSE) |> janitor::clean_names()
  if (all(c("ano","tipo") %in% names(wl_old))) {
    wl_old <- wl_old |>
      dplyr::mutate(
        ano = suppressWarnings(as.integer(extract_year(ano))),
        tipo = as.character(tipo)
      ) |>
      dplyr::filter(!is.na(ano), nzchar(tipo)) |>
      dplyr::distinct(ano, tipo)
    wl_new <- dplyr::bind_rows(wl_old, wl_new) |>
      dplyr::distinct(ano, tipo)
  }
}


# Escribe aunque esté vacío (mantener cabeceras)
wl_new |>
  dplyr::arrange(ano, tipo) |>
  write_clean(fp_wl)

message("✔ qa_excepciones_he_vs_hc.csv actualizado en config/.")

# Fin
