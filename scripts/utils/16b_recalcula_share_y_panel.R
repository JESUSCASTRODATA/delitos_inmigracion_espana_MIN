#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
###############################################################################
# 16b_recalcula_share_y_panel.R
#
# Recalcula el porcentaje de detenciones de extranjeros (share_extranjeros)
# a partir de los totales ya procesados y, si existe, refresca el panel nacional.
#
# Entradas (data/processed/):
#   - detenciones_extranjeros_total.csv    (ano, det_ext)
#   - detenciones_totales_total.csv        (ano, det_tot)
#   - (opcional) panel_nacional_2010_2023.csv (si existe)
# Salidas:
#   - data/processed/detenciones_share_extranjeros.csv (ano, share_extranjeros)
#   - (si existe panel) panel_nacional_2010_2023.csv con share actualizado
# QA:
#   - output/tables/qa_share_recalc_fuera_0_100.csv
#   - output/tables/qa_det_ext_mayor_que_tot_recalc.csv
#   - output/tables/qa_share_recalc_cobertura.csv
###############################################################################

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(janitor); library(here)
})

# ————————————————————————————————————————————————————————————————
# Utils del proyecto
# ————————————————————————————————————————————————————————————————
source_if <- function(path) { if (file.exists(path)) source(path, local = TRUE) }
source_if(here::here("R", "00_utils_limpieza.R"))
source_if(here::here("scripts", "00_utils_limpieza.R"))

# Fallbacks mínimos
if (!exists("abort")) abort <- function(...) stop(paste0(...), call. = FALSE)
if (!exists("write_clean")) write_clean <- function(df, path, na = ""){ dir.create(dirname(path), TRUE, TRUE); readr::write_csv(df, path, na = na); message("Escrito: ", normalizePath(path, winslash = "/")); invisible(path) }
if (!exists("root_init")) root_init <- function(caller = NULL, project_root_hint = NULL){ message("ROOT:16b -> ", getwd()) }

root_init("16b_recalcula_share_y_panel.R")

YEAR_MIN <- 2010L; YEAR_MAX <- 2023L; YEARS_SEQ <- YEAR_MIN:YEAR_MAX

# ————————————————————————————————————————————————————————————————
# Rutas
# ————————————————————————————————————————————————————————————————
if (requireNamespace("here", quietly = TRUE)) {
  here_path <- function(...) here::here(...)
} else {
  base_root <- getwd(); here_path <- function(...) file.path(base_root, ...)
}

p_ext   <- here_path("data","processed","detenciones_extranjeros_total.csv")
p_tot   <- here_path("data","processed","detenciones_totales_total.csv")
p_share <- here_path("data","processed","detenciones_share_extranjeros.csv")
p_panel <- here_path("data","processed","panel_nacional_2010_2023.csv")
qa_dir  <- here_path("output","tables")
dir.create(qa_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(p_ext)) abort("Falta: ", p_ext)
if (!file.exists(p_tot)) abort("Falta: ", p_tot)

# ————————————————————————————————————————————————————————————————
# Carga y sanitización
# ————————————————————————————————————————————————————————————————
ext <- readr::read_csv(p_ext, show_col_types = FALSE) |> clean_names()
# admitir columnas alternativas: valor / det_ext
if (!("det_ext" %in% names(ext))) {
  if ("valor" %in% names(ext)) names(ext)[names(ext)=="valor"] <- "det_ext"
}
stopifnot(all(c("ano","det_ext") %in% names(ext)))

# agregrar por seguridad si hay duplicados de año
ext <- ext |> mutate(ano = as.integer(ano)) |>
  group_by(ano) |> summarise(det_ext = sum(as.numeric(det_ext), na.rm = TRUE), .groups = "drop")

TOT <- readr::read_csv(p_tot, show_col_types = FALSE) |> clean_names()
if (!("det_tot" %in% names(TOT))) {
  if ("valor" %in% names(TOT)) names(TOT)[names(TOT)=="valor"] <- "det_tot"
}
stopifnot(all(c("ano","det_tot") %in% names(TOT)))
TOT <- TOT |> mutate(ano = as.integer(ano)) |>
  group_by(ano) |> summarise(det_tot = sum(as.numeric(det_tot), na.rm = TRUE), .groups = "drop")

# ————————————————————————————————————————————————————————————————
# Share = 100 * det_ext / det_tot
# ————————————————————————————————————————————————————————————————
share <- full_join(TOT, ext, by = "ano") |>
  mutate(share_extranjeros = ifelse(!is.na(det_tot) & det_tot > 0 & !is.na(det_ext),
                                    100 * det_ext / det_tot, NA_real_)) |>
  arrange(ano) |>
  select(ano, share_extranjeros)

# QA: rango y cobertura
qa_share <- share |>
  mutate(flag_fuera = is.na(share_extranjeros) | share_extranjeros < 0 | share_extranjeros > 100)
if (any(qa_share$flag_fuera)) write_clean(qa_share, file.path(qa_dir, "qa_share_recalc_fuera_0_100.csv"))

qa_cons <- full_join(ext, TOT, by = "ano") |>
  mutate(viol = det_ext > det_tot, diff = det_tot - det_ext) |>
  arrange(ano)
if (any(qa_cons$viol, na.rm = TRUE)) write_clean(qa_cons, file.path(qa_dir, "qa_det_ext_mayor_que_tot_recalc.csv"))

present <- sort(unique(share$ano)); missing <- setdiff(YEARS_SEQ, present)
qa_cov <- tibble::tibble(
  variable = "share_extranjeros",
  years_min = ifelse(length(present) > 0, min(present), NA_integer_),
  years_max = ifelse(length(present) > 0, max(present), NA_integer_),
  n_years = length(present),
  n_missing = length(missing),
  missing_list = paste(missing, collapse = ", ")
)
write_clean(qa_cov, file.path(qa_dir, "qa_share_recalc_cobertura.csv"))

# Guardar share
write_clean(share, p_share)

# ————————————————————————————————————————————————————————————————
# Refrescar panel si existe
# ————————————————————————————————————————————————————————————————
if (file.exists(p_panel)) {
  panel <- readr::read_csv(p_panel, show_col_types = FALSE) |> clean_names()
  if ("share_extranjeros" %in% names(panel)) panel <- dplyr::select(panel, -share_extranjeros)
  panel <- panel |>
    mutate(ano = as.integer(ano)) |>
    left_join(share, by = "ano") |>
    arrange(ano)
  write_clean(panel, p_panel)
  message("✅ Recalculado y actualizado panel: ", p_panel)
} else {
  message("ℹ Panel no encontrado (", p_panel, "). Solo se ha recalculado el share.")
}

message("✔ Share recalculado en: ", p_share)
