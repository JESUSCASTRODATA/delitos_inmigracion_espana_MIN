#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
###############################################################################
# 20_panel_nacional.R — Fusión del panel 2010–2023 (nivel nacional)
#
# Integra:
#  - det_tot (TOTAL Infracciones Penales, detenciones totales, conteo)
#  - det_ext (TOTAL Infracciones Penales, detenciones de extranjeros, conteo)
#  - share_extranjeros (proporción 0–1) y pct_extranjeros_poblacion (0–100)
#  - arope, paro_15_29, pib_pc_real
#  - poblacion_total, poblacion_15_29 (agregado o suma de bins)
#  - tasas/logs desde: data/processed/tasas/tasas_transformadas.csv
#
# Salida:
#   data/processed/panel_nacional_2010_2023.csv  (14 filas: 2010…2023)
###############################################################################

suppressPackageStartupMessages({
  library(here); library(readr); library(dplyr); library(janitor)
})

# -----------------------------------------------------------------------------
# Dependencias del proyecto (00_utils_limpieza.R). Si no existe, define fallbacks
# -----------------------------------------------------------------------------
utils_path <- here::here("scripts","00_utils_limpieza.R")
if (file.exists(utils_path)) source(utils_path, local = TRUE)

# Fallbacks mínimos
if (!exists("here_path")) here_path <- function(...) here::here(...)
if (!exists("dir_create")) dir_create <- function(x){dir.create(x, recursive = TRUE, showWarnings = FALSE); x}
if (!exists("write_clean")) write_clean <- function(df, path){dir_create(dirname(path)); readr::write_csv(df, path)}

# -----------------------------------------------------------------------------
# Helpers robustos anti-duplicados
# -----------------------------------------------------------------------------
ensure_cols <- function(df, cols){ for (nm in cols) if (!nm %in% names(df)) df[[nm]] <- NA_real_; df }

best_per_year <- function(df, label="df"){
  if (is.null(df)) return(NULL)
  if (!"ano" %in% names(df)) return(df)
  df <- df %>% mutate(ano = as.integer(ano))
  if (!nrow(df)) return(df)
  if (anyDuplicated(df$ano)) {
    message("⚠ Duplicados por 'ano' en ", label, "; se conserva la fila con más datos no-NA.")
    df <- df %>%
      mutate(.nnas = rowSums(across(everything(), ~ !is.na(.x)))) %>%
      arrange(ano, dplyr::desc(.nnas)) %>%
      group_by(ano) %>% slice_head(n = 1) %>% ungroup() %>% select(-.nnas)
  }
  df
}

join_one_to_one <- function(x, y, by = "ano", name = "<join>"){
  if (is.null(y)) { message("⊘ Omitido join: ", name, " (dataset ausente)"); return(x) }
  x1 <- best_per_year(x,  paste0("panel_pre_", name))
  y1 <- best_per_year(y,  name)
  out <- dplyr::left_join(x1, y1, by = by)
  if (anyDuplicated(out[[by]])) {
    message("⚠ Tras unir ", name, " quedaron duplicados por '", by, "'. Se colapsa por mejor fila.")
    out <- best_per_year(out, paste0("panel_post_", name))
  }
  out
}

# Secuencia de años del estudio
YEARS_SEQ <- 2010:2023

# Rutas de entrada/salida
out_panel <- here_path("data","processed","panel_nacional_2010_2023.csv")
qa_dir    <- here_path("output","tables")

fp <- list(
  det_tot        = here_path("data","processed","detenciones_totales_total_nacional.csv"),
  det_ext        = here_path("data","processed","detenciones_extranjeros_total_nacional.csv"),
  pct_inm        = here_path("data","processed","pct_extranjeros_poblacion.csv"),
  arope          = here_path("data","processed","arope_total.csv"),
  paro_15_29     = here_path("data","processed","paro_15_29_anual.csv"),
  pib            = here_path("data","processed","pib_pc_real_es.csv"),
  pop_tot        = here_path("data","processed","poblacion_total_nacional.csv"),
  pop_15_29      = here_path("data","processed","poblacion_15_29_anual.csv"),
  pop_15_29_bins = here_path("data","processed","poblacion_15_29_bins_anual.csv"),
  tasas_tr       = here_path("data","processed","tasas","tasas_transformadas.csv")
)

# -----------------------------------------------------------------------------
# 1) Base por años
# -----------------------------------------------------------------------------
base <- tibble::tibble(ano = YEARS_SEQ)

# -----------------------------------------------------------------------------
# 2) Carga de insumos (ya reducidos a una fila por año)
# -----------------------------------------------------------------------------
read_csv_clean <- if (exists("read_csv_clean")) read_csv_clean else function(path){
  if (!file.exists(path)) { message("• No existe: ", path); return(NULL) }
  suppressMessages(readr::read_csv(path, show_col_types = FALSE)) |> janitor::clean_names()
}

read_det_total <- function(path, out_name){
  df <- read_csv_clean(path); if (is.null(df)) return(NULL)
  nm <- names(df)
  cand <- c("valor","total","conteo")
  col_val <- intersect(nm, cand)
  if (length(col_val) == 0) {
    numc <- nm[sapply(df, is.numeric)]; numc <- setdiff(numc, c("ano"))
    if (length(numc)) col_val <- numc[1] else return(NULL)
  } else col_val <- col_val[1]
  df <- df |> dplyr::select(dplyr::any_of(c("ano", col_val))) |> dplyr::mutate(ano = as.integer(ano))
  df <- best_per_year(df, out_name)
  dplyr::rename(df, !!out_name := dplyr::all_of(col_val))
}

det_tot <- read_det_total(fp$det_tot, "det_tot")
det_ext <- read_det_total(fp$det_ext, "det_ext")

# Tasas transformadas

tasas <- read_csv_clean(fp$tasas_tr)
if (!is.null(tasas)) {
  vars_keep <- c(
    "ano","share_extranjeros","hc_rate","he_rate","det_tot_rate","det_ext_rate",
    "arope","paro_15_29","pib_pc_real",
    "l_hc_rate","l_he_rate","l_det_tot_rate","l_det_ext_rate","l_pib_pc_real",
    "covid_2020","covid_2021","covid_2020_2021"
  )
  tasas <- tasas %>% mutate(ano = as.integer(ano)) %>% select(any_of(vars_keep))
  tasas <- best_per_year(tasas, "tasas_transformadas")
}

pop_tot  <- read_csv_clean(fp$pop_tot)  %>% dplyr::select(dplyr::any_of(c("ano","poblacion_total")))
pct_inm  <- read_csv_clean(fp$pct_inm)  %>% dplyr::select(dplyr::any_of(c("ano","pct_extranjeros_poblacion")))
if (!is.null(pop_tot)) pop_tot <- best_per_year(pop_tot, "poblacion_total")
if (!is.null(pct_inm)) pct_inm <- best_per_year(pct_inm, "pct_extranjeros_poblacion")

# Población 15–29
if (file.exists(fp$pop_15_29)) {
  pop_15_29 <- read_csv_clean(fp$pop_15_29) %>% dplyr::select(dplyr::any_of(c("ano","poblacion_15_29")))
  pop_15_29 <- best_per_year(pop_15_29, "poblacion_15_29")
} else if (file.exists(fp$pop_15_29_bins)) {
  pop_15_29 <- read_csv_clean(fp$pop_15_29_bins) %>%
    dplyr::select(dplyr::any_of(c("ano","edad_bin","poblacion"))) %>%
    dplyr::group_by(ano) %>% dplyr::summarise(poblacion_15_29 = sum(poblacion, na.rm = TRUE), .groups = "drop")
} else {
  pop_15_29 <- NULL
  message("• No se encontró población 15–29 (ni agregado ni bins).")
}

# Backups robustos
arope_bk <- read_csv_clean(fp$arope)
if (!is.null(arope_bk)) {
  arope_bk <- janitor::clean_names(arope_bk)
  if (!"arope" %in% names(arope_bk)) {
    cand <- intersect(names(arope_bk), c("arope","total","valor","porcentaje","rate","porc"))
    arope_bk <- if (length(cand)) dplyr::mutate(arope_bk, arope = .data[[cand[1]]]) else dplyr::mutate(arope_bk, arope = NA_real_)
  }
  arope_bk <- arope_bk %>% dplyr::select(dplyr::any_of(c("ano","arope"))) %>% dplyr::mutate(ano = as.integer(ano)) %>% dplyr::rename(arope_backup = arope)
  arope_bk <- best_per_year(arope_bk, "arope_backup")
}

paro_bk <- read_csv_clean(fp$paro_15_29)
if (!is.null(paro_bk)) {
  paro_bk <- janitor::clean_names(paro_bk)
  if (!"paro_15_29" %in% names(paro_bk)) {
    cand <- intersect(names(paro_bk), c("paro_15_29","total","valor","porcentaje","rate","porc"))
    paro_bk <- if (length(cand)) dplyr::mutate(paro_bk, paro_15_29 = .data[[cand[1]]]) else dplyr::mutate(paro_bk, paro_15_29 = NA_real_)
  }
  paro_bk <- paro_bk %>% dplyr::select(dplyr::any_of(c("ano","paro_15_29"))) %>% dplyr::mutate(ano = as.integer(ano)) %>% dplyr::rename(paro_15_29_backup = paro_15_29)
  paro_bk <- best_per_year(paro_bk, "paro_15_29_backup")
}

pib_bk <- read_csv_clean(fp$pib)
if (!is.null(pib_bk)) {
  pib_bk <- janitor::clean_names(pib_bk)
  if (!"pib_pc_real" %in% names(pib_bk)) {
    cand <- intersect(names(pib_bk), c("pib_pc_real","pib_real_pc","pib_volumen_pc","pib_pc","valor","total"))
    pib_bk <- if (length(cand)) dplyr::mutate(pib_bk, pib_pc_real = .data[[cand[1]]]) else dplyr::mutate(pib_bk, pib_pc_real = NA_real_)
  }
  pib_bk <- pib_bk %>% dplyr::select(dplyr::any_of(c("ano","pib_pc_real"))) %>% dplyr::mutate(ano = as.integer(ano)) %>% dplyr::rename(pib_pc_real_backup = pib_pc_real)
  pib_bk <- best_per_year(pib_bk, "pib_pc_real_backup")
}

# -----------------------------------------------------------------------------
# 3) Fusión secuencial (cada join fuerza 1-fila-por-año)
# -----------------------------------------------------------------------------
panel <- base %>%
  join_one_to_one(det_tot,   "ano", name = "det_tot") %>%
  join_one_to_one(det_ext,   "ano", name = "det_ext") %>%
  join_one_to_one(tasas,     "ano", name = "tasas_transformadas") %>%
  join_one_to_one(pct_inm,   "ano", name = "pct_extranjeros_poblacion") %>%
  join_one_to_one(arope_bk,  "ano", name = "arope_backup") %>%
  join_one_to_one(paro_bk,   "ano", name = "paro_15_29_backup") %>%
  join_one_to_one(pib_bk,    "ano", name = "pib_pc_real_backup") %>%
  join_one_to_one(pop_tot,   "ano", name = "poblacion_total") %>%
  join_one_to_one(pop_15_29, "ano", name = "poblacion_15_29")

# -----------------------------------------------------------------------------
# 4) Normalizaciones y derivaciones
# -----------------------------------------------------------------------------
panel <- ensure_cols(panel, c("arope","paro_15_29","pib_pc_real","share_extranjeros","pct_extranjeros_poblacion"))

panel <- panel %>%
  mutate(
    arope       = dplyr::coalesce(.data$arope, .data$arope_backup),
    paro_15_29  = dplyr::coalesce(.data$paro_15_29, .data$paro_15_29_backup),
    pib_pc_real = dplyr::coalesce(.data$pib_pc_real, .data$pib_pc_real_backup)
  ) %>%
  select(-any_of(c("arope_backup","paro_15_29_backup","pib_pc_real_backup")))

# share/pct coherentes
if (any(panel$share_extranjeros > 1, na.rm = TRUE)) {
  message("ℹ 'share_extranjeros' parece estar en % (>1); se divide por 100.")
  panel <- panel %>% mutate(share_extranjeros = share_extranjeros/100)
}
if (all(is.na(panel$pct_extranjeros_poblacion)) && any(!is.na(panel$share_extranjeros))) {
  panel <- panel %>% mutate(pct_extranjeros_poblacion = 100 * share_extranjeros)
  message("ℹ 'pct_extranjeros_poblacion' derivado de 'share_extranjeros'*100.")
}
if (all(is.na(panel$share_extranjeros)) && any(!is.na(panel$pct_extranjeros_poblacion))) {
  panel <- panel %>% mutate(share_extranjeros = pct_extranjeros_poblacion/100)
  message("ℹ 'share_extranjeros' derivado de 'pct_extranjeros_poblacion'/100.")
}

# -----------------------------------------------------------------------------
# 5) Selección final, QA básica y escritura
# -----------------------------------------------------------------------------
expected <- c(
  "ano",
  "det_tot","det_ext","det_tot_rate","det_ext_rate",
  "share_extranjeros","pct_extranjeros_poblacion",
  "arope","paro_15_29","pib_pc_real",
  "poblacion_total","poblacion_15_29",
  "l_hc_rate","l_he_rate","l_det_tot_rate","l_det_ext_rate","l_pib_pc_real",
  "covid_2020","covid_2021","covid_2020_2021"
)

for (nm in setdiff(expected, names(panel))) panel[[nm]] <- NA_real_

# Diagnóstico si el tamaño no cuadra
if (nrow(panel) != length(YEARS_SEQ)) {
  message("❗ Tamaño inesperado del panel: ", nrow(panel), " vs ", length(YEARS_SEQ))
  cnt <- panel %>% count(ano, name = "n") %>% arrange(ano)
  print(cnt, n = Inf)
  # Último intento: forzar 1 fila por año (elige la fila con más no-NA)
  panel <- best_per_year(panel, "panel_final")
}

panel <- panel %>%
  dplyr::mutate(ano = as.integer(ano)) %>%
  dplyr::select(all_of(expected)) %>%
  dplyr::arrange(ano)

stopifnot(nrow(panel) == length(YEARS_SEQ))
stopifnot(identical(panel$ano, as.integer(YEARS_SEQ)))

write_clean(panel, out_panel)
message("✔ Panel nacional escrito: ", out_panel)



