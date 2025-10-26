#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
################################################################################
# 19_transformaciones.R — Panel anual unificado + transformaciones
# Proyecto : Delitos e Inmigración en España (2010–2023)
# Autor    : Jesús Castro · JESUSCASTRODATA
# Licencia : MIT (código) · CC BY 4.0 (docs/figuras)
#
# ENTRA (data/processed/)
#   - poblacion_total_nacional.csv                  (ano, poblacion_total)
#   - pct_extranjeros_poblacion.csv                 (ano, pct_extranjeros)   [0..1]
#   - hechos_conocidos_total_nacional.csv          (ano, total)
#   - hechos_esclarecidos_total_nacional.csv       (ano, tipo, he)          [se suma a total]
#   - detenciones_totales_total.csv                (ano, det_tot)
#   - detenciones_extranjeros_total.csv            (ano, det_ext)
#   - detenciones_share_extranjeros.csv            (ano, share_extranjeros)  [%]
#   - pib_pc_real_es.csv                           (ano, pib_pc_real)
#
# SALE
#   - data/processed/core_indicadores.csv
#   - data/processed/core_indicadores_derivadas.csv
#
# QA
#   - output/tables/19_qa_cobertura.csv
#   - output/tables/19_qa_na_y_rangos.csv
################################################################################

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(tidyr); library(janitor)
  library(stringr); library(here);  library(fs);   library(tibble)
})

# ------------------------- Config y Utils -----------------------------------
YEAR_MIN <- 2010L; YEAR_MAX <- 2023L; YEARS_SEQ <- YEAR_MIN:YEAR_MAX

# Utilidades del proyecto (read_raw, write_clean, load_series, etc.)
source(here::here("scripts","00_utils_limpieza.R"))
root_init("scripts/19_transformaciones.R")

msg  <- function(...) message("[19] ", paste0(...))
asrt <- function(cond, ...) { if (!isTRUE(cond)) stop(paste0(...), call. = FALSE) }

# ------------------------- Rutas --------------------------------------------
proc <- function(...) here::here("data","processed", ...)
qa_p <- function(...) here::here("output","tables", ...)

fp_pop_tot   <- proc("poblacion_total_nacional.csv")
fp_pct_ext   <- proc("pct_extranjeros_poblacion.csv")
fp_hc_tot    <- proc("hechos_conocidos_total_nacional.csv")
fp_he_tot    <- proc("hechos_esclarecidos_total_nacional.csv")
fp_det_tot   <- proc("detenciones_totales_total.csv")
fp_det_ext   <- proc("detenciones_extranjeros_total.csv")
fp_det_share <- proc("detenciones_share_extranjeros.csv")
fp_pib       <- proc("pib_pc_real_es.csv")

# Salidas
fp_core      <- proc("core_indicadores.csv")
fp_core_der  <- proc("core_indicadores_derivadas.csv")

# ------------------------- Lectura defensiva --------------------------------
# Población total
asrt(file.exists(fp_pop_tot),  "Falta: ", fp_pop_tot)
P <- read_csv(fp_pop_tot, show_col_types = FALSE) |> clean_names() |>
  transmute(ano = as.integer(ano), poblacion_total = as.numeric(poblacion_total))

# % extranjeros (0..1) — opcional
pct_ext <- if (file.exists(fp_pct_ext)) {
  read_csv(fp_pct_ext, show_col_types = FALSE) |> clean_names() |>
    transmute(ano = as.integer(ano), pct_extranjeros = as.numeric(pct_extranjeros))
} else {
  msg("No existe pct_extranjeros_poblacion.csv — se omitirá esta columna.")
  tibble(ano = integer(), pct_extranjeros = numeric())
}

# HC total
asrt(file.exists(fp_hc_tot),   "Falta: ", fp_hc_tot)
HC <- read_csv(fp_hc_tot, show_col_types = FALSE) |> clean_names() |>
  transmute(ano = as.integer(ano), hc_total = as.numeric(total))

# HE total (sumar por año, ya que viene por tipología)
asrt(file.exists(fp_he_tot),   "Falta: ", fp_he_tot)
HE <- read_csv(fp_he_tot, show_col_types = FALSE) |> clean_names() |>
  mutate(ano = as.integer(ano), he = as.numeric(he)) |>
  group_by(ano) |> summarise(he_total = sum(he, na.rm = TRUE), .groups = "drop")

# Detenciones — total y extranjeros
asrt(file.exists(fp_det_tot),  "Falta: ", fp_det_tot)
asrt(file.exists(fp_det_ext),  "Falta: ", fp_det_ext)
DET_TOT <- read_csv(fp_det_tot, show_col_types = FALSE) |> clean_names() |>
  transmute(ano = as.integer(ano), det_tot = as.numeric(det_tot))
DET_EXT <- read_csv(fp_det_ext, show_col_types = FALSE) |> clean_names() |>
  transmute(ano = as.integer(ano), det_ext = as.numeric(det_ext))

# Share extranjeros en detenciones (%) — opcional (si no, lo calculamos)
share_ext <- if (file.exists(fp_det_share)) {
  read_csv(fp_det_share, show_col_types = FALSE) |> clean_names() |>
    transmute(ano = as.integer(ano), share_extranjeros = as.numeric(share_extranjeros))
} else {
  tibble(ano = integer(), share_extranjeros = numeric())
}

# PIB pc real
asrt(file.exists(fp_pib), "Falta: ", fp_pib)
PIB <- read_csv(fp_pib, show_col_types = FALSE) |> clean_names() |>
  transmute(ano = as.integer(ano), pib_pc_real = as.numeric(pib_pc_real))

# ------------------------- Merge base anual ---------------------------------
panel <- P |>
  join_with_log(pct_ext, by = "ano", mode = "left") |>
  join_with_log(HC,      by = "ano", mode = "left") |>
  join_with_log(HE,      by = "ano", mode = "left") |>
  join_with_log(DET_TOT, by = "ano", mode = "left") |>
  join_with_log(DET_EXT, by = "ano", mode = "left") |>
  join_with_log(share_ext, by = "ano", mode = "left") |>
  join_with_log(PIB,     by = "ano", mode = "left") |>
  arrange(ano)

# Ventana
panel <- panel |> filter(dplyr::between(ano, YEAR_MIN, YEAR_MAX))

# Completa share si falta y hay det_ext/det_tot
panel <- panel |>
  mutate(
    share_extranjeros = dplyr::coalesce(
      share_extranjeros,
      ifelse(det_tot > 0, 100 * det_ext / det_tot, NA_real_)
    )
  )

# Tasas por 100k habitantes (si hay población)
panel <- panel |>
  mutate(
    tasa_hc_100k = ifelse(poblacion_total > 0, 1e5 * hc_total / poblacion_total, NA_real_),
    tasa_he_100k = ifelse(poblacion_total > 0, 1e5 * he_total / poblacion_total, NA_real_)
  )

# Guardamos el core “de nivel” antes de derivadas
write_clean(panel, fp_core)
msg("✓ Escrito core_indicadores.csv")

# ------------------------- Derivadas (logs, Δlog, lags) ---------------------
# util para log seguro
safelog <- function(x) ifelse(is.finite(x) & x > 0, log(x), NA_real_)

# Base: usar solo columnas principales
base <- panel |>
  select(ano, poblacion_total, pct_extranjeros, hc_total, he_total,
         det_tot, det_ext, share_extranjeros, pib_pc_real,
         tasa_hc_100k, tasa_he_100k)

# Logs (niveles) y Δlog (aproximación a %)
base <- base |>
  mutate(
    ln_pib_pc      = safelog(pib_pc_real),
    ln_hc          = safelog(hc_total),
    ln_he          = safelog(he_total),
    ln_det_tot     = safelog(det_tot),
    ln_det_ext     = safelog(det_ext),
    ln_tasa_hc     = safelog(tasa_hc_100k),
    ln_tasa_he     = safelog(tasa_he_100k),
    dln_pib_pc     = ln_pib_pc - dplyr::lag(ln_pib_pc),
    dln_hc         = ln_hc     - dplyr::lag(ln_hc),
    dln_he         = ln_he     - dplyr::lag(ln_he),
    dln_det_tot    = ln_det_tot - dplyr::lag(ln_det_tot),
    dln_det_ext    = ln_det_ext - dplyr::lag(ln_det_ext),
    dln_tasa_hc    = ln_tasa_hc - dplyr::lag(ln_tasa_hc),
    dln_tasa_he    = ln_tasa_he - dplyr::lag(ln_tasa_he),
    # Δ en % (ya tenemos share_extranjeros en % y pct_extranjeros en [0..1])
    d_share_ext    = share_extranjeros - dplyr::lag(share_extranjeros),
    d_pct_extran   = pct_extranjeros   - dplyr::lag(pct_extranjeros)
  )

# Lags T-1 clave (composición en t-1)
base <- base |>
  mutate(
    pct_extranjeros_l1 = dplyr::lag(pct_extranjeros, 1),
    share_extr_l1      = dplyr::lag(share_extranjeros, 1),
    tasa_hc_l1         = dplyr::lag(tasa_hc_100k, 1),
    tasa_he_l1         = dplyr::lag(tasa_he_100k, 1)
  )

# Controles derivados del PIB (YoY, índice base 2010 y z-score)
base_val_2010 <- panel$pib_pc_real[panel$ano == 2010]
base_val_2010 <- if (length(base_val_2010)) base_val_2010[1] else NA_real_

mu_pib <- mean(panel$pib_pc_real, na.rm = TRUE)
sd_pib <- stats::sd(panel$pib_pc_real, na.rm = TRUE)
base <- base |>
  mutate(
    pib_yoy      = pib_pc_real / dplyr::lag(pib_pc_real) - 1,
    pib_idx2010  = ifelse(!is.na(base_val_2010) & !is.na(pib_pc_real),
                          100 * pib_pc_real / base_val_2010, NA_real_),
    pib_z        = if (is.finite(sd_pib) && sd_pib > 0) (pib_pc_real - mu_pib)/sd_pib else NA_real_
  )

# ------------------------- QA mínimos ---------------------------------------
vars_qc <- c("poblacion_total","pct_extranjeros","hc_total","he_total",
             "det_tot","det_ext","share_extranjeros","tasa_hc_100k","tasa_he_100k")

n_obs_vec <- vapply(vars_qc, function(v) sum(is.finite(base[[v]])), integer(1))

qa_cov <- tibble(
  variable  = vars_qc,
  n_obs     = n_obs_vec,
  years_min = min(base$ano, na.rm = TRUE),
  years_max = max(base$ano, na.rm = TRUE),
  n_years   = dplyr::n_distinct(base$ano),
  n_expected= length(YEARS_SEQ)
)

qa_na_rng <- tibble(
  var = names(base),
  n_na = vapply(base, function(v) sum(is.na(v)), integer(1)),
  min  = vapply(base, function(v) suppressWarnings(min(v, na.rm = TRUE)), numeric(1)),
  max  = vapply(base, function(v) suppressWarnings(max(v, na.rm = TRUE)), numeric(1))
)

write_clean(qa_cov,    qa_p("19_qa_cobertura.csv"))
write_clean(qa_na_rng, qa_p("19_qa_na_y_rangos.csv"))

# ------------------------- Escritura final ----------------------------------
write_clean(base, fp_core_der)
msg("✅ Transformaciones listas: core_indicadores.csv + core_indicadores_derivadas.csv")
