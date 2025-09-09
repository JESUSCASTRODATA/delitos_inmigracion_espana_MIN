#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
###############################################################################
# 18_stationarity_checks.R — Pruebas de estacionariedad (ADF/PP) por ventana.
#
# Autor: JESUS CASTRO
# Fecha: 2025-09-04
###############################################################################
suppressPackageStartupMessages({
  library(readr); library(dplyr); library(tidyr); library(here)
  library(urca)
})

ensure_dir <- function(path) if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
ensure_dir(here::here("output/tables"))

infile <- here::here("data/processed/series.csv")
stopifnot(file.exists(infile))
df <- readr::read_csv(infile, show_col_types = FALSE)

windows <- list(
  "2010_2023"      = function(d) d,
  "2010_2019_pre"  = function(d) dplyr::filter(d, ano <= 2019),
  "2020_2023_post" = function(d) dplyr::filter(d, ano >= 2020),
  "sin_covid"      = function(d) {
    if ("covid_2020_2021" %in% names(d)) dplyr::filter(d, !covid_2020_2021 %in% c(1, TRUE)) else d
  }
)

vars <- intersect(c(
  "det_tot_rate","det_ext_rate","share_extranjeros","pct_extranjeros_poblacion",
  "arope","paro_15_29","pib_pc_real"
), names(df))

rows <- list()

for (w in names(windows)) {
  d <- windows[[w]](df) %>% dplyr::arrange(ano)
  for (v in vars) {
    x <- d[[v]]
    x <- x[is.finite(x)]
    if (length(x) < 8) next
    
    adf <- try(urca::ur.df(x, type = "drift", lags = 1), silent = TRUE)
    if (!inherits(adf, "try-error")) {
      stat <- as.numeric(adf@teststat[1])  # tau1
      cv5  <- as.numeric(adf@cval[1,"5pct"])
      rows[[length(rows)+1]] <- tibble::tibble(
        window = w, var = v, test = "ADF(drift,lag=1)",
        stat = stat, cv_5pct = cv5, stationary_5pct = (stat < cv5)
      )
    }
    pp <- try(urca::ur.pp(x, type = "Z-tau", model = "constant", lags = "short"), silent = TRUE)
    if (!inherits(pp, "try-error")) {
      stat <- as.numeric(pp@teststat)
      cv5  <- as.numeric(pp@cval["5pct"])
      rows[[length(rows)+1]] <- tibble::tibble(
        window = w, var = v, test = "PP(Z-tau,const)",
        stat = stat, cv_5pct = cv5, stationary_5pct = (stat < cv5)
      )
    }
  }
}

if (length(rows)) {
  out <- dplyr::bind_rows(rows)
  readr::write_csv(out, here::here("output/tables","18_stationarity.csv"))
  message("✓ Guardado output/tables/18_stationarity.csv")
} else {
  message("⚠️ No hay suficientes observaciones para las pruebas de estacionariedad.")
}