#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
################################################################################
# 38_qc_stationarity_trends.R
#
# Objetivo:
#   - Pruebas de estacionariedad por serie (dataset + key): ADF y KPSS.
#   - Tendencia lineal (pendiente y p-valor) y Spearman (monotonia).
#   - (Opcional) Cambios de regimen con strucchange si esta instalado.
#
# Salidas (output/tables):
#   - qa38_stationarity_trends.csv
#   - qa38_breaks.csv (si se pueden estimar rupturas)
################################################################################

suppressPackageStartupMessages({
  library(here); library(readr); library(dplyr); library(tidyr); library(tibble)
  library(janitor); library(stringr); library(purrr)
})
safe_quit <- function(status = 0){
  # No cierres la sesión si estoy en RStudio o si NO_QUIT=1
  if (nzchar(Sys.getenv("RSTUDIO")) || Sys.getenv("NO_QUIT","0") == "1") {
    return(invisible(status))
  } else {
    quit(status = status)
  }
}


dir.create(here::here('output','tables'), recursive = TRUE, showWarnings = FALSE)

read_csv_auto <- function(fp){
  out <- tryCatch(suppressMessages(readr::read_csv(fp, show_col_types = FALSE)),
                  error = function(e) NULL)
  if (is.null(out)){
    out <- tryCatch(suppressMessages(readr::read_delim(fp, delim = ';', show_col_types = FALSE)),
                    error = function(e) NULL)
  }
  out
}

num_es <- function(x){
  readr::parse_number(as.character(x),
                      locale = readr::locale(decimal_mark = ',', grouping_mark = '.'))
}

# Normaliza datasets a largo
to_long <- function(df, name){
  if (is.null(df)) return(NULL)
  df <- janitor::clean_names(df)
  if (!('ano' %in% names(df))) return(NULL)
  val_candidates <- c('valor','poblacion_total','poblacion_espanola','poblacion_extranjera',
                      'arope','paro_15_29','pib_pc_real')
  vcol <- intersect(val_candidates, names(df))
  if (length(vcol)==0){
    nc <- setdiff(names(df), c('ano','tipo','tipologia','tipologia_penal','sexo'))
    nnum <- nc[vapply(nc, function(c) is.numeric(df[[c]]) || is.integer(df[[c]]), logical(1))]
    vcol <- if (length(nnum)>0) nnum[1] else if (length(nc)>0) nc[1] else return(NULL)
  } else { vcol <- vcol[1] }
  key <- intersect(names(df), c('tipo','tipologia','tipologia_penal','sexo'))
  val <- num_es(df[[vcol]])
  if (length(key)==0){
    tibble(dataset = basename(name), ano = as.integer(df$ano), key = 'TOTAL', value = val)
  } else {
    tibble(dataset = basename(name), ano = as.integer(df$ano), key = do.call(paste, c(df[key], sep='|')), value = val)
  }
}

files <- list.files(here::here('data','processed'), pattern='\\.csv$', recursive=TRUE, full.names=TRUE)
long_list <- purrr::imap(files, ~ to_long(read_csv_auto(.x), .x))
long <- dplyr::bind_rows(long_list[!vapply(long_list, is.null, logical(1))]) %>%
  dplyr::filter(!is.na(ano), is.finite(ano))

if (nrow(long)==0){
  readr::write_csv(tibble(info='No hay series temporales legibles en data/processed'), here::here('output','tables','qa38_stationarity_trends.csv'))
  quit(status = 0)
}

# Comprobar paquetes de tests
has_tseries <- requireNamespace('tseries', quietly = TRUE)
has_strucchange <- requireNamespace('strucchange', quietly = TRUE)

stationarity_by_series <- function(df){
  df <- df %>% arrange(ano)
  x <- suppressWarnings(num_es(df$value))
  years <- df$ano
  res <- list(n = length(x),
              adf_p = NA_real_, kpss_level_p = NA_real_, kpss_trend_p = NA_real_,
              slope = NA_real_, slope_p = NA_real_, spearman_rho = NA_real_, spearman_p = NA_real_,
              class = 'unknown')
  if (sum(is.finite(x)) < 8) return(res)
  
  # ADF y KPSS (si hay paquete)
  if (has_tseries){
    # ADF (H1: estacionario)
    adf <- tryCatch(tseries::adf.test(na.omit(x), k = 0), error = function(e) NULL)
    if (!is.null(adf)) res$adf_p <- adf$p.value
    # KPSS nivel
    kpl <- tryCatch(tseries::kpss.test(na.omit(x), null = 'Level'), error = function(e) NULL)
    if (!is.null(kpl)) res$kpss_level_p <- kpl$p.value
    # KPSS tendencia
    kpt <- tryCatch(tseries::kpss.test(na.omit(x), null = 'Trend'), error = function(e) NULL)
    if (!is.null(kpt)) res$kpss_trend_p <- kpt$p.value
  }
  
  # Tendencia lineal
  fit <- tryCatch(lm(x ~ years), error = function(e) NULL)
  if (!is.null(fit)){
    co <- summary(fit)$coefficients
    res$slope <- unname(co['years','Estimate'])
    res$slope_p <- unname(co['years','Pr(>|t|)'])
  }
  
  # Spearman (monotonia)
  sp <- tryCatch(suppressWarnings(cor.test(years, x, method = 'spearman')), error = function(e) NULL)
  if (!is.null(sp)){
    res$spearman_rho <- unname(sp$estimate)
    res$spearman_p <- sp$p.value
  }
  
  # Clasificacion basica combinando ADF/KPSS
  ap <- res$adf_p; kl <- res$kpss_level_p; kt <- res$kpss_trend_p
  if (!is.na(ap) && !is.na(kl) && ap < 0.05 && kl >= 0.05){
    res$class <- 'stationary'
  } else if (!is.na(ap) && !is.na(kt) && ap < 0.05 && kl < 0.05 && kt >= 0.05){
    res$class <- 'trend_stationary'
  } else if (!is.na(kl) && kl < 0.05 && (is.na(ap) || ap >= 0.05)){
    res$class <- 'difference_stationary_likely'
  } else {
    res$class <- 'inconclusive'
  }
  res
}

summ <- long %>% group_by(dataset, key) %>% group_modify(function(d, k){
  as_tibble(stationarity_by_series(d))
}) %>% ungroup()

readr::write_csv(summ, here::here('output','tables','qa38_stationarity_trends.csv'))

# Rupturas estructurales (opcional)
if (has_strucchange){
  brks <- long %>% group_by(dataset, key) %>% group_modify(function(d, k){
    d <- d %>% arrange(ano)
    x <- suppressWarnings(num_es(d$value))
    if (sum(is.finite(x)) < 10) return(tibble())
    fit <- tryCatch(strucchange::breakpoints(x ~ d$ano), error = function(e) NULL)
    if (is.null(fit)) return(tibble())
    bp <- tryCatch(strucchange::breakpoints(fit), error = function(e) NULL)
    if (is.null(bp)) return(tibble())
    pos <- bp$breakpoints
    if (is.null(pos) || all(is.na(pos))) return(tibble())
    yrs <- d$ano[na.omit(pos)]
    tibble(ano_break = yrs)
  }) %>% ungroup()
  if (nrow(brks) > 0){
    readr::write_csv(brks, here::here('output','tables','qa38_breaks.csv'))
  }
} else {
  message('Paquete strucchange no instalado: se omite la deteccion de rupturas.')
}

message('QC 38 terminado. Revisa:',
        '\n - output/tables/qa38_stationarity_trends.csv',
        '\n - output/tables/qa38_breaks.csv (si existe)')
