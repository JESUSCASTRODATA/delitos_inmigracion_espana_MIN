#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
################################################################################
# 36_qc_units_consistency.R
#
# Objetivo:
#   - Comprobar coherencia de unidades por nombre de columna y valores
#   - Detectar escalas erroneas tipicas (x10, x100) en porcentajes
#   - Señalar posibles columnas por 100k incoherentes
#
# Salidas (output/tables):
#   - qa36_units_anomalies.csv
#
# Dependencias: here, readr, dplyr, tidyr, tibble, janitor, stringr, purrr
################################################################################

suppressPackageStartupMessages({
  library(here); library(readr); library(dplyr); library(tidyr); library(tibble)
  library(janitor); library(stringr); library(purrr)
})

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

percent_like <- function(nm){
  # nombres que sugieren porcentaje
  stringr::str_detect(nm, '(?i)(%|porc|arope|paro|_pct$|pct_)')
}
per100k_like <- function(nm){
  # tasas por 100k
  stringr::str_detect(nm, '(?i)(100k|por_100k|tasa_.*100k)')
}
share_like <- function(nm){
  # proporciones 0..1
  stringr::str_detect(nm, '(?i)(share|propor|ratio)')
}
currency_like <- function(nm){
  # euros o PIB per capita
  stringr::str_detect(nm, '(?i)(pib|euro|eur|\\bpc_real\\b)')
}

anomalies <- tibble::tibble()

files <- list.files(here::here('data','processed'), pattern='\\.csv$', recursive=TRUE, full.names=TRUE)
for (f in files){
  df <- read_csv_auto(f)
  if (is.null(df) || nrow(df) == 0) next
  df <- janitor::clean_names(df)
  
  for (nm in names(df)){
    if (nm %in% c('ano','tipo','tipologia','tipologia_penal','sexo','provincia','ccaa')) next
    x <- suppressWarnings(num_es(df[[nm]]))
    if (all(is.na(x))) next
    
    med <- suppressWarnings(stats::median(x, na.rm = TRUE))
    mx  <- suppressWarnings(max(x, na.rm = TRUE))
    mn  <- suppressWarnings(min(x, na.rm = TRUE))
    
    flag <- NA_character_
    suggest <- NA_character_
    
    # Porcentajes deben ~ [0,100]
    if (percent_like(nm)){
      if (is.finite(med) && med > 1000) { flag <- 'percent_x100';        suggest <- 'dividir entre 100' }
      else if (is.finite(med) && med > 100) { flag <- 'percent_x10';      suggest <- 'dividir entre 10' }
      else if ((is.finite(mx) && mx > 100.5) || (is.finite(mn) && mn < -0.5)) {
        flag <- 'percent_out_of_range'; suggest <- 'revisar origen/escala'
      }
    }
    
    # Shares 0..1
    if (is.na(flag) && share_like(nm)){
      if ((is.finite(mx) && mx > 1.2) || (is.finite(mn) && mn < -0.05)) {
        flag <- 'share_out_of_range'; suggest <- 'si estaba en %, dividir entre 100'
      }
    }
    
    # Tasas por 100k: valores muy altos son sospechosos
    if (is.na(flag) && per100k_like(nm)){
      if (is.finite(mx) && mx > 1e6) { flag <- 'per100k_implausible'; suggest <- 'revisar divisor (poblacion) o escala' }
    }
    
    # Moneda per capita razonable (euros constantes)
    if (is.na(flag) && currency_like(nm)){
      if (is.finite(mx) && (mx < 50 || mx > 200000)) { flag <- 'currency_implausible'; suggest <- 'verificar deflactor/unidad' }
    }
    
    if (!is.na(flag)){
      anomalies <- dplyr::bind_rows(
        anomalies,
        tibble::tibble(
          file = gsub(paste0('^', gsub('\\\\','/', here::here('data','processed')), '/?'), '', gsub('\\\\','/', f)),
          column = nm, median = med, min = mn, max = mx, flag = flag, suggestion = suggest
        )
      )
    }
  }
}

if (nrow(anomalies) > 0){
  readr::write_csv(anomalies, here::here('output','tables','qa36_units_anomalies.csv'))
} else {
  readr::write_csv(tibble::tibble(info='Sin anomalias detectadas'), here::here('output','tables','qa36_units_anomalies.csv'))
}

message('QC 36 terminado. Revisa:',
        '\n - output/tables/qa36_units_anomalies.csv')
