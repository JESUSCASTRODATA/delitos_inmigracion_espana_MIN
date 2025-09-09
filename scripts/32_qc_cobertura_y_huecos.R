#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
################################################################################
# 32_qc_cobertura_y_huecos.R
#
# Objetivo:
#   - Matriz de cobertura (años vs dataset) para 2010–2023.
#   - Detección de huecos (NA o ausencia) por serie temporal en claves estándar.
#   - Sugerencias de imputación (solo diagnóstico): carry-forward/backward simple.
#
# Salidas:
#   output/tables/qa32_cobertura_matrix.csv
#   output/tables/qa32_huecos_detalle.csv
#   output/tables/qa32_imputacion_sugerida.csv (solo vista previa, no modifica datos)
#
# Requisitos: readr, dplyr, tidyr, purrr, tibble, janitor, here, stringr
################################################################################

suppressPackageStartupMessages({
  library(here); library(readr); library(dplyr); library(tidyr)
  library(purrr); library(tibble); library(janitor); library(stringr)
})

dir.create(here("output","tables"), recursive = TRUE, showWarnings = FALSE)

read_csv_auto <- function(fp){
  out <- tryCatch(suppressMessages(readr::read_csv(fp, show_col_types = FALSE)),
                  error = function(e) NULL)
  if (is.null(out)){
    out <- tryCatch(suppressMessages(readr::read_delim(fp, delim = ";", show_col_types = FALSE)),
                    error = function(e) NULL)
  }
  out
}

required_years <- 2010:2023
files <- list.files(here("data","processed"), pattern="\\.csv$", recursive = TRUE, full.names = TRUE)

# Normaliza un dataset a formato largo (ano, key, value)
to_long <- function(df, name){
  df <- df %>% clean_names()
  if (!("ano" %in% names(df))) return(NULL)
  # Detecta columna de valor "principal"
  val_candidates <- c("valor", "poblacion_total", "poblacion_espanola", "poblacion_extranjera",
                      "arope", "paro_15_29", "pib_pc_real")
  vcol <- intersect(val_candidates, names(df))
  if (length(vcol) == 0){
    # usa la primera numérica distinta de ano y clave(s)
    nc <- names(df)[!names(df) %in% c("ano","tipo","tipologia","tipologia_penal","sexo")]
    vcol <- nc[map_lgl(nc, ~ is.numeric(df[[.x]]) || is.integer(df[[.x]]) )]
    vcol <- vcol[1]
  } else {
    vcol <- vcol[1]
  }
  key <- intersect(names(df), c("tipo","tipologia","tipologia_penal","sexo"))
  if (length(key) == 0) key <- "TOTAL"
  if (identical(key, "TOTAL")){
    out <- df %>% transmute(dataset = basename(name), ano = as.integer(ano), key = "TOTAL", value = suppressWarnings(readr::parse_number(as.character(.data[[vcol]]), locale = readr::locale(decimal_mark=",", grouping_mark="."))))
  } else {
    out <- df %>% transmute(dataset = basename(name), ano = as.integer(ano), key = paste(!!!syms(key), sep="|"), value = suppressWarnings(readr::parse_number(as.character(.data[[vcol]]), locale = readr::locale(decimal_mark=",", grouping_mark="."))))
  }
  out
}

long_list <- purrr::imap(files, ~ to_long(read_csv_auto(.x), .x))
long <- bind_rows(long_list[!vapply(long_list, is.null, logical(1))])

# Cobertura por dataset
cov <- long %>% filter(ano %in% required_years) %>% distinct(dataset, ano) %>%
  mutate(present = 1) %>%
  tidyr::pivot_wider(names_from = dataset, values_from = present, values_fill = 0) %>%
  arrange(ano)

readr::write_csv(cov, here("output","tables","qa32_cobertura_matrix.csv"))

# Huecos por serie (dataset+key)
# Def: hay hueco si algún año requerido no aparece o value=NA
all_keys <- long %>% filter(ano %in% required_years) %>% distinct(dataset, key)
huecos <- purrr::pmap_dfr(list(all_keys$dataset, all_keys$key), function(ds, ky){
  sub <- long %>% filter(dataset==ds, key==ky, ano %in% required_years) %>%
    complete(ano = required_years) %>% arrange(ano)
  sub <- sub %>% mutate(flag_missing = is.na(value))
  gaps <- sum(sub$flag_missing)
  if (gaps > 0){
    sub %>% mutate(dataset = ds, key = ky)
  } else {
    tibble()
  }
})

if (nrow(huecos) > 0){
  readr::write_csv(huecos, here("output","tables","qa32_huecos_detalle.csv"))
}

# Sugerencia de imputación (EXCLUSIVAMENTE DIAGNÓSTICA): LOCF/FOCF
imputa_preview <- function(x){
  # carry forward/backward simple
  fwd <- zoo::na.locf(x, na.rm = FALSE)
  bwd <- zoo::na.locf(x, fromLast = TRUE, na.rm = FALSE)
  ifelse(is.na(fwd), bwd, fwd)
}

sug <- tibble()
if (nrow(huecos) > 0){
  suppressPackageStartupMessages({ library(zoo) })
  sug <- huecos %>% group_by(dataset, key) %>% arrange(ano) %>%
    mutate(value_imput = imputa_preview(value)) %>% ungroup()
  readr::write_csv(sug, here("output","tables","qa32_imputacion_sugerida.csv"))
}

message("✅ QC 32 terminado. Revisa:",
        "\n - output/tables/qa32_cobertura_matrix.csv",
        "\n - output/tables/qa32_huecos_detalle.csv (si existe)",
        "\n - output/tables/qa32_imputacion_sugerida.csv (si existe)")
