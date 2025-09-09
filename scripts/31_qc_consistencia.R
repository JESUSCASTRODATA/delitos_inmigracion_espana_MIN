#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
################################################################################
# 31_qc_consistencia.R  — QC cruzado y smoke tests (versión con dplyr:: calificado)
################################################################################

suppressPackageStartupMessages({
  library(here); library(readr); library(dplyr); library(tidyr)
  library(purrr); library(tibble); library(janitor); library(stringr); library(fs)
})

fs::dir_create(here::here("output","tables"))

read_csv_auto <- function(fp){
  out <- tryCatch(suppressMessages(readr::read_csv(fp, show_col_types = FALSE)),
                  error = function(e) NULL)
  if (is.null(out)){
    out <- tryCatch(suppressMessages(readr::read_delim(fp, delim = ";", show_col_types = FALSE)),
                    error = function(e) NULL)
  }
  out
}

num_es <- function(x){
  readr::parse_number(as.character(x),
                      locale = readr::locale(decimal_mark = ",", grouping_mark = "."))
}

num_es_first <- function(x){
  if (is.numeric(x)) return(as.double(x))
  s <- stringr::str_replace_all(as.character(x), "\\s+", " ")
  tok <- stringr::str_extract(s, "-?\\d{1,3}(?:[\\.,]\\d{3})*(?:[\\.,]\\d+)?|-?\\d+(?:[\\.,]\\d+)?")
  readr::parse_number(tok, locale = readr::locale(decimal_mark = ",", grouping_mark = "."))
}

rescale_pct <- function(v){
  med <- stats::median(v, na.rm = TRUE)
  if (!is.finite(med)) return(v)
  if (med > 1000) return(v/100)
  if (med > 100)  return(v/10)
  v
}

add_row <- function(tbl, ...){
  dplyr::bind_rows(tbl, tibble::tibble(...))
}

has_cols <- function(df, cols) all(cols %in% names(df))

#------------------------------------------------------------------------------
# 1) Reconstrucción de tasas/100k y comparación con derivadas
#------------------------------------------------------------------------------
fp_hc   <- here::here("data","processed","hechos_conocidos_total_nacional.csv")
fp_dt   <- here::here("data","processed","detenciones_totales_total_nacional.csv")
fp_de   <- here::here("data","processed","detenciones_extranjeros_total_nacional.csv")
fp_pop  <- here::here("data","processed","poblacion_total_nacional.csv")
fp_tasa <- here::here("data","processed","tasas","tasas_totales_anuales.csv")

hc <- read_csv_auto(fp_hc); if (is.null(hc)) issues_missing <- add_row(tibble::tibble(), file=fp_hc, what="hechos_conocidos", note="No se pudo leer") else issues_missing <- tibble::tibble(file=character(), what=character(), note=character())
dt <- read_csv_auto(fp_dt); if (is.null(dt)) issues_missing <- add_row(issues_missing, file=fp_dt, what="det_tot", note="No se pudo leer")
de <- read_csv_auto(fp_de); if (is.null(de)) issues_missing <- add_row(issues_missing, file=fp_de, what="det_ext", note="No se pudo leer")
pp <- read_csv_auto(fp_pop); if (is.null(pp)) issues_missing <- add_row(issues_missing, file=fp_pop, what="poblacion_total", note="No se pudo leer")
tt <- read_csv_auto(fp_tasa) # opcional

rates_out <- tibble::tibble(ano = integer())

# ---- HC ----
if (!is.null(hc) && !is.null(pp)){
  hc <- hc |> janitor::clean_names() |>
    dplyr::mutate(valor = num_es(.data$valor), tipo = as.character(.data$tipo))
  hc_tot <- hc |> dplyr::filter(stringr::str_detect(tolower(.data$tipo), "total")) |>
    dplyr::transmute(ano = as.integer(.data$ano), hc_total = .data$valor)
  if (nrow(hc_tot) == 0){
    hc_tot <- hc |> dplyr::group_by(.data$ano) |>
      dplyr::summarise(hc_total = sum(.data$valor, na.rm=TRUE), .groups="drop") |>
      dplyr::mutate(ano = as.integer(.data$ano))
  }
  pp_hc <- pp |> janitor::clean_names() |>
    dplyr::transmute(ano = as.integer(.data$ano), poblacion_total = num_es(.data$poblacion_total))
  rates_hc <- hc_tot |>
    dplyr::inner_join(pp_hc, by="ano") |>
    dplyr::mutate(tasa_hc_100k_rebuilt = 1e5 * .data$hc_total / .data$poblacion_total) |>
    dplyr::select(.data$ano, .data$tasa_hc_100k_rebuilt)
  rates_out <- dplyr::full_join(rates_out, rates_hc, by = "ano")
}

# ---- Detenciones totales ----
if (!is.null(dt) && !is.null(pp)){
  dt <- dt |> janitor::clean_names() |>
    dplyr::mutate(valor = num_es(.data$valor), tipo = as.character(.data$tipo))
  dt_tot <- dt |> dplyr::filter(stringr::str_detect(tolower(.data$tipo), "total")) |>
    dplyr::transmute(ano = as.integer(.data$ano), det_tot = .data$valor)
  if (nrow(dt_tot) == 0){
    dt_tot <- dt |> dplyr::group_by(.data$ano) |>
      dplyr::summarise(det_tot = sum(.data$valor, na.rm=TRUE), .groups="drop") |>
      dplyr::mutate(ano = as.integer(.data$ano))
  }
  pp_dt <- pp |> janitor::clean_names() |>
    dplyr::transmute(ano = as.integer(.data$ano), poblacion_total = num_es(.data$poblacion_total))
  rates_dt <- dt_tot |>
    dplyr::inner_join(pp_dt, by="ano") |>
    dplyr::mutate(tasa_det_tot_100k_rebuilt = 1e5 * .data$det_tot / .data$poblacion_total) |>
    dplyr::select(.data$ano, .data$tasa_det_tot_100k_rebuilt)
  rates_out <- dplyr::full_join(rates_out, rates_dt, by = "ano")
}

# ---- Detenciones extranjeros ----
if (!is.null(de) && !is.null(pp)){
  de <- de |> janitor::clean_names() |>
    dplyr::mutate(valor = num_es(.data$valor), tipo = as.character(.data$tipo))
  de_tot <- de |> dplyr::filter(stringr::str_detect(tolower(.data$tipo), "total")) |>
    dplyr::transmute(ano = as.integer(.data$ano), det_ext = .data$valor)
  if (nrow(de_tot) == 0){
    de_tot <- de |> dplyr::group_by(.data$ano) |>
      dplyr::summarise(det_ext = sum(.data$valor, na.rm=TRUE), .groups="drop") |>
      dplyr::mutate(ano = as.integer(.data$ano))
  }
  pp_de <- pp |> janitor::clean_names() |>
    dplyr::transmute(ano = as.integer(.data$ano), poblacion_total = num_es(.data$poblacion_total))
  rates_de <- de_tot |>
    dplyr::inner_join(pp_de, by="ano") |>
    dplyr::mutate(tasa_det_ext_100k_rebuilt = 1e5 * .data$det_ext / .data$poblacion_total) |>
    dplyr::select(.data$ano, .data$tasa_det_ext_100k_rebuilt)
  rates_out <- dplyr::full_join(rates_out, rates_de, by = "ano")
}

# ---- Comparación con tasas derivadas (si existen) ----
rates_cmp <- tibble::tibble()
if (!is.null(tt) && nrow(rates_out) > 0){
  tt <- tt |> janitor::clean_names()
  col_hc <- names(tt)[stringr::str_detect(names(tt), "hc")  & stringr::str_detect(names(tt), "100k")]
  col_dt <- names(tt)[stringr::str_detect(names(tt), "det") & stringr::str_detect(names(tt), "tot") & stringr::str_detect(names(tt), "100k")]
  col_de <- names(tt)[stringr::str_detect(names(tt), "det") & stringr::str_detect(names(tt), "ext") & stringr::str_detect(names(tt), "100k")]
  
  tmp <- tt |>
    dplyr::transmute(
      ano = as.integer(.data$ano),
      tasa_hc_100k      = if (length(col_hc)>0) num_es(.data[[col_hc[1]]]) else NA_real_,
      tasa_det_tot_100k = if (length(col_dt)>0) num_es(.data[[col_dt[1]]]) else NA_real_,
      tasa_det_ext_100k = if (length(col_de)>0) num_es(.data[[col_de[1]]]) else NA_real_
    ) |>
    dplyr::inner_join(rates_out, by="ano")
  
  rates_cmp <- tmp |>
    dplyr::mutate(
      diff_hc_rel = ifelse(!is.na(.data$tasa_hc_100k)      & !is.na(.data$tasa_hc_100k_rebuilt),
                           abs(.data$tasa_hc_100k - .data$tasa_hc_100k_rebuilt)/pmax(1e-9, .data$tasa_hc_100k), NA_real_),
      diff_dt_rel = ifelse(!is.na(.data$tasa_det_tot_100k) & !is.na(.data$tasa_det_tot_100k_rebuilt),
                           abs(.data$tasa_det_tot_100k - .data$tasa_det_tot_100k_rebuilt)/pmax(1e-9, .data$tasa_det_tot_100k), NA_real_),
      diff_de_rel = ifelse(!is.na(.data$tasa_det_ext_100k) & !is.na(.data$tasa_det_ext_100k_rebuilt),
                           abs(.data$tasa_det_ext_100k - .data$tasa_det_ext_100k_rebuilt)/pmax(1e-9, .data$tasa_det_ext_100k), NA_real_),
      flag_hc = !is.na(.data$diff_hc_rel) & .data$diff_hc_rel > 0.01,
      flag_dt = !is.na(.data$diff_dt_rel) & .data$diff_dt_rel > 0.01,
      flag_de = !is.na(.data$diff_de_rel) & .data$diff_de_rel > 0.01
    )
  
  readr::write_csv(rates_cmp, here::here("output","tables","qa31_rates_rebuild.csv"))
} else {
  if (nrow(rates_out) > 0){
    readr::write_csv(rates_out, here::here("output","tables","qa31_rates_rebuild.csv"))
  } else {
    issues_missing <- add_row(issues_missing, file="(varios)", what="rates_out", note="No se pudo reconstruir por falta de inputs")
  }
}

#------------------------------------------------------------------------------
# 2) Share detenciones extranjeros (det_ext / det_tot) y comparación
#------------------------------------------------------------------------------
fp_share <- here::here("data","processed","detenciones_share_extranjeros.csv")
sh <- read_csv_auto(fp_share) # opcional
share_cmp <- tibble::tibble()

if (!is.null(dt) && !is.null(de)){
  base <- dt |> janitor::clean_names() |>
    dplyr::mutate(valor = num_es(.data$valor), tipo = as.character(.data$tipo)) |>
    dplyr::filter(stringr::str_detect(tolower(.data$tipo), "total")) |>
    dplyr::transmute(ano = as.integer(.data$ano), det_tot = .data$valor)
  if (nrow(base) == 0){
    base <- dt |> janitor::clean_names() |>
      dplyr::mutate(valor = num_es(.data$valor)) |>
      dplyr::group_by(.data$ano) |>
      dplyr::summarise(det_tot = sum(.data$valor, na.rm=TRUE), .groups="drop") |>
      dplyr::mutate(ano = as.integer(.data$ano))
  }
  ext <- de |> janitor::clean_names() |>
    dplyr::mutate(valor = num_es(.data$valor), tipo = as.character(.data$tipo)) |>
    dplyr::filter(stringr::str_detect(tolower(.data$tipo), "total")) |>
    dplyr::transmute(ano = as.integer(.data$ano), det_ext = .data$valor)
  if (nrow(ext) == 0){
    ext <- de |> janitor::clean_names() |>
      dplyr::mutate(valor = num_es(.data$valor)) |>
      dplyr::group_by(.data$ano) |>
      dplyr::summarise(det_ext = sum(.data$valor, na.rm=TRUE), .groups="drop") |>
      dplyr::mutate(ano = as.integer(.data$ano))
  }
  
  sc <- base |>
    dplyr::inner_join(ext, by="ano") |>
    dplyr::mutate(share_det_ext = ifelse(.data$det_tot > 0, .data$det_ext/.data$det_tot, NA_real_))
  
  if (!is.null(sh)){
    sh <- sh |> janitor::clean_names()
    share_col <- names(sh)[stringr::str_detect(names(sh), "share") | stringr::str_detect(names(sh), "propor")]
    sh2 <- sh |> dplyr::transmute(
      ano = as.integer(.data$ano),
      share_archivo = if (length(share_col)>0) num_es(.data[[share_col[1]]]) else NA_real_
    )
    if (any(sh2$share_archivo > 1.2, na.rm = TRUE)){
      sh2 <- sh2 |> dplyr::mutate(share_archivo = .data$share_archivo/100)
    }
    share_cmp <- sc |>
      dplyr::inner_join(sh2, by="ano") |>
      dplyr::mutate(diff_abs = abs(.data$share_det_ext - .data$share_archivo),
                    flag_share = !is.na(.data$diff_abs) & .data$diff_abs > 0.005)
  } else {
    share_cmp <- sc
  }
  
  readr::write_csv(share_cmp, here::here("output","tables","qa31_share_det_extr_check.csv"))
} else {
  issues_missing <- add_row(issues_missing, file="(dt/de)", what="share_det_ext", note="Faltan inputs para share")
}

#------------------------------------------------------------------------------
# 3) Rango de % y plausibilidad PIB pc real
#------------------------------------------------------------------------------
ranges_out <- tibble::tibble()

# AROPE
fp_arope <- here::here("data","processed","arope_total.csv")
ar <- read_csv_auto(fp_arope)
if (!is.null(ar)){
  ar <- ar |> janitor::clean_names()
  val_col <- names(ar)[stringr::str_detect(names(ar), "arope")]
  if (length(val_col) > 0){
    ar2 <- ar |>
      dplyr::transmute(ano = as.integer(.data$ano), arope = num_es_first(.data[[val_col[1]]])) |>
      dplyr::mutate(arope = rescale_pct(.data$arope),
                    flag_range = .data$arope < 0 | .data$arope > 100)
    ranges_out <- dplyr::bind_rows(ranges_out, ar2 |> dplyr::mutate(var="AROPE") |> dplyr::select(.data$var, dplyr::everything()))
  }
} else {
  issues_missing <- add_row(issues_missing, file=fp_arope, what="AROPE", note="No se pudo leer")
}

# Paro 15–29
fp_paro <- here::here("data","processed","paro_15_29_anual.csv")
pr <- read_csv_auto(fp_paro)
if (!is.null(pr)){
  pr <- pr |> janitor::clean_names()
  val_col <- names(pr)[stringr::str_detect(names(pr), "paro")]
  if (length(val_col) > 0){
    pr2 <- pr |>
      dplyr::transmute(ano = as.integer(.data$ano), paro_15_29 = num_es_first(.data[[val_col[1]]])) |>
      dplyr::mutate(paro_15_29 = rescale_pct(.data$paro_15_29),
                    flag_range = .data$paro_15_29 < 0 | .data$paro_15_29 > 100)
    ranges_out <- dplyr::bind_rows(ranges_out, pr2 |> dplyr::mutate(var="PARO_15_29") |> dplyr::select(.data$var, dplyr::everything()))
  }
} else {
  issues_missing <- add_row(issues_missing, file=fp_paro, what="PARO_15_29", note="No se pudo leer")
}

# PIB per cápita real
fp_pib <- here::here("data","processed","pib_pc_real_es.csv")
pb <- read_csv_auto(fp_pib)
if (!is.null(pb)){
  pb <- pb |> janitor::clean_names()
  val_col <- names(pb)[stringr::str_detect(names(pb), "pib") | stringr::str_detect(names(pb), "pc_real")]
  if (length(val_col) > 0){
    pb2 <- pb |>
      dplyr::transmute(ano = as.integer(.data$ano), pib_pc_real = num_es(.data[[val_col[1]]])) |>
      dplyr::mutate(flag_range = .data$pib_pc_real < 100 | .data$pib_pc_real > 100000)
    ranges_out <- dplyr::bind_rows(ranges_out, pb2 |> dplyr::mutate(var="PIB_PC_REAL") |> dplyr::select(.data$var, dplyr::everything()))
  }
} else {
  issues_missing <- add_row(issues_missing, file=fp_pib, what="PIB_PC_REAL", note="No se pudo leer")
}

if (nrow(ranges_out) > 0){
  readr::write_csv(ranges_out, here::here("output","tables","qa31_ranges.csv"))
}

#------------------------------------------------------------------------------
# 4) Detección de picos YoY robusta (MAD)
#------------------------------------------------------------------------------
mad_robust <- function(x){
  m <- stats::median(x, na.rm=TRUE)
  mad <- stats::median(abs(x - m), na.rm=TRUE)
  if (!is.finite(mad) || mad == 0) mad <- stats::sd(x, na.rm=TRUE)
  if (!is.finite(mad) || mad == 0) mad <- 1
  list(median=m, mad=mad)
}

yoy_spikes <- function(df, col, k = 3){
  df <- df |> dplyr::arrange(.data$ano)
  x <- num_es(df[[col]])
  d <- c(NA, diff(x))
  r <- mad_robust(d[!is.na(d)])
  thr <- k * r$mad
  tibble::tibble(ano = df$ano, var = col, delta = d, flag_spike = !is.na(d) & abs(d) > thr)
}

spikes_out <- tibble::tibble()

if (exists("hc_tot") && nrow(hc_tot)>0){
  spikes_out <- dplyr::bind_rows(spikes_out, yoy_spikes(hc_tot, "hc_total"))
}
if (exists("dt_tot") && nrow(dt_tot)>0){
  spikes_out <- dplyr::bind_rows(spikes_out, yoy_spikes(dt_tot, "det_tot"))
}
if (exists("de_tot") && nrow(de_tot)>0){
  spikes_out <- dplyr::bind_rows(spikes_out, yoy_spikes(de_tot, "det_ext"))
}

if (!is.null(tt)){
  tt2 <- tt |> janitor::clean_names()
  rate_cols <- names(tt2)[grepl("100k$", names(tt2))]
  for (c in rate_cols){
    dfc <- tt2 |> dplyr::transmute(ano = as.integer(.data$ano), !!c := num_es(.data[[c]]))
    names(dfc)[2] <- "val"
    sp <- yoy_spikes(dplyr::rename(dfc, !!c := .data$val), c)
    spikes_out <- dplyr::bind_rows(spikes_out, sp)
  }
}

if (nrow(spikes_out) > 0){
  readr::write_csv(spikes_out, here::here("output","tables","qa31_yoy_spikes.csv"))
}

#------------------------------------------------------------------------------
# 5) Archivos faltantes / procesos omitidos
#------------------------------------------------------------------------------
if (nrow(issues_missing) > 0){
  readr::write_csv(issues_missing, here::here("output","tables","qa31_missing_or_skipped.csv"))
}

message("✅ QC 31 terminado. Revisa:",
        "\n - output/tables/qa31_rates_rebuild.csv",
        "\n - output/tables/qa31_share_det_extr_check.csv",
        "\n - output/tables/qa31_ranges.csv",
        "\n - output/tables/qa31_yoy_spikes.csv",
        "\n - output/tables/qa31_missing_or_skipped.csv (si existe)")
