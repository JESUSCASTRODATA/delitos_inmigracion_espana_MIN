#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
################################################################################
# 33_qc_outliers_ts.R  — outliers temporales por serie (versión dplyr:: calificada)
################################################################################

suppressPackageStartupMessages({
  library(here); library(readr); library(dplyr); library(tidyr)
  library(purrr); library(tibble); library(janitor); library(stringr); library(fs)
})

fs::dir_create(here::here("output","tables"), recursive = TRUE, showWarnings = FALSE)

#-----------------------------------
# Utilidades de lectura / parseo
#-----------------------------------
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

#-----------------------------------
# Parámetros
#-----------------------------------
required_years <- 2010:2023
ignore_years_default <- c(2020L, 2021L)

fp_ignore <- here::here("config","qa_ignore_years.csv")
if (file.exists(fp_ignore)){
  ig <- read_csv_auto(fp_ignore)
  if (!is.null(ig)){
    ig <- janitor::clean_names(ig)
    col_ano <- names(ig)[tolower(names(ig)) == "ano"][1]
    ignore_years <- if (!is.na(col_ano)) unique(as.integer(ig[[col_ano]])) else ignore_years_default
  } else {
    ignore_years <- ignore_years_default
  }
} else {
  ignore_years <- ignore_years_default
}

#-----------------------------------
# Normalización a formato largo
#-----------------------------------
files <- list.files(here::here("data","processed"), pattern="\\.csv$", recursive = TRUE, full.names = TRUE)

to_long <- function(df, name){
  if (is.null(df)) return(NULL)
  df <- janitor::clean_names(df)
  if (!("ano" %in% names(df))) return(NULL)
  
  val_candidates <- c("valor","poblacion_total","poblacion_espanola","poblacion_extranjera",
                      "arope","paro_15_29","pib_pc_real")
  vcol <- intersect(val_candidates, names(df))
  if (length(vcol) == 0){
    nc <- setdiff(names(df), c("ano","tipo","tipologia","tipologia_penal","sexo"))
    nc_num <- nc[vapply(nc, function(c) is.numeric(df[[c]]) || is.integer(df[[c]]), logical(1))]
    vcol <- if (length(nc_num)) nc_num[1] else if (length(nc)) nc[1] else return(NULL)
  } else {
    vcol <- vcol[1]
  }
  key_cols <- intersect(names(df), c("tipo","tipologia","tipologia_penal","sexo"))
  val <- if (vcol %in% c("arope","paro_15_29")) num_es_first(df[[vcol]]) else num_es(df[[vcol]])
  
  if (length(key_cols) == 0) {
    tibble::tibble(
      dataset = basename(name),
      ano     = as.integer(df$ano),
      key     = "TOTAL",
      value   = val
    )
  } else {
    tibble::tibble(
      dataset = basename(name),
      ano     = as.integer(df$ano),
      key     = do.call(paste, c(df[key_cols], sep="|")),
      value   = val
    )
  }
}

long_list <- purrr::imap(files, ~ to_long(read_csv_auto(.x), .x))
long <- dplyr::bind_rows(long_list[!vapply(long_list, is.null, logical(1))])

# Filtrar años requeridos
long <- long |> dplyr::filter(.data$ano %in% required_years)

#-----------------------------------
# Funciones de outliers
#-----------------------------------
iqr_bounds <- function(x){
  q1 <- stats::quantile(x, 0.25, na.rm = TRUE)
  q3 <- stats::quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lo <- q1 - 1.5 * iqr
  hi <- q3 + 1.5 * iqr
  c(lo=lo, hi=hi)
}

mad_stats <- function(x){
  m <- stats::median(x, na.rm=TRUE)
  mad <- stats::median(abs(x - m), na.rm=TRUE)
  if (!is.finite(mad) || mad==0){
    sdv <- stats::sd(x, na.rm=TRUE)
    if (!is.finite(sdv) || sdv==0) sdv <- 1
    mad <- sdv
  }
  list(median=m, mad=mad)
}

roll_mad_flags <- function(ano, x, k = 3, win = 5){
  n <- length(x); flags <- rep(FALSE, n); z <- rep(NA_real_, n)
  for (i in seq_len(n)){
    l <- max(1, i - floor(win/2)); r <- min(n, i + floor(win/2))
    if ((r - l + 1) < 3) next
    xx <- x[l:r]
    st <- mad_stats(xx)
    if (!is.finite(st$mad) || st$mad==0) next
    z[i] <- (x[i] - st$median) / st$mad
    flags[i] <- is.finite(z[i]) && abs(z[i]) > k
  }
  tibble::tibble(ano=ano, z_robust=z, flag_roll_mad=flags)
}

#-----------------------------------
# 1) Outliers de nivel por IQR
#-----------------------------------
level_out <- long |>
  dplyr::group_by(.data$dataset, .data$key) |>
  dplyr::group_modify(function(d, k){
    b <- iqr_bounds(d$value)
    d |>
      dplyr::mutate(flag_level_iqr = !is.na(.data$value) & (.data$value < b["lo"] | .data$value > b["hi"]),
                    lo = b["lo"], hi = b["hi"])
  }) |>
  dplyr::ungroup()

level_out_final <- level_out |>
  dplyr::filter(.data$flag_level_iqr, !(.data$ano %in% ignore_years)) |>
  dplyr::select(dataset, key, ano, value, lo, hi, flag_level_iqr)

if (nrow(level_out_final) > 0){
  readr::write_csv(level_out_final, here::here("output","tables","qa33_outliers_level_iqr.csv"))
}

#-----------------------------------
# 2) Spikes YoY por MAD (k=3)
#-----------------------------------
yoy_out <- long |>
  dplyr::arrange(.data$dataset, .data$key, .data$ano) |>
  dplyr::group_by(.data$dataset, .data$key) |>
  dplyr::group_modify(function(d, k){
    if (nrow(d) < 4) return(tibble::tibble())
    d <- d |> dplyr::arrange(.data$ano)
    delta <- c(NA_real_, diff(d$value))
    st <- mad_stats(delta[!is.na(delta)])
    thr <- 3 * st$mad
    tibble::tibble(ano = d$ano,
                   value = d$value,
                   delta = delta,
                   thr = thr,
                   flag_yoy = !is.na(delta) & abs(delta) > thr)
  }) |>
  dplyr::ungroup()

yoy_out_final <- yoy_out |>
  dplyr::filter(.data$flag_yoy, !(.data$ano %in% ignore_years)) |>
  dplyr::left_join(long, by=c("dataset","key","ano","value")) |>
  dplyr::select(dataset, key, ano, value, delta, thr, flag_yoy)

if (nrow(yoy_out_final) > 0){
  readr::write_csv(yoy_out_final, here::here("output","tables","qa33_spikes_yoy_mad.csv"))
}

#-----------------------------------
# 3) Rolling MAD (k=3, win=5)
#-----------------------------------
roll_out <- long |>
  dplyr::arrange(.data$dataset, .data$key, .data$ano) |>
  dplyr::group_by(.data$dataset, .data$key) |>
  dplyr::group_modify(function(d, k){
    if (nrow(d) < 5) return(tibble::tibble())
    r <- roll_mad_flags(d$ano, d$value, k = 3, win = 5)
    dplyr::bind_cols(d, r |> dplyr::select(z_robust, flag_roll_mad))
  }) |>
  dplyr::ungroup()

roll_out_final <- roll_out |>
  dplyr::filter(.data$flag_roll_mad, !(.data$ano %in% ignore_years)) |>
  dplyr::select(dataset, key, ano, value, z_robust, flag_roll_mad)

if (nrow(roll_out_final) > 0){
  readr::write_csv(roll_out_final, here::here("output","tables","qa33_rolling_mad.csv"))
}

#-----------------------------------
# 4) Resumen por dataset
#-----------------------------------
summary_out <- long |>
  dplyr::distinct(dataset) |>
  dplyr::left_join(level_out_final |> dplyr::count(dataset, name="n_level_iqr"), by="dataset") |>
  dplyr::left_join(yoy_out_final   |> dplyr::count(dataset, name="n_yoy"), by="dataset") |>
  dplyr::left_join(roll_out_final  |> dplyr::count(dataset, name="n_roll_mad"), by="dataset") |>
  dplyr::mutate(dplyr::across(dplyr::starts_with("n_"), ~ tidyr::replace_na(., 0L))) |>
  dplyr::arrange(dplyr::desc(.data$n_level_iqr + .data$n_yoy + .data$n_roll_mad))

readr::write_csv(summary_out, here::here("output","tables","qa33_outliers_summary.csv"))

message("✅ QC 33 terminado. Revisa:",
        "\n - output/tables/qa33_outliers_level_iqr.csv (si existe)",
        "\n - output/tables/qa33_spikes_yoy_mad.csv (si existe)",
        "\n - output/tables/qa33_rolling_mad.csv (si existe)",
        "\n - output/tables/qa33_outliers_summary.csv")
