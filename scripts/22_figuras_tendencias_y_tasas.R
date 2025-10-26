#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
################################################################################
# 22_figuras_tendencias_y_tasas.R — Series base-100 (HC, HE, % extranjeros) y tasas 100k
# Proyecto   : Delitos e Inmigración en España (2010–2023)
# Autor      : Jesús Castro · JESUSCASTRODATA
# Licencia   : MIT (código) · CC BY 4.0 (docs/figuras)
# Fecha      : 2025-10-06
# Descripción:
#   - Lee el core anual y genera dos figuras clave:
#       (1) 01_tendencias_base100.{png,pdf} — Índices base-100 de HC, HE y % extranjeros
#       (2) 02_tasas_100k.{png,pdf}         — Tasas por 100.000 (HC y HE)
#   - Aplica el tema pearl y paleta Okabe–Ito (scripts/00_theme_figuras.R si existe).
#   - Sombrea el periodo COVID (2020–2021) si esos años están presentes en los datos.
#   - Incluye un QC: valida si la población está en PERSONAS o en MILES comparando
#     la tasa calculada con la del archivo; escribe output/tables/22_qc_tasas.csv.
################################################################################

suppressPackageStartupMessages({
  library(here); library(readr); library(dplyr); library(tidyr); library(janitor)
  library(ggplot2); library(scales); library(stringr); library(purrr)
})

msg <- function(...) message("[22] ", paste0(...))
ensure_dir <- function(p){ dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE); invisible(p) }

# --- Rutas --------------------------------------------------------------------
p_core   <- here::here("data","processed","core_indicadores.csv")
fig1_png <- here::here("output","figures","01_tendencias_base100.png")
fig1_pdf <- here::here("output","figures","01_tendencias_base100.pdf")
fig2_png <- here::here("output","figures","02_tasas_100k.png")
fig2_pdf <- here::here("output","figures","02_tasas_100k.pdf")
qc_csv   <- here::here("output","tables","22_qc_tasas.csv")
invisible(lapply(c(fig1_png, fig1_pdf, fig2_png, fig2_pdf, qc_csv), ensure_dir))

# --- Tema y paleta ------------------------------------------------------------
theme_pearl_lightgrid <- get0("theme_pearl_lightgrid", ifnotfound = NULL)
try({
  src <- here::here("scripts","00_theme_figuras.R")
  if (file.exists(src)) {
    source(src)
    theme_pearl_lightgrid <- get0("theme_pearl_lightgrid", ifnotfound = NULL)
    msg("Tema 'pearl' cargado (00_theme_figuras.R).")
  } else {
    msg("No se encontró 00_theme_figuras.R; uso theme_minimal().")
  }
}, silent = TRUE)
theme_safe <- function() if (is.null(theme_pearl_lightgrid)) ggplot2::theme_minimal() else theme_pearl_lightgrid()

okabe_ito <- c("#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7","#999999")

save_plot <- function(p, file, width = 9, height = 5.2){
  ggplot2::ggsave(filename = file, plot = p, width = width, height = height, dpi = 300,
                  units = "in", limitsize = FALSE)
}

# --- Lectura ------------------------------------------------------------------
stopifnot(file.exists(p_core))
d <- readr::read_csv(p_core, show_col_types = FALSE) |>
  janitor::clean_names() |>
  dplyr::arrange(ano)
if (!"ano" %in% names(d)) stop("[22] Falta columna 'ano' en core.")

# --- Chequeo de columnas necesarias ------------------------------------------
cols_base100 <- c("hc_total","he_total","pct_extranjeros")
cols_tasas   <- c("tasa_hc_100k","tasa_he_100k")
faltan_base100 <- setdiff(cols_base100, names(d))
faltan_tasas   <- setdiff(cols_tasas,   names(d))
if (length(faltan_base100) > 0) msg("AVISO: faltan columnas para base-100: ", paste(faltan_base100, collapse=", "))
if (length(faltan_tasas) > 0)   msg("AVISO: faltan columnas para tasas 100k: ", paste(faltan_tasas, collapse=", "))

# --- Figura 01: Tendencias base-100 ------------------------------------------
if (length(setdiff(cols_base100, faltan_base100)) >= 2){
  base <- d |>
    dplyr::select(tidyselect::all_of(c("ano", cols_base100))) |>
    tidyr::pivot_longer(-ano, names_to = "serie", values_to = "valor") |>
    dplyr::group_by(serie) |>
    dplyr::mutate(
      base = dplyr::first(stats::na.omit(valor)),
      idx100 = 100 * (valor / base)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(serie_lbl = dplyr::recode(
      serie,
      hc_total = "Hechos conocidos",
      he_total = "Hechos esclarecidos",
      pct_extranjeros = "% extranjeros (población)"
    ))
  
  yr_min <- suppressWarnings(min(base$ano, na.rm = TRUE)); yr_max <- suppressWarnings(max(base$ano, na.rm = TRUE))
  has_covid <- any(base$ano %in% 2020:2021)
  
  p1 <- ggplot(base, aes(x = ano, y = idx100, color = serie_lbl)) +
    geom_rect(
      data = if (has_covid) data.frame(xmin = 2019.5, xmax = 2021.5, ymin = -Inf, ymax = Inf) else NULL,
      inherit.aes = FALSE, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = "#efefef", alpha = if (has_covid) 0.4 else 0
    ) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    scale_color_manual(values = okabe_ito[1:3]) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = min(10, yr_max - yr_min + 1)),
                       limits = c(yr_min, yr_max)) +
    scale_y_continuous(labels = scales::label_number(accuracy = 1)) +
    labs(title = "Tendencias base-100",
         subtitle = "Hechos conocidos, esclarecidos y % extranjeros (base = primer año disponible)",
         x = NULL, y = "Índice (base = 100)", color = NULL,
         caption = "Sombreado: 2020–2021 (COVID).") +
    theme_safe()
  
  save_plot(p1, fig1_png); save_plot(p1, fig1_pdf)
  msg("✔ Figura base-100 escrita: ", fig1_png)
} else {
  msg("OMITIDO: no hay suficientes columnas para la figura base-100.")
}

# --- Figura 02: Tasas por 100.000 --------------------------------------------
if (length(setdiff(cols_tasas, faltan_tasas)) >= 1){
  tasas <- d |>
    dplyr::select(tidyselect::all_of(c("ano", cols_tasas))) |>
    tidyr::pivot_longer(-ano, names_to = "serie", values_to = "valor") |>
    dplyr::mutate(serie_lbl = dplyr::recode(
      serie,
      tasa_hc_100k = "Tasa HC (por 100k)",
      tasa_he_100k = "Tasa HE (por 100k)"
    ))
  
  yr_min2 <- suppressWarnings(min(tasas$ano, na.rm = TRUE)); yr_max2 <- suppressWarnings(max(tasas$ano, na.rm = TRUE))
  has_covid2 <- any(tasas$ano %in% 2020:2021)
  
  p2 <- ggplot(tasas, aes(x = ano, y = valor, color = serie_lbl)) +
    geom_rect(
      data = if (has_covid2) data.frame(xmin = 2019.5, xmax = 2021.5, ymin = -Inf, ymax = Inf) else NULL,
      inherit.aes = FALSE, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = "#efefef", alpha = if (has_covid2) 0.4 else 0
    ) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    scale_color_manual(values = okabe_ito[c(2,5)]) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = min(10, yr_max2 - yr_min2 + 1)),
                       limits = c(yr_min2, yr_max2)) +
    scale_y_continuous(labels = scales::label_number(accuracy = 1)) +
    labs(title = "Tasas por 100.000 habitantes",
         subtitle = "Hechos conocidos y esclarecidos",
         x = NULL, y = "Tasa por 100.000", color = NULL,
         caption = "Sombreado: 2020–2021 (COVID).") +
    theme_safe()
  
  save_plot(p2, fig2_png); save_plot(p2, fig2_pdf)
  msg("✔ Figura tasas 100k escrita: ", fig2_png)
} else {
  msg("OMITIDO: no hay columnas de tasas disponibles.")
}

# --- QC de tasas: ¿población en personas o en miles? -------------------------
if (all(c("ano","hc_total","poblacion_total","tasa_hc_100k") %in% names(d))) {
  qc <- d %>%
    transmute(
      ano, hc_total, poblacion_total,
      tasa_calc_ok    = hc_total / poblacion_total * 100000,           # si población = personas
      tasa_calc_miles = hc_total / (poblacion_total * 1000) * 100000,  # si población = miles
      tasa_file       = tasa_hc_100k
    ) %>%
    mutate(
      ratio_ok    = tasa_file / tasa_calc_ok,
      ratio_miles = tasa_file / tasa_calc_miles
    )
  
  med_ok    <- median(qc$ratio_ok,    na.rm = TRUE)
  med_miles <- median(qc$ratio_miles, na.rm = TRUE)
  dev_ok    <- abs(med_ok - 1)
  dev_miles <- abs(med_miles - 1)
  
  msg_dec <- if (is.finite(dev_ok) && dev_ok < 0.05) {
    "[22][QC] PASS: tasas consistentes asumiendo población en PERSONAS (ratio≈1)."
  } else if (is.finite(dev_miles) && dev_miles < 0.05) {
    "[22][QC] WARN: tasas consistentes si población está en MILES. Revisa 'poblacion_total'."
  } else {
    "[22][QC] WARN: discrepancia en tasas (ratio lejos de 1 en ambas hipótesis). Revisa insumos."
  }
  message(msg_dec)
  
  readr::write_csv(qc, qc_csv)
  cat("[22] QC escrito → ", qc_csv, "\n", sep = "")
  suppressWarnings(
    print(
      qc %>% arrange(desc(abs(ratio_ok - 1))) %>%
        select(ano, tasa_file, tasa_calc_ok, ratio_ok) %>% head(5)
    )
  )
} else {
  msg("QC omitido: faltan columnas (ano, hc_total, poblacion_total, tasa_hc_100k).")
}

# --- Mini-check de archivos ---------------------------------------------------
mini_check_22 <- function(){
  figs <- c("01_tendencias_base100.png","01_tendencias_base100.pdf",
            "02_tasas_100k.png","02_tasas_100k.pdf")
  paths <- file.path(here::here("output","figures"), figs)
  ext   <- qc_csv
  cat("\n—— MINI CHECK · 22_figuras ————————————————\n")
  cat("Figuras:\n")
  for (i in seq_along(figs)) {
    cat(sprintf("  - %s: %s\n", figs[i], if (file.exists(paths[i])) "OK" else "NO"))
  }
  cat("QC tasas: ", if (file.exists(ext)) "OK" else "NO", "\n", sep="")
  cat("———————————————————————————————————————————————\n")
}

# Auto-mini-check si se ejecuta interactivamente por source()
if (interactive() && sys.nframe() == 0) {
  mini_check_22()
}

msg("✅ Listo. Figuras en output/figures/ (PNG y PDF) y QC en output/tables/22_qc_tasas.csv.")

