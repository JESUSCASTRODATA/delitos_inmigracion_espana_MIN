#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
###############################################################################
# 22_figuras_tendencias.R — Figuras de tendencias por ventana (tasas, share y macro base-100).
#
# Autor: JESUS CASTRO
# Fecha: 2025-09-04
###############################################################################
suppressPackageStartupMessages({
  library(readr); library(dplyr); library(tidyr); library(ggplot2)
  library(here); library(scales); library(patchwork)
})

theme_set(theme_minimal(base_size = 12))
pearl <- "#FAF9F6"
okabe_ito <- c(
  black = "#000000", orange = "#E69F00", sky = "#56B4E9", green = "#009E73",
  yellow = "#F0E442", blue = "#0072B2", vermilion = "#D55E00", purple = "#CC79A7"
)

ensure_dir <- function(path) if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
figdir <- here::here("output/figures"); ensure_dir(figdir)
tbldir <- here::here("output/tables");  ensure_dir(tbldir)

infile <- here::here("data/processed/series.csv")
stopifnot(file.exists(infile))
df <- readr::read_csv(infile, show_col_types = FALSE)

safe_percent <- function(acc = 0.1) {
  function(x) {
    rng <- range(x, na.rm = TRUE)
    sc  <- if (is.finite(rng[2]) && rng[2] > 1.5) 1 else 100
    scales::label_percent(accuracy = acc, scale = sc)(x)
  }
}

# --- Helper para normalizar unidades (% -> fracción 0–1) ---
norm_prop <- function(v) ifelse(is.na(v), NA_real_, ifelse(v > 1.5, v/100, v))

windows <- list(
  "2010_2023"      = function(d) d,
  "2010_2019_pre"  = function(d) d %>% dplyr::filter(ano <= 2019),
  "2020_2023_post" = function(d) d %>% dplyr::filter(ano >= 2020),
  "sin_covid"      = function(d) {
    if ("covid_2020_2021" %in% names(d)) d %>% dplyr::filter(!covid_2020_2021 %in% c(1, TRUE))
    else d
  }
)

save_plot <- function(p, fname, w = 9, h = 5.5) {
  outfile <- here::here("output/figures", fname)
  ggsave(outfile, p, width = w, height = h, dpi = 150)
  message("Figura: ", outfile)
}

stats_all <- list()

for (tag in names(windows)) {
  dwin <- windows[[tag]](df)
  
  # --- Normaliza unidades de 'share' y '% población' a fracción 0–1 (anti % duplicado) ---
  if (all(c("det_ext","det_tot") %in% names(dwin))) {
    dwin <- dwin %>% mutate(share_extranjeros = det_ext / det_tot) # 0–1
  } else if ("share_extranjeros" %in% names(dwin)) {
    dwin <- dwin %>% mutate(share_extranjeros = norm_prop(share_extranjeros))
  }
  if (all(c("poblacion_extranjera","poblacion_total") %in% names(dwin))) {
    dwin <- dwin %>% mutate(pct_extranjeros_poblacion = poblacion_extranjera / poblacion_total) # 0–1
  } else if ("pct_extranjeros_poblacion" %in% names(dwin)) {
    dwin <- dwin %>% mutate(pct_extranjeros_poblacion = norm_prop(pct_extranjeros_poblacion))
  }
  if ("share_extranjeros" %in% names(dwin)) {
    rng <- range(dwin$share_extranjeros, na.rm = TRUE)
    message(sprintf("[QA] share_extranjeros rango: [%.3f, %.3f]", rng[1], rng[2]))
  }
  if ("pct_extranjeros_poblacion" %in% names(dwin)) {
    rng <- range(dwin$pct_extranjeros_poblacion, na.rm = TRUE)
    message(sprintf("[QA] pct_extranjeros_poblacion rango: [%.3f, %.3f]", rng[1], rng[2]))
  }
  # -----------------------------------------------------------------------------
  
  # 1) Tasas de detenciones
  if (all(c("det_tot_rate","det_ext_rate","ano") %in% names(dwin))) {
    tmp <- dwin %>%
      dplyr::select(ano, det_tot_rate, det_ext_rate) %>%
      tidyr::drop_na() %>%
      tidyr::pivot_longer(-ano, names_to = "serie", values_to = "valor")
    present <- unique(as.character(tmp$serie))
    pal_rates_full <- c(
      det_tot_rate = okabe_ito[["blue"]],
      det_ext_rate = okabe_ito[["sky"]]
    )
    pal_rates <- pal_rates_full[present]
    labs_rates <- c(
      det_tot_rate = "Detenciones totales (tasa)",
      det_ext_rate = "Detenciones extranjeros (tasa)"
    )[present]
    
    p_rates <- ggplot(tmp, aes(x = ano, y = valor, color = serie)) +
      geom_line(linewidth = 1, na.rm = TRUE) +
      geom_point(size = 2, na.rm = TRUE) +
      scale_color_manual(values = pal_rates, breaks = present, labels = labs_rates, drop = TRUE) +
      scale_x_continuous(breaks = pretty) +
      scale_y_continuous(labels = label_number(accuracy = 1)) +
      labs(x = "Año", y = "Tasa por 100.000 hab.", color = NULL,
           title = paste0("Tendencia de tasas de detenciones (", tag, ")")) +
      theme(plot.background = element_rect(fill = pearl, color = NA),
            panel.background = element_rect(fill = pearl, color = NA),
            legend.position = "top")
    save_plot(p_rates, sprintf("22_trend_det_rates_%s.png", tag))
  }
  
  # 2) Share/Pct de extranjeros
  cols_share <- intersect(c("share_extranjeros","pct_extranjeros_poblacion"), names(dwin))
  if (length(cols_share) >= 1) {
    tmp <- dwin %>%
      dplyr::select(any_of(c("ano", cols_share))) %>%
      tidyr::drop_na() %>%
      tidyr::pivot_longer(-ano, names_to = "serie", values_to = "valor")
    present <- unique(as.character(tmp$serie))
    pal_share_full <- c(
      share_extranjeros         = okabe_ito[["green"]],
      pct_extranjeros_poblacion = okabe_ito[["orange"]]
    )
    pal_share  <- pal_share_full[present]
    labs_share <- c(
      share_extranjeros         = "Share de extranjeros en detenciones",
      pct_extranjeros_poblacion = "% de población extranjera"
    )[present]
    
    p_share <- ggplot(tmp, aes(x = ano, y = valor, color = serie)) +
      geom_line(linewidth = 1, na.rm = TRUE) +
      geom_point(size = 2, na.rm = TRUE) +
      scale_color_manual(values = pal_share, breaks = present, labels = labs_share, drop = TRUE) +
      scale_x_continuous(breaks = pretty) +
      scale_y_continuous(labels = safe_percent(0.1)) +
      labs(x = "Año", y = "%", color = NULL,
           title = paste0("Share de extranjeros en detenciones y población (", tag, ")")) +
      theme(plot.background = element_rect(fill = pearl, color = NA),
            panel.background = element_rect(fill = pearl, color = NA),
            legend.position = "top")
    save_plot(p_share, sprintf("22_trend_share_pct_%s.png", tag))
  }
  
  # 3) Macro sociales base-100
  cols_macro <- intersect(c("arope","paro_15_29","pib_pc_real"), names(dwin))
  if (length(cols_macro) >= 2) {
    macro <- dwin %>%
      dplyr::select(ano, any_of(cols_macro)) %>%
      tidyr::drop_na() %>%
      tidyr::pivot_longer(-ano, names_to = "serie", values_to = "valor") %>%
      dplyr::group_by(serie) %>%
      dplyr::arrange(ano, .by_group = TRUE) %>%
      dplyr::mutate(valor_b100 = 100 * valor / dplyr::first(valor)) %>%
      dplyr::ungroup()
    
    present <- unique(as.character(macro$serie))
    pal_macro_full <- c(
      arope       = okabe_ito[["vermilion"]],
      paro_15_29  = okabe_ito[["purple"]],
      pib_pc_real = okabe_ito[["black"]]
    )
    pal_macro  <- pal_macro_full[present]
    labs_macro <- c(
      arope      = "AROPE (b100)",
      paro_15_29 = "Paro 15–29 (b100)",
      pib_pc_real= "PIB pc real (b100)"
    )[present]
    
    p_macro <- ggplot(macro, aes(x = ano, y = valor_b100, color = serie)) +
      geom_line(linewidth = 1, na.rm = TRUE) +
      geom_point(size = 2, na.rm = TRUE) +
      scale_color_manual(values = pal_macro, breaks = present, labels = labs_macro, drop = TRUE) +
      scale_x_continuous(breaks = pretty) +
      labs(x = "Año", y = "Índice (base 100 en primer año disponible)", color = NULL,
           title = paste0("Indicadores macro sociales (base-100, ", tag, ")")) +
      theme(plot.background = element_rect(fill = pearl, color = NA),
            panel.background = element_rect(fill = pearl, color = NA),
            legend.position = "top")
    save_plot(p_macro, sprintf("22_trend_macro_base100_%s.png", tag))
  }
  
  # 4) Small multiples
  vars_facets <- intersect(c("det_tot","det_ext","det_tot_rate","det_ext_rate",
                             "share_extranjeros","pct_extranjeros_poblacion"),
                           names(dwin))
  if (length(vars_facets) >= 2) {
    long <- dwin %>%
      dplyr::select(ano, any_of(vars_facets)) %>%
      tidyr::drop_na() %>%
      tidyr::pivot_longer(-ano, names_to = "serie", values_to = "valor")
    
    p_facets <- ggplot(long, aes(x = ano, y = valor)) +
      geom_line(linewidth = 0.9, color = okabe_ito[["blue"]], na.rm = TRUE) +
      geom_point(size = 1.8,  color = okabe_ito[["sky"]],  na.rm = TRUE) +
      facet_wrap(~ serie, scales = "free_y", ncol = 2) +
      scale_x_continuous(breaks = pretty) +
      labs(x = "Año", y = NULL, title = paste0("Evolución: small multiples (", tag, ")")) +
      theme(plot.background = element_rect(fill = pearl, color = NA),
            panel.background = element_rect(fill = pearl, color = NA))
    save_plot(p_facets, sprintf("22_trend_small_multiples_%s.png", tag))
  }
  
  # 5) Stats simples (correlaciones)
  cols_stat <- intersect(c("det_tot","pct_extranjeros_poblacion","share_extranjeros"), names(dwin))
  if (all(c("det_tot","pct_extranjeros_poblacion") %in% cols_stat)) {
    dx <- dwin %>% dplyr::select(det_tot, pct_extranjeros_poblacion) %>% tidyr::drop_na()
    stats_all[[length(stats_all)+1]] <- tibble::tibble(
      ventana = tag, x = "det_tot", y = "pct_extranjeros_poblacion",
      pearson = suppressWarnings(cor(dx[[1]], dx[[2]], use = "pairwise.complete.obs", method = "pearson")),
      spearman = suppressWarnings(cor(dx[[1]], dx[[2]], use = "pairwise.complete.obs", method = "spearman")),
      kendall = suppressWarnings(cor(dx[[1]], dx[[2]], use = "pairwise.complete.obs", method = "kendall"))
    )
  }
  if (all(c("det_tot","share_extranjeros") %in% cols_stat)) {
    dx <- dwin %>% dplyr::select(det_tot, share_extranjeros) %>% tidyr::drop_na()
    stats_all[[length(stats_all)+1]] <- tibble::tibble(
      ventana = tag, x = "det_tot", y = "share_extranjeros",
      pearson = suppressWarnings(cor(dx[[1]], dx[[2]], use = "pairwise.complete.obs", method = "pearson")),
      spearman = suppressWarnings(cor(dx[[1]], dx[[2]], use = "pairwise.complete.obs", method = "spearman")),
      kendall = suppressWarnings(cor(dx[[1]], dx[[2]], use = "pairwise.complete.obs", method = "kendall"))
    )
  }
  
  message("✓ Guardadas figuras para ventana: ", tag)
}

if (length(stats_all)) {
  stats_tbl <- dplyr::bind_rows(stats_all)
  outfile <- here::here("output/tables", "22_cor_totales_stats.csv")
  readr::write_csv(stats_tbl, outfile)
  message("✓ Stats exportadas a ", outfile)
}

message("✅ Hecho. Figuras en output/figures/ y stats en output/tables/")
