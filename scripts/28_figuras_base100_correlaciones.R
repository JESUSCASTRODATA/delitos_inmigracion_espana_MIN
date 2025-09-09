#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
###############################################################################
# 28_figuras_base100_correlaciones.R — Panel base-100 y correlaciones base-100 por ventana.
#
# Autor: JESUS CASTRO
# Fecha: 2025-09-04
###############################################################################
suppressPackageStartupMessages({
  library(readr); library(dplyr); library(tidyr); library(ggplot2)
  library(here); library(scales); library(patchwork)
})

theme_set(theme_minimal(base_size = 12))
PEARL <- "#FAF9F6"
OI <- c(
  black = "#000000", orange = "#E69F00", sky = "#56B4E9", green = "#009E73",
  yellow = "#F0E442", blue = "#0072B2", vermilion = "#D55E00", purple = "#CC79A7"
)
ensure_dir <- function(path) if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
figdir <- here::here("output/figures"); ensure_dir(figdir)
tbldir <- here::here("output/tables");  ensure_dir(tbldir)

safe_percent <- function(acc = 0.1) {
  function(x) {
    rng <- range(x, na.rm = TRUE)
    sc  <- if (is.finite(rng[2]) && rng[2] > 1.5) 1 else 100
    scales::label_percent(accuracy = acc, scale = sc)(x)
  }
}
b100 <- function(x) { x / dplyr::first(x[is.finite(x)]) * 100 }

mk_smooth_eq <- function(x, y, df) {
  f <- stats::as.formula(paste(y, "~", x))
  fit <- try(stats::lm(f, data = df), silent = TRUE)
  if (inherits(fit, "try-error")) return(list(label = "", x = NA_real_, y = NA_real_))
  sm <- summary(fit)
  b0 <- unname(coef(fit)[1]); b1 <- unname(coef(fit)[2]); r2 <- sm$r.squared
  xr <- range(df[[x]], na.rm = TRUE); yr <- range(df[[y]], na.rm = TRUE)
  list(label = sprintf("y = %.3f + %.3f·x\nR² = %.3f", b0, b1, r2),
       x = xr[1] + 0.03*diff(xr), y = yr[2] - 0.03*diff(yr))
}

save_plot <- function(p, fname, w = 9, h = 5.5) {
  outfile <- here::here("output/figures", fname)
  ggsave(outfile, p, width = w, height = h, dpi = 150)
  message("Figura: ", outfile)
}

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

stats_all <- list()

for (tag in names(windows)) {
  dwin <- windows[[tag]](df)

  base_candidates <- intersect(c("det_tot","det_ext","hc","he","det_tot_rate","det_ext_rate"), names(dwin))
  if (length(base_candidates) >= 2) {
    long_b100 <- dwin |>
      dplyr::select(ano, dplyr::any_of(base_candidates)) |>
      tidyr::drop_na() |>
      tidyr::pivot_longer(-ano, names_to = "serie", values_to = "valor") |>
      dplyr::group_by(serie) |>
      dplyr::arrange(ano, .by_group = TRUE) |>
      dplyr::mutate(valor_b100 = b100(valor)) |>
      dplyr::ungroup()

    present <- unique(as.character(long_b100$serie))
    pal_full <- c(
      det_tot       = OI[["blue"]],
      det_ext       = OI[["sky"]],
      det_tot_rate  = OI[["purple"]],
      det_ext_rate  = OI[["vermilion"]],
      hc            = OI[["green"]],
      he            = OI[["orange"]]
    )
    pal_use <- pal_full[present]
    lab_use <- c(
      det_tot      = "Detenciones totales",
      det_ext      = "Detenciones extranjeros",
      det_tot_rate = "Tasa detenciones totales",
      det_ext_rate = "Tasa detenciones extranjeros",
      hc           = "Hechos conocidos",
      he           = "Hechos esclarecidos"
    )[present]

    p_panel <- ggplot(long_b100, aes(x = ano, y = valor_b100, color = serie)) +
      geom_line(linewidth = 1, na.rm = TRUE) +
      geom_point(size = 2, na.rm = TRUE) +
      scale_color_manual(values = pal_use, breaks = present, labels = lab_use, drop = TRUE) +
      scale_x_continuous(breaks = pretty) +
      labs(x = "Año", y = "Índice (base 100 en primer año de la ventana)",
           color = NULL, title = paste0("Evolución base-100 (", tag, ")")) +
      theme(plot.background = element_rect(fill = PEARL, color = NA),
            panel.background = element_rect(fill = PEARL, color = NA),
            legend.position = "top")
    save_plot(p_panel, sprintf("28_base100_panel_%s.png", tag))
  }

  if (all(c("det_tot","pct_extranjeros_poblacion") %in% names(dwin))) {
    tmp <- dwin |>
      dplyr::select(ano, det_tot, pct_extranjeros_poblacion) |>
      tidyr::drop_na() |>
      dplyr::arrange(ano) |>
      dplyr::mutate(det_tot_b100 = b100(det_tot),
                    pct_b100 = b100(pct_extranjeros_poblacion))

    ann <- mk_smooth_eq("pct_b100", "det_tot_b100", tmp)

    p1 <- ggplot(tmp, aes(x = pct_b100, y = det_tot_b100)) +
      geom_point(size = 3, color = OI[["sky"]]) +
      geom_smooth(method = "lm", se = FALSE, formula = y ~ x, color = OI[["blue"]], linewidth = 1) +
      annotate("text", x = ann$x, y = ann$y, label = ann$label, hjust = 0, vjust = 1, size = 3.3) +
      labs(x = "Población extranjera (base-100)", y = "Detenciones totales (base-100)",
           title = paste0("Base-100: det_tot vs % población extranjera (", tag, ")")) +
      theme(plot.background = element_rect(fill = PEARL, color = NA),
            panel.background = element_rect(fill = PEARL, color = NA))
    save_plot(p1, sprintf("28_base100_scatter_det_tot_vs_pct_%s.png", tag))

    stats_all[[length(stats_all)+1]] <- tibble::tibble(
      ventana = tag, x = "pct_extranjeros_poblacion_b100", y = "det_tot_b100",
      pearson  = suppressWarnings(cor(tmp$pct_b100, tmp$det_tot_b100, method = "pearson")),
      spearman = suppressWarnings(cor(tmp$pct_b100, tmp$det_tot_b100, method = "spearman")),
      kendall  = suppressWarnings(cor(tmp$pct_b100, tmp$det_tot_b100, method = "kendall"))
    )
  }

  if (all(c("det_tot","share_extranjeros") %in% names(dwin))) {
    tmp2 <- dwin |>
      dplyr::select(ano, det_tot, share_extranjeros) |>
      tidyr::drop_na() |>
      dplyr::arrange(ano) |>
      dplyr::mutate(det_tot_b100 = b100(det_tot),
                    share_b100 = b100(share_extranjeros))

    ann2 <- mk_smooth_eq("share_b100", "det_tot_b100", tmp2)

    p2 <- ggplot(tmp2, aes(x = share_b100, y = det_tot_b100)) +
      geom_point(size = 3, color = OI[["green"]]) +
      geom_smooth(method = "lm", se = FALSE, formula = y ~ x, color = OI[["vermilion"]], linewidth = 1) +
      annotate("text", x = ann2$x, y = ann2$y, label = ann2$label, hjust = 0, vjust = 1, size = 3.3) +
      labs(x = "Share de extranjeros en detenciones (base-100)", y = "Detenciones totales (base-100)",
           title = paste0("Base-100: det_tot vs share extranjeros (", tag, ")")) +
      theme(plot.background = element_rect(fill = PEARL, color = NA),
            panel.background = element_rect(fill = PEARL, color = NA))
    save_plot(p2, sprintf("28_base100_scatter_det_tot_vs_share_%s.png", tag))

    stats_all[[length(stats_all)+1]] <- tibble::tibble(
      ventana = tag, x = "share_extranjeros_b100", y = "det_tot_b100",
      pearson  = suppressWarnings(cor(tmp2$share_b100, tmp2$det_tot_b100, method = "pearson")),
      spearman = suppressWarnings(cor(tmp2$share_b100, tmp2$det_tot_b100, method = "spearman")),
      kendall  = suppressWarnings(cor(tmp2$share_b100, tmp2$det_tot_b100, method = "kendall"))
    )
  }

  if (exists("p1") && exists("p2")) {
    grid <- patchwork::wrap_plots(list(p1, p2), ncol = 2)
    save_plot(grid, sprintf("28_base100_grid_%s.png", tag), w = 10, h = 6)
    rm(grid)
  }
  if (exists("p1")) rm(p1)
  if (exists("p2")) rm(p2)

  message("✓ Ventana procesada: ", tag)
}

if (length(stats_all)) {
  stats_tbl <- dplyr::bind_rows(stats_all)
  outfile <- here::here("output/tables", "28_base100_cor_stats.csv")
  readr::write_csv(stats_tbl, outfile)
  message("✓ Stats base-100 exportadas a ", outfile)
}

message("✅ 28 listo: panel base-100, scatters y stats por ventana.")