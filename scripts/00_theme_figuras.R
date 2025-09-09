# -*- coding: UTF-8 -*-
###############################################################################
# 00_theme_figuras.R — Tema unificado (perla + Okabe–Ito)
# Autor: JESUS CASTRO
# Fecha: 2025-09-04
###############################################################################
PEARL <- "#FAF9F6"
OI <- c("#000000","#E69F00","#56B4E9","#009E73",
        "#F0E442","#0072B2","#D55E00","#CC79A7","#999999")

theme_pearl <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.background  = ggplot2::element_rect(fill = PEARL, colour = NA),
      panel.background = ggplot2::element_rect(fill = PEARL, colour = NA),
      legend.background= ggplot2::element_rect(fill = PEARL, colour = NA),
      legend.key       = ggplot2::element_rect(fill = PEARL, colour = NA)
    )
}

ggsave_pearl <- function(filename, plot = ggplot2::last_plot(),
                         width = 12, height = 7, dpi = 300, units = "in", ...) {
  if (!dir.exists(dirname(filename))) dir.create(dirname(filename), recursive = TRUE)
  ggplot2::ggsave(filename, plot, width = width, height = height,
                  dpi = dpi, units = units, bg = PEARL, ...)
}

# (Opcional) escalas rápidas
scale_color_oi <- function(...) ggplot2::scale_color_manual(values = OI, ...)
scale_fill_oi  <- function(...) ggplot2::scale_fill_manual(values = OI, ...)
