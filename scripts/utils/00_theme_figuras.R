#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
# -----------------------------------------------------------------------------
# Proyecto   : Delitos e Inmigración en España (2010–2023)
# Script     : 00_theme_figuras.R
# Versión    : v1.3 (2025-09-22)
# Autor      : Jesús Castro · JESUSCASTRODATA
# Licencia   : MIT (código) · CC BY 4.0 (docs/figuras)
# Descripción: Tema de figuras (fondo perla) y paleta Okabe–Ito (colorblind-safe).
#              Utilidades para guardar figuras con fondo homogéneo (PNG/SVG/PDF)
#              y escalas accesibles (viridis/cividis, formas/linetypes).
# Notas      : Sin efectos secundarios (no altera theme global).
# Dependencias: ggplot2 (y viridisLite para escalas viridis), svglite (SVG)
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Falta 'ggplot2'. Instálalo con renv::restore() o install.packages('ggplot2').")
  }
})

# --- Constantes de estilo -----------------------------------------------------
PEARL <- "#FAF9F6"  # fondo homogéneo de las figuras
OI <- c(            # Okabe–Ito + gris neutro
  "#000000", "#E69F00", "#56B4E9", "#009E73",
  "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999"
)

# Paleta configurable (se puede excluir negro y/o gris)
palette_okabe_ito <- function(include_grey = TRUE, include_black = TRUE) {
  base <- if (isTRUE(include_black)) OI else OI[-1]
  if (isTRUE(include_grey)) base else base[base != "#999999"]
}

# --- Helper compat con ggplot2 >=/< 3.4 --------------------------------------
.has_linewidth <- function() {
  utils::packageVersion("ggplot2") >= "3.4.0"
}

# --- Temas -------------------------------------------------------------------
theme_pearl <- function(base_size = 12, base_family = NULL) {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.background  = ggplot2::element_rect(fill = PEARL, colour = NA),
      panel.background = ggplot2::element_rect(fill = PEARL, colour = NA),
      legend.background= ggplot2::element_rect(fill = PEARL, colour = NA),
      legend.key       = ggplot2::element_rect(fill = PEARL, colour = NA)
    )
}

theme_pearl_lightgrid <- function(base_size = 12, base_family = NULL) {
  g <- theme_pearl(base_size, base_family)
  if (.has_linewidth()) {
    g + ggplot2::theme(
      panel.grid.major = ggplot2::element_line(colour = "#E5E2DA", linewidth = 0.4),
      panel.grid.minor = ggplot2::element_line(colour = "#EEECE6", linewidth = 0.25),
      plot.title       = ggplot2::element_text(face = "bold", size = base_size * 1.15),
      legend.title     = ggplot2::element_text(face = "bold"),
      legend.text      = ggplot2::element_text(size = base_size * 0.95)
    )
  } else {
    g + ggplot2::theme(
      panel.grid.major = ggplot2::element_line(colour = "#E5E2DA", size = 0.4),
      panel.grid.minor = ggplot2::element_line(colour = "#EEECE6", size = 0.25),
      plot.title       = ggplot2::element_text(face = "bold", size = base_size * 1.15),
      legend.title     = ggplot2::element_text(face = "bold"),
      legend.text      = ggplot2::element_text(size = base_size * 0.95)
    )
  }
}

# --- Scales discretas (Okabe–Ito) --------------------------------------------
scale_color_oi <- function(..., include_grey = TRUE, include_black = TRUE) {
  ggplot2::scale_color_manual(values = palette_okabe_ito(include_grey, include_black), ...)
}
scale_fill_oi <- function(..., include_grey = TRUE, include_black = TRUE) {
  ggplot2::scale_fill_manual(values = palette_okabe_ito(include_grey, include_black), ...)
}

# --- Gradientes colorblind-safe (viridis/cividis) -----------------------------
scale_color_viridis_c_safe <- function(...) ggplot2::scale_color_viridis_c(..., option = "cividis")
scale_fill_viridis_c_safe  <- function(...) ggplot2::scale_fill_viridis_c(...,  option = "cividis")
scale_color_viridis_d_safe <- function(...) ggplot2::scale_color_viridis_d(..., option = "cividis")
scale_fill_viridis_d_safe  <- function(...) ggplot2::scale_fill_viridis_d(...,  option = "cividis")

# --- Redundancia visual (accesibilidad: b/n, daltónicos) ---------------------
scale_shape_accessible <- function(...) {
  ggplot2::scale_shape_manual(values = c(16, 17, 15, 3, 7, 8), ...)
}
scale_linetype_accessible <- function(...) {
  ggplot2::scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash", "twodash", "longdash"), ...)
}

# --- Guardado con fondo homogéneo --------------------------------------------
ggsave_pearl <- function(filename,
                         plot = ggplot2::last_plot(),
                         width = 12, height = 7, dpi = 300,
                         units = "in", ...) {
  dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(filename, plot,
                  width = width, height = height, dpi = dpi,
                  units = units, bg = PEARL, ...)
  invisible(filename)
}

# Exportación vectorial con comprobaciones amistosas
ggsave_pearl_svg <- function(filename,
                             plot = ggplot2::last_plot(),
                             width = 12, height = 7, units = "in") {
  if (!requireNamespace("svglite", quietly = TRUE)) {
    stop("Para SVG necesitas 'svglite'. Instálalo con renv::restore() o install.packages('svglite').")
  }
  dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(filename, plot,
                  width = width, height = height, units = units,
                  bg = PEARL, device = "svg")
  invisible(filename)
}

ggsave_pearl_pdf <- function(filename,
                             plot = ggplot2::last_plot(),
                             width = 12, height = 7, units = "in") {
  dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
  # En Windows, capabilities("cairo") es más fiable que inspeccionar exports
  dev <- if (isTRUE(grDevices::capabilities("cairo"))) grDevices::cairo_pdf else "pdf"
  ggplot2::ggsave(filename, plot,
                  width = width, height = height, units = units,
                  bg = PEARL, device = dev)
  invisible(filename)
}

# --- Ejemplo (no ejecuta por defecto) ----------------------------------------
# p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, color = factor(cyl))) +
#        ggplot2::geom_point(size = 3) +
#        theme_pearl_lightgrid() +
#        scale_color_oi(include_black = FALSE) +
#        scale_shape_accessible()
# ggsave_pearl(here::here("output","figures","demo_okabe_ito.png"), p)
# ggsave_pearl_svg(here::here("output","figures","demo_okabe_ito.svg"), p)
# ggsave_pearl_pdf(here::here("output","figures","demo_okabe_ito.pdf"), p)

