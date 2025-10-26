#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
# -----------------------------------------------------------------------------
# 00_theme_figuras.R — Tema “pearl”, paleta Okabe–Ito y helpers de guardado
# Versión  : v2.2 (2025-10-07)
# Autor    : Jesús Castro · JESUSCASTRODATA
# Licencia : MIT (código) · CC BY 4.0 (docs/figuras)
# Notas    : Sin dependencias externas salvo ggplot2, grid y (opcional) ragg.
#            Usa base::capabilities() (no grDevices::capabilities()).
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(ggplot2)
  library(grid)  # unit()
})

# ======================= Paleta Okabe–Ito (colorblind-safe) ===================
oi_colors <- c(
  black   = "#000000",
  orange  = "#E69F00",
  sky     = "#56B4E9",
  green   = "#009E73",
  yellow  = "#F0E442",
  blue    = "#0072B2",
  vermil  = "#D55E00",
  purple  = "#CC79A7"
)

oi_pal <- function(n = NULL, include_black = FALSE) {
  x <- oi_colors
  if (!include_black) x <- x[names(x) != "black"]
  if (is.null(n)) return(unname(x))
  if (n <= length(x)) return(unname(x[seq_len(n)]))
  # reciclar si piden más
  rep(unname(x), length.out = n)
}

scale_color_oi <- function(n = NULL, include_black = FALSE, ...) {
  ggplot2::scale_color_manual(values = oi_pal(n, include_black), ...)
}
scale_fill_oi <- function(n = NULL, include_black = FALSE, ...) {
  ggplot2::scale_fill_manual(values = oi_pal(n, include_black), ...)
}

# ============================== Tema “pearl” ==================================
PEARL_BG <- "#FAF9F6"
PEARL_GRID_MAJOR <- "#E5E2DA"
PEARL_GRID_MINOR <- "#EEECE6"

theme_pearl_lightgrid <- function(base_size = 12, base_family = NULL) {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    ggplot2::theme(
      plot.background  = ggplot2::element_rect(fill = PEARL_BG, colour = NA),
      panel.background = ggplot2::element_rect(fill = PEARL_BG, colour = NA),
      legend.background= ggplot2::element_rect(fill = PEARL_BG, colour = NA),
      legend.key       = ggplot2::element_rect(fill = PEARL_BG, colour = NA),
      panel.grid.major = ggplot2::element_line(colour = PEARL_GRID_MAJOR, linewidth = 0.4),
      panel.grid.minor = ggplot2::element_line(colour = PEARL_GRID_MINOR, linewidth = 0.25),
      plot.title       = ggplot2::element_text(face = "bold", size = base_size * 1.15),
      legend.title     = ggplot2::element_text(face = "bold")
    )
}

# Set por defecto si se desea (descomenta la siguiente línea en tu sesión)
# ggplot2::theme_set(theme_pearl_lightgrid())

# =========================== Dispositivos seguros =============================
# Selección robusta de dispositivo PDF: usa Cairo si está disponible.
select_pdf_device <- function() {
  has_cairo <- isTRUE(base::capabilities("cairo"))
  if (has_cairo && "cairo_pdf" %in% getNamespaceExports("grDevices")) {
    return(grDevices::cairo_pdf)
  } else {
    return(grDevices::pdf)
  }
}

# Apertura de PNG con ragg si existe; si no, base::png
png_open <- function(filename, width_px, height_px, res = 144, bg = PEARL_BG) {
  if (requireNamespace("ragg", quietly = TRUE)) {
    ragg::agg_png(filename, width = width_px, height = height_px, res = res, background = bg)
  } else {
    grDevices::png(filename, width = width_px, height = height_px, res = res, bg = bg)
  }
}

# Cierre garantizado
dev_close_safe <- function() {
  try(grDevices::dev.off(), silent = TRUE)
}

# ============================ Helpers de guardado =============================
# Guarda PNG + PDF (mismo path base). path_no_ext = "output/figures/mi_figura"
ggsave_pearl_bundle <- function(path_no_ext,
                                plot = ggplot2::last_plot(),
                                width = 12, height = 7, units = "in",
                                dpi = 300, bg = PEARL_BG) {
  dir.create(dirname(path_no_ext), recursive = TRUE, showWarnings = FALSE)
  
  # PNG (en píxeles)
  png_file <- paste0(path_no_ext, ".png")
  png_open(png_file, width_px = width * dpi, height_px = height * dpi, res = dpi, bg = bg)
  on.exit(dev_close_safe(), add = TRUE)
  print(plot)
  dev_close_safe()
  
  # PDF
  pdf_file <- paste0(path_no_ext, ".pdf")
  dev <- select_pdf_device()
  ggplot2::ggsave(filename = pdf_file, plot = plot, width = width, height = height,
                  units = units, device = dev, bg = bg)
  invisible(path_no_ext)
}

# Guarda solo PNG (útil para CI rápido)
ggsave_pearl_png <- function(filename,
                             plot = ggplot2::last_plot(),
                             width = 12, height = 7, units = "in",
                             dpi = 300, bg = PEARL_BG) {
  dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
  # ggsave interpreta width/height en unidades físicas → deja que convierta
  ggplot2::ggsave(filename = filename, plot = plot, width = width, height = height,
                  units = units, dpi = dpi, bg = bg)
  invisible(filename)
}

# Guarda solo PDF (usa select_pdf_device)
ggsave_pearl_pdf <- function(filename,
                             plot = ggplot2::last_plot(),
                             width = 12, height = 7, units = "in", bg = PEARL_BG) {
  dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
  dev <- select_pdf_device()
  ggplot2::ggsave(filename = filename, plot = plot, width = width, height = height,
                  units = units, device = dev, bg = bg)
  invisible(filename)
}

# ========================== Utilidades de anotación ===========================
label_pearl <- function(txt, size = 3, padding_pt = 3, ...) {
  ggplot2::geom_label(ggplot2::aes(label = txt), size = size,
                      label.size = 0.2, label.padding = grid::unit(padding_pt, "pt"), ...)
}

arrow_pearl <- function(length_pt = 6, type = "closed") {
  ggplot2::arrow(length = grid::unit(length_pt, "pt"), type = type)
}

# ============================ Modo silencioso opcional ========================
# Desactiva warnings de guides que ensucian la consola (activar solo si quieres)
gg_quiet <- function(enable = TRUE) {
  if (enable) {
    options(
      ggplot2.discrete.fill = NULL,
      ggplot2.discrete.colour = NULL,
      warn = -1
    )
  } else {
    options(warn = 0)
  }
  invisible(enable)
}

# ============================== Tamaños útiles ================================
fig_sizes <- list(
  # tamaños en pulgadas pensados para 144–300 dpi
  half = c(w = 6,  h = 4),
  wide = c(w = 12, h = 7),
  tall = c(w = 7,  h = 10),
  a4l  = c(w = 11.69, h = 8.27),  # A4 landscape
  a4p  = c(w = 8.27,  h = 11.69)  # A4 portrait
)

# =============================== Ejemplo rápido ===============================
# p <- ggplot(mtcars, aes(wt, mpg, color = factor(cyl))) +
#   geom_point(size = 2) +
#   scale_color_oi() +
#   labs(title = "Demo Okabe–Ito + Pearl") +
#   theme_pearl_lightgrid()
# ggsave_pearl_bundle("output/figures/demo_pearl", p, width = fig_sizes$wide["w"], height = fig_sizes$wide["h"])

