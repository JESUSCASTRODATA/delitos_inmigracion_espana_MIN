# -*- coding: UTF-8 -*-
###############################################################################
# 28c_make_heatmap_correlaciones.R — Heatmaps y matrices de correlaciones
# Proyecto: Delitos e Inmigración en España (2010–2023)
# Autor: JESUS CASTRO
# Fecha: 2025-09-07
# Descripción:
#   - Lee output/tables/_debug_cor_hc_pop_prepost.csv (script 23c).
#   - Genera heatmaps (PNG) para NIVELES (base-100) y ΔLOG (variaciones).
#   - Exporta matrices CSV y HTML (gt) con Pearson/Spearman/Kendall por ventana.
# Salidas:
#   - output/figures/correlaciones_heatmap_niveles.png
#   - output/figures/correlaciones_heatmap_dlog.png
#   - output/tables/correlaciones_matrix_niveles.csv (+ .html)
#   - output/tables/correlaciones_matrix_dlog.csv (+ .html)
# Requisitos: here, readr, dplyr, tidyr, ggplot2, gt
###############################################################################

suppressPackageStartupMessages({
  library(here); library(readr); library(dplyr); library(tidyr)
  library(ggplot2); library(gt)
})

# 0) Ancla del proyecto y directorios
try(here::i_am("docs/informe_delitos_inmigracion_espana_2010_2023.Rmd"), silent = TRUE)
dir.create(here::here("output","figures"), showWarnings = FALSE, recursive = TRUE)
dir.create(here::here("output","tables"),  showWarnings = FALSE, recursive = TRUE)

# 1) Entrada requerida
in_csv <- here::here("output","tables","_debug_cor_hc_pop_prepost.csv")
if (!file.exists(in_csv)) {
  stop("No existe: ", in_csv,
       "\nEjecuta antes: source('scripts/23c_check_correlaciones_hc_pobl_ext.R')")
}
df <- readr::read_csv(in_csv, show_col_types = FALSE)

# 2) Orden y etiquetas
win_levels <- c(
  "2010–2019 (pre)",
  "2020–2021 (COVID)",
  "2021–2023 (post)",
  "sin COVID (2010–2019 & 2022–2023)",
  "2010–2023 (completa)"
)
met_levels <- c("pearson","spearman","kendall")
met_labels <- c(pearson = "Pearson", spearman = "Spearman", kendall = "Kendall")

# Paleta Okabe–Ito (daltónico-segura)
OKABE_BLUE  <- "#0072B2"   # negativo
OKABE_RED   <- "#D55E00"   # positivo
OKABE_WHITE <- "#FFFFFF"   # cero

# 3) Función generadora con namespacing explícito
mk_heatmap <- function(dfin, tipo_sel, out_png, out_csv) {
  if (!tipo_sel %in% unique(dfin$tipo)) {
    stop("No encuentro el tipo '", tipo_sel, "' en 'tipo'. Valores: ",
         paste(sort(unique(dfin$tipo)), collapse=", "))
  }
  
  df_sub <- dfin |>
    dplyr::filter(.data$tipo == tipo_sel) |>
    dplyr::mutate(ventana = factor(.data$ventana, levels = win_levels)) |>
    dplyr::arrange(.data$ventana)
  
  # Matriz para CSV/HTML (usa all_of para evitar ambigüedades)
  cols_mat <- c("ventana","n","pearson","spearman","kendall")
  if (!all(cols_mat %in% names(df_sub))) {
    stop("Faltan columnas en df_sub: ",
         paste(setdiff(cols_mat, names(df_sub)), collapse=", "),
         "\nDisponibles: ", paste(names(df_sub), collapse=", "))
  }
  mat <- df_sub |>
    dplyr::select(dplyr::all_of(cols_mat))
  readr::write_csv(mat, out_csv)
  
  # Long para heatmap
  long <- df_sub |>
    dplyr::select(dplyr::all_of(c("ventana","n")), dplyr::all_of(met_levels)) |>
    tidyr::pivot_longer(cols = dplyr::all_of(met_levels),
                        names_to = "metric", values_to = "value") |>
    dplyr::mutate(
      metric = factor(.data$metric, levels = met_levels, labels = met_labels),
      ylab   = paste0(as.character(.data$ventana), " (n=", .data$n, ")"),
      lab    = sprintf("%.3f%s", .data$value, ifelse(.data$n < 4, "\u2020", "")),
      labcol = ifelse(abs(.data$value) > 0.6, "#FFFFFF", "#111827")
    )
  
  p <- ggplot(long, aes(x = metric, y = ylab, fill = value)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = lab, color = I(labcol)), size = 3.6) +
    scale_fill_gradient2(
      low = OKABE_BLUE, mid = OKABE_WHITE, high = OKABE_RED,
      midpoint = 0, limits = c(-1, 1), breaks = seq(-1, 1, by = 0.5),
      name = "Coeficiente"
    ) +
    labs(
      x = "Métrica", y = "Ventana (n)",
      title = paste0("Correlaciones — ", tipo_sel),
      subtitle = "Azul: negativa · Blanco: 0 · Rojo: positiva  ·  †: n<4 (interpretar con cautela)",
      caption = "Fuente: output/tables/_debug_cor_hc_pop_prepost.csv · Paleta Okabe–Ito"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid = element_blank(),
      axis.title.x = element_text(margin = margin(t = 8)),
      axis.title.y = element_text(margin = margin(r = 8)),
      plot.title = element_text(face = "bold")
    )
  
  ggsave(filename = out_png, plot = p, width = 9.5, height = 5, dpi = 200, bg = "white")
  
  # HTML con gt
  html_path <- sub("\\.csv$", ".html", out_csv)
  mat |>
    gt::gt() |>
    gt::fmt_number(columns = where(is.numeric), decimals = 3,
                   sep_mark = ".", dec_mark = ",") |>
    gt::tab_header(
      title = md(paste0("Correlaciones — ", tipo_sel)),
      subtitle = md("Pearson / Spearman / Kendall · Ventanas temporales (†: n<4)")
    ) |>
    gt::gtsave(html_path)
}

# 4) Ejecutar para NIVELES y ΔLOG
mk_heatmap(
  dfin     = df,
  tipo_sel = "niveles",
  out_png  = here::here("output","figures","correlaciones_heatmap_niveles.png"),
  out_csv  = here::here("output","tables","correlaciones_matrix_niveles.csv")
)

mk_heatmap(
  dfin     = df,
  tipo_sel = "Δlog",
  out_png  = here::here("output","figures","correlaciones_heatmap_dlog.png"),
  out_csv  = here::here("output","tables","correlaciones_matrix_dlog.csv")
)

message("\nHecho:\n- output/figures/correlaciones_heatmap_niveles.png",
        "\n- output/figures/correlaciones_heatmap_dlog.png",
        "\n- output/tables/correlaciones_matrix_niveles.csv (+ .html)",
        "\n- output/tables/correlaciones_matrix_dlog.csv (+ .html)")
