#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
################################################################################
# 23_figuras_correlaciones.R — Heatmaps de correlaciones (niveles, Δ y parciales)
# Proyecto   : Delitos e Inmigración en España (2010–2023)
# Autor      : Jesús Castro · JESUSCASTRODATA
# Licencia   : MIT (código) · CC BY 4.0 (docs/figuras)
# Fecha      : 2025-10-06
# Descripción:
#   - Lee tablas tidy de correlaciones generadas por 21_correlaciones_niveles_y_deltas.R
#   - Construye matrices simétricas (x,y) con colapso robusto de duplicados
#   - Renderiza heatmaps (Pearson) para:
#       * NIVELES (total y sin COVID)
#       * DELTAS  (total y sin COVID)
#       * PARCIALES (niveles y Δ) si existen
#   - Mini check en consola (conteos, pares y archivos generados)
# Entradas:
#   - output/tables/corr_niveles.csv   (scope, method, x, y, estimate, p_value, ...)
#   - output/tables/corr_deltas.csv    (idem)
#   - output/tables/corr_parciales.csv (scope, x, y, estimate, p_value, n) [opcional]
# Salidas:
#   - output/figures/03_corr_niveles_total.png
#   - output/figures/03_corr_niveles_sin_covid.png
#   - output/figures/03_corr_deltas_total.png
#   - output/figures/03_corr_deltas_sin_covid.png
#   - output/figures/03_corr_parciales_niveles.png   [si aplica]
#   - output/figures/03_corr_parciales_deltas.png    [si aplica]
################################################################################

suppressPackageStartupMessages({
  library(here); library(readr); library(dplyr); library(tidyr); library(janitor)
  library(stringr); library(purrr); library(ggplot2); library(scales)
})

msg <- function(...) message("[23] ", paste0(...))
ensure_dir <- function(p){ dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE); invisible(p) }

# --- Rutas --------------------------------------------------------------------
pth_niv <- here::here("output","tables","corr_niveles.csv")
pth_del <- here::here("output","tables","corr_deltas.csv")
pth_par <- here::here("output","tables","corr_parciales.csv")

f_niv_tot <- here::here("output","figures","03_corr_niveles_total.png")
f_niv_nc  <- here::here("output","figures","03_corr_niveles_sin_covid.png")
f_del_tot <- here::here("output","figures","03_corr_deltas_total.png")
f_del_nc  <- here::here("output","figures","03_corr_deltas_sin_covid.png")
f_par_lvl <- here::here("output","figures","03_corr_parciales_niveles.png")
f_par_del <- here::here("output","figures","03_corr_parciales_deltas.png")

invisible(lapply(c(f_niv_tot,f_niv_nc,f_del_tot,f_del_nc,f_par_lvl,f_par_del), ensure_dir))

# --- Tema y colores -----------------------------------------------------------
theme_pearl_lightgrid <- NULL
try({
  src <- here::here("scripts","00_theme_figuras.R")
  if (file.exists(src)) {
    source(src)
    theme_pearl_lightgrid <- get0("theme_pearl_lightgrid", ifnotfound = NULL)
    msg("Tema 'pearl' cargado (00_theme_figuras.R).")
  } else {
    msg("AVISO: no se encontró 00_theme_figuras.R — uso theme_minimal().")
  }
}, silent = TRUE)
theme_safe <- function() if (is.null(theme_pearl_lightgrid)) ggplot2::theme_minimal(base_size = 11) else theme_pearl_lightgrid()

# Paleta divergente simple para r∈[-1,1]
scale_corr <- function(){
  ggplot2::scale_fill_gradient2(name = "r",
                                low = "#2166AC", mid = "white", high = "#B2182B",
                                limits = c(-1,1), oob = squish)
}

save_plot <- function(p, file, width = 8, height = 6){
  ggplot2::ggsave(filename = file, plot = p, width = width, height = height,
                  dpi = 300, units = "in", limitsize = FALSE)
}

# --- Helpers robustos ---------------------------------------------------------
# Corr tidy esperada: columnas al menos: scope, method, x, y, estimate
build_corr_matrix <- function(corr_tidy, scope_sel, method_sel = "pearson") {
  df <- corr_tidy %>%
    filter(scope == scope_sel, method == method_sel) %>%
    # colapsa duplicados por promedio (evita warnings de pivot_wider)
    group_by(x, y) %>%
    summarise(r = mean(estimate, na.rm = TRUE), .groups = "drop") %>%
    # simetriza (nos quedamos con una por par)
    mutate(x2 = pmin(x, y), y2 = pmax(x, y)) %>%
    distinct(x = x2, y = y2, .keep_all = TRUE) %>%
    select(x, y, r)
  
  if (nrow(df) == 0) return(NULL)
  vars <- sort(unique(c(df$x, df$y)))
  M <- matrix(NA_real_, length(vars), length(vars), dimnames = list(vars, vars))
  diag(M) <- 1
  for (i in seq_len(nrow(df))) {
    xi <- df$x[i]; yi <- df$y[i]; ri <- df$r[i]
    if (!is.na(ri) && xi %in% vars && yi %in% vars) {
      M[xi, yi] <- ri; M[yi, xi] <- ri
    }
  }
  M
}

mat_to_long <- function(M){
  if (is.null(M)) return(tibble())
  as.data.frame(as.table(M), stringsAsFactors = FALSE) %>%
    rename(x = Var1, y = Var2, r = Freq) %>%
    mutate(x = as.character(x), y = as.character(y))
}

plot_corr_heatmap <- function(M, title, subtitle = NULL){
  df <- mat_to_long(M) %>% mutate(is_diag = x == y)
  if (!nrow(df)) return(NULL)
  ggplot(df, aes(x = x, y = y, fill = r)) +
    geom_tile(color = "white", linewidth = 0.2) +
    geom_text(aes(label = ifelse(is_diag, "1", sprintf("%.2f", r))),
              size = 3, alpha = 0.9) +
    scale_corr() +
    coord_equal() +
    labs(title = title, subtitle = subtitle, x = NULL, y = NULL) +
    theme_safe() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      legend.position = "right"
    )
}

# --- Lectura de datos ---------------------------------------------------------
stopifnot(file.exists(pth_niv))
corr_niv <- readr::read_csv(pth_niv, show_col_types = FALSE) %>% clean_names()
if (!all(c("scope","method","x","y","estimate") %in% names(corr_niv))) {
  stop("[23] corr_niveles.csv no trae las columnas mínimas (scope, method, x, y, estimate).")
}
msg("Niveles: filas=", nrow(corr_niv),
    " | scopes: ", paste(sort(unique(corr_niv$scope)), collapse = ", "),
    " | métodos: ", paste(sort(unique(corr_niv$method)), collapse = ", "))

if (file.exists(pth_del)) {
  corr_del <- readr::read_csv(pth_del, show_col_types = FALSE) %>% clean_names()
  msg("Deltas : filas=", nrow(corr_del),
      " | scopes: ", paste(sort(unique(corr_del$scope)), collapse = ", "),
      " | métodos: ", paste(sort(unique(corr_del$method)), collapse = ", "))
} else {
  corr_del <- NULL
  msg("AVISO: no existe corr_deltas.csv — se omiten heatmaps de Δ.")
}

if (file.exists(pth_par)) {
  corr_par <- readr::read_csv(pth_par, show_col_types = FALSE) %>% clean_names()
  # Normalizamos un pseudo 'method' para parciales si no existe
  if (!"method" %in% names(corr_par)) corr_par <- corr_par %>% mutate(method = "partial")
  msg("Parciales: filas=", nrow(corr_par),
      " | scopes: ", paste(sort(unique(corr_par$scope)), collapse = ", "),
      " | métodos: ", paste(sort(unique(corr_par$method)), collapse = ", "))
} else {
  corr_par <- NULL
  msg("AVISO: no existe corr_parciales.csv — se omiten heatmaps parciales.")
}

# --- Construcción de matrices y guardado -------------------------------------
out_files <- c()

# Niveles — total / sin_covid (Pearson)
M_niv_tot <- build_corr_matrix(corr_niv, scope_sel = "total",     method_sel = "pearson")
M_niv_nc  <- build_corr_matrix(corr_niv, scope_sel = "sin_covid", method_sel = "pearson")

if (!is.null(M_niv_tot)) {
  p <- plot_corr_heatmap(M_niv_tot, "Correlaciones en niveles (Pearson)", "Ventana: total (2010–2023)")
  if (!is.null(p)) { save_plot(p, f_niv_tot); out_files <- c(out_files, f_niv_tot); msg("✔ Figura: ", f_niv_tot) }
}
if (!is.null(M_niv_nc)) {
  p <- plot_corr_heatmap(M_niv_nc, "Correlaciones en niveles (Pearson)", "Ventana: sin COVID (2010–2019 & 2022–2023)")
  if (!is.null(p)) { save_plot(p, f_niv_nc); out_files <- c(out_files, f_niv_nc); msg("✔ Figura: ", f_niv_nc) }
}

# Deltas — total / sin_covid (Pearson)
if (!is.null(corr_del)) {
  M_del_tot <- build_corr_matrix(corr_del, scope_sel = "total",     method_sel = "pearson")
  M_del_nc  <- build_corr_matrix(corr_del, scope_sel = "sin_covid", method_sel = "pearson")
  if (!is.null(M_del_tot)) {
    p <- plot_corr_heatmap(M_del_tot, "Correlaciones en variaciones (Pearson)", "Ventana: total (2010–2023)")
    if (!is.null(p)) { save_plot(p, f_del_tot); out_files <- c(out_files, f_del_tot); msg("✔ Figura: ", f_del_tot) }
  }
  if (!is.null(M_del_nc)) {
    p <- plot_corr_heatmap(M_del_nc, "Correlaciones en variaciones (Pearson)", "Ventana: sin COVID (2010–2019 & 2022–2023)")
    if (!is.null(p)) { save_plot(p, f_del_nc); out_files <- c(out_files, f_del_nc); msg("✔ Figura: ", f_del_nc) }
  }
}

# Parciales — niveles y deltas (si existen)
if (!is.null(corr_par)) {
  # Intentamos separar por método si el script 21 los guardó con distintos method
  meths <- unique(corr_par$method)
  # Niveles parciales
  if (any(meths %in% c("partial","niveles","partial_niveles"))) {
    sel_m <- intersect(meths, c("partial","niveles","partial_niveles"))[1]
    M_par_lvl <- build_corr_matrix(corr_par, scope_sel = "total", method_sel = sel_m)
    if (!is.null(M_par_lvl)) {
      p <- plot_corr_heatmap(M_par_lvl, "Correlaciones parciales (niveles)", "Control: PIB pc, paro joven, AROPE (total)")
      if (!is.null(p)) { save_plot(p, f_par_lvl); out_files <- c(out_files, f_par_lvl); msg("✔ Figura: ", f_par_lvl) }
    }
  }
  # Deltas parciales
  if (any(meths %in% c("partial_delta","deltas"))) {
    sel_m <- intersect(meths, c("partial_delta","deltas"))[1]
    M_par_del <- build_corr_matrix(corr_par, scope_sel = "total", method_sel = sel_m)
    if (!is.null(M_par_del)) {
      p <- plot_corr_heatmap(M_par_del, "Correlaciones parciales (Δ)", "Control: Δ ln PIB pc (total)")
      if (!is.null(p)) { save_plot(p, f_par_del); out_files <- c(out_files, f_par_del); msg("✔ Figura: ", f_par_del) }
    }
  }
}

# --- Mini check en consola ----------------------------------------------------
cat("\n—— MINI CHECK · 23_figuras_correlaciones ————————————————\n")
exist_tbl <- tibble::tibble(
  archivo = c(basename(f_niv_tot), basename(f_niv_nc), basename(f_del_tot), basename(f_del_nc),
              basename(f_par_lvl), basename(f_par_del)),
  path    = c(f_niv_tot, f_niv_nc, f_del_tot, f_del_nc, f_par_lvl, f_par_del),
  existe  = file.exists(c(f_niv_tot, f_niv_nc, f_del_tot, f_del_nc, f_par_lvl, f_par_del))
) %>% filter(existe)
print(exist_tbl, n = nrow(exist_tbl))

# Top 5 pares por |r| en cada bloque disponible
topN <- function(df, scope_sel, method_sel = "pearson", n = 5){
  if (is.null(df)) return(tibble())
  df %>% filter(scope == scope_sel, method == method_sel) %>%
    group_by(x, y) %>%
    summarise(r = mean(estimate, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(abs(r))) %>%
    head(n)
}
if (nrow(exist_tbl)) {
  if (file.exists(pth_niv)) {
    cat("\nTop Pearson (niveles · total):\n"); print(topN(corr_niv, "total"), n=5)
  }
  if (!is.null(corr_del)) {
    cat("\nTop Pearson (deltas  · total):\n"); print(topN(corr_del, "total"), n=5)
  }
  if (!is.null(corr_par)) {
    cat("\nTop parciales (niveles · total):\n"); print(topN(corr_par %>% mutate(method="pearson"), "total"), n=5)
  }
}
cat("———————————————————————————————————————————————\n\n")

msg("✅ Listo. Heatmaps generados en output/figures/.")
