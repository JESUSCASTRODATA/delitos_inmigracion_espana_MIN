#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
# -----------------------------------------------------------------------------
# Proyecto   : Delitos e Inmigración en España (2010–2023)
# Script     : 26a_plot_granger_delta.R
# Versión    : v3.4 (2025-10-06)
# Autor      : Jesús Castro · JESUSCASTRODATA
# Licencia   : MIT (código) · CC BY 4.0 (docs/figuras)
# Descripción: Precedencia temporal en diferencias (Δ / VAR bivariado) con FDR (BH).
#              Estética limpia, ejes ocultos, anti-solapes (ggrepel si disponible),
#              selección de flechas con FDR ≤ 10% y opacidad por diagnóstico PASS.
# Dependencias: readr, dplyr, janitor, ggplot2, stringr, glue, ragg
# Opcional   : ggrepel (mejor etiquetas), svglite (SVG)
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(janitor)
  library(ggplot2); library(stringr); library(glue)
})
stopifnot(requireNamespace("ragg", quietly = TRUE))

# ---------- Raíz robusta desde la ubicación del script ----------
get_thisfile <- function(){
  cmd <- commandArgs(trailingOnly = FALSE)
  i <- grep("^--file=", cmd)
  if (length(i)) return(normalizePath(sub("^--file=", "", cmd[i[1]])))
  if (!is.null(sys.frames()[[1]]$ofile)) return(normalizePath(sys.frames()[[1]]$ofile))
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    p <- tryCatch(rstudioapi::getActiveDocumentContext()$path, error = function(e) "")
    if (nzchar(p)) return(normalizePath(p))
  }
  normalizePath(".")
}
THISFILE <- get_thisfile()
ROOT     <- normalizePath(file.path(dirname(THISFILE), ".."), winslash = "/")
say <- function(...) cat("[26a] ", paste0(...), "\n")

# ---------- Tema “pearl” (fallback) ----------
theme_pearl_lightgrid <- if (exists("theme_pearl_lightgrid")) theme_pearl_lightgrid else
  function(base_size=12, base_family=NULL){
    theme_minimal(base_size=base_size, base_family=base_family) +
      theme(plot.background=element_rect(fill="#FAF9F6", colour=NA),
            panel.background=element_rect(fill="#FAF9F6", colour=NA),
            legend.background=element_rect(fill="#FAF9F6", colour=NA),
            legend.key=element_rect(fill="#FAF9F6", colour=NA))
  }

# ---------- Etiquetas amigables ----------
lab_var <- function(x){
  x <- as.character(x)
  dplyr::case_when(
    # niveles
    x %in% c("pct_extran","pct_extranjeros") ~ "% extranjeros",
    x %in% c("share_extranjeros","share_ext","share_ext_total") ~ "share det. extranjeros",
    x %in% c("tasa_hc_100k") ~ "tasa HC (100k)",
    x %in% c("tasa_he_100k") ~ "tasa HE (100k)",
    # diferencias
    x == "d_pct_extran"        ~ "Δ % extranjeros",
    x == "d_share_ext"         ~ "Δ share det. extranjeros",
    x == "d_tasa_hc_100k"      ~ "Δ tasa HC (100k)",
    x == "d_tasa_he_100k"      ~ "Δ tasa HE (100k)",
    TRUE ~ x
  )
}

# ---------- Rutas IO ----------
in_csv   <- file.path(ROOT, "output", "tables",  "24_granger_results.csv")
out_dir  <- file.path(ROOT, "output", "figures")
png_path <- file.path(out_dir, "granger_delta.png")
pdf_path <- file.path(out_dir, "granger_delta.pdf")

say("IN  :", in_csv)
say("OUT :", png_path, " | ", pdf_path)
if (!file.exists(in_csv)) stop("[26a] Falta input: ", in_csv)

# ---------- Datos ----------
x_raw <- suppressMessages(readr::read_csv(in_csv, show_col_types = FALSE)) |> janitor::clean_names()

# Aceptar ambos nombres de Portmanteau
if (!"portmanteau_p" %in% names(x_raw) && "port_p" %in% names(x_raw)) {
  x_raw <- dplyr::rename(x_raw, portmanteau_p = port_p)
}

fmt_yr <- function(df){
  if (all(c("start_year","end_year") %in% names(df))){
    a <- suppressWarnings(min(df$start_year, na.rm=TRUE))
    b <- suppressWarnings(max(df$end_year, na.rm=TRUE))
    if (is.finite(a) && is.finite(b)) return(glue("{a}–{b}"))
  }
  NULL
}
range24 <- fmt_yr(x_raw)

# Aristas de ambas direcciones con FDR
edges <- dplyr::bind_rows(
  x_raw |> transmute(pair,
                     src = str_trim(sub("^(.*) ~ (.*)$", "\\2", pair)),
                     dst = str_trim(sub("^(.*) ~ (.*)$", "\\1", pair)),
                     p = granger_v2_to_v1_p, dir = "v2→v1",
                     stable_roots, port = portmanteau_p),
  x_raw |> transmute(pair,
                     src = str_trim(sub("^(.*) ~ (.*)$", "\\1", pair)),
                     dst = str_trim(sub("^(.*) ~ (.*)$", "\\2", pair)),
                     p = granger_v1_to_v2_p, dir = "v1→v2",
                     stable_roots, port = portmanteau_p)
) |>
  mutate(p_fdr = p.adjust(p, method="BH"),
         src_lab = lab_var(src), dst_lab = lab_var(dst)) |>
  filter(!is.na(p_fdr), p_fdr <= 0.10)

say("Aristas Δ (FDR≤10%): ", nrow(edges))

# ---------- Nodos / posiciones ----------
plot_empty <- nrow(edges) == 0
if (plot_empty) {
  nodes <- tibble::tibble(
    name = c("Δ % extranjeros","Δ share det. extranjeros","Δ tasa HC (100k)","Δ tasa HE (100k)"),
    x = c(0,1,1,1),
    y = c(0,0.4,0.0,-0.4)
  )
} else {
  nodes <- tibble::tibble(name = unique(c(edges$src_lab, edges$dst_lab)))
  nodes$x <- ifelse(nodes$name == "Δ % extranjeros", 0, 1)
  y_map <- setNames(rep(NA_real_, nrow(nodes)), nodes$name)
  y_map["Δ % extranjeros"]          <- 0
  y_map["Δ share det. extranjeros"] <- 0.4
  y_map["Δ tasa HC (100k)"]         <- 0.0
  y_map["Δ tasa HE (100k)"]         <- -0.4
  missing <- names(y_map)[is.na(y_map)]
  if (length(missing)) y_map[missing] <- seq(-0.6, 0.6, length.out = length(missing))
  nodes$y <- as.numeric(y_map[nodes$name])
}

# ---------- Etiquetas: anti-solapes
use_repel  <- requireNamespace("ggrepel", quietly = TRUE)
right_span <- 0.06  # separación vertical entre etiquetas de la derecha
left_nudge <- 0.06  # desplazamiento horizontal de etiqueta izquierda
right_nudge<- 0.03  # desplazamiento horizontal de etiquetas derechas

node_labels <- nodes |>
  mutate(
    rank_r = ifelse(x == 1, rank(-y, ties.method = "first"), NA_real_),
    n_right = sum(x == 1),
    off_r = ifelse(x == 1, (rank_r - (n_right + 1)/2) * right_span, 0),
    y_lab = y + off_r,
    x_lab = ifelse(x == 0, x - left_nudge, x + right_nudge),
    hjust  = ifelse(x == 0, 1, 0)
  )

# ---------- Aristas preparadas
if (!plot_empty) {
  seg <- edges |>
    transmute(
      x    = nodes$x[match(src_lab, nodes$name)],
      y    = nodes$y[match(src_lab, nodes$name)],
      xend = nodes$x[match(dst_lab, nodes$name)],
      yend = nodes$y[match(dst_lab, nodes$name)],
      dir,
      sig_cat = case_when(p_fdr <= 0.01 ~ "≤ 1%",
                          p_fdr <= 0.05 ~ "≤ 5%",
                          TRUE          ~ "≤ 10%"),
      alpha_diag = ifelse(isTRUE(stable_roots) & !is.na(port) & port > 0.05, 1.00, 0.35)
    ) |>
    filter(is.finite(x) & is.finite(y) & is.finite(xend) & is.finite(yend)) |>
    mutate(sig_cat = factor(sig_cat, levels = c("≤ 1%", "≤ 5%", "≤ 10%")))
} else {
  seg <- nodes[0,] |> mutate(dir = character(),
                             sig_cat = factor(levels=c("≤ 1%","≤ 5%","≤ 10%")),
                             alpha_diag = numeric())
}

# ---------- Plot
p <- ggplot()
if (!plot_empty) {
  p <- p + geom_curve(
    data = seg,
    aes(x = x, y = y, xend = xend, yend = yend,
        linetype = dir, linewidth = sig_cat, alpha = alpha_diag),
    curvature = 0.2,
    arrow = arrow(length = unit(7, "pt"), type = "closed"),
    lineend  = "round", na.rm = TRUE
  )
}
p <- p + geom_point(data = nodes, aes(x, y), size = 3.8)

if (use_repel) {
  p <- p + ggrepel::geom_label_repel(
    data = node_labels,
    aes(x = x, y = y, label = name, hjust = hjust),
    size = 3.1, label.size = 0.2, box.padding = 0.25, point.padding = 0.2,
    max.overlaps = Inf, seed = 42, min.segment.length = 0,
    fill = "#FAF9F6"
  )
} else {
  p <- p + geom_label(
    data = node_labels,
    aes(x = x_lab, y = y_lab, label = name, hjust = hjust),
    size = 3.1, label.size = 0.2, label.padding = unit(3, "pt"),
    fill = "#FAF9F6"
  )
}

p <- p +
  scale_linewidth_manual(values = c("≤ 1%"=1.7, "≤ 5%"=1.35, "≤ 10%"=1.05), drop = FALSE) +
  scale_linetype_manual(values = c("v2→v1"="solid", "v1→v2"="dashed"), drop = FALSE) +
  scale_alpha_identity() +
  scale_x_continuous(limits = c(-0.05, 1.08), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-0.75, 0.75),  expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme_void(base_size = 12) + theme_pearl_lightgrid() +
  labs(
    title    = glue("Precedencia temporal en diferencias (Δ/VAR bivariado){if (!is.null(range24)) glue(' · {range24}') else ''}"),
    subtitle = "Selección de p por estabilidad + Portmanteau; dummy COVID exógena. Se muestran flechas con FDR ≤ 10%.",
    caption  = "Línea sólida: v2→v1 · Discontinua: v1→v2 · Opacidad: diagnóstico PASS (estable & Port>0.05)\nFuente: output/tables/24_granger_results.csv",
    linewidth= "FDR (p)", linetype = "Dirección"
  ) +
  theme(
    legend.position  = "top",
    legend.title     = element_text(face = "bold"),
    legend.direction = "horizontal",
    legend.box       = "vertical",
    legend.box.just  = "left",
    panel.grid       = element_blank(),
    plot.margin      = margin(10, 64, 10, 24)
  ) +
  guides(
    linewidth = guide_legend(order = 1, title.position = "top",
                             nrow = 1, byrow = TRUE,
                             override.aes = list(alpha = 1, linetype = "solid")),
    linetype  = guide_legend(order = 2, title.position = "top",
                             nrow = 1, byrow = TRUE,
                             override.aes = list(alpha = 1, linewidth = 1.2))
  )

# ---------- Guardar
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
pdf_dev <- if (isTRUE(capabilities("cairo"))) grDevices::cairo_pdf else "pdf"
ggsave(png_path, p, width=9, height=6, dpi=300, bg="#FAF9F6", device=ragg::agg_png)
ggsave(pdf_path, p, width=9, height=6, bg="#FAF9F6", device = pdf_dev)
if (requireNamespace("svglite", quietly = TRUE)) {
  ggsave(file.path(out_dir, "granger_delta.svg"), p, width=9, height=6, bg="#FAF9F6", device = svglite::svglite)
}
if (interactive()) print(p)
say("✔ Guardado:\n   - ", png_path, "\n   - ", pdf_path)
