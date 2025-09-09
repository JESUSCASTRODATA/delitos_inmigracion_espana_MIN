#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
###############################################################################
# 22_correlaciones_figuras.R  (Okabe-Ito + ecuación/R² + fondo perla)
###############################################################################

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(ggplot2); library(here); library(patchwork)
})

has_ggrepel <- requireNamespace("ggrepel", quietly = TRUE)
if (has_ggrepel) {
  suppressPackageStartupMessages(library(ggrepel))
} else {
  message("Sugerencia: instala ggrepel (install.packages('ggrepel')) para mejores etiquetas.")
}

theme_set(theme_minimal(base_size = 12))
pearl <- "#FAF9F6"
okabe_ito <- c("#000000","#E69F00","#56B4E9","#009E73",
               "#F0E442","#0072B2","#D55E00","#CC79A7")
col_points <- okabe_ito[3]  # azul cielo
col_line   <- okabe_ito[6]  # azul fuerte

ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
}
figdir <- here::here("output/figures"); ensure_dir(figdir)

infile <- here::here("data/processed/series.csv")
stopifnot(file.exists(infile))

df <- readr::read_csv(infile, show_col_types = FALSE)

pairs <- list(
  c("det_tot","pct_extranjeros_poblacion"),
  c("det_tot","share_extranjeros"),
  c("det_ext","pct_extranjeros_poblacion"),
  c("det_ext","share_extranjeros")
)

mk_lm_annot <- function(x, y) {
  fit <- lm(as.formula(paste(y,"~",x)), data=df)
  sm  <- summary(fit)
  b0 <- unname(coef(fit)[1]); b1 <- unname(coef(fit)[2]); r2 <- sm$r.squared
  xr <- range(df[[x]], na.rm=TRUE); yr <- range(df[[y]], na.rm=TRUE)
  list(
    label = sprintf("y = %.3f + %.3f·x\nR² = %.3f", b0, b1, r2),
    x = xr[1] + 0.03*diff(xr),
    y = yr[2] - 0.03*diff(yr)
  )
}

mk_plot <- function(x, y) {
  ann <- mk_lm_annot(x, y)
  p <- ggplot(df, aes(x=.data[[x]], y=.data[[y]])) +
    geom_point(size=3, color=col_points) +
    geom_smooth(method="lm", se=FALSE, formula=y~x,
                color=col_line, linewidth=1) +
    annotate("text", x=ann$x, y=ann$y, label=ann$label,
             hjust=0, vjust=1, size=3.5)
  if (has_ggrepel) {
    p <- p + ggrepel::geom_text_repel(aes(label=ano), size=3)
  } else {
    p <- p + geom_text(aes(label=ano), size=3, vjust=-0.7)
  }
  p + labs(x=x, y=y, title=paste(x,"vs",y)) +
    theme(plot.background=element_rect(fill=pearl,color=NA),
          panel.background=element_rect(fill=pearl,color=NA))
}

plots <- list()
for (pr in pairs) {
  x <- pr[1]; y <- pr[2]
  if (all(c(x,y) %in% names(df))) {
    p <- mk_plot(x,y)
    outfile <- here::here("output/figures",
                          sprintf("22_cor_%s_vs_%s.png",x,y))
    ggsave(outfile, p, width=7, height=5, dpi=150)
    message("Figura: ", outfile)
    plots[[length(plots)+1]] <- p
  }
}

if (length(plots) >= 2) {
  grid <- patchwork::wrap_plots(plots, ncol=2)
  outfile <- here::here("output/figures","22_cor_grid.png")
  ggsave(outfile, grid, width=10, height=7, dpi=150)
  message("Figura grid: ", outfile)
}
