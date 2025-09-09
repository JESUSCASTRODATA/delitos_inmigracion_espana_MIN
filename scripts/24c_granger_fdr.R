#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
###############################################################################
# 24c_granger_fdr.R — Ajuste FDR (Benjamini–Hochberg) para p-valores de Granger.
#
# Autor: JESUS CASTRO
# Fecha: 2025-09-04
###############################################################################
suppressPackageStartupMessages({
  library(readr); library(dplyr); library(here)
})

infile <- here::here("output/tables","24_var_granger_resultados.csv")
if (!file.exists(infile)) {
  stop("No existe output/tables/24_var_granger_resultados.csv. Ejecuta antes 24_var_granger_pipeline.R")
}
gr <- readr::read_csv(infile, show_col_types = FALSE)

gr2 <- gr %>%
  mutate(
    robust = stable & is.finite(serial_p_value) & serial_p_value >= 0.05 & n >= 8,
    p_y_on_x_fdr = p.adjust(p_y_on_x, method = "BH"),
    p_x_on_y_fdr = p.adjust(p_x_on_y, method = "BH"),
    X_causes_Y_fdr = robust & is.finite(p_y_on_x_fdr) & p_y_on_x_fdr < 0.05,
    Y_causes_X_fdr = robust & is.finite(p_x_on_y_fdr) & p_x_on_y_fdr < 0.05
  )

readr::write_csv(gr2, here::here("output/tables","24_var_granger_resultados_fdr.csv"))
message("✓ Guardado output/tables/24_var_granger_resultados_fdr.csv")

resumen <- gr2 %>%
  group_by(window, policy, y) %>%
  summarise(
    n_models = dplyr::n(),
    n_robust = sum(robust, na.rm=TRUE),
    n_X_causes_Y_fdr = sum(X_causes_Y_fdr, na.rm=TRUE),
    n_Y_causes_X_fdr = sum(Y_causes_X_fdr, na.rm=TRUE),
    .groups = "drop"
  )

readr::write_csv(resumen, here::here("output/tables","29_granger_resumen_fdr.csv"))
message("✓ Guardado output/tables/29_granger_resumen_fdr.csv")