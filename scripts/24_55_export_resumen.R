#!/usr/bin/env Rscript
# 24_55_export_resumen.R — FDR + tablas “listas para informe”
suppressPackageStartupMessages({
  library(here); library(readr); library(dplyr); library(tidyr); library(janitor); library(glue); library(stringr)
})

# Función de estrellitas VECTORIAL
stars <- function(p) {
  out <- rep("", length(p))
  out[!is.na(p) & p <= 0.10] <- "†"
  out[!is.na(p) & p <= 0.05] <- "*"
  out[!is.na(p) & p <= 0.01] <- "**"
  out[!is.na(p) & p <= 0.001] <- "***"
  out
}

# === 24 (Δ) ===
res24 <- readr::read_csv(here::here("output","tables","24_granger_results.csv"), show_col_types = FALSE) |>
  janitor::clean_names()

res24_long <- res24 |>
  pivot_longer(c(granger_v2_to_v1_p, granger_v1_to_v2_p),
               names_to = "dir", values_to = "p") |>
  mutate(
    p_fdr = p.adjust(p, method = "BH"),
    dir_arrow = ifelse(dir == "granger_v2_to_v1_p","v2→v1","v1→v2"),
    badge = ifelse(stable_roots & !is.na(portmanteau_p) & portmanteau_p > 0.05, "estable ✅", "ojo diag ⚠️"),
    p_fmt = sprintf("%.4g", p),
    p_fdr_fmt = sprintf("%.4g", p_fdr),
    p_disp = paste0(p_fmt, stars(p)),
    p_fdr_disp = paste0(p_fdr_fmt, stars(p_fdr))
  ) |>
  select(pair, n, lag_p, badge, portmanteau_p, dir = dir_arrow, p_disp, p_fdr_disp) |>
  arrange(pair, dir)

# === 55 (TY niveles) ===
res55 <- readr::read_csv(here::here("output","tables","55_toda_results.csv"), show_col_types = FALSE) |>
  janitor::clean_names()

res55_long <- res55 |>
  pivot_longer(c(wald_v2_to_v1_p, wald_v1_to_v2_p),
               names_to = "dir", values_to = "p") |>
  mutate(
    p_fdr = p.adjust(p, method = "BH"),
    dir_arrow = ifelse(dir == "wald_v2_to_v1_p","v2→v1","v1→v2"),
    badge = ifelse(!is.na(port_p) & port_p > 0.05, "residuos OK ✅", "autocorr. ⚠️"),
    p_fmt = sprintf("%.4g", p),
    p_fdr_fmt = sprintf("%.4g", p_fdr),
    p_disp = paste0(p_fmt, stars(p)),
    p_fdr_disp = paste0(p_fdr_fmt, stars(p_fdr))
  ) |>
  select(pair, n, lag_p, d_order, k_total, badge, port_p, dir = dir_arrow, p_disp, p_fdr_disp) |>
  arrange(pair, dir)

# === Guardar CSV “listos para informe” ===
out_dir <- here::here("output","tables")
readr::write_csv(res24_long, file.path(out_dir, "24_granger_resumen_FDR.csv"))
readr::write_csv(res55_long, file.path(out_dir, "55_toda_resumen_FDR.csv"))

# === Imprimir vista rápida ===
cat("\n—— 24 (Δ) con FDR ——\n"); print(res24_long, n = Inf)
cat("\n—— 55 (TY niveles) con FDR ——\n"); print(res55_long, n = Inf)
