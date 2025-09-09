#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
###############################################################################
# 24c_smallN_diagnostics.R
#
# Propósito:
#   Diagnóstico rápido para VAR/Granger con muestra corta:
#   - Verifica columnas críticas en tasas_transformadas.csv
#   - Chequea finitud (sin NA/Inf)
#   - Construye Δlog y calcula:
#       * casos completos por par (Y, X)
#       * varianzas de Δlog por serie
#
# Entradas:
#   data/processed/tasas/tasas_transformadas.csv
#
# Salidas:
#   output/logs/25_smallN_diagnostics.txt
#   output/tables/25_finite_counts.csv
#   output/tables/25_diff_cases_per_pair.csv
#   output/tables/25_diff_variances.csv
#   (muestra) output/tables/25_dx_head.csv
###############################################################################

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(janitor)
  library(here);  library(fs);    library(tidyr); library(tibble)
})

# --- setup de carpetas y log --------------------------------------------------
fs::dir_create(here("output","logs"), recurse = TRUE)
fs::dir_create(here("output","tables"), recurse = TRUE)
logf <- here("output","logs","25_smallN_diagnostics.txt")
cat("", file = logf)
log_ <- function(...) {
  msg <- paste0(...); cat(msg, "\n", file = logf, append = TRUE); message(msg)
}

# --- lectura ------------------------------------------------------------------
f <- here("data","processed","tasas","tasas_transformadas.csv")
if (!file.exists(f)) {
  log_("FATAL: No existe: ", f)
  quit(save = "no", status = 1L)
}
df <- readr::read_csv(f, show_col_types = FALSE) |>
  janitor::clean_names() |>
  arrange(ano)

# --- requisitos ---------------------------------------------------------------
need <- c("ano","l_share_extranjeros","l_hc_rate","l_he_rate","l_det_tot_rate","l_det_ext_rate")
missing_need <- setdiff(need, names(df))
if (length(missing_need)) {
  log_("FATAL: Faltan columnas críticas: ", paste(missing_need, collapse = ", "))
  quit(save = "no", status = 1L)
} else {
  log_("✔ Columnas críticas presentes: ", paste(need, collapse = ", "))
}

# --- finitud ------------------------------------------------------------------
finite_counts <- df |>
  summarise(across(all_of(need), ~ sum(!is.finite(.x)), .names = "{.col}_nonfinite")) |>
  pivot_longer(everything(), names_to = "col", values_to = "nonfinite") |>
  arrange(desc(nonfinite))

readr::write_csv(finite_counts, here("output","tables","25_finite_counts.csv"))
log_("✔ Exportado: output/tables/25_finite_counts.csv")
if (any(finite_counts$nonfinite > 0)) {
  log_("WARN: Hay valores no finitos en: ",
       paste(finite_counts$col[finite_counts$nonfinite > 0], collapse = ", "))
} else {
  log_("✔ Todos los campos críticos son finitos (sin NA/Inf).")
}

# --- ventana 2010–2023 + Δlog ------------------------------------------------
d <- df |> filter(ano >= 2010, ano <= 2023)

dx <- d |>
  transmute(
    ano,
    d_l_share_extranjeros = c(NA_real_, diff(l_share_extranjeros)),
    d_l_hc_rate           = c(NA_real_, diff(l_hc_rate)),
    d_l_he_rate           = c(NA_real_, diff(l_he_rate)),
    d_l_det_tot_rate      = c(NA_real_, diff(l_det_tot_rate)),
    d_l_det_ext_rate      = c(NA_real_, diff(l_det_ext_rate))
  )

# Muestra (cabezera) para inspección rápida
readr::write_csv(head(dx, 8), here("output","tables","25_dx_head.csv"))
log_("✔ Exportado: output/tables/25_dx_head.csv (muestra Δlog)")

# --- casos completos por par (tras Δlog) -------------------------------------
pairs <- list(
  c("d_l_det_ext_rate","d_l_share_extranjeros"),
  c("d_l_det_tot_rate","d_l_share_extranjeros"),
  c("d_l_hc_rate","d_l_share_extranjeros"),
  c("d_l_he_rate","d_l_share_extranjeros")
)

pair_counts <- tibble(
  pair = sapply(pairs, \(p) paste(p, collapse = " ~ ")),
  n_complete = sapply(pairs, \(p) sum(complete.cases(dx[, p])))
) |>
  arrange(desc(n_complete))

readr::write_csv(pair_counts, here("output","tables","25_diff_cases_per_pair.csv"))
log_("✔ Exportado: output/tables/25_diff_cases_per_pair.csv")
log_("Resumen casos completos (Δlog) por par:\n",
     paste(sprintf("  - %-35s n=%d", pair_counts$pair, pair_counts$n_complete),
           collapse = "\n"))

# --- varianzas de Δlog --------------------------------------------------------
safe_var <- function(v) { suppressWarnings(var(v, na.rm = TRUE)) }

diff_vars <- dx |>
  select(-ano) |>
  summarise(across(everything(), safe_var)) |>
  pivot_longer(everything(), names_to = "var", values_to = "variance") |>
  arrange(desc(variance))

readr::write_csv(diff_vars, here("output","tables","25_diff_variances.csv"))
log_("✔ Exportado: output/tables/25_diff_variances.csv")

# --- resumen de consola/log ---------------------------------------------------
log_("--- RESUMEN ---")
log_("Archivo: ", f)
log_("Años disponibles en (2010..2023): ",
     paste(range(d$ano, na.rm = TRUE), collapse = "–"))
log_("Casos completos por par (esperable ≈ 13 si no hay huecos tras diff).")
log_("Varianzas Δlog > 0 ⇒ hay variación suficiente para VAR con p pequeño.")

log_("✅ Diagnóstico finalizado. Revisa output/logs/25_smallN_diagnostics.txt y output/tables/")
