#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
###############################################################################
# 23_correlaciones_sin_tendencia.R
# Δlog (variaciones %) — Pearson / Spearman / Kendall
#
# Entradas:
#   - data/processed/tasas/tasas_transformadas.csv   (preferente)
#   - data/processed/panel_nacional_2010_2023.csv    (fallback)
#
# Salidas:
#   - output/tables/23_dlog_cor_long.csv
#   - output/tables/23_dlog_cor_matrix_pearson.csv
#   - output/tables/23_dlog_top.csv
#   - output/tables/23_dlog_qc_series_presentes.csv
###############################################################################

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(here); library(janitor)
  library(tidyr); library(purrr); library(fs); library(stringr)
})

dir_create(here("output","tables"))

# ---------------------- 0) Lectura robusta ----------------------
read_master <- function(){
  fp1 <- here("data","processed","tasas","tasas_transformadas.csv")
  fp2 <- here("data","processed","panel_nacional_2010_2023.csv")
  if (file.exists(fp1)) {
    message("✓ Usando master: ", fp1)
    readr::read_csv(fp1, show_col_types = FALSE) |> janitor::clean_names()
  } else if (file.exists(fp2)) {
    message("ℹ Master no encontrado; usando fallback: ", fp2)
    readr::read_csv(fp2, show_col_types = FALSE) |> janitor::clean_names()
  } else {
    stop("No existe ni tasas/transferencias ni panel_nacional. Genera primero el 20/transformadas.")
  }
}
df0 <- read_master()
stopifnot("ano" %in% names(df0))
df0 <- df0 |> mutate(ano = as.integer(ano))

# ---------------------- 1) Selección de series -------------------
Y_all <- c("d_l_hc_rate","d_l_he_rate","d_l_det_tot_rate","d_l_det_ext_rate")
X_all <- c("d_l_share_extranjeros","d_l_poblacion_extranjera",
           "d_l_pib_pc_real","d_l_paro_15_29","d_l_arope")

presentes <- tibble(
  variable = c(Y_all, X_all),
  presente = c(Y_all, X_all) %in% names(df0)
)
readr::write_csv(presentes, here("output","tables","23_dlog_qc_series_presentes.csv"))

Y <- Y_all[Y_all %in% names(df0)]
X <- X_all[X_all %in% names(df0)]
if (length(Y) == 0 || length(X) == 0) {
  stop("No hay suficientes columnas d_l_* en el dataset. Y presentes: ",
       paste(Y, collapse=", "), " | X presentes: ", paste(X, collapse=", "))
}

# ---------------------- 2) Ventanas temporales -------------------
mask_all  <- df0$ano >= 2010 & df0$ano <= 2023
mask_pre  <- df0$ano >= 2010 & df0$ano <= 2019
mask_post <- df0$ano >= 2020 & df0$ano <= 2023
mask_sin  <- df0$ano %in% setdiff(2010:2023, c(2020, 2021))

wins <- list(
  `2010_2023`      = mask_all,
  `2010_2019_pre`  = mask_pre,
  `2020_2023_post` = mask_post,
  `sin_covid`      = mask_sin
)

# ---------------------- 3) Utilidades de correlación ----------------
safe_cor <- function(x, y, method){
  # Evita errores con series constantes o N insuficiente
  ok <- stats::complete.cases(x, y)
  x <- x[ok]; y <- y[ok]
  if (length(x) < 3 || sd(x)==0 || sd(y)==0) return(c(r=NA_real_, p=NA_real_, n=length(x)))
  r <- suppressWarnings(stats::cor(x, y, method = method))
  p <- tryCatch(stats::cor.test(x, y, method = method, exact = (method=="kendall"))$p.value,
                error = function(e) NA_real_)
  c(r=r, p=p, n=length(x))
}

cor_block <- function(dat, ys, xs, window_name){
  cross <- tidyr::crossing(y = ys, x = xs)
  purrr::pmap_dfr(cross, function(y, x){
    pe <- safe_cor(dat[[y]], dat[[x]], "pearson")
    sp <- safe_cor(dat[[y]], dat[[x]], "spearman")
    ke <- safe_cor(dat[[y]], dat[[x]], "kendall")
    tibble(
      window  = window_name,
      var_y   = y,
      var_x   = x,
      pearson = pe["r"], p_pear = pe["p"], n = as.integer(pe["n"]),
      spearman= sp["r"], p_spear= sp["p"],
      kendall = ke["r"], p_kend = ke["p"]
    )
  })
}

# ---------------------- 4) Cálculo por ventana --------------------
res_all <- list()
for (wname in names(wins)) {
  m <- wins[[wname]]
  dt <- df0[m, ]
  res_all[[wname]] <- cor_block(dt, Y, X, wname)
}
res_long <- bind_rows(res_all) %>%
  mutate(abs_pearson = abs(pearson)) %>%
  arrange(window, desc(abs_pearson))

readr::write_csv(res_long, here("output","tables","23_dlog_cor_long.csv"))

# Matriz Pearson por ventana (Y en filas, X en columnas)
matrices <- lapply(names(wins), function(wname){
  res_long %>%
    filter(window == wname) %>%
    select(var_y, var_x, pearson) %>%
    tidyr::pivot_wider(names_from = var_x, values_from = pearson) %>%
    arrange(var_y)
})
# Para exportar en un único CSV concatenado con etiqueta de ventana:
mat_out <- bind_rows(Map(function(tbl, nm) mutate(tbl, window = nm), matrices, names(wins))) %>%
  relocate(window, var_y)
readr::write_csv(mat_out, here("output","tables","23_dlog_cor_matrix_pearson.csv"))

# Top |r| por método y ventana
top <- res_long %>%
  tidyr::pivot_longer(cols = c(pearson, spearman, kendall),
                      names_to = "method", values_to = "r") %>%
  group_by(window, method) %>%
  slice_max(order_by = abs(r), n = 10, with_ties = FALSE) %>%
  ungroup()
readr::write_csv(top, here("output","tables","23_dlog_top.csv"))

message("✅ Correlaciones Δlog exportadas (23).")
