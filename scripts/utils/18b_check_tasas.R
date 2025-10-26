#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
###############################################################################
# 18b_check_tasas.R — QC + Preflight para ARDL/VAR del master de tasas
#
# Lee:  data/processed/tasas/tasas_transformadas.csv
# Escribe:
#   output/logs/19_check_tasas.txt
#   output/tables/19_qc_na_counts.csv
#   output/tables/19_qc_summary_stats.csv
#   output/tables/19_correlations_pearson.csv
#   output/tables/19_correlations_spearman.csv
#   output/tables/19_stationarity_adf.csv           (si hay 'tseries' o 'urca')
#   output/tables/19_ardl_preflight.csv
#   output/tables/19_det_share_summary.csv          (proporción det. extranjeros)
###############################################################################

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(janitor); library(here)
  library(fs); library(tidyr); library(stringr)
})

# Root & paths
tryCatch(here::i_am("scripts/19_check_tasas.R"), error = function(e) NULL)
ipath <- here::here("data","processed","tasas","tasas_transformadas.csv")
fs::dir_create(here::here("output","logs"))
fs::dir_create(here::here("output","tables"))
logf <- here::here("output","logs","19_check_tasas.txt")
cat("", file = logf)

log_ <- function(...) { msg <- paste0(...); cat(msg, "\n", file = logf, append = TRUE); message(msg) }
ok_file <- file.exists(ipath)
if (!ok_file) stop("No existe: ", ipath)

# Load
df <- readr::read_csv(ipath, show_col_types = FALSE) |> clean_names()
log_("✔ Cargado: ", ipath, "  [n=", nrow(df), " cols=", ncol(df), "]")

# Columns expected
need <- c("ano","hc_rate","he_rate","det_tot_rate","det_ext_rate",
          "share_extranjeros","l_hc_rate","l_he_rate","l_det_tot_rate","l_det_ext_rate","l_share_extranjeros")
opt  <- c("pib_pc_real","l_pib_pc_real","arope","paro_15_29",
          "share_det_ext_sobre_tot","ratio_tasa_ext_vs_tot",
          "poblacion_total","poblacion_espanola","poblacion_extranjera")
missing_need <- setdiff(need, names(df))
if (length(missing_need)) stop("Faltan columnas críticas: ", paste(missing_need, collapse=", "))

present_opt <- intersect(opt, names(df))
log_("✔ Columnas críticas OK. Opcionales presentes: ", paste(present_opt, collapse=", "))

# Basic coverage
yr_min <- suppressWarnings(min(df$ano, na.rm = TRUE))
yr_max <- suppressWarnings(max(df$ano, na.rm = TRUE))
yrs    <- seq(2010L, 2023L)
missing_years <- setdiff(yrs, df$ano)
log_(sprintf("Cobertura años: %d–%d; faltan en [2010..2023]: %s",
             yr_min, yr_max, ifelse(length(missing_years), paste(missing_years, collapse=", "), "ninguno")))

# Duplicados y orden
dups <- df %>% count(ano) %>% filter(n>1)
if (nrow(dups)) {
  log_("⚠ Duplicados por año: ", paste(dups$ano, collapse=", "))
} else log_("✔ Sin duplicados por año")

df <- df %>% arrange(ano)

# NA counts & finite checks
qc_na <- df %>%
  summarise(across(all_of(c(need, present_opt)), ~ sum(is.na(.)), .names = "{.col}_na"))
qc_na_long <- tidyr::pivot_longer(qc_na, everything(), names_to = "col", values_to = "na_count") %>%
  arrange(desc(na_count))
readr::write_csv(qc_na_long, here::here("output","tables","19_qc_na_counts.csv"))
log_("✔ Exportado 19_qc_na_counts.csv")

# Positividad (tasas & shares) y logs finitos
positives <- df %>%
  summarise(
    any_nonpos_hc_rate      = any(!is.finite(hc_rate) | hc_rate <= 0, na.rm = TRUE),
    any_nonpos_he_rate      = any(!is.finite(he_rate) | he_rate <= 0, na.rm = TRUE),
    any_nonpos_det_tot_rate = any(!is.finite(det_tot_rate) | det_tot_rate <= 0, na.rm = TRUE),
    any_nonpos_det_ext_rate = any(!is.finite(det_ext_rate) | det_ext_rate <= 0, na.rm = TRUE),
    any_nonpos_share        = any(!is.finite(share_extranjeros) | share_extranjeros <= 0, na.rm = TRUE),
    any_nonfinite_logs      = any(!is.finite(l_hc_rate) | !is.finite(l_he_rate) |
                                    !is.finite(l_det_tot_rate) | !is.finite(l_det_ext_rate) |
                                    !is.finite(l_share_extranjeros), na.rm = TRUE)
  )
log_("Positividad/logs: ", paste(names(positives), positives |> unlist() |> as.logical(), sep="=", collapse=" ; "))

# Summary stats (tasas principales)
sumstat <- df %>%
  summarise(
    n = n(),
    hc_rate_mean = mean(hc_rate, na.rm = TRUE), hc_rate_sd = sd(hc_rate, na.rm = TRUE),
    he_rate_mean = mean(he_rate, na.rm = TRUE), he_rate_sd = sd(he_rate, na.rm = TRUE),
    det_tot_rate_mean = mean(det_tot_rate, na.rm = TRUE), det_tot_rate_sd = sd(det_tot_rate, na.rm = TRUE),
    det_ext_rate_mean = mean(det_ext_rate, na.rm = TRUE), det_ext_rate_sd = sd(det_ext_rate, na.rm = TRUE),
    share_extranjeros_mean = mean(share_extranjeros, na.rm = TRUE)
  )
readr::write_csv(sumstat, here::here("output","tables","19_qc_summary_stats.csv"))
log_("✔ Exportado 19_qc_summary_stats.csv")

# Correlations (Pearson / Spearman) entre logs y l_share_extranjeros
corr_vars <- c("l_hc_rate","l_he_rate","l_det_tot_rate","l_det_ext_rate")
pear <- lapply(corr_vars, function(y) {
  dd <- df %>% select(all_of(c(y, "l_share_extranjeros"))) %>% drop_na()
  if (nrow(dd) >= 3) {
    data.frame(y = y,
               r = suppressWarnings(cor(dd[[y]], dd$l_share_extranjeros, method = "pearson")),
               n = nrow(dd))
  } else data.frame(y = y, r = NA_real_, n = nrow(dd))
}) |> bind_rows()
spear <- lapply(corr_vars, function(y) {
  dd <- df %>% select(all_of(c(y, "l_share_extranjeros"))) %>% drop_na()
  if (nrow(dd) >= 3) {
    data.frame(y = y,
               r = suppressWarnings(cor(dd[[y]], dd$l_share_extranjeros, method = "spearman")),
               n = nrow(dd))
  } else data.frame(y = y, r = NA_real_, n = nrow(dd))
}) |> bind_rows()

readr::write_csv(pear,  here::here("output","tables","19_correlations_pearson.csv"))
readr::write_csv(spear, here::here("output","tables","19_correlations_spearman.csv"))
log_("✔ Exportados correlations (Pearson/Spearman)")

# (Opcional) Stationarity checks: ADF
adf_rows <- list()
has_tseries <- requireNamespace("tseries", quietly = TRUE)
has_urca    <- requireNamespace("urca", quietly = TRUE)
if (has_tseries || has_urca) {
  vars_adf <- c("l_hc_rate","l_he_rate","l_det_tot_rate","l_det_ext_rate","l_share_extranjeros")
  for (v in vars_adf) {
    x <- df[[v]]
    x <- x[is.finite(x)]
    if (length(x) >= 8) {
      res <- tryCatch({
        if (has_tseries) {
          # tseries::adf.test con k automático
          tt <- tseries::adf.test(x, alternative = "stationary")
          data.frame(var=v, method="tseries::adf.test", stat=unname(tt$statistic), p_value=tt$p.value, n=length(x))
        } else {
          # urca::ur.df con drift
          uu <- urca::ur.df(x, type="drift", selectlags = "AIC")
          # p-value aproximado no viene directo; reportamos stat y lags
          data.frame(var=v, method=paste0("urca::ur.df(", uu@testreg$coefficients[2,4], " t-stat)"),
                     stat=uu@teststat[1], p_value=NA_real_, n=length(x))
        }
      }, error = function(e) data.frame(var=v, method="adf_error", stat=NA_real_, p_value=NA_real_, n=length(x)))
      adf_rows[[length(adf_rows)+1]] <- res
    }
  }
  if (length(adf_rows)) {
    adf_tab <- bind_rows(adf_rows)
    readr::write_csv(adf_tab, here::here("output","tables","19_stationarity_adf.csv"))
    log_("✔ Exportado 19_stationarity_adf.csv")
  } else {
    log_("ℹ ADF: no hay series suficientes (n<8) o todo NA")
  }
} else {
  log_("ℹ Saltando ADF (no están instalados 'tseries' ni 'urca').")
}

# ARDL preflight por ventana
wins <- list(
  `2010_2023` = df$ano >= 2010 & df$ano <= 2023,
  `sin_covid` = df$ano %in% setdiff(2010:2023, c(2020,2021))
)
nzv <- function(v, tol=1e-12) {
  v <- suppressWarnings(as.numeric(v)); ok <- is.finite(v)
  if (sum(ok) < 3) return(TRUE)
  sd(v[ok]) < sqrt(tol) || length(unique(v[ok])) <= 2
}
pre_rows <- list()
Ys <- c("l_hc_rate","l_he_rate","l_det_tot_rate","l_det_ext_rate")
X  <- "l_share_extranjeros"
ctrls <- intersect(c("l_pib_pc_real","arope","paro_15_29"), names(df))

for (w in names(wins)) {
  dd <- df[wins[[w]], , drop=FALSE]
  for (y in Ys) {
    base <- dd %>% select(all_of(c("ano", y, X, ctrls)))
    eff  <- drop_na(base, all_of(c(y, X)))
    n_eff <- nrow(eff)
    has_var_y <- !nzv(eff[[y]])
    has_var_x <- !nzv(eff[[X]])
    nzv_ctrls <- sapply(ctrls, function(cn) if (cn %in% names(eff)) nzv(eff[[cn]]) else NA)
    any_nzv_ctrls <- any(nzv_ctrls, na.rm = TRUE)
    msg <- dplyr::case_when(
      n_eff < 12 ~ "insuficiente (n<12)",
      !has_var_y ~ "Y ~ constante/varianza cero",
      !has_var_x ~ "X ~ constante/varianza cero",
      TRUE ~ "OK"
    )
    pre_rows[[length(pre_rows)+1]] <- data.frame(
      window = w, y = y, x = X, n_eff = n_eff,
      var_y = has_var_y, var_x = has_var_x,
      any_ctrl_nzv = any_nzv_ctrls,
      ctrls = paste(ctrls, collapse="+"),
      status = msg,
      stringsAsFactors = FALSE
    )
  }
}
preflight <- bind_rows(pre_rows)
readr::write_csv(preflight, here::here("output","tables","19_ardl_preflight.csv"))
log_("✔ Exportado 19_ardl_preflight.csv")

# Proporción de detenciones de extranjeros (nivel y resumen por ventanas)
det_share <- df %>%
  transmute(ano,
            share_det_ext_sobre_tot = if ("share_det_ext_sobre_tot" %in% names(df)) share_det_ext_sobre_tot else det_ext / pmax(det_tot, 1),
            det_ext_rate, det_tot_rate, ratio_tasa_ext_vs_tot = if ("ratio_tasa_ext_vs_tot" %in% names(df)) ratio_tasa_ext_vs_tot else det_ext_rate / pmax(det_tot_rate, 1e-9)
  )

sum_det <- list(
  all = det_share %>% summarise(window="2010_2023",
                                n=n(),
                                mean_share_det_ext = mean(share_det_ext_sobre_tot, na.rm = TRUE),
                                median_share_det_ext = median(share_det_ext_sobre_tot, na.rm = TRUE),
                                mean_ratio_rate = mean(ratio_tasa_ext_vs_tot, na.rm = TRUE)),
  sin_covid = det_share %>%
    filter(!(ano %in% c(2020,2021))) %>%
    summarise(window="sin_covid",
              n=n(),
              mean_share_det_ext = mean(share_det_ext_sobre_tot, na.rm = TRUE),
              median_share_det_ext = median(share_det_ext_sobre_tot, na.rm = TRUE),
              mean_ratio_rate = mean(ratio_tasa_ext_vs_tot, na.rm = TRUE))
) |> bind_rows()

readr::write_csv(sum_det, here::here("output","tables","19_det_share_summary.csv"))
log_("✔ Exportado 19_det_share_summary.csv")

log_("✅ CHECK completo. Revisa la carpeta output/tables para los CSV y output/logs/19_check_tasas.txt para el log.")
