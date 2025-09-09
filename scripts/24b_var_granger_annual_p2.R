#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
################################################################################
# 24b_var_granger_annual_p2.R — VAR (Δlog) robusto a n pequeño (p ≤ 2),
#                               Portmanteau ajustado, exógenas opcionales
#                               SIN VARselect por defecto (evita NaNs).
#
# In:  data/processed/tasas/tasas_transformadas.csv
# Out: output/tables/24b_var_granger_resultados.csv
#      output/tables/24b_var_specs.csv
#      output/tables/24b_var_residual_tests.csv
#      output/tables/24b_var_roots_max.csv
#      output/tables/24b_var_granger_resumen.csv
#      output/logs/24b_var_granger_warnings.txt
################################################################################

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(janitor); library(tibble)
  library(here);  library(tidyr); library(stringr)
})

# ------------ Config rápida ---------------------------------------------------
DO_VARSELECT <- FALSE   # TRUE = usar VARselect silenciado; FALSE = regla fija p≤2
ALPHA <- 0.05

# ------------ Logging ---------------------------------------------------------
dir.create(here::here("output","logs"), recursive = TRUE, showWarnings = FALSE)
logf <- here::here("output","logs","24b_var_granger_warnings.txt")
zz <- file(logf, open = "wt")
sink(zz, type = "message")
on.exit({ sink(type = "message"); close(zz) }, add = TRUE)

fatal <- function(msg) {
  dir.create(here::here("output","tables"), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(tibble::tibble(error = msg),
                   here::here("output","tables","24b_var_granger_resultados.csv"))
  message("FATAL: ", msg)
  if (!interactive()) quit(save="no", status = 1L) else stop(msg, call. = FALSE)
}

main <- function() {
  need <- c("vars","lmtest")
  miss <- need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]
  if (length(miss)) fatal(paste0("Faltan paquetes: ", paste(miss, collapse=", "), "."))
  
  f_master <- here::here("data","processed","tasas","tasas_transformadas.csv")
  if (!file.exists(f_master)) fatal(paste("No existe:", f_master))
  
  df0 <- readr::read_csv(f_master, show_col_types = FALSE) |> janitor::clean_names() |> arrange(ano)
  
  # ---- columnas base / autologs si faltan ----
  as_log <- function(v, is_share = FALSE) {
    v <- suppressWarnings(as.numeric(v))
    if (isTRUE(is_share) && max(v, na.rm=TRUE) > 1) v <- v/100
    log(v)
  }
  mk_log <- function(d, raw, lognm, is_share = FALSE) {
    if (!lognm %in% names(d) && raw %in% names(d)) d[[lognm]] <- as_log(d[[raw]], is_share)
    d
  }
  df0 <- df0 |>
    mk_log("share_extranjeros","l_share_extranjeros", TRUE) |>
    mk_log("hc_rate","l_hc_rate") |>
    mk_log("he_rate","l_he_rate") |>
    mk_log("det_tot_rate","l_det_tot_rate") |>
    mk_log("det_ext_rate","l_det_ext_rate") |>
    mk_log("arope","l_arope") |>
    mk_log("paro_15_29","l_paro_15_29") |>
    mk_log("pib_pc_real","l_pib_pc_real") |>
    mutate(ano = as.integer(ano))
  
  must_have <- c("ano","l_share_extranjeros","l_hc_rate","l_he_rate","l_det_tot_rate","l_det_ext_rate")
  if (!all(must_have %in% names(df0))) fatal(paste0("Faltan columnas base: ",
                                                    paste(setdiff(must_have, names(df0)), collapse=", ")))
  
  # ---- helpers ----
  add_covid <- function(d) {
    if (!"covid_2020" %in% names(d)) d$covid_2020 <- as.integer(d$ano == 2020L)
    if (!"covid_2021" %in% names(d)) d$covid_2021 <- as.integer(d$ano == 2021L)
    d
  }
  make_dlogs <- function(d) {
    v <- intersect(c("l_hc_rate","l_he_rate","l_det_tot_rate","l_det_ext_rate",
                     "l_share_extranjeros","l_arope","l_paro_15_29","l_pib_pc_real"), names(d))
    for (nm in v) d[[paste0("d_", nm)]] <- c(NA_real_, diff(as.numeric(d[[nm]])))
    d
  }
  nzv <- function(v, tol = 1e-12) {
    v <- suppressWarnings(as.numeric(v)); ok <- is.finite(v)
    if (sum(ok) < 3) return(TRUE)
    sd(v[ok]) < sqrt(tol) || length(unique(v[ok])) <= 2
  }
  
  # Fitting seguro (sin VARselect por defecto)
  safe_fit <- function(YX, exog, n_eff) {
    # regla p_inicial: p = min(2, floor((n_eff-1)/5)), nunca <1
    p_init <- max(1L, min(2L, floor((n_eff - 1L) / 5L)))
    # si se desea, intentar VARselect silenciado para sugerencia
    p_suggest <- NA_integer_
    if (isTRUE(DO_VARSELECT)) {
      sel <- suppressWarnings(tryCatch(
        vars::VARselect(YX, lag.max = p_init, type = "const", exogen = exog),
        error = function(e) NULL
      ))
      if (!is.null(sel)) {
        p_suggest <- suppressWarnings(as.integer(sel$selection[["SC(n)"]]))
        if (!is.finite(p_suggest)) p_suggest <- NA_integer_
      }
    }
    p_try <- if (is.finite(p_suggest)) min(p_init, p_suggest) else p_init
    
    # reduce p hasta que Sigma sea PD (chol OK)
    fit <- NULL; p_used <- NA_integer_
    while (is.null(fit) && p_try >= 1L) {
      fit <- suppressWarnings(tryCatch(
        vars::VAR(YX, p = p_try, type = "const", exogen = exog),
        error = function(e) NULL
      ))
      if (!is.null(fit)) {
        ok_sig <- tryCatch({ chol(stats::cov(resid(fit))); TRUE }, error = function(e) FALSE)
        if (!ok_sig) fit <- NULL
      }
      if (is.null(fit)) p_try <- p_try - 1L
    }
    if (!is.null(fit)) p_used <- p_try
    list(fit = fit, p_used = p_used, p_suggest = p_suggest, p_init = p_init)
  }
  
  # ---- ventanas, Ys y X ----
  wins <- list(
    `2010_2023` = function(x) x$ano >= 2010 & x$ano <= 2023,
    `sin_covid` = function(x) x$ano %in% setdiff(2010:2023, c(2020L, 2021L))
  )
  Ys_all <- c("d_l_det_ext_rate","d_l_det_tot_rate","d_l_hc_rate","d_l_he_rate")
  Xname  <- "d_l_share_extranjeros"
  
  rows_res <- list(); rows_specs <- list(); rows_diag <- list(); rows_roots <- list()
  
  for (wname in names(wins)) {
    dtw <- df0[wins[[wname]](df0), , drop = FALSE]
    if (nrow(dtw) < 6L) next
    dtw <- add_covid(dtw) |> make_dlogs()
    
    Ys <- Ys_all[Ys_all %in% names(dtw)]
    if (!length(Ys)) fatal("No hay Ys en Δlog: d_l_*")
    if (!Xname %in% names(dtw)) fatal("Falta X 'd_l_share_extranjeros'")
    
    for (use_covid in c(FALSE, TRUE)) {
      for (use_controls in c(FALSE, TRUE)) {
        for (y in Ys) {
          
          base_cols <- c("ano", y, Xname, "covid_2020", "covid_2021",
                         "d_l_arope","d_l_paro_15_29","d_l_pib_pc_real")
          dt_min <- dtw[, intersect(base_cols, names(dtw)), drop = FALSE]
          
          ok_yx <- stats::complete.cases(dt_min[, intersect(c(y,Xname), names(dt_min)), drop = FALSE])
          dt_eff <- dt_min[ok_yx, , drop = FALSE]
          n_eff  <- nrow(dt_eff)
          if (n_eff < 9L) {
            rows_res[[length(rows_res)+1]] <- tibble(
              window = wname, y = y, x = Xname, covid = use_covid, controls = use_controls,
              p_y_on_x = NA_real_, p_x_on_y = NA_real_,
              lag_bic = NA_integer_, p_used = NA_integer_,
              stable = NA, max_modulus = NA_real_,
              serial_p_value = NA_real_, n = n_eff
            )
            next
          }
          
          # Exógenas útiles (sin varianza ~0)
          exog <- NULL
          exog_names <- character(0)
          if (use_covid) {
            covs <- intersect(c("covid_2020","covid_2021"), names(dt_eff))
            covs <- covs[!vapply(dt_eff[, covs, drop = FALSE], nzv, logical(1))]
            exog_names <- c(exog_names, covs)
          }
          if (use_controls) {
            ctrls <- intersect(c("d_l_arope","d_l_paro_15_29","d_l_pib_pc_real"), names(dt_eff))
            ctrls <- ctrls[!vapply(dt_eff[, ctrls, drop = FALSE], nzv, logical(1))]
            exog_names <- c(exog_names, ctrls)
          }
          # Si demasiadas exógenas para n pequeño, recorta a 1–2 más informativas (por varianza)
          if (length(exog_names) > 2) {
            var_rank <- sapply(exog_names, function(cn) var(dt_eff[[cn]], na.rm = TRUE))
            exog_names <- exog_names[order(var_rank, decreasing = TRUE)][1:2]
          }
          if (length(exog_names)) exog <- as.matrix(dt_eff[, exog_names, drop = FALSE])
          
          # Endógenas
          YX <- dt_eff[, c(y, Xname), drop = FALSE]
          YX <- as.data.frame(lapply(YX, function(v) as.numeric(v)))
          rownames(YX) <- dt_eff$ano
          
          # Ajuste seguro
          got <- safe_fit(YX, exog, n_eff)
          fit <- got$fit; p_used <- got$p_used
          
          if (is.null(fit)) {
            rows_res[[length(rows_res)+1]] <- tibble(
              window = wname, y = y, x = Xname, covid = use_covid, controls = use_controls,
              p_y_on_x = NA_real_, p_x_on_y = NA_real_, lag_bic = NA_integer_, p_used = NA_integer_,
              stable = NA, max_modulus = NA_real_, serial_p_value = NA_real_, n = n_eff
            )
            next
          }
          
          # Estabilidad
          roots <- tryCatch(vars::roots(fit, modulus = TRUE), error = function(e) NA_real_)
          max_mod <- suppressWarnings(max(as.numeric(roots), na.rm = TRUE))
          stable_flag <- is.finite(max_mod) && (max_mod < 1)
          
          # Portmanteau ajustado (muestras cortas)
          lag_diag <- max(1L, min(5L, n_eff - p_used - 1L))
          pt_p <- tryCatch(vars::serial.test(fit, lags.pt = lag_diag, type = "PT.adjusted")$serial$p.value,
                           error = function(e) NA_real_)
          if (!is.finite(pt_p)) {
            resY <- tryCatch(resid(fit)[, y], error = function(e) NULL)
            pt_p <- tryCatch(stats::Box.test(resY, lag = lag_diag, type = "Ljung-Box")$p.value,
                             error = function(e) NA_real_)
          }
          
          # Granger
          p_y_on_x <- tryCatch(vars::causality(fit, cause = Xname)$Granger$p.value, error = function(e) NA_real_)
          p_x_on_y <- tryCatch(vars::causality(fit, cause = y)$Granger$p.value,    error = function(e) NA_real_)
          
          # Salidas
          rows_res[[length(rows_res)+1]] <- tibble(
            window = wname, y = y, x = Xname, covid = use_covid, controls = use_controls,
            p_y_on_x = as.numeric(p_y_on_x), p_x_on_y = as.numeric(p_x_on_y),
            lag_bic = if (isTRUE(DO_VARSELECT) && is.finite(got$p_suggest)) as.integer(got$p_suggest) else NA_integer_,
            p_used  = as.integer(p_used),
            stable  = stable_flag, max_modulus = as.numeric(max_mod),
            serial_p_value = as.numeric(pt_p), n = as.integer(n_eff)
          )
          rows_specs[[length(rows_specs)+1]] <- tibble(
            window = wname, y = y, x = Xname, covid = use_covid, controls = use_controls,
            p_init = as.integer(got$p_init),
            p_suggest = if (isTRUE(DO_VARSELECT)) as.integer(got$p_suggest) else NA_integer_,
            p_used = as.integer(p_used),
            exog = paste(exog_names, collapse = "+")
          )
          rows_diag[[length(rows_diag)+1]] <- tibble(
            window = wname, y = y, x = Xname, covid = use_covid, controls = use_controls,
            lags_pt = lag_diag, serial_p_value = pt_p
          )
          rows_roots[[length(rows_roots)+1]] <- tibble(
            window = wname, y = y, x = Xname, covid = use_covid, controls = use_controls,
            max_modulus = max_mod, stable = stable_flag
          )
        }
      }
    }
  }
  
  dir.create(here::here("output","tables"), recursive = TRUE, showWarnings = FALSE)
  
  if (length(rows_res) == 0) fatal("No se generaron resultados (¿datos insuficientes?).")
  
  res_out   <- dplyr::bind_rows(rows_res)   |> arrange(window, y, controls, covid)
  specs_out <- dplyr::bind_rows(rows_specs) |> arrange(window, y, controls, covid)
  diag_out  <- dplyr::bind_rows(rows_diag)  |> arrange(window, y, controls, covid)
  roots_out <- dplyr::bind_rows(rows_roots) |> arrange(window, y, controls, covid)
  
  readr::write_csv(res_out,   here::here("output","tables","24b_var_granger_resultados.csv"))
  readr::write_csv(specs_out, here::here("output","tables","24b_var_specs.csv"))
  readr::write_csv(diag_out,  here::here("output","tables","24b_var_residual_tests.csv"))
  readr::write_csv(roots_out, here::here("output","tables","24b_var_roots_max.csv"))
  
  # Resumen de decisión
  resumen <- res_out |>
    mutate(
      ok_stable = isTRUE(stable) & is.finite(max_modulus) & (max_modulus < 1),
      ok_serial = is.finite(serial_p_value) & serial_p_value > ALPHA,
      sig_x_to_y = is.finite(p_y_on_x) & p_y_on_x < ALPHA,
      sig_y_to_x = is.finite(p_x_on_y) & p_x_on_y < ALPHA,
      decision_x_to_y = dplyr::case_when(
        sig_x_to_y & ok_stable & ok_serial ~ "ACEPTAR (X ⇒ Y)",
        sig_x_to_y & (!ok_stable | !ok_serial) ~ "RECHAZAR (diag falla)",
        TRUE ~ "NO"
      ),
      decision_y_to_x = dplyr::case_when(
        sig_y_to_x & ok_stable & ok_serial ~ "ACEPTAR (Y ⇒ X)",
        sig_y_to_x & (!ok_stable | !ok_serial) ~ "RECHAZAR (diag falla)",
        TRUE ~ "NO"
      )
    )
  readr::write_csv(resumen, here::here("output","tables","24b_var_granger_resumen.csv"))
  
  message("✅ 24b VAR/Granger exportado (p≤2, sin VARselect por defecto): output/tables/24b_var_granger_resumen.csv")
  message("⚠️ Warnings capturados (si los hubiera) en: ", logf)
}

status <- tryCatch({
  withCallingHandlers(main(), warning = function(w) message("WARN: ", conditionMessage(w))); TRUE
}, error = function(e) {
  message("FATAL: ", conditionMessage(e)); FALSE
})
if (!status && !interactive()) quit(save="no", status = 1L)
