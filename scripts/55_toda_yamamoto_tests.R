#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
################################################################################
# 55_toda_yamamoto_tests.R — Robustez con Toda–Yamamoto (VAR en niveles con p+d)
#
# Idea: Estima VAR en niveles con p (BIC, p≤2) + d (orden máximo de integración)
#       y aplica test de Wald sobre los primeros p rezagos para Granger.
#
# Entradas:
#   data/processed/tasas/tasas_transformadas.csv (o niveles equivalentes)
#   (Intento de autocompletar logs 'l_*' si faltan)
#
# Salidas:
#   output/tables/55_ty_granger_resultados.csv
#   output/tables/55_ty_specs.csv
#   output/tables/55_ty_residual_tests.csv
#   output/tables/55_ty_roots_max.csv
#   output/tables/55_ty_granger_resumen.csv
#   output/logs/55_ty_warnings.txt
################################################################################

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(janitor); library(tibble)
  library(here);  library(tidyr); library(stringr)
})

# ---- logging -----------------------------------------------------------------
dir.create(here::here("output","logs"), recursive = TRUE, showWarnings = FALSE)
logf <- here::here("output","logs","55_ty_warnings.txt")
zz <- file(logf, open = "wt")
sink(zz, type = "message")
on.exit({ sink(type = "message"); close(zz) }, add = TRUE)

fatal <- function(msg) {
  dir.create(here::here("output","tables"), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(tibble::tibble(error = msg),
                   here::here("output","tables","55_ty_granger_resultados.csv"))
  message("FATAL: ", msg)
  if (!interactive()) quit(save="no", status = 1L)
  stop(msg, call. = FALSE)
}

main <- function() {
  need <- c("vars","urca","car","lmtest")
  miss <- need[!vapply(need, requireNamespace, FUN.VALUE = logical(1), quietly = TRUE)]
  if (length(miss)) {
    message("Instala paquetes faltantes: ", paste(miss, collapse=", "))
    # Intento de instalación (se puede comentar si no se desea)
    tryCatch({ install.packages(miss) }, error = function(e) NULL)
    miss2 <- need[!vapply(need, requireNamespace, FUN.VALUE = logical(1), quietly = TRUE)]
    if (length(miss2)) fatal(paste("Faltan paquetes tras intentar instalar:", paste(miss2, collapse=", ")))
  }

  f_master <- here::here("data","processed","tasas","tasas_transformadas.csv")
  if (!file.exists(f_master)) fatal(paste("No existe:", f_master))

  df0 <- readr::read_csv(f_master, show_col_types = FALSE) |> janitor::clean_names()
  df0 <- df0 |> mutate(ano = as.integer(ano)) |> arrange(ano)

  # Autocompleta logs si faltan (niveles → logs)
  as_log <- function(v, is_share = FALSE) {
    v <- suppressWarnings(as.numeric(v))
    if (isTRUE(is_share) && max(v, na.rm=TRUE) > 1) v <- v/100
    log(v)
  }
  mk_log <- function(d, raw, lognm, is_share = FALSE) {
    if (!lognm %in% names(d) && raw %in% names(d)) d[[lognm]] <- as_log(d[[raw]], is_share)
    d
  }
  df0 <- mk_log(df0, "share_extranjeros","l_share_extranjeros", TRUE)
  df0 <- mk_log(df0, "hc_rate","l_hc_rate")
  df0 <- mk_log(df0, "he_rate","l_he_rate")
  df0 <- mk_log(df0, "det_tot_rate","l_det_tot_rate")
  df0 <- mk_log(df0, "det_ext_rate","l_det_ext_rate")
  # Controles potenciales (niveles → logs)
  df0 <- mk_log(df0, "arope","l_arope")
  df0 <- mk_log(df0, "paro_15_29","l_paro_15_29")
  df0 <- mk_log(df0, "pib_pc_real","l_pib_pc_real")

  must_have <- c("ano","l_share_extranjeros",
                 "l_hc_rate","l_he_rate","l_det_tot_rate","l_det_ext_rate")
  if (!all(must_have %in% names(df0))) {
    faltan <- paste(setdiff(must_have, names(df0)), collapse=", ")
    fatal(paste0("Faltan columnas base en el CSV maestro: ", faltan))
  }

  # Dummies COVID
  add_covid <- function(d) {
    if (!"covid_2020" %in% names(d)) d$covid_2020 <- ifelse(d$ano == 2020L, 1L, 0L)
    if (!"covid_2021" %in% names(d)) d$covid_2021 <- ifelse(d$ano == 2021L, 1L, 0L)
    d
  }

  # Ventanas
  wins <- list(
    `2010_2023` = function(x) x$ano >= 2010 & x$ano <= 2023,
    `sin_covid` = function(x) x$ano %in% setdiff(2010:2023, c(2020L, 2021L))
  )

  Ys_all <- c("l_det_ext_rate","l_det_tot_rate","l_hc_rate","l_he_rate")
  Xname  <- "l_share_extranjeros"

  rows_res <- list(); rows_specs <- list(); rows_diag <- list(); rows_roots <- list()

  # Helper: orden máximo de integración d (0 o 1) vía ADF (ur.df)
  infer_d <- function(z) {
    # Si no rechaza raíz unitaria en niveles (5%), asumimos I(1)
    adf <- tryCatch(urca::ur.df(z, type = "drift", lags = 1), error = function(e) NULL)
    if (is.null(adf)) return(1L)
    stat <- tryCatch(adf@teststat[["tau2"]], error = function(e) NA_real_)
    crit <- tryCatch(adf@cval[["tau2"]]["5pct"], error = function(e) NA_real_)
    if (is.finite(stat) && is.finite(crit) && stat < crit) 0L else 1L
  }

  for (wname in names(wins)) {
    dtw <- df0[wins[[wname]](df0), , drop = FALSE]
    if (nrow(dtw) < 8L) next
    dtw <- add_covid(dtw)

    # d = max d(Ys, X)
    series <- dtw[, unique(c(Ys_all, Xname)), drop = FALSE]
    d_orders <- sapply(series, function(s) infer_d(as.numeric(s)))
    d <- max(1L, max(as.integer(d_orders), na.rm = TRUE)) # conservador: al menos 1

    for (use_covid in c(FALSE, TRUE)) {
      for (use_controls in c(FALSE, TRUE)) {
        for (y in Ys_all) {
          # Endógenas niveles (logs)
          base_cols <- c("ano", y, Xname, "covid_2020","covid_2021",
                         "l_arope","l_paro_15_29","l_pib_pc_real")
          dt_min <- dtw[, intersect(base_cols, names(dtw)), drop = FALSE]

          ok_yx <- stats::complete.cases(dt_min[, intersect(c(y,Xname), names(dt_min)), drop = FALSE])
          dt_eff <- dt_min[ok_yx, , drop = FALSE]
          n_eff  <- nrow(dt_eff)
          if (n_eff < (5L + d)) {
            rows_res[[length(rows_res)+1]] <- tibble(
              window = wname, y = y, x = Xname, covid = use_covid, controls = use_controls,
              d = d, p_bic = NA_integer_, p_used = NA_integer_,
              w_p_x_to_y = NA_real_, w_p_y_to_x = NA_real_,
              stable = NA, max_modulus = NA_real_, serial_p_value = NA_real_, n = n_eff
            )
            next
          }

          # Exógenas
          exog <- NULL; exog_names <- character(0)
          if (use_covid) {
            covs <- intersect(c("covid_2020","covid_2021"), names(dt_eff))
            exog_names <- c(exog_names, covs)
          }
          if (use_controls) {
            ctrls <- intersect(c("l_arope","l_paro_15_29","l_pib_pc_real"), names(dt_eff))
            exog_names <- c(exog_names, ctrls)
          }
          if (length(exog_names)) exog <- as.matrix(dt_eff[, exog_names, drop = FALSE])

          YX <- dt_eff[, c(y, Xname), drop = FALSE]
          YX <- as.data.frame(lapply(YX, function(v) as.numeric(v)))
          rownames(YX) <- dt_eff$ano

          # Selección p≤2 por BIC
          maxlags <- max(1L, min(2L, n_eff - 2L - d))
          sel <- tryCatch(vars::VARselect(YX, lag.max = maxlags, type = "const", exogen = exog),
                          error = function(e) NULL)
          p_bic <- if (!is.null(sel)) suppressWarnings(as.integer(sel$selection[["SC(n)"]])) else 1L
          p <- max(1L, min(2L, ifelse(is.finite(p_bic), p_bic, 1L)))
          p_aug <- p + d

          fit <- tryCatch(vars::VAR(YX, p = p_aug, type = "const", exogen = exog),
                          error = function(e) NULL)
          if (is.null(fit)) {
            rows_res[[length(rows_res)+1]] <- tibble(
              window = wname, y = y, x = Xname, covid = use_covid, controls = use_controls,
              d = d, p_bic = p_bic, p_used = NA_integer_,
              w_p_x_to_y = NA_real_, w_p_y_to_x = NA_real_,
              stable = NA, max_modulus = NA_real_, serial_p_value = NA_real_, n = n_eff
            )
            next
          }

          # Estabilidad del VAR(p+d)
          roots <- tryCatch(vars::roots(fit, modulus = TRUE), error = function(e) NA_real_)
          max_mod <- suppressWarnings(max(as.numeric(roots), na.rm = TRUE))
          stable_flag <- is.finite(max_mod) && (max_mod < 1)

          # Portmanteau (ajustado) sobre VAR(p+d)
          lag_diag <- max(1L, min(5L, n_eff - p_aug - 1L))
          pt_p <- tryCatch(vars::serial.test(fit, lags.pt = lag_diag, type = "PT.adjusted")$serial$p.value,
                           error = function(e) NA_real_)

          # TY Wald: H0 “X no Granger-causa Y” = coeficientes de los PRIMEROS p rezagos de X en la ecuación de Y son 0
          # 'fit' contiene varresult (lista de lm), usamos car::linearHypothesis
          w_x_to_y <- NA_real_; w_y_to_x <- NA_real_
          try({
            eqY <- fit$varresult[[y]]
            # nombres coeficientes típicos: Xname.l1, Xname.l2, ...
            coefs <- names(coef(eqY))
            R <- coefs[grepl(paste0("^", Xname, "\\.l[1-", p, "]$"), coefs)]
            if (length(R)) {
              LH <- car::linearHypothesis(eqY, R)
              w_x_to_y <- as.numeric(LH[["Pr(>F)"]][2])
            }
          }, silent = TRUE)

          try({
            eqX <- fit$varresult[[Xname]]
            coefs2 <- names(coef(eqX))
            R2 <- coefs2[grepl(paste0("^", y, "\\.l[1-", p, "]$"), coefs2)]
            if (length(R2)) {
              LH2 <- car::linearHypothesis(eqX, R2)
              w_y_to_x <- as.numeric(LH2[["Pr(>F)"]][2])
            }
          }, silent = TRUE)

          rows_res[[length(rows_res)+1]] <- tibble(
            window = wname, y = y, x = Xname, covid = use_covid, controls = use_controls,
            d = d, p_bic = p_bic, p_used = p, p_aug = p_aug,
            w_p_x_to_y = w_x_to_y, w_p_y_to_x = w_y_to_x,
            stable = stable_flag, max_modulus = as.numeric(max_mod),
            serial_p_value = as.numeric(pt_p), n = as.integer(n_eff)
          )
          rows_specs[[length(rows_specs)+1]] <- tibble(
            window = wname, y = y, x = Xname, covid = use_covid, controls = use_controls,
            d = d, p_bic = p_bic, p_used = p, p_aug = p_aug,
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

  readr::write_csv(res_out,   here::here("output","tables","55_ty_granger_resultados.csv"))
  readr::write_csv(specs_out, here::here("output","tables","55_ty_specs.csv"))
  readr::write_csv(diag_out,  here::here("output","tables","55_ty_residual_tests.csv"))
  readr::write_csv(roots_out, here::here("output","tables","55_ty_roots_max.csv"))

  alpha <- 0.05
  resumen <- res_out |>
    mutate(
      ok_stable = isTRUE(stable) & is.finite(max_modulus) & (max_modulus < 1),
      ok_serial = is.finite(serial_p_value) & serial_p_value > alpha,
      sig_x_to_y = is.finite(w_p_x_to_y) & w_p_x_to_y < alpha,
      sig_y_to_x = is.finite(w_p_y_to_x) & w_p_y_to_x < alpha,
      decision_x_to_y = dplyr::case_when(
        sig_x_to_y & ok_serial ~ "ACEPTAR TY (X ⇒ Y)", # TY no requiere pretest estricto de raíces < 1
        sig_x_to_y            ~ "ACEPTAR TY (X ⇒ Y) (serial no OK)",
        TRUE ~ "NO"
      ),
      decision_y_to_x = dplyr::case_when(
        sig_y_to_x & ok_serial ~ "ACEPTAR TY (Y ⇒ X)",
        sig_y_to_x            ~ "ACEPTAR TY (Y ⇒ X) (serial no OK)",
        TRUE ~ "NO"
      )
    )
  readr::write_csv(resumen, here::here("output","tables","55_ty_granger_resumen.csv"))

  message("✅ 55 TY exportado: output/tables/55_ty_granger_resumen.csv")
  message("⚠️ Warnings capturados en: ", logf)
}

status <- tryCatch({ withCallingHandlers(main(), warning = function(w) message("WARN: ", conditionMessage(w))); TRUE },
                   error = function(e) { message("FATAL: ", conditionMessage(e)); FALSE })
if (!status && !interactive()) quit(save="no", status = 1L)
