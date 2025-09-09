#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
###############################################################################
# 24_var_granger.R — VAR bivariado en Δlog (X = d_l_share_extranjeros) vs Y
# Corre dos políticas para evidenciar el cambio:
#   - policy = "keep"   → mantiene 2020–2021 (con/sin dummies COVID)
#   - policy = "remove" → excluye 2020–2021 del análisis
#
# OUT (con columna 'policy' añadida):
#   output/tables/24_var_granger_resultados.csv
#   output/tables/24_var_specs.csv
#   output/tables/24_var_residual_tests.csv
#   output/tables/24_var_roots_max.csv
# IRFs:
#   output/figures/24_irf_{policy}_{window}_{Y}_covid{0|1}.png
###############################################################################

suppressPackageStartupMessages({
  need <- c("dplyr","readr","here","janitor","vars","stringr","rlang")
  miss <- need[!vapply(need, requireNamespace, FUN.VALUE = logical(1), quietly = TRUE)]
  if (length(miss)) {
    stop("Faltan paquetes: ", paste(miss, collapse = ", "),
         ". Instala con:\noptions(repos=c(CRAN='https://cloud.r-project.org'));\n",
         "install.packages(c('", paste(miss, collapse = "','"), "'))", call. = FALSE)
  }
  lapply(need, function(p) try(library(p, character.only = TRUE), silent = TRUE))
})

set.seed(123)

fatal <- function(..., status = 1L){
  message("FATAL: ", paste0(..., collapse = ""))
  if (!interactive()) quit(save="no", status = status)
  stop(paste0(..., collapse = ""), call. = FALSE)
}
info <- function(...) message("ℹ ", paste0(..., collapse = ""))

resolve_data_path <- function(){
  cands <- c(
    here::here("data","processed","tasas","tasas_transformadas.csv"),
    here::here("data","processed","series.csv")
  )
  hit <- cands[file.exists(cands)]
  if (!length(hit)) fatal("No se encontró el archivo de datos. Probé:\n- ",
                          paste(cands, collapse = "\n- "),
                          "\nGenera primero las tasas transformadas.")
  hit[1]
}

YEAR_MIN <- 2010L; YEAR_MAX <- 2023L
COVID_YEARS <- c(2020L, 2021L)

Y_VARS <- c("d_l_hc_rate","d_l_he_rate","d_l_det_tot_rate","d_l_det_ext_rate")
X_NAME <- "d_l_share_extranjeros"

P_MIN <- 1L; P_MAX <- 2L # rezagos a evaluar
IRF_AHEAD <- 12L; IRF_RUNS <- 800L; IRF_CI <- 0.95; IRF_ORTHO <- TRUE

# ---- Lectura y limpieza base -------------------------------------------------
fp  <- resolve_data_path()
df0 <- readr::read_csv(fp, show_col_types = FALSE) |> janitor::clean_names()
if (!"ano" %in% names(df0) && "anio" %in% names(df0)) df0 <- dplyr::rename(df0, ano = anio)

req_cols <- c("ano", X_NAME, Y_VARS)
miss <- setdiff(req_cols, names(df0))
if (length(miss)) fatal("Faltan columnas en ", fp, ":\n- ", paste(miss, collapse = "\n- "),
                        "\nRevisa 19_derivadas_tasas_y_logs.R y/o 20_panel_nacional.R.")

df_base <- df0 |>
  dplyr::mutate(
    dplyr::across(dplyr::all_of(c(X_NAME, Y_VARS)), ~ suppressWarnings(as.numeric(.x))),
    ano = as.integer(ano)
  ) |>
  dplyr::filter(dplyr::between(ano, YEAR_MIN, YEAR_MAX)) |>
  dplyr::arrange(ano) |>
  dplyr::select(ano, dplyr::all_of(X_NAME), dplyr::all_of(Y_VARS))

if (nrow(df_base) < 8) fatal("Datos insuficientes tras filtrar por años (", nrow(df_base), ").")

# ---- Ejecutar para dos políticas: keep y remove -----------------------------
policies <- c("keep","remove")

all_res   <- list()
all_specs <- list()
all_diag  <- list()
all_roots <- list()

dir.create(here::here("output","tables"),  recursive = TRUE, showWarnings = FALSE)
dir.create(here::here("output","figures"), recursive = TRUE, showWarnings = FALSE)

for (policy in policies) {
  
  # 1) Preparar df según política
  df <- df_base
  if (identical(policy, "remove")) {
    df <- dplyr::filter(df, !ano %in% COVID_YEARS)
  }
  
  # 2) Ventanas por política
  if (identical(policy, "remove")) {
    WINDOWS <- list(
      `2010_2023_sin_covid` = setdiff(YEAR_MIN:YEAR_MAX, COVID_YEARS),
      `2010_2019_pre`       = YEAR_MIN:2019L
    )
    covid_loop <- c(FALSE) # no metemos dummies en 'remove'
  } else { # keep
    WINDOWS <- list(
      `2010_2023`      = YEAR_MIN:YEAR_MAX,
      `2010_2019_pre`  = YEAR_MIN:2019L,
      `2020_2023_post` = 2020L:2023L,
      `sin_covid`      = setdiff(YEAR_MIN:YEAR_MAX, COVID_YEARS)
    )
    covid_loop <- c(FALSE, TRUE) # probamos con/sin dummies
  }
  
  # 3) Bucle por ventana
  for (wname in names(WINDOWS)) {
    years <- WINDOWS[[wname]]
    d_win <- df[df$ano %in% years, , drop = FALSE]
    if (nrow(d_win) < 6) { info("Ventana ", wname, " con pocos años (", nrow(d_win), "). Salto."); next }
    
    # 4) Bucle covid (dummies) según política
    for (use_covid in covid_loop) {
      
      # exógenas (solo si policy=keep y la ventana contiene 2020/2021)
      exog <- NULL
      if (isTRUE(use_covid) && identical(policy, "keep")) {
        d_2020 <- as.integer(d_win$ano == 2020L)
        d_2021 <- as.integer(d_win$ano == 2021L)
        if (any(d_2020 != 0L) || any(d_2021 != 0L)) {
          exog <- cbind(d_2020 = d_2020, d_2021 = d_2021)
        } else {
          next # sin años COVID en esta ventana: no tiene sentido use_covid==TRUE
        }
      }
      
      for (y in Y_VARS) {
        keep_rows <- stats::complete.cases(d_win[, c("ano", X_NAME, y), drop = FALSE])
        sub  <- d_win[keep_rows, c("ano", X_NAME, y), drop = FALSE]
        n_eff <- nrow(sub)
        if (n_eff < (P_MIN + 4L)) next
        
        YX <- sub[, c(X_NAME, y), drop = FALSE]
        colnames(YX) <- c(X_NAME, y)
        
        sel <- tryCatch({
          s <- vars::VARselect(YX, lag.max = P_MAX, type = "const")
          aic <- suppressWarnings(as.integer(s$selection[["AIC(n)"]]))
          bic <- suppressWarnings(as.integer(s$selection[["SC(n)"]]))
          aic <- ifelse(is.na(aic) || aic < P_MIN, P_MIN, min(aic, P_MAX))
          bic <- ifelse(is.na(bic) || bic < P_MIN, P_MIN, min(bic, P_MAX))
          list(aic = aic, bic = bic)
        }, error = function(e) list(aic = 1L, bic = 1L))
        
        lag_aic <- sel$aic; lag_bic <- sel$bic
        p_try <- lag_bic
        
        fit <- NULL
        try_fit <- function(p){
          tryCatch(vars::VAR(YX, p = p, type = "const", exogen = exog), error = function(e) NULL)
        }
        fit <- try_fit(p_try)
        if (is.null(fit) && lag_aic != p_try) fit <- try_fit(lag_aic)
        if (is.null(fit)) fit <- try_fit(1L)
        if (is.null(fit)) {
          next
        }
        p_used <- fit$p
        
        # Estabilidad
        roots <- tryCatch(vars::roots(fit, modulus = TRUE), error = function(e) NA_real_)
        max_mod <- suppressWarnings(max(as.numeric(roots), na.rm = TRUE))
        stable_flag <- is.finite(max_mod) && (max_mod < 1)
        
        # Diagnóstico residual
        lag_diag <- max(1L, min(8L, n_eff - p_used - 1L))
        pt_p <- tryCatch(
          vars::serial.test(fit, lags.pt = lag_diag, type = "PT.asymptotic")$serial$p.value,
          error = function(e) NA_real_
        )
        if (!is.finite(pt_p)) {
          resY <- tryCatch(stats::resid(fit)[, y], error = function(e) NULL)
          pt_p <- tryCatch(stats::Box.test(resY, lag = lag_diag, type = "Ljung-Box")$p.value,
                           error = function(e) NA_real_)
        }
        
        # Granger
        p_y_on_x <- tryCatch(vars::causality(fit, cause = X_NAME)$Granger$p.value, error = function(e) NA_real_)
        p_x_on_y <- tryCatch(vars::causality(fit, cause = y)$Granger$p.value,    error = function(e) NA_real_)
        
        robust_flag <- isTRUE(stable_flag) && is.finite(pt_p) && pt_p >= 0.05 && n_eff >= 8
        
        all_res[[length(all_res)+1]] <- tibble::tibble(
          policy = policy, window = wname, y = y, x = X_NAME, covid = use_covid,
          p_y_on_x = as.numeric(p_y_on_x), p_x_on_y = as.numeric(p_x_on_y),
          lag_aic  = as.integer(lag_aic),  lag_bic  = as.integer(lag_bic),
          p_used   = as.integer(p_used),   stable   = stable_flag,
          max_modulus = as.numeric(max_mod),
          serial_p_value = as.numeric(pt_p),
          n = as.integer(n_eff),
          robust = robust_flag
        )
        
        all_specs[[length(all_specs)+1]] <- tibble::tibble(
          policy = policy, window = wname, y = y, x = X_NAME, covid = use_covid,
          p_selected_aic = as.integer(lag_aic),
          p_selected_bic = as.integer(lag_bic),
          p_used = as.integer(p_used),
          exog = if (!is.null(exog)) paste(colnames(exog), collapse = "+") else ""
        )
        
        all_diag[[length(all_diag)+1]] <- tibble::tibble(
          policy = policy, window = wname, y = y, x = X_NAME, covid = use_covid,
          lags_pt = as.integer(lag_diag),
          serial_p_value = as.numeric(pt_p)
        )
        
        all_roots[[length(all_roots)+1]] <- tibble::tibble(
          policy = policy, window = wname, y = y, x = X_NAME, covid = use_covid,
          max_modulus = as.numeric(max_mod),
          stable = stable_flag
        )
        
        # IRFs (PNG base R — robusto)
        if (isTRUE(stable_flag)) {
          fn <- here::here("output","figures",
                           sprintf("24_irf_%s_%s_%s_covid%s.png",
                                   policy, wname, y, ifelse(isTRUE(use_covid),"1","0")))
          irf_obj <- tryCatch(
            vars::irf(fit, impulse = X_NAME, response = y,
                      n.ahead = IRF_AHEAD, boot = TRUE, runs = IRF_RUNS,
                      ci = IRF_CI, ortho = IRF_ORTHO),
            error = function(e) NULL
          )
          if (!is.null(irf_obj)) {
            try({
              grDevices::png(fn, width = 1100, height = 700, res = 125, bg = "white")
              plot(irf_obj, main = sprintf("IRF: %s <- %s | %s | policy=%s",
                                           y, X_NAME, wname, policy))
              grDevices::dev.off()
            }, silent = TRUE)
          }
        }
        
      } # Y loop
    }   # covid loop
  }     # window loop
}       # policy loop

# ---- Exportar todo -----------------------------------------------------------
if (!length(all_res)) fatal("No se generaron resultados (¿datos insuficientes?).")

res_out   <- dplyr::bind_rows(all_res)   |> dplyr::arrange(policy, window, y, covid)
specs_out <- dplyr::bind_rows(all_specs) |> dplyr::arrange(policy, window, y, covid)
diag_out  <- dplyr::bind_rows(all_diag)  |> dplyr::arrange(policy, window, y, covid)
roots_out <- dplyr::bind_rows(all_roots) |> dplyr::arrange(policy, window, y, covid)

readr::write_csv(res_out,   here::here("output","tables","24_var_granger_resultados.csv"))
readr::write_csv(specs_out, here::here("output","tables","24_var_specs.csv"))
readr::write_csv(diag_out,  here::here("output","tables","24_var_residual_tests.csv"))
readr::write_csv(roots_out, here::here("output","tables","24_var_roots_max.csv"))

message("✅ VAR/Granger exportado (con y sin COVID) → output/tables/24_var_granger_resultados.csv")
message("ℹ IRFs por política en: output/figures/24_irf_{policy}_*.png")


