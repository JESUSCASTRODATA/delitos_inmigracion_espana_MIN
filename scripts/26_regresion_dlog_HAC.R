#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
###############################################################################
# 26_regresion_dlog_HAC.R  (blindado contra aliasing de coeficientes)
#
# Δlog(Y) ~ Δlog(X) + controles, errores HAC (Newey–West).
# Ventanas: 2010–2023, 2010–2019, 2020–2023, sin_covid.
# OUT:
#   output/tables/26_dlog_coefs.csv
#   output/tables/26_dlog_overview.csv
#   output/tables/26_dlog_qc_presentes.csv
###############################################################################

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(here); library(janitor)
  library(tidyr); library(purrr); library(fs)
})

# Paquetes opcionales
has_sandwich <- requireNamespace("sandwich", quietly = TRUE)
has_lmtest   <- requireNamespace("lmtest",   quietly = TRUE)

fs::dir_create(here("output","tables"))

# ---------- Helpers ----------
d1 <- function(x) c(NA, diff(x))

hac_vcov <- function(fit, lag = NULL){
  if (!has_sandwich) return(stats::vcov(fit))
  n <- stats::nobs(fit); if (is.null(lag)) lag <- floor(4*(n/100)^(2/9))
  sandwich::NeweyWest(fit, lag = lag, prewhite = FALSE, adjust = TRUE)
}

# TABLA DE COEFICIENTES ROBUSTA A ALIASING
coeftable_hac <- function(fit) {
  b  <- stats::coef(fit)                                # puede contener NA (aliased)
  V  <- hac_vcov(fit)                                   # suele excluir los aliased
  cn_b <- names(b); cn_V <- colnames(V)
  keep <- intersect(cn_b, cn_V)
  if (length(keep) == 0L) {
    return(tibble::tibble(term=character(), estimate=double(),
                          std_error=double(), statistic=double(), p_value=double()))
  }
  b_kept <- b[keep]
  Vk     <- V[keep, keep, drop = FALSE]
  se     <- sqrt(diag(Vk))
  tval   <- b_kept / se
  dfres  <- max(1L, stats::nobs(fit) - length(b_kept))
  pval   <- 2*stats::pt(abs(tval), df = dfres, lower.tail = FALSE)
  tibble::tibble(
    term      = keep,
    estimate  = as.numeric(b_kept),
    std_error = as.numeric(se),
    statistic = as.numeric(tval),
    p_value   = as.numeric(pval)
  )
}

fmt_formula <- function(f) paste(deparse(f, width.cutoff = 500), collapse = " ")

# Lectura master y creación de d_l_* si faltan -------------------------------
read_master <- function(){
  fp1 <- here("data","processed","tasas","tasas_transformadas.csv")
  fp2 <- here("data","processed","panel_nacional_2010_2023.csv")
  if (file.exists(fp1)) {
    message("✓ Usando master: ", fp1)
    return(readr::read_csv(fp1, show_col_types = FALSE) |> janitor::clean_names())
  } else if (file.exists(fp2)) {
    message("ℹ Master no encontrado; usando fallback: ", fp2)
    return(readr::read_csv(fp2, show_col_types = FALSE) |> janitor::clean_names())
  } else {
    stop("No existe ni tasas/transformadas ni el panel_nacional. Ejecuta antes el 20/23.")
  }
}

mk_dlog_if_missing <- function(df){
  lcols  <- grep("^l_.+", names(df), value = TRUE)
  for (lc in lcols) {
    dc <- paste0("d_", lc)
    if (!dc %in% names(df)) df[[dc]] <- d1(df[[lc]])
  }
  df
}

df0 <- read_master() |> mutate(ano = as.integer(ano))
df0 <- mk_dlog_if_missing(df0)

# Variables -------------------------------------------------------------------
Ys_all <- c("d_l_hc_rate","d_l_he_rate","d_l_det_tot_rate","d_l_det_ext_rate")
Xs_all <- c("d_l_share_extranjeros","d_l_poblacion_extranjera")
Z_all  <- c("d_l_pib_pc_real","d_l_paro_15_29","d_l_arope")
covid_dums <- c("covid_2020","covid_2021")

present <- tibble::tibble(variable = c(Ys_all, Xs_all, Z_all, covid_dums),
                          presente = c(Ys_all, Xs_all, Z_all, covid_dums) %in% names(df0))
readr::write_csv(present, here("output","tables","26_dlog_qc_presentes.csv"))

Ys <- Ys_all[Ys_all %in% names(df0)]
Xs <- Xs_all[Xs_all %in% names(df0)]
Zs <- Z_all[Z_all %in% names(df0)]
covid_dums <- covid_dums[covid_dums %in% names(df0)]

if (length(Ys) == 0) stop("No hay series Y (d_l_*_rate) disponibles.")
if (length(Xs) == 0) stop("No hay series X (d_l_share_extranjeros / d_l_poblacion_extranjera) disponibles.")

# Ventanas ---------------------------------------------------------------------
wins <- list(
  `2010_2023`      = df0$ano >= 2010 & df0$ano <= 2023,
  `2010_2019_pre`  = df0$ano >= 2010 & df0$ano <= 2019,
  `2020_2023_post` = df0$ano >= 2020 & df0$ano <= 2023,
  `sin_covid`      = df0$ano %in% setdiff(2010:2023, c(2020, 2021))
)

# Especificaciones
spec_defs <- list(
  A = function(y, x, Zs, covid) list(name="A", rhs=c(x, covid),                add_dy_l1=FALSE),
  B = function(y, x, Zs, covid) list(name="B", rhs=c(x, Zs, covid),            add_dy_l1=FALSE),
  C = function(y, x, Zs, covid) list(name="C", rhs=c(x, Zs, covid, "d_y_l1"),  add_dy_l1=TRUE)
)

# Runner de un modelo ----------------------------------------------------------
run_one <- function(dat, y, x, rhs_vars, add_dy_l1=FALSE){
  dt <- dat
  if (isTRUE(add_dy_l1)) {
    dt <- dt |> mutate(d_y = .data[[y]], d_y_l1 = dplyr::lag(d_y, 1))
  }
  rhs_vars <- rhs_vars[rhs_vars %in% names(dt)]
  if (length(rhs_vars) == 0L) {
    # sin RHS no tiene sentido estimar
    return(list(ok=FALSE, n=0L, k=0L, form_text=paste(y, "~ 1"), coefs=NULL, meta=NULL))
  }
  form <- as.formula(paste(y, "~", paste(rhs_vars, collapse = " + ")))
  dt2 <- dt |> tidyr::drop_na(all_of(all.vars(form)))
  if (nrow(dt2) < 8) {
    return(list(ok=FALSE, n=nrow(dt2), k=length(all.vars(form))-1,
                form_text=fmt_formula(form), coefs=NULL, meta=NULL))
  }
  fit <- stats::lm(form, data = dt2)
  ct  <- coeftable_hac(fit) |>
    mutate(formula = fmt_formula(form),
           n = stats::nobs(fit),
           k = length(stats::coef(fit))-1,
           r_squared = summary(fit)$r.squared,
           adj_r2    = summary(fit)$adj.r.squared,
           aic = AIC(fit), bic = BIC(fit))
  meta <- ct %>% summarise(
    n = max(n), k = max(k),
    r_squared = max(r_squared), adj_r2 = max(adj_r2),
    aic = max(aic), bic = max(bic)
  )
  list(ok=TRUE, n=nrow(dt2), k=length(stats::coef(fit))-1, form_text=fmt_formula(form),
       coefs=ct, meta=meta)
}

# Bucle principal --------------------------------------------------------------
coefs_rows <- list()
overview_rows <- list()

for (wname in names(wins)) {
  dw <- df0[wins[[wname]], , drop = FALSE]
  for (y in Ys) for (x in Xs) {
    covid <- covid_dums
    for (sname in names(spec_defs)) {
      spec <- spec_defs[[sname]](y, x, Zs, covid)
      res  <- run_one(dw, y, x, spec$rhs, add_dy_l1 = spec$add_dy_l1)
      if (!isTRUE(res$ok)) {
        overview_rows[[length(overview_rows)+1]] <- tibble::tibble(
          window=wname, y=y, x=x, spec=sname, formula=res$form_text, n=res$n, k=res$k,
          r_squared=NA_real_, adj_r2=NA_real_, aic=NA_real_, bic=NA_real_,
          coef_x=NA_real_, se_x=NA_real_, t_x=NA_real_, p_x=NA_real_, ci_lo=NA_real_, ci_hi=NA_real_
        )
      } else {
        # Coefs detallados
        coefs_rows[[length(coefs_rows)+1]] <- res$coefs |>
          mutate(window=wname, y=y, x=x, spec=sname, .before=1) |>
          select(window, y, x, spec, formula, term, estimate, std_error, statistic, p_value,
                 n, k, r_squared, adj_r2, aic, bic)
        # Extraer coeficiente del X (si existe)
        row_x <- res$coefs |> filter(term == x)
        if (nrow(row_x) == 1) {
          ci_lo <- row_x$estimate - 1.96*row_x$std_error
          ci_hi <- row_x$estimate + 1.96*row_x$std_error
          overview_rows[[length(overview_rows)+1]] <- tibble::tibble(
            window=wname, y=y, x=x, spec=sname, formula=res$form_text,
            n=res$meta$n[1], k=res$meta$k[1],
            r_squared=res$meta$r_squared[1], adj_r2=res$meta$adj_r2[1],
            aic=res$meta$aic[1], bic=res$meta$bic[1],
            coef_x=row_x$estimate[1], se_x=row_x$std_error[1],
            t_x=row_x$statistic[1], p_x=row_x$p_value[1],
            ci_lo=ci_lo[1], ci_hi=ci_hi[1]
          )
        } else {
          overview_rows[[length(overview_rows)+1]] <- tibble::tibble(
            window=wname, y=y, x=x, spec=sname, formula=res$form_text,
            n=res$meta$n[1], k=res$meta$k[1],
            r_squared=res$meta$r_squared[1], adj_r2=res$meta$adj_r2[1],
            aic=res$meta$aic[1], bic=res$meta$bic[1],
            coef_x=NA_real_, se_x=NA_real_, t_x=NA_real_, p_x=NA_real_,
            ci_lo=NA_real_, ci_hi=NA_real_
          )
        }
      }
    }
  }
}

coefs_out    <- dplyr::bind_rows(coefs_rows)    |> arrange(window, y, x, spec, term)
overview_out <- dplyr::bind_rows(overview_rows) |> arrange(window, y, x, spec)

# Sanidad: sin list-cols
stopifnot(!any(vapply(coefs_out,    is.list, logical(1))))
stopifnot(!any(vapply(overview_out, is.list, logical(1))))

readr::write_csv(coefs_out,    here("output","tables","26_dlog_coefs.csv"))
readr::write_csv(overview_out, here("output","tables","26_dlog_overview.csv"))
message("✅ 26 listo: 26_dlog_coefs.csv y 26_dlog_overview.csv")
