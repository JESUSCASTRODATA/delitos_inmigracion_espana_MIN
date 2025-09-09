#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
###############################################################################
# 27_sanity_checks.R — Denominadores alternativos, placebos, VIF, influencia
###############################################################################

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(here); library(janitor)
  library(tidyr); library(purrr); library(fs)
})

fs::dir_create(here("output","tables"))

# ------------------------- Helpers generales ---------------------------------
d1 <- function(x) c(NA, diff(x))
fmt_formula <- function(f) paste(deparse(f, width.cutoff = 500), collapse = " ")

# HAC (Newey–West) sin depender de 'lmtest' (p-valor via t-Student)
has_sandwich <- requireNamespace("sandwich", quietly = TRUE)
hac_vcov <- function(fit, lag = NULL){
  if (!has_sandwich) return(stats::vcov(fit))
  n <- stats::nobs(fit); if (is.null(lag)) lag <- floor(4*(n/100)^(2/9))
  sandwich::NeweyWest(fit, lag = lag, prewhite = FALSE, adjust = TRUE)
}
coeftable_hac <- function(fit) {
  b  <- stats::coef(fit)
  V  <- hac_vcov(fit)
  cn_b <- names(b); cn_V <- colnames(V)
  keep <- intersect(cn_b, cn_V)
  if (!length(keep)) return(tibble(term=character(), estimate=double(), std_error=double(), statistic=double(), p_value=double()))
  b <- b[keep]; V <- V[keep, keep, drop=FALSE]
  se <- sqrt(diag(V)); tval <- b / se
  df  <- max(1L, stats::nobs(fit) - length(b))
  p   <- 2*stats::pt(abs(tval), df=df, lower.tail=FALSE)
  tibble(term=keep, estimate=as.numeric(b), std_error=as.numeric(se),
         statistic=as.numeric(tval), p_value=as.numeric(p))
}

# VIF manual (sin 'car'): para modelo lineal; ignora intercepto
compute_vif <- function(fit){
  X <- stats::model.matrix(fit)
  nm <- colnames(X)
  nm <- nm[nm != "(Intercept)"]
  if (!length(nm)) return(tibble(term=character(), vif=double()))
  out <- lapply(nm, function(j){
    yj  <- X[, j]
    Xj  <- X[, setdiff(nm, j), drop=FALSE]
    if (ncol(Xj) == 0) return(tibble(term=j, vif=1))
    r2  <- tryCatch(summary(lm(yj ~ Xj))$r.squared, error=function(e) NA_real_)
    vif <- if (is.finite(r2)) 1/(1 - r2) else NA_real_
    tibble(term=j, vif=vif)
  })
  bind_rows(out)
}

# Lectura robusta de series maestras ------------------------------------------
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
    stop("No existe ni tasas/transformadas ni panel_nacional.")
  }
}

df0 <- read_master() |> mutate(ano = as.integer(ano))

# Crear d_l_* si faltan (a partir de l_*)
mk_dlog_if_missing <- function(df){
  lcols <- grep("^l_.+", names(df), value = TRUE)
  for (lc in lcols) {
    dc <- paste0("d_", lc)
    if (!dc %in% names(df)) df[[dc]] <- d1(df[[lc]])
  }
  df
}
df0 <- mk_dlog_if_missing(df0)

# Ventanas de análisis
wins <- list(
  `2010_2023`      = df0$ano >= 2010 & df0$ano <= 2023,
  `2010_2019_pre`  = df0$ano >= 2010 & df0$ano <= 2019,
  `2020_2023_post` = df0$ano >= 2020 & df0$ano <= 2023,
  `sin_covid`      = df0$ano %in% setdiff(2010:2023, c(2020, 2021))
)

# Columnas clave si existen
Ys_dlog <- c("d_l_hc_rate","d_l_he_rate","d_l_det_tot_rate","d_l_det_ext_rate")
Xs_dlog <- c("d_l_share_extranjeros","d_l_poblacion_extranjera")
Z_dlog  <- c("d_l_pib_pc_real","d_l_paro_15_29","d_l_arope")
covid_dums <- c("covid_2020","covid_2021")

Ys_dlog <- Ys_dlog[Ys_dlog %in% names(df0)]
Xs_dlog <- Xs_dlog[Xs_dlog %in% names(df0)]
Z_dlog  <- Z_dlog[ Z_dlog  %in% names(df0)]
covid_dums <- covid_dums[covid_dums %in% names(df0)]

# ------------------------- A) Denominadores alternativos ----------------------
safe_read <- function(path) if (file.exists(path)) readr::read_csv(path, show_col_types = FALSE) |> janitor::clean_names() else NULL

det_tot <- safe_read(here("data","processed","detenciones_totales_total.csv"))      # (ano, det_tot)
det_ext <- safe_read(here("data","processed","detenciones_extranjeros_total.csv"))  # (ano, det_ext)
pop_tot <- safe_read(here("data","processed","poblacion_total_nacional.csv"))       # (ano, poblacion_total)
pop_ext <- safe_read(here("data","processed","poblacion_extranjera_total.csv"))     # (ano, poblacion_extranjera)
pop_15  <- safe_read(here("data","processed","poblacion_15_29_anual.csv"))          # (ano, poblacion_15_29) (si existe)

alt_out <- NULL
if (!is.null(det_tot) && !is.null(pop_tot)) {
  alt <- det_tot |> inner_join(pop_tot, by="ano")
  alt <- alt |> mutate(
    rate_tot_per_total = 1e5 * det_tot / poblacion_total
  )
  if (!is.null(pop_15)) {
    alt <- alt |> inner_join(pop_15, by="ano", relationship = "many-to-many", suffix=c("", "_1529")) |>
      mutate(rate_tot_per_15_29 = 1e5 * det_tot / poblacion_15_29)
  } else {
    alt$rate_tot_per_15_29 <- NA_real_
  }
  alt_out <- alt
}
if (!is.null(det_ext) && !is.null(pop_ext)) {
  if (is.null(alt_out)) alt_out <- det_ext else alt_out <- alt_out |> full_join(det_ext, by="ano")
  alt_out <- alt_out |> full_join(pop_ext, by="ano") |>
    mutate(rate_ext_per_foreign = 1e5 * det_ext / poblacion_extranjera)
}

if (!is.null(alt_out)) {
  alt_out <- alt_out |>
    arrange(ano) |>
    mutate(
      l_rate_tot_total      = log(pmax(rate_tot_per_total,      1e-12)),
      l_rate_tot_15_29      = log(pmax(rate_tot_per_15_29,      1e-12)),
      l_rate_ext_foreign    = log(pmax(rate_ext_per_foreign,    1e-12)),
      d_l_rate_tot_total    = d1(l_rate_tot_total),
      d_l_rate_tot_15_29    = d1(l_rate_tot_15_29),
      d_l_rate_ext_foreign  = d1(l_rate_ext_foreign)
    )
  readr::write_csv(alt_out, here("output","tables","27_alt_rates.csv"))
} else {
  readr::write_csv(tibble(info="No se pudieron construir tasas alternativas: faltan insumos."),
                   here("output","tables","27_alt_rates.csv"))
}

# ------------------------- B) Placebo temporal (lead de X) --------------------
placebo_rows <- list()

run_placebo <- function(dw, y, x, ctrls, covid_cols){
  # Δlog(Y_t) ~ Δlog(X_{t+1}) + ctrls + ΔY_{t-1}
  dY <- dw[[y]]
  Xl <- dplyr::lead(dw[[x]], 1)  # lead de X
  dat <- dw
  dat$X_lead <- Xl
  dat$d_y_l1 <- dplyr::lag(dY, 1)
  rhs <- c("X_lead", ctrls, covid_cols, "d_y_l1")
  rhs <- rhs[rhs %in% names(dat)]
  form <- as.formula(paste(y, "~", paste(rhs, collapse=" + ")))
  dt2 <- dat |> tidyr::drop_na(all_of(all.vars(form)))
  if (nrow(dt2) < 8) return(NULL)
  fit <- stats::lm(form, data = dt2)
  ct  <- coeftable_hac(fit)
  row_x <- ct |> filter(term == "X_lead")
  if (nrow(row_x) == 0) return(NULL)
  ci_lo <- row_x$estimate - 1.96*row_x$std_error
  ci_hi <- row_x$estimate + 1.96*row_x$std_error
  tibble(
    formula = fmt_formula(form),
    n = stats::nobs(fit),
    coef_lead = row_x$estimate[1],
    se_lead   = row_x$std_error[1],
    t_lead    = row_x$statistic[1],
    p_lead    = row_x$p_value[1],
    ci_lo = ci_lo[1], ci_hi = ci_hi[1]
  )
}

for (wname in names(wins)) {
  dw <- df0[wins[[wname]], , drop = FALSE]
  for (y in Ys_dlog) for (x in Xs_dlog) {
    res <- run_placebo(dw, y, x, Z_dlog, covid_dums)
    if (!is.null(res)) {
      res$window <- wname; res$y <- y; res$x <- x
      placebo_rows[[length(placebo_rows)+1]] <- res |> select(window, y, x, everything())
    } else {
      placebo_rows[[length(placebo_rows)+1]] <- tibble(window=wname, y=y, x=x,
                                                       formula=NA_character_, n=NA_integer_,
                                                       coef_lead=NA_real_, se_lead=NA_real_,
                                                       t_lead=NA_real_, p_lead=NA_real_,
                                                       ci_lo=NA_real_, ci_hi=NA_real_)
    }
  }
}
placebo_out <- bind_rows(placebo_rows) |> arrange(window, y, x)
readr::write_csv(placebo_out, here("output","tables","27_placebo_leadX.csv"))

# ------------------------- C) VIF (multicolinealidad) -------------------------
vif_rows <- list()

run_vif <- function(dw, y, x, ctrls, covid_cols){
  rhs <- c(x, ctrls, covid_cols) ; rhs <- rhs[rhs %in% names(dw)]
  form <- as.formula(paste(y, "~", paste(rhs, collapse = " + ")))
  dt2  <- dw |> tidyr::drop_na(all_of(all.vars(form)))
  if (nrow(dt2) < 8 || length(rhs) < 1) return(NULL)
  fit  <- stats::lm(form, data = dt2)
  vf   <- compute_vif(fit) |> mutate(r2_from_vif = ifelse(is.finite(vif), 1 - 1/vif, NA_real_))
  tibble(
    formula = fmt_formula(form),
    n = stats::nobs(fit),
    term = vf$term,
    vif  = vf$vif,
    r2_from_vif = vf$r2_from_vif
  )
}

for (wname in names(wins)) {
  dw <- df0[wins[[wname]], , drop = FALSE]
  for (y in Ys_dlog) for (x in Xs_dlog) {
    res <- run_vif(dw, y, x, Z_dlog, covid_dums)
    if (!is.null(res)) {
      res$window <- wname; res$y <- y; res$x <- x
      vif_rows[[length(vif_rows)+1]] <- res |> select(window, y, x, everything())
    } else {
      vif_rows[[length(vif_rows)+1]] <- tibble(window=wname, y=y, x=x,
                                               formula=NA_character_, n=NA_integer_,
                                               term=NA_character_, vif=NA_real_, r2_from_vif=NA_real_)
    }
  }
}
vif_out <- bind_rows(vif_rows) |> arrange(window, y, x, term)
readr::write_csv(vif_out, here("output","tables","27_vif_multicol.csv"))

# ------------------------- D) Influencia y sensibilidad -----------------------
infl_rows <- list()
sens_rows <- list()

run_influence <- function(dw, y, x, ctrls, covid_cols){
  rhs <- c(x, ctrls, covid_cols, "d_y_l1") ; rhs <- rhs[rhs %in% names(dw)]
  # Añade d_y_l1
  dd <- dw |> mutate(d_y_l1 = dplyr::lag(.data[[y]], 1))
  form <- as.formula(paste(y, "~", paste(rhs, collapse = " + ")))
  d2 <- dd |> tidyr::drop_na(all_of(all.vars(form)))
  if (nrow(d2) < 8) return(NULL)
  fit <- stats::lm(form, data = d2)
  # Métricas de influencia
  infl <- stats::influence.measures(fit)
  summ <- as.data.frame(infl$infmat)
  summ$ano <- d2$ano
  summ$std_resid <- rstandard(fit)
  summ$cooks_d   <- cooks.distance(fit)
  summ$leverage  <- hatvalues(fit)
  p <- length(stats::coef(fit)) - 1
  n <- nrow(d2)
  lev_thr <- 2*(p+1)/n
  ck_thr  <- 4/n
  flags <- tibble(
    ano = summ$ano,
    std_resid = as.numeric(summ$std_resid),
    leverage  = as.numeric(summ$leverage),
    cooks_d   = as.numeric(summ$cooks_d),
    flag_resid = abs(summ$std_resid) > 2.5,
    flag_lev   = summ$leverage > lev_thr,
    flag_cook  = summ$cooks_d > ck_thr,
    flag_any   = (abs(summ$std_resid) > 2.5) | (summ$leverage > lev_thr) | (summ$cooks_d > ck_thr)
  )
  # Coeficiente de X original
  ct  <- coeftable_hac(fit)
  rowx <- ct |> filter(term == x)
  coef0 <- if (nrow(rowx)==1) rowx$estimate[1] else NA_real_
  se0   <- if (nrow(rowx)==1) rowx$std_error[1] else NA_real_
  # Re-estimar excluyendo influyentes
  idx_keep <- which(!flags$flag_any)
  coef1 <- se1 <- NA_real_; n1 <- NA_integer_
  if (length(idx_keep) >= 8) {
    fit2 <- stats::lm(form, data = d2[idx_keep, , drop = FALSE])
    ct2  <- coeftable_hac(fit2)
    r2   <- ct2 |> filter(term == x)
    coef1 <- if (nrow(r2)==1) r2$estimate[1] else NA_real_
    se1   <- if (nrow(r2)==1) r2$std_error[1] else NA_real_
    n1    <- stats::nobs(fit2)
  }
  list(flags = flags, sens = tibble(
    formula = fmt_formula(form),
    n_full = n, coef_x_full = coef0, se_x_full = se0,
    n_excl = n1, coef_x_excl = coef1, se_x_excl = se1
  ))
}

for (wname in names(wins)) {
  dw <- df0[wins[[wname]], , drop = FALSE]
  for (y in Ys_dlog) for (x in Xs_dlog) {
    res <- run_influence(dw, y, x, Z_dlog, covid_dums)
    if (!is.null(res)) {
      flg <- res$flags |> mutate(window=wname, y=y, x=x, .before=1)
      sns <- res$sens  |> mutate(window=wname, y=y, x=x, .before=1)
      infl_rows[[length(infl_rows)+1]] <- flg
      sens_rows[[length(sens_rows)+1]] <- sns
    } else {
      infl_rows[[length(infl_rows)+1]] <- tibble(window=wname, y=y, x=x, ano=NA_integer_,
                                                 std_resid=NA_real_, leverage=NA_real_, cooks_d=NA_real_,
                                                 flag_resid=NA, flag_lev=NA, flag_cook=NA, flag_any=NA)
      sens_rows[[length(sens_rows)+1]] <- tibble(window=wname, y=y, x=x,
                                                 formula=NA_character_, n_full=NA_integer_,
                                                 coef_x_full=NA_real_, se_x_full=NA_real_,
                                                 n_excl=NA_integer_, coef_x_excl=NA_real_, se_x_excl=NA_real_)
    }
  }
}

influence_out <- bind_rows(infl_rows) |> arrange(window, y, x, ano)
sensitivity_out <- bind_rows(sens_rows) |> arrange(window, y, x)

readr::write_csv(influence_out,  here("output","tables","27_influence_flags.csv"))
readr::write_csv(sensitivity_out, here("output","tables","27_sensitivity_excl_influential.csv"))

message("✅ 27 listo: alt_rates, placebos, VIF e influencia/sensibilidad exportados.")
