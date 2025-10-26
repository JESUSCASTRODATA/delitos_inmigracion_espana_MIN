#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
###############################################################################
# 18_stationarity_checks.R — Estacionariedad (ADF/PP/KPSS/ERS/NgP/ZA) por ventana
# Proyecto : Delitos e Inmigración en España (2010–2023)
# Autor    : Jesús Castro · JESUSCASTRODATA
# Fecha    : 2025-09-19
#
# Pruebas: ADF (drift/trend), PP (const/trend), KPSS (mu/tau),
#          ERS (ADF-GLS), Ng–Perron (MZt), Zivot–Andrews (ruptura)
# Ventanas: con_covid (2010–2023), sin_covid (sin 2020–2021), 2010–2019 (pre),
#           2022–2023 (post; se omite si N<8)
# Salidas : output/tables/18_stationarity*.csv, output/figures/18_stationarity_heatmap*.{png,svg}
###############################################################################

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(tidyr); library(here)
  library(urca);  library(purrr); library(stringr); library(ggplot2)
  library(methods); library(janitor)
})

msg <- function(...) message("[18] ", paste0(...))
ensure_dir <- function(path) if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
ensure_dir(here::here("output/tables")); ensure_dir(here::here("output/figures"))

# ------------------ Entrada ------------------
infile_candidates <- c(
  here::here("data/processed","core_indicadores_con_pib.csv"),
  here::here("data/processed","series.csv")
)
infile <- infile_candidates[file.exists(infile_candidates)][1]
if (is.na(infile)) stop("No se encontró ni core_indicadores_con_pib.csv ni series.csv", call. = FALSE)
msg("Usando entrada: ", normalizePath(infile, winslash = "/"))

DF <- readr::read_csv(infile, show_col_types = FALSE) |> janitor::clean_names()

# ------------------ Peek + alias SEGURO (sin fallar) -------------------------
cat("\n[PEEK] Columnas detectadas (", basename(infile), "):\n", sep = "")
print(names(DF))

alias_map <- list(
  ano                       = c("ano","year","anio","año"),
  tasa_hc_por_100k          = c("tasa_hc_por_100k","tasa_hc_100k"),
  tasa_he_por_100k          = c("tasa_he_por_100k","tasa_he_100k"),
  tasa_det_por_100k         = c("tasa_det_por_100k","tasa_det_100k"),
  share_ext                 = c("share_ext","share_extranjeros"),
  pct_extranjeros_poblacion = c("pct_extranjeros_poblacion","pct_extranjeros","porc_extranjeros","porcentaje_extranjeros"),
  pib_pc_real               = c("pib_pc_real")
)

pick_first <- function(nms, pool) { for (nm in nms) if (nm %in% pool) return(nm); NA_character_ }
safe_alias <- function(df, candidates, default = NA_real_) {
  nm <- pick_first(candidates, names(df))
  if (is.na(nm)) return(rep(default, nrow(df)))
  df[[nm]]
}

# Construye columnas canónicas sin errores aunque falten
DF <- DF |>
  mutate(
    ano  = suppressWarnings(as.integer(safe_alias(DF, alias_map$ano, default = NA_integer_))),
    tasa_hc_por_100k  = suppressWarnings(as.numeric(safe_alias(DF, alias_map$tasa_hc_por_100k))),
    tasa_he_por_100k  = suppressWarnings(as.numeric(safe_alias(DF, alias_map$tasa_he_por_100k))),
    tasa_det_por_100k = suppressWarnings(as.numeric(safe_alias(DF, alias_map$tasa_det_por_100k))),
    share_ext                 = suppressWarnings(as.numeric(safe_alias(DF, alias_map$share_ext))),
    pct_extranjeros_poblacion = suppressWarnings(as.numeric(safe_alias(DF, alias_map$pct_extranjeros_poblacion))),
    pib_pc_real               = suppressWarnings(as.numeric(safe_alias(DF, alias_map$pib_pc_real)))
  )

# Aviso de columnas canónicas vacías
can_names <- c("ano","tasa_hc_por_100k","tasa_he_por_100k","tasa_det_por_100k",
               "share_ext","pct_extranjeros_poblacion","pib_pc_real")
missing_soft <- can_names[vapply(DF[can_names], function(v) all(is.na(v)), logical(1))]
if (length(missing_soft)) {
  cat("\n[PEEK] Aviso: columnas canónicas sin datos (todo NA) → ",
      paste(missing_soft, collapse = ", "), "\n", sep = "")
}
if (all(is.na(DF$ano))) stop("Falta columna de año ('ano'/'year'/'anio'). No puedo continuar.", call. = FALSE)

# ------------------ Completar/normalizar canónicas ---------------------------
den_safe <- function(x) pmax(1e-9, as.numeric(x))

# Derivar tasa_det_por_100k si está vacía y hay insumos
if (all(is.na(DF$tasa_det_por_100k)) && all(c("det_tot","poblacion_total") %in% names(DF))) {
  DF <- DF |>
    mutate(tasa_det_por_100k = 1e5 * as.numeric(det_tot) / den_safe(poblacion_total))
  msg("Derivada 'tasa_det_por_100k' desde det_tot/poblacion_total.")
}

# Normalizar proporciones a [0,1] si vinieran en %
mx_share <- suppressWarnings(max(DF$share_ext, na.rm = TRUE))
if (is.finite(mx_share) && mx_share > 1) {
  DF <- DF |> mutate(share_ext = share_ext/100)
  msg("Normalizado 'share_ext' a proporción [0,1].")
}
mx_pct <- suppressWarnings(max(DF$pct_extranjeros_poblacion, na.rm = TRUE))
if (is.finite(mx_pct) && mx_pct > 1) {
  DF <- DF |> mutate(pct_extranjeros_poblacion = pct_extranjeros_poblacion/100)
  msg("Normalizado 'pct_extranjeros_poblacion' a proporción [0,1].")
}

# Rango rápido informativo
rng_hc  <- range(DF$tasa_hc_por_100k,  na.rm=TRUE)
rng_he  <- range(DF$tasa_he_por_100k,  na.rm=TRUE)
rng_dt  <- range(DF$tasa_det_por_100k, na.rm=TRUE)
rng_sh  <- range(DF$share_ext,                 na.rm=TRUE)
rng_pe  <- range(DF$pct_extranjeros_poblacion, na.rm=TRUE)
cat(sprintf(
  "\n[PEEK] Rangos:\n  HC/100k:[%.1f, %.1f]  HE/100k:[%.1f, %.1f]  DET/100k:[%.1f, %.1f]\n  share_ext:[%.3f, %.3f]  pct_extranjeros:[%.3f, %.3f]\n\n",
  rng_hc[1], rng_hc[2], rng_he[1], rng_he[2], rng_dt[1], rng_dt[2], rng_sh[1], rng_sh[2], rng_pe[1], rng_pe[2]
))

# ------------------ Variables a testear --------------------------------------
vars <- intersect(c(
  "tasa_hc_por_100k","tasa_he_por_100k","tasa_det_por_100k",
  "share_ext","pct_extranjeros_poblacion","pib_pc_real"
), names(DF))
if (!length(vars)) stop("Ninguna de las variables esperadas está en el archivo de entrada.", call. = FALSE)

# ------------------ Ventanas -----------------------------------------------
windows <- list(
  "con_covid_2010_2023" = function(d) d,
  "sin_covid_2010_2023" = function(d) d %>% filter(!(ano %in% c(2020L, 2021L))),
  "2010_2019_pre"       = function(d) d %>% filter(ano <= 2019L),
  "2022_2023_post"      = function(d) d %>% filter(ano >= 2022L) # se omite si N<8
)
min_n <- 8L

# ------------------ Tema/colores --------------------------------------------
PEARL <- "#FAF9F6"
COLS  <- c("Level-stationary"="#56B4E9", "Trend-stationary"="#F0E442",
           "Break-stationary"="#CC79A7", "Non-stationary"="#999999")
try({ src <- here::here("scripts","00_theme_figuras.R"); if (file.exists(src)) source(src, local = TRUE) }, silent = TRUE)
theme_used <- if (exists("theme_pearl_lightgrid")) theme_pearl_lightgrid else
  function(...) theme_minimal(base_size = 12) +
  theme(plot.background=element_rect(fill=PEARL, colour=NA),
        panel.background=element_rect(fill=PEARL, colour=NA),
        legend.background=element_rect(fill=PEARL, colour=NA))

# ------------------ Helpers de pruebas (robustos) ----------------------------

safe_num <- function(x) as.numeric(x)

# --- extractor de críticos con nombres flexibles (vector o matriz) -----------
.pick_cv <- function(obj_cval, keys) {
  out <- rep(NA_real_, length(keys)); names(out) <- keys
  if (is.null(obj_cval)) return(out)
  
  if (is.vector(obj_cval)) {
    nm <- names(obj_cval)
    for (k in keys) {
      j <- which(tolower(nm) == tolower(k))
      if (length(j)) out[k] <- as.numeric(obj_cval[j[1]])
    }
    return(out)
  }
  
  if (is.matrix(obj_cval) || is.data.frame(obj_cval)) {
    cn <- colnames(obj_cval); rn <- rownames(obj_cval)
    # filas típicas por teststat
    preferred_rows <- c("tau2","tau3","MZt")
    row_sel <- 1
    for (r in preferred_rows) if (r %in% rn) { row_sel <- which(rn == r)[1]; break }
    for (k in keys) {
      j <- which(tolower(cn) == tolower(k))
      if (length(j)) out[k] <- as.numeric(obj_cval[row_sel, j[1]])
    }
    return(out)
  }
  
  out
}

# ---------------- ADF con fallback: si NA, reintenta con lags=0 ---------------
.run_adf <- function(x, type = c("drift","trend"), lags = 1) {
  type <- match.arg(type)
  z <- try(urca::ur.df(x, type = type, lags = lags), silent = TRUE)
  if (inherits(z, "try-error")) return(list(stat = NA_real_, cval = NULL))
  row <- if (type == "drift") "tau2" else "tau3"
  stat <- suppressWarnings(as.numeric(z@teststat[row]))
  cval <- z@cval
  
  if (!is.finite(stat) && lags > 0) {
    z2 <- try(urca::ur.df(x, type = type, lags = 0), silent = TRUE)
    if (!inherits(z2, "try-error")) {
      stat <- suppressWarnings(as.numeric(z2@teststat[row]))
      cval <- z2@cval
    }
  }
  list(stat = stat, cval = cval)
}

run_adf_drift <- function(x){
  R <- .run_adf(x, "drift", lags = 1)
  cvs <- .pick_cv(R$cval, c("1pct","5pct","10pct"))
  tibble(
    test="ADF(drift,lag<=1,tau2)", stat = R$stat,
    cv_1pct=cvs["1pct"], cv_5pct=cvs["5pct"], cv_10pct=cvs["10pct"],
    reject_1pct = is.finite(stat) && is.finite(cv_1pct)  && stat < cv_1pct,
    reject_5pct = is.finite(stat) && is.finite(cv_5pct)  && stat < cv_5pct,
    reject_10pct= is.finite(stat) && is.finite(cv_10pct) && stat < cv_10pct
  )
}

run_adf_trend <- function(x){
  R <- .run_adf(x, "trend", lags = 1)
  cvs <- .pick_cv(R$cval, c("1pct","5pct","10pct"))
  tibble(
    test="ADF(trend,lag<=1,tau3)", stat = R$stat,
    cv_1pct=cvs["1pct"], cv_5pct=cvs["5pct"], cv_10pct=cvs["10pct"],
    reject_1pct = is.finite(stat) && is.finite(cv_1pct)  && stat < cv_1pct,
    reject_5pct = is.finite(stat) && is.finite(cv_5pct)  && stat < cv_5pct,
    reject_10pct= is.finite(stat) && is.finite(cv_10pct) && stat < cv_10pct
  )
}

# ---------------------- PP con cvals y rejects correctos ----------------------
run_pp_const <- function(x){
  z <- try(urca::ur.pp(x, type = "Z-tau", model = "constant", lags = "long"), silent = TRUE)
  if (inherits(z,"try-error")) return(NULL)
  stat <- suppressWarnings(as.numeric(z@teststat))
  cvs  <- .pick_cv(z@cval, c("1pct","5pct","10pct"))
  tibble(
    test="PP(Z-tau,const,long)", stat,
    cv_1pct=cvs["1pct"], cv_5pct=cvs["5pct"], cv_10pct=cvs["10pct"],
    reject_1pct = is.finite(stat) && is.finite(cv_1pct)  && stat < cv_1pct,
    reject_5pct = is.finite(stat) && is.finite(cv_5pct)  && stat < cv_5pct,
    reject_10pct= is.finite(stat) && is.finite(cv_10pct) && stat < cv_10pct
  )
}

run_pp_trend <- function(x){
  z <- try(urca::ur.pp(x, type = "Z-tau", model = "trend", lags = "long"), silent = TRUE)
  if (inherits(z,"try-error")) return(NULL)
  stat <- suppressWarnings(as.numeric(z@teststat))
  cvs  <- .pick_cv(z@cval, c("1pct","5pct","10pct"))
  tibble(
    test="PP(Z-tau,trend,long)", stat,
    cv_1pct=cvs["1pct"], cv_5pct=cvs["5pct"], cv_10pct=cvs["10pct"],
    reject_1pct = is.finite(stat) && is.finite(cv_1pct)  && stat < cv_1pct,
    reject_5pct = is.finite(stat) && is.finite(cv_5pct)  && stat < cv_5pct,
    reject_10pct= is.finite(stat) && is.finite(cv_10pct) && stat < cv_10pct
  )
}

# ---------------------- KPSS con cvals y rejects correctos --------------------
run_kpss_mu <- function(x){
  z <- try(urca::ur.kpss(x, type = "mu"), silent = TRUE)
  if (inherits(z,"try-error")) return(NULL)
  stat <- suppressWarnings(as.numeric(z@teststat))
  cvs  <- .pick_cv(z@cval, c("1pct","5pct","10pct"))
  tibble(
    test="KPSS(mu: level)", stat,
    cv_1pct=cvs["1pct"], cv_5pct=cvs["5pct"], cv_10pct=cvs["10pct"],
    reject_1pct = is.finite(stat) && is.finite(cv_1pct)  && stat > cv_1pct,
    reject_5pct = is.finite(stat) && is.finite(cv_5pct)  && stat > cv_5pct,
    reject_10pct= is.finite(stat) && is.finite(cv_10pct) && stat > cv_10pct
  )
}

run_kpss_tau <- function(x){
  z <- try(urca::ur.kpss(x, type = "tau"), silent = TRUE)
  if (inherits(z,"try-error")) return(NULL)
  stat <- suppressWarnings(as.numeric(z@teststat))
  cvs  <- .pick_cv(z@cval, c("1pct","5pct","10pct"))
  tibble(
    test="KPSS(tau: trend)", stat,
    cv_1pct=cvs["1pct"], cv_5pct=cvs["5pct"], cv_10pct=cvs["10pct"],
    reject_1pct = is.finite(stat) && is.finite(cv_1pct)  && stat > cv_1pct,
    reject_5pct = is.finite(stat) && is.finite(cv_5pct)  && stat > cv_5pct,
    reject_10pct= is.finite(stat) && is.finite(cv_10pct) && stat > cv_10pct
  )
}

# --------------------------- ERS / Ng–Perron / ZA ----------------------------
run_ers_const <- function(x){
  z <- try(urca::ur.ers(x, type="DF-GLS", model="constant", lag.max=2), silent=TRUE)
  if (inherits(z,"try-error")) return(NULL)
  stat <- suppressWarnings(as.numeric(z@teststat))
  tibble(test="ERS(DF-GLS,const)", stat,
         cv_1pct=NA_real_, cv_5pct=NA_real_, cv_10pct=NA_real_,
         reject_1pct=NA, reject_5pct=NA, reject_10pct=NA)
}
run_ers_trend <- function(x){
  z <- try(urca::ur.ers(x, type="DF-GLS", model="trend", lag.max=2), silent=TRUE)
  if (inherits(z,"try-error")) return(NULL)
  stat <- suppressWarnings(as.numeric(z@teststat))
  tibble(test="ERS(DF-GLS,trend)", stat,
         cv_1pct=NA_real_, cv_5pct=NA_real_, cv_10pct=NA_real_,
         reject_1pct=NA, reject_5pct=NA, reject_10pct=NA)
}

run_ngp_const <- function(x){
  z <- try(urca::ur.ngp(x, model="constant"), silent=TRUE); if (inherits(z,"try-error")) return(NULL)
  stat <- suppressWarnings(as.numeric(z@teststat["MZt"]))
  cvs  <- .pick_cv(z@cval, c("1pct","5pct","10pct"))
  tibble(test="NGP(MZt,const)", stat,
         cv_1pct=cvs["1pct"], cv_5pct=cvs["5pct"], cv_10pct=cvs["10pct"],
         reject_1pct = is.finite(stat) && is.finite(cv_1pct)  && stat < cv_1pct,
         reject_5pct = is.finite(stat) && is.finite(cv_5pct)  && stat < cv_5pct,
         reject_10pct= is.finite(stat) && is.finite(cv_10pct) && stat < cv_10pct)
}
run_ngp_trend <- function(x){
  z <- try(urca::ur.ngp(x, model="trend"), silent=TRUE); if (inherits(z,"try-error")) return(NULL)
  stat <- suppressWarnings(as.numeric(z@teststat["MZt"]))
  cvs  <- .pick_cv(z@cval, c("1pct","5pct","10pct"))
  tibble(test="NGP(MZt,trend)", stat,
         cv_1pct=cvs["1pct"], cv_5pct=cvs["5pct"], cv_10pct=cvs["10pct"],
         reject_1pct = is.finite(stat) && is.finite(cv_1pct)  && stat < cv_1pct,
         reject_5pct = is.finite(stat) && is.finite(cv_5pct)  && stat < cv_5pct,
         reject_10pct= is.finite(stat) && is.finite(cv_10pct) && stat < cv_10pct)
}

run_za <- function(x, years = NULL){
  z <- try(urca::ur.za(x, model="both", lag=1), silent=TRUE); if (inherits(z,"try-error")) return(NULL)
  stat <- suppressWarnings(as.numeric(z@teststat[1]))
  cv5 <- NA_real_; cv10 <- NA_real_
  if ("cval" %in% methods::slotNames(z)) {
    C <- z@cval
    c5n <- intersect(colnames(C), c("5pct","5%")); c10n <- intersect(colnames(C), c("10pct","10%"))
    if (length(c5n))  cv5  <- suppressWarnings(as.numeric(C[1, c5n[1]]))
    if (length(c10n)) cv10 <- suppressWarnings(as.numeric(C[1, c10n[1]]))
  }
  break_idx <- NA_integer_
  for (cand in c("b.point","bp","bpoints","breakpoint")) {
    if (cand %in% methods::slotNames(z)) {
      val <- tryCatch(methods::slot(z, cand), error=function(e) NULL)
      if (is.numeric(val) && length(val)>=1) { break_idx <- as.integer(val[1]); break }
    }
  }
  break_year <- if (!is.null(years) && is.finite(break_idx) &&
                    break_idx>=1 && break_idx<=length(years)) years[break_idx] else NA_integer_
  tibble(test="ZA(break:both)", stat = stat,
         cv_5pct = cv5, cv_10pct = cv10,
         reject_5pct = is.finite(stat)&&is.finite(cv5)&&(stat < cv5),
         reject_10pct= is.finite(stat)&&is.finite(cv10)&&(stat < cv10),
         za_break_index = break_idx, za_break_year = break_year)
}


# ------------------ Loop principal -------------------------------------------
detailed_rows <- list()
summary_rows  <- list()

for (w in names(windows)) {
  dwin <- windows[[w]](DF) %>% arrange(ano)
  if (!"ano" %in% names(dwin)) next
  nobs <- nrow(dwin %>% filter(is.finite(ano)))
  if (nobs < min_n && w == "2022_2023_post") { msg("Ventana ", w, " omitida (N<", min_n, ")."); next }
  
  for (v in vars) {
    # ---- Niveles ----
    x0 <- dwin[[v]] |> safe_num()
    idx <- which(is.finite(x0)); x <- x0[idx]; years <- dwin$ano[idx]
    if (length(x) >= min_n) {
      parts <- list(
        run_adf_drift(x), run_adf_trend(x),
        run_pp_const(x),  run_pp_trend(x),
        run_kpss_mu(x),   run_kpss_tau(x),
        run_ers_const(x), run_ers_trend(x),
        run_ngp_const(x), run_ngp_trend(x),
        run_za(x, years)
      )
      det <- bind_rows(parts[!sapply(parts, is.null)])
      if (nrow(det)) {
        det <- det %>% mutate(window = w, var = v, mode = "level", n_obs = length(x), .before = 1)
        detailed_rows[[length(detailed_rows)+1]] <- det
        res5  <- classify_stationarity_relaxed(det, "5pct")
        res10 <- classify_stationarity_relaxed(det, "10pct")
        summary_rows[[length(summary_rows)+1]] <- tibble(
          window = w, var = v, mode = "level", n_obs = length(x),
          classification_5pct = res5$class,  classification_10pct = res10$class,
          support_level_5 = res5$support_level, support_trend_5 = res5$support_trend,
          support_level_10= res10$support_level, support_trend_10= res10$support_trend,
          za_reject_5 = res5$za_reject, za_reject_10 = res10$za_reject,
          za_break_year = res5$za_break_year
        )
      }
    }
    # ---- Primera diferencia Δx ----
    if (length(x) >= (min_n + 1)) {
      dx <- diff(x)
      parts_d <- list(
        run_adf_drift(dx), run_adf_trend(dx),
        run_pp_const(dx),  run_pp_trend(dx),
        run_kpss_mu(dx),   run_kpss_tau(dx),
        run_ers_const(dx), run_ers_trend(dx),
        run_ngp_const(dx), run_ngp_trend(dx)
      )
      det_d <- bind_rows(parts_d[!sapply(parts_d, is.null)])
      if (nrow(det_d)) {
        det_d <- det_d %>% mutate(window = w, var = v, mode = "diff", n_obs = length(dx), .before = 1)
        detailed_rows[[length(detailed_rows)+1]] <- det_d
        res5d  <- classify_stationarity_relaxed(det_d, "5pct")
        res10d <- classify_stationarity_relaxed(det_d, "10pct")
        summary_rows[[length(summary_rows)+1]] <- tibble(
          window = w, var = v, mode = "diff", n_obs = length(dx),
          classification_5pct = res5d$class,  classification_10pct = res10d$class,
          support_level_5 = res5d$support_level, support_trend_5 = res5d$support_trend,
          support_level_10= res10d$support_level, support_trend_10= res10d$support_trend,
          za_reject_5 = FALSE, za_reject_10 = FALSE,
          za_break_year = NA_integer_
        )
      }
    }
  }
}

# ------------------ Salidas --------------------------------------------------
if (length(detailed_rows)) {
  out_det <- bind_rows(detailed_rows) %>% relocate(window, var, mode, test)
  write_csv(out_det, here::here("output/tables","18_stationarity.csv"))
  msg("Escrito: output/tables/18_stationarity.csv")
} else {
  msg("⚠️ No hubo suficientes observaciones para generar pruebas detalladas.")
}

if (length(summary_rows)) {
  out_sum <- bind_rows(summary_rows) %>% arrange(var, window, mode)
  write_csv(out_sum, here::here("output/tables","18_stationarity_summary.csv"))
  msg("Escrito: output/tables/18_stationarity_summary.csv")
  
  # Tabla ejecutiva (α=5%)
  tab_exec <- out_sum %>%
    mutate(classification_5pct = factor(classification_5pct,
                                        levels=c("Non-stationary","Level-stationary","Trend-stationary","Break-stationary"))) %>%
    group_by(window, mode) %>%
    summarise(
      n_series = n(),
      pct_stationary = mean(classification_5pct != "Non-stationary")*100,
      pct_level = mean(classification_5pct == "Level-stationary")*100,
      pct_trend = mean(classification_5pct == "Trend-stationary")*100,
      pct_break = mean(classification_5pct == "Break-stationary")*100,
      .groups = "drop"
    ) %>% arrange(window, mode)
  write_csv(tab_exec, here::here("output/tables","18_stationarity_summary_table.csv"))
  msg("Escrito: output/tables/18_stationarity_summary_table.csv")
  
  # Heatmap (α=5%)
  sum5 <- out_sum %>%
    transmute(window,
              var,
              mode = factor(mode, levels=c("diff","level"),
                            labels=c("Δ (primera diferencia)","Nivel")),
              classification = factor(classification_5pct,
                                      levels=c("Non-stationary","Level-stationary","Trend-stationary","Break-stationary")))
  lab_short <- c("Non-stationary"="NS","Level-stationary"="L",
                 "Trend-stationary"="T","Break-stationary"="B")
  win_labels <- c(
    "con_covid_2010_2023" = "2010–2023 (con COVID)",
    "sin_covid_2010_2023" = "2010–2023 (sin 2020–2021)",
    "2010_2019_pre"       = "2010–2019 (pre)",
    "2022_2023_post"      = "2022–2023 (post)"
  )
  sum5$window <- factor(sum5$window, levels = names(win_labels),
                        labels = win_labels[names(win_labels)])
  
  p <- ggplot(sum5, aes(x = window, y = var, fill = classification)) +
    geom_tile(color = "white") +
    geom_text(aes(label = lab_short[classification]), size = 4.2, fontface = "bold", color = "black") +
    scale_fill_manual(values = COLS, drop = FALSE) +
    facet_wrap(~ mode, nrow = 1) +
    theme_used() +
    theme(axis.title = element_blank(),
          axis.text.x = element_text(angle = 15, hjust = 1),
          legend.position = "right",
          strip.text = element_text(face = "bold")) +
    labs(title = "Estacionariedad por ventana y transformación",
         subtitle = "Clasificación relajada (α = 5%) con ADF/PP/ERS/Ng–Perron y KPSS; ZA marca rupturas",
         fill = "Clasificación (5%)",
         caption = "NS: Non-stationary · L: Level-stationary · T: Trend-stationary · B: Break-stationary (Zivot–Andrews)")
  ggsave(filename = here::here("output/figures","18_stationarity_heatmap.png"),
         plot = p, width = 14, height = 7, dpi = 300, bg = PEARL)
  ggsave(filename = here::here("output/figures","18_stationarity_heatmap.svg"),
         plot = p, width = 14, height = 7, dpi = 300, bg = PEARL)
  msg("Figuras escritas: output/figures/18_stationarity_heatmap.{png,svg}")
} else {
  msg("⚠️ No hubo suficientes observaciones para generar el resumen.")
}

# ---- Mensaje breve de interpretación ----
msg("Tip: si 'level' es Non/Break-stationary y 'diff' es L/T, trabaja en Δ (o dlog) y usa PIB_pc_real_yoy para ciclo.")

# === (NUEVO) Tabla ejecutiva y heatmap para α = 10% =========================
if (exists("out_sum")) {
  tab_exec_10 <- out_sum %>%
    mutate(classification_10pct = factor(
      classification_10pct,
      levels = c("Non-stationary","Level-stationary","Trend-stationary","Break-stationary")
    )) %>%
    group_by(window, mode) %>%
    summarise(
      n_series       = n(),
      pct_stationary = mean(classification_10pct != "Non-stationary")*100,
      pct_level      = mean(classification_10pct == "Level-stationary")*100,
      pct_trend      = mean(classification_10pct == "Trend-stationary")*100,
      pct_break      = mean(classification_10pct == "Break-stationary")*100,
      .groups = "drop"
    ) %>% arrange(window, mode)
  
  write_csv(tab_exec_10, here::here("output/tables","18_stationarity_summary_table_a10.csv"))
  msg("Escrito: output/tables/18_stationarity_summary_table_a10.csv")
  
  sum10 <- out_sum %>%
    transmute(window,
              var,
              mode = factor(mode, levels = c("diff","level"),
                            labels = c("Δ (primera diferencia)","Nivel")),
              classification = factor(classification_10pct,
                                      levels = c("Non-stationary","Level-stationary","Trend-stationary","Break-stationary")))
  lab_short <- c("Non-stationary"="NS","Level-stationary"="L",
                 "Trend-stationary"="T","Break-stationary"="B")
  win_labels <- c(
    "con_covid_2010_2023" = "2010–2023 (con COVID)",
    "sin_covid_2010_2023" = "2010–2023 (sin 2020–2021)",
    "2010_2019_pre"       = "2010–2019 (pre)",
    "2022_2023_post"      = "2022–2023 (post)"
  )
  sum10$window <- factor(sum10$window, levels = names(win_labels),
                         labels = win_labels[names(win_labels)])
  
  p10 <- ggplot(sum10, aes(x = window, y = var, fill = classification)) +
    geom_tile(color = "white") +
    geom_text(aes(label = lab_short[classification]),
              size = 4.2, fontface = "bold", color = "black") +
    scale_fill_manual(values = COLS, drop = FALSE) +
    facet_wrap(~ mode, nrow = 1) +
    theme_used() +
    theme(axis.title = element_blank(),
          axis.text.x = element_text(angle = 15, hjust = 1),
          legend.position = "right",
          strip.text = element_text(face = "bold")) +
    labs(
      title    = "Estacionariedad por ventana y transformación",
      subtitle = "Clasificación relajada (α = 10%) con ADF/PP/ERS/Ng–Perron y KPSS; ZA marca rupturas",
      fill     = "Clasificación (10%)",
      caption  = "NS: Non-stationary · L: Level-stationary · T: Trend-stationary · B: Break-stationary (Zivot–Andrews)"
    )
  ggsave(filename = here::here("output/figures","18_stationarity_heatmap_a10.png"),
         plot = p10, width = 14, height = 7, dpi = 300, bg = PEARL)
  ggsave(filename = here::here("output/figures","18_stationarity_heatmap_a10.svg"),
         plot = p10, width = 14, height = 7, dpi = 300, bg = PEARL)
  msg("Figuras escritas: output/figures/18_stationarity_heatmap_a10.{png,svg}")
}

# ------------------ Chequeo rápido de consola --------------------------------
qc18_stationarity_console <- function(){
  fp <- here::here("output/tables","18_stationarity_summary.csv")
  if (!file.exists(fp)) { cat("❌ Falta", fp, "\n"); return(invisible(FALSE)) }
  tb <- readr::read_csv(fp, show_col_types = FALSE)
  tb5 <- tb |>
    dplyr::select(window, var, mode, classification_5pct, za_break_year) |>
    dplyr::arrange(mode, window, var)
  cat("\n🧪 Stationarity (α=5%)\n")
  print(tb5, n = 100)
  invisible(TRUE)
}





# === Peek & alias seguro de columnas =========================================
DF <- readr::read_csv(infile, show_col_types = FALSE) |> janitor::clean_names()

cat("\n[PEEK] Columnas detectadas (", basename(infile), "):\n", sep = "")
print(names(DF))

# Muestra qué alias de cada variable encontró
alias_map <- list(
  ano                         = c("ano","year","anio","año"),
  tasa_hc_por_100k            = c("tasa_hc_por_100k","tasa_hc_100k"),
  tasa_he_por_100k            = c("tasa_he_por_100k","tasa_he_100k"),
  tasa_det_por_100k           = c("tasa_det_por_100k","tasa_det_100k"),
  share_ext                   = c("share_ext","share_extranjeros"),
  pct_extranjeros_poblacion   = c("pct_extranjeros_poblacion","pct_extranjeros","porc_extranjeros","porcentaje_extranjeros"),
  pib_pc_real                 = c("pib_pc_real")
)

pick_first <- function(nms, pool) {
  for (nm in nms) if (nm %in% pool) return(nm)
  NA_character_
}

found <- lapply(alias_map, pick_first, pool = names(DF))
cat("\n[PEEK] Alias elegidos:\n")
print(found)

# Helper: devuelve la primera columna disponible; si no hay, vector NA
safe_alias <- function(df, candidates, default = NA_real_) {
  nm <- pick_first(candidates, names(df))
  if (is.na(nm)) return(rep(default, nrow(df)))
  df[[nm]]
}

# Construye columnas canónicas SIN fallar si faltan en el archivo
DF <- DF |>
  dplyr::mutate(
    ano  = suppressWarnings(as.integer(safe_alias(DF, alias_map$ano, default = NA_integer_))),
    tasa_hc_por_100k  = safe_alias(DF, alias_map$tasa_hc_por_100k),
    tasa_he_por_100k  = safe_alias(DF, alias_map$tasa_he_por_100k),
    tasa_det_por_100k = safe_alias(DF, alias_map$tasa_det_por_100k),
    share_ext                 = safe_alias(DF, alias_map$share_ext),
    pct_extranjeros_poblacion = safe_alias(DF, alias_map$pct_extranjeros_poblacion),
    pib_pc_real               = safe_alias(DF, alias_map$pib_pc_real)
  )

# Aviso breve de lo que FALTA (si algo quedó todo NA)
needed <- c("ano","pib_pc_real")  # ajusta según el script
all_na <- function(v) all(is.na(v))
missing_soft <- names(DF)[names(DF) %in% names(alias_map)]
missing_soft <- missing_soft[vapply(DF[missing_soft], all_na, logical(1))]

if (length(missing_soft)) {
  cat("\n[PEEK] Aviso: columnas canónicas sin datos (todo NA) → ",
      paste(missing_soft, collapse = ", "), "\n", sep = "")
}

# En scripts críticos, puedes forzar stop si falta 'ano' u otra clave:
if (all_na(DF$ano)) stop("Falta columna de año ('ano'/'year'/'anio'). No puedo continuar.", call. = FALSE)

cat("[PEEK] Listo: columnas canónicas preparadas.\n\n")
# ============================================================================




qc18_stationarity_console()


# inspecciona las pruebas crudas para una variable/ventana concretas
inspect_var <- function(var = "pib_pc_real", window = "con_covid_2010_2023"){
  det <- readr::read_csv("output/tables/18_stationarity.csv", show_col_types = FALSE)
  det |> dplyr::filter(var == !!var, window == !!window) |>
    dplyr::arrange(mode, test) |>
    dplyr::select(window,var,mode,test,stat, dplyr::starts_with("cv_"), dplyr::starts_with("reject_"))
}
# ejemplos:
inspect_var("pib_pc_real", "con_covid_2010_2023")
inspect_var("tasa_hc_por_100k", "sin_covid_2010_2023")
