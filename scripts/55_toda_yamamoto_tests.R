#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
################################################################################
# 55_toda_yamamoto_tests.R — Tests de precedencia (Toda–Yamamoto) bivariados
# Proyecto : Delitos e Inmigración en España (2010–2023)
# Autor    : Jesús Castro · JESUSCASTRODATA
# Versión  : v1.3 (2025-10-27)
#
# Qué hace:
#   - PARES en NIVELES: (tasa_hc_100k ~ pct_extranjeros),
#                       (tasa_he_100k ~ pct_extranjeros),
#                       (share_extranjeros ~ pct_extranjeros)
#   - Estima VAR en niveles con k = p + d (TY), dummy COVID opcional (2020–2021).
#   - Selecciona p evaluando el MODELO TY: p mínimo con Portmanteau p>0.05;
#       si ninguno cumple, usa el p con mayor Portmanteau (mejor diagnóstico).
#   - Exporta resultados y diagnóstico en CSV (sin list-columns).
#
# Uso:
#   Rscript scripts/55_toda_yamamoto_tests.R
#   MAXLAGS=3 D=1 USE_COVID=1 Rscript scripts/55_toda_yamamoto_tests.R
#   Rscript scripts/55_toda_yamamoto_tests.R --IN="data/processed/core_indicadores_con_pib.csv" MAXLAGS=3 D=1
################################################################################

suppressPackageStartupMessages({
  library(here); library(fs); library(readr); library(dplyr); library(tidyr)
  library(tibble); library(purrr); library(janitor); library(stringr); library(vars)
})

# Evitar choques con otros paquetes
select    <- dplyr::select
filter    <- dplyr::filter
arrange   <- dplyr::arrange
slice     <- dplyr::slice
mutate    <- dplyr::mutate
transmute <- dplyr::transmute
rename    <- dplyr::rename

msg <- function(...) message("[55] ", paste0(...))
grab <- function(x, default = NA_real_) { if (is.null(x) || length(x)==0) return(default); x }

get_arg <- function(name, default = NULL) {
  ev <- Sys.getenv(name, unset = NA_character_)
  if (!is.na(ev) && nzchar(ev)) return(ev)
  vals <- grep(paste0("^--", name, "="), commandArgs(trailingOnly = TRUE), value = TRUE)
  if (length(vals)) return(sub(paste0("^--", name, "="), "", vals[1]))
  default
}

# Parámetros
MAXLAGS   <- as.integer(get_arg("MAXLAGS", "3"))   # hasta p=3 por defecto
D_ORDER   <- as.integer(get_arg("D", "1"))         # orden de integración asumido (TY)
USE_COVID <- as.integer(get_arg("USE_COVID", "1"))
OUT_DIR   <- here::here("output","tables"); fs::dir_create(OUT_DIR)

# Pares (NIVELES)
default_pairs <- tibble::tribble(
  ~v1,                ~v2,
  "tasa_hc_100k",     "pct_extranjeros",
  "tasa_he_100k",     "pct_extranjeros",
  "share_extranjeros","pct_extranjeros"
)

# --- Lectura robusta (punto o coma decimal) y limpieza de nombres
read_core <- function(path) {
  # 1) intento con punto decimal (US)
  suppressMessages({
    x <- try(readr::read_csv(path, show_col_types = FALSE,
                             locale = readr::locale(decimal_mark = ".", grouping_mark = ",")), silent = TRUE)
  })
  if (inherits(x, "try-error")) x <- NULL
  
  # 2) si falló o 'ano' no está correcto, intento con coma decimal (EU)
  need_retry <- is.null(x)
  if (!need_retry) {
    nm <- tolower(names(x))
    need_retry <- !("ano" %in% nm)
  }
  if (need_retry) {
    suppressMessages({
      x2 <- readr::read_csv(path, show_col_types = FALSE,
                            locale = readr::locale(decimal_mark = ",", grouping_mark = "."))
    })
    x <- x2
  }
  
  x <- janitor::clean_names(x)
  if (!"ano" %in% names(x)) stop("[55] Falta columna obligatoria 'ano' en: ", path)
  if (!is.integer(x$ano)) x$ano <- as.integer(x$ano)
  x
}

# Lectura con --IN y fallbacks
IN_ARG <- get_arg("IN", NULL)
cands <- c(
  if (!is.null(IN_ARG)) here::here(IN_ARG) else character(0),
  here::here("data","processed","core_indicadores_con_pib.csv"),
  here::here("data","processed","core_indicadores.csv")
)
checked <- unique(cands)
in_path <- checked[fs::file_exists(checked)][1]
if (is.na(in_path)) {
  msg("Busqué (no encontrados):"); for (p in checked) msg(" - ", p)
  stop("[55] No encontré fichero de entrada en niveles (core_*).")
}
msg("Leyendo: ", in_path)
dat0 <- read_core(in_path)

# Validación de columnas mínimas
needed_any <- unique(c(default_pairs$v1, default_pairs$v2))
present <- intersect(needed_any, names(dat0))
if (!all(needed_any %in% names(dat0))) {
  missing <- setdiff(needed_any, names(dat0))
  stop(paste0(
    "[55] Faltan columnas en el core: ", paste(missing, collapse = ", "), ".\n",
    "     Sugerencias:\n",
    "     - Ejecuta 18_merge_core_pib.R y 19_transformaciones.R para crear tasas y %.\n",
    "     - Verifica nombres: 'pct_extranjeros', 'share_extranjeros', 'tasa_hc_100k', 'tasa_he_100k'.\n"
  ))
}

# Filtrar pares disponibles según el dataset
pairs <- default_pairs %>% filter(v1 %in% names(dat0) & v2 %in% names(dat0)) %>% distinct()
if (nrow(pairs) == 0) stop("[55] Ninguno de los pares definidos existe en el dataset.")
msg("Pares (niveles) a evaluar: ", paste0(pairs$v1, " ~ ", pairs$v2, collapse = " | "))
msg("MAXLAGS=", MAXLAGS, " · D=", D_ORDER, " · USE_COVID=", USE_COVID)

# Helpers
is_stable <- function(fit) { all(Mod(vars::roots(fit)) < 1) }

portmanteau_p <- function(fit, lags_pt) {
  pt <- try(vars::serial.test(fit, lags.pt = lags_pt, type = "PT.asymptotic"), silent = TRUE)
  if (inherits(pt, "try-error")) return(NA_real_)
  suppressWarnings(as.numeric(pt$serial$p.value))
}

causality_dir <- function(fit, cause) {
  cs <- try(vars::causality(fit, cause = cause), silent = TRUE)
  if (inherits(cs, "try-error")) return(list(stat = NA_real_, p = NA_real_))
  g  <- grab(cs$Granger)
  list(
    stat = suppressWarnings(as.numeric(grab(g$statistic))),
    p    = suppressWarnings(as.numeric(grab(g$p.value)))
  )
}

# Núcleo: correr un par en niveles con TY (k = p + d), seleccionando p por diagnóstico del TY
run_pair_toda <- function(df, v1, v2, maxlags = 3, d = 1, use_covid = TRUE) {
  sub <- df %>%
    dplyr::select(ano, dplyr::all_of(c(v1, v2))) %>%
    dplyr::arrange(ano) %>%
    tidyr::drop_na(ano, dplyr::all_of(c(v1, v2)))
  
  n_obs <- nrow(sub)
  if (n_obs < (max(2, maxlags) + 3)) {
    return(list(
      result = tibble(
        pair = paste(v1, "~", v2), n = n_obs, lag_p = NA_integer_, d_order = d, k_total = NA_integer_,
        wald_v2_to_v1_stat = NA_real_, wald_v2_to_v1_p = NA_real_,
        wald_v1_to_v2_stat = NA_real_, wald_v1_to_v2_p = NA_real_,
        stable_roots = NA, port_p = NA_real_, exogen_covid = use_covid,
        start_year = min(sub$ano), end_year = max(sub$ano)
      ),
      diag = tibble(pair = paste(v1, "~", v2), p = NA_integer_, d = d,
                    k_total = NA_integer_, stable = NA, port_p = NA_real_,
                    roots_max_mod = NA_real_)
    ))
  }
  
  Y <- as.matrix(sub[, c(v1, v2)]); colnames(Y) <- c(v1, v2)
  X <- if (isTRUE(use_covid)) cbind(covid = as.integer(sub$ano %in% 2020:2021)) else NULL
  
  # Limitar p máximo “factible” según n (intento conservador)
  p_max_eff <- max(1L, min(as.integer(maxlags), n_obs - 4L - as.integer(d)))
  if (p_max_eff < 1L) p_max_eff <- 1L
  
  cand <- purrr::map_dfr(1:p_max_eff, function(pp) {
    k_tot <- pp + d
    fit_ty_pp <- try(vars::VAR(Y, p = k_tot, type = "const", exogen = X), silent = TRUE)
    if (inherits(fit_ty_pp, "try-error")) return(NULL)
    ptp_ty <- portmanteau_p(fit_ty_pp, lags_pt = max(4, pp + 1))
    st_ty  <- is_stable(fit_ty_pp)
    rmax   <- tryCatch(max(Mod(vars::roots(fit_ty_pp))), error = function(e) NA_real_)
    tibble(p = pp, k_total = k_tot, stable_ty = st_ty, port_ty = ptp_ty,
           roots_max_mod = rmax, fit = list(fit_ty_pp))
  })
  
  if (nrow(cand) == 0) {
    return(list(
      result = tibble(
        pair = paste(v1, "~", v2), n = n_obs, lag_p = 1L, d_order = d, k_total = 1L + d,
        wald_v2_to_v1_stat = NA_real_, wald_v2_to_v1_p = NA_real_,
        wald_v1_to_v2_stat = NA_real_, wald_v1_to_v2_p = NA_real_,
        stable_roots = NA, port_p = NA_real_, exogen_covid = use_covid,
        start_year = min(sub$ano), end_year = max(sub$ano)
      ),
      diag = tibble(pair = paste(v1, "~", v2), p = 1L, d = d, k_total = 1L + d,
                    stable = NA, port_p = NA_real_, roots_max_mod = NA_real_)
    ))
  }
  
  cand <- cand %>% arrange(p)
  
  # Regla: p mínimo con Port>0.05; si no hay, p con mayor Port
  if (any(cand$port_ty > 0.05, na.rm = TRUE)) {
    sel <- cand %>% filter(port_ty > 0.05) %>% slice(1)
  } else {
    sel <- cand %>% arrange(dplyr::desc(port_ty)) %>% slice(1)
    msg("Aviso: ningún TY Port>0.05 para ", v1, " ~ ", v2, " → uso p=", sel$p, " (mejor port).")
  }
  
  fit_ty  <- sel$fit[[1]]
  p_sel   <- sel$p[[1]]
  k_total <- sel$k_total[[1]]
  
  # Causalidad (Wald TY)
  g21 <- causality_dir(fit_ty, cause = v2)  # v2 -> v1
  g12 <- causality_dir(fit_ty, cause = v1)  # v1 -> v2
  
  st_ty  <- sel$stable_ty
  ptp_ty <- sel$port_ty
  
  result <- tibble(
    pair = paste(v1, "~", v2),
    n = n_obs,
    lag_p = as.integer(p_sel),
    d_order = as.integer(d),
    k_total = as.integer(k_total),
    wald_v2_to_v1_stat = g21$stat,
    wald_v2_to_v1_p    = g21$p,
    wald_v1_to_v2_stat = g12$stat,
    wald_v1_to_v2_p    = g12$p,
    stable_roots = st_ty,
    port_p = ptp_ty,
    exogen_covid = isTRUE(use_covid),
    start_year = min(sub$ano),
    end_year   = max(sub$ano)
  )
  
  diag <- tibble(
    pair = paste(v1, "~", v2),
    p = as.integer(p_sel),
    d = as.integer(d),
    k_total = as.integer(k_total),
    stable = st_ty,
    port_p = ptp_ty,
    roots_max_mod = sel$roots_max_mod
  )
  
  list(result = result, diag = diag)
}

# Ejecutar todos los pares
all_res <- list(); all_diag <- list()
for (i in seq_len(nrow(pairs))) {
  v1 <- pairs$v1[i]; v2 <- pairs$v2[i]
  msg("Corriendo TY para: ", v1, " ~ ", v2)
  out <- run_pair_toda(dat0, v1, v2, maxlags = MAXLAGS, d = D_ORDER, use_covid = as.logical(USE_COVID))
  all_res[[i]]  <- out$result
  all_diag[[i]] <- out$diag
}

res_tbl  <- bind_rows(all_res)  %>% arrange(pair)
diag_tbl <- bind_rows(all_diag) %>% arrange(pair, p)

# Escritura (sin list-columns)
p_res  <- file.path(OUT_DIR, "55_toda_results.csv")
p_diag <- file.path(OUT_DIR, "55_toda_diag.csv")
readr::write_csv(res_tbl,  p_res)
readr::write_csv(diag_tbl, p_diag)
msg("✔ TODA → ", p_res)
msg("✔ Diagnóstico → ", p_diag)

# Mini check consola
if (nrow(res_tbl)) {
  msg("—— MINI CHECK · 55_TY ————————————————")
  mini <- res_tbl[, c("pair","n","lag_p","d_order","k_total",
                      "wald_v2_to_v1_p","wald_v1_to_v2_p",
                      "stable_roots","port_p")]
  print(tibble::as_tibble(mini), n = nrow(mini))
}
invisible(TRUE)
