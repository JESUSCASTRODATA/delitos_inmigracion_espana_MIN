#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
###############################################################################
# 24_var_granger_bivar.R — VAR bivariado + Granger con selección por diagnóstico
# Proyecto : Delitos e Inmigración en España (2010–2023)
# Autor    : Jesús Castro · JESUSCASTRODATA
# Fecha    : 2025-10-06
#
# Qué hace:
#   - Construye VAR bivariados para pares (v1 ~ v2) y realiza Granger en ambas direcciones.
#   - Selecciona p ∈ {1..MAXLAGS} con regla:
#       (estable & Portmanteau p>0.05) con p mínimo; si no hay, estable con p mínimo;
#       si no hay, p=1 (fallback).
#   - Soporta dummy COVID (2020–2021) como exógena (USE_COVID=1).
#   - Entrada por --IN=... o fallbacks (var_ready y core_*).
#   - Exporta:
#       output/tables/24_granger_results.csv
#       output/tables/24_granger_diag.csv
#
# Uso:
#   Rscript scripts/24_var_granger_bivar.R
#   MAXLAGS=2 USE_COVID=1 Rscript scripts/24_var_granger_bivar.R
#   Rscript scripts/24_var_granger_bivar.R --IN="data/processed/core_indicadores_derivadas.csv" MAXLAGS=2
###############################################################################

suppressPackageStartupMessages({
  library(here); library(readr); library(dplyr); library(tidyr); library(tibble)
  library(purrr); library(janitor); library(stringr); library(vars)
})

# Evitar conflictos (p.ej., MASS::select)
select    <- dplyr::select
filter    <- dplyr::filter
arrange   <- dplyr::arrange
slice     <- dplyr::slice
mutate    <- dplyr::mutate
transmute <- dplyr::transmute
rename    <- dplyr::rename

# ---------- utilidades ----------
msg  <- function(...) message("[24] ", paste0(...))
grab <- function(x, default = NA_real_) { if (is.null(x) || length(x)==0) return(default); x }
get_arg <- function(name, default = NULL) {
  ev <- Sys.getenv(name, unset = NA_character_)
  if (!is.na(ev) && nzchar(ev)) return(ev)
  vals <- grep(paste0("^--", name, "="), commandArgs(trailingOnly = TRUE), value = TRUE)
  if (length(vals)) return(sub(paste0("^--", name, "="), "", vals[1]))
  default
}

# ---------- parámetros ----------
MAXLAGS   <- as.integer(get_arg("MAXLAGS", "3"))
USE_COVID <- as.integer(get_arg("USE_COVID", "1")) # 1=Sí, 0=No
OUT_DIR   <- here::here("output","tables")
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# Pares por defecto (se filtran a columnas disponibles)
default_pairs <- tibble::tribble(
  ~v1,               ~v2,
  "d_tasa_hc_100k",  "d_pct_extran",
  "d_tasa_he_100k",  "d_pct_extran",
  "d_share_ext",     "d_pct_extran"
)

# ---------- lectura de datos (permite --IN y fallbacks) ----------
IN_ARG <- get_arg("IN", NULL)
cands <- c(
  if (!is.null(IN_ARG)) here::here(IN_ARG) else character(0),
  here::here("data","processed","tasas","var_ready.csv"),
  here::here("data","processed","tasas","tasas_transformadas.csv"),
  here::here("data","processed","tasas","tasas_totales_anuales.csv"),
  here::here("data","processed","core_indicadores_derivadas.csv"),
  here::here("data","processed","core_indicadores_con_pib.csv"),
  here::here("data","processed","core_indicadores.csv")
)
checked <- unique(cands)
in_path <- checked[file.exists(checked)][1]
if (is.na(in_path)) {
  msg("Busqué (no encontrados):"); for (p in checked) msg(" - ", p)
  stop("[24] No encontré ningún fichero de entrada con 'ano'.")
}
msg("Leyendo: ", in_path)
dat0 <- suppressMessages(readr::read_csv(in_path, show_col_types = FALSE)) %>% janitor::clean_names()
if (!"ano" %in% names(dat0)) stop("[24] Falta columna 'ano' en el dataset.")

# Si viene de core_* y faltan d_tasa_* intenta crearlas desde tasas en niveles
maybe_add_deltas <- function(df, lvl, out) {
  if (all(c("ano", lvl) %in% names(df)) && !out %in% names(df)) {
    df <- df %>% arrange(ano) %>% mutate("{out}" := c(NA_real_, diff(.data[[lvl]])))
  }
  df
}
dat0 <- dat0 %>%
  maybe_add_deltas("tasa_hc_100k", "d_tasa_hc_100k") %>%
  maybe_add_deltas("tasa_he_100k", "d_tasa_he_100k")

# ---------- seleccionar pares disponibles ----------
pairs <- default_pairs %>%
  filter(v1 %in% names(dat0) & v2 %in% names(dat0)) %>%
  distinct()
if (nrow(pairs) == 0) stop("[24] Ninguno de los pares por defecto existe en las columnas del dataset.")
msg("Pares a evaluar: ", paste0(pairs$v1, " ~ ", pairs$v2, collapse = " | "))
msg("MAXLAGS=", MAXLAGS, " · USE_COVID=", USE_COVID)

# ---------- helpers de diagnóstico ----------
is_stable <- function(fit) all(Mod(vars::roots(fit)) < 1)
portmanteau_p <- function(fit, lags_pt) {
  pt <- try(vars::serial.test(fit, lags.pt = lags_pt, type = "PT.asymptotic"), silent = TRUE)
  if (inherits(pt, "try-error")) return(NA_real_)
  tryCatch(as.numeric(pt$serial$p.value), error = function(e) NA_real_)
}
causality_dir <- function(fit, cause) {
  cs <- try(vars::causality(fit, cause = cause), silent = TRUE)
  if (inherits(cs, "try-error")) return(list(stat = NA_real_, p = NA_real_))
  g  <- grab(cs$Granger)
  list(stat = suppressWarnings(as.numeric(grab(g$statistic))),
       p    = suppressWarnings(as.numeric(grab(g$p.value))))
}

# ---------- núcleo: correr un par v1 ~ v2 ----------
run_pair <- function(df, v1, v2, maxlags = 3, use_covid = TRUE) {
  sub <- df %>% dplyr::select(ano, dplyr::all_of(c(v1, v2))) %>% dplyr::arrange(ano) %>% tidyr::drop_na()
  n_obs <- nrow(sub)
  if (n_obs < (max(2, maxlags) + 3)) {
    return(list(
      result = tibble(pair = paste(v1, "~", v2),
                      n = n_obs, lag_p = NA_integer_,
                      granger_v2_to_v1_stat = NA_real_, granger_v2_to_v1_p = NA_real_,
                      granger_v1_to_v2_stat = NA_real_, granger_v1_to_v2_p = NA_real_,
                      stable_roots = NA, port_p = NA_real_,
                      exogen_covid = use_covid, start_year = min(sub$ano), end_year = max(sub$ano)),
      diag = tibble(pair = paste(v1, "~", v2),
                    p = NA_integer_, stable = NA, port_p = NA_real_,
                    roots_max_mod = NA_real_, roots_all = NA_character_)
    ))
  }
  
  Y <- as.matrix(sub[, c(v1, v2)]); colnames(Y) <- c(v1, v2)
  X <- if (isTRUE(use_covid)) cbind(covid = as.integer(sub$ano %in% 2020:2021)) else NULL
  
  cand <- purrr::map_dfr(1:maxlags, function(pp) {
    fit <- try(vars::VAR(Y, p = pp, type = "const", exogen = X), silent = TRUE)
    if (inherits(fit, "try-error")) return(NULL)
    st  <- is_stable(fit)
    ptp <- portmanteau_p(fit, lags_pt = max(4, pp + 1))
    rts <- try(Mod(vars::roots(fit)), silent = TRUE)
    rmax <- if (inherits(rts, "try-error")) NA_real_ else max(rts)
    tibble(p = pp, stable = st, port_p = ptp, fit = list(fit), roots_max_mod = rmax,
           roots_all = paste(round(rts, 4), collapse = "; "))
  })
  
  if (nrow(cand) == 0) {
    return(list(
      result = tibble(pair = paste(v1, "~", v2),
                      n = n_obs, lag_p = 1L,
                      granger_v2_to_v1_stat = NA_real_, granger_v2_to_v1_p = NA_real_,
                      granger_v1_to_v2_stat = NA_real_, granger_v1_to_v2_p = NA_real_,
                      stable_roots = NA, port_p = NA_real_,
                      exogen_covid = use_covid, start_year = min(sub$ano), end_year = max(sub$ano)),
      diag = tibble(pair = paste(v1, "~", v2),
                    p = 1L, stable = NA, port_p = NA_real_,
                    roots_max_mod = NA_real_, roots_all = NA_character_)
    ))
  }
  
  # Selección por diagnóstico (con fallback explícito p=1 si nada estable)
  cand <- cand %>% arrange(p)
  if (any(cand$stable & cand$port_p > 0.05, na.rm = TRUE)) {
    sel <- cand %>% filter(stable & port_p > 0.05) %>% slice(1)
  } else if (any(cand$stable, na.rm = TRUE)) {
    sel <- cand %>% filter(stable) %>% slice(1)
  } else {
    sel <- cand %>% slice(1)  # p mínimo (1)
    msg("Aviso: ningún p estable para ", v1, " ~ ", v2, " → uso p=1 (fallback).")
  }
  
  fit <- sel$fit[[1]]; p <- sel$p[[1]]
  
  g21 <- causality_dir(fit, cause = v2)  # v2 -> v1
  g12 <- causality_dir(fit, cause = v1)  # v1 -> v2
  
  res <- tibble(
    pair = paste(v1, "~", v2),
    n = n_obs,
    lag_p = as.integer(p),
    granger_v2_to_v1_stat = g21$stat,
    granger_v2_to_v1_p    = g21$p,
    granger_v1_to_v2_stat = g12$stat,
    granger_v1_to_v2_p    = g12$p,
    stable_roots = is_stable(fit),
    port_p = sel$port_p,
    exogen_covid = isTRUE(use_covid),
    start_year = min(sub$ano),
    end_year   = max(sub$ano)
  )
  
  diag <- cand %>%
    transmute(
      pair = paste(v1, "~", v2),
      p, stable, port_p,
      roots_max_mod,
      roots_all
    )
  
  list(result = res, diag = diag)
}

# ---------- Ejecutar todos los pares ----------
all_res <- list(); all_diag <- list()
for (i in seq_len(nrow(pairs))) {
  v1 <- pairs$v1[i]; v2 <- pairs$v2[i]
  msg("Corriendo VAR para: ", v1, " ~ ", v2)
  out <- run_pair(dat0, v1, v2, maxlags = MAXLAGS, use_covid = as.logical(USE_COVID))
  all_res[[i]]  <- out$result
  all_diag[[i]] <- out$diag
}

res_tbl  <- bind_rows(all_res)  %>% arrange(pair)
diag_tbl <- bind_rows(all_diag) %>% arrange(pair, p)

# Homogeneizar nombre Portmanteau
res_tbl <- res_tbl %>% rename(portmanteau_p = port_p)

# ---------- Escritura ----------
p_res  <- file.path(OUT_DIR, "24_granger_results.csv")
p_diag <- file.path(OUT_DIR, "24_granger_diag.csv")
readr::write_csv(res_tbl,  p_res)
readr::write_csv(diag_tbl, p_diag)
msg("✔ Granger → ", p_res)
msg("✔ Diagnóstico → ", p_diag)

# ---------- Mini-check consola ----------
if (nrow(res_tbl)) {
  msg("—— MINI CHECK · 24_granger ————————————————")
  mini <- res_tbl %>%
    dplyr::select(pair, n, lag_p, granger_v2_to_v1_stat, granger_v2_to_v1_p,
                  granger_v1_to_v2_stat, granger_v1_to_v2_p, stable_roots, portmanteau_p)
  print(mini, n = nrow(mini))
}
invisible(TRUE)



