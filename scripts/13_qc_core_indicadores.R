#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
###############################################################################
# 19_qc_core_indicadores.R — QC previo de core_indicadores_con_pib.csv
# Proyecto : Delitos e Inmigración en España (2010–2023)
# Autor    : Jesús Castro · JESUSCASTRODATA
# Fecha    : 2025-09-19
#
# Salidas (auto-etiquetadas con el número detectado del nombre de archivo):
#  - output/tables/19_qc_core_indicadores_resumen.csv
#  - output/tables/19_qc_core_indicadores_flags.csv
#  - output/tables/19_qc_core_indicadores_hash.csv
###############################################################################

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(tidyr); library(here)
  library(digest); library(glue); library(purrr)
})

# --- Detecta el número del script desde el nombre de archivo ------------------
.script_tag <- (function(){
  candidates <- c(commandArgs(trailingOnly = FALSE), sys.frames()[[1]]$ofile, sys.calls())
  candidates <- unlist(lapply(candidates, as.character), use.names = FALSE)
  nm <- NA_character_
  for (c in candidates) {
    m <- regmatches(c, regexpr("([0-9]{2,})_qc_core_indicadores\\.R$", c))
    if (length(m) && !is.na(m)) { nm <- m; break }
  }
  if (is.na(nm)) {
    files <- list.files(here::here("scripts"), pattern = "^[0-9]{2,}_qc_core_indicadores\\.R$", full.names = TRUE)
    if (length(files)) nm <- basename(files[1])
  }
  num <- NA_character_
  if (!is.na(nm)) num <- sub("_qc_core_indicadores\\.R$", "", basename(nm))
  if (is.na(num) || !grepl("^[0-9]{2,}$", num)) num <- "19"
  num
})()

TAG <- .script_tag
PREFIX <- paste0("[", TAG, "] ")
OUTBASE <- paste0(TAG, "_qc_core_indicadores")

msg <- function(...) message(PREFIX, paste0(...))
ensure_dir <- function(path) if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
ensure_dir(here::here("output/tables"))

# --- Entrada -----------------------------------------------------------------
infile <- here::here("data/processed","core_indicadores_con_pib.csv")
if (!file.exists(infile)) stop(PREFIX, "No existe data/processed/core_indicadores_con_pib.csv", call. = FALSE)
msg("Archivo: ", normalizePath(infile, winslash = "/"))

df <- readr::read_csv(infile, show_col_types = FALSE)

# === Normalización de nombres / escalas ======================================
# Renombrar a convención interna esperada por el QC
df <- df %>%
  rename(
    tasa_hc_por_100k            = any_of("tasa_hc_100k"),
    tasa_he_por_100k            = any_of("tasa_he_100k"),
    share_ext                   = any_of("share_extranjeros"),
    pct_extranjeros_poblacion   = any_of("pct_extranjeros")
  )

# Derivar tasa_det_por_100k si falta
if (!"tasa_det_por_100k" %in% names(df)) {
  if (all(c("det_tot","poblacion_total") %in% names(df))) {
    df <- df %>% mutate(tasa_det_por_100k = 1e5 * det_tot / pmax(1e-9, poblacion_total))
    msg("Derivada: tasa_det_por_100k creada desde det_tot y poblacion_total.")
  } else {
    stop(PREFIX, "Falta tasa_det_por_100k y no puedo derivarla (requiere det_tot y poblacion_total).", call. = FALSE)
  }
}

# Derivar share_ext si sigue faltando (de det_ext/det_tot)
if (!"share_ext" %in% names(df)) {
  if (all(c("det_ext","det_tot") %in% names(df))) {
    df <- df %>% mutate(share_ext = ifelse(det_tot > 0, det_ext / det_tot, NA_real_))
    msg("Derivada: share_ext creada como det_ext/det_tot.")
  } else {
    stop(PREFIX, "No existe share_ext ni (det_ext, det_tot) para derivarlo.", call. = FALSE)
  }
}

# Normalizar escalas a [0,1] si vienen en %
if (max(df$share_ext, na.rm = TRUE) > 1.0001) {
  df <- df %>% mutate(share_ext_pct = share_ext, share_ext = share_ext / 100)
  msg("Normalizado: share_ext parecía %; reescalado a [0,1] (share_ext_pct conserva el % original).")
}
if ("pct_extranjeros_poblacion" %in% names(df) && max(df$pct_extranjeros_poblacion, na.rm = TRUE) > 1.0001) {
  df <- df %>% mutate(pct_extranjeros_poblacion = pct_extranjeros_poblacion / 100)
  msg("Normalizado: pct_extranjeros_poblacion parecía 0–100; reescalado a 0–1.")
}

# Columnas esperadas (tras normalización)
expected_cols <- c(
  "ano","hc_total","he_total","det_tot","det_ext","poblacion_total",
  "pct_extranjeros_poblacion",
  "tasa_hc_por_100k","tasa_he_por_100k","tasa_det_por_100k",
  "share_ext",
  "pib_pc_real","pib_pc_real_yoy","pib_pc_real_idx2010","pib_pc_real_z"
)

# --- Contenedores de salida --------------------------------------------------
flags <- tibble(ano = integer(), variable = character(), valor = double(),
                tipo = character(), motivo = character(), check = character())
add_flag <- function(ano, variable, valor, tipo, motivo, check){
  new <- tibble(ano = as.integer(ano), variable, valor = as.numeric(valor),
                tipo, motivo, check)
  assign("flags", bind_rows(get("flags", envir = .GlobalEnv), new), envir = .GlobalEnv)
}

resumen <- tibble(check = character(), status = character(), detalle = character())
add_check <- function(check, status, detalle = ""){
  assign("resumen", bind_rows(get("resumen", envir = .GlobalEnv),
                              tibble(check, status, detalle)), envir = .GlobalEnv)
}

fail_critico <- FALSE

# 1) Esquema y cobertura ------------------------------------------------------
if (setequal(names(df), expected_cols) || setequal(names(df), c(expected_cols, "share_ext_pct"))) {
  add_check("Esquema columnas", "OK",
            if ("share_ext_pct" %in% names(df)) "Columna extra: share_ext_pct (permitida)" else "Columnas exactas")
  df <- df[, intersect(c(expected_cols, "share_ext_pct"), names(df))]
} else if (all(expected_cols %in% names(df))) {
  add_check("Esquema columnas", "WARN", "Columnas extra presentes (se ignoran)")
  df <- df[, expected_cols]
} else {
  missing <- setdiff(expected_cols, names(df))
  add_check("Esquema columnas", "FAIL", glue("Faltan: {paste(missing, collapse=', ')}"))
  fail_critico <- TRUE
}

if ("ano" %in% names(df)) {
  anos <- sort(unique(df$ano))
  dup  <- nrow(df) - length(anos)
  gap  <- !all(anos == seq(min(anos, na.rm=TRUE), max(anos, na.rm=TRUE)))
  if (dup > 0) { add_check("Duplicados de año", "FAIL", glue("{dup} duplicados")); fail_critico <- TRUE } else add_check("Duplicados de año", "OK")
  if (!setequal(anos, 2010:2023)) {
    add_check("Cobertura 2010–2023", "FAIL", glue("Años presentes: {min(anos)}–{max(anos)}; faltan o sobran"))
    fail_critico <- TRUE
  } else if (isTRUE(gap)) {
    add_check("Cobertura 2010–2023", "FAIL", "Huecos en la secuencia 2010–2023"); fail_critico <- TRUE
  } else add_check("Cobertura 2010–2023", "OK")
} else {
  add_check("Cobertura 2010–2023", "FAIL", "No existe columna ano"); fail_critico <- TRUE
}

# 2) Tipos/NA -----------------------------------------------------------------
if (!is.integer(df$ano)) {
  if (all(df$ano == floor(df$ano))) df$ano <- as.integer(df$ano)
}
if (!is.integer(df$ano)) { add_check("Tipo de ano", "FAIL", "ano no entero"); fail_critico <- TRUE } else add_check("Tipo de ano", "OK")

num_cols <- setdiff(names(df), "ano")
non_num <- num_cols[!vapply(df[num_cols], is.numeric, logical(1))]
if (length(non_num)) { add_check("Tipos numéricos", "FAIL", paste("No numéricas:", paste(non_num, collapse=", "))); fail_critico <- TRUE } else add_check("Tipos numéricos", "OK")

first_year <- min(df$ano, na.rm=TRUE)
na_bad <- df %>%
  pivot_longer(all_of(num_cols), names_to="variable", values_to="valor") %>%
  filter(is.na(valor),
         !(variable=="pib_pc_real_yoy" & ano==first_year))
if (nrow(na_bad) > 0) {
  add_check("NAs no permitidos", "FAIL", glue("{nrow(na_bad)} celdas NA no justificadas"))
  fail_critico <- TRUE
} else add_check("NAs no permitidos", "OK")

# 3) Identidades y rangos -----------------------------------------------------
tol_abs <- 1e-8
tol_rel <- 0.005

viol_he  <- df %>% filter(he_total > hc_total)
if (nrow(viol_he)) { add_check("HE ≤ HC", "FAIL", glue("{nrow(viol_he)} violaciones")); fail_critico <- TRUE } else add_check("HE ≤ HC", "OK")

viol_det <- df %>% filter(det_ext > det_tot)
if (nrow(viol_det)) { add_check("det_ext ≤ det_tot", "FAIL", glue("{nrow(viol_det)} violaciones")); fail_critico <- TRUE } else add_check("det_ext ≤ det_tot", "OK")

cmp_share <- df %>%
  mutate(ratio = det_ext / det_tot,
         diff  = abs(share_ext - ratio))
viol_share <- cmp_share %>% filter(!is.finite(ratio) | diff > tol_abs)
if (nrow(viol_share)) { add_check("share_ext = det_ext/det_tot", "FAIL", glue("{nrow(viol_share)} fuera de tolerancia")); fail_critico <- TRUE } else add_check("share_ext = det_ext/det_tot", "OK")

rng <- range(df$pct_extranjeros_poblacion, na.rm=TRUE)
in_0_1   <- is.finite(rng[1]) && is.finite(rng[2]) && rng[1] >= 0 && rng[2] <= 1
in_0_100 <- is.finite(rng[1]) && is.finite(rng[2]) && rng[1] >= 0 && rng[2] <= 100
if (!(in_0_1 || in_0_100)) {
  add_check("% extranjeros: rango", "FAIL", glue("Rango {round(rng[1],3)}–{round(rng[2],3)} inválido"))
  fail_critico <- TRUE
} else add_check("% extranjeros: rango", "OK", if (in_0_1) "escala [0,1]" else "escala [0,100]")

den_safe <- function(x) pmax(1e-9, x)
rates <- df %>%
  transmute(ano,
            tasa_hc_calc = 1e5 * hc_total / den_safe(poblacion_total),
            tasa_he_calc = 1e5 * he_total / den_safe(poblacion_total),
            tasa_det_calc= 1e5 * det_tot / den_safe(poblacion_total),
            tasa_hc_por_100k, tasa_he_por_100k, tasa_det_por_100k)

bad_rate <- bind_rows(
  rates %>% transmute(ano, variable="tasa_hc_por_100k",
                      rel = abs((tasa_hc_por_100k - tasa_hc_calc)/den_safe(tasa_hc_calc))),
  rates %>% transmute(ano, variable="tasa_he_por_100k",
                      rel = abs((tasa_he_por_100k - tasa_he_calc)/den_safe(tasa_he_calc))),
  rates %>% transmute(ano, variable="tasa_det_por_100k",
                      rel = abs((tasa_det_por_100k - tasa_det_calc)/den_safe(tasa_det_calc)))
) %>% filter(rel > tol_rel)

if (nrow(bad_rate)) { add_check("Tasas ≈ 100k*conteo/pob", "FAIL", glue("{nrow(bad_rate)} discrepancias > {100*tol_rel}%")); fail_critico <- TRUE } else add_check("Tasas ≈ 100k*conteo/pob", "OK")

idx2010 <- df %>% filter(ano == 2010) %>% pull(pib_pc_real_idx2010)
if (length(idx2010)==1 && is.finite(idx2010)) {
  if (abs(idx2010 - 100) <= 1) add_check("PIB idx2010≈100", "OK") else { add_check("PIB idx2010≈100", "FAIL", glue("Valor 2010={round(idx2010,2)}")); fail_critico <- TRUE }
} else { add_check("PIB idx2010≈100", "FAIL", "Sin dato 2010"); fail_critico <- TRUE }

yoy_bad <- df %>% filter(ano != first_year, !is.na(pib_pc_real_yoy) & abs(pib_pc_real_yoy) > 0.12)
if (nrow(yoy_bad)) purrr::walk(seq_len(nrow(yoy_bad)), ~{
  r <- yoy_bad[.x,]
  add_flag(r$ano, "pib_pc_real_yoy", r$pib_pc_real_yoy, "WARN", "YoY > 12%", "PIB_yoy NA/umbral")
})
add_check("PIB_yoy NA/umbral", if (nrow(yoy_bad)) "WARN" else "OK",
          if (nrow(yoy_bad)) glue("{nrow(yoy_bad)} años > 12%") else "")

stab <- function(x) all(is.finite(x)) && length(x) >= 2
pct_chg <- function(v) { v <- as.numeric(v); if (length(v) < 2) return(rep(NA_real_, length(v))); c(NA_real_, diff(v) / head(v, -1)) }

if (stab(df$det_tot))  { yo <- pct_chg(df$det_tot);  purrr::walk(which(abs(yo) > 0.25), ~ add_flag(df$ano[.x], "det_tot_yoy",  yo[.x], "WARN", "|YoY|>25%", "Estabilidad temporal")) }
if (stab(df$det_ext))  { yo <- pct_chg(df$det_ext);  purrr::walk(which(abs(yo) > 0.45), ~ add_flag(df$ano[.x], "det_ext_yoy",  yo[.x], "WARN", "|YoY|>45%", "Estabilidad temporal")) }
if (stab(df$hc_total)) { yo <- pct_chg(df$hc_total); purrr::walk(which(abs(yo) > 0.20), ~ add_flag(df$ano[.x], "hc_total_yoy", yo[.x], "WARN", "|YoY|>20%", "Estabilidad temporal")) }
if (stab(df$he_total)) { yo <- pct_chg(df$he_total); purrr::walk(which(abs(yo) > 0.20), ~ add_flag(df$ano[.x], "he_total_yoy", yo[.x], "WARN", "|YoY|>20%", "Estabilidad temporal")) }
if (stab(df$poblacion_total)) { yo <- pct_chg(df$poblacion_total); purrr::walk(which(abs(yo) > 0.03), ~ add_flag(df$ano[.x], "poblacion_total_yoy", yo[.x], "WARN", "|YoY|>3%", "Estabilidad temporal")) }

if (any(df$share_ext < 0 | df$share_ext > 0.6, na.rm=TRUE)) {
  idx <- which(df$share_ext < 0 | df$share_ext > 0.6)
  purrr::walk(idx, ~ add_flag(df$ano[.x], "share_ext", df$share_ext[.x], "WARN", "fuera de [0,0.6]", "Rango share_ext"))
}
if (stab(df$share_ext)) {
  j <- c(NA_real_, diff(df$share_ext))
  purrr::walk(which(abs(j) > 0.10), ~ add_flag(df$ano[.x], "share_ext_jump", j[.x], "WARN", "|Δ|>10 p.p.", "Saltos share_ext"))
}
if ("pct_extranjeros_poblacion" %in% names(df)) {
  pe <- df$pct_extranjeros_poblacion
  if (max(pe, na.rm=TRUE) > 1) pe <- pe / 100
  j <- c(NA_real_, diff(pe))
  purrr::walk(which(abs(j) > 0.10), ~ add_flag(df$ano[.x], "pct_extranjeros_jump", j[.x], "WARN", "|Δ|>10 p.p.", "Saltos % extranjeros"))
}

ratio <- df$he_total / df$hc_total
bad_ratio <- which(!is.finite(ratio) | ratio <= 0 | ratio > 1)
if (length(bad_ratio)) { add_check("he_total/hc_total in (0,1]", "FAIL", glue("{length(bad_ratio)} fuera de rango")); fail_critico <- TRUE } else add_check("he_total/hc_total in (0,1]", "OK")

if (all(is.finite(df$pct_extranjeros_poblacion)) && all(is.finite(df$share_ext))) {
  rho <- suppressWarnings(cor(df$pct_extranjeros_poblacion, df$share_ext, method = "spearman"))
  if (is.finite(rho) && rho < 0) add_check("Cor(share_ext, %extranjeros)", "WARN", glue("rho(Spearman)={round(rho,3)} < 0"))
  else add_check("Cor(share_ext, %extranjeros)", "OK", glue("rho={round(rho,3)}"))
} else add_check("Cor(share_ext, %extranjeros)", "WARN", "Valores no finitos")

if (all(is.finite(df$pib_pc_real_z))) {
  m <- mean(df$pib_pc_real_z); s <- sd(df$pib_pc_real_z)
  if (abs(m) <= 0.2 && s >= 0.8 && s <= 1.2) add_check("PIB_z centrado", "OK", glue("mean={round(m,2)}, sd={round(s,2)}"))
  else add_check("PIB_z centrado", "WARN", glue("mean={round(m,2)}, sd={round(s,2)} fuera de tolerancia"))
} else add_check("PIB_z centrado", "WARN", "Valores no finitos")

iqr_flags <- function(v, nm, chk){
  ok <- is.finite(v)
  if (sum(ok) < 4) return(invisible(NULL))
  Q1 <- quantile(v[ok], 0.25, names = FALSE)
  Q3 <- quantile(v[ok], 0.75, names = FALSE)
  I  <- Q3 - Q1
  lower <- Q1 - 1.5 * I; upper <- Q3 + 1.5 * I
  idx <- which(v < lower | v > upper)
  if (length(idx)) purrr::walk(idx, ~ add_flag(df$ano[.x], nm, v[.x], "WARN", "Outlier IQR", chk))
}
iqr_flags(df$tasa_hc_por_100k, "tasa_hc_por_100k", "Outliers IQR")
iqr_flags(df$tasa_he_por_100k, "tasa_he_por_100k", "Outliers IQR")
iqr_flags(df$tasa_det_por_100k,"tasa_det_por_100k","Outliers IQR")
iqr_flags(df$share_ext,         "share_ext",       "Outliers IQR")

# --- Salidas (con prefijo dinámico) ------------------------------------------
readr::write_csv(resumen, here::here("output/tables", paste0(OUTBASE, "_resumen.csv")))
msg("Escrito: output/tables/", paste0(OUTBASE, "_resumen.csv"))

if (nrow(flags) == 0) flags <- tibble(ano=integer(), variable=character(), valor=double(), tipo=character(), motivo=character(), check=character())
readr::write_csv(flags,   here::here("output/tables", paste0(OUTBASE, "_flags.csv")))
msg("Escrito: output/tables/", paste0(OUTBASE, "_flags.csv"))

hash <- digest::digest(infile, algo = "sha256", file = TRUE)
bytes <- file.info(infile)$size
info  <- tibble(path = "data/processed/core_indicadores_con_pib.csv",
                sha256 = hash, bytes = bytes, generated_utc = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
readr::write_csv(info,    here::here("output/tables", paste0(OUTBASE, "_hash.csv")))
msg("Escrito: output/tables/", paste0(OUTBASE, "_hash.csv"))

# --- Salida proceso -----------------------------------------------------------
if (fail_critico) {
  msg("QC CRÍTICO: hubo FAIL en checks esenciales. Revisa '", OUTBASE, "_resumen.csv' y '", OUTBASE, "_flags.csv'.")
  if (interactive()) {
    stop(PREFIX, "QC CRÍTICO: corrige antes de continuar.", call. = FALSE)
  } else {
    quit(status = 1, save = "no")
  }
} else {
  msg("QC OK: sin FAIL críticos. Puedes continuar con scripts/18_stationarity_checks.R")
}



