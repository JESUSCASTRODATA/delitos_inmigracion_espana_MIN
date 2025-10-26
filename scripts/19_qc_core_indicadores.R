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
  library(digest); library(glue); library(purrr); library(janitor)
})

# --- Detecta número del script (para prefijo en salidas) ---------------------
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
  num <- if (!is.na(nm)) sub("_qc_core_indicadores\\.R$", "", basename(nm)) else NA_character_
  if (is.na(num) || !grepl("^[0-9]{2,}$", num)) num <- "19"
  num
})()

TAG <- .script_tag
PREFIX <- paste0("[", TAG, "] ")
OUTBASE <- paste0(TAG, "_qc_core_indicadores")
msg <- function(...) message(PREFIX, paste0(...))
ensure_dir <- function(p) if (!dir.exists(p)) dir.create(p, recursive = TRUE, showWarnings = FALSE)
ensure_dir(here::here("output/tables"))

# --- Entrada -----------------------------------------------------------------
infile <- here::here("data/processed","core_indicadores_con_pib.csv")
if (!file.exists(infile)) stop(PREFIX, "No existe data/processed/core_indicadores_con_pib.csv", call. = FALSE)
msg("Archivo: ", normalizePath(infile, winslash = "/"))

raw <- readr::read_csv(infile, show_col_types = FALSE) |> janitor::clean_names()

# --- Aliases → columnas canónicas para el QC ---------------------------------
df <- raw

# ano entero
if (!"ano" %in% names(df)) stop(PREFIX, "Falta columna 'ano' en el CSV.", call. = FALSE)
df <- df |> mutate(ano = suppressWarnings(as.integer(ano)))

# % extranjeros: usa pct_extranjeros (tu columna) → canónico pct_extranjeros_poblacion en [0,1]
pct_ext_alias <- intersect(c("pct_extranjeros_poblacion","pct_extranjeros","porc_extranjeros","porcentaje_extranjeros"), names(df))[1]
if (!is.na(pct_ext_alias)) {
  v <- df[[pct_ext_alias]]
  # normaliza: si max>1 asumimos que venía en %
  v_norm <- if (suppressWarnings(max(v, na.rm = TRUE)) > 1) v/100 else v
  df$pct_extranjeros_poblacion <- as.numeric(v_norm)
} else {
  df$pct_extranjeros_poblacion <- NA_real_
  msg("WARN: No se encontró % extranjeros; se deja NA (algunos checks se degradarán).")
}

# share_ext: prioridad share_extranjeros; si no existe, det_ext/det_tot
if ("share_ext" %in% names(df)) {
  se <- df$share_ext
} else if ("share_extranjeros" %in% names(df)) {
  se <- df$share_extranjeros
} else if (all(c("det_ext","det_tot") %in% names(df))) {
  se <- ifelse(df$det_tot > 0, df$det_ext/df$det_tot, NA_real_)
  msg("INFO: share_ext derivado como det_ext/det_tot (no existía share_ext/share_extranjeros).")
} else {
  se <- NA_real_
  msg("WARN: No hay forma de obtener share_ext (faltan share_extranjeros y/o det_ext/det_tot).")
}
# normaliza share a [0,1] si venía en %
if (suppressWarnings(max(se, na.rm = TRUE)) > 1) {
  se <- se/100
  msg("INFO: share_ext detectado en % → normalizado a proporción [0,1].")
}
df$share_ext <- as.numeric(se)

# Tasas por 100k: acepta tasa_*_por_100k o tasa_*_100k; deriva det si falta
pick_rate <- function(df, base){
  cand <- intersect(paste0(base, c("_por_100k","_100k")), names(df))
  if (length(cand)) as.numeric(df[[cand[1]]]) else NA_real_
}
df$tasa_hc_por_100k <- pick_rate(df, "tasa_hc")
df$tasa_he_por_100k <- pick_rate(df, "tasa_he")
# derivar si faltan usando conteo/población
den_safe <- function(x) pmax(1e-9, x)
if (all(is.na(df$tasa_hc_por_100k)) && all(c("hc_total","poblacion_total") %in% names(df))) {
  df$tasa_hc_por_100k <- 1e5 * df$hc_total / den_safe(df$poblacion_total)
  msg("INFO: Derivada tasa_hc_por_100k desde hc_total/poblacion_total.")
}
if (all(is.na(df$tasa_he_por_100k)) && all(c("he_total","poblacion_total") %in% names(df))) {
  df$tasa_he_por_100k <- 1e5 * df$he_total / den_safe(df$poblacion_total)
  msg("INFO: Derivada tasa_he_por_100k desde he_total/poblacion_total.")
}
# tasa_det: no la traes → la derivamos si es posible
if (!("tasa_det_por_100k" %in% names(df))) {
  if (all(c("det_tot","poblacion_total") %in% names(df))) {
    df$tasa_det_por_100k <- 1e5 * df$det_tot / den_safe(df$poblacion_total)
    msg("INFO: Derivada tasa_det_por_100k desde det_tot/poblacion_total.")
  } else {
    df$tasa_det_por_100k <- NA_real_
    msg("WARN: No se pudo derivar tasa_det_por_100k (faltan det_tot/poblacion_total).")
  }
}

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

# --- 1) Esquema/cobertura mínimo --------------------------------------------
need_min <- c("ano","hc_total","he_total","det_tot","det_ext","poblacion_total",
              "pct_extranjeros_poblacion","share_ext",
              "tasa_hc_por_100k","tasa_he_por_100k","tasa_det_por_100k",
              "pib_pc_real","pib_pc_real_yoy","pib_pc_real_idx2010","pib_pc_real_z")
missing <- setdiff(need_min, names(df))
if (length(missing)) {
  add_check("Esquema (mínimo requerido)", "FAIL", glue("Faltan: {paste(missing, collapse=', ')}"))
  fail_critico <- TRUE
} else add_check("Esquema (mínimo requerido)", "OK")

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

# --- 2) Tipos/NA -------------------------------------------------------------
if (!is.integer(df$ano)) {
  if (all(df$ano == floor(df$ano))) df$ano <- as.integer(df$ano)
}
if (!is.integer(df$ano)) { add_check("Tipo de ano", "FAIL", "ano no entero"); fail_critico <- TRUE } else add_check("Tipo de ano", "OK")

num_cols <- setdiff(names(df), "ano")
non_num <- num_cols[!vapply(df[num_cols], is.numeric, logical(1))]
if (length(non_num)) { add_check("Tipos numéricos", "FAIL", paste("No numéricas:", paste(non_num, collapse=", "))); fail_critico <- TRUE } else add_check("Tipos numéricos", "OK")

first_year <- suppressWarnings(min(df$ano, na.rm=TRUE))
na_bad <- df %>%
  pivot_longer(all_of(num_cols), names_to="variable", values_to="valor") %>%
  filter(is.na(valor),
         !(variable=="pib_pc_real_yoy" & ano==first_year))
if (nrow(na_bad) > 0) {
  add_check("NAs no permitidos", "FAIL", glue("{nrow(na_bad)} celdas NA no justificadas"))
  fail_critico <- TRUE
} else add_check("NAs no permitidos", "OK")

# --- 3) Identidades y rangos -------------------------------------------------
tol_abs <- 1e-8
tol_rel <- 0.005

# HE ≤ HC
viol_he  <- df %>% filter(he_total > hc_total)
if (nrow(viol_he)) { add_check("HE ≤ HC", "FAIL", glue("{nrow(viol_he)} violaciones")); fail_critico <- TRUE } else add_check("HE ≤ HC", "OK")

# det_ext ≤ det_tot
viol_det <- df %>% filter(det_ext > det_tot)
if (nrow(viol_det)) { add_check("det_ext ≤ det_tot", "FAIL", glue("{nrow(viol_det)} violaciones")); fail_critico <- TRUE } else add_check("det_ext ≤ det_tot", "OK")

# share_ext ≈ det_ext/det_tot (si hay ambos)
if (all(c("det_ext","det_tot") %in% names(df))) {
  cmp_share <- df %>% mutate(ratio = det_ext / det_tot,
                             diff  = abs(share_ext - ratio))
  viol_share <- cmp_share %>% filter(!is.finite(ratio) | diff > tol_abs)
  if (nrow(viol_share)) { add_check("share_ext = det_ext/det_tot", "FAIL", glue("{nrow(viol_share)} fuera de tolerancia")) ; fail_critico <- TRUE
  } else add_check("share_ext = det_ext/det_tot", "OK")
}

# % extranjeros rango válido
rng <- range(df$pct_extranjeros_poblacion, na.rm=TRUE)
in_0_1   <- is.finite(rng[1]) && rng[1] >= 0 && is.finite(rng[2]) && rng[2] <= 1
in_0_100 <- is.finite(rng[1]) && rng[1] >= 0 && is.finite(rng[2]) && rng[2] <= 100
if (!(in_0_1 || in_0_100)) {
  add_check("% extranjeros: rango", "FAIL", glue("Rango {round(rng[1],3)}–{round(rng[2],3)} inválido")); fail_critico <- TRUE
} else add_check("% extranjeros: rango", "OK", if (in_0_1) "escala [0,1]" else "escala [0,100]")

# Tasas ≈ 100k * conteo / población
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
) %>% filter(rel > tol_rel & is.finite(rel))

if (nrow(bad_rate)) {
  add_check("Tasas ≈ 100k*conteo/pob", "FAIL", glue("{nrow(bad_rate)} discrepancias > {100*tol_rel}%"))
  fail_critico <- TRUE
} else add_check("Tasas ≈ 100k*conteo/pob", "OK")

# Índice 2010 ≈ 100 ±1
idx2010 <- df %>% filter(ano == 2010) %>% pull(pib_pc_real_idx2010)
if (length(idx2010)==1 && is.finite(idx2010)) {
  if (abs(idx2010 - 100) <= 1) add_check("PIB idx2010≈100", "OK") else {
    add_check("PIB idx2010≈100", "FAIL", glue("Valor 2010={round(idx2010,2)}")); fail_critico <- TRUE
  }
} else {
  add_check("PIB idx2010≈100", "FAIL", "Sin dato 2010"); fail_critico <- TRUE
}

# 4) Estabilidad temporal (banderas no bloqueantes) ---------------------------
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
if (stab(df$pct_extranjeros_poblacion)) {
  pe <- df$pct_extranjeros_poblacion
  if (max(pe, na.rm=TRUE) > 1) pe <- pe / 100
  j <- c(NA_real_, diff(pe))
  purrr::walk(which(abs(j) > 0.10), ~ add_flag(df$ano[.x], "pct_extranjeros_jump", j[.x], "WARN", "|Δ|>10 p.p.", "Saltos % extranjeros"))
}

# 5) Coherencias cruzadas -----------------------------------------------------
ratio <- df$he_total / df$hc_total
bad_ratio <- which(!is.finite(ratio) | ratio <= 0 | ratio > 1)
if (length(bad_ratio)) { add_check("he_total/hc_total in (0,1]", "FAIL", glue("{length(bad_ratio)} fuera de rango")); fail_critico <- TRUE } else add_check("he_total/hc_total in (0,1]", "OK")

pe_norm <- df$pct_extranjeros_poblacion
if (max(pe_norm, na.rm=TRUE) > 1) pe_norm <- pe_norm/100
if (all(is.finite(pe_norm)) && all(is.finite(df$share_ext))) {
  rho <- suppressWarnings(cor(pe_norm, df$share_ext, method = "spearman"))
  if (is.finite(rho) && rho < 0) add_check("Cor(share_ext, %extranjeros)", "WARN", glue("rho(Spearman)={round(rho,3)} < 0"))
  else add_check("Cor(share_ext, %extranjeros)", "OK", glue("rho={round(rho,3)}"))
} else add_check("Cor(share_ext, %extranjeros)", "WARN", "Valores no finitos")

# PIB_z centrado
if (all(is.finite(df$pib_pc_real_z))) {
  m <- mean(df$pib_pc_real_z); s <- sd(df$pib_pc_real_z)
  if (abs(m) <= 0.2 && s >= 0.8 && s <= 1.2) add_check("PIB_z centrado", "OK", glue("mean={round(m,2)}, sd={round(s,2)}"))
  else add_check("PIB_z centrado", "WARN", glue("mean={round(m,2)}, sd={round(s,2)} fuera de tolerancia"))
} else add_check("PIB_z centrado", "WARN", "Valores no finitos")

# --- 6) Outliers (IQR) -------------------------------------------------------
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

# --- Salidas -----------------------------------------------------------------
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
if (any(resumen$status == "FAIL")) {
  msg("QC CRÍTICO: hubo FAIL en checks esenciales. Revisa '", OUTBASE, "_resumen.csv' y '", OUTBASE, "_flags.csv'.")
  if (interactive()) {
    stop(PREFIX, "QC CRÍTICO: corrige antes de continuar.", call. = FALSE)
  } else {
    quit(status = 1, save = "no")
  }
} else {
  msg("QC OK: sin FAIL críticos. Puedes continuar con scripts/18_stationarity_checks.R")
}





suppressPackageStartupMessages({
  library(readr); library(dplyr); library(janitor); library(glue)
})

qc19_console <- function(
    fp = "data/processed/core_indicadores_con_pib.csv",
    year_min = 2010L, year_max = 2023L,
    show_flags = 5
){
  if (!file.exists(fp)) { cat("❌ Falta", fp, "\n"); return(invisible(FALSE)) }
  df <- readr::read_csv(fp, show_col_types = FALSE) |> clean_names()
  
  # ---- alias ----
  pct_alias   <- intersect(c("pct_extranjeros_poblacion","pct_extranjeros","porc_extranjeros","porcentaje_extranjeros"), names(df))[1]
  share_alias <- intersect(c("share_ext","share_extranjeros"), names(df))[1]
  tasa_hc_alias <- intersect(c("tasa_hc_por_100k","tasa_hc_100k"), names(df))[1]
  tasa_he_alias <- intersect(c("tasa_he_por_100k","tasa_he_100k"), names(df))[1]
  tasa_dt_alias <- intersect(c("tasa_det_por_100k","tasa_det_100k"), names(df))[1]
  
  # ---- % extranjeros y share en [0,1] ----
  pe <- if (!is.na(pct_alias)) df[[pct_alias]] else rep(NA_real_, nrow(df))
  if (suppressWarnings(max(pe, na.rm=TRUE)) > 1) pe <- pe/100
  se <- if (!is.na(share_alias)) df[[share_alias]] else {
    if (all(c("det_ext","det_tot") %in% names(df))) ifelse(df$det_tot>0, df$det_ext/df$det_tot, NA_real_) else NA_real_
  }
  if (suppressWarnings(max(se, na.rm=TRUE)) > 1) se <- se/100
  
  # ---- tasas ----
  den_safe <- function(x) pmax(1e-9, x)
  tasa_hc <- if (!is.na(tasa_hc_alias)) df[[tasa_hc_alias]] else if (all(c("hc_total","poblacion_total") %in% names(df))) 1e5*df$hc_total/den_safe(df$poblacion_total) else NA_real_
  tasa_he <- if (!is.na(tasa_he_alias)) df[[tasa_he_alias]] else if (all(c("he_total","poblacion_total") %in% names(df))) 1e5*df$he_total/den_safe(df$poblacion_total) else NA_real_
  tasa_dt <- if (!is.na(tasa_dt_alias)) df[[tasa_dt_alias]] else if (all(c("det_tot","poblacion_total") %in% names(df))) 1e5*df$det_tot/den_safe(df$poblacion_total) else NA_real_
  
  # ---- checks ----
  yrs   <- sort(unique(df$ano)); target <- seq.int(year_min, year_max); miss <- setdiff(target, yrs)
  he_le_hc <- sum(df$he_total > df$hc_total, na.rm=TRUE)
  de_le_dt <- sum(df$det_ext > df$det_tot, na.rm=TRUE)
  ok_share <- sum(abs(se - ifelse(df$det_tot>0, df$det_ext/df$det_tot, NA_real_)) > 1e-8, na.rm=TRUE)
  
  tasa_hc_calc <- 1e5*df$hc_total/den_safe(df$poblacion_total)
  tasa_he_calc <- 1e5*df$he_total/den_safe(df$poblacion_total)
  tasa_dt_calc <- 1e5*df$det_tot/den_safe(df$poblacion_total)
  rel <- function(x,y) abs((x-y)/den_safe(y))
  bad_rates <- sum(c(
    rel(tasa_hc, tasa_hc_calc) > 0.005,
    rel(tasa_he, tasa_he_calc) > 0.005,
    rel(tasa_dt, tasa_dt_calc) > 0.005
  ), na.rm=TRUE)
  
  rho <- suppressWarnings(cor(pe, se, method="spearman", use="complete.obs"))
  fr <- function(x, d=3) if (all(!is.finite(x))) "NA–NA" else sprintf("[%.0*f, %.0*f]", d, min(x, na.rm=TRUE), d, max(x, na.rm=TRUE))
  fr1 <- function(x) if (all(!is.finite(x))) "NA–NA" else sprintf("[%.1f, %.1f]", min(x, na.rm=TRUE), max(x, na.rm=TRUE))
  
  # ---- construir salida resumen (capturada) ----
  lines <- c(
    "\n🧪 QC19 — core_indicadores_con_pib.csv",
    glue("• Cobertura: {length(yrs)}/{length(target)} ({ifelse(length(yrs)>0,min(yrs),NA)}–{ifelse(length(yrs)>0,max(yrs),NA)})  faltan: {ifelse(length(miss)==0,'ninguno',paste(miss, collapse=', '))}"),
    glue("• HE≤HC violaciones: {he_le_hc} | det_ext≤det_tot violaciones: {de_le_dt}"),
    glue("• share ≈ det_ext/det_tot fuera de tolerancia: {ok_share}"),
    glue("• Tasas ≈ 100k*conteo/pob discrepancias (>0.5%): {bad_rates}"),
    glue("• Rango % extranjeros (proporción): {fr(pe,3)}"),
    glue("• Rango share_ext: {fr(se,3)}  |  rho(share, %ext)={ifelse(is.finite(rho), sprintf('%.3f', rho), 'NA')}"),
    glue("• Rangos tasas/100k — HC:{fr1(tasa_hc)}  HE:{fr1(tasa_he)}  DET:{fr1(tasa_dt)}")
  )
  cat(paste0(lines, collapse = "\n"), "\n")
  
  # ---- flags (capturados) ----
  flag_fp <- "output/tables/19_qc_core_indicadores_flags.csv"
  if (file.exists(flag_fp) && show_flags > 0) {
    fl <- readr::read_csv(flag_fp, show_col_types = FALSE)
    if (nrow(fl) > 0) {
      k <- min(show_flags, nrow(fl))
      head_txt <- capture.output(print(utils::head(fl, k), n = k))
      cat(glue("\n• Flags (primeros {k}/{nrow(fl)}):\n"))
      cat(paste(head_txt, collapse = "\n"), "\n")
    } else {
      cat("\n• Flags: (ninguno)\n")
    }
  } else {
    cat("\n• Flags: (archivo no encontrado o show_flags=0)\n")
  }
  
  ok <- (he_le_hc==0) && (de_le_dt==0) && (ok_share==0) && (bad_rates==0) && length(miss)==0
  cat(if (ok) "\n✅ PASS — QC19 sin incidencias críticas\n\n" else "\n❌ Hay incidencias — revisa detalles arriba / flags\n\n")
  invisible(ok)
}
qc19_console()