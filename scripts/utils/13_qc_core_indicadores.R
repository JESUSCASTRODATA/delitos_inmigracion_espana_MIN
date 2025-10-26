#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
###############################################################################
# 13_qc_core_indicadores.R — QC previo de core_indicadores_con_pib.csv
# Proyecto : Delitos e Inmigración en España (2010–2023)
# Autor    : Jesús Castro · JESUSCASTRODATA
# Fecha    : 2025-09-19
#
# Qué verifica (criterios medibles):
#  1) Esquema/cobertura: columnas esperadas; años contiguos 2010–2023; sin duplicados.
#  2) Tipos/NA: ano entero; numéricos finitos (NA solo PIB_yoy primer año).
#  3) Identidades y rangos: HE≤HC; det_ext≤det_tot; share_ext≈det_ext/det_tot;
#     tasas≈100k*conteo/población; pct_extranjeros∈[0,1] o [0,100]; índice≈100 (año 2010).
#  4) Estabilidad temporal (banderas no bloqueantes): umbrales YoY por variable.
#  5) Coherencias cruzadas: he_total/hc_total∈(0,1]; cor(+): share_ext vs pct_extranjeros;
#     pib_pc_real_z centrado (|media|≤0.2, sd∈[0.8,1.2]).
#  6) Outliers (IQR) en tasas y share_ext (banderas).
#
# Salidas:
#  - output/tables/13_qc_core_indicadores_resumen.csv
#  - output/tables/13_qc_core_indicadores_flags.csv
#  - output/tables/13_qc_core_indicadores_hash.csv
###############################################################################

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(tidyr); library(here)
  library(digest); library(glue); library(purrr)
})

msg <- function(...) message("[13] ", paste0(...))
ensure_dir <- function(path) if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
ensure_dir(here::here("output/tables"))

# --- Entrada -----------------------------------------------------------------
infile <- here::here("data/processed","core_indicadores_con_pib.csv")
if (!file.exists(infile)) stop("[13] No existe data/processed/core_indicadores_con_pib.csv", call. = FALSE)
msg("Archivo: ", normalizePath(infile, winslash = "/"))

df <- readr::read_csv(infile, show_col_types = FALSE)

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

# --- Normalización temprana de share_ext (persistente) -----------------------
# Si share_ext viene en %, normaliza a [0,1] y guarda copia en share_ext_pct + backup
if ("share_ext" %in% names(df)) {
  mean_share <- suppressWarnings(mean(df$share_ext, na.rm = TRUE))
  if (is.finite(mean_share) && mean_share > 1) {
    bak <- paste0(infile, ".bak")
    file.copy(infile, bak, overwrite = TRUE)
    df <- df %>%
      mutate(
        share_ext_pct = share_ext,   # % original para trazabilidad
        share_ext     = share_ext / 100
      )
    readr::write_csv(df, infile)
    msg("Normalizado: share_ext ahora en [0,1]. Backup en: ", bak, " · Columna nueva: share_ext_pct (%)")
  }
}

# 1) Esquema y cobertura ------------------------------------------------------
# 1.1 Columnas
if (setequal(names(df), expected_cols) || setequal(names(df), c(expected_cols, "share_ext_pct"))) {
  add_check("Esquema columnas", "OK", if ("share_ext_pct" %in% names(df)) "Columna extra: share_ext_pct (permitida)" else "Columnas exactas")
  df <- df[, intersect(c(expected_cols, "share_ext_pct"), names(df))]
} else if (all(expected_cols %in% names(df))) {
  add_check("Esquema columnas", "WARN", "Columnas extra presentes (se ignoran)")
  df <- df[, expected_cols]
} else {
  missing <- setdiff(expected_cols, names(df))
  add_check("Esquema columnas", "FAIL", glue("Faltan: {paste(missing, collapse=', ')}"))
  fail_critico <- TRUE
}

# 1.2 Periodo/duplicados
if ("ano" %in% names(df)) {
  anos <- sort(unique(df$ano))
  dup  <- nrow(df) - length(anos)
  gap  <- !all(anos == seq(min(anos, na.rm=TRUE), max(anos, na.rm=TRUE)))
  if (dup > 0) { add_check("Duplicados de año", "FAIL", glue("{dup} duplicados")); fail_critico <- TRUE } else add_check("Duplicados de año", "OK")
  # por diseño esperamos 2010–2023
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
# ano entero-ish; numéricos
if (!is.integer(df$ano)) {
  if (all(df$ano == floor(df$ano))) df$ano <- as.integer(df$ano)
}
if (!is.integer(df$ano)) { add_check("Tipo de ano", "FAIL", "ano no entero"); fail_critico <- TRUE } else add_check("Tipo de ano", "OK")

num_cols <- setdiff(names(df), "ano")
non_num <- num_cols[!vapply(df[num_cols], is.numeric, logical(1))]
if (length(non_num)) { add_check("Tipos numéricos", "FAIL", paste("No numéricas:", paste(non_num, collapse=", "))); fail_critico <- TRUE } else add_check("Tipos numéricos", "OK")

# NAs permitidos: pib_pc_real_yoy en primer año
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
tol_abs <- 1e-8   # muy estricto para proporciones
tol_rel <- 0.005  # 0.5% para tasas

# HE ≤ HC
viol_he <- df %>% filter(he_total > hc_total)
if (nrow(viol_he)) { add_check("HE ≤ HC", "FAIL", glue("{nrow(viol_he)} violaciones")); fail_critico <- TRUE } else add_check("HE ≤ HC", "OK")

# det_ext ≤ det_tot
viol_det <- df %>% filter(det_ext > det_tot)
if (nrow(viol_det)) { add_check("det_ext ≤ det_tot", "FAIL", glue("{nrow(viol_det)} violaciones")); fail_critico <- TRUE } else add_check("det_ext ≤ det_tot", "OK")

# share_ext ≈ det_ext/det_tot (en proporción)
cmp_share <- df %>%
  mutate(ratio = det_ext / det_tot,
         diff  = abs(share_ext - ratio))
viol_share <- cmp_share %>% filter(!is.finite(ratio) | diff > tol_abs)
if (nrow(viol_share)) {
  add_check("share_ext = det_ext/det_tot", "FAIL", glue("{nrow(viol_share)} fuera de tolerancia"))
  fail_critico <- TRUE
} else add_check("share_ext = det_ext/det_tot", "OK")

# pct_extranjeros: 0–1 o 0–100 (consistente)
rng <- range(df$pct_extranjeros_poblacion, na.rm=TRUE)
in_0_1   <- rng[1] >= 0 && rng[2] <= 1
in_0_100 <- rng[1] >= 0 && rng[2] <= 100
if (!(in_0_1 || in_0_100)) {
  add_check("% extranjeros: rango", "FAIL", glue("Rango {round(rng[1],3)}–{round(rng[2],3)} inválido")); fail_critico <- TRUE
} else add_check("% extranjeros: rango", "OK", if (in_0_1) "escala [0,1]" else "escala [0,100]")

# Tasas ≈ 100k * conteo / población
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

# PIB_yoy: NA solo primer año; |YoY| ≤ 12% (bandera si excede)
yoy_bad <- df %>% filter(ano != first_year, !is.na(pib_pc_real_yoy) & abs(pib_pc_real_yoy) > 0.12)
if (nrow(yoy_bad)) {
  yoy_bad %>% rowwise() %>%
    do({ add_flag(.$ano, "pib_pc_real_yoy", .$pib_pc_real_yoy, "WARN", "YoY > 12%", "PIB_yoy NA/umbral"); tibble() })
}
add_check("PIB_yoy NA/umbral", if (nrow(yoy_bad)) "WARN" else "OK",
          if (nrow(yoy_bad)) glue("{nrow(yoy_bad)} años > 12%") else "")

# 4) Estabilidad temporal (banderas) -----------------------------------------
stab <- function(x) all(is.finite(x)) && length(x) >= 2

# %Δ helper sin reciclado
pct_chg <- function(v) {
  v <- as.numeric(v)
  if (length(v) < 2) return(rep(NA_real_, length(v)))
  c(NA_real_, diff(v) / head(v, -1))
}

if (stab(df$det_tot)) {
  yo <- pct_chg(df$det_tot); idx <- which(abs(yo) > 0.25)
  purrr::walk(idx, ~ add_flag(df$ano[.x], "det_tot_yoy", yo[.x], "WARN", "|YoY|>25%", "Estabilidad temporal"))
}
if (stab(df$det_ext)) {
  yo <- pct_chg(df$det_ext); idx <- which(abs(yo) > 0.45)
  purrr::walk(idx, ~ add_flag(df$ano[.x], "det_ext_yoy", yo[.x], "WARN", "|YoY|>45%", "Estabilidad temporal"))
}
if (stab(df$hc_total)) {
  yo <- pct_chg(df$hc_total); idx <- which(abs(yo) > 0.20)
  purrr::walk(idx, ~ add_flag(df$ano[.x], "hc_total_yoy", yo[.x], "WARN", "|YoY|>20%", "Estabilidad temporal"))
}
if (stab(df$he_total)) {
  yo <- pct_chg(df$he_total); idx <- which(abs(yo) > 0.20)
  purrr::walk(idx, ~ add_flag(df$ano[.x], "he_total_yoy", yo[.x], "WARN", "|YoY|>20%", "Estabilidad temporal"))
}
if (stab(df$poblacion_total)) {
  yo <- pct_chg(df$poblacion_total); idx <- which(abs(yo) > 0.03)
  purrr::walk(idx, ~ add_flag(df$ano[.x], "poblacion_total_yoy", yo[.x], "WARN", "|YoY|>3%", "Estabilidad temporal"))
}

# share_ext y pct_extranjeros: rango y saltos
if (any(df$share_ext < 0 | df$share_ext > 0.6, na.rm=TRUE)) {
  idx <- which(df$share_ext < 0 | df$share_ext > 0.6)
  purrr::walk(idx, ~ add_flag(df$ano[.x], "share_ext", df$share_ext[.x], "WARN", "fuera de [0,0.6]", "Rango share_ext"))
}
if (stab(df$share_ext)) {
  j <- c(NA_real_, diff(df$share_ext))
  idx <- which(abs(j) > 0.10)
  purrr::walk(idx, ~ add_flag(df$ano[.x], "share_ext_jump", j[.x], "WARN", "|Δ|>10 p.p.", "Saltos share_ext"))
}
if (stab(df$pct_extranjeros_poblacion)) {
  pe <- df$pct_extranjeros_poblacion
  if (max(pe, na.rm=TRUE) > 1) pe <- pe / 100
  j <- c(NA_real_, diff(pe))
  idx <- which(abs(j) > 0.10)
  purrr::walk(idx, ~ add_flag(df$ano[.x], "pct_extranjeros_jump", j[.x], "WARN", "|Δ|>10 p.p.", "Saltos % extranjeros"))
}

# 5) Coherencias cruzadas -----------------------------------------------------
ratio <- df$he_total / df$hc_total
bad_ratio <- which(!is.finite(ratio) | ratio <= 0 | ratio > 1)
if (length(bad_ratio)) { add_check("he_total/hc_total in (0,1]", "FAIL", glue("{length(bad_ratio)} fuera de rango")); fail_critico <- TRUE } else add_check("he_total/hc_total in (0,1]", "OK")

pe_norm <- if (max(df$pct_extranjeros_poblacion, na.rm=TRUE) > 1) df$pct_extranjeros_poblacion/100 else df$pct_extranjeros_poblacion
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

# 6) Outliers (IQR) -----------------------------------------------------------
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
# Resumen
readr::write_csv(resumen, here::here("output/tables","13_qc_core_indicadores_resumen.csv"))
msg("Escrito: output/tables/13_qc_core_indicadores_resumen.csv")

# Flags (si no hay, escribimos cabecera vacía)
if (nrow(flags) == 0) flags <- tibble(ano=integer(), variable=character(), valor=double(), tipo=character(), motivo=character(), check=character())
readr::write_csv(flags, here::here("output/tables","13_qc_core_indicadores_flags.csv"))
msg("Escrito: output/tables/13_qc_core_indicadores_flags.csv")

# Hash/tamaño
hash <- digest::digest(infile, algo = "sha256", file = TRUE)
bytes <- file.info(infile)$size
info  <- tibble(path = "data/processed/core_indicadores_con_pib.csv",
                sha256 = hash, bytes = bytes, generated_utc = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
readr::write_csv(info, here::here("output/tables","13_qc_core_indicadores_hash.csv"))
msg("Escrito: output/tables/13_qc_core_indicadores_hash.csv")

# Mensaje final y código de salida (seguro para RStudio)
if (fail_critico) {
  msg("QC CRÍTICO: hubo FAIL en checks esenciales. Revisa '13_qc_core_indicadores_resumen.csv' y '..._flags.csv'.")
  if (interactive()) {
    stop("[13] QC CRÍTICO: corrige antes de continuar.", call. = FALSE)
  } else {
    quit(status = 1, save = "no")
  }
} else {
  msg("QC OK: sin FAIL críticos. Puedes continuar con scripts/18_stationarity_checks.R")
}
