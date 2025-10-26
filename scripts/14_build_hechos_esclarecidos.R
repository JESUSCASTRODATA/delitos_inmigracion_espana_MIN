#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
################################################################################
# 14_build_hechos_esclarecidos.R — Builder unificado (HE limpio + cierre + unidades + QA)
# Proyecto   : Delitos e Inmigración en España (2010–2023)
# Autor      : Jesús Castro · JESUSCASTRODATA
# Licencia   : MIT (código) · CC BY 4.0 (docs/figuras)
# Fecha      : `r format(Sys.Date())`
# Descripción:
#   - Limpia y armoniza el CSV de Hechos Esclarecidos (HE).
#   - Genera:
#       (a) panel CCAA (excluye regiones especiales),
#       (b) total nacional por tipología (sin duplicar la fila TOTAL),
#       (c) total nacional correcto (usa la fila TOTAL del RAW si existe; fallback sólido).
#   - QA: cobertura, duplicados, negativos, unidades, HE≤HC y diagnósticos.
# Entradas RAW:
#   data/raw/hechos_esclarecidos.csv   (comunidades_autonomas, tipologia_penal, periodo, total)
#   config/map_regiones.csv            (raw_region, propuesta_region) [opcional]
#   config/map_tipologias.csv          (raw_tipo, propuesta_tipo)     [opcional]
#   config/qa_excepciones_he_vs_hc.csv (ano, tipo[, motivo])          [opcional]
# Preferencias HC para QA cruzado:
#   data/processed/hechos_conocidos_total_nacional_por_tipo.csv | ..._total_nacional.csv | data/raw/hechos_conocidos.csv
# Salidas PROC:
#   data/processed/hechos_esclarecidos_panel_ccaa.csv                 (ano, region, tipo, valor)
#   data/processed/hechos_esclarecidos_total_nacional_por_tipo.csv    (ano, tipo, total)
#   data/processed/hechos_esclarecidos_total_nacional.csv             (ano, total)
# Salidas QA:
#   output/tables/14_he_cov_years.csv
#   output/tables/14_he_dups_keys.csv
#   output/tables/14_he_unit_detection.csv
#   output/tables/14_he_nacional_vs_sum_ccaa.csv
#   output/tables/14_he_vs_hc_violaciones.csv
#   output/tables/14_he_vs_hc_fullcheck.csv
#   output/tables/14_he_bad_mapping_rows.csv
#   output/tables/14_he_regiones_especiales_counts.csv
#   output/tables/14_he_total_vs_suma_categorias.csv
#   output/tables/14_he_qa_summary.csv
################################################################################

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(janitor); library(stringr)
  library(here);  library(tidyr); library(tibble)
})

# -------------------------- Config -------------------------------------------
YEAR_MIN <- as.integer(Sys.getenv("YEAR_MIN", "2010"))
YEAR_MAX <- as.integer(Sys.getenv("YEAR_MAX", "2023"))
APPLY_UNIT_FIX <- tolower(Sys.getenv("APPLY_UNIT_FIX","false")) %in% c("1","true","yes","y")
TOL_REL_TOTAL <- as.numeric(Sys.getenv("TOL_REL_TOTAL","0.005"))  # 0.5%
TOL_ABS_TOTAL <- as.numeric(Sys.getenv("TOL_ABS_TOTAL","100"))    # 100

# -------------------------- Utils --------------------------------------------
source(here::here("scripts","00_utils_limpieza.R"))
root_init("scripts/14_build_hechos_esclarecidos.R")
msg <- function(...) message("[14] ", paste0(...))

pick_col <- function(nms, patterns){
  ix <- unique(unlist(lapply(patterns, function(p) stringr::str_which(nms, stringr::regex(p, ignore_case = TRUE)))))
  if (length(ix)) nms[ix[1]] else NA_character_
}

lower_noacc <- function(x){ norm_ascii(x) }

is_special_region <- function(x){
  k <- lower_noacc(x)
  grepl("^total", k) | k %in% c("desconocida", "en el extranjero")
}

is_total_tipo <- function(x){
  x2 <- tolower(stringr::str_trim(x))
  startsWith(x2, "total") | grepl("^total\\b|^total\\s+infr", x2)
}

# -------------------------- Lectura ------------------------------------------
fp_he_raw <- here::here("data","raw","hechos_esclarecidos.csv")
assert_infile(fp_he_raw)
he_raw <- read_raw(fp_he_raw) |> janitor::clean_names()

req_he <- c("comunidades_autonomas","tipologia_penal","periodo","total")
if (!all(req_he %in% names(he_raw))) {
  stop("Cabeceras HE inesperadas. Esperadas: ", paste(req_he, collapse=", "),
       " | Detectadas: ", paste(names(he_raw), collapse=", "))
}

# Mappings
map_reg <- tryCatch(readr::read_csv(here::here("config","map_regiones.csv"), show_col_types = FALSE) |> janitor::clean_names(), error = function(...) tibble())
map_tip <- tryCatch(readr::read_csv(here::here("config","map_tipologias.csv"), show_col_types = FALSE) |> janitor::clean_names(), error = function(...) tibble())
if (!all(c("raw_region","propuesta_region") %in% names(map_reg))) map_reg <- tibble()
if (!all(c("raw_tipo","propuesta_tipo") %in% names(map_tip)))     map_tip <- tibble()

# Whitelist HE≤HC
whitelist <- if (file.exists(here::here("config","qa_excepciones_he_vs_hc.csv"))) {
  readr::read_csv(here::here("config","qa_excepciones_he_vs_hc.csv"), show_col_types = FALSE) |>
    janitor::clean_names() |>
    dplyr::transmute(ano = suppressWarnings(as.integer(ano)), tipo = as.character(tipo)) |>
    dplyr::distinct() |>
    dplyr::filter(!is.na(ano), nzchar(tipo))
} else tibble(ano=integer(), tipo=character())

# Preferencias HC para QA cruzado
hc_ref <- tibble()
fp_hc_tipo <- here::here("data","processed","hechos_conocidos_total_nacional_por_tipo.csv")
fp_hc_tot  <- here::here("data","processed","hechos_conocidos_total_nacional.csv")

if (file.exists(fp_hc_tipo)) {
  hc_ref <- readr::read_csv(fp_hc_tipo, show_col_types = FALSE) |>
    janitor::clean_names() |>
    dplyr::transmute(ano = as.integer(ano), tipo = as.character(tipo), total_hc = limpia_num(total))
} else if (file.exists(fp_hc_tot)) {
  hc_ref <- readr::read_csv(fp_hc_tot, show_col_types = FALSE) |>
    janitor::clean_names() |>
    dplyr::transmute(ano = as.integer(ano), total_hc = limpia_num(total))
} else if (file.exists(here::here("data","raw","hechos_conocidos.csv"))) {
  hc_ref <- read_raw(here::here("data","raw","hechos_conocidos.csv")) |>
    janitor::clean_names() |>
    dplyr::transmute(ano = extract_year(periodo),
                     es_total = grepl("^total", lower_noacc(comunidades_autonomas)),
                     tipo = tipologia_penal,
                     total_hc = limpia_num(total)) |>
    dplyr::filter(es_total) |>
    dplyr::group_by(ano) |> dplyr::summarise(total_hc = sum(total_hc, na.rm = TRUE), .groups="drop")
} else {
  msg("ℹ No hay referencia HC (omitido QA HE≤HC).")
}

# -------------------------- Limpieza base ------------------------------------
he0 <- he_raw |>
  dplyr::transmute(
    region_raw = comunidades_autonomas,
    tipo_raw   = tipologia_penal,
    ano        = extract_year(periodo),
    valor      = limpia_num(total)
  ) |>
  dplyr::filter(!is.na(ano), dplyr::between(ano, YEAR_MIN, YEAR_MAX))

he_special <- he0 |> dplyr::filter(is_special_region(region_raw))
he_norm    <- he0 |> dplyr::filter(!is_special_region(region_raw))

# Mapping por claves normalizadas
he_norm1 <- he_norm |>
  dplyr::mutate(key_reg = lower_noacc(region_raw),
                key_tip = lower_noacc(tipo_raw))

map_reg_key <- if (nrow(map_reg)) map_reg |> dplyr::transmute(key_reg = lower_noacc(raw_region), region  = propuesta_region) |> dplyr::distinct(key_reg, .keep_all = TRUE) else tibble()
map_tip_key <- if (nrow(map_tip)) map_tip |> dplyr::transmute(key_tip = lower_noacc(raw_tipo),   tipo    = propuesta_tipo)    |> dplyr::distinct(key_tip, .keep_all = TRUE) else tibble()

he2 <- he_norm1
if (nrow(map_reg_key)) he2 <- he2 |> dplyr::left_join(map_reg_key, by = "key_reg")
if (nrow(map_tip_key)) he2 <- he2 |> dplyr::left_join(map_tip_key, by = "key_tip")

he2 <- he2 |>
  dplyr::mutate(region = dplyr::coalesce(region, region_raw),
                tipo   = dplyr::coalesce(tipo,   tipo_raw)) |>
  dplyr::select(ano, region, tipo, valor, region_raw, tipo_raw)

# Conteos informativos y bad mapping
write_clean(he_special |> dplyr::count(region_raw, name = "n"), here::here("output","tables","14_he_regiones_especiales_counts.csv"))

bad_map <- he2 |> dplyr::filter(is.na(region) | is.na(tipo))
if (nrow(bad_map)) {
  write_clean(bad_map, here::here("output","tables","14_he_bad_mapping_rows.csv"))
  msg(sprintf("⚠ %d filas con mapeo incompleto (ver 14_he_bad_mapping_rows.csv).", nrow(bad_map)))
} else {
  msg("✓ Sin filas con mapeo incompleto en CCAA (las especiales se tratan aparte).")
}

# -------------------------- Panel CCAA ---------------------------------------
panel_ccaa <- he2 |>
  dplyr::group_by(ano, region, tipo) |>
  dplyr::summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop")

# -------------------------- Nacional por tipología ---------------------------
# Suma de CCAA por tipo (excluye regiones especiales porque panel_ccaa ya las excluyó)
he_nac_por_tipo <- panel_ccaa |>
  dplyr::group_by(ano, tipo) |>
  dplyr::summarise(total = sum(valor, na.rm = TRUE), .groups = "drop") |>
  dplyr::arrange(ano, tipo)

# Diagnóstico: si el RAW trae TOTAL NACIONAL por tipo, compáralo
tmp_total_row <- he_special |>
  dplyr::filter(grepl("^total", lower_noacc(region_raw))) |>
  dplyr::mutate(key_tip = lower_noacc(tipo_raw))
if (nrow(map_tip_key)) tmp_total_row <- dplyr::left_join(tmp_total_row, map_tip_key, by = "key_tip") else tmp_total_row <- dplyr::mutate(tmp_total_row, tipo = tipo_raw)
he_row_por_tipo <- tmp_total_row |>
  dplyr::group_by(ano, tipo) |>
  dplyr::summarise(total_row = sum(valor, na.rm = TRUE), .groups = "drop")

he_join_diag <- dplyr::full_join(he_nac_por_tipo, he_row_por_tipo, by = c("ano","tipo")) |>
  dplyr::mutate(delta = total - total_row,
                rel   = dplyr::if_else(!is.na(total_row) & total_row!=0, abs(delta)/abs(total_row), NA_real_),
                usar_row = !is.na(total_row) & (abs(delta) <= TOL_ABS_TOTAL | rel <= TOL_REL_TOTAL))
write_clean(he_join_diag, here::here("output","tables","14_he_nacional_vs_sum_ccaa.csv"))

# -------------------------- Total nacional (todos los tipos) -----------------
# (A) Preferido: fila TOTAL de tipología en TOTAL NACIONAL
he_total_row <- he_special |>
  dplyr::filter(grepl("^total", lower_noacc(region_raw)) & is_total_tipo(tipo_raw)) |>
  dplyr::group_by(ano) |>
  dplyr::summarise(total = sum(valor, na.rm = TRUE), .groups = "drop")

# (B) Fallback: sumar CCAA SOLO de la tipología TOTAL (si existe en CCAA)
he_total_ccaa <- he2 |>
  dplyr::filter(is_total_tipo(tipo)) |>
  dplyr::group_by(ano) |>
  dplyr::summarise(total = sum(valor, na.rm = TRUE), .groups = "drop")

# (C) Elección
if (nrow(he_total_row)) {
  total_nacional <- he_total_row
} else if (nrow(he_total_ccaa)) {
  total_nacional <- he_total_ccaa
  msg("WARN: usando TOTAL por suma de CCAA (tipología TOTAL) por ausencia de fila TOTAL nacional en RAW.")
} else {
  total_nacional <- tibble(ano = integer(), total = numeric())
  msg("⚠ No se pudo derivar 'total_nacional' (no hay fila TOTAL ni tipología TOTAL por CCAA).")
}

# Diagnóstico: suma de categorías vs fila TOTAL (esperable >1 por solapes)
he_sum_categorias <- he_nac_por_tipo |> dplyr::filter(!is_total_tipo(tipo)) |> dplyr::group_by(ano) |> dplyr::summarise(sum_categorias = sum(total, na.rm = TRUE), .groups = "drop")
qa_total_check <- dplyr::left_join(he_sum_categorias, total_nacional, by = "ano") |>
  dplyr::mutate(inflation_factor = sum_categorias / total)
write_clean(qa_total_check, here::here("output","tables","14_he_total_vs_suma_categorias.csv"))
if (nrow(qa_total_check)) msg(sprintf("WARN: suma categorías vs TOTAL → mediana=%.2f (diagnóstico).", stats::median(qa_total_check$inflation_factor, na.rm = TRUE)))

# -------------------------- Detección/ajuste de unidades ---------------------
med_n <- stats::median(total_nacional$total %||% NA_real_, na.rm = TRUE)
p90_n <- suppressWarnings(stats::quantile(total_nacional$total %||% NA_real_, 0.9, na.rm = TRUE, type = 7))
unit_flag <- is.finite(med_n) && med_n > 1e6 && p90_n < 1e8

unit_tbl <- tibble::tibble(mediana_total = med_n, p90_total = as.numeric(p90_n), probable_miles = unit_flag, aplicar = unit_flag && APPLY_UNIT_FIX)
write_clean(unit_tbl, here::here("output","tables","14_he_unit_detection.csv"))

scale_factor <- if (unit_flag && APPLY_UNIT_FIX) 1e-3 else 1.0
panel_ccaa$valor      <- panel_ccaa$valor * scale_factor
he_nac_por_tipo$total <- he_nac_por_tipo$total * scale_factor
total_nacional$total  <- total_nacional$total * scale_factor

# -------------------------- Salidas ------------------------------------------
write_clean(panel_ccaa |> dplyr::arrange(ano, region, tipo), here::here("data","processed","hechos_esclarecidos_panel_ccaa.csv"))
write_clean(he_nac_por_tipo |> dplyr::arrange(ano, tipo),    here::here("data","processed","hechos_esclarecidos_total_nacional_por_tipo.csv"))
write_clean(total_nacional |> dplyr::arrange(ano),           here::here("data","processed","hechos_esclarecidos_total_nacional.csv"))
msg("✓ HE escritos (panel CCAA, total por tipología y total nacional).")

# -------------------------- QA básicos ---------------------------------------
cov_years <- panel_ccaa |> dplyr::summarise(n = dplyr::n(), .by = c(ano)) |> tidyr::complete(ano = YEAR_MIN:YEAR_MAX, fill = list(n = 0)) |> dplyr::arrange(ano)
write_clean(cov_years, here::here("output","tables","14_he_cov_years.csv"))

dups <- he2 |> dplyr::count(ano, region, tipo, name = "n") |> dplyr::filter(n > 1)
write_clean(dups, here::here("output","tables","14_he_dups_keys.csv"))

# -------------------------- QA cruzado: HE ≤ HC -------------------------------
`%||%` <- function(a,b) if (is.null(a) || all(is.na(a))) b else a
qa_he_hc <- tibble(); viol <- tibble()
if (nrow(hc_ref)) {
  if ("tipo" %in% names(hc_ref)) {
    qa_he_hc <- he_nac_por_tipo |>
      dplyr::left_join(hc_ref, by = c("ano","tipo")) |>
      dplyr::rename(he = total) |>
      dplyr::mutate(flag = dplyr::if_else(is.finite(total_hc), he <= total_hc, NA), delta = he - total_hc) |>
      dplyr::select(ano, tipo, he, total_hc, delta, flag)
  } else {
    he_tot_year <- total_nacional |>
      dplyr::group_by(ano) |>
      dplyr::summarise(he_total = sum(total, na.rm=TRUE), .groups="drop")
    qa_he_hc <- he_tot_year |>
      dplyr::left_join(hc_ref, by="ano") |>
      dplyr::mutate(flag = dplyr::if_else(is.finite(total_hc), he_total <= total_hc, NA), tipo = NA_character_, delta = he_total - total_hc) |>
      dplyr::transmute(ano, tipo, he = he_total, total_hc, delta, flag)
  }
  qa_he_hc <- qa_he_hc |>
    dplyr::mutate(tipo_key = lower_noacc(dplyr::coalesce(as.character(tipo), ""))) |>
    dplyr::left_join(whitelist |> dplyr::mutate(tipo_key = lower_noacc(as.character(tipo))) |> dplyr::select(ano, tipo_key) |> dplyr::distinct(), by = c("ano","tipo_key")) |>
    dplyr::mutate(flag_final = dplyr::if_else(!is.na(tipo_key), TRUE, flag)) |>
    dplyr::select(ano, tipo, he, total_hc, delta, flag = flag_final)
  viol <- qa_he_hc |> dplyr::filter(isFALSE(flag))
  write_clean(qa_he_hc, here::here("output","tables","14_he_vs_hc_fullcheck.csv"))
  write_clean(viol,     here::here("output","tables","14_he_vs_hc_violaciones.csv"))
} else {
  msg("ℹ QA HE≤HC omitido (sin referencia HC).")
}

# -------------------------- Resumen QA ---------------------------------------
add_row <- function(test, status, detalle = NA_character_, output = NA_character_) tibble(test=test, status=status, detalle=detalle, output=output)
ok   <- function(x) if (isTRUE(x)) "PASS" else "FAIL"
warn <- function(x) if (isTRUE(x)) "PASS" else "WARN"

summary_rows <- list(
  add_row("Cobertura de años en panel HE (2010–2023)", ok(all((YEAR_MIN:YEAR_MAX) %in% cov_years$ano[cov_years$n>0])), detalle = paste0("años con datos: ", paste(cov_years$ano[cov_years$n>0], collapse=", ")), output  = here::here("output","tables","14_he_cov_years.csv")),
  add_row("Sin duplicados por (ano,region,tipo) en panel HE", ok(nrow(dups) == 0), detalle = paste0("duplicados: ", nrow(dups)), output  = here::here("output","tables","14_he_dups_keys.csv")),
  add_row("Consistencia unidades (detección)", warn(!unit_flag || (unit_flag && APPLY_UNIT_FIX)), detalle = paste0("probable_miles=", unit_flag, ", aplicado=", APPLY_UNIT_FIX), output  = here::here("output","tables","14_he_unit_detection.csv"))
)

acc_raw <- sum(!is.na(he_join_diag$usar_row) & he_join_diag$usar_row, na.rm = TRUE)
summary_rows[[length(summary_rows)+1]] <- add_row("TOTAL RAW ≈ suma CCAA (diag por tipología)", if (acc_raw>0) "PASS" else "WARN", detalle = paste0("coincidencias aceptables: ", acc_raw, " (TOL_ABS=", TOL_ABS_TOTAL, ", TOL_REL=", TOL_REL_TOTAL, ")"), output  = here::here("output","tables","14_he_nacional_vs_sum_ccaa.csv"))

if (nrow(hc_ref)) {
  n_viol <- if (nrow(viol)) nrow(viol) else 0
  summary_rows[[length(summary_rows)+1]] <- add_row("HE ≤ HC (nacional)", ok(n_viol == 0), detalle = paste0("violaciones: ", n_viol), output  = if (n_viol>0) here::here("output","tables","14_he_vs_hc_violaciones.csv") else NA)
}

qa_summary <- dplyr::bind_rows(summary_rows) |>
  dplyr::mutate(status = factor(status, levels = c("FAIL","WARN","PASS"))) |>
  dplyr::arrange(status, test)

write_clean(qa_summary, here::here("output","tables","14_he_qa_summary.csv"))
msg("✅ Builder HE (14) completado. Revisa output/tables/14_he_*.csv")

