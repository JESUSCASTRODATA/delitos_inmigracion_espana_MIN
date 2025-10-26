#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
################################################################################
# 14_build_hechos_esclarecidos.R — Limpieza + cierre nacional + unidades + QA
#
# IN:
#   data/raw/hechos_esclarecidos.csv   (comunidades_autonomas, tipologia_penal, periodo, total)
#   config/map_regiones.csv            (raw_region, propuesta_region)
#   config/map_tipologias.csv          (raw_tipo, propuesta_tipo)
#   config/qa_excepciones_he_vs_hc.csv (ano, tipo[, motivo])  # whitelist
#   preferido para QA cruzado:
#       data/processed/hechos_conocidos_total_nacional_por_tipo.csv (ano, tipo, total)
#       (fallback) data/processed/hechos_conocidos_total_nacional.csv
#       (fallback) data/raw/hechos_conocidos.csv
#
# OUT:
#   data/processed/hechos_esclarecidos_panel_ccaa.csv
#   data/processed/hechos_esclarecidos_total_nacional.csv
#
# QC:
#   output/tables/14_he_cov_years.csv
#   output/tables/14_he_dups_keys.csv
#   output/tables/14_he_unit_detection.csv
#   output/tables/14_he_nacional_vs_sum_ccaa.csv
#   output/tables/14_he_vs_hc_violaciones.csv
#   output/tables/14_he_vs_hc_fullcheck.csv
#   output/tables/14_he_bad_mapping_rows.csv
#   output/tables/14_he_regiones_especiales_counts.csv
#   output/tables/14_he_qa_summary.csv
#
# FLAGS (env):
#   YEAR_MIN=2010, YEAR_MAX=2023
#   APPLY_UNIT_FIX=false  # reescala a unidades si se detecta factor 1e3
#   TOL_REL_TOTAL=0.005   # 0.5% tolerancia para aceptar TOTAL RAW ≈ suma CCAA (diag)
#   TOL_ABS_TOTAL=100     # 100   tolerancia absoluta (diag)
################################################################################

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(janitor); library(stringr)
  library(here);  library(tidyr); library(purrr)
})

# Utils del proyecto
source(here::here("scripts","00_utils_limpieza.R"))
root_init("scripts/14_build_hechos_esclarecidos.R")

YEAR_MIN <- as.integer(Sys.getenv("YEAR_MIN", "2010"))
YEAR_MAX <- as.integer(Sys.getenv("YEAR_MAX", "2023"))
APPLY_UNIT_FIX <- tolower(Sys.getenv("APPLY_UNIT_FIX","false")) %in% c("1","true","yes","y")
TOL_REL_TOTAL <- as.numeric(Sys.getenv("TOL_REL_TOTAL","0.005"))
TOL_ABS_TOTAL <- as.numeric(Sys.getenv("TOL_ABS_TOTAL","100"))

msg <- function(...) message("[14] ", paste0(...))

norm_chr <- function(x){
  x0 <- as.character(x); x0 <- chartr("\u00A0"," ", x0)
  x0 <- gsub("[\r\n\t]+"," ", x0); x0 <- gsub("\\s+"," ", x0); trimws(x0)
}
extract_year <- function(x){
  x0 <- norm_chr(x); suppressWarnings(as.integer(stringr::str_extract(x0, "[12][0-9]{3}")))
}
lower_noacc <- function(x){ norm_ascii(x) }  # de utils 00

is_special_region <- function(x){
  k <- lower_noacc(x)
  grepl("^total", k) | k %in% c("desconocida", "en el extranjero")
}

dir.create(here::here("data","processed"), recursive = TRUE, showWarnings = FALSE)
dir.create(here::here("output","tables"),  recursive = TRUE, showWarnings = FALSE)

# -------------------- Lecturas --------------------
fp_he_raw <- here::here("data","raw","hechos_esclarecidos.csv")
assert_infile(fp_he_raw)
he_raw <- read_raw(fp_he_raw) |> clean_names()

req_he <- c("comunidades_autonomas","tipologia_penal","periodo","total")
if (!all(req_he %in% names(he_raw))) {
  stop("Cabeceras HE inesperadas. Esperadas: ", paste(req_he, collapse=", "),
       " | Detectadas: ", paste(names(he_raw), collapse=", "))
}

# Mappings
map_reg <- tryCatch(read_csv(here::here("config","map_regiones.csv"), show_col_types = FALSE) |> clean_names(), error = function(...) tibble())
map_tip <- tryCatch(read_csv(here::here("config","map_tipologias.csv"), show_col_types = FALSE) |> clean_names(), error = function(...) tibble())
if (!all(c("raw_region","propuesta_region") %in% names(map_reg))) map_reg <- tibble()
if (!all(c("raw_tipo","propuesta_tipo") %in% names(map_tip)))     map_tip <- tibble()

# Whitelist (motivo opcional)
whitelist <- if (file.exists(here::here("config","qa_excepciones_he_vs_hc.csv"))) {
  read_csv(here::here("config","qa_excepciones_he_vs_hc.csv"), show_col_types = FALSE) |>
    clean_names() |>
    transmute(ano = suppressWarnings(as.integer(ano)), tipo = as.character(tipo)) |>
    distinct() |>
    filter(!is.na(ano), nzchar(tipo))
} else tibble(ano=integer(), tipo=character())

# Preferencia: processed por tipología; si no, total; si no, RAW
hc_ref <- tibble()
fp_hc_tipo <- here::here("data","processed","hechos_conocidos_total_nacional_por_tipo.csv")
fp_hc_tot  <- here::here("data","processed","hechos_conocidos_total_nacional.csv")

if (file.exists(fp_hc_tipo)) {
  hc_ref <- read_csv(fp_hc_tipo, show_col_types = FALSE) |>
    clean_names() |>
    transmute(ano = as.integer(ano), tipo = as.character(tipo), total_hc = limpia_num(total))
} else if (file.exists(fp_hc_tot)) {
  hc_ref <- read_csv(fp_hc_tot, show_col_types = FALSE) |>
    clean_names() |>
    transmute(ano = as.integer(ano), total_hc = limpia_num(total))
} else if (file.exists(here::here("data","raw","hechos_conocidos.csv"))) {
  hc_ref <- read_raw(here::here("data","raw","hechos_conocidos.csv")) |>
    clean_names() |>
    transmute(ano = extract_year(periodo),
              es_total = grepl("^total", lower_noacc(comunidades_autonomas)),
              tipo = tipologia_penal,
              total_hc = limpia_num(total)) |>
    filter(es_total) |>
    group_by(ano) |> summarise(total_hc = sum(total_hc, na.rm = TRUE), .groups="drop")
} else {
  msg("⚠ No hay referencia HC (ni processed ni raw). Se omitirá QA HE≤HC.")
}

# -------------------- Limpieza base HE (con exclusión de especiales) --------------------
he0 <- he_raw |>
  transmute(
    region_raw = comunidades_autonomas,
    tipo_raw   = tipologia_penal,
    ano        = extract_year(periodo),
    valor      = limpia_num(total)
  ) |>
  filter(!is.na(ano), dplyr::between(ano, YEAR_MIN, YEAR_MAX))

he_special <- he0 |> filter(is_special_region(region_raw))
he_norm    <- he0 |> filter(!is_special_region(region_raw))

# Mapping por claves normalizadas
he_norm1 <- he_norm |>
  mutate(
    key_reg = lower_noacc(region_raw),
    key_tip = lower_noacc(tipo_raw)
  )

map_reg_key <- if (nrow(map_reg)) map_reg |> transmute(key_reg = lower_noacc(raw_region),
                                                       region  = propuesta_region) |> distinct(key_reg, .keep_all = TRUE) else tibble()
map_tip_key <- if (nrow(map_tip)) map_tip |> transmute(key_tip = lower_noacc(raw_tipo),
                                                       tipo    = propuesta_tipo) |> distinct(key_tip, .keep_all = TRUE) else tibble()

he2 <- he_norm1
if (nrow(map_reg_key)) he2 <- he2 |> left_join(map_reg_key, by = "key_reg")
if (nrow(map_tip_key)) he2 <- he2 |> left_join(map_tip_key, by = "key_tip")

he2 <- he2 |>
  mutate(region = coalesce(region, region_raw),
         tipo   = coalesce(tipo,   tipo_raw)) |>
  select(ano, region, tipo, valor, region_raw, tipo_raw)

# Conteos informativos y bad mapping
write_clean(he_special |> count(region_raw, name = "n"),
            here::here("output","tables","14_he_regiones_especiales_counts.csv"))

bad_map <- he2 |> filter(is.na(region) | is.na(tipo))
if (nrow(bad_map)) {
  write_clean(bad_map, here::here("output","tables","14_he_bad_mapping_rows.csv"))
  msg(sprintf("⚠ %d filas con mapeo incompleto (ver 14_he_bad_mapping_rows.csv).", nrow(bad_map)))
} else {
  msg("✓ Sin filas con mapeo incompleto en CCAA (las especiales se tratan aparte).")
}

# -------------------- Panel CCAA (solo CCAA mapeadas) --------------------
panel_ccaa <- he2 |>
  group_by(ano, region, tipo) |>
  summarise(he = sum(valor, na.rm = TRUE), .groups = "drop")

# -------------------- Totales nacionales --------------------
# a) Por suma de CCAA (prioritario)
tot_from_sum <- panel_ccaa |>
  group_by(ano, tipo) |>
  summarise(he = sum(he, na.rm = TRUE), .groups = "drop") |>
  mutate(region = "TOTAL NACIONAL") |>
  select(ano, region, tipo, he)

# b) Por fila 'TOTAL NACIONAL' del RAW (diagnóstico) — SIN {} en pipe
tmp_total_row <- he_special |>
  filter(grepl("^total", lower_noacc(region_raw))) |>
  mutate(key_tip = lower_noacc(tipo_raw))

if (nrow(map_tip_key)) {
  tmp_total_row <- left_join(tmp_total_row, map_tip_key, by = "key_tip")
} else {
  tmp_total_row <- mutate(tmp_total_row, tipo = tipo_raw)
}

tot_from_row <- tmp_total_row |>
  group_by(ano, tipo) |>
  summarise(he = sum(valor, na.rm = TRUE), .groups = "drop") |>
  mutate(region = "TOTAL NACIONAL") |>
  select(ano, region, tipo, he)

# Comparativo (diag con tolerancias)
total_join <- full_join(tot_from_sum, tot_from_row, by=c("ano","tipo","region"),
                        suffix=c("_sum","_row")) |>
  mutate(delta = he_sum - he_row,
         rel   = ifelse(!is.na(he_row) & he_row!=0, abs(delta)/abs(he_row), NA_real_),
         usar_raw = !is.na(he_row) & (abs(delta) <= TOL_ABS_TOTAL | rel <= TOL_REL_TOTAL))

write_clean(total_join, here::here("output","tables","14_he_nacional_vs_sum_ccaa.csv"))

# Decisión de producto: usamos SIEMPRE suma CCAA (estable)
tot_nac <- tot_from_sum

# -------------------- Detección/ajuste de unidades --------------------
med_n <- median(tot_nac$he, na.rm = TRUE)
p90_n <- suppressWarnings(quantile(tot_nac$he, 0.9, na.rm = TRUE, type = 7))
unit_flag <- is.finite(med_n) && med_n > 1e6 && p90_n < 1e8

unit_tbl <- tibble(
  mediana_total = med_n,
  p90_total = as.numeric(p90_n),
  probable_miles = unit_flag,
  aplicar = unit_flag && APPLY_UNIT_FIX
)
write_clean(unit_tbl, here::here("output","tables","14_he_unit_detection.csv"))

scale_factor <- if (unit_flag && APPLY_UNIT_FIX) 1e-3 else 1.0
panel_ccaa$he <- panel_ccaa$he * scale_factor
tot_nac$he    <- tot_nac$he * scale_factor

# -------------------- Salidas base --------------------
write_clean(panel_ccaa |> arrange(ano, region, tipo),
            here::here("data","processed","hechos_esclarecidos_panel_ccaa.csv"))
write_clean(tot_nac    |> arrange(ano, tipo),
            here::here("data","processed","hechos_esclarecidos_total_nacional.csv"))
msg("✓ HE escritos (panel CCAA y total nacional).")

# -------------------- QA básicos --------------------
cov_years <- panel_ccaa |>
  summarise(n = dplyr::n(), .by = c(ano)) |>
  complete(ano = YEAR_MIN:YEAR_MAX, fill = list(n = 0)) |>
  arrange(ano)
write_clean(cov_years, here::here("output","tables","14_he_cov_years.csv"))

dups <- he2 |>
  count(ano, region, tipo, name = "n") |>
  filter(n > 1)
write_clean(dups, here::here("output","tables","14_he_dups_keys.csv"))

# -------------------- QA cruzado: HE ≤ HC -------------------------
qa_he_hc <- tibble()
viol <- tibble()

if (nrow(hc_ref)) {
  if ("tipo" %in% names(hc_ref)) {
    qa_he_hc <- tot_nac |>
      left_join(hc_ref, by = c("ano","tipo")) |>
      rename(he = he) |>
      mutate(flag = ifelse(is.finite(total_hc), he <= total_hc, NA),
             delta = he - total_hc) |>
      select(ano, tipo, he, total_hc, delta, flag)
  } else {
    he_tot_year <- tot_nac |> group_by(ano) |> summarise(he_total = sum(he, na.rm=TRUE), .groups="drop")
    qa_he_hc <- he_tot_year |>
      left_join(hc_ref, by="ano") |>
      mutate(flag = ifelse(is.finite(total_hc), he_total <= total_hc, NA),
             tipo = NA_character_, delta = he_total - total_hc) |>
      transmute(ano, tipo, he = he_total, total_hc, delta, flag)
  }
  # aplicar whitelist
  qa_he_hc <- qa_he_hc |>
    mutate(tipo_key = lower_noacc(coalesce(as.character(tipo), ""))) |>
    left_join(whitelist |> mutate(tipo_key = lower_noacc(as.character(tipo))) |> select(ano, tipo_key) |> distinct(),
              by = c("ano","tipo_key")) |>
    mutate(flag_final = ifelse(!is.na(tipo_key), TRUE, flag)) |>
    select(ano, tipo, he, total_hc, delta, flag = flag_final)
  
  viol <- qa_he_hc |> filter(isFALSE(flag))
  write_clean(qa_he_hc, here::here("output","tables","14_he_vs_hc_fullcheck.csv"))
  write_clean(viol,     here::here("output","tables","14_he_vs_hc_violaciones.csv"))
} else {
  msg("ℹ QA HE≤HC omitido (sin referencia HC).")
}

# -------------------- Resumen QA --------------------
add_row <- function(test, status, detalle = NA_character_, output = NA_character_) {
  tibble(test=test, status=status, detalle=detalle, output=output)
}
ok   <- function(x) if (isTRUE(x)) "PASS" else "FAIL"
warn <- function(x) if (isTRUE(x)) "PASS" else "WARN"

summary_rows <- list(
  add_row(
    "Cobertura de años en panel HE (2010–2023)",
    ok(all((YEAR_MIN:YEAR_MAX) %in% cov_years$ano[cov_years$n>0])),
    detalle = paste0("años con datos: ", paste(cov_years$ano[cov_years$n>0], collapse=", ")),
    output  = here::here("output","tables","14_he_cov_years.csv")
  ),
  add_row(
    "Sin duplicados por (ano,region,tipo) en panel HE",
    ok(nrow(dups) == 0),
    detalle = paste0("duplicados: ", nrow(dups)),
    output  = here::here("output","tables","14_he_dups_keys.csv")
  ),
  add_row(
    "Consistencia unidades (detección)",
    warn(!unit_flag || (unit_flag && APPLY_UNIT_FIX)),
    detalle = paste0("probable_miles=", unit_flag, ", aplicado=", APPLY_UNIT_FIX),
    output  = here::here("output","tables","14_he_unit_detection.csv")
  )
)

# Línea INFO sobre TOTAL RAW ≈ suma CCAA (diagnóstico)
acc_raw <- sum(!is.na(total_join$usar_raw) & total_join$usar_raw, na.rm = TRUE)
summary_rows[[length(summary_rows)+1]] <- add_row(
  "TOTAL RAW ≈ suma CCAA (dentro de tolerancias, diagnóstico)",
  if (acc_raw>0) "PASS" else "WARN",
  detalle = paste0("coincidencias aceptables: ", acc_raw,
                   " (TOL_ABS=", TOL_ABS_TOTAL, ", TOL_REL=", TOL_REL_TOTAL, ")"),
  output  = here::here("output","tables","14_he_nacional_vs_sum_ccaa.csv")
)

if (nrow(hc_ref)) {
  n_viol <- if (nrow(viol)) nrow(viol) else 0
  summary_rows[[length(summary_rows)+1]] <- add_row(
    "HE ≤ HC (nacional)",
    ok(n_viol == 0),
    detalle = paste0("violaciones: ", n_viol),
    output  = if (n_viol>0) here::here("output","tables","14_he_vs_hc_violaciones.csv") else NA
  )
}

qa_summary <- bind_rows(summary_rows) |>
  mutate(status = factor(status, levels = c("FAIL","WARN","PASS"))) |>
  arrange(status, test)

write_clean(qa_summary, here::here("output","tables","14_he_qa_summary.csv"))
msg("✅ Builder HE (14) completado. Revisa output/tables/14_he_*.csv")
