#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
###############################################################################
# 15_limpieza_detenciones_totales.R — Nivel Nacional (2010–2023) · v1.5
# Proyecto: Delitos e Inmigración en España (2010–2023)
# Autor: Jesús Castro · JESUSCASTRODATA
# Fecha: 2025-09-18 (act. v1.5)
#
# ENTRA
#   - data/raw/detenciones_tipologia.csv
#   - (opcional) config/map_tipologias.csv  ← normaliza etiquetas (raw_tipo → propuesta_tipo)
# SALE
#   - data/processed/detenciones_totales_total_nacional.csv (ano, tipo, valor)
#   - data/processed/detenciones_totales_total.csv          (ano, det_tot)
# QA
#   - output/tables/qa_det_tot_anios_fuera_rango.csv
#   - output/tables/qa_det_tot_na.csv
#   - output/tables/qa_det_tot_negativos.csv
#   - output/tables/qa_det_tot_duplicados.csv
#   - output/tables/qa_det_tot_cobertura.csv
#   - output/tables/qa_det_tot_cobertura_por_tipo.csv
#   - output/tables/qa_det_tot_vs_suma.csv (si existe etiqueta TOTAL)
#   - output/tables/qa_det_tot_plaus_yoy.csv
#   - output/tables/qa_det_tot_exclusion_regiones.csv
###############################################################################

suppressPackageStartupMessages({
  library(here); library(readr); library(dplyr); library(tidyr); library(janitor)
  library(stringr); library(purrr); library(tibble); library(fs)
})

# Utils del proyecto
source(here::here("scripts","00_utils_limpieza.R"))
root_init("scripts/15_limpieza_detenciones_totales.R")

msg <- function(...) message("[15] ", paste0(...))

YEAR_MIN <- 2010L; YEAR_MAX <- 2023L; YEARS_SEQ <- YEAR_MIN:YEAR_MAX

# ---------- Rutas ----------
fp_in   <- here::here("data","raw","detenciones_tipologia.csv")
fp_map  <- here::here("config","map_tipologias.csv")
fp_nat  <- here::here("data","processed","detenciones_totales_total_nacional.csv")
fp_tot  <- here::here("data","processed","detenciones_totales_total.csv")
qa_dir  <- here::here("output","tables")
dir.create(qa_dir, recursive = TRUE, showWarnings = FALSE)

assert_infile(fp_in)

# ---------- Lectura + detección de columnas ----------
raw <- read_raw(fp_in)  # usa autodetección sep/encoding y todo a texto
cn  <- names(raw); cn_low <- tolower(cn)

pick_col <- function(patterns, not = NULL) {
  pat <- paste(patterns, collapse = "|")
  hit <- stringr::str_detect(cn_low, pat)
  if (!is.null(not)) hit <- hit & !stringr::str_detect(cn_low, not)
  out <- cn[which(hit)[1]]
  ifelse(length(out)==0, NA_character_, out)
}

col_region  <- pick_col(c("comun|ccaa|autono|territ|ambit|ámbito|ambito|region"))
col_tipo    <- pick_col(c("tipolog|delit|infracc|tipo"))
col_periodo <- detect_year_col(cn) %||% pick_col(c("period|anio|año|ano|fecha|year|time"))
col_valor   <- pick_col(c("^total$","^valor$","detenc|hechos|conteo|numero|n[0-9]*$"))

if (any(is.na(c(col_tipo, col_periodo, col_valor)))){
  abort(
    "❌ Columnas requeridas no detectadas.\n  region: ", ifelse(is.na(col_region),  "<NA>", col_region),
    "\n  tipo: ",   ifelse(is.na(col_tipo),   "<NA>", col_tipo),
    "\n  periodo:",ifelse(is.na(col_periodo),"<NA>", col_periodo),
    "\n  valor: ", ifelse(is.na(col_valor),  "<NA>", col_valor)
  )
}

DT <- raw |>
  transmute(
    region  = if (is.na(col_region)) "TOTAL NACIONAL" else .data[[col_region]],
    tipo    = as.character(.data[[col_tipo]]),
    periodo = .data[[col_periodo]],
    valor   = limpia_num(.data[[col_valor]])   # parser robusto ES/EN + tokens NA
  ) |>
  mutate(ano = extract_year(periodo)) |>
  select(ano, region, tipo, valor)

# ---------- QA 1: años fuera de rango + NAs ----------
qa_anios_fuera <- DT |>
  mutate(flag_fuera = is.na(ano) | ano < YEAR_MIN | ano > YEAR_MAX) |>
  filter(flag_fuera) |>
  arrange(ano)
if (nrow(qa_anios_fuera)) write_clean(qa_anios_fuera, file.path(qa_dir, "qa_det_tot_anios_fuera_rango.csv"))

DT <- DT |> filter(!is.na(ano), dplyr::between(ano, YEAR_MIN, YEAR_MAX))

qa_na <- DT |> filter(is.na(valor) | is.na(tipo))
if (nrow(qa_na)) write_clean(qa_na, file.path(qa_dir, "qa_det_tot_na.csv"))

# ---------- Normaliza texto (región/tipo) ----------
normalize_text <- function(x){
  x |> norm_ascii() |> stringr::str_squish()
}
DT <- DT |> mutate(region_norm = normalize_text(region), tipo_norm = normalize_text(tipo))

# ---------- Diagnóstico de exclusión por regiones no-CCAA ----------
excluir_regiones <- c("desconocida","no consta","no especificado","sin especificar",
                      "en el extranjero","extranjero","otros territorios","resto del mundo")
ex_diag <- DT |>
  mutate(excluida = region_norm %in% excluir_regiones | region_norm %in% c("total nacional","nacional","total")) |>
  count(region, region_norm, excluida, name = "n") |>
  arrange(desc(excluida), desc(n))
write_clean(ex_diag, file.path(qa_dir, "qa_det_tot_exclusion_regiones.csv"))

# ---------- Agregación a Total Nacional (por tipología) ----------
has_total_nacional <- any(DT$region_norm %in% c("total nacional","nacional","total"), na.rm = TRUE)

nat <- if (has_total_nacional) {
  msg("Usando 'TOTAL NACIONAL' presente en detenciones.")
  DT |> filter(region_norm %in% c("total nacional","nacional","total")) |>
    group_by(ano, tipo) |>
    summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop")
} else {
  msg("No hay 'TOTAL NACIONAL' → sumando CCAA válidas (excluye desconocida/extranjero).")
  DT |> filter(!region_norm %in% c(excluir_regiones, "total nacional","nacional","total")) |>
    group_by(ano, tipo) |>
    summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop")
}

# ---------- Normalizar tipologías (si hay mapping) *antes* de detectar TOTAL ----------
if (file.exists(fp_map)){
  map_tip <- readr::read_csv(fp_map, show_col_types = FALSE) |>
    janitor::clean_names() |>
    dplyr::distinct(raw_tipo, .keep_all = TRUE)  # evita many-to-many
  if (all(c("raw_tipo","propuesta_tipo") %in% names(map_tip))){
    nat <- nat |>
      dplyr::left_join(map_tip |> dplyr::select(raw_tipo, propuesta_tipo),
                       by = c("tipo" = "raw_tipo")) |>
      dplyr::mutate(tipo = dplyr::coalesce(propuesta_tipo, tipo)) |>
      dplyr::select(-propuesta_tipo)
  }
}

# ---------- QA 2: negativos, duplicados, cobertura ----------
qa_neg <- nat |> filter(!is.na(valor) & valor < 0)
if (nrow(qa_neg)) write_clean(qa_neg, file.path(qa_dir, "qa_det_tot_negativos.csv"))

qa_dup <- nat |> count(ano, tipo, name = "n") |> filter(n > 1)
if (nrow(qa_dup)) write_clean(qa_dup, file.path(qa_dir, "qa_det_tot_duplicados.csv"))

present <- sort(unique(nat$ano)); missing <- setdiff(YEARS_SEQ, present)
qa_cov <- tibble(variable = "detenciones_totales",
                 years_min = ifelse(length(present) > 0, min(present), NA_integer_),
                 years_max = ifelse(length(present) > 0, max(present), NA_integer_),
                 n_years = length(present), n_missing = length(missing),
                 missing_list = paste(missing, collapse = ", "))
write_clean(qa_cov, file.path(qa_dir, "qa_det_tot_cobertura.csv"))

qa_cov_tipo <- nat |> group_by(tipo) |>
  summarise(n_years = n_distinct(ano),
            complete = as.integer(n_years == length(YEARS_SEQ)),
            missing_list = paste(setdiff(YEARS_SEQ, sort(unique(ano))), collapse = ", "),
            .groups = "drop")
write_clean(qa_cov_tipo, file.path(qa_dir, "qa_det_tot_cobertura_por_tipo.csv"))

# ---------- Selección del TOTAL y contraste con suma de tipologías ----------
norm_tipo <- normalize_text(nat$tipo)
mask_total_label <- grepl("^total", norm_tipo) & grepl("deten|investig|persona|imputad", norm_tipo)

nat_totlabel <- nat |> filter(mask_total_label) |> transmute(ano, det_tot = valor)

nat_sum <- nat |> filter(!mask_total_label) |>
  group_by(ano) |>
  summarise(det_tot_sum = sum(valor, na.rm = TRUE), .groups = "drop")

det_tot <- if (nrow(nat_totlabel) > 0) {
  cmp <- nat_totlabel |>
    full_join(nat_sum, by = "ano") |>
    mutate(
      diff = det_tot - det_tot_sum,
      rel_diff = if_else(det_tot_sum > 0, diff / det_tot_sum, NA_real_)
    ) |>
    arrange(ano)
  write_clean(cmp, file.path(qa_dir, "qa_det_tot_vs_suma.csv"))
  # Criterio: |rel_diff| <= 0,5% o |diff| <= 100
  cmp |>
    transmute(
      ano,
      det_tot = dplyr::case_when(
        is.na(det_tot) ~ det_tot_sum,
        is.na(det_tot_sum) ~ det_tot,
        abs(rel_diff) <= 0.005 | abs(diff) <= 100 ~ det_tot,
        TRUE ~ det_tot_sum
      )
    )
} else {
  nat_sum |> transmute(ano, det_tot = det_tot_sum)
}

# ---------- QA 3: plausibilidad y YoY del TOTAL ----------
df_tot <- det_tot |> arrange(ano)
pl_ok <- is.finite(min(df_tot$det_tot, na.rm = TRUE)) &&
  is.finite(max(df_tot$det_tot, na.rm = TRUE)) &&
  (min(df_tot$det_tot, na.rm = TRUE) >= 5e4) && (max(df_tot$det_tot, na.rm = TRUE) <= 2e6)

df_tot <- df_tot |> mutate(
  yoy = det_tot / dplyr::lag(det_tot) - 1,
  yoy_flag = abs(yoy) > 0.25
)
qap <- tibble(
  min = suppressWarnings(min(df_tot$det_tot, na.rm = TRUE)),
  max = suppressWarnings(max(df_tot$det_tot, na.rm = TRUE)),
  plaus_ok = pl_ok,
  yoy_max_abs = suppressWarnings(max(abs(df_tot$yoy), na.rm = TRUE)),
  yoy_breaches = sum(df_tot$yoy_flag, na.rm = TRUE)
)
write_clean(qap, file.path(qa_dir, "qa_det_tot_plaus_yoy.csv"))

# ---------- Salidas ----------
final_nat <- nat |>
  arrange(ano, tipo) |>
  select(ano, tipo, valor)
write_clean(final_nat, fp_nat)

final_tot <- det_tot |>
  arrange(ano) |>
  select(ano, det_tot)
write_clean(final_tot, fp_tot)

msg("✔ Detenciones totales (TOTAL NACIONAL por tipología + serie total) generadas.")
