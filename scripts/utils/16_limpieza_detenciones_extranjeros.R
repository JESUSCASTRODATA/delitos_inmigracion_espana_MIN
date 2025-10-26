#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
###############################################################################
# 16_limpieza_detenciones_extranjeros.R — Nivel Nacional (2010–2023) · v1.5
# Proyecto: Delitos e Inmigración en España (2010–2023)
# Autor: Jesús Castro · JESUSCASTRODATA
# Fecha: 2025-09-18 (act. v1.5)
#
# ENTRA
#   - data/raw/detenciones_extranjeros_tipologia.csv
#   - data/processed/detenciones_totales_total.csv           (para share)
#   - (opcional) config/map_tipologias.csv                   (normalización de etiquetas)
#
# SALE
#   - data/processed/detenciones_extranjeros_total_nacional_por_sexo.csv (ano, tipo, sexo, valor, metric)
#   - data/processed/detenciones_extranjeros_total_nacional.csv          (ano, tipo, valor)
#   - data/processed/detenciones_extranjeros_total.csv                   (ano, det_ext)
#   - data/processed/detenciones_share_extranjeros.csv                   (ano, share_extranjeros)
#
# QA (output/tables)
#   - 16_det_ext_anios_fuera_rango.csv | 16_det_ext_na.csv | 16_det_ext_negativos.csv | 16_det_ext_duplicados.csv
#   - 16_det_ext_cobertura.csv | 16_det_ext_cobertura_por_tipo.csv
#   - 16_det_ext_sexo_total_vs_suma.csv | 16_det_ext_diag_sexo_vs_total.csv
#   - 16_det_ext_vs_suma.csv (etiqueta TOTAL vs suma de tipologías)
#   - 16_det_ext_plaus_yoy.csv (rango y variaciones YoY)
#   - 16_det_ext_share_qc.csv (rango share y violaciones det_ext>det_tot)
#   - 16_det_ext_consistencia.csv (det_ext ≤ det_tot)
#   - 16_det_ext_claves_duplicadas.csv (si aplica)
###############################################################################

suppressPackageStartupMessages({
  library(here); library(readr); library(dplyr); library(tidyr); library(janitor)
  library(stringr); library(purrr); library(tibble); library(fs)
})

# Utils del proyecto
source(here::here("scripts","00_utils_limpieza.R"))
root_init("scripts/16_limpieza_detenciones_extranjeros.R")

msg <- function(...) message("[16] ", paste0(...))

YEAR_MIN <- 2010L; YEAR_MAX <- 2023L; YEARS_SEQ <- YEAR_MIN:YEAR_MAX

# ---------- Rutas ----------
fp_in   <- here::here("data","raw","detenciones_extranjeros_tipologia.csv")
fp_tot  <- here::here("data","processed","detenciones_totales_total.csv")
fp_map  <- here::here("config","map_tipologias.csv")
fp_out_sex   <- here::here("data","processed","detenciones_extranjeros_total_nacional_por_sexo.csv")
fp_out_nat   <- here::here("data","processed","detenciones_extranjeros_total_nacional.csv")
fp_out_tot   <- here::here("data","processed","detenciones_extranjeros_total.csv")
fp_out_share <- here::here("data","processed","detenciones_share_extranjeros.csv")
qa_dir  <- here::here("output","tables"); dir.create(qa_dir, recursive = TRUE, showWarnings = FALSE)

assert_infile(fp_in); assert_infile(fp_tot)

# ---------- Lectura + detección de columnas ----------
raw <- read_raw(fp_in) |> janitor::clean_names()
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
col_sexo    <- pick_col(c("^sexo$|sex"))
col_periodo <- detect_year_col(cn) %||% pick_col(c("period|anio|año|ano|fecha|year|time"))
col_valor   <- pick_col(c("^total$","^valor$","detenc|hechos|conteo|numero|n[0-9]*$"))

if (any(is.na(c(col_tipo, col_periodo, col_valor)))) {
  abort(
    "❌ Columnas requeridas no detectadas.\n  region: ", ifelse(is.na(col_region),  "<NA>", col_region),
    "\n  tipo: ",   ifelse(is.na(col_tipo),   "<NA>", col_tipo),
    "\n  sexo: ",   ifelse(is.na(col_sexo),   "<NA>", col_sexo),
    "\n  periodo:", ifelse(is.na(col_periodo),"<NA>", col_periodo),
    "\n  valor: ",  ifelse(is.na(col_valor),  "<NA>", col_valor)
  )
}

normalize_text <- function(x){ x |> norm_ascii() |> stringr::str_squish() }
normalize_sexo <- function(x){
  z <- normalize_text(x)
  dplyr::case_when(
    z %in% c("h","hombre","hombres","varon","varones","masculino") ~ "hombres",
    z %in% c("m","mujer","mujeres","femenino") ~ "mujeres",
    z %in% c("ambos","ambos sexos","total","total ambos","total sexo","todos") ~ "total",
    TRUE ~ z
  )
}
normalize_tipo_key <- function(x){
  z <- norm_ascii(x)
  z <- gsub("\\s*\\-\\s*", "-", z, perl = TRUE)
  z <- gsub("\\s*\\.\\s*", ".", z, perl = TRUE)
  stringr::str_squish(z)
}

E <- raw |>
  transmute(
    region  = if (is.na(col_region)) "TOTAL NACIONAL" else .data[[col_region]],
    tipo    = as.character(.data[[col_tipo]]),
    sexo    = if (is.na(col_sexo)) "total" else as.character(.data[[col_sexo]]),
    periodo = .data[[col_periodo]],
    valor   = limpia_num(.data[[col_valor]])
  ) |>
  mutate(
    ano      = extract_year(periodo),
    region   = normalize_text(region),
    sexo     = normalize_sexo(sexo),
    tipo_raw = tipo,
    tipo_key = normalize_tipo_key(tipo)
  ) |>
  select(ano, region, tipo_raw, tipo_key, sexo, valor)

# ---------- QA 1: años fuera de rango + NAs ----------
qa_anios_fuera <- E |>
  mutate(flag_fuera = is.na(ano) | ano < YEAR_MIN | ano > YEAR_MAX) |>
  filter(flag_fuera) |>
  arrange(ano)
if (nrow(qa_anios_fuera)) write_clean(qa_anios_fuera, file.path(qa_dir, "16_det_ext_anios_fuera_rango.csv"))

E <- E |> filter(!is.na(ano), dplyr::between(ano, YEAR_MIN, YEAR_MAX))

qa_na <- E |> filter(is.na(valor) | is.na(tipo_raw) | is.na(sexo))
if (nrow(qa_na)) write_clean(qa_na, file.path(qa_dir, "16_det_ext_na.csv"))

# ---------- Agregación a Total Nacional (por sexo y tipo) ----------
excluir_regiones <- c("desconocida","no consta","no especificado","sin especificar",
                      "en el extranjero","extranjero","otros territorios","resto del mundo")

has_total_nacional <- any(E$region %in% c("total nacional","nacional","total"), na.rm = TRUE)

nat_sex <- if (has_total_nacional) {
  msg("Usando 'TOTAL NACIONAL' presente (detenciones extranjeros).")
  E |> filter(region %in% c("total nacional","nacional","total")) |>
    group_by(ano, tipo_key, tipo_raw, sexo) |>
    summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop")
} else {
  msg("No hay 'TOTAL NACIONAL' → sumando CCAA válidas (excluye desconocida/extranjero).")
  E |> filter(!region %in% excluir_regiones) |>
    group_by(ano, tipo_key, tipo_raw, sexo) |>
    summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop")
}

# ---------- Normalizar tipologías desde config/map_tipologias.csv (si existe) ----------
tipo_final <- nat_sex
if (file.exists(fp_map)) {
  map_tip <- readr::read_csv(fp_map, show_col_types = FALSE) |> janitor::clean_names()
  if (all(c("raw_tipo","propuesta_tipo") %in% names(map_tip))) {
    map_tip <- map_tip |> mutate(raw_tipo_key = normalize_tipo_key(raw_tipo))
    tipo_final <- nat_sex |>
      left_join(map_tip |> select(raw_tipo_key, propuesta_tipo), by = c("tipo_key" = "raw_tipo_key")) |>
      mutate(tipo = dplyr::coalesce(propuesta_tipo, tipo_raw)) |>
      select(ano, tipo, sexo, valor)
  } else {
    tipo_final <- nat_sex |> transmute(ano, tipo = tipo_raw, sexo, valor)
  }
} else {
  tipo_final <- nat_sex |> transmute(ano, tipo = tipo_raw, sexo, valor)
}

# ---------- Consolidación por claves (evita duplicados residuales) ----------
dups_keys <- tipo_final |> count(ano, tipo, sexo, name = "n") |> filter(n > 1)
if (nrow(dups_keys)) {
  write_clean(dups_keys, file.path(qa_dir, "16_det_ext_claves_duplicadas.csv"))
  tipo_final <- tipo_final |>
    group_by(ano, tipo, sexo) |>
    summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop")
}

# ---------- QA 2: negativos, duplicados, cobertura ----------
qa_neg <- tipo_final |> filter(valor < 0); if (nrow(qa_neg)) write_clean(qa_neg, file.path(qa_dir, "16_det_ext_negativos.csv"))
qa_dup <- tipo_final |> count(ano, tipo, sexo, name = "n") |> filter(n > 1); if (nrow(qa_dup)) write_clean(qa_dup, file.path(qa_dir, "16_det_ext_duplicados.csv"))

present <- sort(unique(tipo_final$ano)); missing <- setdiff(YEARS_SEQ, present)
qa_cov <- tibble(variable = "detenciones_extranjeros",
                 years_min = ifelse(length(present) > 0, min(present), NA_integer_),
                 years_max = ifelse(length(present) > 0, max(present), NA_integer_),
                 n_years = length(present), n_missing = length(missing),
                 missing_list = paste(missing, collapse = ", "))
write_clean(qa_cov, file.path(qa_dir, "16_det_ext_cobertura.csv"))

qa_cov_tipo <- tipo_final |> group_by(tipo) |>
  summarise(n_years = n_distinct(ano),
            complete = as.integer(n_years == length(YEARS_SEQ)),
            missing_list = paste(setdiff(YEARS_SEQ, sort(unique(ano))), collapse = ", "),
            .groups = "drop")
write_clean(qa_cov_tipo, file.path(qa_dir, "16_det_ext_cobertura_por_tipo.csv"))

# ---------- Coherencia sexo "total" vs suma de sexos ----------
nat_total_sex <- tipo_final |> group_by(ano, tipo) |> summarise(sum_sex = sum(valor, na.rm = TRUE), .groups = "drop")
nat_decl_total <- tipo_final |> filter(sexo == "total") |> select(ano, tipo, total_decl = valor)
qa_sex_cons <- nat_total_sex |>
  left_join(nat_decl_total, by = c("ano","tipo")) |>
  mutate(diff = total_decl - sum_sex,
         rel_diff = ifelse(sum_sex > 0, diff / sum_sex, NA_real_)) |>
  filter(!is.na(total_decl)) |>
  arrange(ano, tipo)
if (nrow(qa_sex_cons)) write_clean(qa_sex_cons, file.path(qa_dir, "16_det_ext_sexo_total_vs_suma.csv"))

# ---------- Salidas por sexo (documentado) ----------
final_sex_raw <- tipo_final |> arrange(ano, tipo, sexo) |> select(ano, tipo, sexo, valor)

# Diagnóstico: ¿suma(H+M) ≈ "total" declarado de forma estable (~1.5x)?
sex_sum <- tipo_final |>
  filter(sexo %in% c("hombres","mujeres")) |>
  group_by(ano) |>
  summarise(det_ext_sex_sum = sum(valor, na.rm = TRUE), .groups="drop")

decl_total <- tipo_final |>
  filter(sexo == "total") |>
  group_by(ano) |>
  summarise(det_ext_decl = sum(valor, na.rm = TRUE), .groups="drop")

diag_sex <- sex_sum |>
  inner_join(decl_total, by="ano") |>
  mutate(ratio_sex_vs_total = det_ext_sex_sum / det_ext_decl)
write_clean(diag_sex, file.path(qa_dir, "16_det_ext_diag_sexo_vs_total.csv"))

ratio_med <- suppressWarnings(median(diag_sex$ratio_sex_vs_total, na.rm=TRUE))
ratio_sd  <- suppressWarnings(sd(diag_sex$ratio_sex_vs_total, na.rm=TRUE))
metric_per_sexo <- if (is.finite(ratio_med) && abs(ratio_med - 1.5) < 0.1 &&
                       is.finite(ratio_sd)  && ratio_sd < 0.05) {
  msg(sprintf("⚠ Desglose por sexo parece otra métrica: suma(H+M) ≈ %.2fx total (probable detenidos+investigados).", ratio_med))
  "detenidos+investigados"
} else {
  "detenidos"
}

final_sex <- final_sex_raw |>
  mutate(metric = ifelse(sexo == "total", "detenidos", metric_per_sexo))
write_clean(final_sex, fp_out_sex)

# ---------- Agregada por tipo (usar 'total' si existe, si no H+M) ----------
nat_tipo <- if (any(tipo_final$sexo == "total")) {
  tipo_final |> filter(sexo == "total") |> select(ano, tipo, valor)
} else {
  tipo_final |> group_by(ano, tipo) |> summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop")
}
final_nat <- nat_tipo |> arrange(ano, tipo) |> select(ano, tipo, valor)
write_clean(final_nat, fp_out_nat)

# ---------- Serie total extranjeros (det_ext) y contraste con suma de tipologías ----------
norm_tipo <- function(x){ x |> norm_ascii() |> stringr::str_squish() }
lab <- norm_tipo(final_nat$tipo)
mask_total_label <- grepl("^total\\s*infracci", lab) | grepl("^total$", lab)

nat_totlabel <- final_nat |> filter(mask_total_label) |> transmute(ano, det_ext = valor)

nat_sum <- final_nat |> filter(!mask_total_label) |>
  group_by(ano) |>
  summarise(det_ext_sum = sum(valor, na.rm = TRUE), .groups = "drop")

det_ext <- if (nrow(nat_totlabel) > 0) {
  cmp <- nat_totlabel |>
    full_join(nat_sum, by = "ano") |>
    mutate(diff = det_ext - det_ext_sum,
           rel_diff = ifelse(det_ext_sum > 0, diff / det_ext_sum, NA_real_)) |>
    arrange(ano)
  write_clean(cmp, file.path(qa_dir, "16_det_ext_vs_suma.csv"))
  cmp |>
    transmute(ano,
              det_ext = dplyr::case_when(
                is.na(det_ext) ~ det_ext_sum,
                is.na(det_ext_sum) ~ det_ext,
                abs(rel_diff) <= 0.005 | abs(diff) <= 100 ~ det_ext,
                TRUE ~ det_ext_sum
              ))
} else {
  nat_sum |> transmute(ano, det_ext = det_ext_sum)
}

det_ext <- det_ext |>
  group_by(ano) |>
  summarise(det_ext = sum(det_ext, na.rm = TRUE), .groups = "drop") |>
  mutate(ano = as.integer(ano))
write_clean(det_ext |> arrange(ano), fp_out_tot)

# ---------- Estimación por sexo consistente con DETENIDOS ----------
sex_hm <- tipo_final |>
  filter(sexo %in% c("hombres","mujeres")) |>
  group_by(ano, sexo) |>
  summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop") |>
  group_by(ano) |>
  mutate(tot_hm = sum(valor, na.rm = TRUE),
         peso   = dplyr::if_else(tot_hm > 0, valor / tot_hm, NA_real_)) |>
  ungroup() |>
  select(ano, sexo, peso)

# Fallback seguro si falta uno de los sexos: 50/50 en ese año
sex_hm <- sex_hm |>
  group_by(ano) |>
  tidyr::complete(sexo = c("hombres","mujeres"), fill = list(peso = NA_real_)) |>
  mutate(peso = ifelse(all(is.na(peso)), 0.5, peso / sum(peso, na.rm = TRUE))) |>
  ungroup()

det_sex_est <- det_ext |>
  left_join(sex_hm, by = "ano") |>
  mutate(
    valor  = det_ext * peso,
    metric = "detenidos (estimado)",
    tipo   = "total infracciones penales"
  ) |>
  select(ano, tipo, sexo, valor, metric) |>
  arrange(ano, sexo)

fp_out_sex_est <- here::here("data","processed","detenciones_extranjeros_detenidos_por_sexo_est.csv")
write_clean(det_sex_est, fp_out_sex_est)
msg("✓ Estimación por sexo (DETENIDOS) → detenciones_extranjeros_detenidos_por_sexo_est.csv")

# ---------- Share extranjeros vs total ----------
DET_TOT <- readr::read_csv(fp_tot, show_col_types = FALSE) |>
  janitor::clean_names() |>
  mutate(ano = as.integer(ano))

share <- det_ext |>
  left_join(DET_TOT, by = "ano") |>
  mutate(share_extranjeros = ifelse(det_tot > 0, 100 * det_ext / det_tot, NA_real_)) |>
  select(ano, share_extranjeros)

# QC share y consistencia det_ext ≤ det_tot
qc_share <- share |> mutate(flag_fuera_0_100 = is.na(share_extranjeros) | share_extranjeros < 0 | share_extranjeros > 100)
qc_cons  <- det_ext |>
  left_join(DET_TOT, by = "ano") |>
  mutate(viol = det_ext > det_tot, diff = det_tot - det_ext) |>
  arrange(ano)

# Plausibilidad y YoY (det_ext)
df_pl <- det_ext |> arrange(ano) |> mutate(yoy = det_ext / dplyr::lag(det_ext) - 1)
qc_pl <- tibble(
  min = suppressWarnings(min(df_pl$det_ext, na.rm = TRUE)),
  max = suppressWarnings(max(df_pl$det_ext, na.rm = TRUE)),
  plaus_ok = (min >= 1e4 & max <= 6e5),
  yoy_max_abs = suppressWarnings(max(abs(df_pl$yoy), na.rm = TRUE)),
  yoy_breaches = sum(abs(df_pl$yoy) > 0.40, na.rm = TRUE)
)

write_clean(share |> arrange(ano), fp_out_share)
write_clean(qc_share, file.path(qa_dir, "16_det_ext_share_qc.csv"))
write_clean(qc_cons,  file.path(qa_dir, "16_det_ext_consistencia.csv"))
write_clean(qc_pl,    file.path(qa_dir, "16_det_ext_plaus_yoy.csv"))

msg("✔ Detenciones de extranjeros (por sexo documentado, agregado, total, estimación por sexo y share) limpias y guardadas.")
