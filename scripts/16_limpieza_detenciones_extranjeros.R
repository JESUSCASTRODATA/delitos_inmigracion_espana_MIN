#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
###############################################################################
# 16_limpieza_detenciones_extranjeros.R — Nivel Nacional (2010–2023)
# Proyecto: Delitos e Inmigración en España (2010–2023)
# Autor: Jesús Castro (Analista de Datos)
# Fecha: 2025-08-23 | Versión: 1.0 (alineado con 00_utils_limpieza.R del 2025-08-22)
#
# Entradas
#   - data/raw/detenciones_extranjeros_tipologia.csv
#   - data/processed/detenciones_totales_total.csv (para share)
#   - (opcional) config/map_tipologias.csv (normalización de etiquetas)
# Dependencias
#   - R/00_utils_limpieza.R → root_init(), assert_infile(), read_raw(),
#     write_clean(), safe_parse_number(), detect_year_col(), extract_year()
# Salidas
#   - data/processed/detenciones_extranjeros_total_nacional_por_sexo.csv (ano, tipo, sexo, valor)
#   - data/processed/detenciones_extranjeros_total_nacional.csv (ano, tipo, valor)
#   - data/processed/detenciones_extranjeros_total.csv (ano, det_ext)
#   - data/processed/detenciones_share_extranjeros.csv (ano, share_extranjeros)
# QA
#   - share_extranjeros ∈ [0,100]
#   - cobertura temporal y duplicados/negativos
#   - consistencia: det_ext ≤ det_tot
###############################################################################

suppressPackageStartupMessages({
  library(here)
  library(readr)
  library(dplyr)
  library(tidyr)
  library(janitor)
  library(stringr)
  library(purrr)
})

# ————————————————————————————————————————————————————————————————
# 0) Cargar utilidades comunes
# ————————————————————————————————————————————————————————————————
source_if <- function(path) { if (file.exists(path)) source(path, local = TRUE) }
source_if(here::here("R", "00_utils_limpieza.R"))
source_if(here::here("scripts", "00_utils_limpieza.R"))

# Fallbacks mínimos
if (!exists("abort")) abort <- function(...) stop(paste0(...), call. = FALSE)
if (!exists("root_init")) root_init <- function(caller = NULL, project_root_hint = NULL){
  msg <- if (!is.null(caller)) paste0("ROOT:", caller) else "ROOT:16_limpieza_detenciones_extranjeros"
  message(msg, " -> ", getwd()); invisible(getwd())
}
if (!exists("assert_infile")) assert_infile <- function(path){ if (!file.exists(path)) abort("No existe: ", path) }
if (!exists("write_clean")) write_clean <- function(df, path, na = ""){ dir.create(dirname(path), TRUE, TRUE); readr::write_csv(df, path, na = na); message("Escrito: ", normalizePath(path, winslash = "/")); invisible(path) }
if (!exists("read_raw")) read_raw <- function(path){ readr::read_delim(path, delim = ";", col_types = readr::cols(.default = readr::col_character()), show_col_types = FALSE) |> janitor::clean_names() }
if (!exists("safe_parse_number")) safe_parse_number <- function(x){ readr::parse_number(as.character(x)) }
if (!exists("detect_year_col")) detect_year_col <- function(nms){
  cand <- c("periodo", "período", "ano", "año", "year", "time", "time_period", "TIME_PERIOD")
  ix <- which(tolower(nms) %in% tolower(cand))
  if (length(ix)) nms[ix[1]] else NA_character_
}
if (!exists("extract_year")) extract_year <- function(x){ y <- stringr::str_extract(as.character(x), "[0-9]{4}"); suppressWarnings(as.integer(y)) }

root_init()

YEAR_MIN <- 2010L
YEAR_MAX <- 2023L
YEARS_SEQ <- YEAR_MIN:YEAR_MAX

# ————————————————————————————————————————————————————————————————
# Rutas
# ————————————————————————————————————————————————————————————————
if (requireNamespace("here", quietly = TRUE)) {
  here_path <- function(...) here::here(...)
} else {
  base_root <- getwd()
  message("Paquete 'here' no disponible; usando getwd(): ", base_root)
  here_path <- function(...) file.path(base_root, ...)
}

fp_in   <- here_path("data", "raw", "detenciones_extranjeros_tipologia.csv")
fp_tot  <- here_path("data", "processed", "detenciones_totales_total.csv")
fp_map  <- here_path("config", "map_tipologias.csv")
fp_out_sex <- here_path("data", "processed", "detenciones_extranjeros_total_nacional_por_sexo.csv")
fp_out_nat <- here_path("data", "processed", "detenciones_extranjeros_total_nacional.csv")
fp_out_tot <- here_path("data", "processed", "detenciones_extranjeros_total.csv")
fp_out_share <- here_path("data", "processed", "detenciones_share_extranjeros.csv")
qa_dir  <- here_path("output", "tables")
dir.create(qa_dir, recursive = TRUE, showWarnings = FALSE)

assert_infile(fp_in)
assert_infile(fp_tot)

# ————————————————————————————————————————————————————————————————
# 1) Lectura y normalización de columnas
# ————————————————————————————————————————————————————————————————
raw <- read_raw(fp_in)
cn <- names(raw)
cn_low <- tolower(cn)

pick_col <- function(patterns, not = NULL) {
  hit <- purrr::map_lgl(cn_low, ~ any(stringr::str_detect(.x, patterns)))
  if (!is.null(not)) hit <- hit & !stringr::str_detect(cn_low, not)
  col <- cn[which(hit)][1]
  ifelse(is.na(col) || length(col)==0, NA_character_, col)
}

col_region  <- pick_col(c("comun", "ccaa", "autono", "territ", "ambito", "ámbito", "region"))
col_tipo    <- pick_col(c("tipolog", "delit", "infracc", "tipo"))
col_sexo    <- pick_col(c("sexo", "sex"))
col_periodo <- detect_year_col(cn); if (is.na(col_periodo)) col_periodo <- pick_col(c("period", "anio", "año", "ano", "fecha", "year", "time"))
col_valor   <- pick_col(c("^total$", "^valor$", "hechos", "conteo", "numero", "n[0-9]*$", "detenc"))

if (is.na(col_tipo) || is.na(col_periodo) || is.na(col_valor)) {
  abort(paste0(
    "❌ Columnas requeridas no detectadas. Encontradas→ ",
    "region:", ifelse(is.na(col_region), "<NA>", col_region),
    " | tipo:",   ifelse(is.na(col_tipo),   "<NA>", col_tipo),
    " | sexo:",   ifelse(is.na(col_sexo),   "<NA>", col_sexo),
    " | periodo:",ifelse(is.na(col_periodo),"<NA>", col_periodo),
    " | valor:",  ifelse(is.na(col_valor),  "<NA>", col_valor)
  ))
}

normalize_text <- function(x) {
  x <- stringr::str_trim(tolower(as.character(x)))
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  x <- stringr::str_squish(x)
  x
}

normalize_sexo <- function(x) {
  z <- normalize_text(x)
  z <- dplyr::case_when(
    z %in% c("h", "hombre", "hombres", "varon", "varones", "masculino") ~ "hombres",
    z %in% c("m", "mujer", "mujeres", "femenino") ~ "mujeres",
    z %in% c("ambos", "ambos sexos", "total", "total ambos", "total sexo", "todos") ~ "total",
    TRUE ~ z
  )
  z
}

E <- raw |>
  transmute(
    region  = if (is.na(col_region)) "TOTAL NACIONAL" else .data[[col_region]],
    tipo    = as.character(.data[[col_tipo]]),
    sexo    = if (is.na(col_sexo)) "total" else as.character(.data[[col_sexo]]),
    periodo = .data[[col_periodo]],
    valor   = safe_parse_number(.data[[col_valor]])
  ) |>
  mutate(
    ano = extract_year(periodo),
    region_norm = normalize_text(region),
    sexo_norm = normalize_sexo(sexo)
  ) |>
  select(ano, region = region_norm, tipo, sexo = sexo_norm, valor)

# ————————————————————————————————————————————————————————————————
# 2) QA básicos
# ————————————————————————————————————————————————————————————————
qa_anios_fuera <- E |>
  mutate(flag_fuera = is.na(ano) | ano < YEAR_MIN | ano > YEAR_MAX) |>
  filter(flag_fuera) |>
  arrange(ano)
if (nrow(qa_anios_fuera) > 0) write_clean(qa_anios_fuera, file.path(qa_dir, "qa_det_ext_anios_fuera_rango.csv"))

E <- E |>
  filter(!is.na(ano), dplyr::between(ano, YEAR_MIN, YEAR_MAX))

qa_na <- E |>
  filter(is.na(valor) | is.na(tipo) | is.na(sexo))
if (nrow(qa_na) > 0) write_clean(qa_na, file.path(qa_dir, "qa_det_ext_na.csv"))

# ————————————————————————————————————————————————————————————————
# 3) Agregación a TOTAL NACIONAL (por sexo y tipo)
# ————————————————————————————————————————————————————————————————
excluir_regiones <- c(
  "desconocida", "no consta", "no especificado", "sin especificar",
  "en el extranjero", "extranjero", "otros territorios", "resto del mundo"
)

has_total_nacional <- any(E$region %in% c("total nacional", "nacional", "total"), na.rm = TRUE)

nat_sex <- if (has_total_nacional) {
  message("✓ Usando 'TOTAL NACIONAL' presente (detenciones extranjeros).")
  E |>
    filter(region %in% c("total nacional", "nacional", "total")) |>
    group_by(ano, tipo, sexo) |>
    summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop")
} else {
  message("ℹ No hay 'TOTAL NACIONAL': se suman CCAA válidas (excluye desconocida/extranjero).")
  E |>
    filter(!region %in% excluir_regiones) |>
    group_by(ano, tipo, sexo) |>
    summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop")
}

# ————————————————————————————————————————————————————————————————
# 4) (Opcional) Normalizar tipologías vía config/map_tipologias.csv
# ————————————————————————————————————————————————————————————————
if (file.exists(fp_map)) {
  map_tip <- readr::read_csv(fp_map, show_col_types = FALSE) |> janitor::clean_names()
  if (all(c("raw_tipo","propuesta_tipo") %in% names(map_tip))) {
    nat_sex <- nat_sex |>
      left_join(map_tip |> select(raw_tipo, propuesta_tipo), by = c("tipo" = "raw_tipo")) |>
      mutate(tipo = dplyr::coalesce(propuesta_tipo, tipo)) |>
      select(-propuesta_tipo)
  }
}

# ————————————————————————————————————————————————————————————————
# 5) QA adicionales
# ————————————————————————————————————————————————————————————————
qa_neg <- nat_sex |> filter(valor < 0)
if (nrow(qa_neg) > 0) write_clean(qa_neg, file.path(qa_dir, "qa_det_ext_negativos.csv"))

qa_dup <- nat_sex |> count(ano, tipo, sexo, name = "n") |> filter(n > 1)
if (nrow(qa_dup) > 0) write_clean(qa_dup, file.path(qa_dir, "qa_det_ext_duplicados.csv"))

present <- sort(unique(nat_sex$ano)); missing <- setdiff(YEARS_SEQ, present)
qa_cov <- tibble::tibble(
  variable = "detenciones_extranjeros",
  years_min = ifelse(length(present) > 0, min(present), NA_integer_),
  years_max = ifelse(length(present) > 0, max(present), NA_integer_),
  n_years = length(present),
  n_missing = length(missing),
  missing_list = paste(missing, collapse = ", ")
)
write_clean(qa_cov, file.path(qa_dir, "qa_det_ext_cobertura.csv"))

# Coherencia sexo total vs suma sexos
nat_total_sex <- nat_sex |>
  group_by(ano, tipo) |>
  summarise(sum_sex = sum(valor, na.rm = TRUE), .groups = "drop")

nat_declared_total <- nat_sex |> filter(sexo == "total") |> select(ano, tipo, total_decl = valor)

qa_sex_cons <- nat_total_sex |>
  left_join(nat_declared_total, by = c("ano","tipo")) |>
  mutate(diff = total_decl - sum_sex,
         rel_diff = ifelse(sum_sex > 0, diff / sum_sex, NA_real_)) |>
  filter(!is.na(total_decl)) |>
  arrange(ano, tipo)
if (nrow(qa_sex_cons) > 0) write_clean(qa_sex_cons, file.path(qa_dir, "qa_det_ext_sexo_total_vs_suma.csv"))

# ————————————————————————————————————————————————————————————————
# 6) Salidas por sexo y agregada por tipo
# ————————————————————————————————————————————————————————————————
# a) Por sexo
final_sex <- nat_sex |> arrange(ano, tipo, sexo) |> select(ano, tipo, sexo, valor)
write_clean(final_sex, fp_out_sex)

# b) Sexo agregado por tipo (prefiere filas con sexo total; si no existen, suma sexos)
if (any(nat_sex$sexo == "total")) {
  nat_tipo <- nat_sex |> filter(sexo == "total") |> select(ano, tipo, valor)
} else {
  nat_tipo <- nat_sex |> group_by(ano, tipo) |> summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop")
}

final_nat <- nat_tipo |> arrange(ano, tipo) |> select(ano, tipo, valor)
write_clean(final_nat, fp_out_nat)

# ————————————————————————————————————————————————————————————————
# 7) Serie total de extranjeros (det_ext)
# ————————————————————————————————————————————————————————————————
# Detectar etiqueta de total por patrón robusto
norm_tipo <- normalize_text(final_nat$tipo)
mask_total <- grepl("^total\\s*infracci", norm_tipo) | grepl("^total$", norm_tipo)

nat_totlabel <- final_nat |> filter(mask_total) |> transmute(ano, det_ext = valor)

nat_sum <- final_nat |>
  filter(!mask_total) |>
  group_by(ano) |>
  summarise(det_ext_sum = sum(valor, na.rm = TRUE), .groups = "drop")

det_ext <- if (nrow(nat_totlabel) > 0) {
  nat_totlabel
} else {
  nat_sum |> transmute(ano, det_ext = det_ext_sum)
}

write_clean(det_ext |> arrange(ano), fp_out_tot)

# ————————————————————————————————————————————————————————————————
# 8) Share de extranjeros vs total
# ————————————————————————————————————————————————————————————————
DET_TOT <- readr::read_csv(fp_tot, show_col_types = FALSE) |> janitor::clean_names() |>
  mutate(ano = as.integer(ano))
stopifnot(all(c("ano","det_tot") %in% names(DET_TOT)))

share <- det_ext |>
  mutate(ano = as.integer(ano)) |>
  left_join(DET_TOT, by = "ano") |>
  mutate(share_extranjeros = ifelse(det_tot > 0, 100 * det_ext / det_tot, NA_real_)) |>
  select(ano, share_extranjeros)

# QA share en [0,100] y consistencia det_ext ≤ det_tot
qa_share <- share |>
  mutate(flag_fuera = is.na(share_extranjeros) | share_extranjeros < 0 | share_extranjeros > 100)
if (any(qa_share$flag_fuera)) write_clean(qa_share, file.path(qa_dir, "qa_share_extranjeros_fuera_0_100.csv"))

qa_cons <- det_ext |>
  left_join(DET_TOT, by = "ano") |>
  mutate(diff = det_tot - det_ext, viol = det_ext > det_tot) |>
  arrange(ano)
if (any(qa_cons$viol, na.rm = TRUE)) write_clean(qa_cons, file.path(qa_dir, "qa_det_ext_mayor_que_tot.csv"))

write_clean(share |> arrange(ano), fp_out_share)

message("✔ Detenciones de extranjeros (por sexo, agregada y share) limpias y guardadas.")
