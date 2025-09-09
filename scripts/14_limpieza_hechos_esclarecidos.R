#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
###############################################################################
# 14_limpieza_hechos_esclarecidos.R — Nivel Nacional (2010–2023)
# Proyecto: Delitos e Inmigración en España (2010–2023)
# Autor: Jesús Castro (Analista de Datos)
# Fecha: 2025-08-23 | Versión: 1.0 (alineado con 00_utils_limpieza.R del 2025-08-22)
#
# Entradas
#   - data/raw/hechos_esclarecidos.csv
#   - data/processed/hechos_conocidos_total_nacional.csv (para chequeo HE ≤ HC)
#   - config/qa_excepciones_he_vs_hc.csv (opcional, whitelist)
# Dependencias
#   - R/00_utils_limpieza.R → root_init(), assert_infile(), read_raw(),
#     write_clean(), safe_parse_number(), detect_year_col(), extract_year(),
#     join_with_log()
# Salidas
#   - data/processed/hechos_esclarecidos_total_nacional.csv (ano, tipo, valor)
#   - output/tables/qa_he_cobertura.csv
#   - output/tables/qa_he_cobertura_por_tipo.csv
#   - output/tables/qa_he_anios_fuera_rango.csv (si aplica)
#   - output/tables/qa_he_negativos.csv (si aplica)
#   - output/tables/qa_he_na.csv (si aplica)
#   - output/tables/qa_he_duplicados.csv (si aplica)
#   - output/tables/qa_coherencia_he_mayor_que_hc.csv (violaciones HE>HC fuera del whitelist)
#   - output/tables/qa_he_sin_match_con_hc.csv (tipos/años sin match en HC)
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

# Fallbacks mínimos si no se cargó el utilitario
if (!exists("abort")) abort <- function(...) stop(paste0(...), call. = FALSE)
if (!exists("root_init")) root_init <- function(caller = NULL, project_root_hint = NULL){
  msg <- if (!is.null(caller)) paste0("ROOT:", caller) else "ROOT:14_limpieza_hechos_esclarecidos"
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
if (!exists("join_with_log")) join_with_log <- function(x, y, by = c("ano","tipo"), mode = c("left","inner")){
  mode <- match.arg(mode)
  miss_in_y <- dplyr::anti_join(x, y, by = by)
  miss_in_x <- dplyr::anti_join(y, x, by = by)
  if (nrow(miss_in_y)) message("[JOIN] claves de X sin match en Y por ", paste(by, collapse=","), ": ", paste(utils::head(apply(miss_in_y[,by,drop=FALSE],1,paste,collapse="/"),5), collapse = ", "), if (nrow(miss_in_y) > 5) " ...")
  if (nrow(miss_in_x)) message("[JOIN] claves de Y sin match en X por ", paste(by, collapse=","), ": ", paste(utils::head(apply(miss_in_x[,by,drop=FALSE],1,paste,collapse="/"),5), collapse = ", "), if (nrow(miss_in_x) > 5) " ...")
  if (mode == "left") dplyr::left_join(x, y, by = by) else dplyr::inner_join(x, y, by = by)
}

# Inicializa raíz
root_init()

YEAR_MIN <- 2010L
YEAR_MAX <- 2023L
YEARS_SEQ <- YEAR_MIN:YEAR_MAX
REL_TOL <- 0        # tolerancia relativa para HE≤HC (0 por ser conteos enteros)
ABS_TOL <- 0        # tolerancia absoluta en unidades

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

fp_in   <- here_path("data", "raw", "hechos_esclarecidos.csv")
fp_hc   <- here_path("data", "processed", "hechos_conocidos_total_nacional.csv")
fp_out  <- here_path("data", "processed", "hechos_esclarecidos_total_nacional.csv")
fp_wl   <- here_path("config", "qa_excepciones_he_vs_hc.csv")
qa_dir  <- here_path("output", "tables")
dir.create(qa_dir, recursive = TRUE, showWarnings = FALSE)

assert_infile(fp_in)
assert_infile(fp_hc)

# ————————————————————————————————————————————————————————————————
# 1) Lectura y normalización de columnas (HE)
# ————————————————————————————————————————————————————————————————
raw <- read_raw(fp_in)
cn <- names(raw)
cn_low <- tolower(cn)

pick_col <- function(patterns, not = NULL) {
  hit <- map_lgl(cn_low, ~ any(stringr::str_detect(.x, patterns)))
  if (!is.null(not)) hit <- hit & !stringr::str_detect(cn_low, not)
  col <- cn[which(hit)][1]
  ifelse(is.na(col) || length(col)==0, NA_character_, col)
}

col_region  <- pick_col(c("comun", "ccaa", "autono", "territ", "ambito", "ámbito", "region"))
col_tipo    <- pick_col(c("tipolog", "delit", "infracc"))
col_periodo <- detect_year_col(cn); if (is.na(col_periodo)) col_periodo <- pick_col(c("period", "anio", "año", "ano", "fecha", "year", "time"))
col_valor   <- pick_col(c("^total$", "^valor$", "hechos", "conteo", "numero", "n[0-9]*$"))

if (any(is.na(c(col_region, col_tipo, col_periodo, col_valor)))) {
  abort(paste0(
    "❌ Columnas requeridas no detectadas (HE). Encontradas→ ",
    "region:", ifelse(is.na(col_region), "NA", col_region),
    " | tipo:", ifelse(is.na(col_tipo), "NA", col_tipo),
    " | periodo:", ifelse(is.na(col_periodo), "NA", col_periodo),
    " | valor:", ifelse(is.na(col_valor), "NA", col_valor)
  ))
}

he <- raw |>
  transmute(
    region  = .data[[col_region]],
    tipo    = as.character(.data[[col_tipo]]),
    periodo = .data[[col_periodo]],
    valor   = safe_parse_number(.data[[col_valor]])
  ) |>
  mutate(ano = extract_year(periodo)) |>
  select(ano, region, tipo, valor)

# ————————————————————————————————————————————————————————————————
# 2) QA básicos en HE
# ————————————————————————————————————————————————————————————————
qa_anios_fuera <- he |>
  mutate(flag_fuera = is.na(ano) | ano < YEAR_MIN | ano > YEAR_MAX) |>
  filter(flag_fuera) |>
  arrange(ano)
if (nrow(qa_anios_fuera) > 0) write_clean(qa_anios_fuera, file.path(qa_dir, "qa_he_anios_fuera_rango.csv"))

he <- he |>
  filter(!is.na(ano), dplyr::between(ano, YEAR_MIN, YEAR_MAX))

qa_na <- he |>
  filter(is.na(valor) | is.na(tipo))
if (nrow(qa_na) > 0) write_clean(qa_na, file.path(qa_dir, "qa_he_na.csv"))

# ————————————————————————————————————————————————————————————————
# 3) Agregación HE a TOTAL NACIONAL
# ————————————————————————————————————————————————————————————————
normalize_text <- function(x) {
  x <- stringr::str_trim(tolower(as.character(x)))
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  x <- stringr::str_squish(x)
  x
}

he <- he |> mutate(region_norm = normalize_text(region))

excluir_regiones <- c(
  "desconocida", "no consta", "no especificado", "sin especificar",
  "en el extranjero", "extranjero", "otros territorios", "resto del mundo"
)

has_total_nacional <- any(he$region_norm %in% c("total nacional", "nacional", "total"), na.rm = TRUE)

nat_he <- if (has_total_nacional) {
  message("✓ Usando 'TOTAL NACIONAL' presente en el fichero (hechos esclarecidos).")
  he |>
    filter(region_norm %in% c("total nacional", "nacional", "total")) |>
    group_by(ano, tipo) |>
    summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop")
} else {
  message("ℹ No hay 'TOTAL NACIONAL' en HE: se suman CCAA válidas (excluye desconocida/extranjero).")
  he |>
    filter(!region_norm %in% excluir_regiones) |>
    group_by(ano, tipo) |>
    summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop")
}

# QA: negativos, duplicados, cobertura HE
qa_neg <- nat_he |> filter(valor < 0)
if (nrow(qa_neg) > 0) write_clean(qa_neg, file.path(qa_dir, "qa_he_negativos.csv"))

qa_dup <- nat_he |> count(ano, tipo, name = "n") |> filter(n > 1)
if (nrow(qa_dup) > 0) write_clean(qa_dup, file.path(qa_dir, "qa_he_duplicados.csv"))

present <- sort(unique(nat_he$ano))
missing <- setdiff(YEARS_SEQ, present)
qa_cov <- tibble::tibble(
  variable = "hechos_esclarecidos",
  years_min = ifelse(length(present) > 0, min(present), NA_integer_),
  years_max = ifelse(length(present) > 0, max(present), NA_integer_),
  n_years = length(present),
  n_missing = length(missing),
  missing_list = paste(missing, collapse = ", ")
)
write_clean(qa_cov, file.path(qa_dir, "qa_he_cobertura.csv"))

qa_cov_tipo <- nat_he |>
  group_by(tipo) |>
  summarise(
    n_years = n_distinct(ano),
    complete = as.integer(n_years == length(YEARS_SEQ)),
    missing_list = paste(setdiff(YEARS_SEQ, sort(unique(ano))), collapse = ", ")
  ) |>
  ungroup()
write_clean(qa_cov_tipo, file.path(qa_dir, "qa_he_cobertura_por_tipo.csv"))

# ————————————————————————————————————————————————————————————————
# 4) Chequeo HE ≤ HC (con whitelist)
# ————————————————————————————————————————————————————————————————
# Coerciones de tipos para joins robustos
nat_he <- nat_he |> mutate(ano = as.integer(ano), tipo = as.character(tipo))

# Cargar HC procesado y forzar tipos compatibles
hc_nat <- readr::read_csv(fp_hc, show_col_types = FALSE) |> janitor::clean_names() |>
  mutate(ano = as.integer(ano), tipo = as.character(tipo))
stopifnot(all(c("ano","tipo","valor") %in% names(hc_nat)))

# Join HE con HC
cmp <- join_with_log(
  nat_he |> rename(hechos_esclarecidos = valor),
  hc_nat |> rename(hechos_conocidos = valor),
  by = c("ano","tipo"), mode = "left"
)

# Log de filas HE sin match en HC
sin_match_hc <- cmp |> filter(is.na(hechos_conocidos)) |> select(ano, tipo, hechos_esclarecidos)
if (nrow(sin_match_hc) > 0) write_clean(sin_match_hc, file.path(qa_dir, "qa_he_sin_match_con_hc.csv"))

# Diagnóstico extendido HE vs HC (incluye posibles desfases temporales)
cmp_diag <- cmp |>
  arrange(tipo, ano) |>
  group_by(tipo) |>
  mutate(hc_lag1 = dplyr::lag(hechos_conocidos)) |>
  ungroup() |>
  mutate(
    exceso = hechos_esclarecidos - hechos_conocidos,
    exceso_pct = ifelse(hechos_conocidos > 0, exceso / hechos_conocidos, NA_real_),
    ok_por_desfase_1y = !is.na(hc_lag1) & (hechos_esclarecidos <= (hechos_conocidos + hc_lag1)),
    severidad = dplyr::case_when(
      is.na(hechos_conocidos) ~ "sin_match_hc",
      exceso <= 0 ~ "ok",
      exceso_pct <= 0.05 ~ "leve (<=5%)",
      exceso_pct <= 0.20 ~ "media (5-20%)",
      TRUE ~ "alta (>20%)"
    )
  )
write_clean(
  cmp_diag |> select(ano, tipo, hechos_conocidos, hechos_esclarecidos, exceso, exceso_pct, hc_lag1, ok_por_desfase_1y, severidad),
  file.path(qa_dir, "qa_he_vs_hc_diagnostico.csv")
)

# Sugerencia de whitelist automática: casos con exceso>0 pero compatibles con desfase 1 año
sugerencia_wl <- cmp_diag |>
  filter(exceso > 0, ok_por_desfase_1y | (!is.na(exceso_pct) & exceso_pct <= 0.05)) |>
  select(ano, tipo) |>
  distinct()
if (nrow(sugerencia_wl) > 0) write_clean(sugerencia_wl, file.path(qa_dir, "qa_sugerencia_whitelist.csv"))

# Violaciones HE > HC (estrictas, sin tolerancia) para reporte principal
viol <- cmp_diag |>
  filter(!is.na(hechos_conocidos)) |>
  mutate(
    umbral = pmax(hechos_conocidos * (1 + REL_TOL), hechos_conocidos + ABS_TOL),
    violacion = hechos_esclarecidos > umbral
  ) |>
  filter(violacion) |>
  arrange(ano, tipo) |>
  transmute(ano, tipo, hechos_conocidos, hechos_esclarecidos, ratio = ifelse(hechos_conocidos > 0, hechos_esclarecidos / hechos_conocidos, NA_real_))

# Aplicar whitelist si existe (coerción de tipos: ano→integer, tipo→character)
wl <- try(readr::read_csv(fp_wl, show_col_types = FALSE) |> janitor::clean_names(), silent = TRUE)
if (inherits(wl, "try-error") || !all(c("ano","tipo") %in% names(wl))) {
  wl <- tibble::tibble(ano = integer(), tipo = character())
} else {
  wl <- wl |>
    mutate(ano = suppressWarnings(as.integer(extract_year(ano))),
           tipo = as.character(tipo)) |>
    filter(!is.na(ano), nzchar(tipo)) |>
    distinct(ano, tipo)
}
viol_final <- dplyr::anti_join(viol, wl |> dplyr::select(ano, tipo), by = c("ano","tipo"))

if (nrow(viol_final) > 0) {
  write_clean(viol_final, file.path(qa_dir, "qa_coherencia_he_mayor_que_hc.csv"))
  message("⚠️  HE > HC detectado fuera del whitelist. Ver output/tables/qa_coherencia_he_mayor_que_hc.csv")
} else {
  message("✓ Coherencia HE ≤ HC (tras whitelist)")
}

# ————————————————————————————————————————————————————————————————
# 5) Escribir salida HE total nacional
# ————————————————————————————————————————————————————————————————
final_he <- nat_he |> arrange(ano, tipo) |> select(ano, tipo, valor)
write_clean(final_he, fp_out)
message("✔ Hechos esclarecidos (TOTAL NACIONAL) limpios y guardados.")

