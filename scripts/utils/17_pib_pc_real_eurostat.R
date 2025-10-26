#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
###############################################################################
# 17_pib_pc_real_eurostat.R — PIB per cápita real (España) 2010–2023
# Proyecto: Delitos e Inmigración en España (2010–2023)
# Autor: Jesús Castro (Analista de Datos)
#
# IN (data/raw/):
#   - Preferente: pib.csv  (SDMX-CSV: unit, na_item, geo, time|TIME_PERIOD, value|OBS_VALUE)
#   - Alternativa: estat_nama_10_pc_filtered_en.csv o cualquier nama_10_pc*.csv
#
# OUT (data/processed/):
#   - pib_pc_real_es.csv  (ano, pib_pc_real) — unidad CLV*_EUR_HAB (volumen encadenado, €/hab)
#
# QA (output/tables/):
#   - 17_pib_qa_filtros.csv         (conteo por unit/na_item/geo)
#   - 17_pib_qa_unidades_dispon.csv (todas las units detectadas)
#   - 17_pib_qa_anios_fuera.csv     (si aplica)
#   - 17_pib_qa_cobertura.csv
#   - 17_pib_qa_duplicados.csv      (si aplica)
#
# Flags (ENV):
#   YEAR_MIN=2010  YEAR_MAX=2023
#   ALLOW_PPS_FALLBACK=false  # si no hay CLV*_EUR_HAB, permite PPS_HAB (lo deja anotado en QA)
###############################################################################

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(tidyr); library(janitor)
  library(stringr); library(here); library(tibble); library(fs)
})

# Utils del proyecto (lectura/normalización/trazas homogéneas)
source(here::here("scripts","00_utils_limpieza.R"))
root_init("scripts/17_pib_pc_real_eurostat.R")

msg  <- function(...) message("[17PIB] ", paste0(...))
abort <- function(...) stop(paste0(...), call. = FALSE)

YEAR_MIN <- as.integer(Sys.getenv("YEAR_MIN","2010"))
YEAR_MAX <- as.integer(Sys.getenv("YEAR_MAX","2023"))
YEARS_SEQ <- YEAR_MIN:YEAR_MAX
ALLOW_PPS_FALLBACK <- tolower(Sys.getenv("ALLOW_PPS_FALLBACK","false")) %in% c("1","true","yes","y")

# -------------------- Local helpers --------------------
detect_delim <- function(path, n = 2000){
  enc <- "UTF-8"
  lines <- tryCatch(readr::read_lines(path, n_max = n, locale = locale(encoding = enc)),
                    error = function(e) readr::read_lines(path, n_max = n, locale = locale(encoding = "Latin1")))
  if (!length(lines)) return(",")
  counts <- c(`;`=sum(str_count(lines,";")), `,`=sum(str_count(lines,",")), `\t`=sum(str_count(lines,"\t")))
  names(counts)[which.max(counts)]
}
detect_encoding <- function(path){
  ge <- tryCatch(readr::guess_encoding(path, n_max = 50000), error = function(e) NULL)
  if (is.null(ge) || nrow(ge)==0) "UTF-8" else ge$encoding[1]
}
read_raw_any <- function(path){
  delim <- detect_delim(path); enc <- detect_encoding(path)
  msg(sprintf("Leyendo '%s' (sep='%s', enc='%s') …", fs::path_file(path), delim, enc))
  readr::read_delim(path, delim = delim, locale = locale(encoding = enc),
                    show_col_types = FALSE, trim_ws = TRUE, guess_max = 200000) |>
    janitor::clean_names()
}
extract_year2 <- function(x){ suppressWarnings(as.integer(stringr::str_extract(as.character(x), "(19|20)[0-9]{2}"))) }

# -------------------- Rutas --------------------
in_candidates <- c(
  here::here("data","raw","pib.csv"),
  here::here("data","raw","estat_nama_10_pc_filtered_en.csv")
)
alt <- list.files(here::here("data","raw"), pattern = "nama_10_pc.*\\.csv$", full.names = TRUE)
if (length(alt)) in_candidates <- c(in_candidates, alt)

fp_in  <- in_candidates[file.exists(in_candidates)][1]
if (is.na(fp_in)) abort("No se encontró un fichero de PIB válido en data/raw/ (pib.csv / nama_10_pc*.csv).")

fp_out <- here::here("data","processed","pib_pc_real_es.csv")
qa_dir <- here::here("output","tables"); dir_create(qa_dir, recurse = TRUE)

# -------------------- Lectura & shape --------------------
raw0 <- read_raw_any(fp_in)
nms  <- names(raw0)

# Detecta formato “wide” (años como columnas) y pivota
year_cols <- nms[grepl("^(19|20)[0-9]{2}$", nms)]
has_value_col <- any(tolower(nms) %in% c("value","values","obs_value","obsvalue","v"))
if (length(year_cols) >= 5 && !has_value_col) {
  raw <- raw0 |>
    pivot_longer(cols = all_of(year_cols), names_to = "time", values_to = "value") |>
    clean_names()
} else {
  raw <- raw0
}
nms <- names(raw)  # ← recalcular tras pivot

# Columnas clave (tolerante a variantes SDMX)
col_unit   <- nms[grepl("^unit$", nms, ignore.case = TRUE)][1]         %||% "unit"
col_naitem <- nms[grepl("^na_?item$", nms, ignore.case = TRUE)][1]     %||% "na_item"
col_geo    <- nms[grepl("^geo$", nms, ignore.case = TRUE)][1]          %||% "geo"
col_time   <- nms[grepl("^time(_period)?$", nms, ignore.case = TRUE)][1] %||%
  nms[grepl("time", nms, ignore.case = TRUE)][1]
col_value_l <- intersect(tolower(c("value","values","obs_value","obsvalue","v")), tolower(nms))[1]
col_value   <- if (!is.na(col_value_l)) nms[tolower(nms)==col_value_l] else NA_character_

if (any(is.na(c(col_unit,col_naitem,col_geo,col_time,col_value))))
  abort(sprintf("❌ Columnas no detectadas (unit=%s, na_item=%s, geo=%s, time=%s, value=%s)",
                col_unit,col_naitem,col_geo,col_time,col_value))

df <- raw |>
  transmute(
    unit    = .data[[col_unit]],
    na_item = .data[[col_naitem]],
    geo     = .data[[col_geo]],
    time    = .data[[col_time]],
    value   = .data[[col_value]]
  ) |>
  mutate(
    unit  = as.character(unit),
    na_item = as.character(na_item),
    geo   = as.character(geo),
    ano   = extract_year2(time),
    valor = safe_parse_number(value)
  )

# -------------------- QA exploratorio --------------------
df |>
  count(unit, na_item, geo, name = "n") |>
  arrange(desc(n)) |>
  write_clean(file.path(qa_dir, "17_pib_qa_filtros.csv"))

tibble(units_disponibles = sort(unique(df$unit))) |>
  write_clean(file.path(qa_dir, "17_pib_qa_unidades_dispon.csv"))

# -------------------- Filtro objetivo --------------------
df_es <- df |> filter(geo %in% c("ES","Spain","ES_TOT","ES00"))
if (!nrow(df_es)) abort("No hay filas para España (geo ES/Spain/ES_TOT/ES00).")

# Valor añadido bruto/PIB encadenado equivalente a “B1GQ”
df_es <- df_es |> filter(toupper(na_item) == "B1GQ")

# Preferencia: CLV*_EUR_HAB (volumen encadenado, euros por habitante)
units_clv_hab <- unique(df_es$unit[grepl("CLV", df_es$unit, ignore.case = TRUE) &
                                     grepl("EUR", df_es$unit, ignore.case = TRUE) &
                                     grepl("HAB", df_es$unit, ignore.case = TRUE)])

# Fallback opcional: PPS_HAB (paridad de poder de compra por habitante)
units_pps_hab <- unique(df_es$unit[grepl("^PPS", df_es$unit, ignore.case = TRUE) &
                                     grepl("HAB", df_es$unit, ignore.case = TRUE)])

if (length(units_clv_hab) == 0) {
  if (!ALLOW_PPS_FALLBACK || length(units_pps_hab) == 0) {
    abort("No se encontró unidad CLV*EUR*HAB (y PPS_HAB no permitido o inexistente).")
  } else {
    msg("⚠ No hay CLV*_EUR_HAB. Usaré PPS_HAB como fallback (ver QA).")
  }
}

# Selección de versión CLV más reciente (CLV2015, CLV2010, …)
extract_clv_year <- function(u){
  y <- suppressWarnings(as.integer(stringr::str_extract(u, "CLV\\s*([0-9]{2,4})")))
  if (!is.na(y) && y < 100) 2000 + y else y
}

unit_sel <- NA_character_
if (length(units_clv_hab)) {
  years <- vapply(units_clv_hab, extract_clv_year, integer(1))
  years[is.na(years)] <- -Inf
  unit_sel <- units_clv_hab[which.max(years)]
} else {
  unit_sel <- units_pps_hab[1]
}
msg("Unidad seleccionada: ", unit_sel)

x <- df_es |>
  filter(unit == unit_sel) |>
  transmute(ano, valor)

# -------------------- QA de rango/duplicados/cobertura --------------------
x_fuera <- x |> mutate(flag_fuera = is.na(ano) | ano < YEAR_MIN | ano > YEAR_MAX) |> filter(flag_fuera)
if (nrow(x_fuera)) write_clean(x_fuera, file.path(qa_dir, "17_pib_qa_anios_fuera.csv"))

x <- x |> filter(!is.na(ano), dplyr::between(ano, YEAR_MIN, YEAR_MAX))

dup <- x |> count(ano, name = "n") |> filter(n > 1)
if (nrow(dup)) write_clean(dup, file.path(qa_dir, "17_pib_qa_duplicados.csv"))

x <- x |> group_by(ano) |> summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop")

present <- sort(unique(x$ano)); missing <- setdiff(YEARS_SEQ, present)
tibble(variable="pib_pc_real_es",
       years_min=ifelse(length(present)>0,min(present),NA_integer_),
       years_max=ifelse(length(present)>0,max(present),NA_integer_),
       n_years=length(present), n_missing=length(missing),
       missing_list=paste(missing, collapse=", ")) |>
  write_clean(file.path(qa_dir, "17_pib_qa_cobertura.csv"))

if (!nrow(x)) abort("❌ No hay filas tras filtrar PIB per cápita.")
if (any(is.na(x$valor))) abort("❌ Existen NA en 'valor' tras el filtrado.")

# -------------------- Salida --------------------
out <- x |> arrange(ano) |> transmute(ano, pib_pc_real = valor)
write_clean(out, fp_out)

if (nrow(out) != length(YEARS_SEQ)) {
  msg(sprintf("⚠ Cobertura incompleta: %d años; esperado %d (%d–%d).",
              nrow(out), length(YEARS_SEQ), YEAR_MIN, YEAR_MAX))
} else {
  msg("✓ Cobertura ", YEAR_MIN, "–", YEAR_MAX, " completa.")
}
