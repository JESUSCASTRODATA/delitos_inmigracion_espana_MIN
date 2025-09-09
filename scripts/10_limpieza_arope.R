#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
###############################################################################
# 10_limpieza_arope.R — Limpieza AROPE (TOTAL NACIONAL, anual 2010–2023)
#
# Entrada:
#   data/raw/arope.csv (sep=';', UTF-8, 5 columnas)
#   Columnas esperadas (nombres flexibles, se normalizan con clean_names):
#     - sexo
#     - edad
#     - tasa_de_riesgo_de_pobreza_o_exclusion_social_y_sus_componentes (indicador)
#     - periodo
#     - total
#
# Salidas:
#   data/processed/arope_total.csv              (ano, arope)
#   output/tables/qc_arope_indicadores.csv     (niveles únicos del indicador)
#   output/tables/qc_arope_trace_filtros.csv   (filas tras cada filtro)
#   output/tables/qc_arope_na_tokens.csv       (tokens que no parsean a número)
#   output/tables/qc_arope_duplicados_por_ano.csv (si aplica)
#   output/tables/qc_arope_cobertura.csv       (años presentes/faltantes 2010–2023)
#
# Requisitos:
#   - scripts/00_utils_limpieza.R (safe_parse_number, extract_year, write_clean, etc.)
#
# Autor: Jesús Castro (Analista de Datos)
# Última actualización: 2025-08-22
###############################################################################

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(here); library(janitor)
  library(stringr); library(tidyr)
})

source(here("scripts","00_utils_limpieza.R"))
# PROJECT_ROOT_HINT <- "D:/delitos_inmigracion_espana"  # desactivado: usamos .here/.Rproj
root_init("scripts/10_limpieza_arope.R", project_root_hint = PROJECT_ROOT_HINT)

# ---------------------------------------------------------------------------
# Paths y directorios
# ---------------------------------------------------------------------------
fp_raw <- here("data","raw","arope.csv")
assert_infile(fp_raw)
dir.create(here("output","tables"), showWarnings = FALSE, recursive = TRUE)
dir.create(here("data","processed"), showWarnings = FALSE, recursive = TRUE)

# ---------------------------------------------------------------------------
# Lectura cruda (todo como texto) y detección de columnas
# ---------------------------------------------------------------------------
raw <- read_raw(fp_raw)  # añade attrs y limpia nombres
nms <- names(raw)

pick_col <- function(patterns, nms) {
  ix <- which(Reduce(`|`, lapply(patterns, function(p) grepl(p, nms, ignore.case = TRUE))))
  if (length(ix)) nms[ix[1]] else NA_character_
}

col_sexo   <- pick_col(c("^sexo$"), nms)
col_edad   <- pick_col(c("^edad$"), nms)
col_ind    <- pick_col(c(
  "tasa_de_riesgo_de_pobreza_o_exclusion_social_y_sus_componentes",
  "arope", "indicador.*arope", "riesgo.*pobreza.*exclus"
), nms)
col_period <- pick_col(c("^periodo$", "period"), nms)
col_total  <- pick_col(c("^(total|valor|porcentaje)$"), nms)

if (any(is.na(c(col_sexo, col_edad, col_ind, col_period, col_total)))) {
  abort("Faltan columnas en AROPE. Detectadas: ", paste(nms, collapse = ", "))
}

# ---------------------------------------------------------------------------
# Estandarización mínima y QC de niveles del indicador
# ---------------------------------------------------------------------------
df0 <- raw |>
  transmute(
    indicador = .data[[col_ind]],
    sexo      = .data[[col_sexo]],
    edad      = .data[[col_edad]],
    periodo   = .data[[col_period]],
    total_chr = .data[[col_total]]
  )

niveles <- df0 |> distinct(indicador) |> arrange(indicador)
write_clean(niveles, here("output","tables","qc_arope_indicadores.csv"))

# ---------------------------------------------------------------------------
# Filtros de dominio + traza de filtros
# ---------------------------------------------------------------------------
raw_total_n <- nrow(raw)

df_ind <- df0 |>
  filter(str_detect(str_to_lower(indicador), "arope"))

df_pop <- df_ind |>
  filter(str_detect(str_to_lower(sexo), "ambos|total")) |>
  filter(str_detect(str_to_lower(edad), "total"))

df1 <- df_pop |>
  mutate(
    ano   = extract_year(periodo),
    arope = safe_parse_number(total_chr)
  ) |>
  filter(!is.na(ano)) |>
  filter(dplyr::between(ano, 2010, 2023))

trace_tbl <- tibble::tibble(
  paso  = c("raw_total", "indicador_arope", "total_poblacion", "rango_2010_2023"),
  filas = c(raw_total_n, nrow(df_ind), nrow(df_pop), nrow(df1))
)
write_clean(trace_tbl, here("output","tables","qc_arope_trace_filtros.csv"))

# ---------------------------------------------------------------------------
# QC de parseo numérico (tokens problemáticos)
# ---------------------------------------------------------------------------
bad_tokens <- df1 |>
  filter(is.na(arope) & !is.na(total_chr) & nzchar(total_chr)) |>
  distinct(muestra = total_chr)

if (nrow(bad_tokens)) {
  write_clean(bad_tokens, here("output","tables","qc_arope_na_tokens.csv"))
} else {
  message("QC AROPE: sin tokens problemáticos en parseo numérico.")
}

# ---------------------------------------------------------------------------
# Bloqueo de duplicados por año (no debe haber más de 1 fila por año)
# ---------------------------------------------------------------------------
dups <- df1 |>
  count(ano, name = "n") |>
  filter(n > 1)

if (nrow(dups)) {
  det_dups <- df1 |>
    semi_join(dups, by = "ano") |>
    arrange(ano)
  write_clean(det_dups, here("output","tables","qc_arope_duplicados_por_ano.csv"))
  stop("Existen años con >1 fila tras filtros. Revisa output/tables/qc_arope_duplicados_por_ano.csv")
}

# ---------------------------------------------------------------------------
# Serie final (única fila por año, ordenada)
# ---------------------------------------------------------------------------
agg <- df1 |>
  select(ano, arope) |>
  arrange(ano)

# Si parece proporción (0–1), avisa (no detiene)
if (mean(agg$arope, na.rm = TRUE) < 1) {
  warning("AROPE parece venir como proporción (0–1). Verifica si debes multiplicar por 100.")
}

# ---------------------------------------------------------------------------
# QC de cobertura 2010–2023
# ---------------------------------------------------------------------------
esqueleto <- tibble::tibble(ano = 2010:2023)
cov <- esqueleto |>
  left_join(agg, by = "ano") |>
  mutate(presente = !is.na(arope))
write_clean(cov, here("output","tables","qc_arope_cobertura.csv"))

pct_na <- mean(is.na(cov$arope))
message(sprintf("Cobertura AROPE 2010–2023: %d/%d años (NA=%.1f%%)",
                sum(!is.na(cov$arope)), nrow(cov), 100*pct_na))

thr <- suppressWarnings(as.numeric(Sys.getenv("NA_MAX_PCT_AROPE", unset = "10")))
if (is.finite(thr) && (100*pct_na > thr) &&
    tolower(Sys.getenv("STRICT_QA","false")) %in% c("true","1","yes")) {
  stop("STRICT_QA AROPE: NA% supera umbral (", round(100*pct_na,1), "% > ", thr, "%)")
}

# Aserciones duras (CI)
stopifnot(nrow(agg) == length(2010:2023))
stopifnot(all(2010:2023 %in% agg$ano))
stopifnot(!any(is.na(agg$arope)))
stopifnot(all(agg$arope >= 0 & agg$arope <= 100))
stopifnot(!any(duplicated(agg$ano)))

# ---------------------------------------------------------------------------
# Export final
# ---------------------------------------------------------------------------
write_clean(agg, here("data","processed","arope_total.csv"))
message("✅ AROPE limpio -> data/processed/arope_total.csv")
