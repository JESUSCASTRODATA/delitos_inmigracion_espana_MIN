#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
################################################################################
# Proyecto : Delitos e Inmigración en España (2010–2023)
# Script   : 10_limpieza_arope.R
# Título   : Limpieza AROPE (total y componentes) 2010–2023
# Autor    : Jesús Castro · JESUSCASTRODATA
# Licencia : MIT (código) · CC BY 4.0 (docs/figuras)
# Fecha    : 2025-10-01 (mod. INE filtros + prioridad 'total')
#
# Qué hace:
#   - Lee data/raw/arope.csv con read_raw() (todo como texto) y normaliza.
#   - Detecta columna de año, columna de dimensiones (componentes) y columna
#     numérica de valores, de forma robusta.
#   - Mantiene únicamente filas “total/ambos sexos/ámbito nacional” en columnas
#     extra (si existen). Para layouts INE, filtra explícitamente:
#       sexo == "Ambos sexos", edad == "Total" y
#       indicador == "Tasa de riesgo de pobreza o exclusión social (indicador AROPE)".
#   - Construye:
#       * data/processed/arope_componentes.csv (ano, componente, valor)
#       * data/processed/arope_total.csv        (ano, arope)
#   - QA compacto con resúmenes y validaciones de rango.
################################################################################

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(here); library(janitor)
  library(stringr); library(tidyr); library(purrr)
})

# ── Utilidades del proyecto ──────────────────────────────────────────────────
source(here::here("scripts","00_utils_limpieza.R"))
root_init("scripts/10_limpieza_arope.R")

msg  <- function(...) message("[10] ", paste0(...))
ok   <- function(...) message("✅ ", paste0(...))
warn <- function(...) warning("⚠️ ", paste0(...), call. = FALSE)

raw_fp  <- here::here("data","raw","arope.csv")
procdir <- here::here("data","processed")
qadir   <- here::here("output","tables")
dir.create(procdir, recursive = TRUE, showWarnings = FALSE)
dir.create(qadir,   recursive = TRUE, showWarnings = FALSE)

# ── Helpers ──────────────────────────────────────────────────────────────────
norm_num <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("..",".")] <- NA_character_
  safe_parse_number(x)
}
norm_txt <- function(x) {
  x <- tolower(as.character(x))
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  stringr::str_squish(x)
}

# Mantiene filas "total/ambos sexos/ámbito nacional" en columnas extra (si existen)
keep_totals_rows <- function(df, used_cols = c("ano")) {
  extra <- setdiff(names(df), used_cols)
  if (!length(extra)) return(df)
  norm <- function(x){
    x <- tolower(trimws(as.character(x)))
    x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
    stringr::str_squish(x)
  }
  is_total_val <- function(v){
    v2 <- norm(v)
    v2 %in% c(
      "total","ambos sexos","ambos_sexos","total nacional","ambito nacional",
      "ambito_nacional","total general","total agregado","total, nacional",
      "nacional","total poblacion","poblacion total"
    )
  }
  df2 <- df
  for (col in extra) {
    vals <- unique(na.omit(norm(df2[[col]])))
    if (length(vals) && any(is_total_val(vals))) {
      df2 <- df2[is_total_val(df2[[col]]) | is.na(df2[[col]]), , drop = FALSE]
    }
  }
  df2
}

# ── Lectura y normalización base ─────────────────────────────────────────────
df0 <- read_raw(raw_fp) |> janitor::clean_names()
ycol <- detect_year_col(names(df0))
if (is.na(ycol)) stop("No se pudo detectar columna de año en AROPE (headers: ", paste(names(df0), collapse=", "), ")")

df0 <- df0 |>
  dplyr::mutate(ano = extract_year(.data[[ycol]])) |>
  dplyr::filter(!is.na(ano), dplyr::between(ano, 2010, 2023))

# ── Filtro específico INE: Ambos sexos / Total / Indicador AROPE ─────────────
cols <- names(df0); has <- function(x) x %in% cols

# Sexo/edad (si existen)
if (has("sexo") && has("edad")) {
  df0 <- df0 |>
    dplyr::filter(tolower(sexo) %in% c("ambos sexos","ambos"),
                  tolower(edad) %in% c("total"))
}

# Indicador/literal del AROPE (si existe alguna columna del indicador)
cand_ind <- cols[grepl("riesgo.*exclusion.*social|arope", cols, ignore.case = TRUE)]
if (length(cand_ind)) {
  indcol <- cand_ind[1]
  df0 <- df0 |>
    dplyr::filter(.data[[indcol]] %in% c("Tasa de riesgo de pobreza o exclusión social (indicador AROPE)"))
}

# ── Detectar columna de componentes ──────────────────────────────────────────
pat_dim <- c("riesgo.*exclusion.*social", "\\barope\\b", "arope", "indicador", "concepto", "descripcion")
cand_dim <- names(df0)[Reduce(`|`, lapply(pat_dim, function(p) grepl(p, names(df0), ignore.case = TRUE)))]
if (!length(cand_dim)) {
  cat_cols <- names(df0)[vapply(df0, function(v) is.character(v) || is.factor(v), logical(1))]
  if (!length(cat_cols)) stop("No se encontró columna de componentes AROPE ni alternativa categórica.")
  cand <- purrr::keep(cat_cols, function(nm) {
    v <- df0[[nm]]
    u <- dplyr::n_distinct(v, na.rm = TRUE)
    u <= max(100, ceiling(0.2 * nrow(df0)))
  })
  if (!length(cand)) cand <- cat_cols[1]
  dim_col <- cand[1]
} else {
  dim_col <- cand_dim[order(!grepl("component", cand_dim, ignore.case = TRUE), -nchar(cand_dim))][1]
}

# ── Detectar columna numérica robustamente ───────────────────────────────────
name_cand <- names(df0)[grepl("total|valor|tasa|porcen", names(df0), ignore.case = TRUE)]
name_cand <- unique(c(name_cand, setdiff(names(df0), c(dim_col, "ano", ycol))))
# PRIORIDAD: si existe columna 'total' (típico INE), probarla primero
name_cand <- unique(c("total", name_cand))

score_num <- function(nm) {
  v <- norm_num(df0[[nm]])
  good <- sum(!is.na(v))
  penalty <- if (good == 0) 1e6 else 0
  -( -good + penalty )
}
if (!length(name_cand)) stop("No se encontró ninguna candidata para columna numérica.")
scores <- vapply(name_cand, score_num, numeric(1))
num_col <- name_cand[which.max(scores)]
if (all(is.na(norm_num(df0[[num_col]])))) stop("No se pudo parsear ninguna columna numérica (revisa el CSV de AROPE).")

# ── Filtrado a totales en dimensiones extra y construcción de componentes ───
df1 <- df0[, unique(c("ano", dim_col, num_col, names(df0))), drop = FALSE]
df1 <- keep_totals_rows(df1, used_cols = c("ano", dim_col, num_col))

df <- df1 |>
  dplyr::transmute(
    ano,
    componente = .data[[dim_col]],
    valor = norm_num(.data[[num_col]])
  ) |>
  dplyr::group_by(ano, componente) |>
  dplyr::summarise(
    valor = if (all(is.na(valor))) NA_real_ else max(valor, na.rm = TRUE),
    .groups = "drop"
  )

write_clean(df |> dplyr::arrange(ano, componente), here::here("data","processed","arope_componentes.csv"))

# ── Total AROPE por año (por etiqueta; fallback = máximo anual) ─────────────
comp_norm <- norm_txt(df$componente)
es_total  <- grepl("tasa.*riesgo.*exclusion.*social", comp_norm) | grepl("\\barope\\b", comp_norm)

arope_tmp <- df |>
  dplyr::mutate(is_total_flag = es_total) |>
  dplyr::group_by(ano) |>
  dplyr::summarise(
    arope_explicit = if (any(is_total_flag, na.rm = TRUE)) {
      v <- valor[is_total_flag]
      if (all(is.na(v))) NA_real_ else max(v, na.rm = TRUE)
    } else NA_real_,
    arope_fallback = if (all(is.na(valor))) NA_real_ else max(valor, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(arope = dplyr::coalesce(arope_explicit, arope_fallback))

if (sum(!is.na(arope_tmp$arope_explicit)) == 0) warn("No se halló etiqueta explícita de AROPE; se usó el máximo anual como fallback.")
if (any(is.na(arope_tmp$arope))) warn("Quedaron NA en AROPE total. Revisa valores y tokens en el bruto.")

arope_tot <- arope_tmp |> dplyr::transmute(ano, arope)
write_clean(arope_tot, here::here("data","processed","arope_total.csv"))
ok("AROPE limpio → data/processed/arope_total.csv y arope_componentes.csv")

# ── QA compacto ──────────────────────────────────────────────────────────────
comp <- readr::read_csv(here::here("data/processed/arope_componentes.csv"), show_col_types = FALSE)
tot  <- readr::read_csv(here::here("data/processed/arope_total.csv"),        show_col_types = FALSE)

comp_summary <- comp %>%
  dplyr::summarise(ano_min=min(ano), ano_max=max(ano),
                   n_na_valor=sum(is.na(valor)))

tot_summary <- tot %>%
  dplyr::summarise(ano_min=min(ano), ano_max=max(ano),
                   n_na_arope=sum(is.na(arope)))

dups <- comp %>% dplyr::count(ano, componente) %>% dplyr::filter(n>1)
faltantes <- tibble::tibble(anos_faltantes = setdiff(2010:2023, tot$ano))

comp_rng <- suppressWarnings(
  comp %>% dplyr::group_by(ano) %>%
    dplyr::summarise(
      min_comp = if (all(is.na(valor))) NA_real_ else min(valor, na.rm=TRUE),
      max_comp = if (all(is.na(valor))) NA_real_ else max(valor, na.rm=TRUE),
      .groups="drop"
    )
)

fuera_rango <- tot %>%
  dplyr::left_join(comp_rng, by="ano") %>%
  dplyr::mutate(flag = ifelse(is.na(min_comp) | is.na(max_comp) | is.na(arope),
                              NA, arope >= min_comp & arope <= max_comp)) %>%
  dplyr::filter(flag == FALSE)

write_clean(comp_summary, file.path(qadir,"10_arope_comp_resumen.csv"))
write_clean(tot_summary,  file.path(qadir,"10_arope_total_resumen.csv"))
write_clean(dups,         file.path(qadir,"10_arope_comp_duplicados.csv"))
write_clean(faltantes,    file.path(qadir,"10_arope_total_anos_faltantes.csv"))
write_clean(fuera_rango,  file.path(qadir,"10_arope_total_fuera_rango_componentes.csv"))

# ── Mini check final ─────────────────────────────────────────────────────────
STRICT_QA <- as.logical(Sys.getenv("STRICT_QA", "FALSE"))

n_bad   <- nrow(fuera_rango)
n_miss  <- length(setdiff(2010:2023, tot$ano))
n_na    <- sum(is.na(tot$arope))

if (n_bad == 0 && n_miss == 0 && n_na == 0) {
  message("✅ [10] Check AROPE OK: 2010–2023 completos, sin NA y dentro del rango de componentes.")
} else {
  warning(sprintf("⚠️ [10] Check AROPE: fuera_rango=%d, años_faltantes=%d, NA=%d",
                  n_bad, n_miss, n_na), call. = FALSE)
  if (STRICT_QA && (n_bad > 0 || n_miss > 0)) {
    stop("[10] QA estricto activado (STRICT_QA=TRUE): hay fuera de rango o años faltantes.")
  }
}



