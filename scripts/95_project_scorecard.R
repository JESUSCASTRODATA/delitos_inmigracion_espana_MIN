#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
################################################################################
# 95_project_scorecard.R
#
# Objetivo:
#   - Calcular una puntuación 0–100 del proyecto a partir de los QA (30–38).
#   - Consolidar fallos e ideas de mejora priorizadas.
#
# Salidas:
#   - output/tables/qa95_scorecard.csv
#   - output/tables/qa95_fallos.csv
#   - output/tables/qa95_mejoras.csv
################################################################################

suppressPackageStartupMessages({
  library(here); library(readr); library(dplyr); library(tidyr)
  library(tibble); library(janitor); library(stringr); library(purrr)
})

dir.create(here::here('output','tables'), recursive = TRUE, showWarnings = FALSE)

# ---------------- Utilidades ----------------
read0 <- function(p){
  if (!file.exists(p)) return(NULL)
  out <- tryCatch(suppressMessages(readr::read_csv(p, show_col_types = FALSE)),
                  error = function(e) NULL)
  if (is.null(out)) return(NULL)
  janitor::clean_names(out)
}

safe_mean_logical <- function(x){
  if (is.null(x)) return(NA_real_)
  x <- as.logical(x)
  if (all(is.na(x))) return(NA_real_)
  mean(x, na.rm = TRUE)
}

# ---------------- Cargar artefactos ----------------
O <- here::here('output','tables')

d34d <- read0(file.path(O, 'qa34_schema_details.csv'))
d34s <- read0(file.path(O, 'qa34_schema_summary.csv'))
d33s <- read0(file.path(O, 'qa33_outliers_summary.csv'))
d33y <- read0(file.path(O, 'qa33_spikes_yoy_mad.csv'))
d33l <- read0(file.path(O, 'qa33_outliers_level_iqr.csv'))
d36  <- read0(file.path(O, 'qa36_units_anomalies.csv'))
d32c <- read0(file.path(O, 'qa32_cobertura_matrix.csv'))
d32h <- read0(file.path(O, 'qa32_huecos_detalle.csv'))
d31r <- read0(file.path(O, 'qa31_rates_rebuild.csv'))
d31s <- read0(file.path(O, 'qa31_share_det_extr_check.csv'))
d31g <- read0(file.path(O, 'qa31_ranges.csv'))
d37t <- read0(file.path(O, 'qa37_tasas_vs_reconstruccion.csv'))
d37s <- read0(file.path(O, 'qa37_share_extranjeros_vs_reconstruccion.csv'))
d37x <- read0(file.path(O, 'qa37_sexo_suma_total.csv'))

# ---------------- Métricas por dimensión ----------------

# 1) Esquema (34)
schema_penalty <- 0
if (!is.null(d34s) && nrow(d34s) > 0 && 'n_violations' %in% names(d34s)) {
  n_schema_files <- nrow(d34s)
  vfiles <- sum((d34s$n_violations %>% replace_na(0L)) > 0L)
  schema_penalty <- vfiles / max(1, n_schema_files)
}

# 2) Consistencias (31, 37)
cons_penalty <- 0; terms <- 0

if (!is.null(d31r) && nrow(d31r) > 0) {
  fhc <- if ('flag_hc' %in% names(d31r)) safe_mean_logical(d31r$flag_hc) else NA_real_
  fdt <- if ('flag_dt' %in% names(d31r)) safe_mean_logical(d31r$flag_dt) else NA_real_
  fde <- if ('flag_de' %in% names(d31r)) safe_mean_logical(d31r$flag_de) else NA_real_
  cons_penalty <- cons_penalty + mean(c(fhc, fdt, fde), na.rm = TRUE); terms <- terms + 1
}

if (!is.null(d37t) && nrow(d37t) > 0) {
  fhc <- if ('flag_hc' %in% names(d37t)) safe_mean_logical(d37t$flag_hc) else NA_real_
  fdt <- if ('flag_dt' %in% names(d37t)) safe_mean_logical(d37t$flag_dt) else NA_real_
  fde <- if ('flag_de' %in% names(d37t)) safe_mean_logical(d37t$flag_de) else NA_real_
  cons_penalty <- cons_penalty + mean(c(fhc, fdt, fde), na.rm = TRUE); terms <- terms + 1
}

if (!is.null(d31s) && nrow(d31s) > 0 && 'flag_share' %in% names(d31s)) {
  fsh <- safe_mean_logical(d31s$flag_share)
  cons_penalty <- cons_penalty + fsh; terms <- terms + 1
}

if (!is.null(d37s) && nrow(d37s) > 0 && 'flag' %in% names(d37s)) {
  fs2 <- safe_mean_logical(d37s$flag)
  cons_penalty <- cons_penalty + fs2; terms <- terms + 1
}

if (terms > 0) cons_penalty <- cons_penalty / terms

# 3) Rango/Unidades (31,36)
range_penalty <- 0; terms <- 0

if (!is.null(d31g) && nrow(d31g) > 0 && 'flag_range' %in% names(d31g)) {
  fr <- safe_mean_logical(d31g$flag_range)
  range_penalty <- range_penalty + fr; terms <- terms + 1
}

if (!is.null(d36) && nrow(d36) > 0 && !('info' %in% names(d36) && nrow(d36) == 1)) {
  files_bad <- length(unique(d36$file))
  range_penalty <- range_penalty + min(1, files_bad / max(1, files_bad + 3))
  terms <- terms + 1
}

if (terms > 0) range_penalty <- range_penalty / terms

# 4) Cobertura/huecos (32)
cov_penalty <- 0
if (!is.null(d32c) && nrow(d32c) > 0) {
  mat <- d32c[, setdiff(names(d32c), 'ano'), drop = FALSE]
  zeros <- sum(mat == 0, na.rm = TRUE)
  total <- sum(!is.na(as.matrix(mat)))
  cov_penalty <- if (total > 0) zeros / total else 0
}

# 5) Outliers (33)
out_penalty <- 0
if (!is.null(d33s) && nrow(d33s) > 0) {
  n1 <- if ('n_level_iqr' %in% names(d33s)) d33s$n_level_iqr else 0
  n2 <- if ('n_yoy'       %in% names(d33s)) d33s$n_yoy       else 0
  n3 <- if ('n_roll_mad'  %in% names(d33s)) d33s$n_roll_mad  else 0
  flagged <- (n1 %>% replace_na(0)) + (n2 %>% replace_na(0)) + (n3 %>% replace_na(0))
  out_penalty <- mean(flagged > 0)
}

# ---------------- Score total ----------------
w_schema <- 0.35; w_cons <- 0.30; w_range <- 0.20; w_cov <- 0.10; w_out <- 0.05
score <- 100 * (1 - (w_schema*schema_penalty + w_cons*cons_penalty + w_range*range_penalty +
                       w_cov*cov_penalty + w_out*out_penalty))
score <- max(0, min(100, score))

# ---------------- Fallos consolidados ----------------
fallos <- tibble()

# Esquema
if (!is.null(d34d) && nrow(d34d) > 0) {
  fallos <- dplyr::bind_rows(
    fallos,
    d34d %>%
      dplyr::mutate(
        dim = 'schema',
        sugerencia = dplyr::case_when(
          check == 'SCHEMA_columns_missing' ~ 'Añadir columnas obligatorias o ajustar contrato',
          check == 'SCHEMA_type_mismatch' ~ 'Corregir parseo/tipo o ajustar types en schemas.yml',
          check == 'SCHEMA_domain_violation' ~ 'Normalizar valores o ampliar dominio permitido',
          check == 'SCHEMA_range_violation' ~ 'Ajustar unidad/escala o rangos en contrato',
          check == 'SCHEMA_key_duplicates' ~ 'Eliminar duplicados o redefinir clave',
          check == 'SCHEMA_coverage_years_missing' ~ 'Completar años faltantes o ajustar coverage',
          TRUE ~ 'Revisar contrato y dataset'
        )
      ) %>%
      dplyr::select(dim, file, check, detail, sugerencia)
  )
}

# Unidades/escala
if (!is.null(d36) && nrow(d36) > 0 && !('info' %in% names(d36) && nrow(d36) == 1)) {
  fallos <- dplyr::bind_rows(
    fallos,
    d36 %>%
      dplyr::transmute(
        dim = 'units',
        file,
        check = flag,
        detail = paste0(column, ' [', min, ',', max, ']'),
        sugerencia = suggestion
      )
  )
}

# Consistencias tasas reconstruidas
if (!is.null(d31r) && nrow(d31r) > 0) {
  d31r2 <- d31r
  if (!'flag_hc' %in% names(d31r2)) d31r2$flag_hc <- FALSE
  if (!'flag_dt' %in% names(d31r2)) d31r2$flag_dt <- FALSE
  if (!'flag_de' %in% names(d31r2)) d31r2$flag_de <- FALSE
  
  f31 <- d31r2 %>%
    dplyr::mutate(
      check = dplyr::case_when(
        flag_hc ~ 'hc>1% diff',
        flag_dt ~ 'det_tot>1% diff',
        flag_de ~ 'det_ext>1% diff',
        TRUE ~ NA_character_
      ),
      detail = paste0('ano=', if ('ano' %in% names(d31r2)) ano else NA_integer_),
      sugerencia = 'Recalcular tasas y revisar poblacion/divisor'
    ) %>%
    dplyr::filter(!is.na(check)) %>%
    dplyr::transmute(
      dim  = 'consistency_rates',
      file = 'qa31_rates_rebuild.csv',
      check, detail, sugerencia
    )
  
  fallos <- dplyr::bind_rows(fallos, f31)
}

# Consistencias share extranjeros
if (!is.null(d31s) && nrow(d31s) > 0 && 'flag_share' %in% names(d31s)) {
  fsh <- d31s %>%
    dplyr::filter(flag_share %>% replace_na(FALSE)) %>%
    dplyr::transmute(
      dim  = 'consistency_share',
      file = 'detenciones_share_extranjeros.csv',
      check = 'share_diff>0.5pp',
      detail = paste0('ano=', if ('ano' %in% names(d31s)) ano else NA_integer_),
      sugerencia = 'Alinear definicion de share (0..1 vs %)'
    )
  fallos <- dplyr::bind_rows(fallos, fsh)
}

# Sumas por sexo H+M vs TOTAL
if (!is.null(d37x) && nrow(d37x) > 0 && all(c('file','ano','tipo','diff','flag') %in% names(d37x))) {
  fallos <- dplyr::bind_rows(
    fallos,
    d37x %>%
      dplyr::filter(flag) %>%
      dplyr::transmute(
        dim  = 'consistency_sexo',
        file, check = 'H+M != TOTAL',
        detail = paste0('ano=', ano, ', tipo=', tipo, ', diff=', diff),
        sugerencia = 'Cuadrar agregaciones por sexo con TOTAL'
      )
  )
}

# Outliers YoY
if (!is.null(d33y) && nrow(d33y) > 0) {
  flag_col <- intersect(names(d33y), c('flag_yoy','flag_spike','flag'))
  if (length(flag_col) > 0) {
    fc <- flag_col[1]
    dcol <- if ('delta' %in% names(d33y)) 'delta' else NULL
    y1 <- d33y %>% dplyr::filter(.data[[fc]] %>% replace_na(FALSE))
    if (nrow(y1) > 0) {
      if (!is.null(dcol)) {
        y1 <- y1 %>% dplyr::mutate(abs_delta = round(abs(.data[[dcol]]), 2))
        fallos <- dplyr::bind_rows(
          fallos,
          y1 %>% dplyr::transmute(
            dim  = 'outliers_yoy',
            file = if ('dataset' %in% names(y1)) dataset else NA_character_,
            check = 'spike_yoy',
            detail = paste0('key=', if ('key' %in% names(y1)) key else 'TOTAL',
                            ', ano=', if ('ano' %in% names(y1)) ano else NA_integer_,
                            ', abs_delta=', abs_delta),
            sugerencia = 'Verificar salto interanual (evento/metodologia)'
          )
        )
      } else {
        fallos <- dplyr::bind_rows(
          fallos,
          y1 %>% dplyr::transmute(
            dim  = 'outliers_yoy',
            file = if ('dataset' %in% names(y1)) dataset else NA_character_,
            check = 'spike_yoy',
            detail = paste0('key=', if ('key' %in% names(y1)) key else 'TOTAL',
                            ', ano=', if ('ano' %in% names(y1)) ano else NA_integer_),
            sugerencia = 'Verificar salto interanual (evento/metodologia)'
          )
        )
      }
    }
  }
}

# Outliers de nivel (IQR)
if (!is.null(d33l) && nrow(d33l) > 0) {
  flag_col <- intersect(names(d33l), c('flag_level_iqr','flag'))
  if (length(flag_col) > 0) {
    fc <- flag_col[1]
    l1 <- d33l %>% dplyr::filter(.data[[fc]] %>% replace_na(FALSE))
    if (nrow(l1) > 0) {
      fallos <- dplyr::bind_rows(
        fallos,
        l1 %>% dplyr::transmute(
          dim  = 'outliers_level',
          file = if ('dataset' %in% names(l1)) dataset else NA_character_,
          check = 'outlier_iqr',
          detail = paste0('key=', if ('key' %in% names(l1)) key else 'TOTAL',
                          ', ano=', if ('ano' %in% names(l1)) ano else NA_integer_),
          sugerencia = 'Revisar valor atipico o redefinir agregacion'
        )
      )
    }
  }
}

# Cobertura: años faltantes
if (!is.null(d32h) && nrow(d32h) > 0) {
  fc <- intersect(names(d32h), c('flag_missing','flag'))
  if (length(fc) > 0) {
    miss <- d32h %>% dplyr::filter(.data[[fc[1]]] %>% replace_na(FALSE)) %>%
      dplyr::transmute(
        dim  = 'coverage',
        file = if ('dataset' %in% names(d32h)) dataset else NA_character_,
        check = 'ano_missing',
        detail = paste0('key=', if ('key' %in% names(d32h)) key else 'TOTAL',
                        ', ano=', if ('ano' %in% names(d32h)) ano else NA_integer_),
        sugerencia = 'Imputar puntualmente o completar fuente'
      )
    fallos <- dplyr::bind_rows(fallos, miss)
  }
}

# Prioridad y orden
prioridad <- function(dim, check){
  if (dim == 'schema') return(1L)
  if (dim %in% c('consistency_rates','consistency_share','consistency_sexo')) return(2L)
  if (dim == 'units') return(3L)
  if (dim == 'coverage') return(4L)
  if (dim %in% c('outliers_yoy','outliers_level')) return(5L)
  6L
}
if (nrow(fallos) > 0) {
  fallos <- fallos %>%
    dplyr::mutate(priority = mapply(prioridad, dim, check)) %>%
    dplyr::arrange(priority, file, check)
}

# ---------------- Mejoras sugeridas ----------------
mejoras <- tibble(
  tarea = c(
    'Normalizar dominios (sexo, tipo) y documentar en schemas.yml',
    'Corregir escalas de porcentajes y shares (QA36)',
    'Armonizar definiciones de tasas (divisor poblacional consistente, 100k)',
    'Completar anos faltantes o justificar huecos; imputacion simple',
    'Revisar outliers (QA33) y anotar explicaciones o corregir origen',
    'Agregar CI que ejecute 80_run_all.R en cada commit',
    'Generar baseline de hashes (QA35) en config/file_hashes_baseline.csv'
  ),
  prioridad = c('Alta','Alta','Alta','Media','Media','Media','Baja')
)

# ---------------- Scorecard ----------------
scorecard <- tibble(
  dimension = c('schema','consistencias','rango_unidades','cobertura','outliers','score_total'),
  weight    = c(0.35, 0.30, 0.20, 0.10, 0.05, NA_real_),
  penalty   = c(schema_penalty, cons_penalty, range_penalty, cov_penalty, out_penalty, NA_real_),
  score     = c(1-schema_penalty, 1-cons_penalty, 1-range_penalty, 1-cov_penalty, 1-out_penalty, score/100) * 100
)

readr::write_csv(scorecard, here::here('output','tables','qa95_scorecard.csv'))
readr::write_csv(fallos,     here::here('output','tables','qa95_fallos.csv'))
readr::write_csv(mejoras,    here::here('output','tables','qa95_mejoras.csv'))

message('QC 95 terminado. Revisa:',
        '\n - output/tables/qa95_scorecard.csv',
        '\n - output/tables/qa95_fallos.csv',
        '\n - output/tables/qa95_mejoras.csv')
