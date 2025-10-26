#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
################################################################################
# 18_merge_core_pib.R — Fusiona PIB pc real (Eurostat) en core_indicadores
#
# Proyecto : Delitos e Inmigración en España (2010–2023)
# Autor    : Jesús Castro · JESUSCASTRODATA
# Licencia : MIT (código) · CC BY 4.0 (docs/figuras)
# Fecha    : 2025-09-27
# Versión  : 1.2.1 (booleans robustos, resolución de colisión pib_pc_real, índice dinámico)
#
# Descripción:
#   Une data/processed/core_indicadores.csv con data/processed/pib_pc_real_es.csv
#   por 'ano' y crea derivadas macro: variación interanual (YoY), índice base
#   (columna dinámica según BASE_INDEX_YEAR) y z-score. Incluye QA extendido
#   (cobertura, años perdidos, rangos, NA, hashes opcionales).
#
# Entradas (data/processed/):
#   - core_indicadores.csv            (mínimo: ano; opcionalmente pib_pc_real)
#   - pib_pc_real_es.csv              (ano, pib_pc_real)
#
# Salidas:
#   - data/processed/core_indicadores_con_pib.csv
#   - output/tables/qa_core_pib.csv
#
# Variables de entorno (opcional):
#   - JOIN_MODE=inner|left         # default: inner
#   - STRICT_WINDOW=TRUE|FALSE     # default: TRUE
#   - WINDOW_MIN=2010              # default: 2010
#   - WINDOW_MAX=2023              # default: 2023
#   - BASE_INDEX_YEAR=2010         # si no existe → primer valor observado
#   - FAIL_ON_YEARS_LOST=TRUE|FALSE# default: FALSE (actúa con JOIN_MODE=inner)
#   - WRITE_QA=TRUE|FALSE          # default: TRUE
#
# Uso:
#   Rscript scripts/18_merge_core_pib.R
#   # Ejemplos:
#   JOIN_MODE=left Rscript scripts/18_merge_core_pib.R
#   STRICT_WINDOW=FALSE Rscript scripts/18_merge_core_pib.R
#   BASE_INDEX_YEAR=2015 Rscript scripts/18_merge_core_pib.R
#
# Notas:
#   - Si core ya trae 'pib_pc_real', se resuelve colisión con sufijos (.core/.pib)
#     y se prioriza el valor de 'pib_pc_real' del archivo PIB (fallback al de core).
#   - YoY defensivo: NA si el año previo es NA o ≤ 0.
#   - El índice se escribe en 'pib_pc_real_idxYYYY' (o '_idx_base' si el año base
#     no es numérico) y se documenta en el QA.
################################################################################


suppressPackageStartupMessages({
  library(readr); library(dplyr); library(tidyr); library(janitor)
  library(stringr); library(here); library(fs); library(tibble)
})

# Utilidades
msg     <- function(...) message("[18MERGE] ", paste0(...))
now_iso <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
has_digest <- requireNamespace("digest", quietly = TRUE)
hash_file  <- function(path) if (!file.exists(path)) NA_character_ else if (has_digest) digest::digest(file = path, algo = "sha256") else NA_character_
parse_bool <- function(x, default = TRUE) { if (is.na(x) || x == "") return(default); tolower(x) %in% c("1","true","t","yes","y","si","sí","s") }

# Config
JOIN_MODE          <- tolower(Sys.getenv("JOIN_MODE", unset = "inner")); if (!JOIN_MODE %in% c("inner","left")) JOIN_MODE <- "inner"
WINDOW_MIN         <- as.integer(Sys.getenv("WINDOW_MIN", "2010"))
WINDOW_MAX         <- as.integer(Sys.getenv("WINDOW_MAX", "2023"))
STRICT_WINDOW      <- parse_bool(Sys.getenv("STRICT_WINDOW",      unset = ""), default = TRUE)
FAIL_ON_YEARS_LOST <- parse_bool(Sys.getenv("FAIL_ON_YEARS_LOST", unset = ""), default = FALSE)
WRITE_QA           <- parse_bool(Sys.getenv("WRITE_QA",           unset = ""), default = TRUE)
if (!is.na(WINDOW_MIN) && !is.na(WINDOW_MAX) && WINDOW_MIN > WINDOW_MAX) { tmp <- WINDOW_MIN; WINDOW_MIN <- WINDOW_MAX; WINDOW_MAX <- tmp }
BASE_INDEX_YEAR_RAW <- Sys.getenv("BASE_INDEX_YEAR", "2010")
BASE_INDEX_YEAR_INT <- suppressWarnings(as.integer(BASE_INDEX_YEAR_RAW))

# Rutas
fp_core <- here::here("data","processed","core_indicadores.csv")
fp_pib  <- here::here("data","processed","pib_pc_real_es.csv")
fp_out  <- here::here("data","processed","core_indicadores_con_pib.csv")
qa_dir  <- here::here("output","tables"); fs::dir_create(qa_dir)
fp_qa   <- file.path(qa_dir, "qa_core_pib.csv")

# Existencia
if (!file.exists(fp_core)) stop("No existe: ", fp_core, call. = FALSE)
if (!file.exists(fp_pib))  stop("No existe: ", fp_pib,  call. = FALSE)

# Carga
core <- readr::read_csv(fp_core, show_col_types = FALSE) |> janitor::clean_names()
pib  <- readr::read_csv(fp_pib,  show_col_types = FALSE) |> janitor::clean_names()
n_in_core <- nrow(core); n_in_pib <- nrow(pib)

# Columnas mínimas
need_core <- c("ano")
need_pib  <- c("ano","pib_pc_real")
if (!all(need_core %in% names(core))) stop("Faltan columnas en core_indicadores: ", paste(setdiff(need_core, names(core)), collapse=", "))
if (!all(need_pib  %in% names(pib)))  stop("Faltan columnas en pib_pc_real_es: ", paste(setdiff(need_pib,  names(pib)),  collapse=", "))

# Tipos
core <- core |> mutate(ano = suppressWarnings(as.integer(ano)))
pib  <- pib  |> mutate(ano = suppressWarnings(as.integer(ano)),
                       pib_pc_real = suppressWarnings(as.numeric(pib_pc_real)))

# Duplicados por año
dup_core <- core |> count(ano, name="n") |> filter(!is.na(ano), n > 1)
dup_pib  <- pib  |> count(ano, name="n")  |> filter(!is.na(ano), n > 1)
if (nrow(dup_core)) { msg("Duplicados en core por 'ano' → se toma la primera fila por año.")
  core <- core |> arrange(ano) |> group_by(ano) |> summarise(across(everything(), dplyr::first), .groups="drop") }
if (nrow(dup_pib))  { msg("Duplicados en PIB por 'ano' → se promedian por año.")
  pib  <- pib  |> group_by(ano) |> summarise(pib_pc_real = mean(pib_pc_real, na.rm = TRUE), .groups="drop") }

# Join con sufijos controlados para evitar pérdida del nombre pib_pc_real
years_core <- sort(unique(core$ano))
years_pib  <- sort(unique(pib$ano))

core2 <- if (JOIN_MODE == "left") {
  core |> left_join(pib, by = "ano", suffix = c(".core",".pib")) |> arrange(ano)
} else {
  core |> inner_join(pib, by = "ano", suffix = c(".core",".pib")) |> arrange(ano)
}
if (!nrow(core2)) stop("Merge vacío: no hay años comunes entre core_indicadores y pib_pc_real_es.", call. = FALSE)

# Resolver colisión de pib_pc_real (preferimos la columna .pib; fallback .core)
has_core_col <- "pib_pc_real.core" %in% names(core2)
has_pib_col  <- "pib_pc_real.pib"  %in% names(core2)
if (has_core_col || has_pib_col) {
  core2 <- core2 |>
    mutate(pib_pc_real = dplyr::coalesce(.data[[if (has_pib_col) "pib_pc_real.pib" else NA_character_]],
                                         .data[[if (has_core_col) "pib_pc_real.core" else NA_character_]])) |>
    select(-any_of(c("pib_pc_real.core","pib_pc_real.pib")))
}
if (!("pib_pc_real" %in% names(core2))) stop("Tras el merge no se encuentra 'pib_pc_real'. Revisa columnas de entrada.", call. = FALSE)

# Ventana estricta
years_after <- sort(unique(core2$ano))
years_lost  <- setdiff(years_core, years_after)
if (length(years_lost) && JOIN_MODE == "inner") {
  msg("Años perdidos por inner join: ", paste(years_lost, collapse = ", "))
  if (FAIL_ON_YEARS_LOST) stop("FAIL_ON_YEARS_LOST=TRUE y se perdieron años del core.", call. = FALSE)
}
if (STRICT_WINDOW && !is.na(WINDOW_MIN) && !is.na(WINDOW_MAX)) {
  core2 <- core2 |> filter(dplyr::between(ano, WINDOW_MIN, WINDOW_MAX))
  if (!nrow(core2)) stop("Tras aplicar ventana estricta no quedan años (", WINDOW_MIN, "–", WINDOW_MAX, ").", call. = FALSE)
}

# Derivadas macro
core2 <- core2 |>
  arrange(ano) |>
  mutate(pib_pc_real_yoy = dplyr::if_else(
    !is.na(dplyr::lag(pib_pc_real)) & dplyr::lag(pib_pc_real) > 0,
    pib_pc_real / dplyr::lag(pib_pc_real) - 1,
    NA_real_))

# Índice base (columna dinámica)
idx_col <- if (!is.na(BASE_INDEX_YEAR_INT)) paste0("pib_pc_real_idx", BASE_INDEX_YEAR_INT) else "pib_pc_real_idx_base"
if (!is.na(BASE_INDEX_YEAR_INT)) {
  base_year <- BASE_INDEX_YEAR_INT
  base_val <- core2 |> filter(ano == base_year) |> summarise(val = dplyr::first(stats::na.omit(pib_pc_real))) |> dplyr::pull(val)
  if (length(base_val) == 0 || is.na(base_val)) {
    base_val <- dplyr::first(stats::na.omit(core2$pib_pc_real))
    warning("Año base solicitado (", base_year, ") no disponible/NA; se usa el primer valor observado para el índice.")
  }
} else {
  base_val <- dplyr::first(stats::na.omit(core2$pib_pc_real))
  if (is.na(base_val)) warning("No hay valor base disponible para construir el índice; el índice quedará NA.")
}
core2[[idx_col]] <- ifelse(!is.na(base_val) & !is.na(core2$pib_pc_real), 100 * core2$pib_pc_real / base_val, NA_real_)

# z-score
z_safely <- function(x) { mu <- mean(x, na.rm = TRUE); sdv <- stats::sd(x, na.rm = TRUE); if (is.na(sdv) || sdv == 0) return(rep(NA_real_, length(x))); as.numeric((x - mu) / sdv) }
core2 <- core2 |> mutate(pib_pc_real_z = z_safely(pib_pc_real))

# Checks
stopifnot(all(core2$ano == sort(core2$ano)))
if (any(!dplyr::between(core2$ano,
                        ifelse(is.na(WINDOW_MIN), min(core2$ano), WINDOW_MIN),
                        ifelse(is.na(WINDOW_MAX), max(core2$ano), WINDOW_MAX)))) {
  warning("Años fuera de ventana esperada tras el merge (revisar filtros).")
}
if (sum(is.na(core2$pib_pc_real)) > 0) warning("NA en 'pib_pc_real' post-merge (revisar LEFT JOIN o datos faltantes).")

# Escritura
fs::dir_create(fs::path_dir(fp_out))
readr::write_csv(core2, fp_out)
msg("✔ Escrito: ", fp_out)

# QA
if (WRITE_QA) {
  years_after <- sort(unique(core2$ano))
  qa <- tibble::tibble(
    run_id              = paste0(now_iso(), "_", substr(hash_file(fp_out), 1, 7)),
    join_mode           = JOIN_MODE,
    strict_window       = STRICT_WINDOW,
    window_min          = WINDOW_MIN,
    window_max          = WINDOW_MAX,
    base_index_year     = ifelse(!is.na(BASE_INDEX_YEAR_INT), BASE_INDEX_YEAR_INT, NA_integer_),
    index_column        = idx_col,
    n_in_core           = n_in_core,
    n_in_pib            = n_in_pib,
    n_out               = nrow(core2),
    years_in_core_min   = suppressWarnings(min(years_core, na.rm = TRUE)),
    years_in_core_max   = suppressWarnings(max(years_core, na.rm = TRUE)),
    years_in_pib_min    = suppressWarnings(min(years_pib,  na.rm = TRUE)),
    years_in_pib_max    = suppressWarnings(max(years_pib,  na.rm = TRUE)),
    years_after_min     = suppressWarnings(min(years_after, na.rm = TRUE)),
    years_after_max     = suppressWarnings(max(years_after, na.rm = TRUE)),
    years_lost_count    = length(years_lost),
    years_lost_list     = paste(years_lost, collapse = " "),
    min_pib_pc          = suppressWarnings(min(core2$pib_pc_real, na.rm = TRUE)),
    max_pib_pc          = suppressWarnings(max(core2$pib_pc_real, na.rm = TRUE)),
    yoy_min             = suppressWarnings(min(core2$pib_pc_real_yoy, na.rm = TRUE)),
    yoy_max             = suppressWarnings(max(core2$pib_pc_real_yoy, na.rm = TRUE)),
    na_pib_pc_real      = sum(is.na(core2$pib_pc_real)),
    na_pib_pc_real_yoy  = sum(is.na(core2$pib_pc_real_yoy)),
    na_pib_pc_real_idx  = sum(is.na(core2[[idx_col]])),
    na_pib_pc_real_z    = sum(is.na(core2$pib_pc_real_z)),
    sha_core            = hash_file(fp_core),
    sha_pib             = hash_file(fp_pib),
    sha_out             = hash_file(fp_out)
  )
  readr::write_csv(qa, fp_qa)
  msg("✓ QA → ", fp_qa)
}

msg("✅ Merge core + PIB completo (YoY / índice / z-score) con QA extendido.")

