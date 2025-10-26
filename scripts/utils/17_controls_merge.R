#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
################################################################################
# 17_controls_merge.R — Añade controles (paro_joven, arope) al core
#
# Proyecto : Delitos e Inmigración en España (2010–2023)
# Autor    : Jesús Castro · JESUSCASTRODATA
# Licencia : MIT (código) · CC BY 4.0 (docs/figuras)
# Fecha    : 2025-09-27
# Versión  : 1.0
#
# Descripción:
#   Une data/processed/core_indicadores.csv con un fichero de controles
#   (por defecto: data/processed/soc_econ_controls.csv) por 'ano', añade
#   columnas 'paro_joven' y 'arope', normaliza a 0–100 si venían 0–1, y escribe
#   el core actualizado. Incluye QA de cobertura y NA.
#
# Entradas:
#   - data/processed/core_indicadores.csv              (requerida; debe tener 'ano')
#   - data/processed/soc_econ_controls.csv             (por defecto; override con CTRL_FILE)
#       columnas esperadas: ano, paro_joven, arope
#
# Salidas:
#   - data/processed/core_indicadores.csv              (por defecto: overwrite)
#     (override con OVERWRITE_CORE=FALSE para escribir core_indicadores_con_controles.csv)
#   - output/tables/qa_controls_merge.csv
#
# Variables de entorno:
#   - CTRL_FILE=path/al/archivo.csv   (default: data/processed/soc_econ_controls.csv)
#   - OVERWRITE_CORE=TRUE|FALSE       (default: TRUE)
################################################################################

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(tidyr); library(janitor)
  library(here);  library(fs);   library(tibble); library(stringr)
})

msg <- function(...) message("[17CTRL] ", paste0(...))
parse_bool <- function(x, default = TRUE) {
  if (is.na(x) || x == "") return(default)
  tolower(x) %in% c("1","true","t","yes","y","si","sí","s")
}

# Rutas
fp_core <- here::here("data","processed","core_indicadores.csv")
fp_ctrl <- Sys.getenv("CTRL_FILE", here::here("data","processed","soc_econ_controls.csv"))
qa_dir  <- here::here("output","tables"); fs::dir_create(qa_dir)
fp_qa   <- file.path(qa_dir, "qa_controls_merge.csv")

# Salida (overwrite por defecto)
OVERWRITE_CORE <- parse_bool(Sys.getenv("OVERWRITE_CORE", unset = ""), default = TRUE)
fp_out <- if (OVERWRITE_CORE) fp_core else here::here("data","processed","core_indicadores_con_controles.csv")

# Existencia
if (!file.exists(fp_core)) stop("No existe core: ", fp_core, call. = FALSE)
if (!file.exists(fp_ctrl)) stop("No existe controles: ", fp_ctrl, call. = FALSE)

# Carga
core <- readr::read_csv(fp_core, show_col_types = FALSE) |> clean_names()
ctrl <- readr::read_csv(fp_ctrl, show_col_types = FALSE) |> clean_names()

# Chequeo mín
if (!"ano" %in% names(core)) stop("core sin 'ano'.")
need <- c("ano","paro_joven","arope")
if (!all(need %in% names(ctrl))) stop("Controles debe contener: ", paste(need, collapse=", "))

# Tipos
core <- core |> mutate(ano = suppressWarnings(as.integer(ano)))
ctrl <- ctrl |> transmute(
  ano        = suppressWarnings(as.integer(ano)),
  paro_joven = suppressWarnings(as.numeric(paro_joven)),
  arope      = suppressWarnings(as.numeric(arope))
)

# Normaliza escalas: si ≤1 asumimos proporción y pasamos a %
ctrl <- ctrl |>
  mutate(
    paro_joven = ifelse(is.finite(paro_joven) & paro_joven <= 1, paro_joven*100, paro_joven),
    arope      = ifelse(is.finite(arope)      & arope      <= 1, arope*100,      arope)
  )

# Merge (left: preserva core)
before_n <- nrow(core)
years_core <- sort(unique(core$ano))
years_ctrl <- sort(unique(ctrl$ano))

core2 <- core |>
  left_join(ctrl, by="ano") |>
  arrange(ano)

# QA simple
na_paro  <- sum(is.na(core2$paro_joven))
na_arope <- sum(is.na(core2$arope))

qa <- tibble(
  n_core                = before_n,
  n_out                 = nrow(core2),
  years_core_min        = suppressWarnings(min(years_core, na.rm = TRUE)),
  years_core_max        = suppressWarnings(max(years_core, na.rm = TRUE)),
  years_ctrl_min        = suppressWarnings(min(years_ctrl, na.rm = TRUE)),
  years_ctrl_max        = suppressWarnings(max(years_ctrl, na.rm = TRUE)),
  years_intersection    = length(intersect(years_core, years_ctrl)),
  na_paro_joven_out     = na_paro,
  na_arope_out          = na_arope
)

# Escritura
fs::dir_create(fs::path_dir(fp_out))
readr::write_csv(core2, fp_out)
readr::write_csv(qa, fp_qa)
msg("✔ Escrito core con controles: ", fp_out)
msg("✓ QA → ", fp_qa)
