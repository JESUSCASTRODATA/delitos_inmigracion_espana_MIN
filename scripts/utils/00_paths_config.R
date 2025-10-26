#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
################################################################################
# 00_paths_config.R — Detección de raíz del proyecto, opciones globales y rutas
#
# Qué hace:
#   - Fija la raíz del proyecto (here::i_am) y crea la estructura de carpetas.
#   - Establece opciones globales (tz, impresión, warnings, seed).
#   - Expone utilidades ligeras para rutas y validaciones mínimas.
#
# Entradas:  (no aplica)
# Salidas:   (no aplica; solo crea directorios si faltan)
#
# Autor: Jesús Castro · JESUSCASTRODATA
# Licencia: MIT (código) · CC BY 4.0 (docs/figuras)
# Última actualización: 2025-09-22
################################################################################

suppressPackageStartupMessages({
  library(here)
  library(fs)
})

# ─────────────────────────── 1) Raíz del proyecto ────────────────────────────
# Marca este archivo como ancla de la raíz del repo. NO mover fuera de scripts/.
here::i_am("scripts/00_paths_config.R")

# Helper: ruta dentro del proyecto (alias corto)
proj_path <- function(...) here::here(...)

# ───────────────────────── 2) Opciones globales ──────────────────────────────
# Zona horaria, impresión y warnings; reproducibilidad soft.
options(
  "repos"              = c(CRAN = "https://cloud.r-project.org"),
  "digits"             = 7,
  "scipen"             = 999,
  "stringsAsFactors"   = FALSE,
  "tz"                 = "Europe/Madrid",
  "warnings.length"    = 8170,
  "width"              = 120,
  "pillar.sigfig"      = 7
)
set.seed(1234)

# Mensajería mínima
msg <- function(...) message("[00] ", paste0(...))

# ───────────────────── 3) Estructura de carpetas del repo ────────────────────
# Ajusta si tu repo usa alguna variante; estas coinciden con tu pipeline actual.
DIRS_EXPECTED <- c(
  "data",
  "data/raw",
  "data/processed",
  "output",
  "output/tables",
  "output/figures",
  "scripts",
  "docs"
)

ensure_dirs <- function(paths) {
  for (p in paths) {
    full <- proj_path(p)
    if (!fs::dir_exists(full)) {
      fs::dir_create(full, recurse = TRUE)
      msg("Creada carpeta: ", fs::path_rel(full, start = proj_path()))
    }
  }
}

ensure_dirs(DIRS_EXPECTED)

# ──────────────────── 4) Validaciones mínimas/útiles ─────────────────────────
# Nota: Tu 00_utils_limpieza.R suele incluir versiones más completas de estos.
# Aquí dejo utilidades ligeras para que 00_paths_config sea autosuficiente.

assert_infile <- function(p_rel) {
  p <- proj_path(p_rel)
  if (!fs::file_exists(p)) stop("No existe: ", p_rel, call. = FALSE)
  invisible(p)
}

assert_outdir <- function(p_rel_dir) {
  d <- proj_path(p_rel_dir)
  if (!fs::dir_exists(d)) fs::dir_create(d, recurse = TRUE)
  invisible(d)
}

# Escritura segura (CSV) con log mínimo
write_clean <- function(df, p_rel) {
  d <- fs::path_dir(p_rel)
  if (nzchar(d)) assert_outdir(d)
  p <- proj_path(p_rel)
  readr::write_csv(df, p, na = "")
  msg("Escrito: ", fs::path_rel(p, start = proj_path()))
  invisible(p)
}

# ────────────────────────── 5) Salida informativa ────────────────────────────
msg("Raíz: ", proj_path())
msg("Estructura verificada (", length(DIRS_EXPECTED), " carpetas).")
