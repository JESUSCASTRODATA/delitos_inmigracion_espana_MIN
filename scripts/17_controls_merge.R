#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
################################################################################
# 17_controls_merge.R — Añade controles (paro_joven, arope) al core
#
# Proyecto : Delitos e Inmigración en España (2010–2023)
# Autor    : Jesús Castro · JESUSCASTRODATA
# Licencia : MIT (código) · CC BY 4.0 (docs/figuras)
# Fecha    : 2025-09-27
# Versión  : 1.1 (limpieza columnas previas, normalización robusta, avisos)
#
# Entradas:
#   - data/processed/core_indicadores.csv              (requerida; debe tener 'ano')
#   - data/processed/soc_econ_controls.csv             (override con CTRL_FILE)
#       columnas esperadas: ano, paro_joven, arope
#
# Salidas:
#   - data/processed/core_indicadores.csv              (por defecto; OVERWRITE_CORE=TRUE)
#     (si OVERWRITE_CORE=FALSE → data/processed/core_indicadores_con_controles.csv)
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

# --- Rutas --------------------------------------------------------------------
fp_core <- here::here("data","processed","core_indicadores.csv")
fp_ctrl <- Sys.getenv("CTRL_FILE", here::here("data","processed","soc_econ_controls.csv"))
qa_dir  <- here::here("output","tables"); fs::dir_create(qa_dir)
fp_qa   <- file.path(qa_dir, "qa_controls_merge.csv")

# Salida (overwrite por defecto)
OVERWRITE_CORE <- parse_bool(Sys.getenv("OVERWRITE_CORE", unset = ""), default = TRUE)
fp_out <- if (OVERWRITE_CORE) fp_core else here::here("data","processed","core_indicadores_con_controles.csv")

# --- Existencia ---------------------------------------------------------------
if (!file.exists(fp_core)) stop("No existe core: ", fp_core, call. = FALSE)
if (!file.exists(fp_ctrl)) stop("No existe controles: ", fp_ctrl, call. = FALSE)

# --- Carga --------------------------------------------------------------------
core <- readr::read_csv(fp_core, show_col_types = FALSE) |> clean_names()
ctrl <- readr::read_csv(fp_ctrl, show_col_types = FALSE) |> clean_names()

# --- Chequeo mínimo -----------------------------------------------------------
if (!"ano" %in% names(core)) stop("core sin 'ano'.")
need <- c("ano","paro_joven","arope")
if (!all(need %in% names(ctrl))) stop("Controles debe contener: ", paste(need, collapse=", "))

# --- Tipos + normalización de escalas ----------------------------------------
core <- core |> mutate(ano = suppressWarnings(as.integer(ano)))

ctrl <- ctrl |>
  transmute(
    ano        = suppressWarnings(as.integer(ano)),
    paro_joven = suppressWarnings(as.numeric(paro_joven)),
    arope      = suppressWarnings(as.numeric(arope))
  ) |>
  mutate(
    paro_joven = ifelse(is.finite(paro_joven) & paro_joven <= 1, paro_joven*100, paro_joven),
    arope      = ifelse(is.finite(arope)      & arope      <= 1, arope*100,      arope)
  )

# Aviso por rangos anómalos
bad_rng <- ctrl |>
  summarise(
    paro_bad  = sum(paro_joven < 0 | paro_joven > 100, na.rm = TRUE),
    arope_bad = sum(arope < 0 | arope > 100, na.rm = TRUE)
  )
if (bad_rng$paro_bad>0 || bad_rng$arope_bad>0) {
  msg(sprintf("⚠ Controles fuera de [0,100]: paro=%d, arope=%d", bad_rng$paro_bad, bad_rng$arope_bad))
}

# Coberturas
years_core <- sort(unique(core$ano))
years_ctrl <- sort(unique(ctrl$ano))
miss_in_ctrl <- setdiff(years_core, years_ctrl)
if (length(miss_in_ctrl)) msg("⚠ Controles faltan para: ", paste(miss_in_ctrl, collapse=", "))

# --- Merge (limpia columnas previas para evitar sufijos .x/.y) ----------------
core2 <- core |>
  dplyr::select(-dplyr::any_of(c("paro_joven","arope"))) |>
  dplyr::left_join(ctrl, by = "ano") |>
  dplyr::arrange(ano)

# --- QA simple ----------------------------------------------------------------
na_paro  <- sum(is.na(core2$paro_joven))
na_arope <- sum(is.na(core2$arope))

qa <- tibble(
  n_core                = nrow(core),
  n_out                 = nrow(core2),
  years_core_min        = suppressWarnings(min(years_core, na.rm = TRUE)),
  years_core_max        = suppressWarnings(max(years_core, na.rm = TRUE)),
  years_ctrl_min        = suppressWarnings(min(years_ctrl, na.rm = TRUE)),
  years_ctrl_max        = suppressWarnings(max(years_ctrl, na.rm = TRUE)),
  years_intersection    = length(intersect(years_core, years_ctrl)),
  na_paro_joven_out     = na_paro,
  na_arope_out          = na_arope,
  rango_paro_min        = suppressWarnings(min(core2$paro_joven, na.rm = TRUE)),
  rango_paro_max        = suppressWarnings(max(core2$paro_joven, na.rm = TRUE)),
  rango_arope_min       = suppressWarnings(min(core2$arope, na.rm = TRUE)),
  rango_arope_max       = suppressWarnings(max(core2$arope, na.rm = TRUE))
)

# --- Escritura ----------------------------------------------------------------
fs::dir_create(fs::path_dir(fp_out))
readr::write_csv(core2, fp_out)
readr::write_csv(qa, fp_qa)
msg("✔ Escrito core con controles: ", fp_out)
msg("✓ QA → ", fp_qa)

# --- Mini resumen consola -----------------------------------------------------
cat("\n🧪 Merge controles — resumen\n")
cat(sprintf("• Años core: %d–%d | controles: %d–%d | intersección: %d\n",
            qa$years_core_min, qa$years_core_max, qa$years_ctrl_min, qa$years_ctrl_max, qa$years_intersection))
cat(sprintf("• NAs salida — paro_joven: %d | arope: %d\n", na_paro, na_arope))
cat(sprintf("• Rango paro_joven: [%.1f, %.1f] | arope: [%.1f, %.1f]\n\n",
            qa$rango_paro_min, qa$rango_paro_max, qa$rango_arope_min, qa$rango_arope_max))




# ---- Diagnóstico rápido ----
library(readr); library(dplyr); library(janitor); library(here)

ctrl_path <- here::here("data","processed","soc_econ_controls.csv")
stopifnot(file.exists(ctrl_path))

ctrl <- read_csv(ctrl_path, show_col_types = FALSE) |> clean_names()
cat("Columnas en soc_econ_controls.csv:\n"); print(names(ctrl))
cat("NA por columna:\n"); print(sapply(ctrl, \(x) sum(is.na(x))))

# ---- Reconstrucción si arope está mal o falta ----
need_rebuild <- !all(c("ano","paro_joven","arope") %in% names(ctrl)) ||
  all(is.na(ctrl$arope))

if (need_rebuild) {
  message("Reconstruyendo soc_econ_controls.csv desde procesados…")
  # Paro 16–29 (ya en %)
  paro <- read_csv(here("data","processed","paro_15_29_anual.csv"), show_col_types = FALSE) |>
    transmute(ano = as.integer(ano), paro_joven = as.numeric(paro_15_29))
  # AROPE total (en %)
  aro  <- read_csv(here("data","processed","arope_total.csv"), show_col_types = FALSE) |>
    transmute(ano = as.integer(ano), arope = as.numeric(arope))
  # Junta y normaliza rangos
  controls <- full_join(paro, aro, by = "ano") |>
    arrange(ano) |>
    mutate(
      paro_joven = ifelse(is.finite(paro_joven) & paro_joven <= 1, paro_joven*100, paro_joven),
      arope      = ifelse(is.finite(arope)      & arope      <= 1, arope*100,      arope)
    )
  # Sanity check
  stopifnot(all(controls$paro_joven >= 0 & controls$paro_joven <= 100, na.rm = TRUE))
  stopifnot(all(controls$arope      >= 0 & controls$arope      <= 100, na.rm = TRUE))
  write_csv(controls, ctrl_path)
  message("✔ Reescrito: ", ctrl_path)
} else {
  message("El archivo ya tiene las columnas correctas; no se reconstruye.")
}


