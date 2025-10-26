#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
################################################################################
# 14a_check_mapping_coverage.R — Cobertura de mapping (tipologías y CCAA)
# Proyecto : Delitos e Inmigración en España (2010–2023)
# Autor    : Jesús Castro · JESUSCASTRODATA
# Licencia : MIT (código) · CC BY 4.0 (docs/figuras)
# Fecha    : 2025-10-02
################################################################################

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(janitor); library(stringr)
  library(here);  library(tidyr); library(purrr); library(tibble)
})

msg <- function(...) message("[14a] ", paste0(...))

# --- Normalizador robusto de claves ------------------------------------------
norm_key <- function(x){
  x <- as.character(x)
  x <- chartr("\u00A0", " ", x)                # NBSP → espacio normal
  x <- gsub("\\s+", " ", x)                    # colapsa espacios
  x <- trimws(x)                               # trim
  x <- tolower(x)                              # minúsculas
  x2 <- tryCatch(iconv(x, to = "ASCII//TRANSLIT"), error = function(e) x)
  ifelse(is.na(x2) | nzchar(x2)==FALSE, x, x2) # si iconv falla, deja x
}

# --- Entradas -----------------------------------------------------------------
fp_hc   <- here::here("data","raw","hechos_conocidos.csv")
fp_he   <- here::here("data","raw","hechos_esclarecidos.csv")
fp_mreg <- here::here("config","map_regiones.csv")
fp_mtip <- here::here("config","map_tipologias.csv")

# --- Salidas ------------------------------------------------------------------
dir.create(here::here("output","tables"), showWarnings = FALSE, recursive = TRUE)
out_ccaa_nm <- here::here("output","tables","14a_ccaa_no_mapeadas.csv")
out_tipo_nm <- here::here("output","tables","14a_tipologias_no_mapeadas.csv")
out_ccaa_dp <- here::here("output","tables","14a_ccaa_duplicados_mapping.csv")
out_tipo_dp <- here::here("output","tables","14a_tipologias_duplicados_mapping.csv")
out_summary <- here::here("output","tables","14a_mapping_summary.csv")
out_cols_hc <- here::here("output","tables","14a_cols_hc.csv")
out_cols_he <- here::here("output","tables","14a_cols_he.csv")

# --- Lecturas (UTF-8 con fallback a Latin-1) ----------------------------------
read_raw_semicolon <- function(path) {
  try_utf8 <- try(
    read_delim(
      file = path, delim = ";",
      col_types = cols(.default = col_character()),
      locale = locale(encoding = "UTF-8"),
      guess_max = 100000, show_col_types = FALSE
    ) |> clean_names(),
    silent = TRUE
  )
  if (!inherits(try_utf8, "try-error")) return(try_utf8)
  # Fallback Latin-1
  read_delim(
    file = path, delim = ";",
    col_types = cols(.default = col_character()),
    locale = locale(encoding = "LATIN1"),
    guess_max = 100000, show_col_types = FALSE
  ) |> clean_names()
}

hc   <- read_raw_semicolon(fp_hc)
he   <- read_raw_semicolon(fp_he)
mreg <- read_csv(fp_mreg, show_col_types = FALSE) |> clean_names()
mtip <- read_csv(fp_mtip, show_col_types = FALSE) |> clean_names()

# --- Inventario de columnas (para inspección) ---------------------------------
write_csv(tibble(idx=seq_along(names(hc)), colname=names(hc)), out_cols_hc)
write_csv(tibble(idx=seq_along(names(he)), colname=names(he)), out_cols_he)
msg("Inventario de columnas escrito: 14a_cols_hc.csv, 14a_cols_he.csv")

# --- Auto-detección de columnas equivalentes ---------------------------------
pick_col <- function(df, candidates, etiqueta){
  nms <- names(df)
  hit <- intersect(candidates, nms)
  if (length(hit) == 0) {
    stop(sprintf("[14a] No encuentro columna para '%s'. Probé: %s. Disponibles: %s",
                 etiqueta, paste(candidates, collapse = ", "),
                 paste(nms, collapse = ", ")), call. = FALSE)
  }
  msg(sprintf("→ %s: '%s'", etiqueta, hit[[1]]))
  hit[[1]]
}

# Candidatas (incluye tus encabezados rotos detectados)
ccaa_candidates <- c(
  "comunidades_autonomas", "comunidades_aut_a3nomas", # <- caso roto
  "ambito_territorial", "territorio", "ccaa",
  "comunidad_autonoma", "comunidades", "region", "autonomia"
)

tipo_candidates <- c(
  "tipologia_penal", "tipolog_a_a_penal",             # <- caso roto
  "tipologia", "tipo_penal", "tipo_delito",
  "grupo_delito", "clasificacion", "categoria", "delito"
)

# Elegimos columnas dinámicamente para HC y HE
hc_ccaa_col  <- pick_col(hc, ccaa_candidates, "CCAA en HC")
hc_tipo_col  <- pick_col(hc, tipo_candidates, "Tipología en HC")
he_ccaa_col  <- pick_col(he, ccaa_candidates, "CCAA en HE")
he_tipo_col  <- pick_col(he, tipo_candidates, "Tipología en HE")

# --- Validación mínima de mappings -------------------------------------------
need_cols <- function(df, cols_needed, df_name){
  missing <- setdiff(cols_needed, names(df))
  if (length(missing)) {
    stop(sprintf("[14a] Faltan columnas en %s: %s",
                 df_name, paste(missing, collapse = ", ")), call. = FALSE)
  }
}
need_cols(mreg, c("raw_region","propuesta_region"), "map_regiones.csv")
need_cols(mtip, c("raw_tipo","propuesta_tipo"),     "map_tipologias.csv")

# --- Dominios reales (HC+HE) usando las columnas detectadas -------------------
dom_ccaa <- dplyr::bind_rows(
  hc |> transmute(raw_region = .data[[hc_ccaa_col]]),
  he |> transmute(raw_region = .data[[he_ccaa_col]])
) |>
  mutate(raw_region_norm = norm_key(raw_region)) |>
  distinct() |>
  arrange(raw_region_norm)

dom_tipo <- dplyr::bind_rows(
  hc |> transmute(raw_tipo = .data[[hc_tipo_col]]),
  he |> transmute(raw_tipo = .data[[he_tipo_col]])
) |>
  mutate(raw_tipo_norm = norm_key(raw_tipo)) |>
  distinct() |>
  arrange(raw_tipo_norm)

# --- Normalizaciones equivalentes en mapping para comparar robustamente -------
mreg2 <- mreg |>
  mutate(raw_region_norm = norm_key(raw_region),
         propuesta_region_norm = norm_key(propuesta_region))

mtip2 <- mtip |>
  mutate(raw_tipo_norm = norm_key(raw_tipo),
         propuesta_tipo_norm = norm_key(propuesta_tipo))

# --- No mapeados (valores en datos que no aparecen en mapping) ----------------
ccaa_no_mapped <- dom_ccaa |> anti_join(mreg2, by = "raw_region_norm")
tipo_no_mapped <- dom_tipo |> anti_join(mtip2, by = "raw_tipo_norm")

# --- Duplicados en mapping (muchos-a-uno inesperado) --------------------------
ccaa_dup_raw <- mreg2 |>
  count(raw_region_norm, propuesta_region_norm, name = "n") |>
  group_by(raw_region_norm) |>
  filter(n() > 1) |>
  ungroup()

tipo_dup_raw <- mtip2 |>
  count(raw_tipo_norm, propuesta_tipo_norm, name = "n") |>
  group_by(raw_tipo_norm) |>
  filter(n() > 1) |>
  ungroup()

# --- Métricas de cobertura ----------------------------------------------------
summary_tbl <- tibble::tibble(
  fecha_iso                 = as.character(Sys.Date()),
  n_dom_ccaa                = nrow(dom_ccaa),
  n_dom_tipo                = nrow(dom_tipo),
  n_ccaa_unmapped           = nrow(ccaa_no_mapped),
  n_tipo_unmapped           = nrow(tipo_no_mapped),
  n_ccaa_dup_mapping        = nrow(ccaa_dup_raw),
  n_tipo_dup_mapping        = nrow(tipo_dup_raw),
  cobertura_ccaa_pct        = round(100 * (1 - nrow(ccaa_no_mapped) / max(1, nrow(dom_ccaa))), 2),
  cobertura_tipo_pct        = round(100 * (1 - nrow(tipo_no_mapped) / max(1, nrow(dom_tipo))), 2)
)

# --- Salidas ------------------------------------------------------------------
write_csv(ccaa_no_mapped, out_ccaa_nm)
write_csv(tipo_no_mapped, out_tipo_nm)
write_csv(ccaa_dup_raw,   out_ccaa_dp)
write_csv(tipo_dup_raw,   out_tipo_dp)
write_csv(summary_tbl,    out_summary)

msg("Dominios escritos en output/tables/:")
msg(basename(out_ccaa_nm)); msg(basename(out_tipo_nm))
msg(basename(out_ccaa_dp)); msg(basename(out_tipo_dp))
msg(basename(out_summary))
msg(sprintf("Cobertura CCAA: %s%% — Cobertura Tipologías: %s%%",
            summary_tbl$cobertura_ccaa_pct, summary_tbl$cobertura_tipo_pct))
msg("✅ 14a_check_mapping_coverage — COMPLETADO")




