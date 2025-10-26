#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
# 14a_check_mapping_coverage.R — Cobertura de mapping (tipologías y CCAA)
suppressPackageStartupMessages({
  library(readr); library(dplyr); library(janitor); library(stringr); library(here); library(tidyr)
})

msg <- function(...) message("[14a] ", paste0(...))
norm_key <- function(x){
  x <- trimws(gsub("\\s+", " ", chartr("\u00A0", " ", as.character(x))))
  x2 <- tolower(x)
  x3 <- tryCatch(iconv(x2, to = "ASCII//TRANSLIT"), error = function(e) x2)
  ifelse(is.na(x3), x2, x3)
}

# --- Entradas
fp_hc   <- here::here("data","raw","hechos_conocidos.csv")
fp_he   <- here::here("data","raw","hechos_esclarecidos.csv")
fp_mreg <- here::here("config","map_regiones.csv")
fp_mtip <- here::here("config","map_tipologias.csv")

dir.create(here::here("output","tables"), TRUE, FALSE)

# --- Lecturas
hc  <- read_delim(fp_hc, delim=";", col_types=cols(.default=col_character()), show_col_types=FALSE) |> clean_names()
he  <- read_delim(fp_he, delim=";", col_types=cols(.default=col_character()), show_col_types=FALSE) |> clean_names()
mreg <- read_csv(fp_mreg, show_col_types=FALSE) |> clean_names()
mtip <- read_csv(fp_mtip, show_col_types=FALSE) |> clean_names()

# --- Dominios reales (HC+HE)
dom_ccaa <- bind_rows(
  hc |> transmute(raw_region = comunidades_autonomas),
  he |> transmute(raw_region = comunidades_autonomas)
) |>
  mutate(raw_region_norm = norm_key(raw_region)) |>
  distinct() |>
  arrange(raw_region_norm)

dom_tipo <- bind_rows(
  hc |> transmute(raw_tipo = tipologia_penal),
  he |> transmute(raw_tipo = tipologia_penal)
) |>
  mutate(raw_tipo_norm = norm_key(raw_tipo)) |>
  distinct() |>
  arrange(raw_tipo_norm)

# --- Normalizaciones equivalentes en mapping para comparar robustamente
mreg2 <- mreg |> mutate(raw_region_norm = norm_key(raw_region))
mtip2 <- mtip |> mutate(raw_tipo_norm   = norm_key(raw_tipo))

# --- No mapeados (valores en datos que no aparecen en mapping)
ccaa_no_mapped <- dom_ccaa |> anti_join(mreg2, by="raw_region_norm")
tipo_no_mapped <- dom_tipo |> anti_join(mtip2, by="raw_tipo_norm")

# --- Duplicados en mapping (muchos-a-uno inesperado)
ccaa_dup_raw <- mreg2 |> count(raw_region_norm, propuesta_region, name="n") |> group_by(raw_region_norm) |> filter(n()>1)
tipo_dup_raw <- mtip2 |> count(raw_tipo_norm, propuesta_tipo,   name="n") |> group_by(raw_tipo_norm)   |> filter(n()>1)

# --- Salidas
out_ccaa_nm <- here::here("output","tables","14a_ccaa_no_mapeadas.csv")
out_tipo_nm <- here::here("output","tables","14a_tipologias_no_mapeadas.csv")
out_ccaa_dp <- here::here("output","tables","14a_ccaa_duplicados_mapping.csv")
out_tipo_dp <- here::here("output","tables","14a_tipologias_duplicados_mapping.csv")

write_csv(ccaa_no_mapped, out_ccaa_nm)
write_csv(tipo_no_mapped, out_tipo_nm)
write_csv(ccaa_dup_raw,   out_ccaa_dp)
write_csv(tipo_dup_raw,   out_tipo_dp)

msg("Dominios escritos en output/tables/:")
msg(basename(out_ccaa_nm)); msg(basename(out_tipo_nm))
msg(basename(out_ccaa_dp)); msg(basename(out_tipo_dp))

# --- Pistas por consola (primeras filas)
if (nrow(ccaa_no_mapped)) {
  msg("⚠ CCAA no mapeadas (ejemplos):")
  print(head(ccaa_no_mapped, 20), n=20)
} else msg("✓ Todas las CCAA del RAW están en el mapping.")

if (nrow(tipo_no_mapped)) {
  msg("⚠ Tipologías no mapeadas (ejemplos):")
  print(head(tipo_no_mapped, 20), n=20)
} else msg("✓ Todas las tipologías del RAW están en el mapping.")

if (nrow(ccaa_dup_raw))  msg("⚠ Hay duplicados por raw_region en map_regiones.csv (ver CSV).")
if (nrow(tipo_dup_raw))  msg("⚠ Hay duplicados por raw_tipo en map_tipologias.csv (ver CSV).")

