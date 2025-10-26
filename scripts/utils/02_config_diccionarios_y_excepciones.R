#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
################################################################################
# 02_config_diccionarios_y_excepciones.R
#
# Objetivo:
#   - Detectar y proponer mapeos canónicos para REGIONES y TIPOLOGÍAS a partir
#     de los RAW (HE y HC), preservando mapeos existentes en config/.
#   - Generar/actualizar whitelist de excepciones HE > HC (por tipología/año)
#     cuando el exceso es pequeño (tolerancia), o si el usuario ya las definió.
#
# Entradas (opcionales pero recomendadas):
#   data/raw/hechos_esclarecidos.csv
#   data/raw/hechos_conocidos.csv
#   data/processed/hechos_conocidos_total_nacional.csv
#
# Config leída/escrita:
#   config/map_regiones.csv                  (raw_region, propuesta_region)
#   config/map_tipologias.csv                (raw_tipo,   propuesta_tipo)
#   config/qa_excepciones_he_vs_hc.csv       (ano, tipo, motivo)
#
# Salidas de QC:
#   output/tables/02_dicc_resumen.csv
#   output/tables/02_dicc_conflictos.csv
#   output/tables/02_dicc_nuevos_pendientes.csv
#   output/tables/02_whitelist_sugerencias.csv
#
# Proyecto : Delitos e Inmigración en España (2010–2023)
# Autor    : Jesús Castro · JESUSCASTRODATA
# Licencia : MIT (código) · CC BY 4.0 (docs/figuras)
# Fecha    : 2025-09-22
################################################################################

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(stringr); library(tidyr)
  library(janitor); library(here); library(purrr); library(tibble); library(fs)
})

# ───────── Parámetros ─────────
YEAR_MIN <- 2010L; YEAR_MAX <- 2023L
TOLERANCIA_REL  <- as.numeric(Sys.getenv("HE_HC_TOL_REL", "0.02"))  # 2%
TOLERANCIA_ABS  <- as.numeric(Sys.getenv("HE_HC_TOL_ABS", "25"))    # 25 casos
QUIET <- as.logical(Sys.getenv("QUIET", "FALSE"))
msg <- function(...) if (!QUIET) message("[02] ", paste0(...))

# ───────── Helpers ─────────
lower_noacc <- function(x){
  x0 <- tolower(trimws(as.character(x)))
  y  <- suppressWarnings(iconv(x0, to = "ASCII//TRANSLIT"))
  y[is.na(y)] <- x0[is.na(y)]
  stringr::str_squish(y)
}
extract_year <- function(x) suppressWarnings(as.integer(stringr::str_extract(as.character(x), "[12][0-9]{3}")))
parse_num_es <- function(x){
  if (is.numeric(x)) return(as.numeric(x))
  x0 <- trimws(gsub("[\u00A0\r\n\t]+"," ", as.character(x)))
  x0 <- ifelse(x0 %in% c("..",".",":","-",""), NA_character_, x0)
  x0 <- gsub("\\.", "", x0); x0 <- gsub(",", ".", x0)
  suppressWarnings(as.numeric(x0))
}
pick_col <- function(nms, patterns){
  ix <- unique(unlist(lapply(patterns, function(p) stringr::str_which(nms, stringr::regex(p, ignore_case = TRUE)))))
  if (length(ix)) nms[ix[1]] else NA_character_
}

# ───────── Rutas ─────────
dir.create(here::here("config"), recursive = TRUE, showWarnings = FALSE)
dir.create(here::here("output","tables"), recursive = TRUE, showWarnings = FALSE)

fp_he_raw  <- here::here("data","raw","hechos_esclarecidos.csv")
fp_hc_raw  <- here::here("data","raw","hechos_conocidos.csv")
fp_hc_tot  <- here::here("data","processed","hechos_conocidos_total_nacional.csv")

fp_map_reg <- here::here("config","map_regiones.csv")
fp_map_tip <- here::here("config","map_tipologias.csv")
fp_wl      <- here::here("config","qa_excepciones_he_vs_hc.csv")

# ───────── Canon región (sugerencias) ─────────
REG_CANON <- c(
  "ANDALUCÍA","ARAGÓN","ASTURIAS","BALEARES","CANARIAS","CANTABRIA","CASTILLA Y LEÓN",
  "CASTILLA-LA MANCHA","CATALUÑA","COMUNITAT VALENCIANA","EXTREMADURA","GALICIA",
  "LA RIOJA","COMUNIDAD DE MADRID","REGIÓN DE MURCIA","NAVARRA","PAÍS VASCO",
  "CEUTA","MELILLA","TOTAL NACIONAL","ESPAÑA","ESPANA","TOTAL"
)
canon_region <- function(x){
  k <- lower_noacc(x)
  k <- gsub("\\s+", " ", k); k <- gsub("-", " ", k)
  k <- gsub("castilla y leon", "CASTILLA Y LEÓN", k, fixed = TRUE)
  k <- gsub("castilla la mancha", "CASTILLA-LA MANCHA", k, fixed = TRUE)
  k <- gsub("comunidad de madrid", "COMUNIDAD DE MADRID", k, fixed = TRUE)
  k <- gsub("comunitat valenciana|valenciana", "COMUNITAT VALENCIANA", k)
  k <- gsub("pais vasco|país vasco", "PAÍS VASCO", k)
  k <- gsub("illes balears|islas baleares|baleares", "BALEARES", k)
  k <- gsub("cataluna|cataluña", "CATALUÑA", k)
  k <- gsub("region de murcia|regi[oó]n de murcia", "REGIÓN DE MURCIA", k)
  k <- gsub("^espana$|^españa$|^total nacional$|^total$", "TOTAL NACIONAL", k)
  cand <- toupper(k)
  ifelse(cand %in% REG_CANON, cand, NA_character_)
}
sugiere_tipo <- function(x){
  z <- lower_noacc(x); z <- gsub("\\s{2,}", " ", z); stringr::str_to_title(z)
}

# ───────── Lecturas RAW (si existen) ─────────
leer_raw_if <- function(fp){
  if (!file.exists(fp)) return(NULL)
  suppressMessages(
    readr::read_delim(fp, delim = ";", show_col_types = FALSE,
                      col_types = cols(.default = col_character()),
                      trim_ws = TRUE) |> janitor::clean_names()
  )
}
he_raw <- leer_raw_if(fp_he_raw)
hc_raw <- leer_raw_if(fp_hc_raw)

# ───────── Diccionario de REGIONES ─────────
get_reg_values <- function(df){
  if (is.null(df)) return(character())
  nms <- names(df)
  col_reg <- pick_col(nms, c("comun|ccaa|autono|region|ámbito|ambito|prov|municip"))
  if (is.na(col_reg)) return(character())
  sort(unique(na.omit(df[[col_reg]])))
}
reg_vals <- unique(c(get_reg_values(he_raw), get_reg_values(hc_raw)))
reg_df_new <- tibble(raw_region = reg_vals) |>
  mutate(propuesta_region = canon_region(raw_region))

map_reg_old <- if (file.exists(fp_map_reg)) readr::read_csv(fp_map_reg, show_col_types = FALSE) |> janitor::clean_names() else tibble(raw_region=character(), propuesta_region=character())
map_reg <- map_reg_old |>
  bind_rows(anti_join(reg_df_new, map_reg_old, by = "raw_region")) |>
  arrange(raw_region)

# ───────── Diccionario de TIPOLOGÍAS ─────────
get_tipo_values <- function(df){
  if (is.null(df)) return(character())
  nms <- names(df)
  col_tip <- pick_col(nms, c("tipolog|delit|infracc|\\btipo\\b|categoria"))
  if (is.na(col_tip)) return(character())
  sort(unique(na.omit(df[[col_tip]])))
}
tipo_vals <- unique(c(get_tipo_values(he_raw), get_tipo_values(hc_raw)))
tip_df_new <- tibble(raw_tipo = tipo_vals) |>
  mutate(propuesta_tipo = sugiere_tipo(raw_tipo))

map_tip_old <- if (file.exists(fp_map_tip)) readr::read_csv(fp_map_tip, show_col_types = FALSE) |> janitor::clean_names() else tibble(raw_tipo=character(), propuesta_tipo=character())
map_tip <- map_tip_old |>
  bind_rows(anti_join(tip_df_new, map_tip_old, by = "raw_tipo")) |>
  arrange(raw_tipo)

# ───────── Escritura de diccionarios ─────────
fs::dir_create(fs::path_dir(fp_map_reg))
readr::write_csv(map_reg, fp_map_reg, na = "")
readr::write_csv(map_tip, fp_map_tip, na = "")
msg("Diccionarios actualizados: config/map_regiones.csv, config/map_tipologias.csv")

# QC diccionarios
conf_reg <- map_reg %>% mutate(rkey = lower_noacc(raw_region)) %>% group_by(rkey) %>% filter(n_distinct(propuesta_region, na.rm = TRUE) > 1) %>% arrange(rkey)
conf_tip <- map_tip %>% mutate(tkey = lower_noacc(raw_tipo))   %>% group_by(tkey) %>% filter(n_distinct(propuesta_tipo,   na.rm = TRUE) > 1) %>% arrange(tkey)
pend_reg <- map_reg %>% filter(is.na(propuesta_region) | propuesta_region == "")
pend_tip <- map_tip %>% filter(is.na(propuesta_tipo)   | propuesta_tipo   == "")

readr::write_csv(bind_rows(
  conf_reg %>% mutate(diccionario="regiones")  %>% ungroup() %>% select(diccionario, raw = raw_region, propuestas = propuesta_region),
  conf_tip %>% mutate(diccionario="tipologias")%>% ungroup() %>% select(diccionario, raw = raw_tipo,   propuestas = propuesta_tipo)
), here::here("output","tables","02_dicc_conflictos.csv"))

readr::write_csv(bind_rows(
  pend_reg %>% mutate(diccionario="regiones")  %>% select(diccionario, raw = raw_region),
  pend_tip %>% mutate(diccionario="tipologias")%>% select(diccionario, raw = raw_tipo)
), here::here("output","tables","02_dicc_nuevos_pendientes.csv"))

# ───────── Whitelist HE vs HC (sugerencias) ─────────
sugerencias <- tibble()

if (!is.null(he_raw)) {
  # HE normalizado
  col_reg_he <- pick_col(names(he_raw), c("comun|ccaa|autono|region|ámbito|ambito|prov|municip"))
  col_tip_he <- pick_col(names(he_raw), c("tipolog|delit|infracc|\\btipo\\b|categoria"))
  col_per_he <- pick_col(names(he_raw), c("period|fecha|ano|año|anio"))
  col_val_he <- pick_col(names(he_raw), c("^total$|valor|numero|n$"))
  
  if (all(!is.na(c(col_reg_he, col_tip_he, col_per_he, col_val_he)))) {
    he_norm <- he_raw %>%
      transmute(
        region_raw = .data[[col_reg_he]],
        tipo_raw   = .data[[col_tip_he]],
        ano        = extract_year(.data[[col_per_he]]),
        he         = parse_num_es(.data[[col_val_he]])
      ) %>%
      left_join(map_reg %>% transmute(raw_region, propuesta_region),
                by = c("region_raw" = "raw_region")) %>%
      left_join(map_tip %>% transmute(raw_tipo,   propuesta_tipo),
                by = c("tipo_raw"   = "raw_tipo")) %>%
      mutate(
        region = coalesce(propuesta_region, region_raw),
        tipo   = coalesce(propuesta_tipo,   tipo_raw)
      ) %>%
      filter(!is.na(ano), between(ano, YEAR_MIN, YEAR_MAX), !is.na(he))
    
    # ---- PARCHE anti doble conteo: preferir fila TOTAL; si no existe, sumar CCAA ----
    agg_nacional_por_tipo <- function(df, val_col) {
      df %>%
        mutate(region_up = toupper(region),
               es_total  = region_up %in% c("TOTAL NACIONAL","TOTAL","ESPAÑA","ESPANA")) %>%
        group_by(ano, tipo) %>%
        summarise(
          valor = if (any(es_total, na.rm = TRUE)) {
            sum(.data[[val_col]][es_total], na.rm = TRUE)
          } else {
            sum(.data[[val_col]][!es_total], na.rm = TRUE)
          },
          .groups = "drop"
        )
    }
    he_nac_tipo <- agg_nacional_por_tipo(he_norm, "he") %>% rename(he = valor)
    
    # HC por tipo (ideal desde RAW)
    hc_nac_tipo <- tibble()
    if (!is.null(hc_raw)) {
      col_reg_hc <- pick_col(names(hc_raw), c("comun|ccaa|autono|region|ámbito|ambito|prov|municip"))
      col_tip_hc <- pick_col(names(hc_raw), c("tipolog|delit|infracc|\\btipo\\b|categoria"))
      col_per_hc <- pick_col(names(hc_raw), c("period|fecha|ano|año|anio"))
      col_val_hc <- pick_col(names(hc_raw), c("^total$|valor|numero|n$"))
      
      if (all(!is.na(c(col_reg_hc, col_tip_hc, col_per_hc, col_val_hc)))) {
        hc_norm <- hc_raw %>%
          transmute(
            region_raw = .data[[col_reg_hc]],
            tipo_raw   = .data[[col_tip_hc]],
            ano        = extract_year(.data[[col_per_hc]]),
            hc         = parse_num_es(.data[[col_val_hc]])
          ) %>%
          left_join(map_reg %>% transmute(raw_region, propuesta_region),
                    by = c("region_raw" = "raw_region")) %>%
          left_join(map_tip %>% transmute(raw_tipo,   propuesta_tipo),
                    by = c("tipo_raw"   = "raw_tipo")) %>%
          mutate(
            region = coalesce(propuesta_region, region_raw),
            tipo   = coalesce(propuesta_tipo,   tipo_raw)
          ) %>%
          filter(!is.na(ano), between(ano, YEAR_MIN, YEAR_MAX), !is.na(hc))
        
        hc_nac_tipo <- agg_nacional_por_tipo(hc_norm, "hc") %>% rename(hc = valor)
      }
    }
    
    if (nrow(hc_nac_tipo)) {
      comp <- he_nac_tipo %>%
        full_join(hc_nac_tipo, by = c("ano","tipo")) %>%
        filter(!is.na(he) & !is.na(hc)) %>%
        mutate(
          delta = he - hc,
          rel   = ifelse(hc > 0, delta / hc, NA_real_),
          supera = delta > 0
        )
      
      sugerencias <- comp %>%
        filter(supera, (rel <= TOLERANCIA_REL | delta <= TOLERANCIA_ABS)) %>%
        transmute(
          ano,
          tipo,
          motivo = paste0("supera_por_poco (Δ=", delta, ", rel=", round(rel*100,2), "%)")
        )
    } else if (file.exists(fp_hc_tot)) {
      hc_tot <- readr::read_csv(fp_hc_tot, show_col_types = FALSE) %>%
        janitor::clean_names() %>%
        transmute(ano = as.integer(ano), hc_total = parse_num_es(total)) %>%
        filter(!is.na(ano), between(ano, YEAR_MIN, YEAR_MAX))
      he_tot <- he_nac_tipo %>% group_by(ano) %>% summarise(he_total = sum(he, na.rm = TRUE), .groups = "drop")
      comp2 <- he_tot %>% inner_join(hc_tot, by = "ano") %>%
        mutate(delta = he_total - hc_total, rel = ifelse(hc_total > 0, delta/hc_total, NA_real_))
      sugerencias <- comp2 %>%
        filter(delta > 0, (rel <= TOLERANCIA_REL | delta <= TOLERANCIA_ABS)) %>%
        transmute(ano, tipo = "(TOTAL)", motivo = paste0("supera_por_poco (Δ=", delta, ", rel=", round(rel*100,2), "%)"))
    }
  }
}

# Fusionar whitelist
wl_old <- if (file.exists(fp_wl)) readr::read_csv(fp_wl, show_col_types = FALSE) %>% janitor::clean_names() else tibble(ano=integer(), tipo=character(), motivo=character())
wl_new <- bind_rows(wl_old, anti_join(sugerencias, wl_old, by = c("ano","tipo"))) %>% arrange(ano, tipo)

readr::write_csv(wl_new, fp_wl, na = "")
readr::write_csv(sugerencias, here::here("output","tables","02_whitelist_sugerencias.csv"), na = "")

# ───────── Resumen final ─────────
qc_resumen <- tibble(
  diccionario = c("regiones","tipologias"),
  filas_total = c(nrow(map_reg), nrow(map_tip)),
  pendientes  = c(nrow(pend_reg), nrow(pend_tip)),
  conflictos  = c(nrow(conf_reg), nrow(conf_tip))
)
readr::write_csv(qc_resumen, here::here("output","tables","02_dicc_resumen.csv"))

msg("Diccionarios -> 02_dicc_resumen.csv; pendientes -> 02_dicc_nuevos_pendientes.csv; conflictos -> 02_dicc_conflictos.csv")
msg("Whitelist actualizada -> config/qa_excepciones_he_vs_hc.csv (sugerencias en 02_whitelist_sugerencias.csv)")
msg("✅ 02_config_diccionarios_y_excepciones completado.")
