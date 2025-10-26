#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
################################################################################
# 13_build_hechos_conocidos.R — Builder unificado (HC limpio + cierre + unidades + QA)
#
# IN (RAW):  data/raw/hechos_conocidos.csv
#            columnas esperadas (flexibles): regiones | tipologia | periodo | total
#            (Opcional) config/map_regiones.csv, config/map_tipologias.csv
#
# OUT (PROC):
#   data/processed/hechos_conocidos_panel_ccaa.csv                 (ano, region, tipo, valor)
#   data/processed/hechos_conocidos_total_nacional.csv             (ano, total)
#   data/processed/hechos_conocidos_total_nacional_por_tipo.csv    (ano, tipo, total)
#
# OUT (QA):
#   output/tables/13_qc_cobertura_anual.csv
#   output/tables/13_qc_duplicados_clave.csv
#   output/tables/13_qc_no_negatividad.csv
#   output/tables/13_qc_unidades_detectadas.csv
#   output/tables/13_qc_nacional_vs_suma_ccaa.csv
#   output/tables/13_qc_summary_checks.csv
#
# Flags/env:
#   DRY_RUN_UNIDADES  = TRUE|FALSE   (default TRUE → no reescala aunque sospeche miles)
#   EMIT_REGIONAL     = TRUE|FALSE   (default TRUE)
#   EMIT_TIPOLOGIA    = TRUE|FALSE   (default TRUE)
#   TOL_REL_TOTAL     = 0.005        (0.5% tolerancia para aceptar TOTAL del RAW)
#   TOL_ABS_TOTAL     = 100          (100 casos tolerancia absoluta)
################################################################################

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(janitor); library(stringr)
  library(tidyr); library(here); library(tibble)
})

# -------------------------- Config -------------------------------------------
YEAR_MIN <- 2010L; YEAR_MAX <- 2023L
DRY_RUN_UNIDADES <- as.logical(Sys.getenv("DRY_RUN_UNIDADES", "TRUE"))
EMIT_REGIONAL    <- as.logical(Sys.getenv("EMIT_REGIONAL", "TRUE"))
EMIT_TIPOLOGIA   <- as.logical(Sys.getenv("EMIT_TIPOLOGIA", "TRUE"))

TOL_REL_TOTAL <- as.numeric(Sys.getenv("TOL_REL_TOTAL", "0.005"))  # 0.5%
TOL_ABS_TOTAL <- as.numeric(Sys.getenv("TOL_ABS_TOTAL", "100"))    # 100 casos

fp_in              <- here::here("data","raw","hechos_conocidos.csv")
fp_map_reg         <- here::here("config","map_regiones.csv")
fp_map_tip         <- here::here("config","map_tipologias.csv")

fp_out_panel       <- here::here("data","processed","hechos_conocidos_panel_ccaa.csv")
fp_out_total       <- here::here("data","processed","hechos_conocidos_total_nacional.csv")
fp_out_total_tipo  <- here::here("data","processed","hechos_conocidos_total_nacional_por_tipo.csv")

qa_dir <- here::here("output","tables")
dir.create(dirname(fp_out_panel), recursive = TRUE, showWarnings = FALSE)
dir.create(qa_dir, recursive = TRUE, showWarnings = FALSE)

# -------------------------- Utils comunes ------------------------------------
source(here::here("scripts","00_utils_limpieza.R"))
root_init("scripts/13_build_hechos_conocidos.R")
msg <- function(...) message("[13] ", paste0(...))

# selección flexible de columnas
pick_col <- function(nms, patterns){
  ix <- unique(unlist(lapply(patterns, function(p) stringr::str_which(nms, stringr::regex(p, ignore_case = TRUE)))))
  if (length(ix)) nms[ix[1]] else NA_character_
}

# regiones “especiales” a excluir del panel CCAA para evitar doble conteo
is_special_region <- function(x){
  z <- tolower(norm_ascii(x))
  z %in% c("total nacional","total","espana","españa","en el extranjero","desconocida")
}

# heurística de escala en miles
detect_scale_thousands <- function(v){
  v2 <- v[is.finite(v)]
  if (!length(v2)) return(FALSE)
  q50 <- stats::median(v2); q95 <- stats::quantile(v2, 0.95, names = FALSE)
  (q50 > 50 & q50 < 2e3 & q95 < 2e4)
}

# -------------------------- Lectura ------------------------------------------
if (!file.exists(fp_in)) stop("No existe: ", fp_in)
raw <- read_raw(fp_in) |> janitor::clean_names()
nms <- names(raw)

col_reg <- pick_col(nms, c("comun|ccaa|autono|region|ámbito|ambito|prov|municip"))
col_tip <- pick_col(nms, c("tipolog|delit|infracc|\\btipo\\b|categoria"))
col_per <- pick_col(nms, c("period|fecha|ano|año|anio|year|time"))
col_val <- pick_col(nms, c("^total$|valor|numero|n$|n_"))

if (any(is.na(c(col_reg,col_tip,col_per,col_val)))) {
  stop("Faltan columnas clave. Detectadas: ", paste(nms, collapse=", "))
}

# -------------------------- Limpieza base ------------------------------------
df0 <- raw |>
  transmute(
    ano        = extract_year(.data[[col_per]]),
    region_raw = norm_ascii(.data[[col_reg]]),
    tipo_raw   = norm_ascii(.data[[col_tip]]),
    valor      = limpia_num(.data[[col_val]])
  ) |>
  filter(!is.na(ano), dplyr::between(ano, YEAR_MIN, YEAR_MAX))

# -------------------------- Mapping región/tipo (si existen) -----------------
map_reg <- if (file.exists(fp_map_reg)) readr::read_csv(fp_map_reg, show_col_types = FALSE) |> janitor::clean_names() else tibble()
map_tip <- if (file.exists(fp_map_tip)) readr::read_csv(fp_map_tip, show_col_types = FALSE) |> janitor::clean_names() else tibble()

if (nrow(map_reg) && all(c("raw_region","propuesta_region") %in% names(map_reg))) {
  df0 <- df0 |>
    left_join(map_reg |> select(raw_region, propuesta_region),
              by = c("region_raw" = "raw_region")) |>
    mutate(region = dplyr::coalesce(propuesta_region, region_raw))
} else {
  df0$region <- df0$region_raw
}

if (nrow(map_tip) && all(c("raw_tipo","propuesta_tipo") %in% names(map_tip))) {
  df0 <- df0 |>
    left_join(map_tip |> select(raw_tipo, propuesta_tipo),
              by = c("tipo_raw" = "raw_tipo")) |>
    mutate(tipo = dplyr::coalesce(propuesta_tipo, tipo_raw))
} else {
  df0$tipo <- df0$tipo_raw
}

df1 <- df0 |> select(ano, region, tipo, valor)

# -------------------------- Normaliza etiqueta de TOTAL ----------------------
df1 <- df1 |>
  mutate(region_up = toupper(region)) |>
  mutate(region_up = dplyr::case_when(
    region_up %in% c("TOTAL","ESPAÑA","ESPANA","TOTAL NACIONAL") ~ "TOTAL NACIONAL",
    TRUE ~ region_up
  ))

# -------------------------- Panel CCAA (excluye especiales) ------------------
panel_ccaa <- df1 |>
  filter(!is_special_region(region_up)) |>
  group_by(ano, region = region_up, tipo) |>
  summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop")

# -------------------------- Nacional por tipología ---------------------------
#  (1) Suma CCAA válidas
nac_sum <- panel_ccaa |>
  group_by(ano, tipo) |>
  summarise(suma_ccaa = sum(valor, na.rm = TRUE), .groups = "drop")

#  (2) Si en el RAW venía TOTAL NACIONAL por tipo, compáralo y acepta si ≈ suma
nac_raw <- df1 |>
  filter(region_up == "TOTAL NACIONAL") |>
  group_by(ano, tipo) |>
  summarise(total_raw = sum(valor, na.rm = TRUE), .groups = "drop")

nac_join <- full_join(nac_sum, nac_raw, by = c("ano","tipo")) |>
  mutate(
    delta = total_raw - suma_ccaa,
    rel   = ifelse(!is.na(total_raw) & suma_ccaa > 0, abs(delta)/pmax(1, suma_ccaa), NA_real_),
    usar_raw = !is.na(total_raw) & (abs(delta) <= TOL_ABS_TOTAL | rel <= TOL_REL_TOTAL),
    total_nac_tipo = ifelse(usar_raw, total_raw, suma_ccaa)
  ) |>
  arrange(ano, tipo)

# -------------------------- Total nacional (todos los tipos) -----------------
total_nacional <- nac_join |>
  group_by(ano) |>
  summarise(total = sum(total_nac_tipo, na.rm = TRUE), .groups = "drop") |>
  arrange(ano)

# -------------------------- Detección/ajuste de unidades ---------------------
det_unidades <- tibble(
  nivel = c("panel_ccaa","nac_por_tipo","total_nacional"),
  sospecha_miles = c(
    detect_scale_thousands(panel_ccaa$valor),
    detect_scale_thousands(nac_join$total_nac_tipo),
    detect_scale_thousands(total_nacional$total)
  )
)
write_clean(det_unidades, file.path(qa_dir, "13_qc_unidades_detectadas.csv"))

if (any(det_unidades$sospecha_miles)) {
  if (DRY_RUN_UNIDADES) {
    msg("Sospecha de escala en miles → DRY-RUN (no reescalo).")
  } else {
    msg("Sospecha de escala en miles → reescalando x1000 …")
    panel_ccaa     <- panel_ccaa     |> mutate(valor = valor * 1000)
    nac_join       <- nac_join       |> mutate(total_nac_tipo = total_nac_tipo * 1000)
    total_nacional <- total_nacional |> mutate(total = total * 1000)
  }
}

# -------------------------- QA -----------------------------------------------
# Cobertura anual
cov_anos <- tibble(ano = YEAR_MIN:YEAR_MAX) |>
  left_join(total_nacional, by = "ano") |>
  mutate(presente = !is.na(total))
write_clean(cov_anos, file.path(qa_dir, "13_qc_cobertura_anual.csv"))

# Duplicados por clave en panel
dups <- panel_ccaa |>
  count(ano, region, tipo) |>
  filter(n > 1)
write_clean(dups, file.path(qa_dir, "13_qc_duplicados_clave.csv"))

# No-negatividad
neg <- panel_ccaa |> filter(is.finite(valor) & valor < 0)
write_clean(neg, file.path(qa_dir, "13_qc_no_negatividad.csv"))

# Comparativa TOTAL RAW vs SUMA CCAA (diagnóstico)
cmp_total_vs_suma <- nac_join |>
  transmute(ano, tipo, suma_ccaa, total_raw, delta = total_raw - suma_ccaa,
            rel = ifelse(suma_ccaa > 0 & !is.na(total_raw), (total_raw - suma_ccaa)/suma_ccaa, NA_real_),
            usar_raw)
write_clean(cmp_total_vs_suma, file.path(qa_dir, "13_qc_nacional_vs_suma_ccaa.csv"))

# Summary compacto
sum_rows <- tibble(
  test = c(
    "Cobertura años total_nacional",
    "Duplicados panel_ccaa",
    "No-negatividad",
    "Posible escala en miles",
    "TOTAL del RAW ≈ suma CCAA (conteo de aceptados)"
  ),
  status = c(
    if (all(cov_anos$presente, na.rm = TRUE)) "PASS" else "WARN",
    if (nrow(dups) == 0) "PASS" else "FAIL",
    if (nrow(neg)  == 0) "PASS" else "FAIL",
    if (any(det_unidades$sospecha_miles)) if (DRY_RUN_UNIDADES) "WARN" else "INFO" else "PASS",
    "INFO"
  ),
  detalle = c(
    sprintf("años con valor: %d/%d", sum(cov_anos$presente, na.rm = TRUE), length(YEAR_MIN:YEAR_MAX)),
    paste0("duplicados: ", nrow(dups)),
    paste0("negativos: ", nrow(neg)),
    paste0("nivel: ", paste(det_unidades$nivel[det_unidades$sospecha_miles], collapse=", ")),
    paste0("tipos aceptados por tolerancia: ", sum(nac_join$usar_raw, na.rm = TRUE))
  ),
  output = c(
    file.path(qa_dir, "13_qc_cobertura_anual.csv"),
    file.path(qa_dir, "13_qc_duplicados_clave.csv"),
    file.path(qa_dir, "13_qc_no_negatividad.csv"),
    file.path(qa_dir, "13_qc_unidades_detectadas.csv"),
    file.path(qa_dir, "13_qc_nacional_vs_suma_ccaa.csv")
  )
)
write_clean(sum_rows, file.path(qa_dir, "13_qc_summary_checks.csv"))

# -------------------------- Escritura salidas --------------------------------
if (EMIT_REGIONAL) {
  write_clean(panel_ccaa |> arrange(ano, region, tipo), fp_out_panel)
  msg("Escrito panel_ccaa → ", fp_out_panel)
}
if (EMIT_TIPOLOGIA) {
  write_clean(nac_join |> select(ano, tipo, total = total_nac_tipo) |> arrange(ano, tipo),
              fp_out_total_tipo)
  msg("Escrito nacional_por_tipo → ", fp_out_total_tipo)
}
write_clean(total_nacional, fp_out_total)
msg("Escrito total_nacional → ", fp_out_total)

msg("✅ HC listo (builder unificado). QA en output/tables/13_qc_*.csv")
