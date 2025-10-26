#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
################################################################################
# 13_build_hechos_conocidos.R — Builder unificado (HC limpio + cierre + unidades + QA)
# Proyecto   : Delitos e Inmigración en España (2010–2023)
# Autor      : Jesús Castro · JESUSCASTRODATA
# Licencia   : MIT (código) · CC BY 4.0 (docs/figuras)
# Fecha      : `r format(Sys.Date())`
# Descripción:
#   Lee el CSV bruto de Hechos Conocidos, normaliza columnas y etiquetas, construye:
#     (a) panel CCAA (excluyendo regiones especiales),
#     (b) total nacional por tipología (sin duplicar la fila TOTAL), y
#     (c) total nacional correcto (preferentemente desde la fila TOTAL del RAW).
#   Incluye detección de unidades (miles), QA de cobertura/duplicados/negativos y
#   comparativa TOTAL (fila) vs suma de categorías (solo diagnóstico).
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

# -------------------------- Utils --------------------------------------------
source(here::here("scripts","00_utils_limpieza.R"))
root_init("scripts/13_build_hechos_conocidos.R")
msg <- function(...) message("[13] ", paste0(...))

pick_col <- function(nms, patterns){
  ix <- unique(unlist(lapply(patterns, function(p) stringr::str_which(nms, stringr::regex(p, ignore_case = TRUE)))))
  if (length(ix)) nms[ix[1]] else NA_character_
}
is_special_region <- function(x){
  z <- tolower(norm_ascii(x))
  z %in% c("total nacional","total","espana","españa","en el extranjero","desconocida")
}
detect_scale_thousands <- function(v){
  v2 <- v[is.finite(v)]
  if (!length(v2)) return(FALSE)
  q50 <- stats::median(v2); q95 <- stats::quantile(v2, 0.95, names = FALSE)
  (q50 > 50 & q50 < 2e3 & q95 < 2e4)
}

# ——— NUEVO: detector robusto de “TOTAL …” por tipología ———
is_total_tipo <- function(x){
  x2 <- tolower(stringr::str_squish(x))
  grepl("^total(\\b|\\s)", x2) |
    grepl("^total\\s+infracciones(\\b|\\s)", x2) |
    grepl("^total\\s+infracciones\\s+penales(\\b|\\s)", x2) |
    grepl("^total\\s+delitos(\\b|\\s)", x2)
}

# ——— NUEVO: nivel jerárquico del prefijo numérico de la tipología ———
# "1" → 1, "1.2" → 2, "1.2.3" → 3, NA si no hay prefijo numérico
tipo_nivel <- function(x){
  code <- stringr::str_extract(x, "^[0-9]+(\\.[0-9]+)*")
  vapply(strsplit(code, "\\."), function(z){
    if (length(z) == 0 || all(is.na(z))) NA_integer_ else length(z)
  }, integer(1))
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
  dplyr::transmute(
    ano        = extract_year(.data[[col_per]]),
    region_raw = norm_ascii(.data[[col_reg]]),
    tipo_raw   = norm_ascii(.data[[col_tip]]),
    valor      = limpia_num(.data[[col_val]])
  ) |>
  dplyr::filter(!is.na(ano), dplyr::between(ano, YEAR_MIN, YEAR_MAX))

# -------------------------- Mapping región/tipo (si existen) -----------------
map_reg <- if (file.exists(fp_map_reg)) readr::read_csv(fp_map_reg, show_col_types = FALSE) |> janitor::clean_names() else tibble()
map_tip <- if (file.exists(fp_map_tip)) readr::read_csv(fp_map_tip, show_col_types = FALSE) |> janitor::clean_names() else tibble()

# Región
if (nrow(map_reg) && all(c("raw_region","propuesta_region") %in% names(map_reg))) {
  df0 <- df0 |>
    dplyr::left_join(
      map_reg |> dplyr::select(raw_region, propuesta_region),
      by = c("region_raw" = "raw_region")
    ) |>
    dplyr::mutate(region = dplyr::coalesce(propuesta_region, region_raw)) |>
    dplyr::select(-propuesta_region)
} else {
  df0$region <- df0$region_raw
}
# Tipología
if (nrow(map_tip) && all(c("raw_tipo","propuesta_tipo") %in% names(map_tip))) {
  df0 <- df0 |>
    dplyr::left_join(
      map_tip |> dplyr::select(raw_tipo, propuesta_tipo),
      by = c("tipo_raw" = "raw_tipo")
    ) |>
    dplyr::mutate(tipo = dplyr::coalesce(propuesta_tipo, tipo_raw)) |>
    dplyr::select(-propuesta_tipo)
} else {
  df0$tipo <- df0$tipo_raw
}

df1 <- df0 |> dplyr::select(ano, region, tipo, valor)

# -------------------------- Normaliza etiqueta de TOTAL ----------------------
df1 <- df1 |>
  dplyr::mutate(region_up = toupper(region)) |>
  dplyr::mutate(region_up = dplyr::case_when(
    region_up %in% c("TOTAL","ESPAÑA","ESPANA","TOTAL NACIONAL") ~ "TOTAL NACIONAL",
    TRUE ~ region_up
  ))

# -------------------------- Panel CCAA (excluye especiales) ------------------
panel_ccaa <- df1 |>
  dplyr::filter(!is_special_region(region_up)) |>
  dplyr::group_by(ano, region = region_up, tipo) |>
  dplyr::summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop")

# -------------------------- Nacional por tipología ---------------------------
#  (1) Suma CCAA válidas
nac_sum <- panel_ccaa |>
  dplyr::group_by(ano, tipo) |>
  dplyr::summarise(suma_ccaa = sum(valor, na.rm = TRUE), .groups = "drop")
#  (2) TOTAL NACIONAL por tipo del RAW (si existe)
nac_raw <- df1 |>
  dplyr::filter(region_up == "TOTAL NACIONAL") |>
  dplyr::group_by(ano, tipo) |>
  dplyr::summarise(total_raw = sum(valor, na.rm = TRUE), .groups = "drop")

nac_join <- dplyr::full_join(nac_sum, nac_raw, by = c("ano","tipo")) |>
  dplyr::mutate(
    delta = total_raw - suma_ccaa,
    rel   = dplyr::if_else(!is.na(total_raw) & suma_ccaa > 0, abs(delta)/pmax(1, suma_ccaa), NA_real_),
    usar_raw = !is.na(total_raw) & (abs(delta) <= TOL_ABS_TOTAL | rel <= TOL_REL_TOTAL),
    total_nac_tipo = dplyr::if_else(usar_raw, total_raw, suma_ccaa)
  ) |>
  dplyr::arrange(ano, tipo)

# -------------------------- Total nacional (uso correcto de TOTAL) -----------
# (A) Total nacional desde la fila TOTAL (preferido)
nac_total_row <- df1 |>
  dplyr::filter(region_up == "TOTAL NACIONAL", is_total_tipo(tipo)) |>
  dplyr::group_by(ano) |>
  dplyr::summarise(total = sum(valor, na.rm = TRUE), .groups = "drop")

# (B) Fallback: suma SOLO categorías de nivel 1 (partición no solapada)
nac_sum_excl_total <- panel_ccaa |>
  dplyr::mutate(nivel_tipo = tipo_nivel(tipo)) |>
  dplyr::filter(!is_total_tipo(tipo), nivel_tipo == 1) |>
  dplyr::group_by(ano) |>
  dplyr::summarise(total = sum(valor, na.rm = TRUE), .groups = "drop")

# Decide qué usar + log
total_nacional <- if (nrow(nac_total_row)) nac_total_row else nac_sum_excl_total
if (nrow(nac_total_row)) {
  msg("Total nacional: usando fila TOTAL del RAW (preferido).")
} else {
  msg("Total nacional: sin fila TOTAL → usando suma de categorías de nivel 1 (fallback).")
}

# -------------------------- QA: TOTAL vs suma categorías ---------------------
qa_total_check <- nac_sum_excl_total |>
  dplyr::rename(sum_categorias = total) |>
  dplyr::inner_join(nac_total_row, by = "ano") |>
  dplyr::mutate(inflation_factor = sum_categorias / total)
write_clean(qa_total_check, file.path(qa_dir, "13_qc_total_vs_suma_categorias.csv"))

if (nrow(qa_total_check)) {
  med_fac <- stats::median(qa_total_check$inflation_factor, na.rm = TRUE)
  msg(sprintf("WARN: suma categorías vs TOTAL (fila) → factor mediano=%.2f. Usando SIEMPRE la fila TOTAL para 'total_nacional'.", med_fac))
}

# -------------------------- Detección/ajuste de unidades ---------------------
det_unidades <- tibble::tibble(
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
    panel_ccaa     <- panel_ccaa     |> dplyr::mutate(valor = valor * 1000)
    nac_join       <- nac_join       |> dplyr::mutate(total_nac_tipo = total_nac_tipo * 1000)
    total_nacional <- total_nacional |> dplyr::mutate(total = total * 1000)
  }
}

# -------------------------- QA resumida --------------------------------------
cov_anos <- tibble::tibble(ano = YEAR_MIN:YEAR_MAX) |>
  dplyr::left_join(total_nacional, by = "ano") |>
  dplyr::mutate(presente = !is.na(total))
write_clean(cov_anos, file.path(qa_dir, "13_qc_cobertura_anual.csv"))

dups <- panel_ccaa |> dplyr::count(ano, region, tipo) |> dplyr::filter(n > 1)
write_clean(dups, file.path(qa_dir, "13_qc_duplicados_clave.csv"))

neg <- panel_ccaa |> dplyr::filter(is.finite(valor) & valor < 0)
write_clean(neg, file.path(qa_dir, "13_qc_no_negatividad.csv"))

cmp_total_vs_suma <- nac_join |>
  dplyr::transmute(ano, tipo, suma_ccaa, total_raw,
                   delta = total_raw - suma_ccaa,
                   rel = dplyr::if_else(suma_ccaa > 0 & !is.na(total_raw),
                                        (total_raw - suma_ccaa)/suma_ccaa, NA_real_),
                   usar_raw)
write_clean(cmp_total_vs_suma, file.path(qa_dir, "13_qc_nacional_vs_suma_ccaa.csv"))

sum_rows <- tibble::tibble(
  test = c(
    "Cobertura años total_nacional",
    "Duplicados panel_ccaa",
    "No-negatividad",
    "Posible escala en miles",
    "TOTAL del RAW ≈ suma CCAA (conteo de aceptados)",
    "TOTAL (fila) vs Suma categorías (inflation factor)"
  ),
  status = c(
    if (all(cov_anos$presente, na.rm = TRUE)) "PASS" else "WARN",
    if (nrow(dups) == 0) "PASS" else "FAIL",
    if (nrow(neg)  == 0) "PASS" else "FAIL",
    if (any(det_unidades$sospecha_miles)) if (DRY_RUN_UNIDADES) "WARN" else "INFO" else "PASS",
    "INFO",
    "INFO"
  ),
  detalle = c(
    sprintf("años con valor: %d/%d", sum(cov_anos$presente, na.rm = TRUE), length(YEAR_MIN:YEAR_MAX)),
    paste0("duplicados: ", nrow(dups)),
    paste0("negativos: ", nrow(neg)),
    paste0("nivel: ", paste(det_unidades$nivel[det_unidades$sospecha_miles], collapse=", ")),
    paste0("tipos aceptados por tolerancia: ", sum(nac_join$usar_raw, na.rm = TRUE)),
    if (nrow(qa_total_check)) sprintf("mediana factor: %.2f", stats::median(qa_total_check$inflation_factor, na.rm = TRUE)) else "NA"
  ),
  output = c(
    file.path(qa_dir, "13_qc_cobertura_anual.csv"),
    file.path(qa_dir, "13_qc_duplicados_clave.csv"),
    file.path(qa_dir, "13_qc_no_negatividad.csv"),
    file.path(qa_dir, "13_qc_unidades_detectadas.csv"),
    file.path(qa_dir, "13_qc_nacional_vs_suma_ccaa.csv"),
    file.path(qa_dir, "13_qc_total_vs_suma_categorias.csv")
  )
)
write_clean(sum_rows, file.path(qa_dir, "13_qc_summary_checks.csv"))

# -------------------------- Escritura salidas --------------------------------
if (EMIT_REGIONAL) {
  write_clean(panel_ccaa |> dplyr::arrange(ano, region, tipo), fp_out_panel)
  msg("Escrito panel_ccaa → ", fp_out_panel)
}
if (EMIT_TIPOLOGIA) {
  write_clean(
    nac_join |> dplyr::filter(!is_total_tipo(tipo)) |>
      dplyr::select(ano, tipo, total = total_nac_tipo) |>
      dplyr::arrange(ano, tipo),
    fp_out_total_tipo
  )
  msg("Escrito nacional_por_tipo → ", fp_out_total_tipo)
}
write_clean(total_nacional, fp_out_total)
msg("Escrito total_nacional → ", fp_out_total)

msg("✅ HC listo (builder unificado). QA en output/tables/13_qc_*.csv")

# ───────────────────── Series “5 delitos violentos” (anual) ─────────────────────
# Fuente: nacional por tipo (evitamos duplicar TOTAL y subtipos, ver mapeo)
tipo_src <- nac_join |>
  dplyr::filter(!is_total_tipo(tipo)) |>
  dplyr::select(ano, tipo, total = total_nac_tipo)

# Mapeo a las 5 categorías finales (suma 3.1 y 3.2; usa 5.3 total; elige 1.3 frente a 2.1)
map_delito5 <- tibble::tribble(
  ~tipo,                                                     ~delito5,                                    ~peso,
  "1.1.1.-homicidios dolosos/asesinatos consumados",         "Homicidios dolosos y asesinatos consumados", 1,
  "1.2.-lesiones",                                           "Lesiones graves",                             1,
  "3.1.-agresion sexual",                                    "Agresiones sexuales (total)",                 1,
  "3.2.-agresion sexual con penetracion",                    "Agresiones sexuales (total)",                 1,
  "5.3.-robos con violencia o intimidacion",                 "Robos con violencia e intimidación",          1,
  "1.3.-malos tratos ambito familiar",                       "Malos tratos en el ámbito familiar",          1
)

violentos5_largo <- tipo_src |>
  dplyr::inner_join(map_delito5, by = "tipo") |>
  dplyr::mutate(total = dplyr::coalesce(total, 0) * peso) |>
  dplyr::group_by(ano, delito5) |>
  dplyr::summarise(total = sum(total, na.rm = TRUE), .groups = "drop") |>
  dplyr::arrange(ano, delito5)

violentos5_ancho <- tidyr::pivot_wider(
  violentos5_largo, names_from = delito5, values_from = total
) |>
  dplyr::arrange(ano)

# Guardado
write_clean(violentos5_largo, file.path(qa_dir, "13_violentos5_series_anual_largo.csv"))
write_clean(violentos5_ancho, file.path(qa_dir, "13_violentos5_series_anual_ancho.csv"))

# (Opcional) sumar un renglón de INFO al summary de QC
try({
  extra_row <- tibble::tibble(
    test    = "Series 5 delitos violentos (anual)",
    status  = "INFO",
    detalle = paste0("Años: ", paste(range(violentos5_largo$ano), collapse = "–"),
                     " | categorías: ", paste(unique(violentos5_largo$delito5), collapse = " · ")),
    output  = file.path(qa_dir, "13_violentos5_series_anual_largo.csv")
  )
  sum_rows <- dplyr::bind_rows(sum_rows, extra_row)
}, silent = TRUE)
# ──────────────────────────────────────────────────────────────────────────────








