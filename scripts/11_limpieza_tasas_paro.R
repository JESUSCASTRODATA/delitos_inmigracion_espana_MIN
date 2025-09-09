#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
###############################################################################
# 11_limpieza_paro_16_29.R — Paro 16–29 anual (ponderado con población)
#
# Entradas:
#   RAW   : data/raw/tasas_paro.csv          (Edad;Sexo;Periodo;Total)
#   PESOS : data/processed/poblacion_15_29_bins_anual.csv
#
# Salidas:
#   data/processed/paro_15_29_anual.csv  (ano, paro_15_29, metodo)
#   output/tables/qc_paro_parse_periodo_samples.csv
#   output/tables/qc_paro_bins_cobertura.csv
#   output/tables/qc_paro_rates_year.csv
#   output/tables/qc_paro_pesos_adj.csv
#   output/tables/qc_paro_join_matrix.csv
#   output/tables/qc_paro_cobertura_anual_15_29.csv
#
# Nota: se llama "paro_15_29" por compatibilidad, pero el cálculo es 16–29
#       (ajuste 4/5 de la banda 15–19).
#
# Autor: Jesús Castro
# Última actualización: 2025-08-22
###############################################################################

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(here); library(janitor)
  library(stringr); library(tidyr); library(purrr); library(rlang)
})

source(here("scripts","00_utils_limpieza.R"))
# PROJECT_ROOT_HINT <- "D:/delitos_inmigracion_espana"  # desactivado: usamos .here/.Rproj
root_init("scripts/11_limpieza_paro_16_29.R", project_root_hint = PROJECT_ROOT_HINT)

anos_target <- 2010:2023
dir.create(here("output","tables"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("data","processed"), recursive = TRUE, showWarnings = FALSE)

# --------------------------- Lectura -----------------------------------------
fp_raw   <- here("data","raw","tasas_paro.csv")
fp_pesos <- here("data","processed","poblacion_15_29_bins_anual.csv")
assert_infile(fp_raw); assert_infile(fp_pesos)

raw <- read_raw(fp_raw)  # añade attrs y clean_names
nms <- names(raw)

pick_col <- function(patterns, nms){
  ix <- which(Reduce(`|`, lapply(patterns, function(p) grepl(p, nms, ignore.case = TRUE))))
  if (length(ix)) nms[ix[1]] else NA_character_
}

col_edad <- pick_col(c("^edad$","grupo.*edad"), nms)
col_sexo <- pick_col(c("^sexo$"), nms)
col_per  <- pick_col(c("^periodo$","period"), nms)
col_val  <- pick_col(c("^total$","tasa|valor|numero|n$"), nms)

if (any(is.na(c(col_edad,col_sexo,col_per,col_val)))) {
  abort("Faltan columnas esperadas en tasas_paro.csv. Detectadas: ", paste(nms, collapse=", "))
}

# --------------------------- Helpers -----------------------------------------
norm_chr <- function(x){
  x0 <- as.character(x)
  x0 <- chartr("\u00A0", " ", x0)  # NBSP -> espacio
  x0 <- gsub("[\t\r\n]+", " ", x0)
  x0 <- gsub("\\s+", " ", x0)
  trimws(x0)
}
safe_parse_number <- function(x){
  x0 <- norm_chr(x)
  x0 <- gsub("\\.", "", x0)  # quita miles
  x0 <- gsub(",", ".", x0)   # coma -> punto
  suppressWarnings(as.numeric(x0))
}
std_sexo_total <- function(x){
  x0 <- tolower(norm_chr(x))
  grepl("^total|^ambos", x0)
}
std_edad_band <- function(x){
  x0 <- tolower(norm_chr(x))
  x0 <- gsub("\u2013|\u2014", "-", x0)  # guiones raros
  case_when(
    grepl("^de\\s*16\\s*a\\s*19", x0) ~ "16-19",
    grepl("^de\\s*20\\s*a\\s*24", x0) ~ "20-24",
    grepl("^de\\s*25\\s*a\\s*29", x0) ~ "25-29",
    TRUE                               ~ NA_character_
  )
}
# ¡Arreglo importante!: no usar \\b\\d{4}\\b (no casa con 2025T2)
parse_periodo_tri <- function(p){
  p0  <- tolower(norm_chr(p))
  ano <- suppressWarnings(as.integer(stringr::str_match(p0, "([12][0-9]{3})")[,2]))
  tri1 <- suppressWarnings(as.integer(stringr::str_match(p0, "t\\s*([1-4])")[,2]))
  tri2 <- suppressWarnings(as.integer(stringr::str_match(p0, "[12][0-9]{3}[^0-9]*([1-4])$")[,2]))
  tri  <- ifelse(is.na(tri1), tri2, tri1)
  tibble::tibble(ano = ano, trimestre = tri)
}

# ------------------------- Preparación de tasas ------------------------------
df <- raw |>
  transmute(
    edad    = .data[[col_edad]],
    sexo    = .data[[col_sexo]],
    periodo = .data[[col_per]],
    tasa_ch = .data[[col_val]]
  ) |>
  filter(std_sexo_total(sexo)) |>
  mutate(
    edad_band = std_edad_band(edad),
    tasa      = safe_parse_number(tasa_ch)
  ) |>
  filter(!is.na(edad_band))

yt <- parse_periodo_tri(df$periodo)

# Muestra de parseo
write_clean(
  tibble::tibble(periodo = df$periodo, ano = yt$ano, trimestre = yt$trimestre) |> head(50),
  here("output","tables","qc_paro_parse_periodo_samples.csv")
)

df2 <- df |>
  bind_cols(yt) |>
  filter(!is.na(ano), !is.na(trimestre)) |>
  filter(ano %in% anos_target) |>
  select(ano, trimestre, edad_band, tasa) |>
  arrange(ano, trimestre, edad_band)

# Traza útil si algo falla
if (nrow(df2) == 0) {
  write_clean(distinct(df, sexo, edad, periodo) |> head(200),
              here("output","tables","qc_paro_debug_unique_combos.csv"))
  stop("No se generó ningún registro trimestral tras el parseo. Revisa output/tables/qc_paro_debug_unique_combos.csv")
}

# -------------------- QC cobertura TRIMESTRAL por banda ----------------------
cov_bins <- df2 |>
  count(ano, edad_band, trimestre, name = "presente") |>
  mutate(presente = presente > 0) |>
  complete(
    ano = anos_target,
    edad_band = c("16-19","20-24","25-29"),
    trimestre = 1:4,
    fill = list(presente = FALSE)
  ) |>
  arrange(ano, edad_band, trimestre)
write_clean(cov_bins, here("output","tables","qc_paro_bins_cobertura.csv"))

# ------------------- Anualización por banda (media trimestral) ---------------
rates_year <- df2 |>
  group_by(ano, edad_band) |>
  summarise(
    n_trimestres = sum(!is.na(tasa) & trimestre %in% 1:4),
    tasa_anual   = ifelse(n_trimestres > 0, mean(tasa, na.rm = TRUE), NA_real_),
    .groups = "drop"
  ) |>
  arrange(ano, edad_band)
write_clean(rates_year, here("output","tables","qc_paro_rates_year.csv"))

if (nrow(rates_year) == 0) {
  abort("No se generó ninguna tasa anual por banda. Revisa el parseo de periodos/edades (ver csv de QC).")
}

# ------------------- Pesos y ajuste 16–19 = 0.8 * 15–19 ----------------------
pesos <- readr::read_csv(fp_pesos, show_col_types = FALSE) |> clean_names()

pesos_adj <- pesos |>
  filter(edad_bin %in% c("15-19","20-24","25-29")) |>
  mutate(
    edad_band = dplyr::case_when(
      edad_bin == "15-19" ~ "16-19",
      TRUE                 ~ edad_bin
    ),
    poblacion_adj = dplyr::case_when(
      edad_bin == "15-19" ~ 0.8 * poblacion,
      TRUE                ~ poblacion
    )
  ) |>
  group_by(ano, edad_band) |>
  summarise(peso = sum(poblacion_adj, na.rm = TRUE), .groups = "drop") |>
  filter(ano %in% anos_target)
write_clean(pesos_adj, here("output","tables","qc_paro_pesos_adj.csv"))

# ------------------- Diagnóstico de join -------------------------------------
join_diag <- full_join(
  rates_year |> mutate(flag_rate = !is.na(tasa_anual)) |> select(ano, edad_band, flag_rate),
  pesos_adj  |> mutate(flag_peso = !is.na(peso))       |> select(ano, edad_band, flag_peso),
  by = c("ano","edad_band")
) |>
  mutate(flag_rate = coalesce(flag_rate, FALSE),
         flag_peso = coalesce(flag_peso, FALSE),
         ok = flag_rate & flag_peso) |>
  arrange(ano, edad_band)
write_clean(join_diag, here("output","tables","qc_paro_join_matrix.csv"))

# ------------------- Ponderación anual 16–29 (con fallbacks) -----------------
paro_pond <- rates_year |>
  left_join(pesos_adj, by = c("ano","edad_band")) |>
  group_by(ano) |>
  summarise(
    n_bandas_rate = sum(!is.na(tasa_anual)),
    n_bandas_peso = sum(!is.na(peso)),
    n_bandas_ok   = sum(!is.na(tasa_anual) & !is.na(peso)),
    sum_pesos     = sum(peso, na.rm = TRUE),
    paro_pond     = ifelse(sum_pesos > 0, sum(tasa_anual * peso, na.rm = TRUE) / sum_pesos, NA_real_),
    paro_media    = ifelse(n_bandas_rate > 0, mean(tasa_anual, na.rm = TRUE), NA_real_),
    metodo        = dplyr::case_when(
      n_bandas_ok == 3                      ~ "ponderado",
      n_bandas_ok > 0 & n_bandas_rate > 0   ~ "mixto",
      sum_pesos == 0 & n_bandas_rate > 0    ~ "media_simple",
      TRUE                                  ~ "sin_datos"
    ),
    paro_15_29    = dplyr::case_when(
      !is.na(paro_pond)                      ~ paro_pond,
      is.na(paro_pond) & !is.na(paro_media)  ~ paro_media,
      TRUE                                   ~ NA_real_
    ),
    .groups = "drop"
  ) |>
  arrange(ano)

# Log rápido por año
invisible(lapply(
  split(paro_pond, seq_len(nrow(paro_pond))),
  function(r) message(sprintf("%d: %s (bandas OK=%d)", r$ano, r$metodo, r$n_bandas_ok))
))

paro_15_29 <- paro_pond |>
  filter(ano %in% anos_target) |>
  select(ano, paro_15_29, metodo)

# ------------------- QC final ------------------------------------------------
esqueleto <- tibble::tibble(ano = anos_target)
cov_final <- esqueleto |>
  left_join(paro_15_29, by = "ano") |>
  mutate(presente = !is.na(paro_15_29))
write_clean(cov_final, here("output","tables","qc_paro_cobertura_anual_15_29.csv"))

pct_na <- mean(is.na(cov_final$paro_15_29))
message(sprintf("Cobertura PARO 16–29 2010–2023: %d/%d años (NA=%.1f%%)",
                sum(!is.na(cov_final$paro_15_29)), nrow(cov_final), 100*pct_na))

# Aserciones suaves
stopifnot(all(paro_15_29$paro_15_29 >= 0 & paro_15_29$paro_15_29 <= 100, na.rm = TRUE))
stopifnot(!any(duplicated(paro_15_29$ano)))

# ------------------- Export final -------------------------------------------
write_clean(paro_15_29, here("data","processed","paro_15_29_anual.csv"))
message("✅ Paro 16–29 limpio -> data/processed/paro_15_29_anual.csv")
