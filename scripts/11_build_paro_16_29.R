#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
################################################################################
# 11_build_paro_16_29.R — Script unificado (pesos EPA + paro 16–29 ponderado)
#
# Proyecto : Delitos e Inmigración en España (2010–2023)
# Autor    : Jesús Castro · JESUSCASTRODATA
# Licencia : MIT (código) · CC BY 4.0 (docs/figuras)
# Fecha    : 2025-09-21 (rev. mejoras: autodet enc/sep, QA extra, bitácora)
################################################################################

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(janitor); library(stringr)
  library(here);  library(tidyr); library(purrr); library(fs)
})

# -------------------------- Configuración ------------------------------------
ANOS <- 2010:2023
dir.create(here::here("output","tables"), recursive = TRUE, showWarnings = FALSE)
dir.create(here::here("data","processed"), recursive = TRUE, showWarnings = FALSE)

fp_epa_raw   <- here::here("data","raw","epa_activos_16_29_trimestral.csv")
fp_paro_raw  <- here::here("data","raw","tasas_paro.csv")
fp_activos   <- here::here("data","processed","activos_16_29_bins_anual.csv")
fp_out_paro  <- here::here("data","processed","paro_15_29_anual.csv")
logfile      <- here::here("output","tables","qc_paro_log.txt")

# Utils del proyecto (read_raw, logging, root_init, etc.)
source(here::here("scripts","00_utils_limpieza.R"))
root_init("scripts/11_build_paro_16_29.R")

# -------------------------- Helpers ------------------------------------------
msg <- function(...) message("[11] ", paste0(...))
norm_chr <- function(x){
  x0 <- as.character(x); x0 <- chartr("\u00A0"," ", x0)
  x0 <- gsub("[\r\n\t]+"," ", x0); x0 <- gsub("\\s+"," ", x0); trimws(x0)
}
pick_col <- function(pats, nms){
  hit <- Reduce(`|`, lapply(pats, function(p) grepl(p, nms, ignore.case = TRUE)))
  if (!any(hit)) NA_character_ else nms[which(hit)[1]]
}
extract_year <- function(x){
  x0 <- tolower(norm_chr(x))
  as.integer(stringr::str_match(x0, "([12][0-9]{3})")[,2])
}
extract_quarter <- function(x){
  x0 <- tolower(norm_chr(x))
  q1 <- suppressWarnings(as.integer(stringr::str_match(x0, "t\\s*([1-4])")[,2]))
  q2 <- suppressWarnings(as.integer(stringr::str_match(x0, "trimestre\\s*([1-4])")[,2]))
  q3 <- suppressWarnings(as.integer(stringr::str_match(x0, "trim\\.?\\s*([1-4])")[,2]))
  q4 <- suppressWarnings(as.integer(stringr::str_match(x0, "q\\s*([1-4])")[,2]))
  dplyr::coalesce(q1, q2, q3, q4)
}
parse_num_es <- function(x){
  suppressWarnings(
    readr::parse_number(norm_chr(x),
                        locale = readr::locale(decimal_mark = ",", grouping_mark = "."))
  )
}

# =============================================================================
# Parte A) Activos EPA 16–29 por banda (pesos anuales)
# =============================================================================
if (!file.exists(fp_epa_raw)) stop("No existe: ", fp_epa_raw)

# ⬇️ Lectura robusta (autodet sep/encoding + logging)
epa <- read_raw(fp_epa_raw) |> janitor::clean_names()
nms_epa <- names(epa)

# pickers robustos
col_sexo    <- pick_col(c("^sexo$","^sex$"), nms_epa)
col_edad    <- pick_col(c("grupo.*edad","^edad$","age"), nms_epa)
col_periodo <- pick_col(c("^periodo$","^period$","time|period"), nms_epa)
col_valor   <- pick_col(c("^valor$","^total$","^value$","^obs_?value$"), nms_epa)

if (any(is.na(c(col_sexo,col_edad,col_periodo,col_valor)))) {
  stop("EPA: faltan columnas esperadas. Detectadas: ", paste(nms_epa, collapse=", "))
}

epa_sel <- dplyr::transmute(
  epa,
  sexo    = .data[[col_sexo]],
  edad    = .data[[col_edad]],
  periodo = .data[[col_periodo]],
  valor   = .data[[col_valor]]
)

ok_ambos <- grepl("^ambos|^total|^both|^overall", tolower(norm_chr(epa_sel$sexo)))
epa_sel  <- epa_sel[ok_ambos, , drop = FALSE]

epa_sel$edad_bin  <- dplyr::case_when(
  stringr::str_detect(epa_sel$edad, "16\\D*19") ~ "16-19",
  stringr::str_detect(epa_sel$edad, "20\\D*24") ~ "20-24",
  stringr::str_detect(epa_sel$edad, "25\\D*29") ~ "25-29",
  TRUE ~ NA_character_
)
epa_sel$ano       <- extract_year(epa_sel$periodo)
epa_sel$trimestre <- extract_quarter(epa_sel$periodo)
epa_sel$valor_num <- parse_num_es(epa_sel$valor)

keep_epa <- !is.na(epa_sel$edad_bin) & !is.na(epa_sel$ano) & !is.na(epa_sel$trimestre)
epa2 <- epa_sel[keep_epa, c("ano","trimestre","edad_bin","valor_num"), drop = FALSE]
epa2 <- epa2[epa2$ano %in% ANOS, , drop = FALSE]
if (!nrow(epa2)) stop("EPA: Tras el filtrado no quedan registros.")

# QC cobertura EPA (export)
cov_quarter <- tidyr::complete(
  dplyr::count(epa2, ano, edad_bin, trimestre, name = "n"),
  ano = ANOS, edad_bin = c("16-19","20-24","25-29"), trimestre = 1:4, fill = list(n = 0)
) |> dplyr::arrange(ano, edad_bin, trimestre)
readr::write_csv(cov_quarter, here::here("output","tables","qc_epa_activos_cobertura.csv"))

# Validación mínima de cobertura (opcional endurecer a stop)
chk_cov <- cov_quarter |>
  dplyr::group_by(ano, edad_bin) |>
  dplyr::summarise(n_trimestres = sum(n > 0), .groups="drop")
if (any(chk_cov$n_trimestres == 0)) {
  warning("EPA: hay (año,banda) sin ningún trimestre observado; la media anual puede quedar sesgada.", call. = FALSE)
}

# Muestra de combinaciones (para inspección)
uniques_sample <- unique(epa[, c(col_sexo, col_edad, col_periodo)])
uniques_sample <- utils::head(uniques_sample, 200)
colnames(uniques_sample) <- c("sexo","edad","periodo")
readr::write_csv(uniques_sample, here::here("output","tables","qc_epa_periodo_edad_sexo_unique_sample.csv"))

# Media anual por banda
pesos <- epa2 |>
  dplyr::group_by(ano, edad_bin) |>
  dplyr::summarise(activos = mean(valor_num, na.rm = TRUE), .groups = "drop") |>
  dplyr::arrange(ano, edad_bin)

out_activos <- dplyr::mutate(pesos, activos = as.numeric(activos))
readr::write_csv(out_activos, fp_activos)
msg("✓ Pesos EPA → data/processed/activos_16_29_bins_anual.csv")

# =============================================================================
# Parte B) Tasas de paro por banda → anual → 16–29 ponderado
# =============================================================================
if (!file.exists(fp_paro_raw)) stop("No existe: ", fp_paro_raw)

# ⬇️ Lectura robusta (autodet + logging)
paro_raw <- read_raw(fp_paro_raw) |> janitor::clean_names()
nms_paro <- names(paro_raw)

# Pickers flexibles (en vez de exigir exactamente 'edad','sexo','periodo','total')
pick1 <- function(patterns, nms) {
  ix <- unique(unlist(lapply(patterns, function(p) grep(p, nms, ignore.case = TRUE))))
  if (length(ix)) nms[ix[1]] else NA_character_
}
col_edad    <- pick1(c("^edad$", "grupo.*edad", "age"), nms_paro)
col_sexo    <- pick1(c("^sexo$", "^sex$"), nms_paro)
col_periodo <- pick1(c("^periodo$", "^period$", "time", "period"), nms_paro)
col_total   <- pick1(c("^total$", "^valor$", "^value$", "^obs_?value$"), nms_paro)

if (any(is.na(c(col_edad, col_sexo, col_periodo, col_total)))) {
  stop(
    "Paro: faltan columnas mapeables.\n",
    "Detectadas: ", paste(nms_paro, collapse = ", "), "\n",
    "Se esperaba poder mapear: edad/sexo/periodo/total"
  )
}

# Dump de muestra para QA de parsing de periodo/edad/sexo
readr::write_csv(
  paro_raw |> dplyr::distinct(.data[[col_edad]], .data[[col_sexo]], .data[[col_periodo]]) |> head(200) |>
    rlang::set_names(c("edad","sexo","periodo")),
  here::here("output","tables","qc_paro_parse_periodo_samples.csv")
)

# Normalización a columnas estándar
# Helpers locales (se reutilizan)
std_sexo_total <- function(x){
  grepl("^total|^ambos|^both|^overall", tolower(norm_chr(x)))
}
std_edad_band_16_29 <- function(x){
  x0 <- tolower(norm_chr(x))
  x0 <- gsub("\u2013|\u2014","-", x0)  # guiones tipográficos
  dplyr::case_when(
    grepl("^de\\s*16\\s*a\\s*19|^16\\s*-\\s*19", x0) ~ "16-19",
    grepl("^de\\s*20\\s*a\\s*24|^20\\s*-\\s*24", x0) ~ "20-24",
    grepl("^de\\s*25\\s*a\\s*29|^25\\s*-\\s*29", x0) ~ "25-29",
    TRUE ~ NA_character_
  )
}
parse_rate_num <- function(x){
  x0 <- norm_chr(x)
  x0 <- ifelse(x0 %in% c("..","."), NA_character_, x0)
  x0 <- gsub("\\.", "", x0)     # miles
  x0 <- gsub(",", ".", x0)      # decimal coma → punto
  suppressWarnings(as.numeric(x0))
}

df_paro <- paro_raw |>
  dplyr::transmute(
    edad    = .data[[col_edad]],
    sexo    = .data[[col_sexo]],
    periodo = .data[[col_periodo]],
    tasa_ch = .data[[col_total]]
  ) |>
  dplyr::filter(std_sexo_total(sexo)) |>
  dplyr::mutate(
    edad_band = std_edad_band_16_29(edad),
    tasa      = parse_rate_num(tasa_ch),
    ano       = extract_year(periodo),
    trimestre = extract_quarter(periodo)
  )

# *** Validación dura: exigir las 3 bandas 16–19/20–24/25–29 en ALGÚN año ***
bands_present <- sort(na.omit(unique(df_paro$edad_band)))
if (!all(c("16-19","20-24","25-29") %in% bands_present)) {
  vals_edad <- paste(utils::head(sort(unique(paro_raw[[col_edad]])), 50), collapse = " | ")
  stop(paste0(
    "Paro: el fichero no trae bandas 16–19/20–24/25–29.\n",
    "Valores detectados en '", col_edad, "': ", vals_edad, "\n",
    "Necesitas el dataset de tasas de paro POR GRUPOS DE EDAD (INE/JAXI) ",
    "con esas bandas. Colócalo en data/raw/tasas_paro.csv y reejecuta."
  ))
}

# Depura trimestres y ventana
df2 <- df_paro |>
  dplyr::filter(!is.na(edad_band), !is.na(tasa), !is.na(ano), !is.na(trimestre), ano %in% ANOS) |>
  dplyr::select(ano, trimestre, edad_band, tasa) |>
  dplyr::arrange(ano, trimestre, edad_band)

# Cobertura trimestral por banda (QA)
cov_bins <- df2 |>
  dplyr::count(ano, edad_band, trimestre, name = "presente") |>
  dplyr::mutate(presente = presente > 0) |>
  tidyr::complete(ano = ANOS,
                  edad_band = c("16-19","20-24","25-29"),
                  trimestre = 1:4,
                  fill = list(presente = FALSE)) |>
  dplyr::arrange(ano, edad_band, trimestre)
readr::write_csv(cov_bins, here::here("output","tables","qc_paro_bins_cobertura.csv"))

# Anualización (media trimestral) por banda
rates_year <- df2 |>
  dplyr::group_by(ano, edad_band) |>
  dplyr::summarise(
    n_trimestres = sum(!is.na(tasa) & trimestre %in% 1:4),
    tasa_anual   = ifelse(n_trimestres > 0, mean(tasa, na.rm = TRUE), NA_real_),
    .groups = "drop"
  ) |>
  dplyr::arrange(ano, edad_band)
readr::write_csv(rates_year, here::here("output","tables","qc_paro_rates_year.csv"))
if (nrow(rates_year) == 0) stop("Paro: no se generó ninguna tasa anual por banda.")

# Ponderación con activos EPA
pesos_adj <- out_activos |>
  dplyr::transmute(ano, edad_band = edad_bin, peso = activos) |>
  dplyr::filter(edad_band %in% c("16-19","20-24","25-29")) |>
  dplyr::arrange(ano, edad_band)
readr::write_csv(pesos_adj, here::here("output","tables","qc_paro_pesos_adj.csv"))

# Matriz de join (QA)
join_diag <- dplyr::full_join(
  rates_year |> dplyr::mutate(flag_rate = !is.na(tasa_anual)) |> dplyr::select(ano, edad_band, flag_rate),
  pesos_adj  |> dplyr::mutate(flag_peso = !is.na(peso))       |> dplyr::select(ano, edad_band, flag_peso),
  by = c("ano","edad_band")
) |>
  dplyr::mutate(flag_rate = dplyr::coalesce(flag_rate, FALSE),
                flag_peso = dplyr::coalesce(flag_peso, FALSE),
                ok = flag_rate & flag_peso) |>
  dplyr::arrange(ano, edad_band)
readr::write_csv(join_diag, here::here("output","tables","qc_paro_join_matrix.csv"))

# Cálculo final 16–29 ponderado
write("[PESOS] Usando EPA (activos) 16–19/20–24/25–29.", file = logfile)
paro_pond <- dplyr::left_join(rates_year, pesos_adj, by = c("ano","edad_band")) |>
  dplyr::group_by(ano) |>
  dplyr::summarise(
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
    paro_15_29    = dplyr::coalesce(paro_pond, paro_media),
    .groups = "drop"
  ) |>
  dplyr::arrange(ano)

paro_15_29 <- paro_pond |> dplyr::filter(ano %in% ANOS) |> dplyr::select(ano, paro_15_29, metodo)

# QC final y export
esqueleto <- tibble::tibble(ano = ANOS)
cov_final <- esqueleto |>
  dplyr::left_join(paro_15_29, by = "ano") |>
  dplyr::mutate(presente = !is.na(paro_15_29))
readr::write_csv(cov_final, here::here("output","tables","qc_paro_cobertura_anual_15_29.csv"))

# Años faltantes (para runbook)
miss <- tibble::tibble(ano = setdiff(ANOS, paro_15_29$ano))
readr::write_csv(miss, here::here("output","tables","qc_paro_15_29_anos_faltantes.csv"))

stopifnot(all(paro_15_29$paro_15_29 >= 0 & paro_15_29$paro_15_29 <= 100, na.rm = TRUE))
stopifnot(!any(duplicated(paro_15_29$ano)))

readr::write_csv(paro_15_29, fp_out_paro)
msg("✅ Paro 16–29 limpio → data/processed/paro_15_29_anual.csv")

# Resumen para bitácora
msg(sprintf("OUT: %s | n=%d años [%d–%d] | método(s): %s",
            fs::path_rel(fp_out_paro, start = here::here()),
            nrow(paro_15_29), min(paro_15_29$ano), max(paro_15_29$ano),
            paste(unique(paro_15_29$metodo), collapse=", ")))




pesos <- readr::read_csv(here::here("data/processed/activos_16_29_bins_anual.csv"), show_col_types = FALSE)
xtabs(!is.na(activos) ~ ano + edad_bin, pesos)



# ───────────────── Mini-check consola (añadir al final o ejecutar en consola) ─────────────────
qc11_console <- function() {
  suppressPackageStartupMessages({library(readr); library(dplyr); library(here); library(janitor)})
  fp_out_paro <- here::here("data","processed","paro_15_29_anual.csv")
  fp_pesos    <- here::here("data","processed","activos_16_29_bins_anual.csv")
  fp_rates    <- here::here("output","tables","qc_paro_rates_year.csv")
  fp_join     <- here::here("output","tables","qc_paro_join_matrix.csv")
  
  if (!file.exists(fp_out_paro)) { cat("❌ No existe:", fp_out_paro, "\n"); return(invisible(FALSE)) }
  P <- read_csv(fp_out_paro, show_col_types = FALSE) |> clean_names()
  Y <- 2010:2023; miss <- setdiff(Y, P$ano)
  
  cat("\n🔎 QC11 — Paro 16–29 ponderado\n")
  cat(sprintf("• Cobertura años: %d/14 (2010–2023)  faltan: %s\n",
              nrow(P), ifelse(length(miss)==0,"ninguno",paste(miss, collapse=", "))))
  if (all(c("paro_15_29","metodo") %in% names(P))) {
    rng <- range(P$paro_15_29, na.rm = TRUE)
    cat(sprintf("• Rango paro_15_29: [%.1f, %.1f]   métodos: %s\n",
                rng[1], rng[2], paste(sort(unique(P$metodo)), collapse=", ")))
  }
  if (file.exists(fp_pesos)) {
    pesos <- read_csv(fp_pesos, show_col_types = FALSE) |> clean_names()
    tab <- with(pesos, table(ano, edad_bin, useNA = "no"))
    cat("• Cobertura pesos EPA (conteo por año×banda):\n"); print(tab)
  }
  if (file.exists(fp_rates)) {
    rates <- read_csv(fp_rates, show_col_types = FALSE) |> clean_names()
    any_na <- sum(is.na(rates$tasa_anual))
    cat(sprintf("• Tasa anual por banda con NA: %d filas\n", any_na))
  }
  if (file.exists(fp_join)) {
    J <- read_csv(fp_join, show_col_types = FALSE) |> clean_names()
    bad <- sum(!J$ok)
    cat(sprintf("• Join rates×pesos — combinaciones sin ambos datos: %d\n", bad))
  }
  
  cat("\n• Preview (últimos 5):\n")
  print(dplyr::arrange(P, ano) |> tail(5), n = 5)
  if (all(is.finite(P$paro_15_29)) && all(P$paro_15_29 >= 0 & P$paro_15_29 <= 100)) {
    cat("\n✅ PASS — Paro 16–29 OK\n\n"); return(invisible(TRUE))
  } else {
    cat("\n❌ OJO — Valores fuera de [0,100] o NA\n\n"); return(invisible(FALSE))
  }
}

# Lánzalo tras correr el script:
# qc11_console()



