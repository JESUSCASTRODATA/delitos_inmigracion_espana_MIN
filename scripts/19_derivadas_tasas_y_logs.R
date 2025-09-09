#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
###############################################################################
# 19_derivadas_tasas_y_logs.R
# Proyecto: Delitos e Inmigración en España (2010–2023) — Nivel Nacional
# Autor: Jesús Castro (Analista de Datos)
# Salidas:
#   - data/processed/tasas/tasas_totales_anuales.csv
#   - data/processed/tasas/tasas_transformadas.csv
#   - output/tables/qc_consistencia.csv (QC básico)
###############################################################################

suppressPackageStartupMessages({
  library(here); library(readr); library(dplyr); library(tidyr)
  library(janitor); library(stringr); library(fs); library(purrr); library(tibble)
})

options(scipen = 999)

# ============================== Helpers ======================================
assert_infile <- function(path) {
  if (!file.exists(path)) stop(sprintf("Falta el fichero requerido: %s", path), call. = FALSE)
}
write_clean <- function(df, path_out) {
  fs::dir_create(fs::path_dir(path_out))
  readr::write_csv(df, path_out, na = "")
  message(sprintf("✓ Escrito: %s (%s filas)", path_out, nrow(df)))
}
parse_num_safe <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  readr::parse_number(as.character(x),
                      locale = readr::locale(decimal_mark = ",", grouping_mark = "."))
}
safe_log <- function(x) ifelse(is.finite(x) & x > 0, log(x), NA_real_)
base100 <- function(x, base) ifelse(is.na(x) | is.na(base), NA_real_, 100 * x / base)
get_base <- function(df, var, base_year=2010) {
  val <- df[[var]][df$ano == base_year]
  if (length(val)==0 || is.na(val)) NA_real_ else val
}
pick_first_existing <- function(candidates) {
  hit <- candidates[file.exists(candidates)]
  if (length(hit) == 0) {
    stop("No se encontró ninguno de: \n- ", paste(candidates, collapse="\n- "), call. = FALSE)
  }
  hit[[1]]
}
read_std <- function(path) readr::read_csv(path, show_col_types = FALSE) |> janitor::clean_names()

# ============================== Entradas =====================================
# Población
fp_pop_tot <- here("data/processed/poblacion_total_nacional.csv")
fp_pop_ext <- here("data/processed/poblacion_extranjera_total.csv")
fp_pop_esp <- here("data/processed/poblacion_espanola_total.csv") # opcional

# HC/HE (acepta: (ano,hc_total|he_total) o (ano,tipo,valor))
fp_hc <- pick_first_existing(c(here("data/processed/hechos_conocidos_total_nacional.csv")))
fp_he <- pick_first_existing(c(here("data/processed/hechos_esclarecidos_total_nacional.csv")))

# Detenciones (nombres alternativos comunes)
fp_det_tot <- pick_first_existing(c(
  here("data/processed/detenciones_totales_total_nacional.csv"),
  here("data/processed/detenciones_totales_total.csv")
))
fp_det_ext <- pick_first_existing(c(
  here("data/processed/detenciones_extranjeros_total_nacional.csv"),
  here("data/processed/detenciones_extranjeros_total.csv")
))

# AROPE / Paro 15–29 / PIB pc real (nombres alternativos)
fp_arope <- pick_first_existing(c(here("data/processed/arope_total.csv")))
fp_paro  <- pick_first_existing(c(
  here("data/processed/paro_juvenil_15_29.csv"),
  here("data/processed/paro_15_29_anual.csv")
))
fp_pib   <- pick_first_existing(c(
  here("data/processed/pib_pc_real_eurostat.csv"),
  here("data/processed/pib_pc_real_es.csv")
))

# Salidas
p_out_dir_tasas <- here("data/processed/tasas")
p_out_tasas     <- here("data/processed/tasas/tasas_totales_anuales.csv")
p_out_tr        <- here("data/processed/tasas/tasas_transformadas.csv")
p_out_qc        <- here("output/tables/qc_consistencia.csv")

# Requeridos mínimos
invisible(lapply(c(fp_pop_tot, fp_pop_ext, fp_hc, fp_he, fp_det_tot, fp_det_ext, fp_arope, fp_paro, fp_pib), assert_infile))
# fp_pop_esp es opcional

# =============================== Lecturas ====================================
pop_tot <- read_std(fp_pop_tot) |> select(ano, poblacion_total)
pop_ext <- read_std(fp_pop_ext) |> select(ano, poblacion_extranjera)

pop_esp <- NULL
if (file.exists(fp_pop_esp)) {
  pop_esp <- read_std(fp_pop_esp) |> select(ano, poblacion_espanola)
}

# Hechos conocidos
hc_raw <- read_std(fp_hc)
if ("hc_total" %in% names(hc_raw)) {
  hc <- hc_raw |> mutate(hc_total = parse_num_safe(hc_total)) |> select(ano, hc_total)
} else if (all(c("ano","valor") %in% names(hc_raw))) {
  hc <- hc_raw |> mutate(valor = parse_num_safe(valor)) |>
    group_by(ano) |> summarise(hc_total = sum(valor, na.rm = TRUE), .groups = "drop")
} else {
  stop("hechos_conocidos_total_nacional.csv no tiene columnas esperadas ('hc_total' o 'valor').")
}

# Hechos esclarecidos
he_raw <- read_std(fp_he)
if ("he_total" %in% names(he_raw)) {
  he <- he_raw |> mutate(he_total = parse_num_safe(he_total)) |> select(ano, he_total)
} else if (all(c("ano","valor") %in% names(he_raw))) {
  he <- he_raw |> mutate(valor = parse_num_safe(valor)) |>
    group_by(ano) |> summarise(he_total = sum(valor, na.rm = TRUE), .groups = "drop")
} else {
  stop("hechos_esclarecidos_total_nacional.csv no tiene columnas esperadas ('he_total' o 'valor').")
}

# Detenciones totales
det_tot_raw <- read_std(fp_det_tot)
if ("det_tot_total" %in% names(det_tot_raw)) {
  det_tot <- det_tot_raw |> mutate(det_tot_total = parse_num_safe(det_tot_total)) |> select(ano, det_tot_total)
} else if ("det_tot" %in% names(det_tot_raw)) {
  det_tot <- det_tot_raw |> mutate(det_tot_total = parse_num_safe(det_tot)) |> select(ano, det_tot_total)
} else if (all(c("ano","valor") %in% names(det_tot_raw))) {
  det_tot <- det_tot_raw |> mutate(valor = parse_num_safe(valor)) |>
    group_by(ano) |> summarise(det_tot_total = sum(valor, na.rm = TRUE), .groups = "drop")
} else {
  stop("Detenciones totales: columnas no reconocidas (esperado 'det_tot_total' o 'det_tot' o 'valor').")
}

# Detenciones de extranjeros
det_ext_raw <- read_std(fp_det_ext)
if ("det_ext_total" %in% names(det_ext_raw)) {
  det_ext <- det_ext_raw |> mutate(det_ext_total = parse_num_safe(det_ext_total)) |> select(ano, det_ext_total)
} else if ("det_ext" %in% names(det_ext_raw)) {
  det_ext <- det_ext_raw |> mutate(det_ext_total = parse_num_safe(det_ext)) |> select(ano, det_ext_total)
} else if (all(c("ano","valor") %in% names(det_ext_raw))) {
  det_ext <- det_ext_raw |> mutate(valor = parse_num_safe(valor)) |>
    group_by(ano) |> summarise(det_ext_total = sum(valor, na.rm = TRUE), .groups = "drop")
} else {
  stop("Detenciones extranjeros: columnas no reconocidas (esperado 'det_ext_total' o 'det_ext' o 'valor').")
}

# AROPE / Paro / PIB
arope <- read_std(fp_arope) |> mutate(arope = parse_num_safe(arope)) |> select(ano, arope)
paro  <- read_std(fp_paro)
if (!"paro_15_29" %in% names(paro)) {
  stop("El fichero de paro debe contener la columna 'paro_15_29'.")
}
paro  <- paro |> mutate(paro_15_29 = parse_num_safe(paro_15_29)) |> select(ano, paro_15_29)
pib_pc <- read_std(fp_pib)
if (!"pib_pc_real" %in% names(pib_pc)) {
  stop("El fichero de PIB per cápita debe contener la columna 'pib_pc_real'.")
}
pib_pc <- pib_pc |> mutate(pib_pc_real = parse_num_safe(pib_pc_real)) |> select(ano, pib_pc_real)

# ============================== Join maestro =================================
df <- pop_tot |>
  inner_join(pop_ext,  by = "ano") |>
  inner_join(hc,       by = "ano") |>
  inner_join(he,       by = "ano") |>
  inner_join(det_tot,  by = "ano") |>
  inner_join(det_ext,  by = "ano") |>
  inner_join(arope,    by = "ano") |>
  inner_join(paro,     by = "ano") |>
  inner_join(pib_pc,   by = "ano")

if (!is.null(pop_esp)) {
  df <- df |> left_join(pop_esp, by = "ano")
}
df <- df |> arrange(ano)

# =============================== Derivadas ====================================
df <- df |>
  mutate(
    share_extranjeros = poblacion_extranjera / poblacion_total,
    hc_rate      = hc_total      / poblacion_total * 100000,
    he_rate      = he_total      / poblacion_total * 100000,
    det_tot_rate = det_tot_total / poblacion_total * 100000,
    det_ext_rate = det_ext_total / poblacion_total * 100000,
    covid_2020 = if_else(ano == 2020, 1L, 0L),
    covid_2021 = if_else(ano == 2021, 1L, 0L),
    covid_2020_2021 = if_else(ano %in% c(2020, 2021), 1L, 0L)
  )

# ================================ QC básico ==================================
years_full <- 2010:2023
missing_years <- setdiff(years_full, df$ano)

qc_list <- list(
  tibble(regla="cobertura_2010_2023",
         detalle=paste0("rango=", paste(range(df$ano), collapse="-")),
         valor=if (length(missing_years)==0) "OK" else paste("Faltan:", paste(missing_years, collapse=",")),
         extra=NA_character_)
)

viol <- df |> filter(!is.na(he_total), !is.na(hc_total), he_total > hc_total)
if (nrow(viol)==0) {
  qc_list <- append(qc_list, list(tibble(regla="he_le_hc", detalle="sin_violaciones", valor="OK", extra=NA_character_)))
} else {
  qc_list <- append(qc_list, list(viol |> transmute(regla="he_le_hc", detalle=as.character(ano), valor=he_total - hc_total, extra="he_total - hc_total")))
}

add_nonneg <- function(v, name) tibble(regla=paste0("no_negativo_",name), detalle="min", valor=min(v,na.rm=TRUE), extra=NA_character_)
qc_list <- append(qc_list, list(
  add_nonneg(df$poblacion_total,"poblacion_total"),
  add_nonneg(df$poblacion_extranjera,"poblacion_extranjera"),
  add_nonneg(df$hc_total,"hc_total"),
  add_nonneg(df$he_total,"he_total"),
  add_nonneg(df$det_tot_total,"det_tot_total"),
  add_nonneg(df$det_ext_total,"det_ext_total")
))

out_share <- df |> filter(!is.na(share_extranjeros) & (share_extranjeros < 0 | share_extranjeros > 1.2))
if (nrow(out_share)==0) {
  qc_list <- append(qc_list, list(tibble(regla="share_bounds", detalle="dentro_[0,1.2]", valor="OK", extra=NA_character_)))
} else {
  qc_list <- append(qc_list, list(out_share |> transmute(regla="share_bounds", detalle=as.character(ano), valor=share_extranjeros, extra=NA_character_)))
}

if ("poblacion_espanola" %in% names(df)) {
  qc_list <- append(qc_list, list(
    df |> filter(!is.na(poblacion_espanola)) |>
      transmute(regla="cierre_total_vs_espanola_extranjera", detalle=as.character(ano),
                valor = (poblacion_total - (poblacion_espanola + poblacion_extranjera)),
                extra="dif absoluta (total - (esp+ext))")
  ))
}

# Forzar tipos homogéneos en QC (evita error de bind_rows por tipos mixtos)
qc_tbl <- qc_list |>
  purrr::map(function(df) {
    if (!"regla"   %in% names(df)) df$regla   <- NA_character_
    if (!"detalle" %in% names(df)) df$detalle <- NA_character_
    if (!"valor"   %in% names(df)) df$valor   <- NA_character_
    if (!"extra"   %in% names(df)) df$extra   <- NA_character_
    dplyr::mutate(df,
                  regla   = as.character(regla),
                  detalle = as.character(detalle),
                  valor   = as.character(valor),
                  extra   = as.character(extra)
    )
  }) |>
  dplyr::bind_rows()

fs::dir_create(fs::path_dir(p_out_qc))
readr::write_csv(qc_tbl, p_out_qc)
message("✓ QC básico escrito en ", p_out_qc)

# ================================ Salidas ====================================
# 1) Tasas totales anuales
fs::dir_create(p_out_dir_tasas)
tasas_out <- df |>
  select(
    ano,
    poblacion_total, poblacion_extranjera, share_extranjeros,
    hc_rate, he_rate, det_tot_rate, det_ext_rate,
    arope, paro_15_29, pib_pc_real,
    covid_2020, covid_2021, covid_2020_2021
  )
write_clean(tasas_out, p_out_tasas)

# 2) Transformaciones (logs, Δlogs, base100)
bases <- setNames(lapply(c("hc_rate","he_rate","det_tot_rate","det_ext_rate","share_extranjeros","pib_pc_real"),
                         function(v) get_base(tasas_out, v, 2010)),
                  c("hc_rate","he_rate","det_tot_rate","det_ext_rate","share_extranjeros","pib_pc_real"))

transf <- tasas_out |>
  mutate(
    l_hc_rate        = safe_log(hc_rate),
    l_he_rate        = safe_log(he_rate),
    l_det_tot_rate   = safe_log(det_tot_rate),
    l_det_ext_rate   = safe_log(det_ext_rate),
    l_share_extranjeros = safe_log(share_extranjeros),
    l_pib_pc_real    = safe_log(pib_pc_real)
  ) |>
  arrange(ano) |>
  mutate(
    d_l_hc_rate        = l_hc_rate - lag(l_hc_rate),
    d_l_he_rate        = l_he_rate - lag(l_he_rate),
    d_l_det_tot_rate   = l_det_tot_rate - lag(l_det_tot_rate),
    d_l_det_ext_rate   = l_det_ext_rate - lag(l_det_ext_rate),
    d_l_share_extranjeros = l_share_extranjeros - lag(l_share_extranjeros),
    d_l_pib_pc_real    = l_pib_pc_real - lag(l_pib_pc_real),
    base100_hc_rate        = base100(hc_rate,      bases[["hc_rate"]]),
    base100_he_rate        = base100(he_rate,      bases[["he_rate"]]),
    base100_det_tot_rate   = base100(det_tot_rate, bases[["det_tot_rate"]]),
    base100_det_ext_rate   = base100(det_ext_rate, bases[["det_ext_rate"]]),
    base100_share_extranjeros = base100(share_extranjeros, bases[["share_extranjeros"]]),
    base100_pib_pc_real    = base100(pib_pc_real,  bases[["pib_pc_real"]])
  )
write_clean(transf, p_out_tr)

message("✅ Listo: 'tasas_totales_anuales.csv' y 'tasas_transformadas.csv' generados.")
