#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
################################################################################
# 19_transformaciones.R — Panel anual unificado + transformaciones
#
# Proyecto : Delitos e Inmigración en España (2010–2023)
# Autor    : Jesús Castro · JESUSCASTRODATA
# Fecha    : 2025-10-06
# Licencia : MIT (código) · CC BY 4.0 (docs/figuras)
#
# Descripción
#   Construye el panel anual TOTAL NACIONAL (2010–2023), integrando población,
#   criminalidad (HC/HE/Detenciones), composición (% y share extranjeros),
#   PIB per cápita real y variables opcionales (AROPE, paro joven 15–29).
#   Calcula tasas por 100k, logs/Δlogs, lags y controles (YoY, índice 2010, z-score).
#   Incluye QA de cobertura/rangos/identidades y un chequeo de consola.
#
# Entradas (data/processed/)
#   - poblacion_total_nacional.csv              (ano, poblacion_total)
#   - pct_extranjeros_poblacion.csv             (ano, pct_extranjeros)         [opc]
#   - hechos_conocidos_total_nacional.csv       (ano, total -> hc_total)
#   - hechos_esclarecidos_total_nacional.csv    (ano, he/total/valor -> he_total)
#   - detenciones_totales_total.csv             (ano, det_tot)
#   - detenciones_extranjeros_total.csv         (ano, det_ext)
#   - detenciones_share_extranjeros.csv         (ano, share_extranjeros %)     [opc]
#   - pib_pc_real_es.csv                        (ano, pib_pc_real)
#   - arope.csv                                 (o bien crudo INE)             [opc]
#   - paro_15_29_anual.csv                      (ano, paro_joven/valor/tasa)   [opc]
#
# Salidas (data/processed/)
#   - core_indicadores.csv
#   - core_indicadores_derivadas.csv
#
# QA (output/tables/)
#   - 19_qa_cobertura.csv
#   - 19_qa_na_y_rangos.csv
#
# Notas:
#   - pct_extranjeros se autoescala a [0,1] si viene en % (0–100).
#   - share_extranjeros se completa a partir de det_ext/det_tot si falta.
#   - AROPE se normaliza automáticamente si llega ×10 (26,9 → 269).
################################################################################

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(tidyr); library(janitor)
  library(stringr); library(here);  library(fs);   library(tibble)
})

# ------------------------- Config y Utils -----------------------------------
YEAR_MIN <- 2010L; YEAR_MAX <- 2023L; YEARS_SEQ <- YEAR_MIN:YEAR_MAX

# Utilidades del proyecto
source(here::here("scripts","00_utils_limpieza.R"))
root_init("scripts/19_transformaciones.R")

msg  <- function(...) message("[19] ", paste0(...))
asrt <- function(cond, ...) { if (!isTRUE(cond)) stop(paste0(...), call. = FALSE) }

# Fallback: join_with_log() si no existe en utils
if (!exists("join_with_log")) {
  join_with_log <- function(x, y, by, mode = "left") {
    if (missing(y) || is.null(y) || !is.data.frame(y) || nrow(y) == 0) return(x)
    dplyr::left_join(x, y, by = by)
  }
}

# --- Parseo numérico robusto (coma/punto; plausibilidad ≤100 para %)
coerce_num <- function(v){
  if (is.list(v)) {
    v <- vapply(v, function(z) if (length(z)==0) NA_character_ else as.character(z), "", USE.NAMES = FALSE)
  }
  x <- as.character(v)
  
  p_dot <- suppressWarnings(readr::parse_number(x, locale = readr::locale(decimal_mark=".", grouping_mark=",")))
  p_com <- suppressWarnings(readr::parse_number(x, locale = readr::locale(decimal_mark=",", grouping_mark=".")))
  
  has_comma <- grepl(",", x, fixed = TRUE)
  has_dot   <- grepl("\\.", x)
  
  out <- p_dot
  # solo coma decimal → usar p_com
  out[has_comma & !has_dot] <- p_com[has_comma & !has_dot]
  
  # casos ambiguos (ambos o ninguno): decidir por plausibilidad
  both_or_none <- (has_comma & has_dot) | (!has_comma & !has_dot)
  idx <- which(both_or_none)
  if (length(idx)) {
    cand_dot <- p_dot[idx]; cand_com <- p_com[idx]
    pick_com <- is.finite(cand_com) & (cand_com <= 100) & (!is.finite(cand_dot) | cand_dot > 100)
    pick_dot <- is.finite(cand_dot) & (cand_dot <= 100) & (!is.finite(cand_com) | cand_com > 100)
    out[idx[pick_com]] <- cand_com[pick_com]
    out[idx[pick_dot]] <- cand_dot[pick_dot]
    undecided <- idx[!(pick_com | pick_dot)]
    if (length(undecided)) {
      use_com <- is.finite(p_com[undecided]) & (!is.finite(p_dot[undecided]) | TRUE)
      out[undecided[use_com]] <- p_com[undecided[use_com]]
      use_dot <- is.finite(p_dot[undecided]) & !use_com
      out[undecided[use_dot]] <- p_dot[undecided[use_dot]]
    }
  }
  out
}

prepare_arope_total <- function(df){
  df <- df |>
    janitor::clean_names() |>
    # Armoniza nombres si vienen crudos del INE
    dplyr::rename(ano   = dplyr::any_of("periodo"),
                  arope = dplyr::any_of("total"))
  
  # Candidatos de columnas auxiliares (si existen en el crudo)
  c_sex <- intersect(c("sexo","sex"), names(df))
  c_age <- intersect(c("edad","age","grupo_edad","grupo_de_edad"), names(df))
  c_cmp <- intersect(c(
    "tasa_de_riesgo_de_pobreza_o_exclusion_social_y_sus_componentes",
    "indicador","componente","concepto","serie","descripcion","descripcion_de_la_magnitud"
  ), names(df))
  
  out <- df
  
  # Filtro por Total/Ambos si están esas columnas
  if (length(c_sex))
    out <- out |> dplyr::filter(is.na(.data[[c_sex[1]]]) | .data[[c_sex[1]]] %in% c("Total","Ambos sexos","Ambos","total","ambos sexos"))
  
  if (length(c_age))
    out <- out |> dplyr::filter(is.na(.data[[c_age[1]]]) | .data[[c_age[1]]] %in% c("Total","Todas las edades","Total edades","total"))
  
  # Filtro de CONCEPTO: mantener solo el INDICADOR AROPE (no componentes)
  if (length(c_cmp)) {
    cc <- c_cmp[1]
    low <- stringr::str_to_lower(out[[cc]])
    keep <- stringr::str_detect(low, "arope|exclus") &
      !stringr::str_detect(low, "carencia|privaci|baja\\s+intensidad")
    out <- out[keep %in% TRUE | is.na(keep), , drop = FALSE]
  }
  
  # Elegir columnas de año y valor robustamente
  if (!"ano" %in% names(out)) {
    ycand <- intersect(c("ano","year","anio","año","periodo"), names(out))
    if (length(ycand)) out <- dplyr::rename(out, ano = !!ycand[1])
  }
  if (!"arope" %in% names(out)) {
    vcand <- intersect(c("arope","valor","total","rate","tasa","porcentaje","porcentajes","value"), names(out))
    if (length(vcand)) out <- dplyr::rename(out, arope = !!vcand[1])
  }
  
  # Construcción final
  out <- out |>
    dplyr::transmute(
      ano   = suppressWarnings(as.integer(.data[["ano"]])),
      arope = coerce_num(.data[["arope"]])
    ) |>
    dplyr::filter(is.finite(ano), is.finite(arope)) |>
    dplyr::group_by(ano) |>
    dplyr::summarise(arope = mean(arope, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(ano) |>
    # Normalización anti ×10 (ej.: 269 → 26.9)
    dplyr::mutate(arope = ifelse(arope > 100 & arope <= 1000, arope/10, arope))
  
  out
}


# ------------------------- Rutas --------------------------------------------
proc <- function(...) here::here("data","processed", ...)
qa_p <- function(...) here::here("output","tables", ...)

fp_pop_tot   <- proc("poblacion_total_nacional.csv")
fp_pct_ext   <- proc("pct_extranjeros_poblacion.csv")
fp_hc_tot    <- proc("hechos_conocidos_total_nacional.csv")
fp_he_tot    <- proc("hechos_esclarecidos_total_nacional.csv")
fp_det_tot   <- proc("detenciones_totales_total.csv")
fp_det_ext   <- proc("detenciones_extranjeros_total.csv")
fp_det_share <- proc("detenciones_share_extranjeros.csv")
fp_pib       <- proc("pib_pc_real_es.csv")
# Opcionales
fp_arope     <- proc("arope.csv")
fp_paro_j    <- proc("paro_15_29_anual.csv")

# Salidas
fp_core      <- proc("core_indicadores.csv")
fp_core_der  <- proc("core_indicadores_derivadas.csv")

# ------------------------- Lectura defensiva --------------------------------
asrt(file.exists(fp_pop_tot),  "Falta: ", fp_pop_tot)
P <- read_csv(fp_pop_tot, show_col_types = FALSE) |>
  clean_names() |>
  transmute(ano = as.integer(ano), poblacion_total = as.numeric(poblacion_total))

pct_ext <- if (file.exists(fp_pct_ext)) {
  read_csv(fp_pct_ext, show_col_types = FALSE) |>
    clean_names() |>
    transmute(ano = as.integer(ano), pct_extranjeros = as.numeric(pct_extranjeros)) |>
    mutate(pct_extranjeros = ifelse(pct_extranjeros > 1, pct_extranjeros/100, pct_extranjeros))
} else {
  msg("No existe pct_extranjeros_poblacion.csv — se omitirá esta columna.")
  tibble(ano = integer(), pct_extranjeros = numeric())
}

asrt(file.exists(fp_hc_tot),   "Falta: ", fp_hc_tot)
HC <- read_csv(fp_hc_tot, show_col_types = FALSE) |>
  clean_names() |>
  transmute(ano = as.integer(ano), hc_total = as.numeric(total))

asrt(file.exists(fp_he_tot), "Falta: ", fp_he_tot)
HE_raw <- readr::read_csv(fp_he_tot, show_col_types = FALSE) |>
  janitor::clean_names()
he_col_cand <- intersect(c("he","total","valor","n","he_total"), names(HE_raw))
asrt(length(he_col_cand)>0, "No se encontró columna de valor en hechos_esclarecidos_total_nacional.csv (esperado: he/total/valor).")
he_col <- he_col_cand[[1]]
HE <- HE_raw |>
  mutate(
    ano = suppressWarnings(as.integer(.data[["ano"]])),
    he  = coerce_num(.data[[he_col]])
  ) |>
  filter(is.finite(ano)) |>
  group_by(ano) |>
  summarise(he_total = sum(he, na.rm = TRUE), .groups = "drop")

asrt(file.exists(fp_det_tot),  "Falta: ", fp_det_tot)
asrt(file.exists(fp_det_ext),  "Falta: ", fp_det_ext)
DET_TOT <- read_csv(fp_det_tot, show_col_types = FALSE) |>
  clean_names() |>
  transmute(ano = as.integer(ano), det_tot = as.numeric(det_tot))
DET_EXT <- read_csv(fp_det_ext, show_col_types = FALSE) |>
  clean_names() |>
  transmute(ano = as.integer(ano), det_ext = as.numeric(det_ext))

share_ext <- if (file.exists(fp_det_share)) {
  read_csv(fp_det_share, show_col_types = FALSE) |>
    clean_names() |>
    transmute(ano = as.integer(ano), share_extranjeros = as.numeric(share_extranjeros)) |>
    mutate(share_extranjeros = ifelse(share_extranjeros <= 1, 100*share_extranjeros, share_extranjeros))
} else {
  tibble(ano = integer(), share_extranjeros = numeric())
}

asrt(file.exists(fp_pib), "Falta: ", fp_pib)
PIB <- read_csv(fp_pib, show_col_types = FALSE) |>
  clean_names() |>
  transmute(ano = as.integer(ano), pib_pc_real = as.numeric(pib_pc_real))

AROPE <- if (file.exists(fp_arope)) {
  suppressMessages(readr::read_csv(fp_arope, show_col_types = FALSE)) |>
    prepare_arope_total()
} else NULL

PARO_J <- if (file.exists(fp_paro_j)) {
  tmp <- suppressMessages(readr::read_csv(fp_paro_j, show_col_types = FALSE)) |>
    janitor::clean_names()
  val_cand <- intersect(c("paro_15_29","valor","tasa","paro_joven"), names(tmp))
  if (!length(val_cand)) NULL else
    tmp |>
    transmute(
      ano        = suppressWarnings(as.integer(ano)),
      paro_joven = coerce_num(.data[[val_cand[1]]])
    ) |>
    filter(is.finite(ano))
} else NULL

# ------------------------- Merge base anual ---------------------------------
panel <- P |>
  join_with_log(pct_ext,    by = "ano", mode = "left") |>
  join_with_log(HC,         by = "ano", mode = "left") |>
  join_with_log(HE,         by = "ano", mode = "left") |>
  join_with_log(DET_TOT,    by = "ano", mode = "left") |>
  join_with_log(DET_EXT,    by = "ano", mode = "left") |>
  join_with_log(share_ext,  by = "ano", mode = "left") |>
  join_with_log(PIB,        by = "ano", mode = "left") |>
  arrange(ano)

if (!is.null(AROPE) && nrow(AROPE)>0)  panel <- join_with_log(panel, AROPE,  by = "ano", mode = "left")
if (!is.null(PARO_J) && nrow(PARO_J)>0) panel <- join_with_log(panel, PARO_J, by = "ano", mode = "left")

panel <- panel |> filter(dplyr::between(ano, YEAR_MIN, YEAR_MAX))

panel <- panel |>
  mutate(
    share_extranjeros = dplyr::coalesce(
      share_extranjeros,
      ifelse(det_tot > 0, 100 * det_ext / det_tot, NA_real_)
    )
  )

panel <- panel |>
  mutate(
    tasa_hc_100k = ifelse(poblacion_total > 0, 1e5 * hc_total / poblacion_total, NA_real_),
    tasa_he_100k = ifelse(poblacion_total > 0, 1e5 * he_total / poblacion_total, NA_real_)
  )

write_clean(panel, fp_core)
msg("✓ Escrito core_indicadores.csv")

# ------------------------- Derivadas (logs, Δlog, lags) ---------------------
safelog <- function(x) ifelse(is.finite(x) & x > 0, log(x), NA_real_)

# Base de derivadas (selección de columnas)
base <- panel |>
  dplyr::select(
    ano, poblacion_total, pct_extranjeros, hc_total, he_total,
    det_tot, det_ext, share_extranjeros, pib_pc_real,
    tasa_hc_100k, tasa_he_100k,
    dplyr::any_of(c("arope","paro_joven"))
  )

# Logs y Δlogs + Δ en niveles
base <- base |>
  mutate(
    ln_pib_pc      = safelog(pib_pc_real),
    ln_hc          = safelog(hc_total),
    ln_he          = safelog(he_total),
    ln_det_tot     = safelog(det_tot),
    ln_det_ext     = safelog(det_ext),
    ln_tasa_hc     = safelog(tasa_hc_100k),
    ln_tasa_he     = safelog(tasa_he_100k),
    
    dln_pib_pc     = ln_pib_pc  - dplyr::lag(ln_pib_pc),
    dln_hc         = ln_hc      - dplyr::lag(ln_hc),
    dln_he         = ln_he      - dplyr::lag(ln_he),
    dln_det_tot    = ln_det_tot - dplyr::lag(ln_det_tot),
    dln_det_ext    = ln_det_ext - dplyr::lag(ln_det_ext),
    dln_tasa_hc    = ln_tasa_hc - dplyr::lag(ln_tasa_hc),
    dln_tasa_he    = ln_tasa_he - dplyr::lag(ln_tasa_he),
    
    # Δ en niveles (puntos por 100k)
    d_tasa_hc_100k = tasa_hc_100k - dplyr::lag(tasa_hc_100k),
    d_tasa_he_100k = tasa_he_100k - dplyr::lag(tasa_he_100k),
    
    # Δ en % y otras
    d_share_ext    = share_extranjeros - dplyr::lag(share_extranjeros),
    d_pct_extran   = pct_extranjeros   - dplyr::lag(pct_extranjeros)
  ) |>
  mutate(
    pct_extranjeros_l1 = dplyr::lag(pct_extranjeros, 1),
    share_extr_l1      = dplyr::lag(share_extranjeros, 1),
    tasa_hc_l1         = dplyr::lag(tasa_hc_100k, 1),
    tasa_he_l1         = dplyr::lag(tasa_he_100k, 1)
  )

# Controles PIB (YoY, índice 2010, z-score)
base_val_2010 <- panel$pib_pc_real[panel$ano == 2010]
base_val_2010 <- if (length(base_val_2010)) base_val_2010[1] else NA_real_
mu_pib <- mean(panel$pib_pc_real, na.rm = TRUE)
sd_pib <- stats::sd(panel$pib_pc_real, na.rm = TRUE)

base <- base |>
  mutate(
    pib_yoy      = pib_pc_real / dplyr::lag(pib_pc_real) - 1,
    pib_idx2010  = ifelse(!is.na(base_val_2010) & !is.na(pib_pc_real),
                          100 * pib_pc_real / base_val_2010, NA_real_),
    pib_z        = if (is.finite(sd_pib) && sd_pib > 0) (pib_pc_real - mu_pib)/sd_pib else NA_real_
  )

# ------------------------- QA mínimos ---------------------------------------
vars_qc <- c("poblacion_total","pct_extranjeros","hc_total","he_total",
             "det_tot","det_ext","share_extranjeros","tasa_hc_100k","tasa_he_100k",
             "arope","paro_joven")

n_obs_vec <- vapply(vars_qc, function(v) sum(is.finite(base[[v]])), integer(1))

safe_min <- function(v) if (all(is.na(v))) NA_real_ else suppressWarnings(min(v, na.rm = TRUE))
safe_max <- function(v) if (all(is.na(v))) NA_real_ else suppressWarnings(max(v, na.rm = TRUE))

qa_cov <- tibble(
  variable  = vars_qc,
  n_obs     = n_obs_vec,
  years_min = min(base$ano, na.rm = TRUE),
  years_max = max(base$ano, na.rm = TRUE),
  n_years   = dplyr::n_distinct(base$ano),
  n_expected= length(YEARS_SEQ)
)

qa_na_rng <- tibble(
  var = names(base),
  n_na = vapply(base, function(v) sum(is.na(v)), integer(1)),
  min  = vapply(base, safe_min, numeric(1)),
  max  = vapply(base, safe_max, numeric(1))
)

write_clean(qa_cov,    qa_p("19_qa_cobertura.csv"))
write_clean(qa_na_rng, qa_p("19_qa_na_y_rangos.csv"))

# ------------------------- Escritura final ----------------------------------
write_clean(base, fp_core_der)
msg("✅ Transformaciones listas: core_indicadores.csv + core_indicadores_derivadas.csv")

# ── QC SOLO CONSOLA 19_transformaciones ──────────────────────────────────────
check_19_console <- function(panel, base,
                             year_min = YEAR_MIN, year_max = YEAR_MAX,
                             tol_rate = 1e-9, tol_share = 1e-9,
                             strict = FALSE) {
  cat("\n============== QC 19_transformaciones (solo consola) ==============\n")
  has <- function(nm, df=panel) nm %in% names(df)
  rng <- function(x) if (all(is.na(x))) NA_real_ else diff(range(x, na.rm = TRUE))
  
  ## 1) Cobertura y duplicados
  yrs <- sort(unique(panel$ano))
  cat("Años en panel: { ", paste(yrs, collapse=", "), " }\n", sep = "")
  miss <- setdiff(year_min:year_max, yrs)
  if (!length(miss)) cat("✔ Cobertura contigua ", year_min, "–", year_max, ".\n", sep="") 
  else cat("✖ Cobertura incompleta. Faltan: ", paste(miss, collapse=", "), "\n", sep="")
  dup <- panel |> dplyr::count(ano, name="n") |> dplyr::filter(n>1)
  if (nrow(dup)==0) cat("✔ Sin duplicados por año en panel.\n") else { cat("✖ Duplicados por año:\n"); print(dup, n=20) }
  
  ## 2) Rangos y signos
  if (all(has("hc_total"),has("he_total"),has("det_tot"),has("det_ext"),has("poblacion_total"))) {
    negs <- panel |>
      dplyr::select(ano, hc_total, he_total, det_tot, det_ext, poblacion_total) |>
      tidyr::pivot_longer(-ano) |>
      dplyr::filter(is.finite(value) & value < 0)
    if (nrow(negs)==0) cat("✔ Conteos no negativos (HC/HE/Det/Población).\n")
    else { cat("✖ Valores negativos (muestra):\n"); print(utils::head(negs, 10), row.names = FALSE) }
  }
  if (has("pct_extranjeros")) {
    bad <- panel |> dplyr::filter(is.finite(pct_extranjeros) & (pct_extranjeros<0 | pct_extranjeros>1))
    if (nrow(bad)==0) cat("✔ pct_extranjeros ∈ [0,1].\n") else { cat("✖ pct_extranjeros fuera de [0,1].\n"); print(bad[,c("ano","pct_extranjeros")]) }
  }
  if (has("share_extranjeros")) {
    bad <- panel |> dplyr::filter(is.finite(share_extranjeros) & (share_extranjeros<0 | share_extranjeros>100))
    if (nrow(bad)==0) cat("✔ share_extranjeros ∈ [0,100] (%).\n") else { cat("✖ share_extranjeros fuera de [0,100].\n"); print(bad[,c("ano","share_extranjeros")]) }
  }
  
  ## 3) Consistencias
  if (all(has("hc_total"),has("he_total"))) {
    viol <- panel |> dplyr::filter(is.finite(hc_total), is.finite(he_total), he_total > hc_total)
    if (nrow(viol)==0) cat("✔ Identidad HE ≤ HC.\n") else { cat("✖ HE > HC (muestra):\n"); print(viol[,c("ano","hc_total","he_total")]) }
  }
  if (all(has("det_ext"),has("det_tot"),has("share_extranjeros"))) {
    chk <- panel |>
      dplyr::mutate(calc = dplyr::if_else(det_tot>0, 100*det_ext/det_tot, NA_real_),
                    diff = abs(share_extranjeros - calc)) |>
      dplyr::filter(is.finite(calc), is.finite(share_extranjeros), diff > 100*tol_share)
    if (nrow(chk)==0) cat("✔ share_extranjeros coherente con det_ext/det_tot.\n")
    else { cat("✖ share_extranjeros inconsistente (muestra):\n"); print(utils::head(chk[,c("ano","share_extranjeros","calc","diff")], 10), row.names = FALSE) }
  }
  if (all(has("tasa_hc_100k"), has("hc_total"), has("poblacion_total"))) {
    inco <- panel |>
      dplyr::mutate(calc = dplyr::if_else(poblacion_total>0, 1e5*hc_total/poblacion_total, NA_real_),
                    rel = abs(tasa_hc_100k - calc)/pmax(abs(calc), 1e-12)) |>
      dplyr::filter(is.finite(tasa_hc_100k), is.finite(calc), rel > tol_rate)
    if (nrow(inco)==0) cat("✔ tasa_hc_100k coherente.\n") else { cat("✖ tasa_hc_100k incoherente (muestra):\n"); print(inco[,c("ano","tasa_hc_100k","calc","rel")]) }
  }
  if (all(has("tasa_he_100k"), has("he_total"), has("poblacion_total"))) {
    inco <- panel |>
      dplyr::mutate(calc = dplyr::if_else(poblacion_total>0, 1e5*he_total/poblacion_total, NA_real_),
                    rel = abs(tasa_he_100k - calc)/pmax(abs(calc), 1e-12)) |>
      dplyr::filter(is.finite(tasa_he_100k), is.finite(calc), rel > tol_rate)
    if (nrow(inco)==0) cat("✔ tasa_he_100k coherente.\n") else { cat("✖ tasa_he_100k incoherente (muestra):\n"); print(inco[,c("ano","tasa_he_100k","calc","rel")]) }
  }
  
  ## 4) Derivadas (informativo)
  if ("d_share_ext" %in% names(base)) cat("i  media(d_share_ext) ≈ ", round(mean(base$d_share_ext, na.rm=TRUE), 3), " p.p.\n", sep="")
  if ("d_pct_extran" %in% names(base)) cat("i  media(d_pct_extran) ≈ ", round(mean(base$d_pct_extran, na.rm=TRUE), 4), " (proporción)\n", sep="")
  if ("pib_yoy" %in% names(base)) {
    out <- base |> dplyr::filter(is.finite(pib_yoy) & (pib_yoy < -0.15 | pib_yoy > 0.15))
    if (nrow(out)==0) cat("✔ pib_yoy dentro de ±15% (informativo).\n") else { cat("ℹ pib_yoy fuera de ±15%:\n"); print(out[,c("ano","pib_yoy")], row.names = FALSE) }
  }
  
  ## 5) NAs por variable clave
  na_tab <- panel |>
    dplyr::select(ano, dplyr::any_of(c("poblacion_total","pct_extranjeros","hc_total","he_total",
                                       "det_tot","det_ext","share_extranjeros","tasa_hc_100k","tasa_he_100k","pib_pc_real",
                                       "arope","paro_joven"))) |>
    tidyr::pivot_longer(-ano, names_to="var", values_to="val") |>
    dplyr::group_by(var) |>
    dplyr::summarise(n_na = sum(is.na(val)), .groups="drop") |>
    dplyr::arrange(dplyr::desc(n_na))
  cat("NAs por variable (panel):\n"); print(na_tab, row.names = FALSE)
  
  ## 6) Presencia de opcionales (arope / paro_joven)
  cat("Opcionales presentes: ",
      paste(c(if (has("arope")) "arope" else NULL,
              if (has("paro_joven")) "paro_joven" else NULL), collapse=", "),
      if (!has("arope") && !has("paro_joven")) "(ninguno)" else "", "\n", sep="")
  
  ## Modo estricto: aborta en incoherencias críticas
  if (isTRUE(strict)) {
    fails <- c()
    if (any(panel$he_total > panel$hc_total, na.rm = TRUE)) fails <- c(fails, "HE>HC")
    if (any(panel$share_extranjeros < 0 | panel$share_extranjeros > 100, na.rm = TRUE)) fails <- c(fails, "share fuera [0,100]")
    if (any(panel$poblacion_total <= 0, na.rm = TRUE)) fails <- c(fails, "población <= 0")
    if (length(fails)) stop("QC19 estricto falló: ", paste(unique(fails), collapse="; "), call. = FALSE)
  }
  
  cat("=====================================================================\n\n")
  invisible(TRUE)
}

# (Opcional) Ejecuta QC de consola al final si estás en sesión interactiva
if (interactive()) {
  check_19_console(panel, base, strict = FALSE)
}




# Ejecuta el QC de consola (pon strict=TRUE si quieres que pare ante incoherencias)
check_19_console(panel, base, strict = FALSE)






# Ver qué quedó en AROPE tras prepare_arope_total()
if (exists("AROPE") && !is.null(AROPE)) {
  print(AROPE, n = nrow(AROPE))
} else {
  cat("AROPE quedó NULL tras prepare_arope_total()\n")
}

# Ver tipos y valores crudos del fichero (sin preparar)
tmp_raw <- suppressMessages(readr::read_csv(fp_arope, show_col_types = FALSE)) |> janitor::clean_names()
str(tmp_raw)
unique(head(tmp_raw$periodo, 20))   # si existe 'periodo'
unique(head(tmp_raw$ano, 20))       # si existe 'ano'

# Comparar años numéricos:
yrs_panel <- sort(unique(panel$ano))
yrs_arope <- if (exists("AROPE") && !is.null(AROPE)) sort(unique(AROPE$ano)) else integer()
setdiff(yrs_panel, yrs_arope)   # los que faltan en AROPE
setdiff(yrs_arope, yrs_panel)   # los que sobran en AROPE

