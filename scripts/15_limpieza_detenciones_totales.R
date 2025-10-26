#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
###############################################################################
# 15_limpieza_detenciones_totales.R — Nivel Nacional (2010–2023) · v1.6
# Proyecto: Delitos e Inmigración en España (2010–2023)
# Autor: Jesús Castro · JESUSCASTRODATA
# Fecha: 2025-09-18 (act. v1.6)
#
# ENTRA
#   - data/raw/detenciones_tipologia.csv
#   - (opcional) config/map_tipologias.csv  ← normaliza etiquetas (raw_tipo → propuesta_tipo)
# SALE
#   - data/processed/detenciones_totales_total_nacional.csv (ano, tipo, valor)
#   - data/processed/detenciones_totales_total.csv          (ano, det_tot)
# QA
#   - output/tables/qa_det_tot_anios_fuera_rango.csv
#   - output/tables/qa_det_tot_na.csv
#   - output/tables/qa_det_tot_negativos.csv
#   - output/tables/qa_det_tot_duplicados.csv
#   - output/tables/qa_det_tot_cobertura.csv
#   - output/tables/qa_det_tot_cobertura_por_tipo.csv
#   - output/tables/qa_det_tot_vs_suma.csv (si existe etiqueta TOTAL)
#   - output/tables/qa_det_tot_plaus_yoy.csv
#   - output/tables/qa_det_tot_exclusion_regiones.csv
###############################################################################

suppressPackageStartupMessages({
  library(here); library(readr); library(dplyr); library(tidyr); library(janitor)
  library(stringr); library(purrr); library(tibble); library(fs)
})

# Utils del proyecto
source(here::here("scripts","00_utils_limpieza.R"))
root_init("scripts/15_limpieza_detenciones_totales.R")

msg <- function(...) message("[15] ", paste0(...))

YEAR_MIN <- 2010L; YEAR_MAX <- 2023L; YEARS_SEQ <- YEAR_MIN:YEAR_MAX

# ---------- Rutas ----------
fp_in   <- here::here("data","raw","detenciones_tipologia.csv")
fp_map  <- here::here("config","map_tipologias.csv")
fp_nat  <- here::here("data","processed","detenciones_totales_total_nacional.csv")
fp_tot  <- here::here("data","processed","detenciones_totales_total.csv")
qa_dir  <- here::here("output","tables")
dir.create(qa_dir, recursive = TRUE, showWarnings = FALSE)

assert_infile(fp_in)

# ---------- Lectura + detección de columnas ----------
raw <- read_raw(fp_in)  # usa autodetección sep/encoding y todo a texto
cn  <- names(raw); cn_low <- tolower(cn)

pick_col <- function(patterns, not = NULL) {
  pat <- paste(patterns, collapse = "|")
  hit <- stringr::str_detect(cn_low, pat)
  if (!is.null(not)) hit <- hit & !stringr::str_detect(cn_low, not)
  idx <- which(hit)[1]
  if (length(idx) == 0 || is.na(idx)) return(NA_character_)
  cn[idx]
}

col_region  <- pick_col(c("comun|ccaa|autono|territ|ambit|ámbito|ambito|region"))
col_tipo    <- pick_col(c("tipolog|delit|infracc|tipo"))

# --- periodo: intenta detect_year_col; si no, cae a patrones ------------------
col_periodo <- detect_year_col(cn)
if (is.null(col_periodo) || is.na(col_periodo) || !nzchar(col_periodo)) {
  col_periodo <- pick_col(c("period|anio|a\u00f1o|ano|fecha|year|time"))
}

col_valor   <- pick_col(c("^total$","^valor$","detenc|hechos|conteo|numero|n[0-9]*$"))

if (any(is.na(c(col_tipo, col_periodo, col_valor)))) {
  stop(sprintf(
    "❌ Columnas requeridas no detectadas.\n  region: %s\n  tipo: %s\n  periodo: %s\n  valor: %s",
    ifelse(is.na(col_region),  "<NA>", col_region),
    ifelse(is.na(col_tipo),    "<NA>", col_tipo),
    ifelse(is.na(col_periodo), "<NA>", col_periodo),
    ifelse(is.na(col_valor),   "<NA>", col_valor)
  ), call. = FALSE)
}

DT <- raw |>
  transmute(
    region  = if (is.na(col_region)) "TOTAL NACIONAL" else .data[[col_region]],
    tipo    = as.character(.data[[col_tipo]]),
    periodo = .data[[col_periodo]],
    valor   = limpia_num(.data[[col_valor]])   # parser robusto ES/EN + tokens NA
  ) |>
  mutate(ano = extract_year(periodo)) |>
  select(ano, region, tipo, valor)

# ---------- QA 1: años fuera de rango + NAs ----------
qa_anios_fuera <- DT |>
  mutate(flag_fuera = is.na(ano) | ano < YEAR_MIN | ano > YEAR_MAX) |>
  filter(flag_fuera) |>
  arrange(ano)
if (nrow(qa_anios_fuera)) write_clean(qa_anios_fuera, file.path(qa_dir, "qa_det_tot_anios_fuera_rango.csv"))

DT <- DT |> filter(!is.na(ano), dplyr::between(ano, YEAR_MIN, YEAR_MAX))

qa_na <- DT |> filter(is.na(valor) | is.na(tipo))
if (nrow(qa_na)) write_clean(qa_na, file.path(qa_dir, "qa_det_tot_na.csv"))

# ---------- Normaliza texto (región/tipo) ----------
normalize_text <- function(x){
  x |> norm_ascii() |> stringr::str_squish() |> tolower()
}
DT <- DT |> mutate(region_norm = normalize_text(region), tipo_norm = normalize_text(tipo))

# ---------- Diagnóstico de exclusión por regiones no-CCAA ----------
excluir_regiones <- c("desconocida","no consta","no especificado","sin especificar",
                      "en el extranjero","extranjero","otros territorios","resto del mundo")
ex_diag <- DT |>
  mutate(excluida = region_norm %in% excluir_regiones | region_norm %in% c("total nacional","nacional","total")) |>
  count(region, region_norm, excluida, name = "n") |>
  arrange(desc(excluida), desc(n))
write_clean(ex_diag, file.path(qa_dir, "qa_det_tot_exclusion_regiones.csv"))

# ---------- Agregación a Total Nacional (por tipología) ----------
has_total_nacional <- any(DT$region_norm %in% c("total nacional","nacional","total"), na.rm = TRUE)

nat <- if (has_total_nacional) {
  msg("Usando 'TOTAL NACIONAL' presente en detenciones.")
  DT |> filter(region_norm %in% c("total nacional","nacional","total")) |>
    group_by(ano, tipo) |>
    summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop")
} else {
  msg("No hay 'TOTAL NACIONAL' → sumando CCAA válidas (excluye desconocida/extranjero).")
  DT |> filter(!region_norm %in% c(excluir_regiones, "total nacional","nacional","total")) |>
    group_by(ano, tipo) |>
    summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop")
}

# ---------- Normalizar tipologías (si hay mapping) por clave normalizada ------
if (file.exists(fp_map)){
  map_tip <- readr::read_csv(fp_map, show_col_types = FALSE) |>
    janitor::clean_names() |>
    dplyr::distinct(raw_tipo, .keep_all = TRUE) |>
    dplyr::mutate(raw_tipo_norm = normalize_text(raw_tipo))
  
  nat <- nat |>
    dplyr::mutate(tipo_norm = normalize_text(tipo)) |>
    dplyr::left_join(map_tip |> dplyr::select(raw_tipo_norm, propuesta_tipo),
                     by = c("tipo_norm" = "raw_tipo_norm")) |>
    dplyr::mutate(tipo = dplyr::coalesce(propuesta_tipo, tipo)) |>
    dplyr::select(-propuesta_tipo, -tipo_norm)
}

# ---------- QA 2: negativos, duplicados, cobertura ----------
qa_neg <- nat |> filter(!is.na(valor) & valor < 0)
if (nrow(qa_neg)) write_clean(qa_neg, file.path(qa_dir, "qa_det_tot_negativos.csv"))

qa_dup <- nat |> count(ano, tipo, name = "n") |> filter(n > 1)
if (nrow(qa_dup)) write_clean(qa_dup, file.path(qa_dir, "qa_det_tot_duplicados.csv"))

present <- sort(unique(nat$ano)); missing <- setdiff(YEARS_SEQ, present)
qa_cov <- tibble(variable = "detenciones_totales",
                 years_min = ifelse(length(present) > 0, min(present), NA_integer_),
                 years_max = ifelse(length(present) > 0, max(present), NA_integer_),
                 n_years = length(present), n_missing = length(missing),
                 missing_list = paste(missing, collapse = ", "))
write_clean(qa_cov, file.path(qa_dir, "qa_det_tot_cobertura.csv"))

qa_cov_tipo <- nat |> group_by(tipo) |>
  summarise(n_years = n_distinct(ano),
            complete = as.integer(n_years == length(YEARS_SEQ)),
            missing_list = paste(setdiff(YEARS_SEQ, sort(unique(ano))), collapse = ", "),
            .groups = "drop")
write_clean(qa_cov_tipo, file.path(qa_dir, "qa_det_tot_cobertura_por_tipo.csv"))

# ---------- Selección del TOTAL y contraste con suma de tipologías ----------
norm_tipo <- normalize_text(nat$tipo)
mask_total_label <- grepl("^total", norm_tipo) & grepl("deten|investig|persona|imputad", norm_tipo)

nat_totlabel <- nat |> filter(mask_total_label) |> transmute(ano, det_tot = valor)

nat_sum <- nat |> filter(!mask_total_label) |>
  group_by(ano) |>
  summarise(det_tot_sum = sum(valor, na.rm = TRUE), .groups = "drop")

det_tot <- if (nrow(nat_totlabel) > 0) {
  cmp <- nat_totlabel |>
    full_join(nat_sum, by = "ano") |>
    mutate(
      diff = det_tot - det_tot_sum,
      rel_diff = if_else(det_tot_sum > 0, diff / det_tot_sum, NA_real_)
    ) |>
    arrange(ano)
  write_clean(cmp, file.path(qa_dir, "qa_det_tot_vs_suma.csv"))
  # Criterio: |rel_diff| <= 0,5% o |diff| <= 100
  cmp |>
    transmute(
      ano,
      det_tot = dplyr::case_when(
        is.na(det_tot) ~ det_tot_sum,
        is.na(det_tot_sum) ~ det_tot,
        abs(rel_diff) <= 0.005 | abs(diff) <= 100 ~ det_tot,
        TRUE ~ det_tot_sum
      )
    )
} else {
  nat_sum |> transmute(ano, det_tot = det_tot_sum)
}

# ---------- QA 3: plausibilidad y YoY del TOTAL ----------
df_tot <- det_tot |> arrange(ano)
pl_ok <- is.finite(min(df_tot$det_tot, na.rm = TRUE)) &&
  is.finite(max(df_tot$det_tot, na.rm = TRUE)) &&
  (min(df_tot$det_tot, na.rm = TRUE) >= 5e4) && (max(df_tot$det_tot, na.rm = TRUE) <= 2e6)

df_tot <- df_tot |> mutate(
  yoy = det_tot / dplyr::lag(det_tot) - 1,
  yoy_flag = abs(yoy) > 0.25
)
qap <- tibble(
  min = suppressWarnings(min(df_tot$det_tot, na.rm = TRUE)),
  max = suppressWarnings(max(df_tot$det_tot, na.rm = TRUE)),
  plaus_ok = pl_ok,
  yoy_max_abs = suppressWarnings(max(abs(df_tot$yoy), na.rm = TRUE)),
  yoy_breaches = sum(df_tot$yoy_flag, na.rm = TRUE)
)
write_clean(qap, file.path(qa_dir, "qa_det_tot_plaus_yoy.csv"))

# ---------- Salidas ----------
final_nat <- nat |>
  arrange(ano, tipo) |>
  select(ano, tipo, valor)
write_clean(final_nat, fp_nat)

final_tot <- det_tot |>
  arrange(ano) |>
  select(ano, det_tot)
write_clean(final_tot, fp_tot)

msg("✔ Detenciones totales (TOTAL NACIONAL por tipología + serie total) generadas.")




# 15_qc_console.R — Smoke check en consola para detenciones (script 15)
qc15_console <- function(strict = FALSE,
                         year_min = 2010L, year_max = 2023L,
                         tol_rel = 0.005, tol_abs = 100) {
  
  suppressPackageStartupMessages({
    library(here); library(readr); library(dplyr); library(janitor)
  })
  
  YEARS <- year_min:year_max
  p_nat <- here::here("data","processed","detenciones_totales_total_nacional.csv")
  p_tot <- here::here("data","processed","detenciones_totales_total.csv")
  
  # Existencia
  if (!file.exists(p_nat) || !file.exists(p_tot)) {
    cat("❌ Faltan salidas del 15:\n",
        "   -", if (!file.exists(p_nat)) "NO " else "OK ", "detenciones_totales_total_nacional.csv\n",
        "   -", if (!file.exists(p_tot)) "NO " else "OK ", "detenciones_totales_total.csv\n", sep = "")
    if (strict) stop("Falta(n) archivo(s) de salida.", call. = FALSE) else return(invisible(FALSE))
  }
  
  # Lectura
  nat <- readr::read_csv(p_nat, show_col_types = FALSE) |> janitor::clean_names()
  tot <- readr::read_csv(p_tot, show_col_types = FALSE) |> janitor::clean_names()
  
  # Esquema
  req1 <- all(c("ano","tipo","valor") %in% names(nat))
  req2 <- all(c("ano","det_tot") %in% names(tot))
  if (!req1 || !req2) {
    cat("❌ Esquema incorrecto.\n")
    if (strict) stop("Esquema incorrecto.", call. = FALSE) else return(invisible(FALSE))
  }
  
  # Tipos/valores
  nat$valor   <- suppressWarnings(as.numeric(nat$valor))
  tot$det_tot <- suppressWarnings(as.numeric(tot$det_tot))
  
  # Cobertura + duplicados
  cov_nat <- sort(unique(nat$ano)); cov_tot <- sort(unique(tot$ano))
  miss_nat <- setdiff(YEARS, cov_nat); miss_tot <- setdiff(YEARS, cov_tot)
  dup_nat <- nat |> count(ano, tipo) |> filter(n > 1) |> nrow()
  dup_tot <- tot |> count(ano) |> filter(n > 1) |> nrow()
  
  # NAs/negativos
  na_nat <- sum(!is.finite(nat$valor))
  na_tot <- sum(!is.finite(tot$det_tot))
  neg_nat <- sum(nat$valor < 0, na.rm = TRUE)
  neg_tot <- sum(tot$det_tot < 0, na.rm = TRUE)
  
  # Suma por tipologías vs total (tolerancias)
  sum_by_year <- nat |> group_by(ano) |> summarise(sum_nat = sum(valor, na.rm = TRUE), .groups = "drop")
  cmp <- tot |> left_join(sum_by_year, by = "ano") |>
    mutate(diff = det_tot - sum_nat,
           rel_diff = if_else(sum_nat > 0, diff / sum_nat, NA_real_),
           pass = is.na(rel_diff) | (abs(rel_diff) <= tol_rel | abs(diff) <= tol_abs))
  fails_cmp <- sum(!cmp$pass, na.rm = TRUE)
  
  # Plausibilidad + YoY
  rng <- range(tot$det_tot, na.rm = TRUE)
  plaus_ok <- is.finite(rng[1]) && is.finite(rng[2]) && rng[1] >= 5e4 && rng[2] <= 2e6
  yoy_breaches <- tot |> arrange(ano) |> mutate(yoy = det_tot / dplyr::lag(det_tot) - 1,
                                                flag = abs(yoy) > 0.25) |> summarise(n = sum(flag, na.rm = TRUE)) |> pull(n)
  
  # ---- Output en consola ----
  cat("\n🧪 QC detenciones — resumen\n")
  cat(sprintf("• Cobertura NAT: %d/%d (%s–%s)  faltan: %s\n",
              length(cov_nat), length(YEARS),
              ifelse(length(cov_nat)>0, min(cov_nat), NA_integer_),
              ifelse(length(cov_nat)>0, max(cov_nat), NA_integer_),
              ifelse(length(miss_nat)==0, "ninguno", paste(miss_nat, collapse=", "))))
  cat(sprintf("• Cobertura TOT: %d/%d (%s–%s)  faltan: %s\n",
              length(cov_tot), length(YEARS),
              ifelse(length(cov_tot)>0, min(cov_tot), NA_integer_),
              ifelse(length(cov_tot)>0, max(cov_tot), NA_integer_),
              ifelse(length(miss_tot)==0, "ninguno", paste(miss_tot, collapse=", "))))
  cat(sprintf("• Duplicados (nat/tot): %d / %d\n", dup_nat, dup_tot))
  cat(sprintf("• NAs (nat/tot): %d / %d   Negativos (nat/tot): %d / %d\n", na_nat, na_tot, neg_nat, neg_tot))
  cat(sprintf("• sum(tipos) vs total — fallos: %d  (tol rel=%.3f, abs=%d)\n", fails_cmp, tol_rel, tol_abs))
  cat(sprintf("• Plausible rango total: %s   |YoY|>25%%: %d\n", ifelse(plaus_ok,"OK","NO"), yoy_breaches))
  
  hard_fail <- any(c(length(miss_nat)>0, length(miss_tot)>0,
                     na_nat>0, na_tot>0, neg_nat>0, neg_tot>0,
                     dup_nat>0, dup_tot>0, fails_cmp>0, !plaus_ok))
  
  if (hard_fail) {
    cat("❌ FAIL — revisa los puntos rojos arriba.\n\n")
    if (strict) stop("QC 15 falló (modo estricto).", call. = FALSE)
    return(invisible(FALSE))
  } else {
    cat("✅ PASS — 15 (detenciones) OK\n\n")
    return(invisible(TRUE))
  }
}
# --- Helpers robustos para Top-5 violentos -----------------------------------
.top5_build <- function(wide){
  suppressPackageStartupMessages({ library(dplyr); library(stringr); library(tidyr); library(janitor) })
  wide <- janitor::clean_names(wide)
  
  norm <- function(x){ x <- tolower(x); iconv(x, to="ASCII//TRANSLIT"); }
  
  # Patrones objetivo (agregados principales)
  pat <- list(
    homicidios = "homicidios?\\s*dolosos?.*asesinatos?.*consumados?",
    lesiones   = "^\\s*(\\d+\\.)?\\s*.*\\blesiones\\b(?!.*lev(e|es)|.*imprudenc)",   # evita leves/imprudentes si existieran
    mal_trat   = "malos\\s*tratos\\s.*ambito\\s*familiar(?!.*habitual)",             # excluye 'habituales'
    robos_viol = "robos?\\s*con\\s*violencia.*intimidacion",
    agres_tot  = "agresiones?\\s*sexuales?.*(\\(total\\)|\\btotal\\b|$)"             # acepta '(total)' o 'total' o solo etiqueta base
  )
  
  # 1) Normaliza y marca subcategorías a excluir explícitamente
  df <- wide |>
    mutate(
      delito5_norm = norm(delito5),
      is_sub_maltr = str_detect(delito5_norm, "malos\\s*tratos.*habitual"),
      is_agres     = str_detect(delito5_norm, "agresiones?\\s*sexuales?")
    )
  
  # 2) Por año, construimos cada objetivo:
  build_year <- function(yy){
    d <- df |> filter(ano == yy)
    
    # a) Homicidios (consumados)
    a_hom <- d |> filter(str_detect(delito5_norm, pat$homicidios)) |>
      summarise(cat="Homicidios consumados", total=sum(total,na.rm=TRUE),
                extranjeros=sum(extranjeros,na.rm=TRUE), .groups="drop")
    
    # b) Lesiones (agregado); si hay varias filas de lesiones, suma todas menos variantes “leves” si existieran
    a_les <- d |> filter(str_detect(delito5_norm, pat$lesiones)) |>
      summarise(cat="Lesiones", total=sum(total,na.rm=TRUE),
                extranjeros=sum(extranjeros,na.rm=TRUE), .groups="drop")
    
    # c) Malos tratos (agregado, excluyendo 'habituales')
    a_mal <- d |> filter(str_detect(delito5_norm, pat$mal_trat) & !is_sub_maltr) |>
      summarise(cat="Malos tratos (ámbito familiar)", total=sum(total,na.rm=TRUE),
                extranjeros=sum(extranjeros,na.rm=TRUE), .groups="drop")
    
    # d) Robos con violencia e intimidación
    a_rob <- d |> filter(str_detect(delito5_norm, pat$robos_viol)) |>
      summarise(cat="Robos con violencia e intimidación", total=sum(total,na.rm=TRUE),
                extranjeros=sum(extranjeros,na.rm=TRUE), .groups="drop")
    
    # e) Agresiones sexuales (TOTAL preferente). Si no existe fila 'total', agregamos todas las agresiones sexuales.
    agres_total_exist <- d |> filter(str_detect(delito5_norm, pat$agres_tot))
    a_agr <- if (nrow(agres_total_exist) > 0) {
      agres_total_exist |>
        summarise(cat="Agresiones sexuales (total)", total=sum(total,na.rm=TRUE),
                  extranjeros=sum(extranjeros,na.rm=TRUE), .groups="drop")
    } else {
      # fallback: suma todas las filas de agresiones sexuales
      d |> filter(is_agres) |>
        summarise(cat="Agresiones sexuales (total)*", total=sum(total,na.rm=TRUE),
                  extranjeros=sum(extranjeros,na.rm=TRUE), .groups="drop")
    }
    
    out <- bind_rows(a_hom, a_les, a_mal, a_rob, a_agr) |>
      mutate(ano = yy,
             pct_extranjeros = if_else(total > 0, 100 * extranjeros / total, NA_real_)) |>
      select(ano, delito5 = cat, total, extranjeros, pct_extranjeros)
    
    # si alguna categoría quedó vacía, rellena con NA para visibilidad
    needed <- c("Homicidios consumados","Lesiones",
                "Malos tratos (ámbito familiar)",
                "Robos con violencia e intimidación",
                "Agresiones sexuales (total)","Agresiones sexuales (total)*")
    out <- out |> complete(delito5 = needed, fill=list(total=NA_real_, extranjeros=NA_real_, pct_extranjeros=NA_real_)) |>
      filter(delito5 %in% needed[1:5] | delito5 == "Agresiones sexuales (total)*")
    
    out
  }
  
  years <- sort(unique(df$ano))
  bind_rows(lapply(years, build_year)) |>
    arrange(ano, factor(delito5, levels = c(
      "Homicidios consumados",
      "Lesiones",
      "Malos tratos (ámbito familiar)",
      "Robos con violencia e intimidación",
      "Agresiones sexuales (total)",
      "Agresiones sexuales (total)*"  # solo si fue necesario el fallback
    )))
}

# --- QC en consola (reemplaza tu qc_top5_console por este) --------------------
qc_top5_console <- function(strict = FALSE) {
  suppressPackageStartupMessages({ library(here); library(readr); library(dplyr); library(janitor) })
  
  p_top5 <- here::here("output","tables","detenciones_violentos_top5_pct_ext.csv")
  p_wide <- here::here("output","tables","detenciones_tabla_wide.csv")
  
  if (file.exists(p_top5)) {
    base <- readr::read_csv(p_top5, show_col_types = FALSE) |> janitor::clean_names()
  } else if (file.exists(p_wide)) {
    wide <- readr::read_csv(p_wide, show_col_types = FALSE) |> janitor::clean_names()
    base <- .top5_build(wide)
  } else {
    stop("❌ No encuentro ni detenciones_violentos_top5_pct_ext.csv ni detenciones_tabla_wide.csv", call. = FALSE)
  }
  
  # Validaciones
  base$total        <- suppressWarnings(as.numeric(base$total))
  base$extranjeros  <- suppressWarnings(as.numeric(base$extranjeros))
  base$pct_extranjeros <- suppressWarnings(as.numeric(base$pct_extranjeros))
  
  by_year <- base |> count(ano, name="n")
  bad <- by_year |> filter(n != 5)
  
  cat("\n🔎 QC Top-5 violentos — resumen\n")
  cat(sprintf("• Años cubiertos: %d (%s–%s)\n", length(unique(base$ano)), min(base$ano), max(base$ano)))
  cat(sprintf("• Filas por año = 5: %s %s\n",
              ifelse(nrow(bad)==0,"OK","NO"),
              ifelse(nrow(bad)==0,"", paste0(" — años con !=5: ", paste(bad$ano, collapse=", ")))))
  cat(sprintf("• NAs → total: %d | pct_extranjeros: %d\n",
              sum(!is.finite(base$total)), sum(is.na(base$pct_extranjeros))))
  cat(sprintf("• pct_extranjeros fuera [0,100]: %d | extranjeros>total: %d\n",
              sum(base$pct_extranjeros < 0 | base$pct_extranjeros > 100, na.rm=TRUE),
              nrow(base |> filter(!is.na(extranjeros), !is.na(total), extranjeros > total))))
  last <- max(base$ano)
  cat(sprintf("\n• Preview %d:\n", last))
  print(base |> filter(ano==last), n=5)
  
  hard_fail <- nrow(bad) > 0 ||
    any(base$pct_extranjeros < 0 | base$pct_extranjeros > 100, na.rm=TRUE) ||
    nrow(base |> filter(!is.na(extranjeros), !is.na(total), extranjeros > total)) > 0
  
  if (hard_fail) {
    cat("\n❌ FAIL — revisa mapping/regex. (Se evita subapartados y se agregan agresiones si falta TOTAL)\n\n")
    if (strict) stop("QC Top-5 falló (modo estricto).", call. = FALSE)
    invisible(FALSE)
  } else {
    cat("\n✅ PASS — Top-5 violentos OK\n\n")
    invisible(TRUE)
  }
}


# ---------- Extra: Top-5 violentos (mismos nombres que el 13) ----------------
suppressPackageStartupMessages({ library(tidyr) })

norm_simple <- function(x){
  x <- tolower(as.character(x))
  x <- iconv(x, to = "ASCII//TRANSLIT")
  gsub("[[:space:]]+", " ", trimws(x))
}

p_ext <- here::here("data","processed","detenciones_extranjeros_total_nacional.csv")
p_map <- here::here("config","map_tipologias.csv")
p_out_top5 <- here::here("output","tables","detenciones_violentos_top5_pct_ext.csv")
p_out_chk  <- here::here("output","tables","detenciones_violentos_top5_check.csv")
# CSV gemelo con el formato del 13 (solo totales en ancho y con los mismos encabezados)
p_out_like13 <- here::here("output","tables","detenciones_violentos_top5_like13.csv")

# Partimos de 'nat' (ano, tipo, valor) ya creado arriba
stopifnot(all(c("ano","tipo","valor") %in% names(nat)))

# 1) Mapping a 'delito5' (si existe mapping, úsalo; si no, usa 'tipo')
delito_col <- "tipo"
nat_for_top <- nat
if (file.exists(p_map)) {
  map_tip2 <- readr::read_csv(p_map, show_col_types = FALSE) |> janitor::clean_names()
  objetivo <- intersect(c("delito5","propuesta_tipo","tipo_simpl","tipo5","grupo_delito"), names(map_tip2))
  if (length(objetivo) >= 1 && all(c("raw_tipo", objetivo[1]) %in% names(map_tip2))) {
    key <- objetivo[1]
    nat_for_top <- nat_for_top |>
      dplyr::left_join(map_tip2 |> dplyr::select(raw_tipo, !!rlang::sym(key)),
                       by = c("tipo" = "raw_tipo")) |>
      dplyr::mutate(delito5 = dplyr::coalesce(.data[[key]], tipo))
    delito_col <- "delito5"
  } else {
    nat_for_top <- nat_for_top |> dplyr::mutate(delito5 = tipo); delito_col <- "delito5"
  }
} else {
  nat_for_top <- nat_for_top |> dplyr::mutate(delito5 = tipo); delito_col <- "delito5"
}

tot_agg <- nat_for_top |>
  dplyr::group_by(ano, !!rlang::sym(delito_col)) |>
  dplyr::summarise(total = sum(as.numeric(valor), na.rm = TRUE), .groups = "drop") |>
  dplyr::rename(delito5 = !!rlang::sym(delito_col))

# 2) Añade extranjeros si existe fichero; si no, deja NA
has_ext <- file.exists(p_ext)
if (has_ext) {
  ext_raw <- readr::read_csv(p_ext, show_col_types = FALSE) |> janitor::clean_names()
  has_ext <- all(c("ano","tipo","valor") %in% names(ext_raw))
}
if (has_ext) {
  ext_for_top <- ext_raw
  if (exists("map_tip2") && exists("objetivo") && length(objetivo) >= 1 &&
      all(c("raw_tipo", objetivo[1]) %in% names(map_tip2))) {
    key <- objetivo[1]
    ext_for_top <- ext_for_top |>
      dplyr::left_join(map_tip2 |> dplyr::select(raw_tipo, !!rlang::sym(key)),
                       by = c("tipo" = "raw_tipo")) |>
      dplyr::mutate(delito5 = dplyr::coalesce(.data[[key]], tipo))
  } else {
    ext_for_top <- ext_for_top |> dplyr::mutate(delito5 = tipo)
  }
  ext_agg <- ext_for_top |>
    dplyr::group_by(ano, delito5) |>
    dplyr::summarise(extranjeros = sum(as.numeric(valor), na.rm = TRUE), .groups = "drop")
  base_wide <- tot_agg |>
    dplyr::left_join(ext_agg, by = c("ano","delito5")) |>
    dplyr::mutate(extranjeros = tidyr::replace_na(extranjeros, 0),
                  pct_extranjeros = dplyr::if_else(total > 0, 100 * extranjeros / total, NA_real_))
} else {
  base_wide <- tot_agg |>
    dplyr::mutate(extranjeros = NA_real_, pct_extranjeros = NA_real_)
}

# 3) Patrones (norm.) y construcción por año con ETIQUETAS EXACTAS DEL “13”
pat_hom     <- "homicidios?\\s*dolosos?.*asesinatos?.*consumados?"
pat_les     <- "(^|[^a-z])lesiones($|[^a-z])"                      # sumaremos y luego renombramos a “Lesiones graves”
pat_mt      <- "malos\\s*tratos.*ambito\\s*familiar"
pat_mt_hab  <- "malos\\s*tratos.*habituales?"
pat_rv      <- "robos?\\s*con\\s*violencia.*intimidacion"
pat_as_total <- "(agresion|agresiones)\\s*sexual(es)?\\s*.*(\\(total\\)|\\btotal\\b)"
# fallback SOLO 3.1 y 3.2
pat_as_any   <- "(agresion|agresiones)\\s*sexual(es)?"
pat_31 <- "^\\s*3\\.1\\b|agresion\\s*sexual(\\b|$)"
pat_32 <- "^\\s*3\\.2\\b|agresion\\s*sexual\\s*con\\s*penetracion"

years <- sort(unique(base_wide$ano))
build_year <- function(yy){
  d <- base_wide |>
    dplyr::filter(ano == yy) |>
    dplyr::mutate(delito5_norm = norm_simple(delito5))
  
  # a) Homicidios (etiqueta del 13)
  a_hom <- d[grepl(pat_hom, d$delito5_norm), ]
  s_hom <- data.frame(delito5="Homicidios dolosos y asesinatos consumados",
                      total=sum(a_hom$total,na.rm=TRUE),
                      extranjeros=sum(a_hom$extranjeros,na.rm=TRUE))
  
  # b) Lesiones → etiqueta del 13 = “Lesiones graves”
  a_les <- d[grepl(pat_les, d$delito5_norm), ]
  s_les <- data.frame(delito5="Lesiones graves",
                      total=sum(a_les$total,na.rm=TRUE),
                      extranjeros=sum(a_les$extranjeros,na.rm=TRUE))
  
  # c) Malos tratos (excluye “habituales”), etiqueta del 13
  a_mt  <- d[grepl(pat_mt, d$delito5_norm) & !grepl(pat_mt_hab, d$delito5_norm), ]
  s_mt  <- data.frame(delito5="Malos tratos en el ámbito familiar",
                      total=sum(a_mt$total,na.rm=TRUE),
                      extranjeros=sum(a_mt$extranjeros,na.rm=TRUE))
  
  # d) Robos con violencia e intimidación (igual)
  a_rv  <- d[grepl(pat_rv, d$delito5_norm), ]
  s_rv  <- data.frame(delito5="Robos con violencia e intimidación",
                      total=sum(a_rv$total,na.rm=TRUE),
                      extranjeros=sum(a_rv$extranjeros,na.rm=TRUE))
  
  # e) Agresiones sexuales — etiqueta EXACTA del 13 sin asterisco
  a_as_total <- d[grepl(pat_as_total, d$delito5_norm), ]
  if (nrow(a_as_total) > 0) {
    a_as <- a_as_total
  } else {
    sel_31_32 <- (grepl(pat_31, d$delito5_norm) | grepl(pat_32, d$delito5_norm)) & grepl(pat_as_any, d$delito5_norm)
    a_as <- d[sel_31_32, ]
  }
  s_as  <- data.frame(delito5="Agresiones sexuales (total)",
                      total=sum(a_as$total,na.rm=TRUE),
                      extranjeros=sum(a_as$extranjeros,na.rm=TRUE))
  
  out <- dplyr::bind_rows(s_as, s_hom, s_les, s_mt, s_rv) |>
    dplyr::mutate(ano = yy,
                  pct_extranjeros = dplyr::if_else(total>0, 100*extranjeros/total, NA_real_)) |>
    dplyr::select(ano, delito5, total, extranjeros, pct_extranjeros)
  
  out
}

top5 <- dplyr::bind_rows(lapply(years, build_year))

# Orden EXACTO como en tu CSV del 13
order_13 <- c("Agresiones sexuales (total)",
              "Homicidios dolosos y asesinatos consumados",
              "Lesiones graves",
              "Malos tratos en el ámbito familiar",
              "Robos con violencia e intimidación")
top5 <- top5 |>
  dplyr::mutate(delito5 = factor(delito5, levels = order_13)) |>
  dplyr::arrange(ano, delito5)

# Check 5 por año
chk <- top5 |> dplyr::count(ano, name="n_delitos") |> dplyr::mutate(ok_5 = (n_delitos == 5L))
readr::write_csv(top5, p_out_top5)
readr::write_csv(chk,  p_out_chk)

# 4) Export “como el 13” (ancho, solo totales, columnas en ese orden)
like13 <- top5 |>
  dplyr::select(ano, delito5, total) |>
  tidyr::pivot_wider(names_from = delito5, values_from = total) |>
  dplyr::select(ano, dplyr::all_of(order_13))
readr::write_csv(like13, p_out_like13)

msg("Top5 (etiquetas 13) → escritos:")
msg(basename(p_out_top5)); msg(basename(p_out_chk)); msg(basename(p_out_like13))
# ----------------------------------------------------------------------------- 







qc_top5_console()
# o en modo estricto:
qc_top5_console(strict = TRUE)


