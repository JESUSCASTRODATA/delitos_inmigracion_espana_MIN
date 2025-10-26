#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
################################################################################
# 20a_audit_core_y_derivadas.R
# Proyecto   : Delitos e Inmigración en España (2010–2023)
# Autor      : Jesús Castro · JESUSCASTRODATA
# Licencia   : MIT (código) · CC BY 4.0 (docs/figuras)
# Descripción:
#   Auditoría integral del core y derivadas (2010–2023):
#   - Estructura: columnas, cobertura (min/max/years/missing), duplicados
#   - Cotas/rangos: población>0; hc/he/det>=0; %/share en rangos plausibles
#   - Identidades: tasas por 100k y share = det_ext/det_tot*100
#   - Derivadas: lags, Δlog consistentes; PIB YoY / índice 2010 / z-score
#   - Salidas: CSVs de QAs, resumen en consola, 20a_summary.md y 20a_summary.json
#   - CI/Orquestador: devuelve exit code 1 si algún check falla
################################################################################

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(tidyr); library(janitor)
  library(stringr); library(here); library(fs); library(tibble); library(purrr)
  library(jsonlite)
})

`%||%` <- function(x, y) if (is.null(x) || is.na(x)) y else x

YEAR_MIN <- 2010L; YEAR_MAX <- 2023L
tol_abs  <- 1e-8        # tolerancia absoluta para Δlog / yoy
tol_rel  <- 5e-4        # 0.05% para identidades (tasas/share)

msg   <- function(...) message("[20a] ", paste0(...))
qa_dir <- here::here("output","tables"); fs::dir_create(qa_dir)

fp_core <- here::here("data","processed","core_indicadores.csv")
fp_der  <- here::here("data","processed","core_indicadores_derivadas.csv")

stopifnot(file.exists(fp_core))
if (!file.exists(fp_der)) msg("⚠ No existe core_indicadores_derivadas.csv; se omiten checks de derivadas.")

core <- readr::read_csv(fp_core, show_col_types = FALSE) |> clean_names()
der  <- if (file.exists(fp_der)) readr::read_csv(fp_der, show_col_types = FALSE) |> clean_names() else tibble()

# ───────── Cabeceras detectadas ─────────
msg("Columnas core: ", paste(names(core), collapse = ", "))
if (nrow(der)) msg("Columnas der : ", paste(names(der), collapse = ", "))

write_csv(tibble(dataset="core", columna = names(core)), file.path(qa_dir, "20a_core_columnas.csv"))
if (nrow(der)) write_csv(tibble(dataset="derivadas", columna = names(der)), file.path(qa_dir, "20a_der_columnas.csv"))

stopifnot("ano" %in% names(core))
core <- core |> mutate(ano = as.integer(ano))
if (nrow(der)) der <- der |> mutate(ano = as.integer(ano))

# ───────── Duplicados y cobertura ─────────
dup_core <- core |> count(ano, name="n") |> filter(!is.na(ano), n>1)
if (nrow(dup_core)) write_csv(dup_core, file.path(qa_dir, "20a_dup_core_por_ano.csv"))

cov_core <- tibble(
  years_min = suppressWarnings(min(core$ano, na.rm=TRUE)),
  years_max = suppressWarnings(max(core$ano, na.rm=TRUE)),
  n_years   = n_distinct(core$ano),
  missing   = paste(setdiff(YEAR_MIN:YEAR_MAX, sort(unique(core$ano))), collapse=", ")
)
write_csv(cov_core, file.path(qa_dir, "20a_cov_core.csv"))

if (nrow(der)) {
  dup_der <- der |> count(ano, name="n") |> filter(!is.na(ano), n>1)
  if (nrow(dup_der)) write_csv(dup_der, file.path(qa_dir, "20a_dup_der_por_ano.csv"))
  cov_der <- tibble(
    years_min = suppressWarnings(min(der$ano, na.rm=TRUE)),
    years_max = suppressWarnings(max(der$ano, na.rm=TRUE)),
    n_years   = n_distinct(der$ano),
    missing   = paste(setdiff(YEAR_MIN:YEAR_MAX, sort(unique(der$ano))), collapse=", ")
  )
  write_csv(cov_der, file.path(qa_dir, "20a_cov_der.csv"))
}

# ───────── Cotas / rangos ─────────
has <- function(cols, nm = names(core)) all(cols %in% nm)

pobl_neg <- if (has("poblacion_total")) core |> filter(is.finite(poblacion_total) & poblacion_total <= 0) else tibble()
hc_neg   <- if (has("hc_total"))        core |> filter(is.finite(hc_total)        & hc_total        < 0) else tibble()
he_neg   <- if (has("he_total"))        core |> filter(is.finite(he_total)        & he_total        < 0) else tibble()

det_neg  <- bind_rows(
  if (has("det_tot")) core |> filter(is.finite(det_tot)  & det_tot  < 0) else tibble(),
  if (has("det_ext")) core |> filter(is.finite(det_ext)  & det_ext  < 0) else tibble()
)

pct_out  <- if (has("pct_extranjeros"))
  core |> filter(is.finite(pct_extranjeros) & (pct_extranjeros < 0 | pct_extranjeros > 1)) else tibble()

shr_out  <- if (has("share_extranjeros"))
  core |> filter(is.finite(share_extranjeros) & (share_extranjeros < 0 | share_extranjeros > 100)) else tibble()

pib_pos  <- if (has("pib_pc_real"))
  core |> filter(is.finite(pib_pc_real) & pib_pc_real <= 0) else tibble()

viol_tbl <- bind_rows(
  pobl_neg |> mutate(check="pobl_neg"),
  hc_neg   |> mutate(check="hc_neg"),
  he_neg   |> mutate(check="he_neg"),
  det_neg  |> mutate(check="det_neg"),
  pct_out  |> mutate(check="pct_out"),
  shr_out  |> mutate(check="shr_out"),
  pib_pos  |> mutate(check="pib_pos")
)
if (nrow(viol_tbl)) write_csv(viol_tbl, file.path(qa_dir, "20a_violaciones_cotas.csv"))

# ───────── Identidades tasas / share ─────────
iden <- tibble()
if (has(c("poblacion_total","hc_total","he_total"))) {
  iden <- core |>
    mutate(
      tasa_hc_chk = ifelse(poblacion_total > 0, 1e5 * hc_total / poblacion_total, NA_real_),
      tasa_he_chk = ifelse(poblacion_total > 0, 1e5 * he_total / poblacion_total, NA_real_)
    )
  if ("tasa_hc_100k" %in% names(core))
    iden <- iden |> mutate(tasa_hc_err = abs(tasa_hc_100k - tasa_hc_chk) / pmax(1, abs(tasa_hc_chk)))
  if ("tasa_he_100k" %in% names(core))
    iden <- iden |> mutate(tasa_he_err = abs(tasa_he_100k - tasa_he_chk) / pmax(1, abs(tasa_he_chk)))
}
if (has(c("det_tot","det_ext"))) {
  iden <- if (nrow(iden)) iden else core
  iden <- iden |> mutate(share_chk = ifelse(det_tot > 0, 100 * det_ext / det_tot, NA_real_))
  if ("share_extranjeros" %in% names(core))
    iden <- iden |> mutate(share_err = abs(share_extranjeros - share_chk) / pmax(1, abs(share_chk)))
}

iden_bad <- tibble()
if (nrow(iden)) {
  iden_bad <- bind_rows(
    if ("tasa_hc_err" %in% names(iden)) iden |> filter(is.finite(tasa_hc_err) & tasa_hc_err > tol_rel) |> transmute(ano, tipo="tasa_hc_err", err=tasa_hc_err) else tibble(),
    if ("tasa_he_err" %in% names(iden)) iden |> filter(is.finite(tasa_he_err) & tasa_he_err > tol_rel) |> transmute(ano, tipo="tasa_he_err", err=tasa_he_err) else tibble(),
    if ("share_err"   %in% names(iden)) iden |> filter(is.finite(share_err)   & share_err   > tol_rel) |> transmute(ano, tipo="share_err",   err=share_err)   else tibble()
  )
  
  # ⬇⬇⬇ CORRECCIÓN: construir vector y usar any_of() correctamente
  cols_detalle <- c(
    "tasa_hc_100k","tasa_hc_chk","tasa_hc_err",
    "tasa_he_100k","tasa_he_chk","tasa_he_err",
    "share_extranjeros","share_chk","share_err"
  )
  
  readr::write_csv(
    iden |>
      dplyr::select(ano, tidyselect::any_of(cols_detalle)),
    file.path(qa_dir, "20a_identidades_detalle.csv")
  )
  
  if (nrow(iden_bad)) write_csv(iden_bad, file.path(qa_dir, "20a_identidades_violaciones.csv"))
}

# ───────── Consistencias de derivadas ─────────
lags_bad <- tibble(); dlog_bad <- tibble(); pib_yoy_bad <- tibble(); pib_idx_bad <- FALSE; pib_z_ok <- TRUE

if (nrow(der)) {
  d <- der |> arrange(ano)
  
  # Lags (ejemplo para % extranjeros)
  if (all(c("pct_extranjeros","pct_extranjeros_l1") %in% names(d))) {
    l_ok <- d$pct_extranjeros_l1 == dplyr::lag(d$pct_extranjeros)
    lags_bad <- tibble(ano = d$ano, ok = l_ok) |> filter(!(ok %in% c(TRUE, NA)))
    if (nrow(lags_bad)) write_csv(lags_bad, file.path(qa_dir, "20a_lags_violaciones.csv"))
  }
  
  # Δlog: ln_x - lag(ln_x) == dln_x
  dlog_bad <- bind_rows(
    if (all(c("ln_pib_pc","dln_pib_pc") %in% names(d)))
      d |> filter(is.finite(ln_pib_pc) & is.finite(dplyr::lag(ln_pib_pc)) &
                    abs(dln_pib_pc - (ln_pib_pc - dplyr::lag(ln_pib_pc))) > tol_abs) |>
      transmute(ano, var="pib_pc", err = abs(dln_pib_pc - (ln_pib_pc - dplyr::lag(ln_pib_pc)))) else tibble(),
    if (all(c("ln_hc","dln_hc") %in% names(d)))
      d |> filter(is.finite(ln_hc) & is.finite(dplyr::lag(ln_hc)) &
                    abs(dln_hc - (ln_hc - dplyr::lag(ln_hc))) > tol_abs) |>
      transmute(ano, var="hc", err = abs(dln_hc - (ln_hc - dplyr::lag(ln_hc)))) else tibble(),
    if (all(c("ln_he","dln_he") %in% names(d)))
      d |> filter(is.finite(ln_he) & is.finite(dplyr::lag(ln_he)) &
                    abs(dln_he - (ln_he - dplyr::lag(ln_he))) > tol_abs) |>
      transmute(ano, var="he", err = abs(dln_he - (ln_he - dplyr::lag(ln_he)))) else tibble()
  )
  if (nrow(dlog_bad)) write_csv(dlog_bad, file.path(qa_dir, "20a_dlog_violaciones.csv"))
  
  # PIB: YoY, índice base 2010, z-score
  if (all(c("pib_pc_real","pib_yoy") %in% names(d))) {
    pib_chk <- d |> transmute(ano, pib_pc_real, pib_yoy, yoy_chk = pib_pc_real / dplyr::lag(pib_pc_real) - 1) |>
      mutate(yoy_err = abs(pib_yoy - yoy_chk))
    pib_yoy_bad <- pib_chk |> filter(is.finite(yoy_err) & yoy_err > tol_abs)
    if (nrow(pib_yoy_bad)) write_csv(pib_yoy_bad, file.path(qa_dir, "20a_pib_yoy_violaciones.csv"))
  }
  if ("pib_idx2010" %in% names(d)) {
    idx2010 <- d |> filter(ano == 2010) |> pull(pib_idx2010)
    pib_idx_bad <- if (length(idx2010) && is.finite(idx2010)) abs(idx2010 - 100) > 1e-10 else FALSE
  }
  if ("pib_z" %in% names(d)) {
    z <- d$pib_z[is.finite(d$pib_z)]
    if (length(z) >= 3) pib_z_ok <- (abs(mean(z)) < 1e-6 & abs(sd(z) - 1) < 1e-6)
  }
}

# ───────── Check adicional de dominio: HE ≤ HC (agregado) ─────────
he_gt_hc <- tibble()
if (all(c("he_total","hc_total") %in% names(core))) {
  he_gt_hc <- core |> filter(is.finite(he_total), is.finite(hc_total), he_total > hc_total)
  if (nrow(he_gt_hc)) readr::write_csv(he_gt_hc, file.path(qa_dir, "20a_he_mayor_que_hc.csv"))
}

# ───────── Resumen consola ─────────
cat("\n=== QA 20a — Informe rápido ===\n")
cov_ok <- (suppressWarnings(min(core$ano, na.rm=TRUE)) <= YEAR_MIN) &
  (suppressWarnings(max(core$ano, na.rm=TRUE)) >= YEAR_MAX)

dup_n      <- nrow(dup_core)
viol_n     <- nrow(viol_tbl)
iden_n     <- nrow(iden_bad)
dlog_n     <- nrow(dlog_bad)
pib_yoy_n  <- nrow(pib_yoy_bad)

cat("Cobertura core (2010–2023): ", if (cov_ok) "PASS" else "FAIL", "\n", sep = "")
cat("Duplicados core: ", if (dup_n == 0) "PASS" else paste0("FAIL (", dup_n, ")"), "\n", sep = "")
cat("Cotas/rangos: ", if (viol_n == 0) "PASS" else "WARN/FAIL (ver 20a_violaciones_cotas.csv)", "\n", sep = "")

if (nrow(iden)) {
  cat("Identidades tasas/share: ", if (iden_n == 0) "PASS" else "FAIL (ver 20a_identidades_violaciones.csv)", "\n", sep = "")
} else {
  cat("Identidades tasas/share: OMITIDO (faltan columnas)\n", sep = "")
}

if (nrow(der)) {
  cat("Lags/Δlog PIB/HC/HE: ", if (dlog_n == 0) "PASS" else "FAIL (ver 20a_dlog_violaciones.csv)", "\n", sep = "")
  cat("PIB YoY/index/z-score: ", if ((pib_yoy_n == 0) && !pib_idx_bad && pib_z_ok) "PASS" else "FAIL (ver 20a_pib_yoy_violaciones.csv)", "\n", sep = "")
} else {
  cat("Checks derivadas: OMITIDOS (no hay core_indicadores_derivadas.csv)\n", sep = "")
}
msg("Tablas QA en: ", qa_dir)

# ───────── Resumen Markdown ─────────
md <- c(
  "# QA 20a — Resumen",
  "",
  sprintf("- Cobertura core (2010–2023): **%s**", if (cov_ok) "PASS" else "FAIL"),
  sprintf("- Duplicados core: **%s**", if (dup_n==0) "PASS" else paste0("FAIL (", dup_n, ")")),
  sprintf("- Cotas/rangos: **%s**", if (viol_n==0) "PASS" else "WARN/FAIL (ver 20a_violaciones_cotas.csv)"),
  if (nrow(iden))
    sprintf("- Identidades tasas/share: **%s**", if (iden_n==0) "PASS" else "FAIL (ver 20a_identidades_violaciones.csv)")
  else "- Identidades tasas/share: **OMITIDO** (faltan columnas)",
  if (nrow(der))
    sprintf("- Lags/Δlog PIB/HC/HE: **%s**", if (dlog_n==0) "PASS" else "FAIL (ver 20a_dlog_violaciones.csv)")
  else "- Lags/Δlog: **OMITIDO** (no hay derivadas)",
  if (nrow(der))
    sprintf("- PIB YoY/index/z-score: **%s**", if ((pib_yoy_n==0) && !pib_idx_bad && pib_z_ok) "PASS" else "FAIL (ver 20a_pib_yoy_violaciones.csv)")
  else "- PIB YoY/index/z: **OMITIDO** (no hay derivadas)",
  if (nrow(he_gt_hc))
    sprintf("- Regla HE ≤ HC: **FAIL (%d casos; ver 20a_he_mayor_que_hc.csv)**", nrow(he_gt_hc))
  else "- Regla HE ≤ HC: **PASS**"
)
writeLines(md, file.path(qa_dir, "20a_summary.md"))

# ───────── Resumen JSON + exit code (CI) ─────────
summary_list <- list(
  coverage_pass   = cov_ok,
  duplicates_pass = (dup_n == 0),
  ranges_pass     = (viol_n == 0),
  identities_pass = if (nrow(iden)) (iden_n == 0) else NA,
  dlog_pass       = if (nrow(der)) (dlog_n == 0) else NA,
  pib_block_pass  = if (nrow(der)) ((pib_yoy_n == 0) && !pib_idx_bad && pib_z_ok) else NA,
  he_leq_hc_pass  = (nrow(he_gt_hc) == 0),
  generated_at    = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
)
jsonlite::write_json(summary_list, file.path(qa_dir, "20a_summary.json"), pretty = TRUE, auto_unbox = TRUE)

all_pass <- all(
  isTRUE(summary_list$coverage_pass),
  isTRUE(summary_list$duplicates_pass),
  isTRUE(summary_list$ranges_pass),
  isTRUE(summary_list$he_leq_hc_pass),
  isTRUE(summary_list$identities_pass %||% TRUE),
  isTRUE(summary_list$dlog_pass %||% TRUE),
  isTRUE(summary_list$pib_block_pass %||% TRUE)
)

if (!all_pass) {
  message("[20a] Algún check falló. Revisa output/tables/*.csv y 20a_summary.md")
  quit(save = "no", status = 1)
}

