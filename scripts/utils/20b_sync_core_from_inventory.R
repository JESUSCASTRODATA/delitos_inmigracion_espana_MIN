#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
# 20b_sync_core_from_inventory.R — Normaliza core + derivadas con tus ficheros reales (robusto y memory-safe)
# Autor: Jesús Castro · JESUSCASTRODATA
# Licencia: MIT (código) · CC BY 4.0 (docs/figuras)
# Fecha: 2025-09-28

suppressPackageStartupMessages({
  library(here); library(readr); library(dplyr); library(tidyr); library(janitor)
  library(stringr); library(fs); library(purrr); library(vctrs)
})

YEAR_MIN <- 2010L; YEAR_MAX <- 2023L
msg <- function(...) message("[20b] ", paste0(...))
dir_create(here("output","tables"))

# ---------- Paths ----------
p_core0   <- here("data","processed","core_indicadores.csv")
p_core_p  <- here("data","processed","core_indicadores_con_pib.csv")
p_der0    <- here("data","processed","core_indicadores_derivadas.csv")

p_pct     <- here("data","processed","pct_extranjeros_poblacion.csv")
p_pop_tot <- here("data","processed","poblacion_total_nacional.csv")
p_pop_ext <- here("data","processed","poblacion_extranjera_total.csv")
p_det_shr <- here("data","processed","detenciones_share_extranjeros.csv")

p_hc      <- here("data","processed","hechos_conocidos_total_nacional.csv")
p_he      <- here("data","processed","hechos_esclarecidos_total_nacional.csv")

p_paro    <- here("data","processed","paro_15_29_anual.csv")
p_pib     <- here("data","processed","pib_pc_real_es.csv")
p_arope   <- here("data","processed","arope_total.csv")  # si no, usa arope.csv

# ---------- Utils ----------
safe_read <- function(p) if (file.exists(p)) suppressMessages(read_csv(p, show_col_types = FALSE) |> clean_names()) else NULL
std_year  <- function(df){
  if (is.null(df)) return(NULL)
  if (!"ano" %in% names(df) && "periodo" %in% names(df))
    df <- df %>% mutate(ano = readr::parse_integer(str_extract(periodo, "\\d{4}")))
  df
}
mk_rate <- function(x, pop) ifelse(is.finite(x) & is.finite(pop) & pop>0, 1e5 * x / pop, NA_real_)
mk_dlog <- function(x){ if (is.null(x) || all(is.na(x))) return(rep(NA_real_, length(x))); y <- log(x); c(NA_real_, diff(y)) }
pick_first <- function(df, cand) { nm <- intersect(cand, names(df)); if (!length(nm)) return(NULL); df[[nm[1]]] }

# ======================================================================
# 1) CORE — Base y merges (solo lo necesario; robusto a nombres)
# ======================================================================
core <- safe_read(p_core_p)
if (is.null(core)) core <- safe_read(p_core0)
if (is.null(core)) core <- tibble(ano = YEAR_MIN:YEAR_MAX)

core <- core %>%
  std_year() %>%
  filter(between(ano, YEAR_MIN, YEAR_MAX)) %>%
  arrange(ano)

# PIB per cápita real
pib <- safe_read(p_pib) %>% std_year()
if (!is.null(pib)) {
  cand <- names(pib)[str_detect(names(pib), "(?i)^(pib.*pc.*real|pib_pc_real|pib.*volumen|pib.*encaden|pib.*chain)")]
  col  <- setdiff(cand, c("ano","periodo"))[1]
  if (is.na(col)) col <- setdiff(names(pib), c("ano","periodo"))[1]
  if (!is.na(col)) core <- core %>% left_join(pib %>% transmute(ano, pib_pc_real = .data[[col]]), by="ano")
}

# Población total
pop_tot <- safe_read(p_pop_tot) %>% std_year()
if (!is.null(pop_tot)) {
  nmT <- names(pop_tot)[str_detect(names(pop_tot), "(?i)poblacion_total|^total$")]
  if (length(nmT)) core <- core %>% left_join(pop_tot %>% transmute(ano, poblacion_total = .data[[nmT[1]]]), by="ano")
}

# Hechos totales y tasas
hc <- safe_read(p_hc) %>% std_year()
he <- safe_read(p_he) %>% std_year()
if (!is.null(hc)) { nm <- setdiff(names(hc), c("ano","periodo")); if (length(nm)) core <- core %>% left_join(hc %>% transmute(ano, hc_total = .data[[nm[1]]]), by="ano") }
if (!is.null(he)) { nm <- setdiff(names(he), c("ano","periodo")); if (length(nm)) core <- core %>% left_join(he %>% transmute(ano, he_total = .data[[nm[1]]]), by="ano") }

if (!"tasa_hc_100k" %in% names(core) && all(c("hc_total","poblacion_total") %in% names(core))) core <- core %>% mutate(tasa_hc_100k = mk_rate(hc_total, poblacion_total))
if (!"tasa_he_100k" %in% names(core) && all(c("he_total","poblacion_total") %in% names(core))) core <- core %>% mutate(tasa_he_100k = mk_rate(he_total, poblacion_total))

# % Extranjeros
pct <- safe_read(p_pct) %>% std_year()
if (!is.null(pct)) {
  cand <- names(pct)[str_detect(names(pct), "(?i)^pct_extran")]
  if (length(cand)) core <- core %>% left_join(pct %>% transmute(ano, pct_extranjeros = .data[[cand[1]]]), by="ano")
} else {
  E <- safe_read(p_pop_ext) %>% std_year()
  if (!is.null(E) && !"poblacion_extranjera" %in% names(E)) {
    nmE <- names(E)[str_detect(names(E), "(?i)extranjera")]
    if (length(nmE)) E <- E %>% rename(poblacion_extranjera = !!nmE[1])
  }
  if (!is.null(E) && "poblacion_total" %in% names(core)) {
    core <- core %>% left_join(E %>% select(ano, poblacion_extranjera), by="ano") %>%
      mutate(pct_extranjeros = ifelse(poblacion_total>0, poblacion_extranjera/poblacion_total, NA_real_))
  }
}

# Share detenciones extranjeros
shr <- safe_read(p_det_shr) %>% std_year()
if (!is.null(shr) && !"share_extranjeros" %in% names(core)) {
  cand <- names(shr)[str_detect(names(shr), "(?i)(share|porc).*extran")]
  col  <- setdiff(cand, c("ano","periodo"))[1]
  if (!is.na(col)) {
    v <- shr %>% transmute(ano, share_extranjeros = .data[[col]])
    rng <- range(v$share_extranjeros, na.rm = TRUE)
    if (is.finite(rng[2]) && rng[2] <= 1.01) v <- v %>% mutate(share_extranjeros = 100*share_extranjeros)
    core <- core %>% left_join(v, by="ano")
  }
}

# Paro joven
paro <- safe_read(p_paro) %>% std_year()
if (!is.null(paro)) {
  cand <- names(paro)[str_detect(names(paro), "(?i)(^paro_?joven$|paro_?15_?29|tasa)")]
  col  <- setdiff(cand, c("ano","periodo"))[1]
  if (!is.na(col)) core <- core %>% left_join(paro %>% transmute(ano, paro_joven = .data[[col]]), by="ano")
}

# AROPE
aro <- safe_read(p_arope) %>% std_year()
if (!is.null(aro)) {
  cand <- names(aro)[str_detect(names(aro), "(?i)^(arope$|riesgo.*pobreza|exclusion)")]
  col  <- setdiff(cand, c("ano","periodo"))[1]
  if (!is.na(col)) core <- core %>% left_join(aro %>% transmute(ano, arope = .data[[col]]), by="ano")
}

# ======================================================================
# 2) Parche duplicados .x/.y, normalizaciones y deduplicación por año
# ======================================================================
core <- core %>%
  mutate(
    pib_pc_real      = coalesce(pick_first(core, c("pib_pc_real","pib_pc_real.x","pib_pc_real.y"))),
    poblacion_total  = coalesce(pick_first(core, c("poblacion_total","poblacion_total.x","poblacion_total.y"))),
    hc_total         = coalesce(pick_first(core, c("hc_total","hc_total.x","hc_total.y"))),
    he_total         = coalesce(pick_first(core, c("he_total","he_total.x","he_total.y"))),
    pct_extranjeros  = coalesce(pick_first(core, c("pct_extranjeros","pct_extranjeros.x","pct_extranjeros.y")))
  ) %>%
  select(-any_of(c("pib_pc_real.x","pib_pc_real.y",
                   "poblacion_total.x","poblacion_total.y",
                   "hc_total.x","hc_total.y",
                   "he_total.x","he_total.y",
                   "pct_extranjeros.x","pct_extranjeros.y")))

# Normaliza pct_extranjeros a [0,1] si venía 0–100
if ("pct_extranjeros" %in% names(core)) {
  mx <- suppressWarnings(max(core$pct_extranjeros, na.rm = TRUE))
  if (is.finite(mx) && mx > 1.5) core <- core %>% mutate(pct_extranjeros = pct_extranjeros/100)
}

# QA duplicados por año (antes de distinct)
dup_core <- core %>% count(ano) %>% filter(n > 1)
if (nrow(dup_core)) {
  write_csv(dup_core, here("output","tables","20b_dup_anos_core.csv"))
  msg("WARN: Detectados años duplicados en CORE (ver 20b_dup_anos_core.csv). Se aplicará distinct().")
}

# Deduplicación (evita many-to-many y explosión de memoria)
core <- core %>% arrange(ano) %>% distinct(ano, .keep_all = TRUE)

# Select seguro y orden final
core <- core %>%
  select(any_of(c(
    "ano","pib_pc_real","tasa_hc_100k","tasa_he_100k",
    "share_extranjeros","pct_extranjeros","paro_joven","arope",
    "poblacion_total","hc_total","he_total"
  )), everything()) %>%
  filter(between(ano, YEAR_MIN, YEAR_MAX)) %>%
  arrange(ano)

# Backup + escribir core
if (file.exists(p_core0)) file_copy(p_core0, paste0(p_core0, ".bak_", format(Sys.time(), "%Y%m%d_%H%M%S")))
write_csv(core, p_core0)
msg("Core actualizado → ", p_core0)

# ======================================================================
# 3) DERIVADAS — RECREACIÓN DESDE CERO (rápida, sin leer archivo viejo)
# ======================================================================
if (file.exists(p_der0)) {
  file_copy(p_der0, paste0(p_der0, ".bak_", format(Sys.time(), "%Y%m%d_%H%M%S")))
  file_delete(p_der0)
  msg("Backup derivadas → ", paste0(p_der0, ".bak_*"))
}

der_new <- core %>%
  arrange(ano) %>%
  transmute(
    ano,
    dln_pib_pc      = if ("pib_pc_real"      %in% names(core)) mk_dlog(pib_pc_real)                                else NA_real_,
    d_tasa_hc_100k  = if ("tasa_hc_100k"     %in% names(core)) tasa_hc_100k - dplyr::lag(tasa_hc_100k)             else NA_real_,
    d_tasa_he_100k  = if ("tasa_he_100k"     %in% names(core)) tasa_he_100k - dplyr::lag(tasa_he_100k)             else NA_real_,
    d_pct_extran    = if ("pct_extranjeros"  %in% names(core)) pct_extranjeros - dplyr::lag(pct_extranjeros)       else NA_real_,
    d_share_ext     = if ("share_extranjeros"%in% names(core)) share_extranjeros - dplyr::lag(share_extranjeros)   else NA_real_
  )

write_csv(der_new, p_der0)
msg("✅ Derivadas recreadas (compactas) → ", p_der0)

# ======================================================================
# 4) QA resumido
# ======================================================================
qa_core <- tibble(
  var = c("tasa_hc_100k","tasa_he_100k","share_extranjeros","pct_extranjeros","pib_pc_real","paro_joven","arope"),
  in_core = var %in% names(core),
  n_na = sapply(var, function(v) if (v %in% names(core)) sum(is.na(core[[v]])) else NA_integer_)
)
write_csv(qa_core, here("output","tables","20b_qc_core_vars.csv"))

qa_der <- tibble(
  var = c("dln_pib_pc","d_tasa_hc_100k","d_tasa_he_100k","d_pct_extran","d_share_ext"),
  in_der = var %in% names(der_new),
  n_na = sapply(var, function(v) if (v %in% names(der_new)) sum(is.na(der_new[[v]])) else NA_integer_)
)
write_csv(qa_der, here("output","tables","20b_qc_der_vars.csv"))

msg("QA → output/tables/20b_qc_core_vars.csv y 20b_qc_der_vars.csv")
msg("Listo. Ejecuta ahora 20_preguntas_basicas.R con IO estable y sin indexados lentos.")
