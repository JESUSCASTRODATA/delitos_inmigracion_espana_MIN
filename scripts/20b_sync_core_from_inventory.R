#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
################################################################################
# 20b_sync_core_from_inventory.R
# Normaliza core + derivadas con tus ficheros reales (robusto y memory-safe)
#
# Proyecto : Delitos e Inmigración en España (2010–2023)
# Autor    : Jesús Castro · JESUSCASTRODATA
# Licencia : MIT (código) · CC BY 4.0 (docs/figuras)
# Fecha    : 2025-09-28 (rev: fix sufijos .x/.y y _x/_y)
################################################################################

suppressPackageStartupMessages({
  library(here); library(readr); library(dplyr); library(tidyr); library(janitor)
  library(stringr); library(fs); library(purrr); library(vctrs)
  library(rlang); library(tidyselect)
})

YEAR_MIN <- 2010L; YEAR_MAX <- 2023L
msg <- function(...) message("[20b] ", paste0(...))
fs::dir_create(here::here("output","tables"))

# ---------- Paths ----------
p_core0   <- here::here("data","processed","core_indicadores.csv")
p_core_p  <- here::here("data","processed","core_indicadores_con_pib.csv")
p_der0    <- here::here("data","processed","core_indicadores_derivadas.csv")

p_pct     <- here::here("data","processed","pct_extranjeros_poblacion.csv")
p_pop_tot <- here::here("data","processed","poblacion_total_nacional.csv")
p_pop_ext <- here::here("data","processed","poblacion_extranjera_total.csv")
p_det_shr <- here::here("data","processed","detenciones_share_extranjeros.csv")
p_det_tot <- here::here("data","processed","detenciones_totales_total.csv")
p_det_ext <- here::here("data","processed","detenciones_extranjeros_total.csv")

p_hc      <- here::here("data","processed","hechos_conocidos_total_nacional.csv")
p_he      <- here::here("data","processed","hechos_esclarecidos_total_nacional.csv")

p_paro    <- here::here("data","processed","paro_15_29_anual.csv")
p_pib     <- here::here("data","processed","pib_pc_real_es.csv")
p_arope   <- here::here("data","processed","arope_total.csv")  # si no, usa arope.csv

# ---------- Utils ----------
coalesce_first <- function(df, cols) {
  cols <- intersect(cols, names(df))
  if (!length(cols)) return(NULL)
  if (length(cols) == 1) return(df[[cols[1]]])
  reduce(cols[-1], ~ dplyr::coalesce(.x, df[[.y]]), .init = df[[cols[1]]])
}

safe_read <- function(p) if (file.exists(p)) suppressMessages(readr::read_csv(p, show_col_types = FALSE) |> janitor::clean_names()) else NULL

std_year  <- function(df){
  if (is.null(df)) return(NULL)
  if (!"ano" %in% names(df) && "periodo" %in% names(df)) {
    per_chr <- as.character(df$periodo)
    df <- df %>% mutate(ano = readr::parse_integer(stringr::str_extract(per_chr, "\\d{4}")))
  }
  df
}

mk_rate <- function(x, pop) ifelse(is.finite(x) & is.finite(pop) & pop>0, 1e5 * x / pop, NA_real_)
mk_dlog <- function(x){
  if (is.null(x) || all(is.na(x))) return(rep(NA_real_, length(x)))
  y <- suppressWarnings(log(x))
  c(NA_real_, diff(y))
}

# ======================================================================
# 1) CORE — Base (solo años) y merges (robusto a nombres)
# ======================================================================
core <- tibble(ano = YEAR_MIN:YEAR_MAX)

# PIB pc real
pib <- safe_read(p_pib) %>% std_year()
if (!is.null(pib)) {
  cand <- names(pib)[str_detect(names(pib), "(?i)^(pib.*pc.*real|pib_pc_real|pib.*volumen|pib.*encaden|pib.*chain)")]
  col  <- setdiff(cand, c("ano","periodo"))[1] %||% setdiff(names(pib), c("ano","periodo"))[1]
  if (!is.na(col)) {
    core <- core %>% left_join(pib %>% transmute(ano, pib_pc_real = .data[[col]]), by = "ano")
    msg("PIB pc: merge desde pib_pc_real_es.csv ← columna '", col, "'.")
  }
}

# Población total
pop_tot <- safe_read(p_pop_tot) %>% std_year()
if (!is.null(pop_tot)) {
  nmT <- names(pop_tot)[str_detect(names(pop_tot), "(?i)poblacion_total|^total$")][1]
  if (!is.na(nmT)) {
    core <- core %>% left_join(pop_tot %>% transmute(ano, poblacion_total = .data[[nmT]]), by="ano")
    msg("Población total: merge desde poblacion_total_nacional.csv ← '", nmT, "'.")
  }
}

# HC y HE (niveles) + tasas
hc <- safe_read(p_hc) %>% std_year()
he <- safe_read(p_he) %>% std_year()
if (!is.null(hc)) {
  nm <- setdiff(names(hc), c("ano","periodo"))[1]
  if (!is.na(nm)) core <- core %>% left_join(hc %>% transmute(ano, hc_total = .data[[nm]]), by="ano") %>%
      { msg("HC: merge desde hechos_conocidos_total_nacional.csv ← '", nm, "'."); . }
}
if (!is.null(he)) {
  nm <- setdiff(names(he), c("ano","periodo"))[1]
  if (!is.na(nm)) core <- core %>% left_join(he %>% transmute(ano, he_total = .data[[nm]]), by="ano") %>%
      { msg("HE: merge desde hechos_esclarecidos_total_nacional.csv ← '", nm, "'."); . }
}
if (!"tasa_hc_100k" %in% names(core) && all(c("hc_total","poblacion_total") %in% names(core)))
  core <- core %>% mutate(tasa_hc_100k = mk_rate(hc_total, poblacion_total))
if (!"tasa_he_100k" %in% names(core) && all(c("he_total","poblacion_total") %in% names(core)))
  core <- core %>% mutate(tasa_he_100k = mk_rate(he_total, poblacion_total))
msg("  · tasa_hc_100k: ", ifelse("tasa_hc_100k" %in% names(core), "OK", "NO"))
msg("  · tasa_he_100k: ", ifelse("tasa_he_100k" %in% names(core), "OK", "NO"))

# Detenciones totales y de extranjeros (si están)
det_tot <- safe_read(p_det_tot) %>% std_year()
det_ext <- safe_read(p_det_ext) %>% std_year()
if (!is.null(det_tot)) {
  nm <- setdiff(names(det_tot), c("ano","periodo"))[1]
  if (!is.na(nm)) core <- core %>% left_join(det_tot %>% transmute(ano, det_tot = .data[[nm]]), by="ano")
}
if (!is.null(det_ext)) {
  nm <- setdiff(names(det_ext), c("ano","periodo"))[1]
  if (!is.na(nm)) core <- core %>% left_join(det_ext %>% transmute(ano, det_ext = .data[[nm]]), by="ano")
}

# % Extranjeros
pct <- safe_read(p_pct) %>% std_year()
if (!is.null(pct)) {
  cand <- names(pct)[str_detect(names(pct), "(?i)^pct_extran")]
  if (length(cand)) {
    core <- core %>% left_join(pct %>% transmute(ano, pct_extranjeros = .data[[cand[1]]]), by="ano")
    msg("% extranjeros: merge desde pct_extranjeros_poblacion.csv ← '", cand[1], "'.")
  }
} else {
  E <- safe_read(p_pop_ext) %>% std_year()
  if (!is.null(E)) {
    nmE <- names(E)[str_detect(names(E), "(?i)poblacion_extranjera|extranjera")][1]
    if (!is.na(nmE) && "poblacion_total" %in% names(core)) {
      core <- core %>% left_join(E %>% transmute(ano, poblacion_extranjera = .data[[nmE]]), by="ano") %>%
        mutate(pct_extranjeros = ifelse(poblacion_total>0, poblacion_extranjera/poblacion_total, NA_real_))
      msg("% extranjeros: reconstruido desde poblacion_extranjera/total ← '", nmE, "'.")
    }
  }
}

# Share detenciones extranjeros
shr <- safe_read(p_det_shr) %>% std_year()
if (!is.null(shr)) {
  cand <- names(shr)[str_detect(names(shr), "(?i)(share|porc).*extran")][1]
  if (!is.na(cand)) {
    v <- shr %>% transmute(ano, share_extranjeros = .data[[cand]])
    rng <- range(v$share_extranjeros, na.rm = TRUE)
    if (is.finite(rng[2]) && rng[2] <= 1.01) v <- v %>% mutate(share_extranjeros = 100*share_extranjeros)
    core <- core %>% left_join(v, by="ano")
    msg("Share detenciones: merge desde detenciones_share_extranjeros.csv ← '", cand, "'.")
  }
}
# si no hay share pero sí det_tot/det_ext, construirlo:
if (!"share_extranjeros" %in% names(core) && all(c("det_tot","det_ext") %in% names(core))) {
  core <- core %>% mutate(share_extranjeros = ifelse(is.finite(det_tot) & det_tot>0, 100*det_ext/det_tot, NA_real_))
  msg("Share detenciones: construido como 100*det_ext/det_tot.")
}

# Paro joven (15–29)
paro <- safe_read(p_paro) %>% std_year()
if (!is.null(paro)) {
  if ("paro_15_29" %in% names(paro)) {
    core <- core %>% left_join(paro %>% transmute(ano = as.integer(ano), paro_joven = as.numeric(paro_15_29)), by="ano")
    msg("Paro joven: tomado de paro_15_29_anual.csv ← 'paro_15_29'.")
  } else {
    # heurística alternativa
    cand <- names(paro)[str_detect(names(paro), "(?i)(^paro_?joven$|paro_?15_?29|tasa)")]
    if (length(cand)) {
      core <- core %>% left_join(paro %>% transmute(ano = as.integer(ano), paro_joven = as.numeric(.data[[cand[1]]])), by="ano")
      msg("Paro joven: tomado de paro_15_29_anual.csv ← '", cand[1], "'.")
    }
  }
}

# AROPE (prefer arope_total.csv)
aro <- safe_read(p_arope) %>% std_year()
if (is.null(aro)) {
  p_arope_crudo <- here::here("data","processed","arope.csv")
  aro_raw <- safe_read(p_arope_crudo)
  if (!is.null(aro_raw)) {
    aro <- aro_raw %>%
      clean_names() %>%
      { if ("sexo" %in% names(.)) filter(., is.na(sexo) | sexo %in% c("Ambos sexos","Ambos","total")) else . } %>%
      { if ("edad" %in% names(.)) filter(., is.na(edad) | edad %in% c("Total","Todas las edades","total")) else . } %>%
      transmute(ano = suppressWarnings(as.integer(.data[["periodo"]])),
                arope = suppressWarnings(as.numeric(.data[["total"]]))) %>%
      filter(is.finite(ano), is.finite(arope)) %>%
      mutate(arope = ifelse(arope > 100 & arope <= 1000, arope/10, arope)) %>%
      group_by(ano) %>% summarise(arope = mean(arope, na.rm = TRUE), .groups = "drop") %>%
      arrange(ano)
    msg("AROPE: tomado de arope.csv (crudo) periodo→ano y total→arope (auto /10 si venía ×10).")
  }
} else {
  if (all(c("ano","arope") %in% names(aro))) {
    aro <- aro %>% transmute(ano = as.integer(ano), arope = as.numeric(arope)) %>% arrange(ano)
    msg("AROPE: tomado de arope_total.csv ← 'arope'.")
  } else {
    msg("WARN: arope_total.csv existe pero no trae (ano, arope).")
    aro <- NULL
  }
}
if (!is.null(aro)) core <- core %>% left_join(aro, by="ano")

# ======================================================================
# 2) Coalesce de duplicados y purga de sufijos (.x/.y y _x/_y) + normalizaciones
# ======================================================================
# Coalesce por patrón que cubre .x/.y y _x/_y (también repetidos)
vars_base <- c(
  "pib_pc_real","poblacion_total","hc_total","he_total",
  "tasa_hc_100k","tasa_he_100k","share_extranjeros",
  "pct_extranjeros","paro_joven","arope","det_tot","det_ext"
)

for (nm in vars_base) {
  patt <- paste0("^", nm, "($|([._][xy])([._][xy])*)")
  cols <- grep(patt, names(core), value = TRUE)
  if (length(cols) > 1) {
    core <- core %>% dplyr::mutate(!!nm := dplyr::coalesce(!!!rlang::syms(cols)))
  }
}
# Purga de columnas con sufijos residuales .x/.y y _x/_y (encadenados)
core <- core %>% dplyr::select(-tidyselect::matches("[._](x|y)([._](x|y))*$"))

# % extranjeros en [0,1] si venía en 0–100
if ("pct_extranjeros" %in% names(core)) {
  mx <- suppressWarnings(max(core$pct_extranjeros, na.rm = TRUE))
  if (is.finite(mx) && mx > 1.5) core <- core %>% mutate(pct_extranjeros = pct_extranjeros / 100)
}

# Deduplicación por año
if ("ano" %in% names(core)) {
  core <- core %>% arrange(ano) %>% distinct(ano, .keep_all = TRUE) %>%
    filter(dplyr::between(ano, YEAR_MIN, YEAR_MAX)) %>% arrange(ano)
}

# Orden final
core <- core %>% dplyr::select(any_of(c(
  "ano","pib_pc_real","tasa_hc_100k","tasa_he_100k",
  "share_extranjeros","pct_extranjeros","paro_joven","arope",
  "poblacion_total","hc_total","he_total","det_tot","det_ext"
)), tidyselect::everything())

# Backup + escribir core
if (file.exists(p_core0)) fs::file_copy(p_core0, paste0(p_core0, ".bak_", format(Sys.time(), "%Y%m%d_%H%M%S")))
readr::write_csv(core, p_core0)
msg("Core actualizado → ", p_core0)

# ======================================================================
# 3) DERIVADAS — recreación compacta
# ======================================================================
if (file.exists(p_der0)) {
  fs::file_copy(p_der0, paste0(p_der0, ".bak_", format(Sys.time(), "%Y%m%d_%H%M%S")))
  fs::file_delete(p_der0)
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

readr::write_csv(der_new, p_der0)
msg("✅ Derivadas recreadas (compactas) → ", p_der0)

# ======================================================================
# 4) QA resumido + mini informe consola
# ======================================================================
qa_core <- tibble(
  var = c("tasa_hc_100k","tasa_he_100k","share_extranjeros","pct_extranjeros","pib_pc_real","paro_joven","arope","poblacion_total","hc_total","he_total"),
  in_core = var %in% names(core),
  n_na = sapply(var, function(v) if (v %in% names(core)) sum(is.na(core[[v]])) else NA_integer_)
)
readr::write_csv(qa_core, here::here("output","tables","20b_qc_core_vars.csv"))

qa_der <- tibble(
  var = c("dln_pib_pc","d_tasa_hc_100k","d_tasa_he_100k","d_pct_extran","d_share_ext"),
  in_der = var %in% names(der_new),
  n_na = sapply(var, function(v) if (v %in% names(der_new)) sum(is.na(der_new[[v]])) else NA_integer_)
)
readr::write_csv(qa_der, here::here("output","tables","20b_qc_der_vars.csv"))

# Mini resumen
ok_cols <- c("ano","pib_pc_real","tasa_hc_100k","tasa_he_100k",
             "share_extranjeros","pct_extranjeros","paro_joven","arope",
             "poblacion_total","hc_total","he_total")
miss_cols <- setdiff(ok_cols, names(core))

dup_rows <- core %>% count(ano) %>% filter(n>1) %>% nrow()
rng_ok <- (
  (!"pct_extranjeros" %in% names(core) || (max(core$pct_extranjeros, na.rm=TRUE) <= 1.5)) &&
    (!"share_extranjeros" %in% names(core) || (max(core$share_extranjeros, na.rm=TRUE) <= 100.5))
)
suf_cols <- grep("[._](x|y)([._](x|y))*$", names(core), value = TRUE)

cat("\n=== 20b · Sync resumen ===\n")
cat("Columnas clave presentes: ", if (length(miss_cols)==0) "PASS" else paste0("WARN (faltan: ", paste(miss_cols, collapse=", "), ")"), "\n", sep="")
cat("Duplicados por año tras distinct(): ", if (dup_rows==0) "PASS" else paste0("WARN (", dup_rows, ")"), "\n", sep="")
cat("Rangos pct/share plausibles: ", if (rng_ok) "PASS" else "WARN", "\n", sep="")
cat("Columnas con sufijos residuales: ", if (length(suf_cols)==0) "NINGUNA" else paste(suf_cols, collapse=", "), "\n", sep="")
cat("=================================\n\n")

msg("QA → output/tables/20b_qc_core_vars.csv y 20b_qc_der_vars.csv")
msg("Listo. Ejecuta ahora 20_preguntas_basicas.R con IO estable.")


