#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
################################################################################
# 18b_check_tasas.R — QC + Preflight para ARDL/VAR del master de tasas
# Proyecto : Delitos e Inmigración en España (2010–2023)
# Autor    : Jesús Castro · JESUSCASTRODATA
# Licencia : MIT (código) · CC BY 4.0 (docs/figuras)
# Fecha    : 2025-10-03
#
# Qué hace:
#   - Carga el master (fallback entre varios inputs).
#   - Enlaza AROPE y Paro 15–29 si existen (con guardas anti-duplicación).
#   - Detecta/normaliza alias y calcula tasas por 100k si faltan.
#   - Si detecta replicación pura por año, colapsa a 1 fila/año de forma segura.
#   - Genera QC (NA counts, summary stats, correlaciones, ADF opcional, preflight ARDL).
#   - Imprime un QC de coherencia SOLO CONSOLA.
#
# Entradas (usa la primera que exista):
#   data/processed/tasas/tasas_transformadas.csv
#   data/processed/tasas/tasas.csv
#   data/processed/core_indicadores_con_controles.csv
#   data/processed/core_indicadores_derivadas.csv
#   data/processed/core_indicadores_con_pib.csv
#
# Enriquecimiento opcional:
#   data/processed/arope.csv            → arope
#   data/processed/paro_15_29_anual.csv → paro_15_29
#
# Salidas:
#   output/logs/19_check_tasas.txt
#   output/tables/19_qc_na_counts.csv
#   output/tables/19_qc_summary_stats.csv
#   output/tables/19_correlations_pearson.csv
#   output/tables/19_correlations_spearman.csv
#   output/tables/19_stationarity_adf.csv      (si hay tseries/urca)
#   output/tables/19_ardl_preflight.csv
#   output/tables/19_det_share_summary.csv
#
# Requisitos:
#   Paquetes: dplyr, readr, janitor, here, fs, tidyr, stringr, rlang, tibble
################################################################################

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(janitor); library(here)
  library(fs); library(tidyr); library(stringr); library(rlang); library(tibble)
})

# ── Paths / init ──────────────────────────────────────────────────────────────
tryCatch(here::i_am("scripts/18b_check_tasas.R"), error = function(e) NULL)

candidates <- c(
  here::here("data","processed","tasas","tasas_transformadas.csv"),
  here::here("data","processed","tasas","tasas.csv"),
  here::here("data","processed","core_indicadores_con_controles.csv"),
  here::here("data","processed","core_indicadores_derivadas.csv"),
  here::here("data","processed","core_indicadores_con_pib.csv")
)
ipath <- candidates[file.exists(candidates)][1]
if (is.na(ipath)) stop("No se encontró ningún input candidato. Revisa 'data/processed/'.")

fs::dir_create(here::here("output","logs"))
fs::dir_create(here::here("output","tables"))
logf <- here::here("output","logs","19_check_tasas.txt")
cat("", file = logf)

log_ <- function(...) { msg <- paste0(...); cat(msg, "\n", file = logf, append = TRUE); message(msg) }
pick_first <- function(pool, candidates) { for (nm in candidates) if (nm %in% pool) return(nm); NA_character_ }
num <- function(x) suppressWarnings(as.numeric(x))

# Guardas anti-duplicación (many-to-many)
warn_dupes <- function(df, key = "ano", label = "df"){
  d <- df |> dplyr::count(.data[[key]], name="n") |> dplyr::filter(n>1)
  if (nrow(d)) log_("⚠ ", label, ": clave ", key, " con duplicados; ej: ", dplyr::first(d[[key]]))
  invisible(nrow(d))
}

# ── Load & clean ─────────────────────────────────────────────────────────────
df_raw <- readr::read_csv(ipath, show_col_types = FALSE) |> clean_names()
log_("✔ Cargado: ", ipath, "  [n=", nrow(df_raw), " cols=", ncol(df_raw), "]")

# ── Enriquecimiento opcional (si existen) ────────────────────────────────────
j_arope <- here::here("data","processed","arope.csv")
j_paro  <- here::here("data","processed","paro_15_29_anual.csv")

safe_join_year_value <- function(df_in, file_label, value_candidates, value_out_name){
  df_in <- df_in |> clean_names()
  log_("ℹ ", file_label, " columnas: ", paste(names(df_in), collapse=", "))
  ycol <- pick_first(names(df_in), c("ano","year","anio","año","periodo"))
  if (is.na(ycol)) {
    log_("⚠ ", file_label, ": no se encontró columna de año (ano/year/anio/año/periodo). Join omitido.")
    return(NULL)
  }
  vcol <- intersect(value_candidates, names(df_in))
  if (!length(vcol)) {
    log_("⚠ ", file_label, ": no se encontró columna de valor (", paste(value_candidates, collapse=" / "), "). Join omitido.")
    return(NULL)
  }
  out <- df_in |>
    transmute(
      ano = suppressWarnings(as.integer(.data[[ycol]])),
      !!value_out_name := suppressWarnings(as.numeric(.data[[vcol[1]]]))
    ) |>
    filter(is.finite(ano))
  log_("✔ Join preparado desde ", file_label, " → columnas: ano + ", value_out_name,
       " (año=", ycol, "; valor=", vcol[1], ")")
  out
}
# --- Limpieza AROPE a TOTAL por año (1 fila/año) -----------------------------
prepare_arope_total <- function(df){
  df <- df |> janitor::clean_names()
  
  # Detecta columnas (robusto a nombres)
  ycol <- dplyr::first(intersect(c("ano","year","anio","año","periodo"), names(df)))
  vcol <- dplyr::first(intersect(c("arope","valor","total","rate","tasa"), names(df)))
  c_sex <- intersect(c("sexo","sex"), names(df))
  c_age <- intersect(c("edad","age","grupo_edad"), names(df))
  c_cmp <- intersect(c(
    "tasa_de_riesgo_de_pobreza_o_exclusion_social_y_sus_componentes",
    "indicador","componente","concepto","serie","descripcion"
  ), names(df))
  
  if (is.na(ycol) || is.na(vcol)) return(NULL)
  
  out <- df
  
  # Filtros estándar: Ambos sexos / Total edades si existen
  if (length(c_sex)) {
    out <- out |> dplyr::filter(is.na(.data[[c_sex[1]]]) |
                                  .data[[c_sex[1]]] %in% c("Total","Ambos sexos","Ambos Sexos","Ambos"))
  }
  if (length(c_age)) {
    out <- out |> dplyr::filter(is.na(.data[[c_age[1]]]) |
                                  .data[[c_age[1]]] %in% c("Total","Todas las edades","Total edades"))
  }
  # Quedarse con el indicador AROPE agregado (no subcomponentes)
  if (length(c_cmp)) {
    out <- out |>
      dplyr::filter(stringr::str_detect(tolower(.data[[c_cmp[1]]]), "arope") &
                      !stringr::str_detect(tolower(.data[[c_cmp[1]]]),
                                           "carencia|privaci|baja\\s+intensidad|riesgo\\s+de\\s+pobreza"))
  }
  
  # Construir tabla año + valor y colapsar (si quedaran múltiples)
  out <- out |>
    dplyr::transmute(
      ano   = suppressWarnings(as.integer(.data[[ycol]])),
      arope = suppressWarnings(as.numeric(.data[[vcol]]))
    ) |>
    dplyr::filter(is.finite(ano), is.finite(arope)) |>
    dplyr::group_by(ano) |>
    dplyr::summarise(arope = mean(arope, na.rm = TRUE), .groups = "drop")
  
  out
}

# Aviso de duplicados en input base
warn_dupes(df_raw, "ano", "df_raw (antes de joins)")

# AROPE (limpieza a TOTAL por año + join 1:1)
if (file.exists(j_arope)) {
  arope_df <- readr::read_csv(j_arope, show_col_types = FALSE)
  arope_j  <- prepare_arope_total(arope_df)
  if (!is.null(arope_j)) {
    warn_dupes(arope_j, "ano", "AROPE (tras limpiar)")
    df_raw <- df_raw |> clean_names() |> dplyr::left_join(arope_j, by = "ano")
    log_("✔ Join opcional AROPE aplicado (TOTAL por año).")
  } else {
    log_("⚠ AROPE no tiene columnas de año/valor esperadas; join omitido.")
  }
}


# Paro 15–29
if (file.exists(j_paro)) {
  paro_df <- readr::read_csv(j_paro, show_col_types = FALSE)
  paro_j  <- safe_join_year_value(paro_df, "Paro 15–29", c("paro_15_29","tasa","valor","rate"), "paro_15_29")
  if (!is.null(paro_j)) {
    warn_dupes(paro_j, "ano", "Paro 15–29")
    df_raw <- df_raw |> clean_names() |> left_join(paro_j |> distinct(ano, .keep_all = TRUE), by = "ano")
    log_("✔ Join opcional Paro 15–29 aplicado.")
  }
}

post_join <- df_raw |> count(ano, name="filas_por_ano") |> arrange(ano)
if (any(post_join$filas_por_ano > 1)) {
  log_("⚠ Tras joins hay multiplicidad por año. Ej: ", post_join$ano[1], " → ", post_join$filas_por_ano[1], " filas")
}

# ── Aliases / derivación de columnas canónicas ───────────────────────────────
df <- df_raw |> clean_names()

# Año
ano_col <- pick_first(names(df), c("ano","year","anio","año"))
if (is.na(ano_col)) stop("Falta columna de año (ano/year/anio/año).")
df <- df |> mutate(ano = as.integer(.data[[ano_col]]))

# Conteos / población / tasas / share
nms <- names(df)
nm_hc   <- pick_first(nms, c("hc_total","hechos_conocidos","hechos_conocidos_total","hc"))
nm_he   <- pick_first(nms, c("he_total","hechos_esclarecidos","he","he_total_nacional"))
nm_dtt  <- pick_first(nms, c("det_tot","detenciones_totales","det_total","detenciones_total"))
nm_dte  <- pick_first(nms, c("det_ext","detenciones_extranjeros","det_extranjeros","det_ext_total"))
nm_pop  <- pick_first(nms, c("poblacion_total","poblacion","poblacion_total_nacional","poblacion_residente"))
nm_thc  <- pick_first(nms, c("tasa_hc_100k","hc_tasa_100k","tasa_hechos_conocidos","tasa_hc","tasa_hc_por_100k"))
nm_the  <- pick_first(nms, c("tasa_he_100k","he_tasa_100k","tasa_hechos_esclarecidos","tasa_he","tasa_he_por_100k"))
nm_tdt  <- pick_first(nms, c("tasa_det_tot_100k","det_tot_tasa_100k","tasa_detenciones_totales","tasa_det_tot","tasa_det_por_100k"))
nm_tde  <- pick_first(nms, c("tasa_det_ext_100k","det_ext_tasa_100k","tasa_detenciones_extranjeros","tasa_det_ext"))
nm_share <- pick_first(nms, c("det_share_ext","share_ext_detenciones","ratio_det_ext","share_ext","porc_det_ext"))

# Normaliza share
if (!is.na(nm_share) && nm_share != "det_share_ext") {
  df <- df |> dplyr::rename(det_share_ext = !!rlang::sym(nm_share))
  log_("ℹ Normalizado alias: '", nm_share, "' → 'det_share_ext'")
}

# Numéricos
to_num <- unique(na.omit(c(nm_hc, nm_he, nm_dtt, nm_dte, nm_pop)))
if (length(to_num)) df <- df |> mutate(across(all_of(to_num), num))

# Calcula tasas si faltan (por 100k)
calc_rate <- function(count, pop) ifelse(is.finite(pop) & pop > 0, 1e5 * count / pop, NA_real_)
if (is.na(nm_thc) && !is.na(nm_pop) && !is.na(nm_hc)) { df <- df |> mutate(tasa_hc_100k = calc_rate(.data[[nm_hc]], .data[[nm_pop]])); nm_thc <- "tasa_hc_100k"; log_("✔ Creada 'tasa_hc_100k'") }
if (is.na(nm_the) && !is.na(nm_pop) && !is.na(nm_he)) { df <- df |> mutate(tasa_he_100k = calc_rate(.data[[nm_he]], .data[[nm_pop]])); nm_the <- "tasa_he_100k"; log_("✔ Creada 'tasa_he_100k'") }
if (is.na(nm_tdt) && !is.na(nm_pop) && !is.na(nm_dtt)) { df <- df |> mutate(tasa_det_tot_100k = calc_rate(.data[[nm_dtt]], .data[[nm_pop]])); nm_tdt <- "tasa_det_tot_100k"; log_("✔ Creada 'tasa_det_tot_100k'") }
if (is.na(nm_tde) && !is.na(nm_pop) && !is.na(nm_dte)) { df <- df |> mutate(tasa_det_ext_100k = calc_rate(.data[[nm_dte]], .data[[nm_pop]])); nm_tde <- "tasa_det_ext_100k"; log_("✔ Creada 'tasa_det_ext_100k'") }

# det_share_ext si falta
if (is.na(nm_share) && all(!is.na(c(nm_dte, nm_dtt)))) {
  df <- df |> mutate(det_share_ext = ifelse(is.finite(.data[[nm_dtt]]) & .data[[nm_dtt]]>0,
                                            .data[[nm_dte]]/.data[[nm_dtt]], NA_real_))
  log_("✔ Creada 'det_share_ext' = det_ext / det_tot")
}

# ── Colapso seguro a 1 fila/año si hay replicación pura ──────────────────────
value_cols <- intersect(c("hc_total","he_total","det_tot","det_ext","poblacion_total",
                          "tasa_hc_100k","tasa_he_100k","tasa_det_tot_100k","tasa_det_ext_100k",
                          "det_share_ext","pct_extranjeros","arope","paro_15_29"), names(df))
rng <- df |>
  dplyr::group_by(ano) |>
  dplyr::summarise(dplyr::across(dplyr::all_of(value_cols),
                                 ~{
                                   v <- suppressWarnings(as.numeric(.x))
                                   if (all(is.na(v))) NA_real_ else diff(range(v, na.rm = TRUE))
                                 },
                                 .names = "rng_{.col}"),
                   .groups = "drop")
replicacion_pura <- all(dplyr::select(rng, dplyr::starts_with("rng_")) == 0, na.rm = TRUE)

if (replicacion_pura) {
  log_("ℹ Detectada replicación pura por año. Se colapsa a 1 fila/año.")
  df <- df |>
    dplyr::group_by(ano) |>
    dplyr::summarise(dplyr::across(dplyr::all_of(value_cols), ~ dplyr::first(.x)), .groups = "drop") |>
    dplyr::mutate(
      tasa_hc_100k      = dplyr::if_else(poblacion_total>0, 1e5*hc_total/poblacion_total, NA_real_),
      tasa_he_100k      = dplyr::if_else(poblacion_total>0, 1e5*he_total/poblacion_total, NA_real_),
      tasa_det_tot_100k = dplyr::if_else(poblacion_total>0, 1e5*det_tot/poblacion_total, NA_real_),
      tasa_det_ext_100k = dplyr::if_else(poblacion_total>0, 1e5*det_ext/poblacion_total, NA_real_),
      det_share_ext     = dplyr::if_else(det_tot>0, det_ext/det_tot, NA_real_)
    )
} else {
  log_("ℹ Variación real dentro de año: no se colapsa automáticamente.")
}

# ── QC: NA counts ────────────────────────────────────────────────────────────
qc_vars <- c(
  "ano", nm_hc, nm_he, nm_dtt, nm_dte, nm_pop,
  nm_thc, nm_the, nm_tdt, nm_tde,
  pick_first(names(df), c("pct_extranjeros","share_extranjeros","pct_extranjeros_poblacion")),
  "det_share_ext"
) |> unique() |> setdiff(NA_character_)

qc_na <- df |>
  summarise(across(all_of(qc_vars), ~sum(is.na(.x))), .groups = "drop") |>
  pivot_longer(everything(), names_to = "variable", values_to = "na_count")
readr::write_csv(qc_na, here::here("output","tables","19_qc_na_counts.csv"))
log_("✔ Escrito: output/tables/19_qc_na_counts.csv")

# ── Summary stats (names_pattern robusto) ────────────────────────────────────
meas_vars <- setdiff(qc_vars, "ano")
if (length(meas_vars) == 0) {
  log_("ℹ No hay variables numéricas para resumen; se omite 19_qc_summary_stats.csv")
} else {
  qc_sum <- df |>
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(meas_vars),
        list(
          n    = ~sum(!is.na(.x)),
          mean = ~mean(.x, na.rm = TRUE),
          sd   = ~sd(.x,   na.rm = TRUE),
          min  = ~min(.x,  na.rm = TRUE),
          p25  = ~quantile(.x, 0.25, na.rm = TRUE),
          p50  = ~quantile(.x, 0.50, na.rm = TRUE),
          p75  = ~quantile(.x, 0.75, na.rm = TRUE),
          max  = ~max(.x,  na.rm = TRUE)
        )
      )
    ) |>
    tidyr::pivot_longer(
      dplyr::everything(),
      names_to = c("variable", ".value"),
      names_pattern = "^(.*)_(n|mean|sd|min|p25|p50|p75|max)$"
    ) |>
    dplyr::arrange(variable)
  
  readr::write_csv(qc_sum, here::here("output","tables","19_qc_summary_stats.csv"))
  log_("✔ Escrito: output/tables/19_qc_summary_stats.csv")
}

# ── Correlaciones (Pearson / Spearman) ---------------------------------------
corr_vars <- setdiff(qc_vars, c("ano"))
safe_cor <- function(m, method) {
  out <- suppressWarnings(stats::cor(m, use="pairwise.complete.obs", method = method))
  as.data.frame(out) |> tibble::rownames_to_column("var1") |>
    tidyr::pivot_longer(-var1, names_to="var2", values_to=paste0("corr_",method))
}
M <- df |> select(all_of(corr_vars)) |> mutate(across(everything(), as.numeric)) |> as.matrix()
pear <- safe_cor(M, "pearson")
spear<- safe_cor(M, "spearman")
pear |> readr::write_csv(here::here("output","tables","19_correlations_pearson.csv"))
spear|> readr::write_csv(here::here("output","tables","19_correlations_spearman.csv"))
log_("✔ Escrito: correlations (pearson/spearman)")

# ── (Opcional) Estacionariedad ADF si hay paquetes ---------------------------
adf_rows <- list()
if (requireNamespace("tseries", quietly = TRUE)) {
  for (v in corr_vars) {
    vec <- suppressWarnings(as.numeric(df[[v]]))
    if (all(is.na(vec))) next
    pval <- tryCatch(tseries::adf.test(vec, k = 1)$p.value, error = function(e) NA_real_)
    adf_rows[[length(adf_rows)+1]] <- tibble::tibble(variable = v, adf_pvalue = pval, engine = "tseries")
  }
} else if (requireNamespace("urca", quietly = TRUE)) {
  for (v in corr_vars) {
    vec <- suppressWarnings(as.numeric(df[[v]]))
    if (all(is.na(vec))) next
    ur <- tryCatch(urca::ur.df(vec, type = "trend", lags = 1), error = function(e) NULL)
    pv <- tryCatch(ur@cval[,"5pct"], error = function(e) NA_real_)
    adf_rows[[length(adf_rows)+1]] <- tibble::tibble(variable = v, adf_5pct_cval = pv, engine = "urca")
  }
}
if (length(adf_rows)) {
  adf_tab <- dplyr::bind_rows(adf_rows)
  readr::write_csv(adf_tab, here::here("output","tables","19_stationarity_adf.csv"))
  log_("✔ Escrito: output/tables/19_stationarity_adf.csv")
} else {
  log_("ℹ Paquetes 'tseries'/'urca' no disponibles; se omite ADF.")
}

# ── Preflight ARDL básico (ligero) -------------------------------------------
preflight <- df |>
  summarise(
    n = n(),
    any_na = any(is.na(c_across(all_of(corr_vars)))),
    var_det_ext_pos = if ("det_ext" %in% names(df)) all(det_ext >= 0, na.rm=TRUE) else NA,
    var_det_tot_pos = if ("det_tot" %in% names(df)) all(det_tot >= 0, na.rm=TRUE) else NA
  )
readr::write_csv(preflight, here::here("output","tables","19_ardl_preflight.csv"))
log_("✔ Escrito: output/tables/19_ardl_preflight.csv")

# ── Resumen share de detenciones de extranjeros ------------------------------
if ("det_share_ext" %in% names(df)) {
  det_share_summary <- df |>
    summarise(n = sum(!is.na(det_share_ext)),
              mean = mean(det_share_ext, na.rm=TRUE),
              sd = sd(det_share_ext, na.rm=TRUE),
              min = min(det_share_ext, na.rm=TRUE),
              p25 = quantile(det_share_ext, 0.25, na.rm=TRUE),
              p50 = quantile(det_share_ext, 0.50, na.rm=TRUE),
              p75 = quantile(det_share_ext, 0.75, na.rm=TRUE),
              max = max(det_share_ext, na.rm=TRUE))
  readr::write_csv(det_share_summary, here::here("output","tables","19_det_share_summary.csv"))
  log_("✔ Escrito: output/tables/19_det_share_summary.csv")
} else {
  log_("ℹ 'det_share_ext' no disponible; se omite su resumen.")
}

# ── QC SOLO CONSOLA (coherencia rápida) --------------------------------------
check_tasas_console <- function(df,
                                year_min = 2010, year_max = 2023,
                                whitelist_he_gt_hc = NULL,
                                tol_rate = 1e-6, show_max_rows = 5) {
  msg <- function(...) cat(paste0(...), "\n")
  stopifnot(is.data.frame(df))
  nms <- names(df)
  pick <- function(pool, cand){ for (nm in cand) if (nm %in% pool) return(nm); NA_character_ }
  c_ano <- pick(nms, c("ano","year","anio","año"))
  c_hc  <- pick(nms, c("hc_total","hc"))
  c_he  <- pick(nms, c("he_total","he"))
  c_dt  <- pick(nms, c("det_tot","detenciones_totales"))
  c_de  <- pick(nms, c("det_ext","detenciones_extranjeros"))
  c_pop <- pick(nms, c("poblacion_total","poblacion"))
  c_thc <- pick(nms, c("tasa_hc_100k","tasa_hc_por_100k","tasa_hechos_conocidos"))
  c_the <- pick(nms, c("tasa_he_100k","tasa_he_por_100k","tasa_hechos_esclarecidos"))
  c_tdt <- pick(nms, c("tasa_det_tot_100k","tasa_det_por_100k","tasa_detenciones_totales"))
  c_tde <- pick(nms, c("tasa_det_ext_100k","tasa_det_extranjeros_100k","tasa_det_ext"))
  c_share <- pick(nms, c("det_share_ext","share_ext"))
  
  must <- c(c_ano, c_hc, c_he, c_dt, c_de)
  if (any(is.na(must))) { faltan <- c("ano","hc_total","he_total","det_tot","det_ext")[is.na(must)]; stop("Faltan columnas críticas: ", paste(faltan, collapse=", ")) }
  
  num <- function(x) suppressWarnings(as.numeric(x))
  yr <- sort(unique(num(df[[c_ano]])))
  msg("── QC (solo consola) 18b — n=", nrow(df), " filas; años: {", paste(yr, collapse=", "), "}")
  
  target <- seq.int(year_min, year_max)
  missing_years <- setdiff(target, yr); extra_years <- setdiff(yr, target)
  if (!length(missing_years) && !length(extra_years)) msg("✔ Cobertura de años ", year_min, "–", year_max, " contigua.")
  else {
    msg("✖ Cobertura de años NO contigua.")
    if (length(missing_years)) msg("  · Faltan: ", paste(missing_years, collapse=", "))
    if (length(extra_years))   msg("  · Sobran: ", paste(extra_years, collapse=", "))
  }
  
  dup <- df |> mutate(.ano = num(.data[[c_ano]])) |> count(.ano, name="n") |> filter(n > 1)
  if (nrow(dup) == 0) msg("✔ Sin duplicados por año.") else { msg("✖ Duplicados por año (muestra):"); print(utils::head(dup, show_max_rows), row.names = FALSE) }
  
  neg_tbl <- df |>
    transmute(ano = num(.data[[c_ano]]),
              hc = num(.data[[c_hc]]), he = num(.data[[c_he]]),
              det_tot = num(.data[[c_dt]]), det_ext = num(.data[[c_de]])) |>
    filter(hc < 0 | he < 0 | det_tot < 0 | det_ext < 0)
  if (nrow(neg_tbl) == 0) msg("✔ Conteos no negativos (hc, he, det_tot, det_ext).")
  else { msg("✖ Valores negativos (muestra):"); print(utils::head(neg_tbl, show_max_rows), row.names = FALSE) }
  
  he_gt_hc <- df |>
    transmute(ano = num(.data[[c_ano]]), hc = num(.data[[c_hc]]), he = num(.data[[c_he]])) |>
    filter(is.finite(hc), is.finite(he), he > hc)
  if (!is.null(whitelist_he_gt_hc) && nrow(he_gt_hc)) {
    key <- c("ano","tipo"); key <- key[key %in% names(whitelist_he_gt_hc)]
    wl <- select(whitelist_he_gt_hc, all_of(key)) |> distinct()
    he_gt_hc <- he_gt_hc |> anti_join(wl, by = key)
  }
  if (nrow(he_gt_hc) == 0) msg("✔ Identidad HE ≤ HC respetada (tras whitelist si aplica).")
  else { msg("✖ HE > HC en ", nrow(he_gt_hc), " filas (muestra):"); print(utils::head(he_gt_hc, show_max_rows), row.names = FALSE) }
  
  if (!is.na(c_share)) {
    share_bad <- df |>
      transmute(ano = num(.data[[c_ano]]),
                det_tot = num(.data[[c_dt]]),
                det_ext = num(.data[[c_de]]),
                share  = num(.data[[c_share]]),
                share_calc = if_else(det_tot > 0, det_ext/det_tot, NA_real_),
                diff = abs(share - share_calc)) |>
      filter(is.finite(share) & (share < -1e-12 | share > 1 + 1e-12) |
               (is.finite(share_calc) & diff > 1e-9))
    if (nrow(share_bad) == 0) msg("✔ det_share_ext dentro de [0,1] y consistente con det_ext/det_tot.")
    else { msg("✖ Inconsistencias en det_share_ext (muestra):"); print(utils::head(share_bad, show_max_rows), row.names = FALSE) }
  } else msg("ℹ No hay columna de share (det_share_ext/share_ext) para verificar.")
  
  check_rate <- function(count_col, pop_col, rate_col, label) {
    if (is.na(count_col) || is.na(pop_col) || is.na(rate_col)) return(invisible(NULL))
    tab <- df |>
      transmute(ano = num(.data[[c_ano]]),
                count = num(.data[[count_col]]),
                pop   = num(.data[[pop_col]]),
                rate  = num(.data[[rate_col]]),
                rate_calc = if_else(pop > 0, 1e5 * count / pop, NA_real_),
                rel_err = abs(rate - rate_calc) / pmax(abs(rate_calc), 1e-9))
    bad <- tab |> filter(is.finite(rate), is.finite(rate_calc), rel_err > tol_rate)
    if (nrow(bad) == 0) msg("✔ ", label, " consistente (|error rel| ≤ ", tol_rate, ").")
    else { msg("✖ ", label, " inconsistente en ", nrow(bad), " filas (muestra):"); print(utils::head(bad[, c("ano","count","pop","rate","rate_calc","rel_err")], show_max_rows), row.names = FALSE) }
  }
  if (!is.na(c_pop)) {
    check_rate(c_hc,  c_pop, c_thc, "tasa_hc_100k")
    check_rate(c_he,  c_pop, c_the, "tasa_he_100k")
    check_rate(c_dt,  c_pop, c_tdt, "tasa_det_tot_100k")
    check_rate(c_de,  c_pop, c_tde, "tasa_det_ext_100k")
  } else msg("ℹ No hay población → se omite verificación de consistencia de tasas.")
  
  # Outliers IQR informativo
  iq_out <- function(x){
    x <- num(x); x <- x[is.finite(x)]
    if (length(x) < 5) return(logical(0))
    q <- stats::quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
    i <- q[2] - q[1]; lo <- q[1] - 1.5*i; hi <- q[2] + 1.5*i
    function(v) v < lo | v > hi
  }
  flagged <- list()
  if (!is.na(c_thc)) flagged$tasa_hc_100k  <- df[[c_thc]]
  if (!is.na(c_the)) flagged$tasa_he_100k  <- df[[c_the]]
  if (!is.na(c_tdt)) flagged$tasa_det_tot_100k <- df[[c_tdt]]
  if (!is.na(c_tde)) flagged$tasa_det_ext_100k <- df[[c_tde]]
  if (!is.na(c_share)) flagged$det_share_ext <- df[[c_share]]
  if (length(flagged)) {
    msg("— Outliers IQR (informativo):")
    for (nm in names(flagged)) {
      f <- iq_out(flagged[[nm]]); if (length(f) == 0) next
      vec <- num(flagged[[nm]]); anos <- num(df[[c_ano]]); out_idx <- which(f(vec))
      if (length(out_idx)) msg("  · ", nm, ": ", length(out_idx), " potencial(es). Años (muestra): ", paste(utils::head(anos[out_idx], show_max_rows), collapse=", "))
      else msg("  · ", nm, ": sin outliers IQR.")
    }
  }
  
  # Correlaciones rápidas
  safe_cor <- function(mat, method) { suppressWarnings(stats::cor(mat, use="pairwise.complete.obs", method=method)) }
  cor_vars <- intersect(c(c_thc, c_the, c_tdt, c_tde, c_share), nms)
  if (length(cor_vars) >= 2) {
    M <- df |> select(all_of(cor_vars)) |> mutate(across(everything(), ~suppressWarnings(as.numeric(.)))) |> as.matrix()
    cp <- safe_cor(M, "pearson"); cs <- safe_cor(M, "spearman")
    msg("— Correlaciones Pearson (redondeadas):"); print(round(cp, 3))
    msg("— Correlaciones Spearman (redondeadas):"); print(round(cs, 3))
  } else msg("ℹ Correlaciones omitidas (variables insuficientes).")
  
  msg("✅ QC de consola finalizado.")
  invisible(TRUE)
}

# Ejecutar QC de consola al final
check_tasas_console(df, year_min = 2010, year_max = 2023)

log_("✅ 18b_check_tasas.R finalizado.")
