#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
# -----------------------------------------------------------------------------
# Proyecto   : Delitos e Inmigración en España (2010–2023)
# Script     : 32_compare_violentos_core.R
# Versión    : v1.8 (2025-10-07)
# Autor      : Jesús Castro · JESUSCASTRODATA
# Licencia   : MIT (código) · CC BY 4.0 (docs/figuras)
# Descripción:
#   Compara 5 delitos violentos y su relación con % de extranjeros.
#   Mapeo FIJO (muchos→uno):
#     - "Agresiones sexuales (total)" = 3.1.-agresion sexual + 3.2.-agresion sexual con penetracion
#     - "Homicidios dolosos y asesinatos consumados" = 1.1.1.-homicidios dolosos/asesinatos consumados
#     - "Lesiones graves" = 1.2.-lesiones
#     - "Malos tratos en el ámbito familiar" = 1.3.-malos tratos ambito familiar
#     - "Robos con violencia e intimidación" = 5.3.-robos con violencia o intimidacion
#
# Ejecuta ambos escenarios en un run:
#   • CON COVID      → sin sufijo
#   • SIN COVID      → excluye años EXCLUDE_YEARS (por defecto 2020–2021) → sufijo "_nocovid"
#
# Entradas (vía entorno o paths por defecto):
#   CORE_CSV     = data/processed/core_indicadores_con_pib.csv
#   HC_TIPOS_CSV = data/processed/hechos_conocidos_total_nacional_por_tipo.csv
#   EXCLUDE_YEARS = "2020,2021"   # años a excluir en la versión no-COVID
#
# Salidas (por escenario; sufijo "" o "_nocovid"):
#   output/tables/32_violentos_fijos{suffix}.csv
#   output/tables/32_correlaciones{suffix}.csv
#   output/tables/32_correlaciones_delta{suffix}.csv
#   output/figures/32_series_pct{suffix}.png
#   output/figures/32_dispersion_pct{suffix}.png
#   output/figures/32_dispersion_delta{suffix}.png
#   (Además siempre: output/tables/32_catalogo_tipos.csv)
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(here); library(readr); library(dplyr); library(tidyr)
  library(janitor); library(stringr); library(ggplot2)
})

msg <- function(...) message("[32] ", paste0(...))

# ---- Tema “pearl” (fallback si no existe tu 00_theme_figuras.R) --------------
.have_theme <- FALSE
for (p in c(here::here("scripts","00_theme_figuras.R"), here::here("R","00_theme_figuras.R"))) {
  if (file.exists(p)) { source(p); .have_theme <- TRUE; break }
}
if (!.have_theme) {
  theme_pearl_lightgrid <- function(base_size=12, base_family=NULL){
    theme_minimal(base_size=base_size, base_family=base_family) +
      theme(plot.background=element_rect(fill="#FAF9F6", colour=NA),
            panel.background=element_rect(fill="#FAF9F6", colour=NA),
            legend.background=element_rect(fill="#FAF9F6", colour=NA),
            legend.key=element_rect(fill="#FAF9F6", colour=NA))
  }
}
PEARL_BG <- "#FAF9F6"
use_ragg <- requireNamespace("ragg", quietly = TRUE)
save_png <- function(file, plot, w=1200, h=800, res=144, bg=PEARL_BG){
  if (use_ragg) ragg::agg_png(file, w, h, res=res, background=bg) else png(file, w, h, res=res, bg=bg)
  on.exit(try(dev.off(), silent=TRUE), add=TRUE); print(plot)
}

# ---- Rutas E/S ----------------------------------------------------------------
OUTDIR <- Sys.getenv("OUT_DIR", unset = here::here("output"))
TABDIR <- file.path(OUTDIR, "tables")
FIGDIR <- file.path(OUTDIR, "figures")
dir.create(TABDIR, recursive = TRUE, showWarnings = FALSE)
dir.create(FIGDIR, recursive = TRUE, showWarnings = FALSE)

p_core <- Sys.getenv("CORE_CSV", unset = here::here("data","processed","core_indicadores_con_pib.csv"))
p_hc   <- Sys.getenv("HC_TIPOS_CSV", unset = here::here("data","processed","hechos_conocidos_total_nacional_por_tipo.csv"))
if (!file.exists(p_core)) stop("No existe CORE: ", p_core)
if (!file.exists(p_hc))   stop("No existe HC por tipo: ", p_hc)

msg("CORE  : ", p_core); msg("HC    : ", p_hc); msg("OUT   : ", OUTDIR)

# ---- Lectura bases ------------------------------------------------------------
core <- readr::read_csv(p_core, show_col_types=FALSE) |> clean_names()
hc   <- readr::read_csv(p_hc,   show_col_types=FALSE) |> clean_names()

# Catálogo (auditoría)
cat_out <- file.path(TABDIR, "32_catalogo_tipos.csv")
hc |> distinct(tipo) |> arrange(tipo) |> readr::write_csv(cat_out)
msg("Catálogo de tipos → ", cat_out)

# CORE columnas clave
col_pct <- intersect(c("pct_extranjeros","pct_extran","porc_extranjeros","porcentaje_extranjeros"), names(core))[1]
col_pop <- intersect(c("poblacion_total","poblacion","pob_total","pop_total"), names(core))[1]
if (is.na(col_pct) || is.na(col_pop) || !"ano" %in% names(core))
  stop("[32] Faltan columnas en CORE (ano/pct_extranjeros/poblacion_total).")

core0 <- core |>
  transmute(ano = as.integer(ano),
            pct_extran_raw = .data[[col_pct]],
            poblacion_total = .data[[col_pop]]) |>
  mutate(pct_extran = if (mean(pct_extran_raw, na.rm=TRUE) > 1) pct_extran_raw/100 else pct_extran_raw,
         num_extranjeros = poblacion_total * pct_extran)

# Mapeo FIJO muchos→uno para los 5 delitos
map_tbl <- tibble::tibble(
  delito = c(
    "Agresiones sexuales (total)",
    "Agresiones sexuales (total)",
    "Homicidios dolosos y asesinatos consumados",
    "Lesiones graves",
    "Malos tratos en el ámbito familiar",
    "Robos con violencia e intimidación"
  ),
  tipo = c(
    "3.1.-agresion sexual",
    "3.2.-agresion sexual con penetracion",
    "1.1.1.-homicidios dolosos/asesinatos consumados",
    "1.2.-lesiones",
    "1.3.-malos tratos ambito familiar",
    "5.3.-robos con violencia o intimidacion"
  )
)
faltan_tipos <- setdiff(map_tbl$tipo, unique(hc$tipo))
if (length(faltan_tipos)) stop("[32] En el mapeo faltan en HC: ", paste(faltan_tipos, collapse=" | "),
                               ". Revisa '", cat_out, "'.")
msg("Mapeo fijo aplicado: ",
    paste0(map_tbl$delito, " ← ", map_tbl$tipo, collapse=" | "))

col_total <- intersect(c("total","n","conteo","numero","valor","hc_total"), names(hc))[1]
if (is.na(col_total)) stop("[32] No encuentro columna de conteo total en HC.")

# --------------------------- función de ejecución -----------------------------
run_scenario <- function(exclude_years = integer(0), suffix = "", covid_lbl = "") {
  msg("→ Escenario: ", if (length(exclude_years)) paste0("SIN COVID (excluye: ", paste(exclude_years, collapse=", "), ")") else "CON COVID")
  
  hc5 <- hc |>
    mutate(ano = as.integer(ano),
           total = as.numeric(.data[[col_total]])) |>
    semi_join(map_tbl, by = c("tipo" = "tipo")) |>
    left_join(map_tbl, by = c("tipo" = "tipo")) |>
    count(ano, delito, wt = total, name = "total")
  
  core_sel <- core0
  if (length(exclude_years)) {
    hc5      <- hc5      |> filter(!ano %in% exclude_years)
    core_sel <- core_sel |> filter(!ano %in% exclude_years)
  }
  if (nrow(hc5) == 0) stop("[32] No hay filas tras mapeo/filtro en este escenario.")
  
  df <- hc5 |>
    left_join(core_sel, by="ano") |>
    mutate(tasa_100k = if_else(is.finite(poblacion_total) & poblacion_total>0, 1e5*total/poblacion_total, NA_real_),
           num_extranjeros_miles = num_extranjeros/1000) |>
    arrange(delito, ano)
  
  # tablas
  out_csv   <- file.path(TABDIR, paste0("32_violentos_fijos", suffix, ".csv"))
  write_csv(df, out_csv); msg("✔ Tabla → ", out_csv)
  
  corr_tbl <- df |>
    group_by(delito) |>
    summarise(
      n = sum(!is.na(tasa_100k) & !is.na(pct_extran)),
      pearson_r  = suppressWarnings(cor(tasa_100k, pct_extran, use="complete.obs", method="pearson")),
      spearman_r = suppressWarnings(cor(tasa_100k, pct_extran, use="complete.obs", method="spearman"))
    ) |>
    ungroup()
  out_corr  <- file.path(TABDIR, paste0("32_correlaciones", suffix, ".csv"))
  write_csv(corr_tbl, out_corr); msg("✔ Correlaciones (niveles) → ", out_corr)
  
  # series
  scaler <- df |>
    group_by(delito) |>
    summarise(max_tasa = max(tasa_100k, na.rm=TRUE),
              max_pct  = max(pct_extran, na.rm=TRUE), .groups="drop") |>
    mutate(f = if_else(is.finite(max_tasa) & is.finite(max_pct) & max_pct>0, max_tasa/max_pct, 1))
  df_sc <- df |> left_join(scaler, by="delito") |> mutate(pct_scaled = pct_extran * f)
  
  p1 <- ggplot(df_sc, aes(ano)) +
    geom_line(aes(y = tasa_100k), linewidth=1) +
    geom_line(aes(y = pct_scaled), linetype="dashed", linewidth=1) +
    facet_wrap(~ delito, scales="free_y", ncol=3) +
    labs(title = paste0("Delitos violentos: tasa vs % extranjeros (reescalado)", covid_lbl),
         subtitle = "Continua: tasa por 100k · Discontinua: % extranjeros reescalado por panel",
         x = NULL, y = "Tasa por 100k",
         caption = "Fuente: HC por tipo (mapeo fijo) + Core (población y % extranjeros)") +
    theme_pearl_lightgrid() +
    scale_y_continuous(sec.axis = sec_axis(~., name = "% extranjeros (reescalado)")) +
    theme(panel.grid.minor = element_blank())
  f1 <- file.path(FIGDIR, paste0("32_series_pct", suffix, ".png"))
  save_png(f1, p1); msg("✔ Figura → ", f1)
  
  # dispersión niveles
  p2 <- ggplot(df, aes(pct_extran, tasa_100k)) +
    geom_point(alpha=.85, na.rm=TRUE) +
    geom_smooth(method="lm", se=FALSE, na.rm=TRUE) +
    facet_wrap(~ delito, scales="free", ncol=3) +
    labs(title = paste0("Tasa (100k) vs % extranjeros — 5 delitos (niveles)", covid_lbl),
         x = "% extranjeros (share 0–1)", y = "Tasa por 100k",
         caption = "Recta: ajuste OLS por delito") +
    theme_pearl_lightgrid()
  f2 <- file.path(FIGDIR, paste0("32_dispersion_pct", suffix, ".png"))
  save_png(f2, p2); msg("✔ Figura → ", f2)
  
  # Δ por tramos contiguos
  df_d <- df |>
    arrange(delito, ano) |>
    group_by(delito) |>
    mutate(gap = (ano - dplyr::lag(ano)) > 1,
           tramo = cumsum(dplyr::coalesce(gap, FALSE))) |>
    group_by(delito, tramo) |>
    mutate(d_tasa_100k  = tasa_100k  - dplyr::lag(tasa_100k),
           d_pct_extran = pct_extran - dplyr::lag(pct_extran)) |>
    ungroup() |>
    select(-gap, -tramo)
  
  corr_d <- df_d |>
    group_by(delito) |>
    summarise(
      n = sum(!is.na(d_tasa_100k) & !is.na(d_pct_extran)),
      pearson_r  = suppressWarnings(cor(d_tasa_100k, d_pct_extran, use="complete.obs", method="pearson")),
      spearman_r = suppressWarnings(cor(d_tasa_100k, d_pct_extran, use="complete.obs", method="spearman"))
    ) |>
    ungroup()
  out_corr_d <- file.path(TABDIR, paste0("32_correlaciones_delta", suffix, ".csv"))
  write_csv(corr_d, out_corr_d); msg("✔ Correlaciones (Δ) → ", out_corr_d)
  
  df_d_plot <- df_d |> filter(!is.na(d_tasa_100k), !is.na(d_pct_extran))
  p3 <- ggplot(df_d_plot, aes(d_pct_extran, d_tasa_100k)) +
    geom_hline(yintercept = 0, linewidth = 0.3, alpha = .6) +
    geom_vline(xintercept = 0, linewidth = 0.3, alpha = .6) +
    geom_point(alpha=.85) +
    geom_smooth(method="lm", se=FALSE) +
    facet_wrap(~ delito, scales="free", ncol=3) +
    labs(title = paste0("Δ Tasa (100k) vs Δ % extranjeros — 5 delitos", covid_lbl),
         x = "Δ % extranjeros (share)", y = "Δ Tasa por 100k",
         caption = "OLS sobre variaciones año-a-año (tramos contiguos)") +
    theme_pearl_lightgrid()
  f3 <- file.path(FIGDIR, paste0("32_dispersion_delta", suffix, ".png"))
  save_png(f3, p3); msg("✔ Figura Δ → ", f3)
  
  msg("— MINI CHECK ", if (length(exclude_years)) "(sin COVID)" else "(con COVID)", " —")
  msg("Correlaciones (niveles):"); print(corr_tbl, n = nrow(corr_tbl))
  msg("Correlaciones (Δ)     :"); print(corr_d,   n = nrow(corr_d))
}

# ------------------------------ Ejecuta ambos ---------------------------------
# CON COVID
run_scenario(exclude_years = integer(0), suffix = "", covid_lbl = "")

# SIN COVID (por defecto 2020–2021; configurable)
EXCLUDE_YEARS <- as.integer(strsplit(Sys.getenv("EXCLUDE_YEARS", "2020,2021"), ",")[[1]])
run_scenario(exclude_years = EXCLUDE_YEARS,
             suffix = "_nocovid",
             covid_lbl = paste0(" · excluyendo ", paste(EXCLUDE_YEARS, collapse="–")))

invisible(TRUE)

