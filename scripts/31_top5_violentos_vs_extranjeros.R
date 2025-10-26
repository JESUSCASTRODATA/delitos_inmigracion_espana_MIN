#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
# -----------------------------------------------------------------------------
# Proyecto   : Delitos e Inmigración en España (2010–2023)
# Script     : 31_top5_violentos_vs_extranjeros.R
# Versión    : v2.3 (2025-10-06)
# Autor      : Jesús Castro · JESUSCASTRODATA
# Licencia   : MIT (código) · CC BY 4.0 (docs/figuras)
# Descripción: HC/HE “violentos” → tasas/100k con población del core, Top-5 por
#              media 3y y comparación con % extranjeros (Δ-corr + Granger p=1).
# Dependencias: readr, dplyr, tidyr, janitor, stringr, vars
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(tidyr); library(janitor)
  library(stringr); library(vars)
})

say <- function(...) cat("[31] ", paste0(...), "\n")

# --- Rutas (ajustadas a tu proyecto) ---
ROOT   <- "D:/delitos_inmigracion_espana_MIN"
p_core <- file.path(ROOT, "data/processed/core_indicadores_con_pib.csv")
p_hc   <- file.path(ROOT, "data/processed/hechos_conocidos_total_nacional_por_tipo.csv")
p_he   <- file.path(ROOT, "data/processed/hechos_esclarecidos_total_nacional_por_tipo.csv")
OUT_T  <- file.path(ROOT, "output", "tables")
dir.create(OUT_T, recursive = TRUE, showWarnings = FALSE)

stopifnot(file.exists(p_core), file.exists(p_hc), file.exists(p_he))

# --- Utils ---
mk_delta <- function(x) c(NA, diff(x))
last_k_years <- function(df, k=3L){
  yrs <- sort(unique(df$ano)); if (length(yrs) < k) k <- length(yrs)
  dplyr::filter(df, ano %in% tail(yrs, k))
}
granger_quick <- function(y, x, p = 1L) {
  z <- tibble::tibble(y = y, x = x) |> tidyr::drop_na()
  if (nrow(z) < (p + 3)) return(c(NA_real_, NA_real_))
  fit <- try(vars::VAR(as.matrix(z), p = p, type = "const"), silent = TRUE)
  if (inherits(fit, "try-error")) return(c(NA_real_, NA_real_))
  p_x_to_y <- try(vars::causality(fit, cause = "x")$Granger$p.value, silent = TRUE)
  p_y_to_x <- try(vars::causality(fit, cause = "y")$Granger$p.value, silent = TRUE)
  c(suppressWarnings(as.numeric(p_x_to_y)), suppressWarnings(as.numeric(p_y_to_x)))
}

# --- CORE ---
core <- suppressMessages(readr::read_csv(p_core, show_col_types = FALSE)) |>
  janitor::clean_names() |>
  transmute(ano = as.integer(ano),
            poblacion_total = as.numeric(poblacion_total),
            pct_extranjeros = as.numeric(pct_extranjeros))

# --- HC/HE (totales -> tasa/100k) ---
read_block <- function(p, label){
  suppressMessages(readr::read_csv(p, show_col_types = FALSE)) |>
    janitor::clean_names() |>
    transmute(
      ano  = as.integer(ano),
      tipo = as.character(tipo),
      total = as.numeric(total),
      bloque = label
    ) |>
    filter(!is.na(ano), !is.na(tipo)) |>
    left_join(core, by = "ano") |>
    mutate(tasa_100k = 1e5 * total / poblacion_total) |>
    select(ano, tipo, bloque, total, tasa_100k, pct_extranjeros)
}

hc <- read_block(p_hc, "HC")
he <- read_block(p_he, "HE")

# --- Violentos (robos con violencia / intimidación) ---
re_viol <- "(?i)robos?\\s+con\\s+viol|intimid"
hc_v <- dplyr::filter(hc, str_detect(tipo, re_viol))
he_v <- dplyr::filter(he, str_detect(tipo, re_viol))

say("HC violentos únicos: ", length(unique(hc_v$tipo)))
say("HE violentos únicos: ", length(unique(he_v$tipo)))

# --- Top-5 por media 3y ---
pick_topk <- function(df, k = 5L){
  if (!nrow(df)) return(character())
  df |>
    group_by(tipo) |>
    reframe(mean_last3 = mean(last_k_years(cur_data() |> select(ano, tasa_100k), 3)$tasa_100k, na.rm = TRUE)) |>
    arrange(desc(mean_last3)) |>
    slice_head(n = k) |>
    pull(tipo)
}
hc_top <- pick_topk(hc_v, 5L)
he_top <- pick_topk(he_v, 5L)
say("Top HC: ", paste(hc_top, collapse=" | "))
say("Top HE: ", paste(he_top, collapse=" | "))

# --- Resumen 1-fila-por-tipo (reframe) ---
mk_summary <- function(df, top_types, bloque){
  if (!length(top_types)) return(tibble())
  
  df %>%
    filter(tipo %in% top_types) %>%
    arrange(tipo, ano) %>%
    group_by(tipo) %>%
    reframe(
      bloque = bloque,
      years  = paste0(min(ano, na.rm=TRUE), "–", max(ano, na.rm=TRUE)),
      mean_3y = {
        sub3 <- tail(sort(unique(ano)), 3)
        mean(tasa_100k[ano %in% sub3], na.rm = TRUE)
      },
      corr_d = {
        d_tasa <- mk_delta(tasa_100k)
        d_extr <- mk_delta(pct_extranjeros)
        suppressWarnings(stats::cor(d_tasa, d_extr, use = "complete.obs"))
      },
      p_extr_to_tasa = {
        d_tasa <- mk_delta(tasa_100k)
        d_extr <- mk_delta(pct_extranjeros)
        granger_quick(d_tasa, d_extr, p = 1)[1]
      },
      p_tasa_to_extr = {
        d_tasa <- mk_delta(tasa_100k)
        d_extr <- mk_delta(pct_extranjeros)
        granger_quick(d_tasa, d_extr, p = 1)[2]
      }
    ) %>%
    ungroup()
}

sum_hc <- mk_summary(hc_v, hc_top, "HC")
sum_he <- mk_summary(he_v, he_top, "HE")

# --- Colapsar a 1 fila por (bloque, tipo), ajustar FDR y exportar -------------
res <- bind_rows(sum_hc, sum_he) %>%
  group_by(bloque, tipo) %>%
  summarise(
    years           = first(years),
    mean_3y         = first(mean_3y),
    corr_d          = first(corr_d),
    p_extr_to_tasa  = first(p_extr_to_tasa),
    p_tasa_to_extr  = first(p_tasa_to_extr),
    .groups = "drop"
  ) %>%
  mutate(
    p_extr_to_tasa_fdr = p.adjust(p_extr_to_tasa, method = "BH"),
    p_tasa_to_extr_fdr = p.adjust(p_tasa_to_extr, method = "BH")
  ) %>%
  arrange(bloque, desc(mean_3y)) %>%
  mutate(
    mean_3y = round(mean_3y, 2),
    corr_d  = round(corr_d, 3),
    across(starts_with("p_"), ~round(.x, 4))
  )

# Chequeo defensivo: máximo 8 filas (4 HC + 4 HE)
if (nrow(res) > (length(unique(hc_top)) + length(unique(he_top)))) {
  warning("[31] Persisten duplicados inesperados; aplico slice_head por grupo.")
  res <- res %>% group_by(bloque, tipo) %>% slice_head(n = 1) %>% ungroup()
}

# --- Export ---
out_csv <- file.path(OUT_T, "31_top5_violentos_vs_extranjeros.csv")
readr::write_csv(res, out_csv)
say("✔ Tabla → ", out_csv)

# --- Mini check ---
cat("\n—— MINI CHECK · 31_top5_violentos ————————————————\n")
print(res, n = nrow(res))
cat("———————————————————————————————————————————————\n")

