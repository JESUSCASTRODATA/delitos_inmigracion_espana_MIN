#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
###############################################################################
# 70_export_figs.R — Exporta figuras de todas las series en data/processed
# - Tema "blanco perla" (#FAF9F6) + paleta Okabe–Ito (colorblind-safe)
# - Facetas por clave (TOTAL o hasta 8 claves)
# - Marcado de puntos con flags QA33 (spikes YoY y outliers de nivel IQR)
# - Sombreado COVID (2020–2021) si aplica
# Salida: PNGs en output/figs
###############################################################################

suppressPackageStartupMessages({
  library(here); library(readr); library(dplyr); library(tidyr)
  library(janitor); library(fs); library(ggplot2); library(stringr); library(purrr)
})

# ---------- Tema y utilidades gráficas ---------------------------------------
theme_ok <- tryCatch({
  # Si tienes el tema unificado, se usa:
  source(here::here("scripts","00_theme_figuras.R"), local = TRUE)
  TRUE
}, error = function(e) FALSE)

if (!isTRUE(theme_ok)) {
  # Fallback mínimo (por si no existe 00_theme_figuras.R)
  PEARL <- "#FAF9F6"
  OI <- c("#000000","#E69F00","#56B4E9","#009E73",
          "#F0E442","#0072B2","#D55E00","#CC79A7")
  theme_pearl <- function(base_size = 12) {
    theme_minimal(base_size = base_size) +
      theme(
        plot.background  = element_rect(fill = PEARL, colour = NA),
        panel.background = element_rect(fill = PEARL, colour = NA),
        legend.background= element_rect(fill = PEARL, colour = NA),
        legend.key       = element_rect(fill = PEARL, colour = NA),
        panel.grid.major = element_line(colour = "#EDEBE6", linewidth = .35),
        panel.grid.minor = element_line(colour = "#EEEDE8", linewidth = .2)
      )
  }
  ggsave_pearl <- function(filename, plot = ggplot2::last_plot(),
                           width = 12, height = 7, dpi = 300, units = "in", ...) {
    ggplot2::ggsave(filename, plot, width = width, height = height,
                    dpi = dpi, units = units, bg = PEARL, ...)
  }
}

# ---------- Comprobaciones y salidas -----------------------------------------
if (!requireNamespace('ggplot2', quietly = TRUE)){
  stop('Se requiere ggplot2 para 70_export_figs.R')
}

fs::dir_create(here::here('output','figs'))
fs::dir_create(here::here('output','tables'))

# ---------- Lectura robusta ---------------------------------------------------
read_csv_auto <- function(fp){
  out <- tryCatch(suppressMessages(readr::read_csv(fp, show_col_types = FALSE)),
                  error = function(e) NULL)
  if (is.null(out)){
    out <- tryCatch(suppressMessages(readr::read_delim(fp, delim = ';', show_col_types = FALSE)),
                    error = function(e) NULL)
  }
  out
}

num_es <- function(x){
  readr::parse_number(as.character(x),
                      locale = readr::locale(decimal_mark = ',', grouping_mark = '.'))
}

to_long <- function(df, name){
  if (is.null(df)) return(NULL)
  df <- janitor::clean_names(df)
  if (!('ano' %in% names(df))) return(NULL)
  
  # Candidatas de valor
  val_candidates <- c('valor','poblacion_total','poblacion_espanola','poblacion_extranjera',
                      'arope','paro_15_29','pib_pc_real')
  vcol <- intersect(val_candidates, names(df))
  if (length(vcol)==0){
    nc <- setdiff(names(df), c('ano','tipo','tipologia','tipologia_penal','sexo'))
    nnum <- nc[vapply(nc, function(c) is.numeric(df[[c]]) || is.integer(df[[c]]), logical(1))]
    vcol <- if (length(nnum)>0) nnum[1] else if (length(nc)>0) nc[1] else return(NULL)
  } else { vcol <- vcol[1] }
  
  key <- intersect(names(df), c('tipo','tipologia','tipologia_penal','sexo'))
  val <- num_es(df[[vcol]])
  
  if (length(key)==0){
    tibble(
      dataset = basename(name),
      ano     = as.integer(df$ano),
      key     = 'TOTAL',
      value   = val
    )
  } else {
    tibble(
      dataset = basename(name),
      ano     = as.integer(df$ano),
      key     = do.call(paste, c(df[key], sep='|')),
      value   = val
    )
  }
}

# ---------- Construye long con todos los CSV de processed --------------------
files <- list.files(here::here('data','processed'), pattern='\\.csv$', recursive=TRUE, full.names=TRUE)
long_list <- purrr::imap(files, ~ to_long(read_csv_auto(.x), .x))
long <- dplyr::bind_rows(long_list[!vapply(long_list, is.null, logical(1))]) |>
  filter(!is.na(ano)) |>
  arrange(dataset, key, ano)

if (nrow(long)==0){
  cat('No hay datos para graficar.\n')
  quit(status = 0)
}

# ---------- Flags de QA33 (si existen) ---------------------------------------
flag_spikes <- here::here('output','tables','qa33_spikes_yoy_mad.csv')
flag_level  <- here::here('output','tables','qa33_outliers_level_iqr.csv')
sp <- if (file.exists(flag_spikes)) suppressMessages(readr::read_csv(flag_spikes, show_col_types = FALSE)) else NULL
lv <- if (file.exists(flag_level))  suppressMessages(readr::read_csv(flag_level,  show_col_types = FALSE)) else NULL

# Normaliza nombres esperados de flags
if (!is.null(sp)) sp <- janitor::clean_names(sp)
if (!is.null(lv)) lv <- janitor::clean_names(lv)

# ---------- Funciones de ayuda para gráfico ----------------------------------
shade_covid <- function(df){
  rng <- range(df$ano, na.rm = TRUE)
  # sombrear si el rango abarca 2020–2021
  if (is.finite(rng[1]) && is.finite(rng[2]) && rng[1] <= 2021 && rng[2] >= 2020) {
    annotate("rect", xmin = 2019.5, xmax = 2021.5, ymin = -Inf, ymax = Inf,
             alpha = 0.08, fill = "grey50")
  } else {
    NULL
  }
}

# Color por clave (hasta 8 facetas; si TOTAL, un solo color)
pal_oi <- c("#0072B2","#009E73","#E69F00","#CC79A7","#56B4E9","#D55E00","#F0E442","#000000")

# ---------- Bucle por dataset -------------------------------------------------
by_ds <- split(long, long$dataset)

for (ds in names(by_ds)){
  d <- by_ds[[ds]] |> arrange(ano)
  
  # Selección de claves: TOTAL o primeras 8
  keys <- unique(d$key)
  if ('TOTAL' %in% keys) {
    d <- d %>% filter(key == 'TOTAL')
  } else if (length(keys) > 8){
    keys8 <- keys[1:8]
    d <- d %>% filter(key %in% keys8)
  }
  
  # Construye base del plot
  # Si hay varias claves, coloreamos por clave; si sólo hay TOTAL, sin leyenda
  multi_key <- length(unique(d$key)) > 1
  aes_base  <- if (multi_key) aes(x = ano, y = value, group = key, colour = key) else aes(x = ano, y = value, group = key)
  
  p <- ggplot(d, aes_base) +
    shade_covid(d) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 1.8) +
    facet_wrap(~ key, scales = 'free_y') +
    labs(title = paste0('Serie temporal: ', ds),
         x = 'Año', y = 'Valor') +
    theme_pearl()
  
  if (multi_key) {
    # paleta estable y discreta (máx 8)
    p <- p + scale_colour_manual(values = setNames(pal_oi[seq_len(min(length(unique(d$key)), length(pal_oi)))], unique(d$key))) +
      guides(colour = "none")  # ocultamos leyenda (ya está facetado)
  }
  
  # Puntos con flags si hay QA33
  if (!is.null(sp) && all(c('dataset','key','ano','flag_yoy') %in% names(sp))){
    spd <- sp %>% filter(dataset == ds, isTRUE(flag_yoy)) %>% distinct(key, ano)
    if (nrow(spd) > 0){
      p <- p + geom_point(
        data = semi_join(d, spd, by = c('key','ano')),
        aes(x = ano, y = value),
        size = 3, shape = 21, stroke = 0.5, colour = "#A45A00", fill = "#FFC27A"
      )
    }
  }
  if (!is.null(lv) && all(c('dataset','key','ano','flag_level_iqr') %in% names(lv))){
    lvd <- lv %>% filter(dataset == ds, isTRUE(flag_level_iqr)) %>% distinct(key, ano)
    if (nrow(lvd) > 0){
      p <- p + geom_point(
        data = semi_join(d, lvd, by = c('key','ano')),
        aes(x = ano, y = value),
        size = 3.2, shape = 24, stroke = 0.5, colour = "#7A0030", fill = "#F79CA8"
      )
    }
  }
  
  # Guardado
  out <- here::here('output','figs', paste0(gsub('[^A-Za-z0-9_]+','_', tools::file_path_sans_ext(ds)), '_ts.png'))
  ggsave_pearl(filename = out, plot = p, width = 12, height = 7)
  
}

cat('QC 70: figuras exportadas en output/figs\n')


