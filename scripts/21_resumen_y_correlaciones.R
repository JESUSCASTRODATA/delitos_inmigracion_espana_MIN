#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
###############################################################################
# 21_correlaciones_totales.R
#
# Objetivo:
#   Correlaciones (Pearson, Spearman, Kendall) entre:
#     - delitos_totales (TOTAL Infracciones Penales por año)
#     - extranjeros_totales (población extranjera por año)
#     - espanoles_totales  (población española por año)
#
# Entradas (data/processed/):
#   - hechos_conocidos_total_nacional.csv  (ano, tipo, valor)  --> delitos_totales
#   - poblacion_es_extr_por_ano.csv        (ano, poblacion_espanola, poblacion_extranjera)
#
# Salidas (output/tables/):
#   - 21_cor_totales_dataset.csv           (dataset fusionado y limpio)
#   - 21_cor_totales_cor_long.csv          (long: window, method, var1, var2, r, p, n)
#   - 21_cor_totales_cor_wide.csv          (matrices por método y ventana, una por hoja si exportas a xlsx)
#   - 21_cor_totales_top.csv               (top |r| por ventana y método)
###############################################################################

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(here); library(janitor)
  library(stringr); library(tidyr); library(purrr); library(fs)
})

YEAR_MIN <- 2010L; YEAR_MAX <- 2023L
dir_create(here("output","tables"))

# ----------------------- Helpers -----------------------
to_ascii_lower <- function(x){
  x0 <- tolower(trimws(as.character(x)))
  iconv(x0, from = "UTF-8", to = "ASCII//TRANSLIT")
}
is_total_infrac <- function(tipo){
  s <- to_ascii_lower(tipo)
  grepl("total\\s+infracciones\\s+penales", s)
}
read_hc_total <- function(path){
  stopifnot(file.exists(path))
  df <- readr::read_csv(path, show_col_types = FALSE) %>% clean_names()
  need <- c("ano","tipo","valor")
  if (!all(need %in% names(df))) stop("Faltan columnas en HC: ", paste(setdiff(need, names(df)), collapse=", "))
  df %>%
    filter(is_total_infrac(tipo)) %>%
    transmute(ano = as.integer(ano), delitos_totales = as.numeric(valor)) %>%
    arrange(ano)
}
read_pop_es_extr <- function(path){
  stopifnot(file.exists(path))
  df <- readr::read_csv(path, show_col_types = FALSE) %>% clean_names()
  need <- c("ano","poblacion_espanola","poblacion_extranjera")
  if (!all(need %in% names(df))) stop("Faltan columnas en población: ", paste(setdiff(need, names(df)), collapse=", "))
  df %>%
    transmute(
      ano = as.integer(ano),
      espanoles_totales  = as.numeric(poblacion_espanola),
      extranjeros_totales= as.numeric(poblacion_extranjera)
    ) %>% arrange(ano)
}

pairwise_cor <- function(dat, cols, method = c("pearson","spearman","kendall")){
  method <- match.arg(method)
  stopifnot(all(cols %in% names(dat)))
  combs <- t(combn(cols, 2))
  tibble(var1 = combs[,1], var2 = combs[,2]) %>%
    rowwise() %>%
    mutate(
      n = sum(stats::complete.cases(dat[[var1]], dat[[var2]])),
      r = suppressWarnings(stats::cor(dat[[var1]], dat[[var2]], use = "pairwise.complete.obs", method = method)),
      p = tryCatch(stats::cor.test(dat[[var1]], dat[[var2]], method = method)$p.value, error = function(e) NA_real_)
    ) %>% ungroup()
}

# ----------------------- Lectura -----------------------
fp_hc   <- here("data","processed","hechos_conocidos_total_nacional.csv")
fp_pop  <- here("data","processed","poblacion_es_extr_por_ano.csv")

hc  <- read_hc_total(fp_hc)
pop <- read_pop_es_extr(fp_pop)

# ----------------------- Merge + filtro años -----------------------
base <- tibble(ano = YEAR_MIN:YEAR_MAX)
df <- base %>%
  left_join(hc,  by = "ano") %>%
  left_join(pop, by = "ano") %>%
  filter(dplyr::between(ano, YEAR_MIN, YEAR_MAX))

# QA rápida: NAs
qa_na <- df %>% summarise(across(-ano, ~ sum(is.na(.x))))
readr::write_csv(df, here("output","tables","21_cor_totales_dataset.csv"))

# ----------------------- Ventanas -----------------------
mask_all  <- df$ano >= 2010 & df$ano <= 2023
mask_pre  <- df$ano >= 2010 & df$ano <= 2019
mask_post <- df$ano >= 2020 & df$ano <= 2023
mask_sin  <- df$ano %in% setdiff(2010:2023, c(2020, 2021))

wins <- list(
  `2010_2023`      = mask_all,
  `2010_2019_pre`  = mask_pre,
  `2020_2023_post` = mask_post,
  `sin_covid`      = mask_sin
)

cols <- c("delitos_totales","extranjeros_totales","espanoles_totales")
methods <- c("pearson","spearman","kendall")

# ----------------------- Correlaciones -----------------------
res_all <- list()

for (wname in names(wins)) {
  m <- wins[[wname]]
  dt <- df[m, ]
  for (met in methods) {
    rtb <- pairwise_cor(dt, cols, method = met) %>%
      mutate(window = wname, method = met)
    res_all[[length(res_all) + 1]] <- rtb
  }
}

res_long <- bind_rows(res_all) %>%
  select(window, method, var1, var2, r, p, n) %>%
  arrange(window, method, desc(abs(r)))
readr::write_csv(res_long, here("output","tables","21_cor_totales_cor_long.csv"))

# Wide (una mini-matriz por método y ventana)
res_wide <- res_long %>%
  mutate(pair = paste(var1, var2, sep = "_vs_")) %>%
  select(window, method, pair, r, p, n) %>%
  arrange(window, method, desc(abs(r)))
readr::write_csv(res_wide, here("output","tables","21_cor_totales_cor_wide.csv"))

# Top |r|
top <- res_long %>%
  group_by(window, method) %>%
  slice_max(order_by = abs(r), n = 3, with_ties = FALSE) %>%
  ungroup()
readr::write_csv(top, here("output","tables","21_cor_totales_top.csv"))

message("✅ Correlaciones (totales) exportadas en output/tables/")
