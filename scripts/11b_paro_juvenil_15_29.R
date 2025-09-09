
#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
###############################################################################
# 11b_paro_juvenil_15_29.R
#
# Calcula la tasa de paro 15–29 ponderada por población de bins 15–19/20–24/25–29.
# IN: data/processed/poblacion_15_29_bins_anual.csv (ano, edad_bin, poblacion)
#     data/processed/tasas_paro_por_bin.csv (ano, edad_bin, paro_rate)
# OUT: data/processed/paro_15_29_anual.csv (ano, paro_15_29)
###############################################################################

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(janitor); library(here); library(tidyr)
})

pop_bins_file <- here::here("data/processed/poblacion_15_29_bins_anual.csv")
paro_bins_file <- here::here("data/processed/tasas_paro_por_bin.csv")
outfile <- here::here("data/processed/paro_15_29_anual.csv")

if (!file.exists(pop_bins_file) || !file.exists(paro_bins_file)) {
  warning("Faltan entradas para calcular el paro 15–29; se omite cálculo.")
  quit(save = "no", status = 0)
}

pop <- readr::read_csv(pop_bins_file, show_col_types = FALSE) |> janitor::clean_names()
paro <- readr::read_csv(paro_bins_file, show_col_types = FALSE) |> janitor::clean_names()

df <- paro |>
  dplyr::inner_join(pop, by = c("ano","edad_bin")) |>
  dplyr::group_by(ano) |>
  dplyr::summarise(
    paro_15_29 = sum(paro_rate * poblacion) / sum(poblacion),
    .groups = "drop"
  ) |>
  dplyr::arrange(ano)

readr::write_csv(df, outfile)
message("Escrito: ", outfile, " (", nrow(df), " filas)")
