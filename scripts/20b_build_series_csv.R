
#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
###############################################################################
# 20b_build_series_csv.R
#
# Construye el dataset consolidado "series.csv" que consumirá el Rmd.
# Fuente principal: data/processed/panel_nacional_2010_2023.csv
# OUT: data/processed/series.csv
###############################################################################

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(here); library(janitor)
})

root <- here::here()
message("ROOT: ", root)

infile <- here::here("data/processed/panel_nacional_2010_2023.csv")
outfile <- here::here("data/processed/series.csv")

stopifnot(file.exists(infile))

df <- readr::read_csv(infile, show_col_types = FALSE) |>
  janitor::clean_names()

# Selección mínima defendible
keep <- c(
  "ano",
  "det_tot","det_ext",
  "det_tot_rate","det_ext_rate",
  "share_extranjeros","pct_extranjeros_poblacion",
  "arope","paro_15_29","pib_pc_real",
  "poblacion_total","poblacion_15_29",
  "l_hc_rate","l_he_rate","l_det_tot_rate","l_det_ext_rate","l_pib_pc_real",
  "covid_2020","covid_2021","covid_2020_2021"
)

missing <- setdiff(keep, names(df))
if (length(missing)) {
  warning("Columnas faltantes en panel: ", paste(missing, collapse=", "),
          "\nSe continuará sin ellas.")
  keep <- intersect(keep, names(df))
}

series <- df |>
  dplyr::select(dplyr::all_of(keep)) |>
  dplyr::arrange(ano)

readr::write_csv(series, outfile)
message("Escrito: ", outfile, " (", nrow(series), " filas, ", ncol(series), " columnas)")
