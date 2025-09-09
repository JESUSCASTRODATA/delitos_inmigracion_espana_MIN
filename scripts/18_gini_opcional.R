
#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
###############################################################################
# 18_gini_opcional.R
#
# Limpia data/raw/gini.csv y genera data/processed/gini_total.csv (2010–2021).
# No se integra en el panel principal por defecto; se usa para robustez.
###############################################################################

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(janitor); library(here); library(stringr)
})

infile <- here::here("data/raw/gini.csv")
outfile <- here::here("data/processed/gini_total.csv")

if (!file.exists(infile)) {
  stop("No existe data/raw/gini.csv. Aporta el fichero o desactiva este paso.")
}

raw <- suppressWarnings(readr::read_csv2(infile, show_col_types = FALSE))
if (nrow(raw) == 0) {
  raw <- suppressWarnings(readr::read_csv(infile, show_col_types = FALSE))
}
df <- raw |> janitor::clean_names()

# Heurísticas comunes de Eurostat/INE (ajusta si cambia tu gini.csv)
candidatos_ano <- grep("ano|año|year|periodo|time|period", names(df), value = TRUE)
candidatos_val <- grep("gini|valor|value|indice|index|total", names(df), value = TRUE)

if (!length(candidatos_ano) || !length(candidatos_val)) {
  stop("No se encuentran columnas de año/valor plausibles en gini.csv")
}

col_ano <- candidatos_ano[[1]]
col_val <- candidatos_val[[1]]

gini <- df |>
  dplyr::transmute(
    ano = as.integer(!!rlang::sym(col_ano)),
    gini = suppressWarnings(as.numeric(gsub(",", ".", as.character(!!rlang::sym(col_val)))))
  ) |>
  dplyr::filter(!is.na(ano), !is.na(gini)) |>
  dplyr::filter(ano >= 2010, ano <= 2023) |>
  dplyr::arrange(ano)

readr::write_csv(gini, outfile)
message("Escrito: ", outfile, " (", nrow(gini), " filas)")
