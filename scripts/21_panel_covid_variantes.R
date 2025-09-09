#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
# 21_panel_covid_variantes.R
# Propósito: generar variantes del panel con dummies COVID (2020, 2021)
# Entradas : data/processed/panel_nacional_2010_2023.csv
# Salidas  : data/processed/panel_con_covid_dummies.csv
#            data/processed/panel_sin_covid.csv
# Uso      : source("D:/delitos_inmigracion_espana/scripts/21_panel_covid_variantes.R")
# Requiere : dplyr, readr, here
# Nota     : no modifica el panel original (crea ficheros nuevos)

library(dplyr); library(readr); library(here)

p_in  <- here("data","processed","panel_nacional_2010_2023.csv")
p_out1<- here("data","processed","panel_con_covid_dummies.csv")
p_out2<- here("data","processed","panel_sin_covid.csv")

panel <- read_csv(p_in, show_col_types = FALSE)

panel_cov <- panel %>%
  mutate(
    d2020 = as.integer(ano == 2020),
    d2021 = as.integer(ano == 2021),
    dcovid = as.integer(ano %in% c(2020,2021))
  )
write_csv(panel_cov, p_out1)

panel_sin <- panel %>% filter(!ano %in% c(2020,2021))
write_csv(panel_sin, p_out2)

message("Con dummies -> ", p_out1)
message("Sin COVID -> ", p_out2)
