#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
################################################################################
# 60_reproducibility_env.R
#
# Objetivo:
#   - Capturar entorno de reproducibilidad (sessionInfo, paquetes, sistema).
#
# Salidas:
#   - output/repro/sessionInfo.txt
#   - output/repro/packages.csv
#   - output/repro/sysinfo.txt
################################################################################

suppressPackageStartupMessages({
  library(here); library(readr)
})

dir.create(here::here('output','repro'), recursive = TRUE, showWarnings = FALSE)

# sessionInfo
sink(here::here('output','repro','sessionInfo.txt'))
print(sessionInfo())
sink()

# paquetes instalados
ip <- as.data.frame(installed.packages(), stringsAsFactors = FALSE)
cols <- intersect(colnames(ip), c('Package','Version','Priority','Built','LibPath'))
ip2 <- ip[, cols, drop = FALSE][order(ip[,'Package']), ]
readr::write_csv(ip2, here::here('output','repro','packages.csv'))

# sysinfo
sink(here::here('output','repro','sysinfo.txt'))
cat('R.version.string: ', R.version.string, '\n', sep='')
cat('Platform: ', R.version$platform, '\n', sep='')
cat('OS.type: ', .Platform$OS.type, '\n', sep='')
cat('Path: ', normalizePath('.'), '\n', sep='')
cat('TZ: ', Sys.getenv('TZ', unset=''), '\n', sep='')
cat('LANG: ', Sys.getenv('LANG', unset=''), '\n', sep='')
cat('Locale: ', paste(Sys.getlocale(), collapse=' | '), '\n', sep='')
sink()

message('Env reproducibilidad guardado en output/repro')
