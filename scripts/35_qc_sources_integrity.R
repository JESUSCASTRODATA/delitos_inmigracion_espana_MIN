#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
################################################################################
# 35_qc_sources_integrity.R
#
# Objetivo:
#   - Calcular hashes SHA256 de ficheros en data/raw y data/processed
#   - Comparar con una línea base (config/file_hashes_baseline.csv) si existe
#   - Reportar cambios: NEW / MODIFIED / DELETED
#
# Salidas (output/tables):
#   - qa35_file_hashes_snapshot.csv
#   - qa35_file_hashes_diff.csv (si hay baseline)
#
# Dependencias: digest, fs, here, readr, dplyr, tibble, janitor
################################################################################

suppressPackageStartupMessages({
  library(here); library(readr); library(dplyr); library(tibble); library(janitor)
  library(fs); library(digest)
})

dir.create(here("output","tables"), recursive = TRUE, showWarnings = FALSE)

snap_dir <- function(base, subdir){
  p <- here(base, subdir)
  if (!dir_exists(p)) return(tibble(dir=character(), file=character(), size=integer(), mtime=as.POSIXct(character()), sha256=character()))
  files <- dir_ls(p, recurse = TRUE, type = "file")
  if (length(files) == 0) return(tibble(dir=character(), file=character(), size=integer(), mtime=as.POSIXct(character()), sha256=character()))
  info <- file_info(files)
  tibble(
    dir = subdir,
    file = path_rel(files, start = here(base)),
    size = as.double(info$size),
    mtime = as.POSIXct(info$modification_time, tz = "UTC"),
    sha256 = vapply(files, function(f) digest::digest(file = f, algo = "sha256"), FUN.VALUE = character(1))
  )
}

snap <- bind_rows(
  snap_dir("data", "raw"),
  snap_dir("data", "processed")
) %>% arrange(dir, file)

write_csv(snap, here("output","tables","qa35_file_hashes_snapshot.csv"))

# Comparación con baseline, si existe
baseline_path <- here("config","file_hashes_baseline.csv")
if (file_exists(baseline_path)){
  base <- suppressMessages(readr::read_csv(baseline_path, show_col_types = FALSE)) %>% clean_names()
  # asegurar columnas
  if (!all(c("dir","file","sha256") %in% names(base))){
    warning("Baseline sin columnas esperadas (dir,file,sha256). Se ignora la comparación.")
    quit(status = 0)
  }
  # join y estados
  cmp <- full_join(base %>% mutate(state="BASE"), snap %>% mutate(state="SNAP"),
                   by = c("dir","file"), suffix = c("_base","_snap")) %>%
    mutate(
      status = case_when(
        is.na(sha256_base) & !is.na(sha256_snap) ~ "NEW",
        !is.na(sha256_base) & is.na(sha256_snap) ~ "DELETED",
        !is.na(sha256_base) & !is.na(sha256_snap) & sha256_base != sha256_snap ~ "MODIFIED",
        TRUE ~ "UNCHANGED"
      )
    ) %>% arrange(dir, file)
  write_csv(cmp, here("output","tables","qa35_file_hashes_diff.csv"))
} else {
  message("No hay baseline en config/file_hashes_baseline.csv. Puedes crearla copiando qa35_file_hashes_snapshot.csv a config/.")
}

message("✅ QC 35 terminado. Revisa:",
        "\n - output/tables/qa35_file_hashes_snapshot.csv",
        "\n - output/tables/qa35_file_hashes_diff.csv (si existe)")

