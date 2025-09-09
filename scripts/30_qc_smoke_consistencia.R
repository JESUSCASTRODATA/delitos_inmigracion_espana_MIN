#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
###############################################################################
# 30_qc_smoke_consistencia.R — QC rápido de consistencia del pipeline
#
# Produce:
#   output/tables/30_qc_summary.csv   (tabla de checks PASS/WARN/FAIL)
#   output/tables/30_qc_duplicates.csv (detalles de duplicados por tabla)
#   output/tables/30_qc_notes.txt     (notas y recuentos útiles)
###############################################################################

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(here); library(janitor)
  library(tidyr); library(stringr); library(fs)
})

fs::dir_create(here::here("output","tables"))

# --------------------------- helpers -----------------------------------------
has_cols <- function(df, cols) all(cols %in% names(df))

safe_read <- function(path) {
  if (!file.exists(path)) return(NULL)
  readr::read_csv(path, show_col_types = FALSE) |> janitor::clean_names()
}

add_row <- function(df, ...) dplyr::bind_rows(df, dplyr::tibble(...))

dup_report <- function(df, keys) {
  if (is.null(df) || !length(keys) || !has_cols(df, keys)) return(NULL)
  df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) |>
    dplyr::summarise(freq = dplyr::n(), .groups = "drop") |>
    dplyr::filter(.data$freq > 1L)
}

chk_status <- function(ok) if (isTRUE(ok)) "PASS" else "FAIL"

# --------------------------- carga tablas ------------------------------------
fp24 <- here::here("output","tables","24_var_granger_resultados.csv")
fp25 <- here::here("output","tables","25_ecm_coefs.csv")
fp26 <- here::here("output","tables","26_dlog_overview.csv")
fp28 <- here::here("output","tables","28_base100_series.csv")
fp29g<- here::here("output","tables","29_granger_resumen.csv")

tb24 <- safe_read(fp24)
tb25 <- safe_read(fp25)
tb26 <- safe_read(fp26)
tb28 <- safe_read(fp28)
tb29g<- safe_read(fp29g)

summary_qc <- dplyr::tibble(check = character(), table = character(),
                            status = character(), details = character())
dups_out   <- NULL
notes      <- character()

# --------------------------- 1) existencia mínima ----------------------------
exists_ok <- list(
  `24_var_granger_resultados.csv` = !is.null(tb24),
  `25_ecm_coefs.csv`              = !is.null(tb25),
  `26_dlog_overview.csv`          = !is.null(tb26),
  `28_base100_series.csv`         = !is.null(tb28),
  `29_granger_resumen.csv`        = !is.null(tb29g)
)

for (nm in names(exists_ok)) {
  summary_qc <- add_row(
    summary_qc,
    check = "file_exists", table = nm,
    status = chk_status(exists_ok[[nm]]),
    details = if (exists_ok[[nm]]) "OK" else "No encontrado"
  )
}

# --------------------------- 2) duplicados por claves ------------------------
# Define claves esperadas (ajusta si cambiaste tus tablas)
keys <- list(
  `24_var_granger_resultados.csv` = c("policy","window","y","x","covid"),
  `25_ecm_coefs.csv`              = c("window","y","x","term"),
  `26_dlog_overview.csv`          = c("window","y","x","spec"),
  `28_base100_series.csv`         = c("window","ano"),
  `29_granger_resumen.csv`        = c("policy","window","y")
)

tbs  <- list(
  `24_var_granger_resultados.csv` = tb24,
  `25_ecm_coefs.csv`              = tb25,
  `26_dlog_overview.csv`          = tb26,
  `28_base100_series.csv`         = tb28,
  `29_granger_resumen.csv`        = tb29g
)

for (nm in names(tbs)) {
  df <- tbs[[nm]]
  kl <- keys[[nm]]
  if (is.null(df)) next
  if (!has_cols(df, kl)) {
    summary_qc <- add_row(summary_qc, check = "keys_present", table = nm,
                          status = "WARN",
                          details = paste0("Faltan columnas clave: ",
                                           paste(setdiff(kl, names(df)), collapse = ", ")))
    next
  }
  dups <- dup_report(df, kl)
  if (!is.null(dups) && nrow(dups) > 0) {
    summary_qc <- add_row(summary_qc, check = "duplicates", table = nm,
                          status = "FAIL",
                          details = paste0(nrow(dups), " combinaciones duplicadas"))
    dups_out <- dplyr::bind_rows(dups_out,
                                 dplyr::mutate(dups, .table = nm, .before = 1))
  } else {
    summary_qc <- add_row(summary_qc, check = "duplicates", table = nm,
                          status = "PASS", details = "Sin duplicados por clave")
  }
}

# --------------------------- 3) ventanas esperadas ---------------------------
expected_keep   <- c("2010_2023","2010_2019_pre","2020_2023_post","sin_covid")
expected_remove <- c("2010_2023_sin_covid","2010_2019_pre")

if (!is.null(tb24) && has_cols(tb24, c("policy","window"))) {
  win_keep    <- sort(unique(tb24$window[tb24$policy == "keep"]))
  win_remove  <- sort(unique(tb24$window[tb24$policy == "remove"]))
  miss_keep   <- setdiff(expected_keep, win_keep)
  miss_remove <- setdiff(expected_remove, win_remove)
  
  summary_qc <- add_row(summary_qc, check = "windows_keep_present", table = "24_var_granger_resultados.csv",
                        status = if (length(miss_keep)==0) "PASS" else "WARN",
                        details = if (length(miss_keep)==0) "OK" else paste("Faltan:", paste(miss_keep, collapse=", ")))
  summary_qc <- add_row(summary_qc, check = "windows_remove_present", table = "24_var_granger_resultados.csv",
                        status = if (length(miss_remove)==0) "PASS" else "WARN",
                        details = if (length(miss_remove)==0) "OK" else paste("Faltan:", paste(miss_remove, collapse=", ")))
}

# --------------------------- 4) COVID consistencia ---------------------------
# En policy=keep, ventanas '2010_2019_pre' y 'sin_covid' no deben tener covid==TRUE
if (!is.null(tb24) && has_cols(tb24, c("policy","window","covid"))) {
  bad_keep <- tb24 |>
    dplyr::filter(.data$policy == "keep",
                  .data$window %in% c("2010_2019_pre","sin_covid"),
                  isTRUE(.data$covid))
  summary_qc <- add_row(summary_qc, check = "covid_flags_incompatible",
                        table = "24_var_granger_resultados.csv",
                        status = if (nrow(bad_keep)==0) "PASS" else "FAIL",
                        details = if (nrow(bad_keep)==0) "OK"
                        else paste0("Filas covid=TRUE en ventanas sin COVID: ", nrow(bad_keep)))
}

# --------------------------- 5) Serie base100 (años) -------------------------
if (!is.null(tb28) && has_cols(tb28, c("window","ano"))) {
  # 'sin_covid' debe excluir 2020 y 2021
  bad_years <- tb28 |>
    dplyr::filter(.data$window == "sin_covid", .data$ano %in% c(2020L, 2021L))
  summary_qc <- add_row(summary_qc, check = "base100_sin_covid_sin_2020_2021",
                        table = "28_base100_series.csv",
                        status = if (nrow(bad_years)==0) "PASS" else "FAIL",
                        details = if (nrow(bad_years)==0) "OK"
                        else paste0("Años indebidos: ",
                                    paste(unique(bad_years$ano), collapse=", ")))
}

# --------------------------- 6) notas útiles ---------------------------------
if (!is.null(tb24)) {
  n_sig <- tb24 |>
    dplyr::mutate(robust = !is.na(.data$stable) & .data$stable &
                    is.finite(.data$serial_p_value) & .data$serial_p_value >= 0.05 & .data$n >= 8,
                  X_causes_Y = is.finite(.data$p_y_on_x) & .data$p_y_on_x < 0.05) |>
    dplyr::filter(.data$robust, .data$X_causes_Y) |>
    dplyr::count(.data$policy, .data$window, .data$y, name = "n_sig")
  notes <- c(notes, "# Robustos y significativos (X⇒Y, α=0.05):",
             capture.output(print(n_sig, n = 200)))
}
if (!is.null(tb25)) {
  ecm_stat <- tb25 |>
    dplyr::mutate(sig = .data$p_value < 0.05, neg = .data$estimate < 0) |>
    dplyr::count(.data$window, .data$y, .data$sig, .data$neg, name = "n")
  notes <- c(notes, "\n# ECM alpha (signo y significancia):",
             capture.output(print(ecm_stat, n = 200)))
}

# --------------------------- 7) export ---------------------------------------
readr::write_csv(summary_qc, here::here("output","tables","30_qc_summary.csv"))
if (!is.null(dups_out)) readr::write_csv(dups_out, here::here("output","tables","30_qc_duplicates.csv"))
writeLines(notes, here::here("output","tables","30_qc_notes.txt"))

message("✅ QC listo: 30_qc_summary.csv", if (!is.null(dups_out)) ", 30_qc_duplicates.csv" else "",
        " y 30_qc_notes.txt en output/tables/")

