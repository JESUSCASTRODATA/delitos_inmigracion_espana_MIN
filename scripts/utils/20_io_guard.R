#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
# 20_io_guard.R — Contratos de entrada/salida para 20_preguntas_basicas.R

suppressPackageStartupMessages({
  library(here); library(readr); library(dplyr); library(janitor); library(stringr); library(fs); library(tidyr)
})

YEAR_MIN <- 2010L; YEAR_MAX <- 2023L
out_dir  <- here("output","tables"); fs::dir_create(out_dir)

req_files <- tibble::tibble(
  path = c(
    here("data","processed","core_indicadores.csv")
  ),
  role = "REQUIRED"
)

opt_files <- tibble::tibble(
  path = c(
    here("data","processed","core_indicadores_derivadas.csv"),
    here("data","processed","pct_extranjeros_poblacion.csv"),
    here("data","processed","poblacion_extranjera_total.csv"),
    here("data","processed","poblacion_total_nacional.csv")
  ),
  role = "OPTIONAL"
)

expect_core <- c(
  "ano",
  # variables de respuesta (niveles)
  "tasa_hc_100k","tasa_he_100k","share_extranjeros",
  # controles niveles
  "pib_pc_real","paro_joven","arope",
  # X principal (puede reconstruirse)
  "pct_extranjeros"
)

expect_der <- c(
  "ano",
  # controles en Δ
  "dln_pib_pc",
  # X e Ys en Δ (pueden calcularse si faltan d_tasa_*):
  "d_pct_extran","d_share_ext","d_tasa_hc_100k","d_tasa_he_100k"
)

exists_tbl <- function(df) df %>% mutate(exists = file.exists(path))
core_path  <- req_files$path[1]

io_exists <- bind_rows(exists_tbl(req_files), exists_tbl(opt_files))
write_csv(io_exists, file.path(out_dir, "20_io_exists.csv"))

# ---------- Lecturas seguras ----------
safe_read <- function(p) if (file.exists(p)) suppressMessages(read_csv(p, show_col_types = FALSE) |> clean_names()) else NULL
core <- safe_read(core_path)

# ---- 20_QC_INLINE_IN ----
suppressPackageStartupMessages({ library(readr); library(dplyr); library(janitor); library(stringr); library(here); library(fs); library(purrr); library(tidyr) })
YEAR_MIN <- 2010L; YEAR_MAX <- 2023L
dir_create(here("output","tables"))

.qc_msg <- function(...) message("[20:QC-IN] ", paste0(...))
.qc_rows <- function(x) if (is.null(x)) NA_integer_ else nrow(x)

qc_expect <- list(
  core = list(
    path = here("data","processed","core_indicadores.csv"),
    required = c("ano","tasa_hc_100k","tasa_he_100k","share_extranjeros","pib_pc_real","paro_joven","arope"),
    nice = c("pct_extranjeros")
  ),
  der = list(
    path = here("data","processed","core_indicadores_derivadas.csv"),
    optional = TRUE,
    required = c("ano","dln_pib_pc"),
    nice = c("d_pct_extran","d_share_ext","d_tasa_hc_100k","d_tasa_he_100k")
  ),
  px = list(path = here("data","processed","pct_extranjeros_poblacion.csv"), optional = TRUE),
  ext = list(path = here("data","processed","poblacion_extranjera_total.csv"), optional = TRUE),
  tot = list(path = here("data","processed","poblacion_total_nacional.csv"), optional = TRUE)
)

.safe_read <- function(p) if (file.exists(p)) suppressMessages(read_csv(p, show_col_types = FALSE) |> clean_names()) else NULL

qc_tbl <- tibble(file = character(), exists = logical(), n_rows = integer(), missing_required = character(), year_coverage = character())
for (nm in names(qc_expect)) {
  spec <- qc_expect[[nm]]
  p <- spec$path; ex <- file.exists(p)
  df <- .safe_read(p)
  miss <- character()
  if (ex && !is.null(spec$required)) miss <- setdiff(spec$required, names(df))
  yrs <- if (ex && "ano" %in% names(df)) sort(unique(df$ano)) else integer()
  cov <- if (length(yrs)) sprintf("%s … %s (n=%d)", min(yrs), max(yrs), length(yrs)) else "N/D"
  qc_tbl <- bind_rows(qc_tbl, tibble(file = nm, exists = ex, n_rows = .qc_rows(df), missing_required = paste(miss, collapse="|"), year_coverage = cov))
  # preview de esquema y NAs
  if (ex) {
    write_csv(tibble(col = names(df), type = vctrs::vec_ptype_full(df), n_na = colSums(is.na(df))), here("output","tables", paste0("20_schema_", nm, ".csv")))
    write_csv(head(df, 10), here("output","tables", paste0("20_preview_", nm, ".csv")))
  }
}

write_csv(qc_tbl, here("output","tables","20_qc_inputs_overview.csv"))

# Semáforo rápido
sev <- "PASS"
if (!qc_tbl$exists[qc_tbl$file=="core"]) { .qc_msg("Falta core_indicadores.csv → FAIL"); sev <- "FAIL" }
if (qc_tbl$exists[qc_tbl$file=="core"] && nzchar(qc_tbl$missing_required[qc_tbl$file=="core"])) {
  .qc_msg("Core sin columnas clave: ", qc_tbl$missing_required[qc_tbl$file=="core"]); sev <- if (sev=="PASS") "WARN" else sev
}
if (sev=="FAIL") stop("[20:QC-IN] Entradas críticas ausentes. Revisa output/tables/20_qc_inputs_overview.csv")
.qc_msg("QC-IN listo (", sev, ").")
# ---- /20_QC_INLINE_IN ----


# ---------- Validaciones core ----------
viol <- list()

add_viol <- function(type, msg, sev=c("FAIL","WARN","INFO"), ctx=list()){
  viol[[length(viol)+1]] <<- c(list(type=type, message=msg, severity=match.arg(sev)), ctx)
}

if (is.null(core)) {
  add_viol("file_missing", sprintf("No existe %s", core_path), "FAIL")
} else {
  # columnas mínimas
  miss_core <- setdiff(expect_core, names(core))
  if (length(miss_core)) add_viol("core_missing_cols", paste(miss_core, collapse=", "), "WARN")
  
  # rango temporal
  if (!"ano" %in% names(core)) {
    add_viol("no_ano", "Falta columna 'ano' en core_indicadores.csv", "FAIL")
  } else {
    yrs <- sort(unique(core$ano))
    if (!all(YEAR_MIN:YEAR_MAX %in% yrs)) {
      add_viol("core_year_coverage",
               sprintf("Cobertura incompleta. Esperado %d-%d; presente: %s",
                       YEAR_MIN, YEAR_MAX, paste(yrs, collapse=", ")), "WARN")
    }
  }
  
  # controles en niveles
  for (cname in c("pib_pc_real","paro_joven","arope")) {
    if (!cname %in% names(core)) add_viol("control_missing", sprintf("Falta control de niveles: %s", cname), "WARN")
  }
  
  # tasa deltas si faltan → advertir (20_* las genera si no existen)
  for (dname in c("d_tasa_hc_100k","d_tasa_he_100k")) {
    if (!dname %in% names(core)) add_viol("delta_missing",
                                          sprintf("Δ faltante en core (%s). Se podrá construir on-the-fly, mejor añadirlo en derivadas.", dname), "INFO")
  }
}

# ---------- Derivadas (opcionales) ----------
der_path <- here("data","processed","core_indicadores_derivadas.csv")
der <- safe_read(der_path)
if (!is.null(der)) {
  miss_der <- setdiff(expect_der, names(der))
  if (length(miss_der)) add_viol("derivadas_missing_cols", paste(miss_der, collapse=", "), "WARN")
  # n pequeños en Δ (perderás el primer año)
  if ("ano" %in% names(der)) {
    yrsd <- sort(unique(der$ano))
    if (length(yrsd) >= 2 && min(yrsd) == YEAR_MIN) {
      add_viol("delta_first_year_lost", sprintf("En Δ se pierde %d por diferencia.", YEAR_MIN), "INFO")
    }
  }
} else {
  add_viol("derivadas_absent", "No existe core_indicadores_derivadas.csv — se omiten parciales en Δ", "WARN")
}

# ---------- Reconstrucción pct_extranjeros (si falta) ----------
need_pct <- is.null(core) || !"pct_extranjeros" %in% names(core)
if (need_pct) {
  px <- safe_read(here("data","processed","pct_extranjeros_poblacion.csv"))
  if (!is.null(px) && "pct_extranjeros" %in% names(px)) {
    add_viol("pct_from_file", "pct_extranjeros se tomará de pct_extranjeros_poblacion.csv", "INFO")
  } else {
    E  <- safe_read(here("data","processed","poblacion_extranjera_total.csv"))
    Tt <- safe_read(here("data","processed","poblacion_total_nacional.csv"))
    if (!is.null(E) && !is.null(Tt)) {
      nE <- names(E)[str_detect(names(E),"extranjera")]
      nT <- names(Tt)[str_detect(names(Tt),"total")]
      if (length(nE) && length(nT)) {
        add_viol("pct_from_pop", "pct_extranjeros se reconstruirá como extranjera/total (fallback)", "INFO")
      } else {
        add_viol("pct_impossible", "No se pudo identificar columnas para reconstruir pct_extranjeros", "FAIL")
      }
    } else {
      add_viol("pct_impossible", "No hay fichero dedicado ni poblaciones para derivarlo", "FAIL")
    }
  }
}

# ---------- NA críticos ----------
check_na <- function(df, cols, name){
  if (is.null(df)) return()
  bad <- cols[cols %in% names(df)] |> lapply(\(c) sum(is.na(df[[c]]))) |> unlist()
  bad <- bad[bad>0]
  if (length(bad)) add_viol("na_critical", sprintf("NAs en %s: %s", name,
                                                   paste(paste0(names(bad),"=",bad), collapse=", ")), "WARN")
}
check_na(core, c("tasa_hc_100k","tasa_he_100k","share_extranjeros","pib_pc_real","paro_joven","arope"), "core")

# ---------- Salidas ----------
viol_tbl <- purrr::map_dfr(viol, tibble::as_tibble)
if (!nrow(viol_tbl)) viol_tbl <- tibble(type="ok", message="Todo listo", severity="PASS")
write_csv(viol_tbl, file.path(out_dir, "20_io_violations.csv"))

summary_row <- viol_tbl %>%
  count(severity) %>%
  tidyr::pivot_wider(names_from=severity, values_from=n, values_fill=0) %>%
  mutate(timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
write_csv(summary_row, file.path(out_dir, "20_io_summary.csv"))

message("20_io_guard.R -> listo. Revisa output/tables/20_io_violations.csv y 20_io_summary.csv.")


# ---- 20_QC_INLINE_OUT ----
suppressPackageStartupMessages({ library(readr); library(dplyr); library(janitor); library(here); library(fs) })
.qco <- function(...) message("[20:QC-OUT] ", paste0(...))
out_dir <- here("output","tables"); fig_dir <- here("output","figures")

exp_files <- tibble::tibble(
  path = c(
    file.path(out_dir,"20_corr_niveles_all.csv"),
    file.path(out_dir,"20_corr_deltas_all.csv"),
    file.path(out_dir,"20_corr_niveles_sin_covid.csv"),
    file.path(out_dir,"20_corr_deltas_sin_covid.csv"),
    file.path(out_dir,"20_pb_summary.md")
  ),
  required = c(TRUE, TRUE, TRUE, TRUE, TRUE)
)

exists_out <- exp_files %>%
  mutate(exists = file.exists(path),
         n_rows = purrr::map_int(path, \(p) if (file.exists(p) && endsWith(p,".csv")) nrow(suppressMessages(readr::read_csv(p, show_col_types = FALSE))) else NA_integer_))

write_csv(exists_out, file.path(out_dir, "20_qc_outputs_overview.csv"))

# esquema mínimo para “corr_*”
check_cols <- c("x","y","pearson_r","pearson_p","n")
ok_schema <- TRUE
for (p in exp_files$path[grepl("corr_", exp_files$path)]) {
  if (file.exists(p)) {
    d <- suppressMessages(readr::read_csv(p, show_col_types = FALSE) |> janitor::clean_names())
    miss <- setdiff(check_cols, names(d))
    if (length(miss)) { ok_schema <- FALSE; writeLines(paste("Missing:", paste(miss, collapse=", ")), file.path(out_dir, paste0("SCHEMA_WARN_", basename(p), ".txt"))) }
  }
}

# recuento de figuras generadas
n_fig <- if (dir_exists(fig_dir)) length(fs::dir_ls(fig_dir, regexp = "20_(niv|del)_.*\\.png$")) else 0L
readr::write_lines(as.character(n_fig), file.path(out_dir, "20_qc_n_figures.txt"))

sev <- if (all(exists_out$exists[exists_out$required]) && ok_schema) "PASS" else "WARN"
.qco("QC-OUT listo (", sev, "). Revisa 20_qc_outputs_overview.csv y SCHEMA_WARN_*.txt si aparecen.")
# ---- /20_QC_INLINE_OUT ----

