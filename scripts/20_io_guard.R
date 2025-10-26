#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
################################################################################
# 20_io_guard.R — Contratos de entrada/salida para 20_preguntas_basicas.R
#
# Proyecto : Delitos e Inmigración en España (2010–2023)
# Autor    : Jesús Castro · JESUSCASTRODATA
# Fecha    : 2025-10-06
# Licencia : MIT (código) · CC BY 4.0 (docs/figuras)
#
# Descripción:
#   Verifica PRE-condiciones (inputs mínimos y esquema) para 20_preguntas_basicas.R
#   y POST-condiciones (salidas esperadas y esquema básico de correlaciones).
#   Emite tablas de QC en output/tables/ y aborta si faltan entradas críticas.
#
# Entradas requeridas:
#   - data/processed/core_indicadores.csv
#
# Entradas opcionales:
#   - data/processed/core_indicadores_derivadas.csv
#   - data/processed/pct_extranjeros_poblacion.csv
#   - data/processed/poblacion_extranjera_total.csv
#   - data/processed/poblacion_total_nacional.csv
#
# Salidas QC:
#   - output/tables/20_io_exists.csv
#   - output/tables/20_qc_inputs_overview.csv
#   - output/tables/20_schema_{core,der,px,ext,tot}.csv
#   - output/tables/20_preview_{core,der,px,ext,tot}.csv
#   - output/tables/20_io_violations.csv
#   - output/tables/20_io_summary.csv
#   - output/tables/20_qc_outputs_overview.csv
#   - output/tables/20_qc_n_figures.txt
#   - output/tables/SCHEMA_WARN_*.txt (si aplica)
################################################################################

suppressPackageStartupMessages({
  library(here); library(readr); library(dplyr); library(janitor)
  library(stringr); library(fs);   library(tidyr); library(purrr); library(vctrs)
})

YEAR_MIN <- 2010L; YEAR_MAX <- 2023L
out_dir  <- here("output","tables"); fs::dir_create(out_dir)

# ---------- Expectativas de ficheros ----------
req_files <- tibble::tibble(
  path = c(here("data","processed","core_indicadores.csv")),
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

exists_tbl <- function(df) df %>% mutate(exists = file.exists(path))
io_exists <- bind_rows(exists_tbl(req_files), exists_tbl(opt_files))
write_csv(io_exists, file.path(out_dir, "20_io_exists.csv"))

# ---------- Expectativas de columnas ----------
expect_core <- c(
  "ano",
  # Y en niveles
  "tasa_hc_100k","tasa_he_100k","share_extranjeros",
  # Controles en niveles
  "pib_pc_real","paro_joven","arope",
  # X principal (reconstruible si falta)
  "pct_extranjeros"
)

expect_der <- c(
  "ano",
  # control en Δ (mínimo)
  "dln_pib_pc",
  # Δ recomendadas
  "d_pct_extran","d_share_ext","d_tasa_hc_100k","d_tasa_he_100k"
)

# ---------- Lectura segura ----------
safe_read <- function(p) {
  if (!file.exists(p)) return(NULL)
  suppressMessages(readr::read_csv(p, show_col_types = FALSE) |> janitor::clean_names())
}

# ---------- QC-IN (entradas) ----------
.qc_msg <- function(...) message("[20:QC-IN] ", paste0(...))
.qc_rows <- function(x) if (is.null(x)) NA_integer_ else nrow(x)

qc_expect <- list(
  core = list(
    path = here("data","processed","core_indicadores.csv"),
    required = c("ano","tasa_hc_100k","tasa_he_100k","share_extranjeros",
                 "pib_pc_real","paro_joven","arope"),
    nice = c("pct_extranjeros")
  ),
  der = list(
    path = here("data","processed","core_indicadores_derivadas.csv"),
    optional = TRUE,
    required = c("ano","dln_pib_pc"),
    nice = c("d_pct_extran","d_share_ext","d_tasa_hc_100k","d_tasa_he_100k")
  ),
  px  = list(path = here("data","processed","pct_extranjeros_poblacion.csv"),       optional = TRUE),
  ext = list(path = here("data","processed","poblacion_extranjera_total.csv"),      optional = TRUE),
  tot = list(path = here("data","processed","poblacion_total_nacional.csv"),        optional = TRUE)
)

schema_df <- function(df) {
  if (is.null(df)) return(tibble(col = character(), type = character(), n_na = integer()))
  tibble(
    col  = names(df),
    type = map_chr(df, vctrs::vec_ptype_full),  # FIX: por columna
    n_na = vapply(df, function(x) sum(is.na(x)), integer(1))
  )
}

qc_tbl <- tibble(file = character(), exists = logical(), n_rows = integer(),
                 missing_required = character(), year_coverage = character())
for (nm in names(qc_expect)) {
  spec <- qc_expect[[nm]]
  p <- spec$path; ex <- file.exists(p)
  df <- safe_read(p)
  miss <- character()
  if (ex && !is.null(spec$required)) miss <- setdiff(spec$required, names(df))
  yrs <- if (ex && "ano" %in% names(df)) sort(unique(df$ano)) else integer()
  cov <- if (length(yrs)) sprintf("%s … %s (n=%d)", min(yrs), max(yrs), length(yrs)) else "N/D"
  qc_tbl <- bind_rows(qc_tbl,
                      tibble(file = nm, exists = ex, n_rows = .qc_rows(df),
                             missing_required = paste(miss, collapse="|"),
                             year_coverage = cov))
  if (ex) {
    write_csv(schema_df(df), here("output","tables", paste0("20_schema_", nm, ".csv")))
    write_csv(utils::head(df, 10), here("output","tables", paste0("20_preview_", nm, ".csv")))
  }
}
write_csv(qc_tbl, here("output","tables","20_qc_inputs_overview.csv"))

# Semáforo de entradas
sev <- "PASS"
if (!qc_tbl$exists[qc_tbl$file=="core"]) { .qc_msg("Falta core_indicadores.csv → FAIL"); sev <- "FAIL" }
if (qc_tbl$exists[qc_tbl$file=="core"] && nzchar(qc_tbl$missing_required[qc_tbl$file=="core"])) {
  .qc_msg("Core sin columnas clave: ", qc_tbl$missing_required[qc_tbl$file=="core"]); if (sev=="PASS") sev <- "WARN"
}
if (sev=="FAIL") stop("[20:QC-IN] Entradas críticas ausentes. Revisa 20_qc_inputs_overview.csv")
.qc_msg("QC-IN listo (", sev, ").")

# ---------- Derivadas disponibles para decidir si reportar Δ faltantes ----------
der_path <- here("data","processed","core_indicadores_derivadas.csv")
der <- safe_read(der_path)

have_deltas_in_der <- !is.null(der) && all(c("d_tasa_hc_100k","d_tasa_he_100k") %in% names(der))

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
  
  # Δ solo informar si TAMPOCO están en derivadas
  for (dname in c("d_tasa_hc_100k","d_tasa_he_100k")) {
    if (!dname %in% names(core) && !(!is.null(der) && dname %in% names(der))) {
      add_viol("delta_missing",
               sprintf("Δ faltante en core (%s). Se podrá construir on-the-fly, mejor añadirlo en derivadas.", dname),
               "INFO")
    }
  }
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

# ---------- Salidas QC (violaciones + resumen) ----------
viol_tbl <- purrr::map_dfr(viol, tibble::as_tibble)
if (!nrow(viol_tbl)) viol_tbl <- tibble(type="ok", message="Todo listo", severity="PASS")
write_csv(viol_tbl, file.path(out_dir, "20_io_violations.csv"))

summary_row <- viol_tbl %>%
  count(severity) %>%
  tidyr::pivot_wider(names_from=severity, values_from=n, values_fill=0) %>%
  mutate(timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
write_csv(summary_row, file.path(out_dir, "20_io_summary.csv"))

message("20_io_guard.R -> listo. Revisa output/tables/20_io_violations.csv y 20_io_summary.csv.")

# ---------- QC-OUT (post-condiciones) ----------
.qco <- function(...) message("[20:QC-OUT] ", paste0(...))
fig_dir <- here("output","figures")

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
  mutate(
    exists = file.exists(path),
    n_rows = purrr::map_int(path, \(p)
                            if (file.exists(p) && endsWith(p,".csv"))
                              nrow(suppressMessages(readr::read_csv(p, show_col_types = FALSE)))
                            else NA_integer_)
  )
write_csv(exists_out, file.path(out_dir, "20_qc_outputs_overview.csv"))

# Esquema mínimo esperado para corr_*
check_cols <- c("x","y","pearson_r","pearson_p","n")
ok_schema <- TRUE
for (p in exp_files$path[grepl("corr_", exp_files$path)]) {
  if (file.exists(p)) {
    d <- suppressMessages(readr::read_csv(p, show_col_types = FALSE) |> janitor::clean_names())
    miss <- setdiff(check_cols, names(d))
    if (length(miss)) {
      ok_schema <- FALSE
      warn_file <- file.path(out_dir, paste0("SCHEMA_WARN_", basename(p), ".txt"))
      writeLines(paste("Missing:", paste(miss, collapse=", ")), warn_file)
    }
  }
}

# Recuento de figuras generadas (prefijo 20_)
n_fig <- if (dir_exists(fig_dir)) length(fs::dir_ls(fig_dir, regexp = "20_(niv|del)_.*\\.png$")) else 0L
readr::write_lines(as.character(n_fig), file.path(out_dir, "20_qc_n_figures.txt"))

sev_out <- if (all(exists_out$exists[exists_out$required]) && ok_schema) "PASS" else "WARN"
.qco("QC-OUT listo (", sev_out, "). Revisa 20_qc_outputs_overview.csv y SCHEMA_WARN_*.txt si aparecen.")

# Echo de resumen al final
print(readr::read_csv(file.path(out_dir, "20_io_summary.csv"), show_col_types = FALSE))







mini_check_20 <- function(strict = FALSE,
                          year_min = 2010L, year_max = 2023L,
                          out_dir = here::here("output","tables")) {
  suppressPackageStartupMessages({library(readr); library(dplyr); library(janitor); library(here)})
  
  cat("\n—— MINI CHECK 20 ————————————————————————————————\n")
  
  vio_path <- file.path(out_dir, "20_io_violations.csv")
  ovi_in   <- file.path(out_dir, "20_qc_inputs_overview.csv")
  ovi_out  <- file.path(out_dir, "20_qc_outputs_overview.csv")
  nfig_p   <- file.path(out_dir, "20_qc_n_figures.txt")
  
  vio <- if (file.exists(vio_path)) suppressMessages(read_csv(vio_path, show_col_types = FALSE)) else NULL
  ov_in <- if (file.exists(ovi_in))  suppressMessages(read_csv(ovi_in,  show_col_types = FALSE)) else NULL
  ov_ou <- if (file.exists(ovi_out)) suppressMessages(read_csv(ovi_out, show_col_types = FALSE)) else NULL
  nfig  <- if (file.exists(nfig_p))  suppressWarnings(as.integer(readr::read_lines(nfig_p, n_max = 1))) else NA_integer_
  
  if (!is.null(ov_in)) {
    core_row <- ov_in %>% dplyr::filter(file == "core")
    cat("Entradas → core_indicadores.csv: ",
        if (nrow(core_row) && isTRUE(core_row$exists[1])) "OK" else "FALTA", "\n", sep = "")
    if (nrow(core_row)) {
      mr <- core_row$missing_required[1]
      if (!is.null(mr) && !is.na(mr) && nzchar(mr)) {
        cat("  Faltan columnas core: ", mr, "\n", sep = "")
      }
    }
    cat("Resumen inputs (existen/n_rows):\n")
    print(ov_in %>% dplyr::select(file, exists, n_rows, year_coverage), n = nrow(ov_in))
  } else cat("No se encontró 20_qc_inputs_overview.csv\n")
  
  
  if (!is.null(ov_ou)) {
    cat("\nSalidas esperadas (corr*/md):\n")
    print(ov_ou %>% select(path, exists, n_rows), n = nrow(ov_ou))
    cat("Figuras 20_* detectadas: ", ifelse(is.na(nfig), "N/D", nfig), "\n", sep = "")
  } else cat("\nNo se encontró 20_qc_outputs_overview.csv\n")
  
  core_p <- here::here("data","processed","core_indicadores.csv")
  if (file.exists(core_p)) {
    core <- suppressMessages(read_csv(core_p, show_col_types = FALSE) %>% clean_names())
    yrs  <- sort(unique(core$ano))
    miss <- setdiff(seq.int(year_min, year_max), yrs)
    cat("\nCobertura core: { ", paste(yrs, collapse = ", "), " }\n", sep = "")
    if (length(miss) == 0) cat("✔ Cobertura contigua ", year_min, "–", year_max, ".\n", sep = "")
    else cat("✖ Cobertura incompleta. Faltan: ", paste(miss, collapse = ", "), "\n", sep = "")
    
    quick_vars <- c("tasa_hc_100k","tasa_he_100k","share_extranjeros","pct_extranjeros","pib_pc_real","arope","paro_joven")
    quick_vars <- intersect(quick_vars, names(core))
    if (length(quick_vars)) {
      cat("Rangos rápidos (min..max):\n")
      rng <- lapply(quick_vars, function(v){
        x <- core[[v]]
        c(min = suppressWarnings(min(x, na.rm = TRUE)),
          max = suppressWarnings(max(x, na.rm = TRUE)),
          n_na = sum(is.na(x)))
      })
      df_rng <- do.call(rbind, rng) %>% as.data.frame()
      df_rng$var <- quick_vars
      df_rng <- df_rng %>% select(var, min, max, n_na)
      print(df_rng, row.names = FALSE)
    }
  } else {
    cat("\nNo existe core_indicadores.csv (no se pueden hacer checks de cobertura/columnas).\n")
  }
  
  if (!is.null(vio)) {
    tally <- vio %>% count(severity) %>% tidyr::pivot_wider(names_from = severity, values_from = n, values_fill = 0)
    cat("\nViolaciones registradas (por severidad):\n")
    print(tally)
    if (nrow(vio)) {
      cat("Últimas 5:\n")
      print(utils::head(vio %>% select(severity, type, message), 5), row.names = FALSE)
    }
    if (isTRUE(strict)) {
      if ("FAIL" %in% names(tally) && tally$FAIL[1] > 0) stop("mini_check_20(strict=TRUE): FAIL detectado.")
    }
  } else cat("\nNo se encontró 20_io_violations.csv\n")
  
  cat("———————————————————————————————————————————————\n\n")
  invisible(TRUE)
}
# Auto-run si se ejecuta por source() en interactivo
if (interactive() && sys.nframe() == 0) {
  mini_check_20(strict = FALSE, out_dir = here::here("output","tables"))
}

