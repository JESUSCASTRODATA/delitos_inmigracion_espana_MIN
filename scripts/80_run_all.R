#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
# =============================================================================
# Script     : 80_run_all.R
# Proyecto   : Delitos e Inmigración en España (2010–2023)
# Autor      : Jesús Castro · JESUSCASTRODATA
# Versión    : v3.0 (2025-10-27)
# Licencia   : MIT (código) · CC BY 4.0 (documentación y figuras)
# Descripción: Ejecuta el pipeline completo en orden lógico y seguro.
#              Incluye validaciones, control de errores, logs y cronómetro.
# Entradas   : scripts/xx_*.R
# Salidas    : output/logs/80_run_all.log (+ logs de scripts individuales)
# =============================================================================

suppressPackageStartupMessages({
  library(here)
  library(fs)
  library(glue)
  library(crayon)
})

# -----------------------------------------------------------------------------
# FUNCIONES AUXILIARES
# -----------------------------------------------------------------------------

run_script <- function(script_path) {
  start_time <- Sys.time()
  cat(blue$bold(glue("\n▶ Ejecutando {basename(script_path)} ...\n")))
  flush.console()
  
  tryCatch({
    source(script_path, local = TRUE)
    elapsed <- round(difftime(Sys.time(), start_time, units = "secs"), 1)
    cat(green$bold(glue("✔ {basename(script_path)} completado ({elapsed}s)\n")))
    TRUE
  },
  error = function(e) {
    cat(red$bold(glue("✖ Error en {basename(script_path)}: {e$message}\n")))
    return(FALSE)
  })
}

# -----------------------------------------------------------------------------
# INICIALIZACIÓN GLOBAL
# -----------------------------------------------------------------------------

log_dir <- here("output", "logs")
dir_create(log_dir)
log_file <- path(log_dir, "80_run_all.log")

sink(log_file, append = FALSE, split = TRUE)
cat(glue("=== PIPELINE EJECUCIÓN COMPLETA — {Sys.time()} ===\n"))

start_global <- Sys.time()
setwd(here())

# -----------------------------------------------------------------------------
# ORDEN DE EJECUCIÓN (puede ajustarse si cambian dependencias)
# -----------------------------------------------------------------------------

scripts_order <- c(
  # 00s — configuración
  "00_paths_config.R",
  "00_theme_figuras.R",
  "00_utils_limpieza.R",
  "01_validacion_raw.R",
  "02_config_diccionarios_y_excepciones.R",
  "03_qa_global_integridad.R",
  
  # 10–19 — limpieza y métricas
  "10_limpieza_arope.R",
  "11_build_paro_16_29.R",
  "12_build_poblacion.R",
  "13_build_hechos_conocidos.R",
  "14_build_hechos_esclarecidos.R",
  "14a_check_mapping_coverage.R",
  "15_limpieza_detenciones_totales.R",
  "16_limpieza_detenciones_extranjeros.R",
  "17_pib_pc_real_eurostat.R",
  "17_controls_merge.R",
  "18_gini_opcional.R",
  "18_merge_core_pib.R",
  "18b_check_tasas.R",
  "19_qc_core_indicadores.R",
  "19_transformaciones.R",
  
  # 20–29 — análisis descriptivo
  "20_preguntas_basicas.R",
  "20a_audit_core_y_derivadas.R",
  "20b_sync_core_from_inventory.R",
  "21_correlaciones_niveles_y_deltas.R",
  "22_figuras_tendencias_y_tasas.R",
  "23_figuras_correlaciones.R",
  "24_var_granger_bivar.R",
  "26_plot_granger_graph.R",
  "26a_plot_granger_delta.R",
  "31_top5_violentos_vs_extranjeros.R",
  "32_compare_violentos_core.R",
  
  # 50–59 — robustez y econometría
  "55_toda_yamamoto_tests.R",
  "56_55_export_resumen.R",
  "57_plot_granger_toda.R",
  
  # 70+ — exportación final
  "70_export_figs.R"
)

# -----------------------------------------------------------------------------
# EJECUCIÓN SECUENCIAL
# -----------------------------------------------------------------------------

results <- logical(length(scripts_order))
names(results) <- scripts_order

for (i in seq_along(scripts_order)) {
  script_file <- here("scripts", scripts_order[i])
  if (!file_exists(script_file)) {
    cat(yellow$bold(glue("⚠ Omitido (no encontrado): {scripts_order[i]}\n")))
    results[i] <- NA
    next
  }
  results[i] <- run_script(script_file)
}

# -----------------------------------------------------------------------------
# RESUMEN FINAL
# -----------------------------------------------------------------------------

elapsed_global <- round(difftime(Sys.time(), start_global, units = "mins"), 1)
ok <- sum(results, na.rm = TRUE)
fail <- sum(results == FALSE, na.rm = TRUE)
miss <- sum(is.na(results))

cat("\n───────────────────────────────────────────────\n")
cat(glue("EJECUCIÓN FINALIZADA — {Sys.time()}\n"))
cat(glue("✔ Éxitos: {ok}  ✖ Errores: {fail}  ⚠ Omitidos: {miss}\n"))
cat(glue("Duración total: {elapsed_global} minutos\n"))
cat("Log completo: output/logs/80_run_all.log\n")
cat("───────────────────────────────────────────────\n")

sink()  # cerrar log

# -----------------------------------------------------------------------------
# SALIDA DE ESTADO
# -----------------------------------------------------------------------------
if (fail > 0) {
  stop(glue("Se detectaron {fail} errores. Revisar el log."), call. = FALSE)
} else {
  cat(green$bold("\n✅ Pipeline completado sin errores.\n"))
}
