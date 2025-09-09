# -*- coding: UTF-8 -*-
###############################################################################
# 80_run_all.R — Orquestación completa del pipeline (con ADF/PP + FDR + render)
# Autor: JESUS CASTRO
# Fecha: 2025-09-07
###############################################################################

RUN_BUILD            <- TRUE   # 20b_build_series_csv.R
RUN_STATIONARITY     <- TRUE   # 18_stationarity_checks.R
RUN_FIG_TRENDS       <- TRUE   # 22_figuras_tendencias.R
RUN_FIG_BASE100      <- TRUE   # 28_figuras_base100_correlaciones.R
RUN_CORR_DEBUG       <- TRUE   # 23c_check_correlaciones_hc_pobl_ext.R   
RUN_CORR_HEATMAP     <- TRUE   # 28c_make_heatmap_correlaciones.R        
RUN_VAR_GRANGER      <- TRUE   # 24_var_granger_pipeline.R
RUN_GRANGER_FDR      <- TRUE   # 24c_granger_fdr.R
RUN_FINAL_FIGS       <- TRUE   # 29_figuras_resultados_finales.R
RENDER_RMD           <- TRUE   # Render del informe al final 

# Informe por defecto (ajusta si usas el otro nombre)
RMD_PATH <- "docs/FINAL_Informe_TFG_Delitos_Inmigracion_ES_2010_2023.Rmd"
ALT_RMD_PATH <- "docs/informe_delitos_inmigracion_espana_2010_2023.Rmd"

options(stringsAsFactors = FALSE, scipen = 6, warn = 1)
Sys.setlocale(category = "LC_TIME", locale = "C")

suppressPackageStartupMessages({ library(here); library(fs) })

ensure_dir <- function(path) if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
logdir <- here::here("output","logs"); ensure_dir(logdir)
logfile <- here::here("output","logs","80_run_all_summary.txt")

ts <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")
say <- function(...) cat(sprintf("[%s] ", ts()), ..., "\n", sep="")
append_log <- function(...) { cat(sprintf("[%s] ", ts()), ..., "\n", sep = "", file = logfile, append = TRUE) }

need_pkgs <- c(
  "readr","dplyr","tidyr","ggplot2","scales","here","janitor","fs","stringr","patchwork",
  "vars","urca","lmtest","rmarkdown","png","gt"   # gt para matrices/HTML de correlaciones
)
check_packages <- function(pkgs) {
  miss <- pkgs[!vapply(pkgs, requireNamespace, FUN.VALUE = logical(1), quietly = TRUE)]
  if (length(miss)) {
    say("Faltan paquetes: ", paste(miss, collapse=", "))
    append_log("Faltan paquetes: ", paste(miss, collapse=", "))
    say("Instálalos con: install.packages(c(", paste(sprintf('\"%s\"', miss), collapse=", "), "))")
  }
  invisible(miss)
}

# -------- Fallback CSL si falta docs/apa.csl (para el render del Rmd) --------
ensure_csl <- function() {
  csl_path <- here::here("docs","apa.csl")
  if (!file.exists(csl_path)) {
    dir.create(dirname(csl_path), showWarnings = FALSE, recursive = TRUE)
    csl_xml <- c(
      '<?xml version="1.0" encoding="utf-8"?>',
      '<style xmlns="http://purl.org/net/xbiblio/csl" version="1.0.1" class="in-text" default-locale="en-US">',
      '  <info>',
      '    <title>APA Lite (Fallback)</title>',
      '    <id>http://example.org/styles/apa-lite-fallback</id>',
      '    <link href="http://example.org/styles/apa-lite-fallback" rel="self"/>',
      '    <author><name>Generated Fallback</name></author>',
      '    <category citation-format="author-date"/>',
      '    <updated>2025-09-07T00:00:00Z</updated>',
      '  </info>',
      '  <macro name="author">',
      '    <names variable="author">',
      '      <name form="short" and="symbol" delimiter=", " initialize-with="."/>',
      '      <label form="short" prefix=" " text-case="lowercase" strip-periods="true"/>',
      '      <substitute><names variable="editor"/><names variable="translator"/><text variable="title"/></substitute>',
      '    </names>',
      '  </macro>',
      '  <macro name="year"><date variable="issued"><date-part name="year"/></date></macro>',
      '  <macro name="title"><text variable="title"/></macro>',
      '  <macro name="container">',
      '    <group delimiter=", ">',
      '      <text variable="container-title" font-style="italic"/>',
      '      <group><text variable="volume" font-style="italic"/><text variable="issue" prefix="(" suffix=")"/></group>',
      '      <text variable="page"/><text variable="publisher"/><text variable="publisher-place"/>',
      '    </group>',
      '  </macro>',
      '  <citation disambiguate-add-year-suffix="true" collapse="year-suffix">',
      '    <layout prefix="(" suffix=")"" delimiter="; ">',
      '      <group delimiter=", ">',
      '        <text macro="author"/>',
      '        <text macro="year"/>',
      '        <text variable="locator"/>',
      '      </group>',
      '    </layout>',
      '  </citation>',
      '  <bibliography hanging-indent="true">',
      '    <sort><key macro="author"/><key variable="issued"/></sort>',
      '    <layout suffix="."">',
      '      <group delimiter=". ">',
      '        <group delimiter=" "><text macro="author"/><text macro="year" prefix="(" suffix=")"/></group>',
      '        <text macro="title"/>',
      '        <text macro="container"/>',
      '        <text variable="DOI" prefix="https://doi.org/"/>',
      '        <text variable="URL"/>',
      '      </group>',
      '    </layout>',
      '  </bibliography>',
      '</style>'
    )
    writeLines(csl_xml, csl_path, useBytes = TRUE)
    say("'docs/apa.csl' no existía; se creó un estilo APA básico.")
  }
}

run_step <- function(label, fun) {
  say(">> ", label, " ..."); append_log("BEGIN ", label); t0 <- Sys.time(); ok <- TRUE; err_msg <- NULL
  tryCatch({ fun() }, error = function(e) { ok <<- FALSE; err_msg <<- conditionMessage(e) })
  dur <- as.numeric(difftime(Sys.time(), t0, units="secs"))
  if (ok) { say("OK ", label, " (", sprintf("%.1fs", dur), ")"); append_log("END ", label, " OK in ", sprintf("%.1fs", dur)) }
  else    { say("ERROR ", label, " — ", err_msg); append_log("END ", label, " FAIL in ", sprintf("%.1fs", dur), " — ", err_msg) }
  invisible(ok)
}

list_outputs <- function() {
  figs <- tryCatch(fs::dir_ls(here::here("output","figures"), recurse = FALSE, type = "file"), error=function(e) character())
  tbls <- tryCatch(fs::dir_ls(here::here("output","tables"),  recurse = FALSE, type = "file"), error=function(e) character())
  say(sprintf("Resumen de outputs — Figuras: %d, Tablas: %d", length(figs), length(tbls)))
  append_log(sprintf("Figuras: %d", length(figs))); append_log(sprintf("Tablas: %d", length(tbls)))
  if (length(figs)) say("  Ejemplos figuras:\n   - ", paste(utils::head(basename(figs), 5), collapse="\n   - "))
  if (length(tbls)) say("  Ejemplos tablas:\n   - ", paste(utils::head(basename(tbls), 5), collapse="\n   - "))
}

miss <- check_packages(need_pkgs)
if (length(miss)) { say("Abortado por paquetes faltantes."); append_log("ABORT missing pkgs"); quit(status=1, save="no") }

ok_all <- TRUE

if (RUN_BUILD)            ok_all <- ok_all && run_step("1) Build series (20b)",               function() source(here::here("scripts","20b_build_series_csv.R"), local=TRUE))
if (RUN_STATIONARITY)     ok_all <- ok_all && run_step("2) Estacionariedad (18)",             function() source(here::here("scripts","18_stationarity_checks.R"), local=TRUE))
if (RUN_FIG_TRENDS)       ok_all <- ok_all && run_step("3) Figuras tendencias (22)",          function() source(here::here("scripts","22_figuras_tendencias.R"), local=TRUE))
if (RUN_FIG_BASE100)      ok_all <- ok_all && run_step("4) Base-100 + correlaciones (28)",    function() source(here::here("scripts","28_figuras_base100_correlaciones.R"), local=TRUE))

# Nuevos pasos: correlaciones HC vs población extranjera y heatmaps
if (RUN_CORR_DEBUG)       ok_all <- ok_all && run_step("4.1) Correlaciones HC vs Población extranjera (23c)",
                                                       function() source(here::here("scripts","23c_check_correlaciones_hc_pobl_ext.R"), local=TRUE))

if (RUN_CORR_HEATMAP)     ok_all <- ok_all && run_step("4.2) Heatmaps correlaciones (28c)",
                                                       function() source(here::here("scripts","28c_make_heatmap_correlaciones.R"), local=TRUE))

if (RUN_VAR_GRANGER)      ok_all <- ok_all && run_step("5) VAR/Granger + IRFs (24)",          function() source(here::here("scripts","24_var_granger_pipeline.R"), local=TRUE))
if (RUN_GRANGER_FDR)      ok_all <- ok_all && run_step("6) FDR y resumen Granger (24c)",      function() source(here::here("scripts","24c_granger_fdr.R"), local=TRUE))
if (RUN_FINAL_FIGS)       ok_all <- ok_all && run_step("7) Figuras finales (29)",             function() source(here::here("scripts","29_figuras_resultados_finales.R"), local=TRUE))

# Render Rmd (intenta RMD_PATH; si no existe, prueba ALT_RMD_PATH)
if (RENDER_RMD) {
  target_rmd <- if (file.exists(here::here(RMD_PATH))) here::here(RMD_PATH) else if (file.exists(here::here(ALT_RMD_PATH))) here::here(ALT_RMD_PATH) else NA_character_
  if (!is.na(target_rmd)) {
    ensure_csl()
    ok_all <- ok_all && run_step("8) Render informe Rmd", function() rmarkdown::render(target_rmd, envir = new.env()))
  } else {
    say("Rmd no encontrado en: ", here::here(RMD_PATH), " ni en: ", here::here(ALT_RMD_PATH))
    append_log("Rmd no encontrado.")
  }
}

list_outputs()

if (ok_all) {
  say("Pipeline completado sin errores críticos.")
  append_log("DONE OK")
} else {
  say("Pipeline completado con errores.")
  append_log("DONE with errors")
}

say("Log de esta ejecución: ", logfile)
