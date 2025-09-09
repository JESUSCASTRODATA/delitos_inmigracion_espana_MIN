#!/usr/bin/env Rscript
suppressPackageStartupMessages({ library(here) })

# --- deps mínimas ---
need <- setdiff(c("rmarkdown","knitr"), rownames(installed.packages()))
if (length(need)) install.packages(need, repos="https://cloud.r-project.org")
suppressPackageStartupMessages({ library(rmarkdown); library(knitr) })

# --- opciones seguras de render ---
options(bitmapType = "cairo")
Sys.setenv(R_DEFAULT_DEVICE = "png", RGL_USE_NULL = "true", NO_BROWSER = "1")
knitr::opts_chunk$set(dev = "png", dev.args = list(type = "cairo"), dpi = 150, fig.retina = 1)

# --- utilidades ---
msg <- function(...) cat(sprintf(...), "\n")

find_rmd <- function() {
  # 1) CLI arg
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) >= 1 && file.exists(args[1])) return(normalizePath(args[1], winslash = "/"))
  # 2) ENV
  envp <- Sys.getenv("REPORT_RMD", unset = "")
  if (nzchar(envp) && file.exists(envp)) return(normalizePath(envp, winslash = "/"))
  # 3) Candidatos comunes
  cands <- c(
    here::here("docs","Delitos_Inmigracion_Espana_2010_2023.Rmd"),
    here::here("Delitos_Inmigracion_Espana_2010_2023.Rmd")
  )
  hits <- cands[file.exists(cands)]
  if (length(hits)) return(normalizePath(hits[1], winslash = "/"))
  # 4) Primer .Rmd en docs/ o raíz
  all <- c(list.files(here::here("docs"), pattern="\\.Rmd$", full.names=TRUE),
           list.files(here::here(),       pattern="\\.Rmd$", full.names=TRUE))
  if (length(all)) return(normalizePath(all[1], winslash = "/"))
  return(NA_character_)
}

write_skeleton_rmd <- function(path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  lines <- c(
    "---",
    'title: "Delitos e inmigración en España (2010–2023)"',
    'author: "Equipo de análisis"',
    'date: "`r format(Sys.Date(), \\"%d %B %Y\\")`"',
    "output:",
    "  html_document:",
    "    toc: true",
    "    toc_float: true",
    "    number_sections: true",
    "    df_print: paged",
    "  pdf_document: default",
    "fontsize: 11pt",
    "geometry: margin=1in",
    "---",
    "",
    "```{r setup, include=FALSE}",
    "knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)",
    "library(readr); library(dplyr); library(here); library(ggplot2); library(knitr)",
    "fig_dir  <- here('output','figures')",
    "tab_dir  <- here('output','tables')",
    "inc_if <- function(fp, fun){ if (file.exists(fp)) fun(fp) }",
    "```",
    "",
    "# Resumen ejecutivo",
    "",
    "- Ventanas analizadas: 2010–2023, 2010–2019, 2020–2023, y **sin COVID** (excluye 2020–2021).",
    "- Este informe se genera automáticamente desde el pipeline (scripts 24–34).",
    "",
    "# Series base-100",
    "```{r}",
    "inc_if(file.path(fig_dir,'28_base100_sin_covid.png'), function(x) { knitr::include_graphics(x) })",
    "inc_if(file.path(fig_dir,'28_base100_with_covid.png'), function(x) { knitr::include_graphics(x) })",
    "```",
    "",
    "# VAR/Granger",
    "```{r}",
    "gr <- tryCatch(readr::read_csv(file.path(tab_dir,'24_var_granger_resultados.csv'), show_col_types = FALSE), error=function(e) NULL)",
    "if (!is.null(gr)) {",
    "  grs <- gr %>% mutate(robust = !is.na(stable) & stable & is.finite(serial_p_value) & serial_p_value >= 0.05 & n >= 8,",
    "                       X_causes_Y = is.finite(p_y_on_x) & p_y_on_x < 0.05) %>%",
    "             group_by(policy, window, y) %>% summarise(n_sig = sum(robust & X_causes_Y), .groups='drop')",
    "  knitr::kable(grs, caption='Granger robusto (X⇒Y, α=0.05) por política/ventana/Y')",
    "}",
    "```",
    "",
    "## IRFs (mosaico)",
    "```{r}",
    "inc_if(file.path(fig_dir,'29_irf_grid.png'), function(x) { knitr::include_graphics(x) })",
    "inc_if(file.path(fig_dir,'29_irf_grid_keep.png'), function(x) { knitr::include_graphics(x) })",
    "inc_if(file.path(fig_dir,'29_irf_grid_remove.png'), function(x) { knitr::include_graphics(x) })",
    "```",
    "",
    "# Coefplots dlog y ECM",
    "```{r}",
    "inc_if(file.path(fig_dir,'29_coefplot_dlog.png'), function(x) { knitr::include_graphics(x) })",
    "inc_if(file.path(fig_dir,'29_ecm_alpha.png'),     function(x) { knitr::include_graphics(x) })",
    "```",
    "",
    "# Tablas de apoyo",
    "```{r}",
    "for (nm in c('29_resumen_signos.csv','25_ecm_coefs.csv','28_correlaciones.csv','28_base100_series.csv')){",
    "  fp <- file.path(tab_dir, nm);",
    "  if (file.exists(fp)) {",
    "    tb <- readr::read_csv(fp, show_col_types = FALSE);",
    "    cat('## ', nm, '\\n\\n', sep='');",
    "    print(knitr::kable(head(tb, 30)));",
    "    cat('\\n\\n');",
    "  }",
    "}",
    "```",
    "",
    "# Notas metodológicas",
    "",
    "- Colores/tema: *Okabe–Ito* + fondo perla (`00_theme_figuras.R`).",
    "- Exclusión de COVID: ventanas *sin_covid* excluyen 2020–2021.",
    "- ECM: se reporta α del término de corrección (`ecm1`).",
    ""
  )
  writeLines(lines, path, useBytes = TRUE)
}

# --- localizar o crear Rmd ---
rmd <- find_rmd()
if (is.na(rmd)) {
  rmd <- here::here("docs","Delitos_Inmigracion_Espana_2010_2023.Rmd")
  msg("No había Rmd. Creo plantilla en: %s", rmd)
  write_skeleton_rmd(rmd)
} else {
  msg("Usando Rmd: %s", rmd)
}

# --- render ---
out_dir <- dirname(rmd)
dir.create(here::here("output","knit"), showWarnings = FALSE, recursive = TRUE)

# Siempre HTML
html_ok <- TRUE
try({
  rmarkdown::render(
    input = rmd,
    output_format = "html_document",
    output_dir = out_dir,
    intermediates_dir = here::here("output","knit"),
    clean = TRUE,
    envir = new.env(),
    encoding = "UTF-8"
  )
}, silent = FALSE)

# PDF solo si LaTeX disponible
pdf_try <- FALSE
pdf_ok <- FALSE
if (requireNamespace("tinytex", quietly = TRUE) && tinytex::is_tinytex()) {
  pdf_try <- TRUE
  msg("TinyTeX detectado: intento PDF…")
  try({
    rmarkdown::render(
      input = rmd,
      output_format = "pdf_document",
      output_dir = out_dir,
      intermediates_dir = here::here("output","knit"),
      clean = TRUE,
      envir = new.env(),
      encoding = "UTF-8"
    )
    pdf_ok <- TRUE
  }, silent = TRUE)
} else {
  msg("No hay TinyTeX/LaTeX: omito PDF (solo HTML). Si quieres PDF: tinytex::install_tinytex().")
}

msg("✅ Render terminado. HTML en: %s", file.path(out_dir, sub("\\\\.[Rr]md$", ".html", basename(rmd))))
if (pdf_try) msg("PDF: %s", if (pdf_ok) "OK" else "fallido")
