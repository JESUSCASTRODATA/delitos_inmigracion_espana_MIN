#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
################################################################################
# 70_export_figs.R — Empaqueta figuras (ZIP) y PDFs (contact-sheet y combinado)
# Proyecto   : Delitos e Inmigración en España (2010–2023)
# Autor      : Jesús Castro · JESUSCASTRODATA
# Licencia   : MIT (código) · CC BY 4.0 (docs/figuras)
# Versión    : v2.1 (2025-10-06)
################################################################################

suppressPackageStartupMessages({
  library(fs); library(dplyr); library(purrr); library(stringr); library(rlang)
})

say <- function(...) cat("[70] ", paste0(...), "\n")

# --- raíz robusta: carpeta del script ---
get_thisfile <- function(){
  cmd <- commandArgs(trailingOnly = FALSE)
  i <- grep("^--file=", cmd)
  if (length(i)) return(normalizePath(sub("^--file=", "", cmd[i[1]])))
  if (!is.null(sys.frames()[[1]]$ofile)) return(normalizePath(sys.frames()[[1]]$ofile))
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    p <- tryCatch(rstudioapi::getActiveDocumentContext()$path, error = function(e) "")
    if (nzchar(p)) return(normalizePath(p))
  }
  normalizePath(".")
}
THISFILE <- get_thisfile()
ROOT     <- path_norm(path_dir(THISFILE) |> path(".."))
FIG_DIR  <- path(ROOT, "output", "figures")
REL_DIR  <- path(ROOT, "output", "release")
dir_create(REL_DIR)

say("ROOT   : ", ROOT)
say("FIG_DIR: ", FIG_DIR)
say("REL_DIR: ", REL_DIR)

if (!dir_exists(FIG_DIR)) {
  abort("[70] No existe la carpeta de figuras: ", FIG_DIR)
}

# --- recolectar figuras ---
# Incluye PNG y PDF por defecto; activa include_svg=TRUE si también quieres SVG.
include_svg <- FALSE
pat <- if (include_svg) "(?i)\\.(png|pdf|svg)$" else "(?i)\\.(png|pdf)$"
files <- dir_ls(FIG_DIR, regexp = pat, type = "file", recurse = FALSE)

if (!length(files)) {
  say("AVISO: no hay figuras en ", FIG_DIR)
  quit(save = "no")
}

# Orden alfabético, nombres relativos
files <- sort(files)
rel_names <- path_file(files)

# --- crear ZIP (preferir zip::zipr; fallback a utils::zip) ---
zip_path <- path(REL_DIR, "figs_all.zip")
if (file_exists(zip_path)) file_delete(zip_path)

zip_ok <- FALSE
if (requireNamespace("zip", quietly = TRUE)) {
  # zip::zipr permite pasar nombres destino con 'mode = "cherry-pick"'
  say("Creando ZIP con {zip} …")
  zip::zipr(zipfile = zip_path, files = files, root = FIG_DIR)
  zip_ok <- file_exists(zip_path)
} else {
  say("Paquete {zip} no disponible; intento con utils::zip …")
  owd <- setwd(FIG_DIR); on.exit(setwd(owd), add = TRUE)
  # utils::zip usa rutas relativas desde el wd
  utils::zip(zipfile = zip_path, files = rel_names)
  zip_ok <- file_exists(zip_path)
}
if (zip_ok) say("✔ ZIP → ", zip_path) else say("✖ No se pudo crear el ZIP.")

# --- Contact sheet (montaje de PNG) con {magick} opcional ---
pngs <- files[str_detect(files, "(?i)\\.png$")]
contact_pdf <- path(REL_DIR, "figs_contact_sheet.pdf")

if (length(pngs) && requireNamespace("magick", quietly = TRUE)) {
  say("Generando contact sheet (magick) …")
  imgs <- purrr::map(pngs, magick::image_read)
  
  # Fondo/blanco y padding per-imagen (compatibles con versiones antiguas de {magick})
  imgs <- purrr::map(imgs, ~ {
    .x <- magick::image_background(.x, color = "white", flatten = TRUE)
    magick::image_border(.x, color = "white", geometry = "15x15")
  })
  
  # Montaje 2 columnas; evita usar 'background=' (no está soportado en tu versión)
  montage <- magick::image_montage(do.call(c, imgs),
                                   tile = "2x",
                                   geometry = "1000x1000+10+10")
  
  magick::image_write(montage, path = contact_pdf, format = "pdf")
  say("✔ Contact sheet PDF → ", contact_pdf)
  
} else if (!length(pngs)) {
  say("Nota: no hay PNG; omito contact sheet.")
} else {
  say("Nota: instala {magick} para generar contact sheet.")
}

# --- PDF combinado de todas las figuras PDF con {pdftools} (opcional) ---
pdfs <- files[str_detect(files, "(?i)\\.pdf$")]
merged_pdf <- path(REL_DIR, "figs_merged.pdf")
if (length(pdfs) && requireNamespace("pdftools", quietly = TRUE)) {
  say("Combinando PDFs con {pdftools} …")
  # pdf_combine requiere rutas; produce un único PDF multi-página
  pdftools::pdf_combine(input = pdfs, output = merged_pdf)
  say("✔ PDF combinado → ", merged_pdf)
} else if (!length(pdfs)) {
  say("Nota: no hay PDFs individuales para combinar.")
} else {
  say("Nota: instala {pdftools} para combinar PDFs.")
}

# --- checksums SHA256 de entregables ---
deliveries <- c(if (file_exists(zip_path)) zip_path,
                if (file_exists(contact_pdf)) contact_pdf,
                if (file_exists(merged_pdf)) merged_pdf)

if (length(deliveries)) {
  sha <- map_chr(deliveries, ~ as.character(openssl::sha256(file(.x))))
  tibble::tibble(file = deliveries, sha256 = sha) |>
    readr::write_csv(path(REL_DIR, "checksums_sha256.csv"))
  say("✔ Checksums → ", path(REL_DIR, "checksums_sha256.csv"))
} else {
  say("Aviso: no hay entregables para calcular checksums.")
}

# --- mini check ---
cat("\n—— MINI CHECK · 70_export_figs ————————————————\n")
cat("Total figuras detectadas: ", length(files), "\n", sep = "")
print(tibble::tibble(ejemplos = head(rel_names, 6)), n = 6)
cat("ZIP: ", if (file_exists(zip_path)) "OK" else "NO", "\n", sep = "")
cat("Contact sheet PDF: ", if (file_exists(contact_pdf)) "OK" else "NO", "\n", sep = "")
cat("PDF combinado: ", if (file_exists(merged_pdf)) "OK" else "NO", "\n", sep = "")
cat("———————————————————————————————————————————————\n\n")
