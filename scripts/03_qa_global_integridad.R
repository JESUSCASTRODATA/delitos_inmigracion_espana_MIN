#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
################################################################################
# Proyecto : Delitos e Inmigración en España (2010–2023)
# Script   : 03_qa_global_integridad.R
# Título   : QA integral de exportados (coherencia y consistencia)
# Autor    : Jesús Castro · JESUSCASTRODATA
# Licencia : MIT (código) · CC BY 4.0 (docs/figuras)
# Fecha    : 2025-09-22
################################################################################

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(tidyr); library(stringr)
  library(janitor); library(here); library(purrr); library(tibble); library(fs)
})

# ───────────────────────── Utils / rutas ─────────────────────────
here_path <- function(...) here::here(...)
source_if <- function(path) { if (file.exists(path)) source(path, local = TRUE) }

# utilidades mínimas si no están cargadas desde 00_utils_limpieza.R
safe_read_csv <- function(path) {
  if (!file.exists(path)) return(tibble::tibble())
  df <- suppressMessages(readr::read_csv(path, show_col_types = FALSE))
  janitor::clean_names(df)
}
write_clean <- function(df, path, na="") {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(df, path, na = na)
  message("Escrito: ", normalizePath(path, winslash="/", mustWork = FALSE))
  invisible(path)
}
extract_year <- function(x) suppressWarnings(as.integer(stringr::str_extract(as.character(x), "\\d{4}")))
sfnum <- function(x){
  x <- gsub("\\.", "", x); x <- gsub(",", ".", x); x <- gsub("[^0-9\\-\\.]", "", x)
  suppressWarnings(as.numeric(x))
}
read_raw <- function(path, delim=";") {
  readr::read_delim(path, delim = delim, col_types = readr::cols(.default = readr::col_character()),
                    show_col_types = FALSE, trim_ws = TRUE) |>
    janitor::clean_names()
}

# Si tienes utilidades globales, cárgalas (no pasa nada si faltan)
source_if(here_path("scripts","00_utils_limpieza.R"))

YEAR_MIN <- 2010L; YEAR_MAX <- 2023L
qa_dir   <- here_path("output","tables"); dir.create(qa_dir, recursive = TRUE, showWarnings = FALSE)
cfg_dir  <- here_path("config"); raw_dir <- here_path("data","raw"); proc_dir <- here_path("data","processed")
diag_dir <- here_path("diagnostics"); dir.create(diag_dir, recursive = TRUE, showWarnings = FALSE)

add_row_df <- function(test, status, detalle = NA_character_, output = NA_character_) {
  tibble::tibble(test = test, status = status, detalle = detalle, output = output)
}
ok   <- function(x) if (isTRUE(x)) "PASS" else "FAIL"
warn <- function(x) if (isTRUE(x)) "PASS" else "WARN"

# ───────── 0) Escaneo rápido de problemas de lectura ─────────
scan_read_problems <- function(files) {
  purrr::map_dfr(files, function(fp) {
    try1 <- try(
      readr::read_delim(fp, delim = ";", col_types = readr::cols(.default = readr::col_character()),
                        show_col_types = FALSE, trim_ws = TRUE),
      silent = TRUE
    )
    sep_used <- if (inherits(try1, "try-error")) {
      try1 <- try(
        readr::read_delim(fp, delim = ",", col_types = readr::cols(.default = readr::col_character()),
                          show_col_types = FALSE, trim_ws = TRUE),
        silent = TRUE
      )
      ","
    } else ";"
    if (inherits(try1, "try-error")) {
      return(tibble::tibble(archivo = basename(fp), sep = NA_character_, n_problems = NA_integer_))
    }
    probs <- readr::problems(try1)
    tibble::tibble(archivo = basename(fp), sep = sep_used, n_problems = nrow(probs))
  })
}
files_raw <- if (dir.exists(raw_dir)) list.files(raw_dir, "\\.csv$", full.names = TRUE) else character()
probs_df  <- if (length(files_raw)) scan_read_problems(files_raw) else tibble::tibble(archivo=character(), sep=character(), n_problems=integer())
out_read_probs <- here_path("diagnostics","read_problems.csv")
readr::write_csv(probs_df, out_read_probs)
message("📝 Guardado: ", out_read_probs,
        "  (", sum(is.na(probs_df$n_problems)), " fallos de lectura; ",
        sum(!is.na(probs_df$n_problems) & probs_df$n_problems > 0), " archivos con problemas)")

# ───────── 1) Mappings: duplicados y catálogo ─────────
map_reg <- safe_read_csv(here_path("config","map_regiones.csv"))
map_tip <- safe_read_csv(here_path("config","map_tipologias.csv"))

if (!nrow(map_reg) || !all(c("raw_region","propuesta_region") %in% names(map_reg))) {
  warning("No se encontró config/map_regiones.csv válido; algunas pruebas se omitirán.")
}
if (!nrow(map_tip) || !all(c("raw_tipo","propuesta_tipo") %in% names(map_tip))) {
  warning("No se encontró config/map_tipologias.csv válido; algunas pruebas se omitirán.")
}

summary_rows <- list()

# Duplicados por raw_region
dup_raw_reg <- if (nrow(map_reg)) map_reg |>
  dplyr::count(raw_region, name="n") |>
  dplyr::filter(n>1) else tibble::tibble()
out_dup_raw_reg <- here_path("output","tables","qa_map_regiones_duplicados_por_raw.csv")
if (nrow(dup_raw_reg)) write_clean(dup_raw_reg, out_dup_raw_reg)
summary_rows[[length(summary_rows)+1]] <- add_row_df(
  "map_regiones sin duplicados por raw_region",
  ok(nrow(dup_raw_reg)==0),
  detalle = paste0("duplicados_raw_region: ", nrow(dup_raw_reg)),
  output  = if (nrow(dup_raw_reg)) out_dup_raw_reg else NA
)

# Variantes por región propuesta (INFO)
variantes_ccaa <- if (nrow(map_reg)) map_reg |>
  dplyr::count(propuesta_region, name="n_variantes") else
    tibble::tibble(propuesta_region=character(), n_variantes=integer())
write_clean(variantes_ccaa, here_path("output","tables","qa_map_regiones_variantes_por_ccaa.csv"))

# Catálogo canónico CCAA + Ceuta/Melilla
canon_map <- c(
  "andalucia"="Andalucía","aragon"="Aragón","principado de asturias"="Principado de Asturias",
  "illes balears"="Illes Balears","canarias"="Canarias","cantabria"="Cantabria",
  "castilla y leon"="Castilla y León","castilla - la mancha"="Castilla-La Mancha",
  "castilla la mancha"="Castilla-La Mancha","cataluna"="Cataluña",
  "comunidad valenciana"="Comunidad Valenciana","extremadura"="Extremadura","galicia"="Galicia",
  "comunidad de madrid"="Comunidad de Madrid","region de murcia"="Región de Murcia",
  "comunidad foral de navarra"="Comunidad Foral de Navarra","pais vasco"="País Vasco",
  "la rioja"="La Rioja","ceuta"="Ceuta","melilla"="Melilla"
)
ccaa_canon <- unname(unique(canon_map))

# Extras por defecto (ámbitos no-CCAA admitidos)
extras_defecto <- c(
  "TOTAL NACIONAL","TOTAL","ESPAÑA","ESPANA","ÁMBITO NACIONAL","AMBITO NACIONAL",
  "ESPAÑA PENINSULAR","CEUTA Y MELILLA","Sin dato","Extranjero","OTROS","NO CONSTA","IGNORADO"
)

# Extras desde config (si existe)
extras_cfg_path <- here_path("config","map_regiones_extras.csv")
extras_archivo <- if (file.exists(extras_cfg_path)) {
  suppressMessages(readr::read_csv(extras_cfg_path, show_col_types = FALSE)) |>
    janitor::clean_names() |>
    dplyr::filter(!is.na(propuesta_region)) |>
    dplyr::pull(propuesta_region)
} else character()

regiones_permitidas <- unique(c(ccaa_canon, extras_defecto, extras_archivo))

# 1a) Fuera de catálogo en mapping final
fuera_final <- if (nrow(map_reg)) map_reg |>
  dplyr::filter(!is.na(propuesta_region) & !(propuesta_region %in% regiones_permitidas)) else tibble::tibble()
out_fuera_final <- here_path("output","tables","qa_map_regiones_fuera_catalogo_en_map_final.csv")
if (nrow(fuera_final)) write_clean(fuera_final, out_fuera_final)
summary_rows[[length(summary_rows)+1]] <- add_row_df(
  "map_regiones sin valores fuera de catálogo (mapping final)",
  ok(nrow(fuera_final)==0),
  detalle = paste0("fuera_catalogo_en_map_final: ", nrow(fuera_final)),
  output  = if (nrow(fuera_final)) out_fuera_final else NA
)

# 1b) NAs en propuesta_region (pendientes de mapping)
na_en_propuesta <- if (nrow(map_reg)) map_reg |>
  dplyr::filter(is.na(propuesta_region) | propuesta_region=="") else tibble::tibble()
out_na_propuesta <- here_path("output","tables","qa_map_regiones_propuesta_region_na.csv")
if (nrow(na_en_propuesta)) write_clean(na_en_propuesta, out_na_propuesta)
summary_rows[[length(summary_rows)+1]] <- add_row_df(
  "map_regiones sin NAs en propuesta_region",
  ok(nrow(na_en_propuesta)==0),
  detalle = paste0("pendientes_de_mapping: ", nrow(na_en_propuesta)),
  output  = if (nrow(na_en_propuesta)) out_na_propuesta else NA
)

# (INFO) Diagnóstico previo del paso 02
diag_fuera_path <- here_path("output","tables","qa_map_regiones_fuera_catalogo.csv")
diag_fuera <- safe_read_csv(diag_fuera_path)
summary_rows[[length(summary_rows)+1]] <- add_row_df(
  "INFO: regiones fuera de catálogo detectadas (diagnóstico de 02)",
  "PASS",
  detalle = paste0("valores_diagnosticados: ", nrow(diag_fuera)),
  output  = if (nrow(diag_fuera)) diag_fuera_path else NA
)

# Tipologías: duplicados por propuesta
dup_tip <- if (nrow(map_tip)) map_tip |>
  dplyr::count(propuesta_tipo, name="n") |>
  dplyr::filter(n>1) else tibble::tibble()
out_dup_tip <- here_path("output","tables","qa_map_tipologias_duplicados.csv")
if (nrow(dup_tip)) write_clean(dup_tip, out_dup_tip)
summary_rows[[length(summary_rows)+1]] <- add_row_df(
  "map_tipologias sin duplicados por propuesta",
  ok(nrow(dup_tip)==0),
  detalle = paste0("duplicados: ", nrow(dup_tip)),
  output  = if (nrow(dup_tip)) out_dup_tip else NA
)

# ───────── 2) Diagnóstico de lectura: años fuera (revalidar en processed) ─────────
resumen_path <- here_path("diagnostics","resumen_raw.csv")
resumen_raw  <- safe_read_csv(resumen_path)

prefer_processed <- function(fname) {
  fp_proc <- here_path("data","processed", fname)
  if (file.exists(fp_proc)) fp_proc else here_path("data","raw", fname)
}
revalida_anos <- function(archivo) {
  fp <- prefer_processed(archivo); if (!file.exists(fp)) return(NA_integer_)
  dat <- suppressMessages(readr::read_csv(fp, show_col_types = FALSE)) |>
    janitor::clean_names()
  nms <- names(dat); colp <- nms[grepl("(ano|año|anio|year|period)", nms, ignore.case = TRUE)][1]
  if (is.na(colp)) return(NA_integer_)
  yy <- extract_year(dat[[colp]]); sum(!is.na(yy) & (yy < YEAR_MIN | yy > YEAR_MAX))
}

if (nrow(resumen_raw)) {
  off_years_raw <- resumen_raw |>
    dplyr::filter(!is.na(anos_fuera_2010_2023) & anos_fuera_2010_2023 > 0)
  if (nrow(off_years_raw)) {
    reval <- tibble::tibble(
      archivo = off_years_raw$archivo,
      fuera_en_processed = vapply(off_years_raw$archivo, revalida_anos, integer(1))
    )
    still_bad <- reval |> dplyr::filter(!is.na(fuera_en_processed) & fuera_en_processed > 0)
    out_off_years <- here_path("output","tables","qa_raw_anos_fuera_2010_2023.csv")
    write_clean(off_years_raw |> dplyr::left_join(reval, by = "archivo"), out_off_years)
    summary_rows[[length(summary_rows)+1]] <- add_row_df(
      "años fuera de 2010–2023 (se revalida en processed/ si existe)",
      ok(nrow(still_bad) == 0),
      detalle = paste0("ficheros con años fuera tras revalidar: ", nrow(still_bad)),
      output  = out_off_years
    )
  } else {
    summary_rows[[length(summary_rows)+1]] <- add_row_df(
      "años fuera de 2010–2023 (se revalida en processed/ si existe)", "PASS",
      detalle = "ficheros con años fuera tras revalidar: 0", output = NA
    )
  }
}

# Tokens problemáticos (si los hay)
parse_path   <- here_path("diagnostics","raw_numeric_parse_samples.csv")
parse_issues <- safe_read_csv(parse_path)

if (nrow(parse_issues)) {
  parse_issues <- janitor::clean_names(parse_issues)
  tok_col <- if ("token_muestra" %in% names(parse_issues)) "token_muestra" else
    if ("token" %in% names(parse_issues)) "token" else NA_character_
  numlike_re <- "(?<!\\d)\\d{1,3}(?:[\\.,]\\d{3})*(?:[\\.,]\\d+)?%?(?!\\d)"
  has_digits <- if (!is.na(tok_col)) any(grepl(numlike_re, parse_issues[[tok_col]], perl = TRUE)) else FALSE
  
  summary_rows[[length(summary_rows)+1]] <- add_row_df(
    if (has_digits)
      "parseo numérico (tokens con dígitos no parseables — revisar mappings/unidades)"
    else
      "parseo numérico (sólo tokens sin dígitos — informativo)",
    if (has_digits) "WARN" else "PASS",
    detalle = paste0("registros conflictivos: ", nrow(parse_issues),
                     if (!has_digits) " (todos sin dígitos)" else ""),
    output  = parse_path
  )
} else {
  summary_rows[[length(summary_rows)+1]] <- add_row_df(
    "parseo numérico (muestras de tokens problemáticos)", "PASS",
    detalle = "registros conflictivos: 0", output = NA
  )
}

# ───────── 3) HE: Esclarecidos ≤ Conocidos (año, región, tipo) ─────────
hc   <- if (file.exists(here_path("data","raw","hechos_conocidos.csv"))) read_raw(here_path("data","raw","hechos_conocidos.csv")) else tibble::tibble()
he   <- if (file.exists(here_path("data","raw","hechos_esclarecidos.csv"))) read_raw(here_path("data","raw","hechos_esclarecidos.csv")) else tibble::tibble()

get_col <- function(nms, pat) { nm <- nms[grepl(pat, nms, ignore.case=TRUE)]; if (length(nm)) nm[[1]] else NA_character_ }
if (nrow(hc) && nrow(he) && nrow(map_reg) && nrow(map_tip)) {
  col_tipo_hc <- get_col(names(hc), "tipolog|delit|infracc|\\btipo\\b")
  col_tipo_he <- get_col(names(he), "tipolog|delit|infracc|\\btipo\\b")
  col_reg_hc  <- get_col(names(hc), "comun|ccaa|autono|region|prov|ambito|ámbito|municip")
  col_reg_he  <- get_col(names(he), "comun|ccaa|autono|region|prov|ambito|ámbito|municip")
  col_val_hc  <- get_col(names(hc), "total|valor|numero|n|hechos|conocidos")
  col_val_he  <- get_col(names(he), "total|valor|numero|n|esclarecidos")
  col_ano_hc  <- get_col(names(hc), "ano|anio|year|period")
  col_ano_he  <- get_col(names(he), "ano|anio|year|period")
  
  stopifnot(!is.na(col_tipo_hc), !is.na(col_tipo_he),
            !is.na(col_reg_hc),  !is.na(col_reg_he),
            !is.na(col_val_hc),  !is.na(col_val_he))
  
  map_reg_slim <- map_reg |> dplyr::select(raw_region, propuesta_region)
  map_tip_slim <- map_tip |> dplyr::select(raw_tipo,   propuesta_tipo)
  
  hc2 <- hc |>
    dplyr::mutate(ano = extract_year(.data[[col_ano_hc]]),
                  tipo = .data[[col_tipo_hc]], region_raw = .data[[col_reg_hc]],
                  valor = sfnum(.data[[col_val_hc]])) |>
    dplyr::left_join(map_tip_slim, by = c("tipo" = "raw_tipo")) |>
    dplyr::left_join(map_reg_slim, by = c("region_raw" = "raw_region")) |>
    dplyr::transmute(ano, region = propuesta_region, tipo = propuesta_tipo, conocidos = valor) |>
    dplyr::filter(!is.na(ano), !is.na(region), !is.na(tipo))
  
  he2 <- he |>
    dplyr::mutate(ano = extract_year(.data[[col_ano_he]]),
                  tipo = .data[[col_tipo_he]], region_raw = .data[[col_reg_he]],
                  valor = sfnum(.data[[col_val_he]])) |>
    dplyr::left_join(map_tip_slim, by = c("tipo" = "raw_tipo")) |>
    dplyr::left_join(map_reg_slim, by = c("region_raw" = "raw_region")) |>
    dplyr::transmute(ano, region = propuesta_region, tipo = propuesta_tipo, esclarecidos = valor) |>
    dplyr::filter(!is.na(ano), !is.na(region), !is.na(tipo))
  
  chk_he_hc <- hc2 |>
    dplyr::full_join(he2, by = c("ano","region","tipo")) |>
    tidyr::replace_na(list(conocidos=0, esclarecidos=0)) |>
    dplyr::mutate(flag = esclarecidos <= conocidos, delta = esclarecidos - conocidos) |>
    dplyr::arrange(ano, region, tipo)
  
  viol_he_hc <- chk_he_hc |> dplyr::filter(!flag)
  out_he_hc <- here_path("output","tables","qa_he_vs_hc_violaciones.csv")
  if (nrow(viol_he_hc)) write_clean(viol_he_hc, out_he_hc)
  
  summary_rows[[length(summary_rows)+1]] <- add_row_df(
    "HE ≤ HC por (año, región, tipo)",
    ok(nrow(viol_he_hc)==0),
    detalle = paste0("violaciones: ", nrow(viol_he_hc)),
    output  = if (nrow(viol_he_hc)) out_he_hc else NA
  )
}

# ───────── 4) Detenciones: Extranjeros ≤ Total (año, tipo) ─────────
det_tot <- if (file.exists(here_path("data","raw","detenciones_tipologia.csv"))) read_raw(here_path("data","raw","detenciones_tipologia.csv")) else tibble::tibble()
det_ext <- if (file.exists(here_path("data","raw","detenciones_extranjeros_tipologia.csv"))) read_raw(here_path("data","raw","detenciones_extranjeros_tipologia.csv")) else tibble::tibble()

if (nrow(det_tot) && nrow(det_ext) && nrow(map_tip)) {
  col_tipo_dt <- get_col(names(det_tot), "tipolog|delit|infracc|\\btipo\\b")
  col_tipo_de <- get_col(names(det_ext), "tipolog|delit|infracc|\\btipo\\b")
  col_val_dt  <- get_col(names(det_tot), "total|valor|numero|n|detencion")
  col_val_de  <- get_col(names(det_ext), "total|valor|numero|n|detencion")
  col_ano_dt  <- get_col(names(det_tot), "ano|anio|year|period")
  col_ano_de  <- get_col(names(det_ext), "ano|anio|year|period")
  
  dt2 <- det_tot |>
    dplyr::transmute(ano = extract_year(.data[[col_ano_dt]]), tipo = .data[[col_tipo_dt]], total = sfnum(.data[[col_val_dt]])) |>
    dplyr::left_join(map_tip |> dplyr::select(raw_tipo, propuesta_tipo), by = c("tipo"="raw_tipo")) |>
    dplyr::transmute(ano, tipo = propuesta_tipo, total) |>
    dplyr::filter(!is.na(ano), !is.na(tipo))
  
  de2 <- det_ext |>
    dplyr::transmute(ano = extract_year(.data[[col_ano_de]]), tipo = .data[[col_tipo_de]], ext = sfnum(.data[[col_val_de]])) |>
    dplyr::left_join(map_tip |> dplyr::select(raw_tipo, propuesta_tipo), by = c("tipo"="raw_tipo")) |>
    dplyr::transmute(ano, tipo = propuesta_tipo, ext) |>
    dplyr::filter(!is.na(ano), !is.na(tipo))
  
  chk_det <- dt2 |>
    dplyr::group_by(ano, tipo) |> dplyr::summarise(total = sum(total, na.rm=TRUE), .groups="drop") |>
    dplyr::inner_join(de2 |> dplyr::group_by(ano, tipo) |> dplyr::summarise(ext = sum(ext, na.rm=TRUE), .groups="drop"), by=c("ano","tipo")) |>
    dplyr::mutate(flag = ext <= total, delta = ext - total) |>
    dplyr::arrange(ano, tipo)
  
  viol_det <- chk_det |> dplyr::filter(!flag)
  out_det <- here_path("output","tables","qa_det_extranjeros_vs_total_violaciones.csv")
  if (nrow(viol_det)) write_clean(viol_det, out_det)
  
  summary_rows[[length(summary_rows)+1]] <- add_row_df(
    "Detenciones extranjeros ≤ detenciones totales (año, tipo)",
    ok(nrow(viol_det)==0),
    detalle = paste0("violaciones: ", nrow(viol_det)),
    output  = if (nrow(viol_det)) out_det else NA
  )
}

# ───────── 5) No-negatividad y NAs en claves ─────────
nn_chk <- dplyr::bind_rows(
  if (exists("hc2")) hc2 |> dplyr::transmute(dataset="hechos_conocidos", ano, region, tipo, valor = conocidos) else tibble::tibble(),
  if (exists("he2")) he2 |> dplyr::transmute(dataset="hechos_esclarecidos", ano, region, tipo, valor = esclarecidos) else tibble::tibble(),
  if (exists("dt2")) dt2 |> dplyr::transmute(dataset="detenciones_total", ano, region=NA_character_, tipo, valor = total) else tibble::tibble(),
  if (exists("de2")) de2 |> dplyr::transmute(dataset="detenciones_extranjeros", ano, region=NA_character_, tipo, valor = ext) else tibble::tibble()
)

if (nrow(nn_chk)) {
  neg <- nn_chk |> dplyr::filter(!is.na(valor) & valor < 0)
  out_neg <- here_path("output","tables","qa_valores_negativos.csv")
  if (nrow(neg)) write_clean(neg, out_neg)
  na_keys <- nn_chk |> dplyr::filter(is.na(ano) | is.na(tipo) | (dataset %in% c("hechos_conocidos","hechos_esclarecidos") & is.na(region)))
  out_na <- here_path("output","tables","qa_na_en_claves.csv")
  if (nrow(na_keys)) write_clean(na_keys, out_na)
  
  summary_rows[[length(summary_rows)+1]] <- add_row_df(
    "Valores no negativos", ok(nrow(neg)==0), detalle = paste0("negativos: ", nrow(neg)),
    output = if (nrow(neg)) out_neg else NA
  )
  summary_rows[[length(summary_rows)+1]] <- add_row_df(
    "Sin NAs en claves (año/tipo/[región])", ok(nrow(na_keys)==0),
    detalle = paste0("filas con NA en clave: ", nrow(na_keys)),
    output  = if (nrow(na_keys)) out_na else NA
  )
}

# ───────── 6) Resumen y salida ─────────
summary_df <- dplyr::bind_rows(summary_rows) |>
  dplyr::mutate(status = factor(status, levels = c("FAIL","WARN","PASS"))) |>
  dplyr::arrange(status, test)

out_summary <- here_path("output","tables","qa_summary_checks.csv")
write_clean(summary_df, out_summary)
message("\n✅ QA completado. Revisa: ", out_summary)
print(summary_df, n = nrow(summary_df))

# ───────── 7) Extras opcionales sobre tokens problemáticos ─────────
if (file.exists(parse_path)) {
  probs <- safe_read_csv(parse_path) |>
    dplyr::rename(token = token_muestra)
  write_clean(probs, here_path("output","tables","qa_parse_tokens_muestras.csv"))
  top_tokens <- probs |> dplyr::count(token, sort = TRUE)
  by_filecol <- probs |> dplyr::count(archivo, columna, sort = TRUE)
  examples3  <- probs |> dplyr::group_by(token) |> dplyr::slice_head(n = 3) |> dplyr::ungroup() |> dplyr::arrange(token)
  write_clean(top_tokens, here_path("output","tables","qa_parse_tokens_top.csv"))
  write_clean(by_filecol, here_path("output","tables","qa_parse_tokens_por_archivo_columna.csv"))
  write_clean(examples3, here_path("output","tables","qa_parse_tokens_3_ejemplos.csv"))
}




# === Quick check de outputs de 03_qa_global_integridad.R ===
suppressPackageStartupMessages({
  library(readr); library(dplyr); library(janitor); library(here); library(tibble)
})

p <- function(...) here::here(...)
exists_csv <- function(fp) file.exists(fp) && grepl("\\.csv$", fp, ignore.case = TRUE)

# 1) Resumen principal (siempre debería existir)
fp_summary <- p("output","tables","qa_summary_checks.csv")
if (!exists_csv(fp_summary)) {
  stop("No encuentro qa_summary_checks.csv en output/tables/. ¿Se ejecutó el script 03?")
}
qa <- readr::read_csv(fp_summary, show_col_types = FALSE) |> clean_names()

cat("\n== QA SUMMARY ==\n")
print(qa, n = nrow(qa))
cat("\nTotales ->  PASS:", sum(qa$status=="PASS"),
    " WARN:", sum(qa$status=="WARN"),
    " FAIL:", sum(qa$status=="FAIL"), "\n")

# Si hay FAIL o WARN, muéstralos
if (any(qa$status %in% c("FAIL","WARN"))) {
  cat("\n== Tests NO PASS ==\n")
  print(qa %>% filter(status %in% c("FAIL","WARN")) %>% arrange(status, test), n = 50)
}

# 2) Archivos de detalle (solo existen si hay incidencias)
maybe_show <- function(relpath, n = 5, label = NULL) {
  fp <- p(relpath)
  if (exists_csv(fp)) {
    cat("\n--", ifelse(is.null(label), basename(fp), label), "--\n")
    df <- readr::read_csv(fp, show_col_types = FALSE) |> clean_names()
    print(head(df, n))
    invisible(df)
  } else {
    cat("\n--", ifelse(is.null(label), basename(relpath), label), "-- (no existe, probablemente sin incidencias)\n")
    invisible(NULL)
  }
}

# Diccionarios / mappings
maybe_show(file.path("output","tables","qa_map_regiones_duplicados_por_raw.csv"))
maybe_show(file.path("output","tables","qa_map_regiones_variantes_por_ccaa.csv"))
maybe_show(file.path("output","tables","qa_map_regiones_fuera_catalogo_en_map_final.csv"))
maybe_show(file.path("output","tables","qa_map_regiones_propuesta_region_na.csv"))
maybe_show(file.path("output","tables","qa_map_tipologias_duplicados.csv"))

# Lectura / diagnósticos
maybe_show(file.path("diagnostics","read_problems.csv"))
maybe_show(file.path("output","tables","qa_raw_anos_fuera_2010_2023.csv"))
maybe_show(file.path("diagnostics","raw_numeric_parse_samples.csv"), label="raw_numeric_parse_samples (diagnóstico)")
maybe_show(file.path("output","tables","qa_parse_tokens_muestras.csv"))
maybe_show(file.path("output","tables","qa_parse_tokens_top.csv"))
maybe_show(file.path("output","tables","qa_parse_tokens_por_archivo_columna.csv"))
maybe_show(file.path("output","tables","qa_parse_tokens_3_ejemplos.csv"))

# Reglas sustantivas
maybe_show(file.path("output","tables","qa_he_vs_hc_violaciones.csv"))
maybe_show(file.path("output","tables","qa_det_extranjeros_vs_total_violaciones.csv"))
maybe_show(file.path("output","tables","qa_valores_negativos.csv"))
maybe_show(file.path("output","tables","qa_na_en_claves.csv"))

cat("\n✅ Revisión rápida completada.\n")

