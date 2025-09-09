#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
################################################################################
# 34_qc_schema_contracts.R  — valida contratos de esquema (robusto)
################################################################################

suppressPackageStartupMessages({
  library(here); library(readr); library(dplyr); library(tidyr); library(janitor)
  library(purrr); library(tibble); library(yaml); library(stringr); library(rlang); library(fs)
})

fs::dir_create(here::here("output","tables"), recurse = TRUE)

`%||%` <- function(x, y) if (is.null(x) || length(x)==0) y else x

read_csv_auto <- function(fp){
  out <- tryCatch(suppressMessages(readr::read_csv(fp, show_col_types = FALSE)),
                  error = function(e) NULL)
  if (is.null(out)){
    out <- tryCatch(suppressMessages(readr::read_delim(fp, delim = ";", show_col_types = FALSE)),
                    error = function(e) NULL)
  }
  out
}

num_es <- function(x){
  readr::parse_number(as.character(x),
                      locale = readr::locale(decimal_mark = ",", grouping_mark = "."))
}

is_type_ok <- function(x, type){
  type <- tolower(type)
  if (type %in% c("numeric","double")){
    sx <- suppressWarnings(num_es(x)); return(mean(!is.na(sx)) >= 0.95)
  } else if (type %in% c("integer","int")){
    sx <- suppressWarnings(num_es(x)); ok <- mean(!is.na(sx)) >= 0.95
    if (!ok) return(FALSE)
    return(mean(abs(sx - round(sx)) < 1e-8, na.rm=TRUE) >= 0.95)
  } else if (type == "string"){
    return(TRUE)
  }
  TRUE
}

viol <- function(file, check, detail, extra = list()){
  base <- tibble(file = file, check = check, detail = detail)
  if (!length(extra)) return(base)
  if (is.null(names(extra)) || any(!nzchar(names(extra)))) names(extra) <- paste0("extra_", seq_along(extra))
  tibble::add_column(base, !!!extra)
}

# ----------- Localización del YAML (ENV/CLI) y autogeneración si falta --------
args <- commandArgs(trailingOnly = TRUE)
schemas_path <- Sys.getenv("SCHEMAS_YML", unset = NA)
if (is.na(schemas_path) || !nzchar(schemas_path)) {
  schemas_path <- if (length(args) >= 1) args[1] else here::here("config","schemas.yml")
}
fs::dir_create(dirname(schemas_path), recurse = TRUE)

if (!file.exists(schemas_path)) {
  message("ℹ No existe schemas.yml. Creo una PLANTILLA en: ", schemas_path)
  template <- list(
    schemas = list(
      list(
        path = "hechos_conocidos_total_nacional.csv",
        required = TRUE,
        required_columns = list("ano","tipo","valor"),
        types = list(ano="integer", valor="numeric"),
        key = list("ano","tipo"),
        coverage = list(from=2010, to=2023)
      ),
      list(
        path = "detenciones_totales_total_nacional.csv",
        required = TRUE,
        required_columns = list("ano","tipo","valor"),
        types = list(ano="integer", valor="numeric"),
        key = list("ano","tipo"),
        coverage = list(from=2010, to=2023)
      ),
      list(
        path = "detenciones_extranjeros_total_nacional.csv",
        required = TRUE,
        required_columns = list("ano","tipo","valor"),
        types = list(ano="integer", valor="numeric"),
        key = list("ano","tipo"),
        coverage = list(from=2010, to=2023)
      ),
      list(
        path = "poblacion_total_nacional.csv",
        required = TRUE,
        required_columns = list("ano","poblacion_total"),
        types = list(ano="integer", poblacion_total="numeric"),
        key = list("ano"),
        coverage = list(from=2010, to=2023)
      ),
      list(
        path = "tasas/tasas_totales_anuales.csv",
        required = FALSE,
        required_columns = list("ano"),
        types = list(ano="integer"),
        key = list("ano")
      ),
      list(
        path = "detenciones_share_extranjeros.csv",
        required = FALSE,
        required_columns = list("ano"),
        types = list(ano="integer"),
        key = list("ano")
      )
    )
  )
  yaml::write_yaml(template, schemas_path)
  message("✅ Plantilla creada. Puedes editar dominios/rangos si quieres endurecer reglas.")
}

cfg <- yaml::read_yaml(schemas_path)
schemas <- cfg$schemas %||% list()

processed_root <- here::here("data","processed")
all_files <- list.files(processed_root, recursive = TRUE, full.names = TRUE)
relpath <- function(x) gsub(paste0("^", gsub("\\\\","/", processed_root), "/?"), "", gsub("\\\\","/", x))

violations <- tibble(file=character(), check=character(), detail=character())

# --------------------------------- Validación --------------------------------
for (sch in schemas){
  targets <- character()
  if (!is.null(sch$path)){
    f <- file.path(processed_root, sch$path)
    if (file.exists(f)){
      targets <- c(targets, f)
    } else if (isTRUE(sch$required)){
      violations <- dplyr::bind_rows(violations, viol(sch$path, "FILE_missing", "El fichero requerido no existe"))
    }
  }
  if (!is.null(sch$pattern)){
    hits <- grep(sch$pattern, relpath(all_files), value = TRUE)
    if (length(hits) == 0 && isTRUE(sch$required)){
      violations <- dplyr::bind_rows(violations, viol(sch$pattern, "FILE_missing_pattern", "No hay ficheros que cumplan el patrón"))
    }
    if (length(hits) > 0) targets <- c(targets, file.path(processed_root, hits))
  }
  if (!length(targets)) next
  
  for (f in unique(targets)){
    rp <- relpath(f)
    df <- read_csv_auto(f)
    if (is.null(df)){
      violations <- dplyr::bind_rows(violations, viol(rp, "FILE_unreadable", "No se pudo leer el CSV"))
      next
    }
    df <- df |> janitor::clean_names()
    
    # 1) Columnas obligatorias
    if (!is.null(sch$required_columns)){
      miss <- setdiff(tolower(unlist(sch$required_columns)), names(df))
      if (length(miss) > 0){
        violations <- dplyr::bind_rows(violations, viol(rp, "SCHEMA_columns_missing", paste(miss, collapse=", ")))
      }
    }
    
    # 2) Tipos
    if (!is.null(sch$types)){
      for (nm in names(sch$types)){
        col <- tolower(nm)
        if (!col %in% names(df)) next
        typ <- as.character(sch$types[[nm]])
        if (!is_type_ok(df[[col]], typ)){
          violations <- dplyr::bind_rows(violations, viol(rp, "SCHEMA_type_mismatch", paste0(col, " != ", typ)))
        }
      }
    }
    
    # 3) Dominios
    if (!is.null(sch$domains)){
      for (nm in names(sch$domains)){
        col <- tolower(nm)
        if (!col %in% names(df)) next
        allowed <- as.character(unlist(sch$domains[[nm]]))
        bad <- df |>
          dplyr::filter(!is.na(.data[[col]]) & !(as.character(.data[[col]]) %in% allowed)) |>
          dplyr::distinct(.data[[col]])
        if (nrow(bad) > 0){
          violations <- dplyr::bind_rows(
            violations,
            viol(rp, "SCHEMA_domain_violation",
                 paste0(col, " fuera de dominio: ", paste(head(bad[[col]], 10), collapse=" | ")))
          )
        }
      }
    }
    
    # 4) Rango numérico
    if (!is.null(sch$ranges)){
      for (nm in names(sch$ranges)){
        col <- tolower(nm)
        if (!col %in% names(df)) next
        rng <- sch$ranges[[nm]]
        x <- suppressWarnings(num_es(df[[col]]))
        cond <- rep(FALSE, length(x))
        if (!is.null(rng$min)) cond <- cond | (x < as.numeric(rng$min))
        if (!is.null(rng$max)) cond <- cond | (x > as.numeric(rng$max))
        n_bad <- sum(cond, na.rm=TRUE)
        if (n_bad > 0){
          violations <- dplyr::bind_rows(
            violations,
            viol(rp, "SCHEMA_range_violation",
                 paste0(col, " fuera de [", (rng$min %||% "-inf"), ", ", (rng$max %||% "+inf"), "] (", n_bad, " filas)"))
          )
        }
      }
    }
    
    # 5) Unicidad por clave
    if (!is.null(sch$key)){
      keys <- tolower(unlist(sch$key))
      if (all(keys %in% names(df))){
        dups <- df |>
          dplyr::group_by(dplyr::across(dplyr::all_of(keys))) |>
          dplyr::mutate(.n = dplyr::n()) |>
          dplyr::filter(.data$.n > 1) |>
          dplyr::ungroup() |>
          dplyr::select(dplyr::all_of(keys)) |>
          dplyr::distinct()
        if (nrow(dups) > 0){
          violations <- dplyr::bind_rows(
            violations,
            viol(rp, "SCHEMA_key_duplicates",
                 paste("Duplicados por", paste(keys, collapse=", ")),
                 list(n_rows = nrow(dups)))
          )
        }
      } else {
        missk <- setdiff(keys, names(df))
        if (length(missk) > 0){
          violations <- dplyr::bind_rows(violations, viol(rp, "SCHEMA_key_missing_columns", paste(missk, collapse=", ")))
        }
      }
    }
    
    # 6) Cobertura temporal
    if (!is.null(sch$coverage) && "ano" %in% names(df)){
      yrs  <- suppressWarnings(as.integer(df$ano))
      from <- as.integer(sch$coverage$from %||% min(yrs, na.rm=TRUE))
      to   <- as.integer(sch$coverage$to   %||% max(yrs, na.rm=TRUE))
      miss <- setdiff(seq.int(from, to), unique(yrs))
      if (length(miss) > 0){
        violations <- dplyr::bind_rows(
          violations,
          viol(rp, "SCHEMA_coverage_years_missing", paste(miss, collapse=", "))
        )
      }
    }
  }
}

# ------------------------------- Export ---------------------------------------
summ <- if (nrow(violations) == 0){
  tibble(file = character(), n_checks = integer(), n_violations = integer())
} else {
  violations |> dplyr::count(file, name="n_violations") |> dplyr::mutate(n_checks = NA_integer_)
}

readr::write_csv(violations, here::here("output","tables","qa34_schema_details.csv"))
readr::write_csv(summ,       here::here("output","tables","qa34_schema_summary.csv"))

message("✅ QC 34 terminado. Revisa:",
        "\n - output/tables/qa34_schema_summary.csv",
        "\n - output/tables/qa34_schema_details.csv",
        "\nℹ Archivo de esquemas usado: ", schemas_path,
        "\n  (ENV SCHEMAS_YML o argumento CLI para personalizar)")
