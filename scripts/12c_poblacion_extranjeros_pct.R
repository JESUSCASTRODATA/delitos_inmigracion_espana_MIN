#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
###############################################################################
# 12c_poblacion_extranjeros_pct.R
# OUT:
#   data/processed/poblacion_es_extr_por_ano.csv
#   data/processed/pct_extranjeros_poblacion.csv
###############################################################################

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(stringr); library(janitor); library(here); library(fs)
})

YEAR_MIN <- 2010L; YEAR_MAX <- 2023L
write_clean <- function(df, path){ fs::dir_create(fs::path_dir(path)); readr::write_csv(df, path, na=""); message("✓ Escrito: ", path) }
assert_infile <- function(p) if (!file.exists(p)) stop("No existe: ", p, call. = FALSE)

fp_tot <- here::here("data","processed","poblacion_total_nacional.csv")
fp_esp <- here::here("data","processed","poblacion_espanola_total.csv")
fp_ext <- here::here("data","processed","poblacion_extranjera_total.csv")

if (file.exists(fp_tot) && file.exists(fp_esp) && file.exists(fp_ext)) {
  tot <- readr::read_csv(fp_tot, show_col_types=FALSE) %>% clean_names()
  esp <- readr::read_csv(fp_esp, show_col_types=FALSE) %>% clean_names()
  ext <- readr::read_csv(fp_ext, show_col_types=FALSE) %>% clean_names()
  agg <- tot %>% inner_join(esp, by="ano") %>% inner_join(ext, by="ano") %>%
    arrange(ano)
} else {
  # Fallback calculando como en 12b
  detect_delim <- function(path, n=2000){
    lines <- tryCatch(readr::read_lines(path, n_max=n, locale=locale(encoding="UTF-8")),
                      error=function(e) readr::read_lines(path, n_max=n, locale=locale(encoding="Latin1")))
    if (length(lines)==0) return(";")
    counts <- c(`;`=sum(str_count(lines,";")), `,`=sum(str_count(lines,",")), `\t`=sum(str_count(lines,"\t")))
    names(counts)[which.max(counts)]
  }
  detect_encoding <- function(path){
    ge <- tryCatch(readr::guess_encoding(path, n_max=100000), error=function(e) NULL)
    if (is.null(ge) || nrow(ge)==0) "UTF-8" else ge$encoding[1]
  }
  read_raw_once <- function(path){
    delim <- detect_delim(path); enc <- detect_encoding(path)
    janitor::clean_names(readr::read_delim(path, delim=delim, locale=locale(encoding=enc),
                                           guess_max=200000, show_col_types=FALSE))
  }
  norm_chr <- function(x){ trimws(gsub("\\s+"," ", chartr("\u00A0"," ", as.character(x)))) }
  parse_entero_es <- function(x){
    if (is.numeric(x)) return(as.numeric(x))
    s <- tolower(norm_chr(x))
    mask <- s == "" | s %in% c("-", "na", "n/a", "nd", "n.d.", "s/d", "sd", "sin dato", ":", "..", "...", "…")
    mask <- mask | grepl("^[\\.:…\\-]+$", s)
    s[mask] <- NA_character_
    suppressWarnings(
      as.numeric(readr::parse_number(s, locale = locale(decimal_mark = ",", grouping_mark = ".")))
    )
  }
  pick_col <- function(patterns, nms){
    idx <- unique(unlist(lapply(patterns, function(p){
      stringr::str_which(nms, stringr::regex(p, ignore_case = TRUE))
    })))
    if (length(idx)>0) nms[idx[1]] else NA_character_
  }
  std_is_total_sex <- function(x){ s <- tolower(norm_chr(x)); grepl("\\btotal\\b|ambos", s) }
  std_is_total_age <- function(x){ s <- tolower(norm_chr(x)); grepl("^total\\b|todas", s) }
  norm_nat <- function(x){
    s <- tolower(norm_chr(x))
    dplyr::case_when(
      grepl("^total\\b", s) ~ "total",
      grepl("espa", s)      ~ "espanola",
      grepl("extran", s)    ~ "extranjera",
      TRUE                  ~ "otra"
    )
  }
  parse_periodo_es <- function(p){
    p0 <- tolower(norm_chr(p))
    ano <- suppressWarnings(as.integer(stringr::str_match(p0, "([12][0-9]{3})")[,2]))
    mes <- dplyr::case_when(
      grepl("enero", p0) ~ 1L, grepl("abril", p0) ~ 4L, grepl("julio", p0) ~ 7L, grepl("octubre", p0) ~ 10L, TRUE ~ NA_integer_
    )
    tri <- dplyr::case_when(mes==1L ~ 1L, mes==4L ~ 2L, mes==7L ~ 3L, mes==10L ~ 4L, TRUE ~ NA_integer_)
    tibble::tibble(ano=ano, trimestre=tri)
  }
  
  fp_in <- here::here("data","raw","poblacion_espana.csv"); assert_infile(fp_in)
  raw <- read_raw_once(fp_in); nms <- names(raw)
  col_nat <- pick_col(c("^nacionalidad$","\\bnac\\b","nacionalid"), nms)
  col_age <- pick_col(c("grupo_quinquenal_de_edad","edad","todas"), nms)
  col_sex <- pick_col(c("^sexo$","genero"), nms)
  col_per <- pick_col(c("^periodo$","period","fecha","time"), nms)
  col_val <- pick_col(c("^total$","poblaci(o|ó)n","valor","numero","n$","conteo","cantidad"), nms)
  if (any(is.na(c(col_nat,col_age,col_sex,col_per,col_val))))
    stop("Faltan columnas en el RAW población.")
  
  df0 <- raw %>%
    transmute(
      nacionalidad = norm_nat(.data[[col_nat]]),
      edad_total   = std_is_total_age(.data[[col_age]]),
      sexo_total   = std_is_total_sex(.data[[col_sex]]),
      periodo      = .data[[col_per]],
      valor        = parse_entero_es(.data[[col_val]])
    )
  yt <- parse_periodo_es(df0$periodo)
  
  df <- df0 %>%
    bind_cols(yt) %>%
    filter(sexo_total, edad_total, !is.na(ano), !is.na(trimestre)) %>%
    filter(dplyr::between(ano, YEAR_MIN, YEAR_MAX)) %>%
    select(ano, trimestre, nacionalidad, valor)
  
  ann <- df %>%
    group_by(ano, nacionalidad) %>%
    summarise(
      v_julio = dplyr::first(valor[trimestre==3 & !is.na(valor)], default = NA_real_),
      v_media = ifelse(any(!is.na(valor)), mean(valor, na.rm = TRUE), NA_real_),
      val_anual = coalesce(v_julio, v_media),
      .groups = "drop"
    )
  
  tot <- ann %>% filter(nacionalidad == "total")     %>% transmute(ano, poblacion_total = val_anual)
  esp <- ann %>% filter(nacionalidad == "espanola")  %>% transmute(ano, poblacion_espanola = val_anual)
  ext_direct <- ann %>% filter(nacionalidad == "extranjera") %>% transmute(ano, poblacion_extranjera = val_anual)
  
  agg <- tot %>% full_join(esp, by="ano") %>% full_join(ext_direct, by="ano") %>%
    mutate(poblacion_extranjera = ifelse(is.na(poblacion_extranjera),
                                         pmax(0, poblacion_total - poblacion_espanola),
                                         poblacion_extranjera)) %>%
    arrange(ano)
}

# ENTEROS + % extranjeros
agg <- agg %>%
  mutate(
    poblacion_espanola   = as.integer(round(poblacion_espanola)),
    poblacion_extranjera = as.integer(round(poblacion_extranjera)),
    poblacion_total      = as.integer(round(poblacion_total))
  )
pct <- agg %>%
  mutate(pct_extranjeros_poblacion = round(100 * poblacion_extranjera / ifelse(poblacion_total>0, poblacion_total, NA), 2)) %>%
  select(ano, pct_extranjeros_poblacion)

# Controles (avisos)
if (any(abs(agg$poblacion_total - (agg$poblacion_espanola + agg$poblacion_extranjera)) > pmax(2, 0.005*agg$poblacion_total), na.rm = TRUE))
  warning("Cierre: Total != Española + Extranjera; se derivó extranjera por diferencia cuando faltaba.")
med_tot <- median(agg$poblacion_total, na.rm = TRUE)
if (is.finite(med_tot) && !(3.5e7 <= med_tot && med_tot <= 5.5e7))
  warning("Plausibilidad: total fuera de [35M, 55M]. Revisa el RAW.")

# OUT
write_clean(agg, here::here("data","processed","poblacion_es_extr_por_ano.csv"))
write_clean(pct, here::here("data","processed","pct_extranjeros_poblacion.csv"))
message("✅ Hecho.")
