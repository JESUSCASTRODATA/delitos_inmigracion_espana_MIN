#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
###############################################################################
# 12_limpieza_poblacion.R — Población España (pesos 15–29 por quinquenios)
# RAW: data/raw/poblacion_espana.csv  (Nacionalidad, Grupo quinquenal de edad,
#       Sexo, Periodo, Total)
# OUT: data/processed/poblacion_15_29_bins_anual.csv (ano, edad_bin, poblacion)
###############################################################################

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(tidyr); library(stringr)
  library(janitor); library(here); library(fs); library(tibble)
})

YEAR_MIN <- 2010L; YEAR_MAX <- 2023L

# ---------- Helpers ----------
assert_infile <- function(p) if (!file.exists(p)) stop("No existe: ", p, call. = FALSE)

detect_delim <- function(path, n = 2000){
  enc <- "UTF-8"
  lines <- tryCatch(readr::read_lines(path, n_max = n, locale = locale(encoding = enc)),
                    error = function(e) readr::read_lines(path, n_max = n, locale = locale(encoding = "Latin1")))
  if (length(lines) == 0) return(";")
  counts <- c(`;`=sum(str_count(lines,";")), `,`=sum(str_count(lines,",")), `\t`=sum(str_count(lines,"\t")))
  names(counts)[which.max(counts)]
}
detect_encoding <- function(path){
  ge <- tryCatch(readr::guess_encoding(path, n_max = 100000), error = function(e) NULL)
  if (is.null(ge) || nrow(ge)==0) "UTF-8" else ge$encoding[1]
}
read_raw_once <- function(path){
  delim <- detect_delim(path); enc <- detect_encoding(path)
  message(sprintf("→ Leyendo RAW con delim='%s', encoding='%s' ...", delim, enc))
  janitor::clean_names(readr::read_delim(path, delim = delim, locale = locale(encoding = enc),
                                         guess_max = 200000, show_col_types = FALSE))
}
norm_chr <- function(x){ trimws(gsub("\\s+"," ", chartr("\u00A0"," ", as.character(x)))) }
parse_entero_es <- function(x){
  if (is.numeric(x)) return(as.numeric(x))
  s <- tolower(norm_chr(x))
  # tokens vacíos/placeholder -> NA
  mask <- s == "" | s %in% c("-", "na", "n/a", "nd", "n.d.", "s/d", "sd",
                             "sin dato", ":", "..", "...", "…")
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
  if (length(idx) > 0) nms[idx[1]] else NA_character_
}
std_sexo_total <- function(x){ s <- tolower(norm_chr(x)); grepl("\\btotal\\b|ambos", s) }
std_nacionalidad_total <- function(x){ s <- tolower(norm_chr(x)); grepl("^total\\b", s) }
std_grupo_quinquenal <- function(x){
  s <- tolower(norm_chr(x)); s <- gsub("\u2013|\u2014","-",s)
  dplyr::case_when(
    grepl("^de\\s*15\\s*a\\s*19|^15\\s*-\\s*19", s) ~ "15-19",
    grepl("^de\\s*20\\s*a\\s*24|^20\\s*-\\s*24", s) ~ "20-24",
    grepl("^de\\s*25\\s*a\\s*29|^25\\s*-\\s*29", s) ~ "25-29",
    TRUE ~ NA_character_
  )
}
parse_periodo_es <- function(p){
  p0 <- tolower(norm_chr(p))
  ano <- suppressWarnings(as.integer(stringr::str_match(p0, "([12][0-9]{3})")[,2]))
  mes <- dplyr::case_when(
    grepl("enero",   p0) ~ 1L,
    grepl("abril",   p0) ~ 4L,
    grepl("julio",   p0) ~ 7L,
    grepl("octubre", p0) ~ 10L,
    TRUE ~ NA_integer_
  )
  tri <- dplyr::case_when(mes==1L ~ 1L, mes==4L ~ 2L, mes==7L ~ 3L, mes==10L ~ 4L, TRUE ~ NA_integer_)
  tibble::tibble(ano = ano, trimestre = tri)
}
write_clean <- function(df, path){ fs::dir_create(fs::path_dir(path)); readr::write_csv(df, path, na=""); message("✓ Escrito: ", path) }

# ---------- Lectura & mapping ----------
fp_in <- here::here("data","raw","poblacion_espana.csv")
assert_infile(fp_in)
raw <- read_raw_once(fp_in); nms <- names(raw)

col_nac   <- pick_col(c("^nacionalidad$","\\bnac\\b","nacionalid"), nms)
col_grupo <- pick_col(c("^grupo_quinquenal_de_edad$","grupo.*edad","quinquenal"), nms)
col_sexo  <- pick_col(c("^sexo$","genero"), nms)
col_per   <- pick_col(c("^periodo$","period","fecha","time"), nms)
col_val   <- pick_col(c("^total$","poblaci(o|ó)n","valor","numero","n$","conteo","cantidad"), nms)
if (any(is.na(c(col_nac,col_grupo,col_sexo,col_per,col_val))))
  stop("Faltan columnas en el RAW. Detectadas: ", paste(nms, collapse=", "))

message("→ Normalizando y filtrando …")
df0 <- raw %>%
  transmute(
    nacionalidad = .data[[col_nac]],
    grupo_raw    = .data[[col_grupo]],
    sexo         = .data[[col_sexo]],
    periodo      = .data[[col_per]],
    total_chr    = .data[[col_val]]
  ) %>%
  mutate(
    edad_bin      = std_grupo_quinquenal(grupo_raw),
    es_nac_total  = std_nacionalidad_total(nacionalidad),
    es_sexo_total = std_sexo_total(sexo),
    valor         = parse_entero_es(total_chr)
  )

# QC parser
n_na <- sum(is.na(df0$valor))
if (n_na > 0) message(sprintf("⚠ Valores no numéricos convertidos a NA en RAW: %d", n_na))

yt <- parse_periodo_es(df0$periodo)

df1 <- df0 %>%
  bind_cols(yt) %>%
  filter(es_nac_total, es_sexo_total, !is.na(edad_bin), !is.na(ano), !is.na(trimestre)) %>%
  filter(dplyr::between(ano, YEAR_MIN, YEAR_MAX)) %>%
  select(ano, trimestre, edad_bin, valor)

message("→ Anualizando (preferencia JULIO) …")
pob_anual <- df1 %>%
  group_by(ano, edad_bin) %>%
  summarise(
    p_julio = dplyr::first(valor[trimestre==3 & !is.na(valor)], default = NA_real_),
    p_media = ifelse(any(!is.na(valor)), mean(valor, na.rm = TRUE), NA_real_),
    poblacion = coalesce(p_julio, p_media),
    .groups = "drop"
  ) %>%
  arrange(ano, edad_bin) %>%
  mutate(poblacion = as.integer(round(poblacion)))

# QC suave
if (any(is.na(pob_anual$poblacion))) warning("Algún año/banda quedó NA tras anualizar (sin julio ni media disponible).")
rng <- summarise(pob_anual, max = max(poblacion, na.rm = TRUE))
if (is.finite(rng$max) && rng$max > 1e7) warning("Valor muy alto en una banda 15–29; revisa el RAW.")

# ---------- OUT ----------
write_clean(pob_anual, here::here("data","processed","poblacion_15_29_bins_anual.csv"))
message("✅ Hecho.")
