
#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
# -----------------------------------------------------------------------------
# 12_build_poblacion.R — Bandas 15–29 (JULIO→media) + Totales (Total/Española/Extranjera) + % Extranjeros
# Entradas:  data/raw/poblacion_espana.csv
# Salidas:
#   - data/processed/poblacion_total_15_29_bins_anual.csv          (ano, edad_bin, poblacion)
#   - data/processed/poblacion_total_nacional.csv                  (ano, poblacion_total)
#   - data/processed/poblacion_espanola_total.csv                  (ano, poblacion_espanola)
#   - data/processed/poblacion_extranjera_total.csv                (ano, poblacion_extranjera)
#   - data/processed/pct_extranjeros_poblacion.csv [OPCIONAL]      (ano, pct_extranjeros ∈ [0,1])
# QC:
#   - output/tables/cov_poblacion_15_29.csv
#   - output/tables/cov_poblacion_15_29_criticos.csv
#   - output/tables/12b_qc_cobertura_trimestral.csv
#   - output/tables/12b_qc_sin_valores.csv
#   - output/tables/12b_qc_desviacion_cierre.csv
#   - output/tables/12b_meta_origen.csv
# Flags ENV:
#   BUILD_BINS_15_29=TRUE|FALSE
#   BUILD_TOTALES=TRUE|FALSE
#   BUILD_PCT_EXTRANJEROS=TRUE|FALSE
#   GENERATE_16_29_APPROX=TRUE|FALSE   # (compat con 16–29; usa 4/5 de 15–19)
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(tidyr); library(stringr)
  library(janitor); library(here); library(fs); library(tibble); library(digest)
})

# ---------------------- Config / Utils ---------------------------------------
YEAR_MIN <- 2010L; YEAR_MAX <- 2023L
BUILD_BINS_15_29      <- as.logical(Sys.getenv("BUILD_BINS_15_29", "TRUE"))
BUILD_TOTALES         <- as.logical(Sys.getenv("BUILD_TOTALES", "TRUE"))
BUILD_PCT_EXTRANJEROS <- as.logical(Sys.getenv("BUILD_PCT_EXTRANJEROS", "TRUE"))
GENERATE_16_29_APPROX <- as.logical(Sys.getenv("GENERATE_16_29_APPROX", "FALSE"))

dir.create(here::here("output","tables"), recursive = TRUE, showWarnings = FALSE)
dir.create(here::here("data","processed"), recursive = TRUE, showWarnings = FALSE)

# Usa utilidades comunes si existen
utils_path <- here::here("scripts","00_utils_limpieza.R")
if (file.exists(utils_path)) source(utils_path)

# Fallbacks mínimos si no existen en utils
if (!exists("root_init")) root_init <- function(x) message("ROOT: ", x, " -> ", normalizePath(here::here()), appendLF = TRUE)
if (!exists("write_clean")) write_clean <- function(df, path, na = "") {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(df, path, na = na)
  message("[12] Escrito: ", fs::path_abs(path))
  invisible(path)
}
if (!exists("read_raw")) read_raw <- function(path) {
  readr::read_delim(path, delim = ";",
                    locale = readr::locale(encoding = "UTF-8", decimal_mark = ",", grouping_mark = "."),
                    show_col_types = FALSE, guess_max = 200000) |>
    janitor::clean_names()
}

root_init("scripts/12_build_poblacion.R")
msg <- function(...) message("[12] ", paste0(...))
asrt <- function(cond, ...) { if (!isTRUE(cond)) stop(paste0(...), call. = FALSE) }

norm_chr <- function(x){
  x0 <- chartr("\u00A0", " ", as.character(x))
  x0 <- gsub("[\r\n\t]+", " ", x0)
  x0 <- gsub("\\s+", " ", x0)
  trimws(x0)
}
norm_key <- function(x){
  s <- tolower(norm_chr(x))
  s2 <- tryCatch({
    s0 <- iconv(s, to = "ASCII//TRANSLIT"); s0[is.na(s0)] <- s[is.na(s0)]; s0
  }, error = function(e) s)
  stringr::str_squish(s2)
}
parse_entero_es <- function(x){
  if (is.numeric(x)) return(as.numeric(x))
  s <- tolower(norm_chr(x))
  mask <- s == "" | s %in% c("-", "na", "n/a", "nd", "n.d.", "s/d", "sd", "sin dato", ":", "..", "...", "…") |
    grepl("^[\\.:…\\-]+$", s)
  s[mask] <- NA_character_
  suppressWarnings(as.numeric(readr::parse_number(s, locale = readr::locale(decimal_mark = ",", grouping_mark = "."))))
}
parse_periodo_es <- function(p){ # “1 de julio de 2023” → (ano, trimestre)
  p0 <- norm_key(p)
  ano <- suppressWarnings(as.integer(stringr::str_match(p0, "([12][0-9]{3})")[,2]))
  mes <- dplyr::case_when(
    grepl("\\benero\\b",   p0) ~ 1L,
    grepl("\\babril\\b",   p0) ~ 4L,
    grepl("\\bjulio\\b",   p0) ~ 7L,
    grepl("\\boctubre\\b", p0) ~ 10L,
    TRUE ~ NA_integer_
  )
  tri <- dplyr::case_when(mes==1L~1L, mes==4L~2L, mes==7L~3L, mes==10L~4L, TRUE~NA_integer_)
  tibble(ano = ano, trimestre = tri)
}

# ---------------------- Lectura robusta ---------------------------------------
fp_in <- here::here("data","raw","poblacion_espana.csv")
asrt(file.exists(fp_in), "No existe: ", fp_in)

raw <- read_raw(fp_in) |> janitor::clean_names()

# Cabeceras esperadas (flexibilizamos leves variaciones)
need <- c("nacionalidad","grupo_quinquenal_de_edad","sexo","periodo","total")
miss <- setdiff(need, names(raw))
asrt(length(miss) == 0, "Faltan columnas esperadas: ", paste(miss, collapse=", "))

# ---------------------- Normalización base ------------------------------------
df0 <- raw |>
  transmute(
    nacionalidad = norm_chr(.data[["nacionalidad"]]),
    grupo_raw    = norm_chr(.data[["grupo_quinquenal_de_edad"]]),
    sexo         = norm_chr(.data[["sexo"]]),
    periodo      = norm_chr(.data[["periodo"]]),
    valor        = parse_entero_es(.data[["total"]])
  )

yt <- parse_periodo_es(df0$periodo)
if (all(is.na(yt$trimestre)) && any(!is.na(yt$ano))) {
  msg("Periodo sin trimestre explícito: fallback trimestre=3 (julio).")
  yt$trimestre <- 3L
}

df1 <- df0 |>
  bind_cols(yt) |>
  mutate(
    nac_key  = norm_key(nacionalidad),
    sexo_key = norm_key(sexo),
    grp_key  = norm_key(grupo_raw)
  ) |>
  filter(!is.na(ano), !is.na(trimestre), between(ano, YEAR_MIN, YEAR_MAX))

# ---------------------- A) BANDAS 15–29 ---------------------------------------
if (BUILD_BINS_15_29) {
  df_bins <- df1 |>
    filter(sexo_key %in% c("total","ambos sexos","ambos","total ambos sexos"),
           nac_key  %in% c("total")) |>
    mutate(
      edad_bin = case_when(
        grp_key == "de 15 a 19 anos" ~ "15-19",
        grp_key == "de 20 a 24 anos" ~ "20-24",
        grp_key == "de 25 a 29 anos" ~ "25-29",
        TRUE ~ NA_character_
      )
    ) |>
    filter(!is.na(edad_bin)) |>
    select(ano, trimestre, edad_bin, valor)
  
  asrt(nrow(df_bins) > 0, "[12] Universo bandas vacío tras filtrado. Revisa 'grupo_quinquenal_de_edad'.")
  
  cov_quarter <- df_bins |>
    count(ano, edad_bin, trimestre, name = "n") |>
    tidyr::complete(ano = YEAR_MIN:YEAR_MAX,
                    edad_bin = c("15-19","20-24","25-29"),
                    trimestre = 1:4,
                    fill = list(n = 0)) |>
    arrange(ano, edad_bin, trimestre) |>
    mutate(presente = n > 0)
  
  write_clean(cov_quarter, here::here("output","tables","cov_poblacion_15_29.csv"))
  write_clean(
    cov_quarter |>
      group_by(ano, edad_bin) |>
      summarise(n_no_na = sum(n > 0),
                tiene_julio_val = any(trimestre==3 & n>0), .groups="drop") |>
      filter(n_no_na == 0),
    here::here("output","tables","cov_poblacion_15_29_criticos.csv")
  )
  
  pob_anual_bins <- df_bins |>
    group_by(ano, edad_bin) |>
    summarise(
      p_julio = dplyr::first(valor[trimestre==3 & !is.na(valor)], default = NA_real_),
      p_media = ifelse(any(!is.na(valor)), mean(valor, na.rm = TRUE), NA_real_),
      poblacion = dplyr::coalesce(p_julio, p_media),
      .groups = "drop"
    ) |>
    arrange(ano, edad_bin) |>
    mutate(poblacion = as.integer(round(poblacion)))
  
  write_clean(pob_anual_bins, here::here("data","processed","poblacion_total_15_29_bins_anual.csv"))
  
  # 16–29 aproximado (opcional)
  if (GENERATE_16_29_APPROX) {
    bins16 <- pob_anual_bins |>
      mutate(
        poblacion = ifelse(edad_bin == "15-19", as.integer(round(poblacion * 4/5)), poblacion),
        edad_bin  = ifelse(edad_bin == "15-19", "16-19", edad_bin)
      )
    write_clean(bins16, here::here("data","processed","poblacion_16_29_bins_anual_aprox.csv"))
    
    tot16 <- bins16 |>
      group_by(ano) |>
      summarise(poblacion_16_29 = sum(poblacion, na.rm = TRUE), .groups = "drop")
    write_clean(tot16, here::here("data","processed","poblacion_16_29_total_aprox.csv"))
  }
}

# ---------------------- B) TOTALES / EXTRANJERA -------------------------------
if (BUILD_TOTALES) {
  df_tot <- df1 |>
    filter(sexo_key %in% c("total","ambos sexos","ambos","total ambos sexos")) |>
    mutate(
      nac_std = case_when(
        grepl("\\btotal\\b",       nac_key) ~ "Total",
        grepl("\\bespan(ol|ola)\\b",nac_key) ~ "Española",
        TRUE ~ NA_character_
      ),
      is_todas = grepl("^todas", grp_key)
    ) |>
    filter(!is.na(nac_std))
  
  asrt(nrow(df_tot) > 0, "[12b] Universo totales vacío tras filtrado.")
  
  tri <- if (any(df_tot$is_todas, na.rm = TRUE)) {
    df_tot |>
      filter(is_todas) |>
      group_by(ano, trimestre, nac_std) |>
      summarise(valor_tri = sum(valor, na.rm = TRUE), .groups = "drop")
  } else {
    df_tot |>
      group_by(ano, trimestre, nac_std) |>
      summarise(valor_tri = sum(valor, na.rm = TRUE), .groups = "drop")
  }
  
  cov12b <- tri |>
    group_by(ano, nac_std) |>
    summarise(n_trimestres = n_distinct(trimestre),
              n_no_na = sum(!is.na(valor_tri)),
              .groups = "drop") |>
    arrange(ano, nac_std)
  write_clean(cov12b, here::here("output","tables","12b_qc_cobertura_trimestral.csv"))
  write_clean(cov12b |> filter(n_no_na == 0),
              here::here("output","tables","12b_qc_sin_valores.csv"))
  
  annualize <- function(df){
    df |>
      group_by(ano) |>
      summarise(
        p_julio = dplyr::first(valor_tri[trimestre==3 & !is.na(valor_tri)], default = NA_real_),
        p_media = ifelse(any(!is.na(valor_tri)), mean(valor_tri, na.rm = TRUE), NA_real_),
        poblacion = as.numeric(dplyr::coalesce(p_julio, p_media)),
        .groups = "drop"
      ) |>
      arrange(ano)
  }
  
  p_tot <- tri |> filter(nac_std=="Total")    |> annualize() |> rename(poblacion_total = poblacion)
  p_esp <- tri |> filter(nac_std=="Española") |> annualize() |> rename(poblacion_espanola = poblacion)
  
  asrt(nrow(p_tot) > 0 && all(!is.na(p_tot$poblacion_total)),
       "[12b] 'Total' con NA tras anualizar.")
  asrt(nrow(p_esp) > 0 && all(!is.na(p_esp$poblacion_espanola)),
       "[12b] 'Española' con NA tras anualizar.")
  
  out <- p_tot |>
    left_join(p_esp, by = "ano") |>
    mutate(poblacion_extranjera = poblacion_total - poblacion_espanola) |>
    arrange(ano)
  
  asrt(all(!is.na(out$poblacion_extranjera)), "[12b] 'Extranjera' quedó NA (falta Total o Española).")
  asrt(all(out$poblacion_extranjera >= 0),    "[12b] 'Extranjera' negativa. Revisa Total/Española o unidades.")
  
  # Cierre contable (QA)
  cmp <- out |>
    mutate(diff = poblacion_total - (poblacion_espanola + poblacion_extranjera),
           rel = abs(diff)/pmax(1, poblacion_total))
  write_clean(cmp, here::here("output","tables","12b_qc_desviacion_cierre.csv"))
  
  # META origen
  rng <- suppressWarnings(range(out$ano[is.finite(out$ano)], na.rm = TRUE))
  meta <- tibble::tibble(
    archivo = basename(fp_in),
    hash_sha256 = digest::digest(file = fp_in, algo = "sha256"),
    filas = nrow(raw),
    cols  = ncol(raw),
    desde = rng[1],
    hasta = rng[2]
  )
  write_clean(meta, here::here("output","tables","12b_meta_origen.csv"))
  
  # OUT CSV
  write_clean(out |> select(ano, poblacion_total),
              here::here("data","processed","poblacion_total_nacional.csv"))
  write_clean(out |> select(ano, poblacion_espanola),
              here::here("data","processed","poblacion_espanola_total.csv"))
  write_clean(out |> select(ano, poblacion_extranjera),
              here::here("data","processed","poblacion_extranjera_total.csv"))
  
  # ---------------------- C) % EXTRANJEROS -----------------------------------
  if (BUILD_PCT_EXTRANJEROS) {
    pct <- out |>
      mutate(pct_extranjeros = ifelse(poblacion_total > 0,
                                      poblacion_extranjera / poblacion_total, NA_real_)) |>
      select(ano, pct_extranjeros)
    asrt(all(pct$pct_extranjeros >= 0 & pct$pct_extranjeros <= 1, na.rm = TRUE),
         "[12c] pct_extranjeros fuera de [0,1].")
    write_clean(pct, here::here("data","processed","pct_extranjeros_poblacion.csv"))
  }
}

msg("✅ Población unificada generada (bandas, totales y % extranjeros si activado).")




