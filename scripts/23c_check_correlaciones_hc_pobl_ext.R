# -*- coding: UTF-8 -*-
###############################################################################
# 23c_check_correlaciones_hc_pobl_ext.R — Correlaciones por ventanas
# Proyecto: Delitos e Inmigración en España (2010–2023)
# Autor: JESUS CASTRO
# Fecha: 2025-09-07
# Descripción:
#   - Lee tabla anual base-100 (HC y población extranjera).
#   - Calcula correlaciones (Pearson / Spearman / Kendall) en ventanas:
#       2010–2023 (completa), 2010–2019 (pre), 2020–2021 (COVID),
#       2021–2023 (post), sin COVID (2010–2019 & 2022–2023).
#   - Repite el cálculo en Δlog (variaciones anuales).
#   - Exporta: output/tables/_debug_cor_hc_pop_prepost.csv
# Requisitos: here, readr, dplyr, tidyr, tibble
# Uso: source("scripts/23c_check_correlaciones_hc_pobl_ext.R")
###############################################################################

suppressPackageStartupMessages({
  library(here); library(readr); library(dplyr); library(tidyr); library(tibble)
})

# 0) Ancla del proyecto (no falla si el Rmd no existe)
try(here::i_am("docs/informe_delitos_inmigracion_espana_2010_2023.Rmd"), silent = TRUE)

# 1) Candidatas a tabla ANUAL con series base-100
cand <- c(
  "output/tables/base100_hc_poblaciones_full.csv",   # recomendada (incluye COVID)
  "output/tables/28_base100_series.csv",             # alternativa
  "output/tables/base100_hc_poblaciones_no_covid.csv" # sin COVID
)

exists_vec <- file.exists(here::here(cand))
if (!any(exists_vec)) {
  cat("\nNo encontré ninguna tabla anual de las candidatas.\n",
      "Contenido de output/tables/: \n")
  print(tryCatch(list.files(here::here("output","tables")), error = function(e) "No existe output/tables"))
  stop("\nAcción necesaria: exporta una tabla con columnas (ano, hc_idx, pextr_idx), ",
       "o edita 'cand' con la ruta correcta.")
}
src <- cand[which(exists_vec)[1]]
message("Usando tabla: ", src)

# 2) Lectura
df0 <- readr::read_csv(here::here(src), show_col_types = FALSE)

# 3) Detección/mapeo de columnas
pick_col <- function(nms, candidates) {
  ix <- which(tolower(nms) %in% tolower(candidates))
  if (length(ix)) nms[ix[1]] else NA_character_
}

year_candidates  <- c("ano","anio","a\u00f1o","year","anyo")
hc_candidates    <- c("hc_idx","hc_base100","HC_base100","hcindex","hc_index",
                      "hc_b100","hc","base100_hc","hc_base_100")
pextr_candidates <- c("pextr_idx","pobl_extranjera_idx","pobl_extranjera_base100",
                      "poblacion_extranjera_base100","pct_extranjeros_poblacion_base100",
                      "pobl_extranjeros_base100","pextr_base100","pextr_b100",
                      "pobl_ext_idx","pobl_ext","pext_b100")  # incluye tu columna

nms <- names(df0)
col_year  <- pick_col(nms, year_candidates)
col_hc    <- pick_col(nms, hc_candidates)
col_pextr <- pick_col(nms, pextr_candidates)

if (any(is.na(c(col_year,col_hc,col_pextr)))) {
  msg <- paste0(
    "Columnas detectadas: ", paste(nms, collapse=", "), "\n",
    "Necesito (alguna de): año=", paste(year_candidates, collapse="/"),
    " | hc=", paste(hc_candidates, collapse="/"),
    " | pobl_ext=", paste(pextr_candidates, collapse="/")
  )
  stop("No pude mapear columnas.\n", msg)
}

df <- df0 |>
  transmute(
    ano        = .data[[col_year]],
    hc_idx     = as.numeric(.data[[col_hc]]),
    pextr_idx  = as.numeric(.data[[col_pextr]])
  ) |>
  drop_na() |>
  arrange(ano)

# 4) Chequeos básicos
if (!all(diff(df$ano) > 0)) stop("Los años no están estrictamente ordenados de menor a mayor.")
if (anyDuplicated(df$ano)) stop("Hay años duplicados en la tabla.")

# 5) Función de correlaciones por ventana
corr_block <- function(d, etiqueta, x="hc_idx", y="pextr_idx") {
  tibble(
    ventana  = etiqueta,
    n        = nrow(d),
    pearson  = cor(d[[x]], d[[y]], method = "pearson"),
    spearman = cor(d[[x]], d[[y]], method = "spearman"),
    kendall  = cor(d[[x]], d[[y]], method = "kendall")
  )
}

# 6) Ventanas (NIVELES)
res_levels <- bind_rows(
  corr_block(filter(df, ano>=2010, ano<=2023), "2010–2023 (completa)"),
  corr_block(filter(df, ano>=2010, ano<=2019), "2010–2019 (pre)"),
  corr_block(filter(df, ano>=2020, ano<=2021), "2020–2021 (COVID)"),
  corr_block(filter(df, ano>=2021, ano<=2023), "2021–2023 (post)"),
  corr_block(filter(df, !(ano %in% c(2020, 2021))), "sin COVID (2010–2019 & 2022–2023)")
)

# 7) Δlog (variaciones)
dlog <- df |>
  mutate(
    dl_hc   = if_else(ano > min(ano), log(hc_idx)    - lag(log(hc_idx)),    NA_real_),
    dl_pext = if_else(ano > min(ano), log(pextr_idx) - lag(log(pextr_idx)), NA_real_)
  ) |>
  drop_na()

res_dlog <- bind_rows(
  corr_block(filter(dlog, ano>=2011, ano<=2023), "Δlog 2011–2023 (completa)", "dl_hc","dl_pext"),
  corr_block(filter(dlog, ano>=2011, ano<=2019), "Δlog 2011–2019 (pre)",      "dl_hc","dl_pext"),
  corr_block(filter(dlog, ano>=2020, ano<=2021), "Δlog 2020–2021 (COVID)",    "dl_hc","dl_pext"),
  corr_block(filter(dlog, ano>=2021, ano<=2023), "Δlog 2021–2023 (post)",     "dl_hc","dl_pext"),
  corr_block(filter(dlog, !(ano %in% c(2020, 2021))), "Δlog sin COVID",       "dl_hc","dl_pext")
)

# 8) Avisos útiles
warns <- c()
sp_post <- res_levels$spearman[res_levels$ventana == "2021–2023 (post)"]
n_post  <- res_levels$n[res_levels$ventana == "2021–2023 (post)"]
if (length(sp_post) == 1 && abs(sp_post) > 0.99 && n_post > 3) {
  warns <- c(warns, "Spearman≈1 con n>3: revise orden de filas/origen de datos.")
}
if (length(sp_post) == 1 && abs(sp_post) > 0.99 && n_post == 3) {
  warns <- c(warns, "Spearman≈1 con n=3 es esperable si la subventana es monótona (interpretar con cautela).")
}

# 9) Salida (CSV y consola)
out <- bind_rows(
  mutate(res_levels, tipo = "niveles"),
  mutate(res_dlog,   tipo = "Δlog")
) |>
  relocate(tipo, ventana, n)

dir.create(here::here("output","tables"), showWarnings = FALSE, recursive = TRUE)
out_path <- here::here("output","tables","_debug_cor_hc_pop_prepost.csv")
write_csv(out, out_path)

message("\n==> Correlaciones guardadas en: ", out_path, "\n")
print(out, n = nrow(out))

if (length(warns)) {
  message("\nAvisos:\n- ", paste(warns, collapse = "\n- "))
} else {
  message("\nSin avisos.")
}
