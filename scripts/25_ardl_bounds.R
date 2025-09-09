#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
###############################################################################
# 25_ecm_estimar.R — ECM bivariado (Y niveles ~ X niveles), α en ecm1
#
# OUT: output/tables/25_ecm_coefs.csv  (columnas: term, estimate, std_error,
#      p_value, window, y, x)
#
# Lógica:
#  1) Para cada ventana y cada Y en niveles, elegimos X en niveles (por defecto
#     l_share_extranjeros; si no existe, heurística sobre "extr").
#  2) Cointegración estática: l_y ~ l_x → residuales → ecm1 = lag(residuales).
#  3) ECM: d_l_y ~ ecm1 + d_l_x + lag(d_l_y) + lag(d_l_x)  (OLS)
#  4) Exportamos SOLO la fila del término "ecm1".
#
# Ventanas (alineadas con tu 24/29):
#   - 2010_2023
#   - 2010_2019_pre
#   - 2020_2023_post
#   - sin_covid (excluye 2020 y 2021)
###############################################################################

suppressPackageStartupMessages({
  need <- c("dplyr","readr","here","janitor","stringr","broom","rlang")
  miss <- need[!vapply(need, requireNamespace, FUN.VALUE = logical(1), quietly = TRUE)]
  if (length(miss)) {
    stop("Faltan paquetes: ", paste(miss, collapse = ", "),
         ". Instala con:\noptions(repos=c(CRAN='https://cloud.r-project.org'));\n",
         "install.packages(c('", paste(miss, collapse = "','"), "'))", call. = FALSE)
  }
  lapply(need, function(p) try(library(p, character.only = TRUE), silent = TRUE))
})

YEAR_MIN <- 2010L; YEAR_MAX <- 2023L
COVID_YEARS <- c(2020L, 2021L)

# Y en niveles (coinciden con el recode del 29)
Y_LEVELS <- c("l_hc_rate","l_he_rate","l_det_tot_rate","l_det_ext_rate")

# Heurística para X en niveles (preferimos l_share_extranjeros)
pick_x_level <- function(cols, y){
  # 1) preferido
  if ("l_share_extranjeros" %in% cols) return("l_share_extranjeros")
  # 2) alguna con "extr" en el nombre (sin ser y)
  cand <- setdiff(cols[str_detect(cols, "^l_.*extr")], y)
  if (length(cand)) return(cand[1])
  # 3) cualquier l_* distinto de y
  cand <- setdiff(cols[str_detect(cols, "^l_")], y)
  if (length(cand)) return(cand[1])
  # 4) último recurso
  setdiff(cols, y)[1]
}

# Conversión segura a numérico
as_num <- function(x) suppressWarnings(as.numeric(x))

# Localiza archivo de datos (como en el 24)
resolve_data_path <- function(){
  cands <- c(
    here::here("data","processed","tasas","tasas_transformadas.csv"),
    here::here("data","processed","series.csv")
  )
  hit <- cands[file.exists(cands)]
  if (!length(hit)) stop("No se encontró el archivo de datos. Probé:\n- ",
                         paste(cands, collapse = "\n- "),
                         "\nGenera primero las tasas transformadas.", call. = FALSE)
  hit[1]
}

# Ventanas
WINDOWS <- list(
  `2010_2023`      = YEAR_MIN:YEAR_MAX,
  `2010_2019_pre`  = YEAR_MIN:2019L,
  `2020_2023_post` = 2020L:2023L,
  `sin_covid`      = setdiff(YEAR_MIN:YEAR_MAX, COVID_YEARS)
)

# ============================== Lectura base =================================
fp  <- resolve_data_path()
df0 <- readr::read_csv(fp, show_col_types = FALSE) |> janitor::clean_names()
if (!"ano" %in% names(df0) && "anio" %in% names(df0)) df0 <- dplyr::rename(df0, ano = anio)

# chequear que existan d_l_* para Y y X candidate principal
need_any <- c("ano", Y_LEVELS, paste0("d_", Y_LEVELS))
missing_any <- setdiff(need_any, names(df0))
if (length(missing_any)) {
  message("⚠️ Faltan columnas esperadas (algunas pueden no ser críticas): ",
          paste(missing_any, collapse = ", "))
}

# Filtra años y fuerza numérico
df <- df0 |>
  dplyr::filter(dplyr::between(ano, YEAR_MIN, YEAR_MAX)) |>
  dplyr::mutate(dplyr::across(-ano, as_num)) |>
  dplyr::arrange(ano)

# ============================ Estimación ECM =================================
rows <- list()

for (wname in names(WINDOWS)) {
  years <- WINDOWS[[wname]]
  dw <- df[df$ano %in% years, , drop = FALSE]
  if (nrow(dw) < 8) { message("ℹ Ventana ", wname, " con pocos años (", nrow(dw), "). Salto."); next }
  
  cols <- names(dw)
  
  for (yL in Y_LEVELS) {
    if (!yL %in% cols) { next }
    
    # Δlog(Y)
    dy <- paste0("d_", yL)
    if (!dy %in% cols) { next }
    
    # elegir X en niveles y su Δlog
    xL <- pick_x_level(cols, yL)
    dx <- paste0("d_", xL)
    if (!dx %in% cols) {
      # si no existe d_l_* para esa X, prueba con d_l_share_extranjeros (muy probable)
      if ("d_l_share_extranjeros" %in% cols) {
        dx <- "d_l_share_extranjeros"
        # si la X de niveles no es l_share_extranjeros, igual la cointegración usará xL
      } else {
        next
      }
    }
    
    # preparar datos
    dat <- dw |>
      dplyr::select(ano, !!yL, !!xL, !!dy, !!dx) |>
      dplyr::rename(yL = !!yL, xL = !!xL, dy = !!dy, dx = !!dx) |>
      dplyr::mutate(dplyr::across(c(yL,xL,dy,dx), as_num)) |>
      dplyr::arrange(ano)
    
    # cointegración estática (Y niveles ~ X niveles)
    co <- tryCatch(stats::lm(yL ~ xL, data = dat), error = function(e) NULL)
    if (is.null(co)) next
    dat$ecm_raw <- resid(co)
    dat$ecm1 <- dplyr::lag(dat$ecm_raw, 1)
    
    # variables de control de corto plazo (lag 1)
    dat$dy_l1 <- dplyr::lag(dat$dy, 1)
    dat$dx_l1 <- dplyr::lag(dat$dx, 1)
    
    # dataset para ECM (quita NAs por lag)
    ecmd <- dat |>
      dplyr::filter(stats::complete.cases(dy, ecm1, dx, dy_l1, dx_l1))
    
    if (nrow(ecmd) < 8) next
    
    # modelo ECM básico (simple y robusto)
    m <- tryCatch(stats::lm(dy ~ ecm1 + dx + dy_l1 + dx_l1, data = ecmd),
                  error = function(e) NULL)
    if (is.null(m)) next
    
    # recoger coeficiente del término de corrección (ecm1)
    tb <- broom::tidy(m) |>
      janitor::clean_names() |>
      dplyr::rename(std_error = dplyr::any_of(c("std.error","std_error")),
                    p_value  = dplyr::any_of(c("p.value","p_value"))) |>
      dplyr::mutate(
        term = dplyr::case_when(
          tolower(term) == "ecm1" ~ "ecm1",
          TRUE ~ term
        )
      ) |>
      dplyr::filter(term == "ecm1") |>
      dplyr::mutate(
        window = wname,
        y = yL,
        x = xL
      ) |>
      dplyr::select(term, estimate, std_error, p_value, window, y, x)
    
    if (nrow(tb)) rows[[length(rows)+1]] <- tb
  }
}

if (!length(rows)) {
  message("⚠️ No se pudieron estimar ECM válidos (sin ecm1). Exporto CSV vacío con cabeceras.")
  out_empty <- tibble::tibble(
    term = character(), estimate = numeric(), std_error = numeric(), p_value = numeric(),
    window = character(), y = character(), x = character()
  )
  fs::dir_create(here::here("output","tables"))
  readr::write_csv(out_empty, here::here("output","tables","25_ecm_coefs.csv"))
} else {
  out <- dplyr::bind_rows(rows) |>
    dplyr::arrange(window, y)
  fs::dir_create(here::here("output","tables"))
  readr::write_csv(out, here::here("output","tables","25_ecm_coefs.csv"))
}

message("✅ 25 listo: exportado output/tables/25_ecm_coefs.csv")
