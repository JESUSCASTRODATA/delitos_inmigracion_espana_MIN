#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
################################################################################
# 21_correlaciones_niveles_y_deltas.R — Correlaciones (niveles, Δ/Δlog) y parciales
# Proyecto   : Delitos e Inmigración en España (2010–2023)
# Autor      : Jesús Castro · JESUSCASTRODATA
# Licencia   : MIT (código) · CC BY 4.0 (docs/figuras)
# Fecha      : generado automáticamente
# Descripción:
#   - Correlaciones bivariadas (Pearson, Spearman, Kendall) en niveles y en Δ.
#   - Correlaciones parciales controlando por PIB (Δ), y por PIB pc, paro joven, AROPE (niveles) si existen.
#   - Ventanas: total (2010–2023) y “sin COVID” (2010–2019 & 2022–2023).
#   - FDR (Benjamini–Hochberg) opcional por bloque.
################################################################################

suppressPackageStartupMessages({
  library(here); library(readr); library(dplyr); library(tidyr); library(janitor)
  library(stringr); library(purrr); library(broom)
})

msg <- function(...) message("[21] ", paste0(...))
ensure_dir <- function(p){ dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE); invisible(p) }
`%||%` <- function(a,b) if (is.null(a)) b else a

# Rutas
p_core   <- here::here("data","processed","core_indicadores.csv")
p_deriv  <- here::here("data","processed","core_indicadores_derivadas.csv")

p_out_lvl  <- here::here("output","tables","corr_niveles.csv")
p_out_del  <- here::here("output","tables","corr_deltas.csv")
p_out_par  <- here::here("output","tables","corr_parciales.csv")
p_out_fdr  <- here::here("output","tables","corr_fdr.csv")
invisible(lapply(c(p_out_lvl,p_out_del,p_out_par,p_out_fdr), ensure_dir))

# Lectura
stopifnot(file.exists(p_core))
core <- readr::read_csv(p_core, show_col_types = FALSE) |> clean_names() |> arrange(ano)
deriv <- if (file.exists(p_deriv)) readr::read_csv(p_deriv, show_col_types = FALSE) |> clean_names() |> arrange(ano) else NULL

if (!"ano" %in% names(core)) stop("[21] Falta columna 'ano' en core.")

# Ventanas
no_covid_years <- c(2010:2019, 2022:2023)

# Detección flexible de nombres (alias robustos)
pick_one <- function(nms, patterns) {
  hit <- nms[Reduce("|", lapply(patterns, function(p) grepl(p, nms, ignore.case = TRUE)))]
  if (length(hit)) hit[1] else NA_character_
}

N <- names(core)
col_hc   <- pick_one(N, c("^tasa_hc_100k$", "hc.*100k"))
col_he   <- pick_one(N, c("^tasa_he_100k$", "he.*100k"))
col_share<- pick_one(N, c("^share_extran", "share.*extr"))
col_pct  <- pick_one(N, c("^pct_extran", "porc.*extran"))
col_paro <- pick_one(N, c("^paro_joven", "paro.*(15|16).*29"))
col_aro  <- pick_one(N, c("^arope", "riesgo.*pobreza|exclusion"))
col_pib  <- pick_one(N, c("^pib_pc_real", "pib.*pc"))

# Variables presentes (niveles)
vars_presentes_lvl <- na.omit(c(col_hc, col_he, col_share, col_pct))
if (length(vars_presentes_lvl) < 2) stop("[21] No hay suficientes variables en niveles para correlacionar.")

# Deltas (derivadas)
if (!is.null(deriv)) {
  Nd <- names(deriv)
  d_hc   <- pick_one(Nd, c("^d_tasa_hc_100k$", "d.*hc.*100k"))
  d_he   <- pick_one(Nd, c("^d_tasa_he_100k$", "d.*he.*100k"))
  d_pct  <- pick_one(Nd, c("^d_pct_extran$", "^d_pct_extranjeros$", "d.*pct.*extran"))
  d_share<- pick_one(Nd, c("^d_share_ext$", "^d_share_extran", "d.*share.*extr"))
  d_pib  <- pick_one(Nd, c("^dln_pib_pc$", "d.?ln.*pib.*pc"))
  vars_presentes_del <- na.omit(c(d_hc, d_he, d_pct, d_share))
} else {
  d_pib <- NULL
  vars_presentes_del <- character(0)
}

# Controles
controls_lvl <- na.omit(c(col_pib, col_paro, col_aro))  # PIB pc, paro joven, AROPE si están
controls_del <- na.omit(c(d_pib))                       # Δ ln PIB pc si está

# Helpers correlación
cor_pair <- function(x, y, method){
  suppressWarnings({
    ct <- try(cor.test(x, y, method = method, exact = FALSE, conf.level = 0.95), silent = TRUE)
  })
  if (inherits(ct, "try-error")) return(tibble(estimate = NA_real_, p_value = NA_real_, conf_low = NA_real_, conf_high = NA_real_))
  out <- broom::tidy(ct)
  tibble(
    estimate = out$estimate %||% NA_real_,
    p_value  = out$p.value %||% NA_real_,
    conf_low = if ("conf.low"  %in% names(out)) out$conf.low  else NA_real_,
    conf_high= if ("conf.high" %in% names(out)) out$conf.high else NA_real_
  )
}

pairs_from <- function(vars){
  if (length(vars) < 2) return(tibble(x = character(), y = character()))
  M <- t(combn(vars, 2))
  tibble(x = M[,1], y = M[,2])
}

compute_block <- function(df, vars, years, scope_label){
  d <- df |>
    filter(ano %in% years) |>
    select(any_of(c("ano", vars))) |>
    drop_na()
  if (nrow(d) < 3) return(tibble())
  pairs <- pairs_from(vars)
  methods <- c("pearson","spearman","kendall")
  purrr::pmap_dfr(list(pairs$x, pairs$y), function(xv, yv){
    purrr::map_dfr(methods, function(m){
      r <- cor_pair(d[[xv]], d[[yv]], m)
      tibble(scope = scope_label, method = m, x = xv, y = yv, n = nrow(d), !!!r)
    })
  })
}

# Niveles
msg("Correlaciones en niveles…")
res_lvl_all <- compute_block(core, vars_presentes_lvl, unique(core$ano), "total")
res_lvl_nc  <- compute_block(core, vars_presentes_lvl, no_covid_years,  "sin_covid")
res_lvl <- bind_rows(res_lvl_all, res_lvl_nc)
readr::write_csv(res_lvl, p_out_lvl)
msg("✔ Escrito: ", p_out_lvl)

# Deltas
if (length(vars_presentes_del) >= 2 && !is.null(deriv)){
  msg("Correlaciones en Δ…")
  res_del_all <- compute_block(deriv, vars_presentes_del, unique(deriv$ano), "total")
  res_del_nc  <- compute_block(deriv, vars_presentes_del, no_covid_years,  "sin_covid")
  res_del <- bind_rows(res_del_all, res_del_nc)
  readr::write_csv(res_del, p_out_del)
  msg("✔ Escrito: ", p_out_del)
} else {
  msg("AVISO: no hay suficientes variables de Δ para correlacionar; se omite 'corr_deltas.csv'.")
}

# Parciales
can_partial <- function(){ requireNamespace("ppcor", quietly = TRUE) }
pcor_pair <- function(df, x, y, controls){
  vars <- c(x, y, controls)
  d <- df |> select(any_of(vars)) |> drop_na()
  if (nrow(d) < (length(controls) + 3)) return(tibble(estimate = NA_real_, p_value = NA_real_, n = nrow(d)))
  res <- try(ppcor::pcor.test(d[[x]], d[[y]], d[, controls, drop=FALSE]), silent = TRUE)
  if (inherits(res, "try-error")) return(tibble(estimate = NA_real_, p_value = NA_real_, n = nrow(d)))
  tibble(estimate = unname(res$estimate), p_value = unname(res$p.value), n = nrow(d))
}

res_par <- tibble()
if (can_partial()){
  # Pares típicos en niveles
  target_pairs_lvl <- pairs_from(na.omit(c(col_pct, col_share, col_hc, col_he)))
  if (nrow(target_pairs_lvl) && length(controls_lvl)){
    msg("Correlaciones parciales (niveles) controlando por: ", paste(controls_lvl, collapse=", "))
    res_par_lvl_all <- purrr::pmap_dfr(list(target_pairs_lvl$x, target_pairs_lvl$y), function(xv,yv){
      out <- pcor_pair(core, xv, yv, controls_lvl)
      mutate(out, scope="total", method="partial", x=xv, y=yv)
    })
    res_par_lvl_nc  <- purrr::pmap_dfr(list(target_pairs_lvl$x, target_pairs_lvl$y), function(xv,yv){
      out <- pcor_pair(filter(core, ano %in% no_covid_years), xv, yv, controls_lvl)
      mutate(out, scope="sin_covid", method="partial", x=xv, y=yv)
    })
    res_par <- bind_rows(res_par, res_par_lvl_all, res_par_lvl_nc)
  }
  # Pares en deltas (si hay Δ y control Δ ln PIB)
  if (!is.null(deriv) && length(controls_del)){
    target_pairs_del <- pairs_from(na.omit(c(d_pct, d_share, d_hc, d_he)))
    if (nrow(target_pairs_del)){
      msg("Correlaciones parciales (Δ) controlando por: ", paste(controls_del, collapse=", "))
      res_par_del_all <- purrr::pmap_dfr(list(target_pairs_del$x, target_pairs_del$y), function(xv,yv){
        out <- pcor_pair(deriv, xv, yv, controls_del)
        mutate(out, scope="total", method="partial_delta", x=xv, y=yv)
      })
      res_par_del_nc  <- purrr::pmap_dfr(list(target_pairs_del$x, target_pairs_del$y), function(xv,yv){
        out <- pcor_pair(filter(deriv, ano %in% no_covid_years), xv, yv, controls_del)
        mutate(out, scope="sin_covid", method="partial_delta", x=xv, y=yv)
      })
      res_par <- bind_rows(res_par, res_par_del_all, res_par_del_nc)
    }
  }
  if (nrow(res_par)) {
    readr::write_csv(res_par, p_out_par)
    msg("✔ Escrito: ", p_out_par)
  } else {
    msg("AVISO: no hay pares/controles suficientes para parciales; se omite 'corr_parciales.csv'.")
  }
} else {
  msg("AVISO: paquete 'ppcor' no disponible; se omiten correlaciones parciales.")
}

# FDR por bloque
acc <- list()
if (file.exists(p_out_lvl))  acc[["niveles"]]   <- readr::read_csv(p_out_lvl, show_col_types = FALSE)
if (file.exists(p_out_del))  acc[["deltas"]]    <- readr::read_csv(p_out_del, show_col_types = FALSE)
if (file.exists(p_out_par))  acc[["parciales"]] <- readr::read_csv(p_out_par, show_col_types = FALSE)

if (length(acc) > 0){
  msg("Aplicando FDR (BH)…")
  fdr_tbl <- purrr::imap_dfr(acc, function(df,name){
    df |> mutate(block = name) |>
      group_by(block, scope, method = if ("method" %in% names(df)) method else "pearson") |>
      mutate(p_adj_fdr = p.adjust(p_value, method = "BH")) |>
      ungroup()
  })
  readr::write_csv(fdr_tbl, p_out_fdr)
  msg("✔ Escrito: ", p_out_fdr)
}

msg("✅ Listo. Tablas en output/tables/: corr_niveles.csv, corr_deltas.csv, corr_parciales.csv (si aplica), corr_fdr.csv.")






mini_check_21 <- function(out_dir = here::here("output","tables")) {
  suppressPackageStartupMessages({
    library(readr); library(dplyr); library(janitor); library(fs); library(stringr)
  })
  cat("\n—— MINI CHECK · 21_correlaciones ————————————————\n")
  
  # 1) Archivos esperados
  files <- c("corr_niveles.csv","corr_deltas.csv","corr_parciales.csv","corr_fdr.csv")
  paths <- file.path(out_dir, files)
  exists_tbl <- tibble(archivo = files,
                       path = paths,
                       existe = file.exists(paths),
                       n_rows = purrr::map_int(paths, ~ if (file.exists(.x)) nrow(suppressMessages(read_csv(.x, show_col_types = FALSE))) else NA_integer_))
  print(exists_tbl, n = nrow(exists_tbl))
  
  # Helper para leer si existe
  rd <- function(fname) {
    p <- file.path(out_dir, fname)
    if (file.exists(p)) suppressMessages(read_csv(p, show_col_types = FALSE) |> clean_names()) else NULL
  }
  
  niv <- rd("corr_niveles.csv")
  del <- rd("corr_deltas.csv")
  par <- rd("corr_parciales.csv")
  fdr <- rd("corr_fdr.csv")
  
  # 2) Validaciones rápidas por bloque
  quick_block <- function(df, nombre) {
    if (is.null(df)) { cat(paste0("\n", nombre, ": NO ENCONTRADO\n")); return(invisible(NULL)) }
    req_cols <- c("scope","method","x","y","n","estimate","p_value")
    faltan <- setdiff(req_cols, names(df))
    if (length(faltan)) cat(nombre, "→ columnas faltantes:", paste(faltan, collapse=", "), "\n")
    
    n_na_est <- sum(is.na(df$estimate))
    n_na_p   <- sum(is.na(df$p_value))
    n_smalln <- sum(df$n < 3, na.rm = TRUE)
    scopes   <- paste(sort(unique(df$scope)), collapse=", ")
    methods  <- paste(sort(unique(df$method)), collapse=", ")
    
    cat("\n", nombre, ":\n", sep = "")
    cat("  scopes:  ", scopes, "\n", sep="")
    cat("  methods: ", methods, "\n", sep="")
    cat("  NA en r: ", n_na_est, " | NA en p: ", n_na_p, " | n<3: ", n_smalln, "\n", sep="")
    
    # Top 5 por |r| (Pearson, scope total)
    if (all(c("estimate","method","scope") %in% names(df))) {
      top <- df |> filter(method == "pearson", scope == "total") |>
        mutate(abs_r = abs(estimate)) |>
        arrange(desc(abs_r)) |>
        transmute(x, y, r = round(estimate,3), p = signif(p_value,3), n) |>
        head(5)
      if (nrow(top)) { cat("  Top Pearson (total):\n"); print(top, n = nrow(top)) }
    }
  }
  
  quick_block(niv, "NIVELES")
  quick_block(del, "DELTAS")
  quick_block(par, "PARCIALES")
  
  # 3) FDR: hallazgos significativos
  if (!is.null(fdr) && all(c("p_adj_fdr","estimate") %in% names(fdr))) {
    sig <- fdr |> filter(!is.na(p_adj_fdr), p_adj_fdr <= 0.05) |>
      arrange(p_adj_fdr) |>
      transmute(scope, method, x, y, r = round(estimate,3), p_adj_fdr = signif(p_adj_fdr,3))
    cat("\nFDR (BH) — significativos (q≤0.05): ", if (nrow(sig)) "" else "NINGUNO\n", sep="")
    if (nrow(sig)) print(sig, n = min(10, nrow(sig)))
  } else {
    cat("\nFDR: NO ENCONTRADO o sin columnas requeridas.\n")
  }
  
  # 4) Señales de incoherencia comunes
  issues <- c()
  if (!is.null(niv) && "n" %in% names(niv) && any(niv$n < 3, na.rm = TRUE)) issues <- c(issues, "n<3 en niveles")
  if (!is.null(del) && "n" %in% names(del) && any(del$n < 3, na.rm = TRUE)) issues <- c(issues, "n<3 en deltas")
  if (!is.null(niv) && any(is.na(niv$estimate))) issues <- c(issues, "NA en estimate (niveles)")
  if (!is.null(del) && any(is.na(del$estimate))) issues <- c(issues, "NA en estimate (deltas)")
  
  cat("\nResumen coherencia: ", if (length(issues)) paste("WARN →", paste(unique(issues), collapse=" | ")) else "OK", "\n", sep="")
  cat("———————————————————————————————————————————————\n\n")
  invisible(TRUE)
}
mini_check_21()