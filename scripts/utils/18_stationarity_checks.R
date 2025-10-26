#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
###############################################################################
# 18_stationarity_checks.R — Estacionariedad (ADF/PP/KPSS/ERS/NgP/ZA) por ventana
# Proyecto : Delitos e Inmigración en España (2010–2023)
# Autor    : Jesús Castro · JESUSCASTRODATA
# Fecha    : 2025-09-19
#
# Pruebas: ADF (drift/trend), PP (const/trend), KPSS (mu/tau),
#          ERS (ADF-GLS), Ng–Perron (MZt), Zivot–Andrews (ruptura)
# Ventanas: con_covid (2010–2023), sin_covid (sin 2020–2021), 2010–2019 (pre),
#           2022–2023 (post; se omite si N<8)
# Salidas : output/tables/18_stationarity*.csv, output/figures/18_stationarity_heatmap.{png,svg}
###############################################################################

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(tidyr); library(here)
  library(urca);  library(purrr); library(stringr); library(ggplot2)
  library(methods)
})

msg <- function(...) message("[18] ", paste0(...))

# ------------------ Entradas / salidas ------------------
ensure_dir <- function(path) if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
ensure_dir(here::here("output/tables")); ensure_dir(here::here("output/figures"))

infile_candidates <- c(
  here::here("data/processed","core_indicadores_con_pib.csv"),
  here::here("data/processed","series.csv")
)
infile <- infile_candidates[file.exists(infile_candidates)][1]
if (is.na(infile)) stop("No se encontró ni core_indicadores_con_pib.csv ni series.csv", call. = FALSE)
msg("Usando entrada: ", normalizePath(infile, winslash = "/"))
DF <- readr::read_csv(infile, show_col_types = FALSE)

vars <- intersect(c(
  "tasa_hc_por_100k","tasa_he_por_100k","tasa_det_por_100k",
  "share_ext","pct_extranjeros_poblacion","pib_pc_real"
), names(DF))
if (!length(vars)) stop("Ninguna de las variables esperadas está en el archivo de entrada.", call. = FALSE)

# ------------------ Ventanas ------------------
windows <- list(
  "con_covid_2010_2023" = function(d) d,
  "sin_covid_2010_2023" = function(d) d %>% filter(!(ano %in% c(2020L, 2021L))),
  "2010_2019_pre"       = function(d) d %>% filter(ano <= 2019L),
  "2022_2023_post"      = function(d) d %>% filter(ano >= 2022L) # se omite si N<8
)
min_n <- 8L

# ------------------ Tema/colores --------------------------------------------
PEARL <- "#FAF9F6"
COLS  <- c("Level-stationary"="#56B4E9", "Trend-stationary"="#F0E442",
           "Break-stationary"="#CC79A7", "Non-stationary"="#999999")
try({ src <- here::here("scripts","00_theme_figuras.R"); if (file.exists(src)) source(src, local = TRUE) }, silent = TRUE)
theme_used <- if (exists("theme_pearl_lightgrid")) theme_pearl_lightgrid else
  function(...) theme_minimal(base_size = 12) +
  theme(plot.background=element_rect(fill=PEARL, colour=NA),
        panel.background=element_rect(fill=PEARL, colour=NA),
        legend.background=element_rect(fill=PEARL, colour=NA))

# ------------------ Helpers de pruebas --------------------------------------
safe_num <- function(x) as.numeric(x)

# ADF / PP
run_adf_drift <- function(x){
  z <- try(ur.df(x, type = "drift", lags = 1), silent = TRUE); if (inherits(z,"try-error")) return(NULL)
  stat <- suppressWarnings(as.numeric(z@teststat["tau2"]))
  cvals <- suppressWarnings(as.numeric(z@cval["tau2", c("1pct","5pct","10pct")]))
  tibble(test="ADF(drift,lag=1,tau2)", stat, cv_1pct=cvals[1], cv_5pct=cvals[2], cv_10pct=cvals[3],
         reject_1pct=is.finite(stat)&&(stat<cvals[1]),
         reject_5pct=is.finite(stat)&&(stat<cvals[2]),
         reject_10pct=is.finite(stat)&&(stat<cvals[3]))
}
run_adf_trend <- function(x){
  z <- try(ur.df(x, type = "trend", lags = 1), silent = TRUE); if (inherits(z,"try-error")) return(NULL)
  stat <- suppressWarnings(as.numeric(z@teststat["tau3"]))
  cvals <- suppressWarnings(as.numeric(z@cval["tau3", c("1pct","5pct","10pct")]))
  tibble(test="ADF(trend,lag=1,tau3)", stat, cv_1pct=cvals[1], cv_5pct=cvals[2], cv_10pct=cvals[3],
         reject_1pct=is.finite(stat)&&(stat<cvals[1]),
         reject_5pct=is.finite(stat)&&(stat<cvals[2]),
         reject_10pct=is.finite(stat)&&(stat<cvals[3]))
}
run_pp_const <- function(x){
  z <- try(ur.pp(x, type = "Z-tau", model = "constant", lags = "short"), silent = TRUE); if (inherits(z,"try-error")) return(NULL)
  stat <- suppressWarnings(as.numeric(z@teststat))
  cvals <- suppressWarnings(as.numeric(z@cval[c("1pct","5pct","10pct")]))
  tibble(test="PP(Z-tau,const,short)", stat, cv_1pct=cvals[1], cv_5pct=cvals[2], cv_10pct=cvals[3],
         reject_1pct=is.finite(stat)&&(stat<cvals[1]),
         reject_5pct=is.finite(stat)&&(stat<cvals[2]),
         reject_10pct=is.finite(stat)&&(stat<cvals[3]))
}
run_pp_trend <- function(x){
  z <- try(ur.pp(x, type = "Z-tau", model = "trend", lags = "short"), silent = TRUE); if (inherits(z,"try-error")) return(NULL)
  stat <- suppressWarnings(as.numeric(z@teststat))
  cvals <- suppressWarnings(as.numeric(z@cval[c("1pct","5pct","10pct")]))
  tibble(test="PP(Z-tau,trend,short)", stat, cv_1pct=cvals[1], cv_5pct=cvals[2], cv_10pct=cvals[3],
         reject_1pct=is.finite(stat)&&(stat<cvals[1]),
         reject_5pct=is.finite(stat)&&(stat<cvals[2]),
         reject_10pct=is.finite(stat)&&(stat<cvals[3]))
}

# KPSS
run_kpss_mu <- function(x){
  z <- try(ur.kpss(x, type = "mu"), silent = TRUE);  if (inherits(z,"try-error")) return(NULL)
  stat <- suppressWarnings(as.numeric(z@teststat))
  cvals <- suppressWarnings(as.numeric(z@cval[c("1pct","5pct","10pct")]))
  tibble(test="KPSS(mu: level)", stat, cv_1pct=cvals[1], cv_5pct=cvals[2], cv_10pct=cvals[3],
         reject_1pct=is.finite(stat)&&(stat>cvals[1]),
         reject_5pct=is.finite(stat)&&(stat>cvals[2]),
         reject_10pct=is.finite(stat)&&(stat>cvals[3]))
}
run_kpss_tau <- function(x){
  z <- try(ur.kpss(x, type = "tau"), silent = TRUE); if (inherits(z,"try-error")) return(NULL)
  stat <- suppressWarnings(as.numeric(z@teststat))
  cvals <- suppressWarnings(as.numeric(z@cval[c("1pct","5pct","10pct")]))
  tibble(test="KPSS(tau: trend)", stat, cv_1pct=cvals[1], cv_5pct=cvals[2], cv_10pct=cvals[3],
         reject_1pct=is.finite(stat)&&(stat>cvals[1]),
         reject_5pct=is.finite(stat)&&(stat>cvals[2]),
         reject_10pct=is.finite(stat)&&(stat>cvals[3]))
}

# ERS (DF-GLS)
run_ers_const <- function(x){
  z <- try(ur.ers(x, type="DF-GLS", model="constant", lag.max=2), silent=TRUE); if (inherits(z,"try-error")) return(NULL)
  stat <- suppressWarnings(as.numeric(z@teststat))
  cvals <- suppressWarnings(as.numeric(z@cval[c("1pct","5pct","10pct")]))
  tibble(test="ERS(DF-GLS,const)", stat, cv_1pct=cvals[1], cv_5pct=cvals[2], cv_10pct=cvals[3],
         reject_1pct=is.finite(stat)&&(stat<cvals[1]),
         reject_5pct=is.finite(stat)&&(stat<cvals[2]),
         reject_10pct=is.finite(stat)&&(stat<cvals[3]))
}
run_ers_trend <- function(x){
  z <- try(ur.ers(x, type="DF-GLS", model="trend", lag.max=2), silent=TRUE); if (inherits(z,"try-error")) return(NULL)
  stat <- suppressWarnings(as.numeric(z@teststat))
  cvals <- suppressWarnings(as.numeric(z@cval[c("1pct","5pct","10pct")]))
  tibble(test="ERS(DF-GLS,trend)", stat, cv_1pct=cvals[1], cv_5pct=cvals[2], cv_10pct=cvals[3],
         reject_1pct=is.finite(stat)&&(stat<cvals[1]),
         reject_5pct=is.finite(stat)&&(stat<cvals[2]),
         reject_10pct=is.finite(stat)&&(stat<cvals[3]))
}

# Ng–Perron
run_ngp_const <- function(x){
  z <- try(ur.ngp(x, model="constant"), silent=TRUE); if (inherits(z,"try-error")) return(NULL)
  stat <- suppressWarnings(as.numeric(z@teststat["MZt"]))
  cvals <- suppressWarnings(as.numeric(z@cval["MZt", c("1pct","5pct","10pct")]))
  tibble(test="NGP(MZt,const)", stat, cv_1pct=cvals[1], cv_5pct=cvals[2], cv_10pct=cvals[3],
         reject_1pct=is.finite(stat)&&(stat<cvals[1]),
         reject_5pct=is.finite(stat)&&(stat<cvals[2]),
         reject_10pct=is.finite(stat)&&(stat<cvals[3]))
}
run_ngp_trend <- function(x){
  z <- try(ur.ngp(x, model="trend"), silent=TRUE); if (inherits(z,"try-error")) return(NULL)
  stat <- suppressWarnings(as.numeric(z@teststat["MZt"]))
  cvals <- suppressWarnings(as.numeric(z@cval["MZt", c("1pct","5pct","10pct")]))
  tibble(test="NGP(MZt,trend)", stat, cv_1pct=cvals[1], cv_5pct=cvals[2], cv_10pct=cvals[3],
         reject_1pct=is.finite(stat)&&(stat<cvals[1]),
         reject_5pct=is.finite(stat)&&(stat<cvals[2]),
         reject_10pct=is.finite(stat)&&(stat<cvals[3]))
}

# Zivot–Andrews (ruptura)
run_za <- function(x, years = NULL){
  z <- try(ur.za(x, model="both", lag=1), silent=TRUE); if (inherits(z,"try-error")) return(NULL)
  stat <- suppressWarnings(as.numeric(z@teststat[1]))
  cv5 <- NA_real_; cv10 <- NA_real_
  if ("cval" %in% methods::slotNames(z)) {
    C <- z@cval
    c5n <- intersect(colnames(C), c("5pct","5%")); c10n <- intersect(colnames(C), c("10pct","10%"))
    if (length(c5n))  cv5  <- suppressWarnings(as.numeric(C[1, c5n[1]]))
    if (length(c10n)) cv10 <- suppressWarnings(as.numeric(C[1, c10n[1]]))
  }
  break_idx <- NA_integer_
  for (cand in c("b.point","bp","bpoints","breakpoint")) {
    if (cand %in% methods::slotNames(z)) {
      val <- tryCatch(methods::slot(z, cand), error=function(e) NULL)
      if (is.numeric(val) && length(val)>=1) { break_idx <- as.integer(val[1]); break }
    }
  }
  break_year <- if (!is.null(years) && is.finite(break_idx) &&
                    break_idx>=1 && break_idx<=length(years)) years[break_idx] else NA_integer_
  tibble(test="ZA(break:both)", stat = stat,
         cv_5pct = cv5, cv_10pct = cv10,
         reject_5pct = is.finite(stat)&&is.finite(cv5)&&(stat < cv5),
         reject_10pct= is.finite(stat)&&is.finite(cv10)&&(stat < cv10),
         za_break_index = break_idx, za_break_year = break_year)
}

# Clasificación (relajada) con soporte y ZA seguro
classify_stationarity_relaxed <- function(rows_df, alpha = c("5pct","10pct")){
  alpha <- match.arg(alpha)
  flag <- paste0("reject_", alpha)
  
  is_unit_level <- rows_df$test %in% c("ADF(drift,lag=1,tau2)","PP(Z-tau,const,short)",
                                       "ERS(DF-GLS,const)","NGP(MZt,const)")
  is_unit_trend <- rows_df$test %in% c("ADF(trend,lag=1,tau3)","PP(Z-tau,trend,short)",
                                       "ERS(DF-GLS,trend)","NGP(MZt,trend)")
  is_kpss_mu  <- rows_df$test == "KPSS(mu: level)"
  is_kpss_tau <- rows_df$test == "KPSS(tau: trend)"
  is_za       <- rows_df$test == "ZA(break:both)"
  
  sig_level <- c(any(rows_df[[flag]] & is_unit_level, na.rm = TRUE),
                 any(!rows_df[[flag]] & is_kpss_mu,   na.rm = TRUE))
  sig_trend <- c(any(rows_df[[flag]] & is_unit_trend, na.rm = TRUE),
                 any(!rows_df[[flag]] & is_kpss_tau,  na.rm = TRUE))
  za_reject <- any(rows_df[[flag]] & is_za, na.rm = TRUE)
  
  score_level <- sum(sig_level, na.rm = TRUE)
  score_trend <- sum(sig_trend, na.rm = TRUE)
  
  base_class <- if (score_level == 0 && score_trend == 0) "Non-stationary"
  else if (score_level > score_trend) "Level-stationary"
  else if (score_trend > score_level) "Trend-stationary"
  else "Trend-stationary"  # empate → conservador
  
  # Año de quiebre seguro
  za_break_year <- NA_integer_
  if ("za_break_year" %in% names(rows_df)) {
    v <- rows_df$za_break_year[is_za]
    if (length(v) >= 1 && is.finite(v[1])) za_break_year <- as.integer(v[1])
  }
  
  cls <- if (base_class == "Non-stationary" && za_reject) "Break-stationary" else base_class
  
  list(
    class          = cls,
    support_level  = score_level,
    support_trend  = score_trend,
    za_reject      = isTRUE(za_reject),
    za_break_year  = za_break_year
  )
}

# ------------------ Loop principal -------------------------------------------
detailed_rows <- list()
summary_rows  <- list()

for (w in names(windows)) {
  dwin <- windows[[w]](DF) %>% arrange(ano)
  if (!"ano" %in% names(dwin)) next
  nobs <- nrow(dwin %>% filter(is.finite(ano)))
  if (nobs < min_n && w == "2022_2023_post") { msg("Ventana ", w, " omitida (N<", min_n, ")."); next }
  
  for (v in vars) {
    # ---- Niveles ----
    x0 <- dwin[[v]] |> safe_num()
    idx <- which(is.finite(x0)); x <- x0[idx]; years <- dwin$ano[idx]
    if (length(x) >= min_n) {
      parts <- list(
        run_adf_drift(x), run_adf_trend(x),
        run_pp_const(x),  run_pp_trend(x),
        run_kpss_mu(x),   run_kpss_tau(x),
        run_ers_const(x), run_ers_trend(x),
        run_ngp_const(x), run_ngp_trend(x),
        run_za(x, years)
      )
      det <- bind_rows(parts[!sapply(parts, is.null)])
      if (nrow(det)) {
        det <- det %>% mutate(window = w, var = v, mode = "level", n_obs = length(x), .before = 1)
        detailed_rows[[length(detailed_rows)+1]] <- det
        
        res5  <- classify_stationarity_relaxed(det, "5pct")
        res10 <- classify_stationarity_relaxed(det, "10pct")
        summary_rows[[length(summary_rows)+1]] <- tibble(
          window = w, var = v, mode = "level", n_obs = length(x),
          classification_5pct = res5$class,  classification_10pct = res10$class,
          support_level_5 = res5$support_level, support_trend_5 = res5$support_trend,
          support_level_10= res10$support_level, support_trend_10= res10$support_trend,
          za_reject_5 = res5$za_reject, za_reject_10 = res10$za_reject,
          za_break_year = res5$za_break_year
        )
      }
    }
    
    # ---- Primera diferencia Δx ----
    if (length(x) >= (min_n + 1)) {
      dx <- diff(x)
      parts_d <- list(
        run_adf_drift(dx), run_adf_trend(dx),
        run_pp_const(dx),  run_pp_trend(dx),
        run_kpss_mu(dx),   run_kpss_tau(dx),
        run_ers_const(dx), run_ers_trend(dx),
        run_ngp_const(dx), run_ngp_trend(dx)
      )
      det_d <- bind_rows(parts_d[!sapply(parts_d, is.null)])
      if (nrow(det_d)) {
        det_d <- det_d %>% mutate(window = w, var = v, mode = "diff", n_obs = length(dx), .before = 1)
        detailed_rows[[length(detailed_rows)+1]] <- det_d
        
        res5d  <- classify_stationarity_relaxed(det_d, "5pct")
        res10d <- classify_stationarity_relaxed(det_d, "10pct")
        summary_rows[[length(summary_rows)+1]] <- tibble(
          window = w, var = v, mode = "diff", n_obs = length(dx),
          classification_5pct = res5d$class,  classification_10pct = res10d$class,
          support_level_5 = res5d$support_level, support_trend_5 = res5d$support_trend,
          support_level_10= res10d$support_level, support_trend_10= res10d$support_trend,
          za_reject_5 = FALSE, za_reject_10 = FALSE,
          za_break_year = NA_integer_
        )
      }
    }
  }
}

# ------------------ Salidas --------------------------------------------------
if (length(detailed_rows)) {
  out_det <- bind_rows(detailed_rows) %>% relocate(window, var, mode, test)
  write_csv(out_det, here::here("output/tables","18_stationarity.csv"))
  msg("Escrito: output/tables/18_stationarity.csv")
} else {
  msg("⚠️ No hubo suficientes observaciones para generar pruebas detalladas.")
}

if (length(summary_rows)) {
  out_sum <- bind_rows(summary_rows) %>% arrange(var, window, mode)
  write_csv(out_sum, here::here("output/tables","18_stationarity_summary.csv"))
  msg("Escrito: output/tables/18_stationarity_summary.csv")
  
  # Tabla ejecutiva (α=5%)
  tab_exec <- out_sum %>%
    mutate(classification_5pct = factor(classification_5pct,
                                        levels=c("Non-stationary","Level-stationary","Trend-stationary","Break-stationary"))) %>%
    group_by(window, mode) %>%
    summarise(
      n_series = n(),
      pct_stationary = mean(classification_5pct != "Non-stationary")*100,
      pct_level = mean(classification_5pct == "Level-stationary")*100,
      pct_trend = mean(classification_5pct == "Trend-stationary")*100,
      pct_break = mean(classification_5pct == "Break-stationary")*100,
      .groups = "drop"
    ) %>% arrange(window, mode)
  write_csv(tab_exec, here::here("output/tables","18_stationarity_summary_table.csv"))
  msg("Escrito: output/tables/18_stationarity_summary_table.csv")
  
  # Heatmap (α=5%)
  sum5 <- out_sum %>%
    transmute(window,
              var,
              mode = factor(mode, levels=c("diff","level"),
                            labels=c("Δ (primera diferencia)","Nivel")),
              classification = factor(classification_5pct,
                                      levels=c("Non-stationary","Level-stationary","Trend-stationary","Break-stationary")))
  lab_short <- c("Non-stationary"="NS","Level-stationary"="L",
                 "Trend-stationary"="T","Break-stationary"="B")
  win_labels <- c(
    "con_covid_2010_2023" = "2010–2023 (con COVID)",
    "sin_covid_2010_2023" = "2010–2023 (sin 2020–2021)",
    "2010_2019_pre"       = "2010–2019 (pre)",
    "2022_2023_post"      = "2022–2023 (post)"
  )
  sum5$window <- factor(sum5$window, levels = names(win_labels),
                        labels = win_labels[names(win_labels)])
  
  p <- ggplot(sum5, aes(x = window, y = var, fill = classification)) +
    geom_tile(color = "white") +
    geom_text(aes(label = lab_short[classification]), size = 4.2, fontface = "bold", color = "black") +
    scale_fill_manual(values = COLS, drop = FALSE) +
    facet_wrap(~ mode, nrow = 1) +
    theme_used() +
    theme(axis.title = element_blank(),
          axis.text.x = element_text(angle = 15, hjust = 1),
          legend.position = "right",
          strip.text = element_text(face = "bold")) +
    labs(title = "Estacionariedad por ventana y transformación",
         subtitle = "Clasificación relajada (α = 5%) con ADF/PP/ERS/Ng–Perron y KPSS; ZA marca rupturas",
         fill = "Clasificación (5%)",
         caption = "NS: Non-stationary · L: Level-stationary · T: Trend-stationary · B: Break-stationary (Zivot–Andrews)")
  ggsave(filename = here::here("output/figures","18_stationarity_heatmap.png"),
         plot = p, width = 14, height = 7, dpi = 300, bg = PEARL)
  ggsave(filename = here::here("output/figures","18_stationarity_heatmap.svg"),
         plot = p, width = 14, height = 7, dpi = 300, bg = PEARL)
  msg("Figuras escritas: output/figures/18_stationarity_heatmap.{png,svg}")
} else {
  msg("⚠️ No hubo suficientes observaciones para generar el resumen.")
}

# ---- Mensaje breve de interpretación ----
msg("Tip: si 'level' es Non/Break-stationary y 'diff' es L/T, trabaja en Δ (o dlog) y usa PIB_pc_real_yoy para ciclo.")

# === (NUEVO) Tabla ejecutiva y heatmap para α = 10% =========================
tab_exec_10 <- out_sum %>%
  mutate(classification_10pct = factor(
    classification_10pct,
    levels = c("Non-stationary","Level-stationary","Trend-stationary","Break-stationary")
  )) %>%
  group_by(window, mode) %>%
  summarise(
    n_series       = n(),
    pct_stationary = mean(classification_10pct != "Non-stationary")*100,
    pct_level      = mean(classification_10pct == "Level-stationary")*100,
    pct_trend      = mean(classification_10pct == "Trend-stationary")*100,
    pct_break      = mean(classification_10pct == "Break-stationary")*100,
    .groups = "drop"
  ) %>% arrange(window, mode)

write_csv(tab_exec_10, here::here("output/tables","18_stationarity_summary_table_a10.csv"))
msg("Escrito: output/tables/18_stationarity_summary_table_a10.csv")

# Heatmap α = 10%
sum10 <- out_sum %>%
  transmute(window,
            var,
            mode = factor(mode, levels = c("diff","level"),
                          labels = c("Δ (primera diferencia)","Nivel")),
            classification = factor(classification_10pct,
                                    levels = c("Non-stationary","Level-stationary","Trend-stationary","Break-stationary")))

lab_short <- c("Non-stationary"="NS","Level-stationary"="L",
               "Trend-stationary"="T","Break-stationary"="B")

win_labels <- c(
  "con_covid_2010_2023" = "2010–2023 (con COVID)",
  "sin_covid_2010_2023" = "2010–2023 (sin 2020–2021)",
  "2010_2019_pre"       = "2010–2019 (pre)",
  "2022_2023_post"      = "2022–2023 (post)"
)
sum10$window <- factor(sum10$window, levels = names(win_labels),
                       labels = win_labels[names(win_labels)])

p10 <- ggplot2::ggplot(sum10, aes(x = window, y = var, fill = classification)) +
  ggplot2::geom_tile(color = "white") +
  ggplot2::geom_text(aes(label = lab_short[classification]),
                     size = 4.2, fontface = "bold", color = "black") +
  ggplot2::scale_fill_manual(values = COLS, drop = FALSE) +
  ggplot2::facet_wrap(~ mode, nrow = 1) +
  theme_used() +
  ggplot2::theme(
    axis.title = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(angle = 15, hjust = 1),
    legend.position = "right",
    strip.text = ggplot2::element_text(face = "bold")
  ) +
  ggplot2::labs(
    title    = "Estacionariedad por ventana y transformación",
    subtitle = "Clasificación relajada (α = 10%) con ADF/PP/ERS/Ng–Perron y KPSS; ZA marca rupturas",
    fill     = "Clasificación (10%)",
    caption  = "NS: Non-stationary · L: Level-stationary · T: Trend-stationary · B: Break-stationary (Zivot–Andrews)"
  )

ggplot2::ggsave(filename = here::here("output/figures","18_stationarity_heatmap_a10.png"),
                plot = p10, width = 14, height = 7, dpi = 300, bg = PEARL)
ggplot2::ggsave(filename = here::here("output/figures","18_stationarity_heatmap_a10.svg"),
                plot = p10, width = 14, height = 7, dpi = 300, bg = PEARL)
msg("Figuras escritas: output/figures/18_stationarity_heatmap_a10.{png,svg}")
# ============================================================================
