#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
###############################################################################
# 24_var_granger_pipeline.R — VAR/Granger bivar y generación de IRFs por ventana y política.
#
# Autor: JESUS CASTRO
# Fecha: 2025-09-04
###############################################################################
suppressPackageStartupMessages({
  library(dplyr); library(readr); library(tidyr); library(here)
  library(ggplot2); library(scales)
  library(vars); library(urca); library(lmtest)
})

PEARL <- "#FAF9F6"
OI <- c(
  black = "#000000", orange = "#E69F00", sky = "#56B4E9", green = "#009E73",
  yellow = "#F0E442", blue = "#0072B2", vermilion = "#D55E00", purple = "#CC79A7"
)
theme_pearl <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.background  = ggplot2::element_rect(fill = PEARL, colour = NA),
      panel.background = ggplot2::element_rect(fill = PEARL, colour = NA),
      legend.background= ggplot2::element_rect(fill = PEARL, colour = NA),
      legend.key       = ggplot2::element_rect(fill = PEARL, colour = NA)
    )
}
ensure_dir <- function(path) if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
ensure_dir(here::here("output/figures"))
ensure_dir(here::here("output/tables"))

infile <- here::here("data/processed/series.csv")
stopifnot(file.exists(infile))
df <- readr::read_csv(infile, show_col_types = FALSE)

windows <- list(
  "2010_2023"      = function(d) d,
  "2010_2019_pre"  = function(d) dplyr::filter(d, ano <= 2019),
  "2020_2023_post" = function(d) dplyr::filter(d, ano >= 2020),
  "sin_covid"      = function(d) {
    if ("covid_2020_2021" %in% names(d)) dplyr::filter(d, !covid_2020_2021 %in% c(1, TRUE)) else d
  }
)

policies <- list(
  keep   = function(d) d,
  remove = function(d) {
    if ("covid_2020_2021" %in% names(d)) dplyr::filter(d, !covid_2020_2021 %in% c(1, TRUE)) else d
  }
)

pairs <- list(
  list(y="det_tot_rate", x="share_extranjeros"),
  list(y="det_tot_rate", x="pct_extranjeros_poblacion"),
  list(y="det_ext_rate", x="share_extranjeros"),
  list(y="det_ext_rate", x="pct_extranjeros_poblacion")
)

results <- list()

run_var <- function(dat, y, x, maxlags = 2) {
  if (nrow(dat) < (maxlags + 2)) return(NULL)
  Z <- dat[, c(y, x)]
  colnames(Z) <- c("Y", "X")
  sel <- VARselect(Z, lag.max = min(maxlags, nrow(Z)-1), type = "const")
  p_used <- sel$selection[["AIC(n)"]]
  if (is.null(p_used) || is.na(p_used) || p_used < 1) p_used <- 1
  p_used <- as.integer(p_used)
  fit <- try(VAR(Z, p = p_used, type = "const"), silent = TRUE)
  if (inherits(fit, "try-error")) return(NULL)

  st <- try(roots(fit, modulus = TRUE), silent = TRUE)
  stable <- if (!inherits(st, "try-error")) all(st < 1) else NA

  ser <- try(serial.test(fit, lags.pt = max(2, 2*p_used), type = "PT.asymptotic"), silent = TRUE)
  serial_p <- if (!inherits(ser, "try-error")) as.numeric(ser$serial$p.value) else NA_real_

  g_xy <- try(causality(fit, cause = "X"), silent = TRUE)
  g_yx <- try(causality(fit, cause = "Y"), silent = TRUE)
  p_y_on_x <- if (!inherits(g_xy, "try-error")) as.numeric(g_xy$Granger$p.value) else NA_real_
  p_x_on_y <- if (!inherits(g_yx, "try-error")) as.numeric(g_yx$Granger$p.value) else NA_real_

  list(fit = fit, p_used = p_used, stable = stable,
       serial_p_value = serial_p, p_y_on_x = p_y_on_x, p_x_on_y = p_x_on_y)
}

save_irf_plot <- function(fit, impulse, response, title, outfile) {
  set.seed(123)  # reproducibilidad
  ir <- try(irf(fit, impulse = impulse, response = response, n.ahead = 8,
                boot = TRUE, runs = 500, ci = 0.95), silent = TRUE)
  if (inherits(ir, "try-error")) return(FALSE)
  v  <- ir$irf[[impulse]][, response, drop = TRUE]
  lo <- ir$Lower[[impulse]][, response, drop = TRUE]
  hi <- ir$Upper[[impulse]][, response, drop = TRUE]
  dfp <- data.frame(h = seq_along(v)-1, irf = as.numeric(v), lo = as.numeric(lo), hi = as.numeric(hi))

  p <- ggplot(dfp, aes(x = h, y = irf)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.25, fill = OI[["sky"]]) +
    geom_line(linewidth = 1, color = OI[["blue"]]) +
    labs(x = "Horizonte (años)", y = "Respuesta acumulada", title = title) +
    theme_pearl() +
    theme(legend.position = "none")
  ggsave(outfile, p, width = 7.5, height = 4.8, dpi = 150, bg = PEARL)
  TRUE
}

for (wname in names(windows)) {
  dwin <- windows[[wname]](df)
  for (pname in names(policies)) {
    dpol <- policies[[pname]](dwin)
    for (pr in pairs) {
      y <- pr$y; x <- pr$x
      if (!all(c(y, x, "ano") %in% names(dpol))) next
      dd <- dpol %>% dplyr::select(ano, all_of(c(y, x))) %>% tidyr::drop_na() %>% dplyr::arrange(ano)
      if (nrow(dd) < 8) next

      out <- run_var(dd, y, x, maxlags = 2)
      if (is.null(out)) next

      results[[length(results)+1]] <- tibble::tibble(
        policy = pname, window = wname, y = y, x = x,
        covid = ifelse(pname == "keep", TRUE, FALSE),
        p_y_on_x = out$p_y_on_x, p_x_on_y = out$p_x_on_y,
        stable = out$stable, serial_p_value = out$serial_p_value,
        n = nrow(dd), p_used = out$p_used
      )

      outfile1 <- here::here("output","figures", sprintf("24_irf_%s_%s_on_%s_%s.png", pname, y, x, wname))
      ttl1 <- sprintf("IRF: %s ← shock en %s (%s, %s)", y, x, wname, pname)
      save_irf_plot(out$fit, impulse = "X", response = "Y", title = ttl1, outfile = outfile1)

      outfile2 <- here::here("output","figures", sprintf("24_irf_%s_%s_on_%s_%s.png", pname, x, y, wname))
      ttl2 <- sprintf("IRF: %s ← shock en %s (%s, %s)", x, y, wname, pname)
      save_irf_plot(out$fit, impulse = "Y", response = "X", title = ttl2, outfile = outfile2)
    }
  }
}

if (length(results)) {
  res_tbl <- dplyr::bind_rows(results)
  readr::write_csv(res_tbl, here::here("output/tables","24_var_granger_resultados.csv"))
  message("✓ Resultados Granger guardados en output/tables/24_var_granger_resultados.csv")
} else {
  message("⚠️ No se generaron resultados (muestras insuficientes o variables ausentes).")
}

message("✅ 24 listo: VAR/Granger + IRFs por ventana y política.")