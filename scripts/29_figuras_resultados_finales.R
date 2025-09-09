#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
###############################################################################
# 29_figuras_resultados_finales.R — Figuras finales: coefplots, ECM alpha y mosaicos IRF.
#
# Autor: JESUS CASTRO
# Fecha: 2025-09-04
###############################################################################
suppressPackageStartupMessages({
  library(dplyr); library(readr); library(here); library(janitor)
  library(tidyr); library(ggplot2); library(scales); library(fs); library(stringr)
  library(grid)
})

fs::dir_create(here::here("output","figures"))
fs::dir_create(here::here("output","tables"))

theme_ok <- tryCatch({
  source(here::here("scripts","00_theme_figuras.R"), local = TRUE)
  TRUE
}, error = function(e) FALSE)

if (!isTRUE(theme_ok)) {
  PEARL <- "#FAF9F6"
  OI <- c("#000000","#E69F00","#56B4E9","#009E73",
          "#F0E442","#0072B2","#D55E00","#CC79A7","#999999")
  theme_pearl <- function(base_size = 12) {
    ggplot2::theme_minimal(base_size = base_size) +
      ggplot2::theme(
        plot.background  = ggplot2::element_rect(fill = PEARL, colour = NA),
        panel.background = ggplot2::element_rect(fill = PEARL, colour = NA),
        legend.background= ggplot2::element_rect(fill = PEARL, colour = NA),
        legend.key       = ggplot2::element_rect(fill = PEARL, colour = NA)
      )
  }
  ggsave_pearl <- function(filename, plot = ggplot2::last_plot(),
                           width = 12, height = 7, dpi = 300, units = "in", ...) {
    fs::dir_create(dirname(filename))
    ggplot2::ggsave(filename, plot, width = width, height = height,
                    dpi = dpi, units = units, bg = PEARL, ...)
  }
  message("Cargado tema fallback 'pearl' y utilidades.")
}

has_cols <- function(df, cols) all(cols %in% names(df))

# 0) Resumen Granger
fp24 <- here::here("output","tables","24_var_granger_resultados.csv")
if (file.exists(fp24)) {
  gr <- readr::read_csv(fp24, show_col_types = FALSE) |> janitor::clean_names()
  need24 <- c("policy","window","y","x","covid","p_y_on_x","p_x_on_y","stable","serial_p_value","n","p_used")
  if (has_cols(gr, need24)) {
    gr2 <- gr |>
      dplyr::mutate(
        robust = !is.na(.data$stable) & .data$stable &
          is.finite(.data$serial_p_value) & .data$serial_p_value >= 0.05 &
          .data$n >= 8,
        X_causes_Y = is.finite(.data$p_y_on_x) & .data$p_y_on_x < 0.05,
        Y_causes_X = is.finite(.data$p_x_on_y) & .data$p_x_on_y < 0.05
      )
    res_gr <- gr2 |>
      dplyr::group_by(.data$policy, .data$window, .data$y) |>
      dplyr::summarise(
        n_models     = dplyr::n(),
        n_robust     = sum(.data$robust, na.rm = TRUE),
        n_X_causes_Y = sum(.data$robust & .data$X_causes_Y, na.rm = TRUE),
        n_Y_causes_X = sum(.data$robust & .data$Y_causes_X, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::arrange(.data$policy, .data$window, .data$y)
    readr::write_csv(res_gr, here::here("output","tables","29_granger_resumen.csv"))
  } else {
    message("24_var_granger_resultados.csv sin columnas esperadas; omito resumen Granger.")
  }
} else {
  message("No existe 24_var_granger_resultados.csv; omito resumen Granger.")
}

# 1) Coefplots Δlog (script 26)
fp26 <- here::here("output","tables","26_dlog_overview.csv")

if (file.exists(fp26)) {
  ov <- readr::read_csv(fp26, show_col_types = FALSE) |> janitor::clean_names()
  need26 <- c("y","x","window","spec","coef_x","se_x","p_x")
  if (!has_cols(ov, need26)) {
    message("26_dlog_overview.csv no trae columnas esperadas; omito coefplot.")
  } else {
    x_candidates <- ov |> dplyr::filter(!is.na(.data$x)) |> dplyr::pull(.data$x) |> unique()
    x_main <- if ("d_l_share_extranjeros" %in% x_candidates) "d_l_share_extranjeros" else x_candidates[1]
    ov1 <- ov |> dplyr::filter(.data$x == x_main)
    labY <- c(
      d_l_hc_rate       = "dlog(HC)",
      d_l_he_rate       = "dlog(HE)",
      d_l_det_tot_rate  = "dlog(Detenciones totales)",
      d_l_det_ext_rate  = "dlog(Detenciones extranjeros)"
    )
    labSpec <- c(A="A: bivar", B="B: +controles", C="C: +controles + dlog(Y[-1])")
    labWin  <- c(`2010_2023`="2010–2023", `2010_2019_pre`="2010–2019",
                 `2020_2023_post`="2020–2023", sin_covid="Sin COVID",
                 `2010_2023_sin_covid`="2010–2023 (sin COVID)")
    plt <- ov1 |>
      dplyr::mutate(
        y_lab   = dplyr::recode(.data$y, !!!labY, .default = .data$y),
        spec_lab= dplyr::recode(.data$spec, !!!labSpec, .default = .data$spec),
        win_lab = dplyr::recode(.data$window, !!!labWin, .default = .data$window),
        sig = dplyr::case_when(
          is.finite(.data$p_x) & .data$p_x < 0.01 ~ "***",
          is.finite(.data$p_x) & .data$p_x < 0.05 ~ "**",
          is.finite(.data$p_x) & .data$p_x < 0.10 ~ "*",
          TRUE ~ "ns"
        )
      ) |>
      dplyr::filter(is.finite(.data$coef_x), is.finite(.data$se_x)) |>
      dplyr::mutate(lo = .data$coef_x - 1.96*.data$se_x,
                    hi = .data$coef_x + 1.96*.data$se_x)
    if (nrow(plt) > 0) {
      specs <- unique(plt$spec_lab)
      col_map <- stats::setNames(OI[pmin(seq_along(specs), length(OI))], specs)
      g_coef <- ggplot2::ggplot(plt, ggplot2::aes(x = win_lab, y = coef_x, ymin = lo, ymax = hi,
                                                  colour = spec_lab, shape = sig, group = spec_lab)) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.5, alpha = 0.7) +
        ggplot2::geom_pointrange(position = ggplot2::position_dodge(width = 0.6), size = 0.4) +
        ggplot2::facet_wrap(~ y_lab, scales = "free_y") +
        ggplot2::scale_color_manual(values = col_map, guide = ggplot2::guide_legend(title = "Especificación")) +
        ggplot2::scale_shape_manual(values = c("ns"=16, "*"=17, "**"=15, "***"=8),
                                    breaks = c("***","**","*","ns"),
                                    labels = c("***","**","*","ns"),
                                    guide = ggplot2::guide_legend(title = "p-valor")) +
        ggplot2::labs(title = "Elasticidades contemporáneas (dlog(Y) ~ dlog(X))",
                      subtitle = paste0("X = ", x_main, " | IC95% | símbolos = significancia"),
                      x = "Ventana", y = "Coeficiente (elasticidad)") +
        theme_pearl() + ggplot2::theme(legend.position = "bottom")
      ggsave_pearl(here::here("output","figures","29_coefplot_dlog.png"), g_coef, width = 12, height = 7)
    } else message("No hay filas válidas para coefplot en 26_dlog_overview.csv.")
    extra_cols <- intersect(c("n","r_squared","adj_r2"), names(ov1))
    resumen <- ov1 |>
      dplyr::mutate(
        signo = dplyr::case_when(
          is.finite(.data$coef_x) & .data$coef_x > 0 ~ "positivo",
          is.finite(.data$coef_x) & .data$coef_x < 0 ~ "negativo",
          TRUE ~ NA_character_
        ),
        signif = dplyr::case_when(
          is.finite(.data$p_x) & .data$p_x < 0.01 ~ "p<0.01",
          is.finite(.data$p_x) & .data$p_x < 0.05 ~ "p<0.05",
          is.finite(.data$p_x) & .data$p_x < 0.10 ~ "p<0.10",
          is.finite(.data$p_x)                   ~ "ns",
          TRUE ~ NA_character_
        )
      ) |>
      dplyr::select(dplyr::any_of(c("window","y","x","spec","coef_x","se_x","p_x","signo","signif", extra_cols))) |>
      dplyr::arrange(.data$y, .data$window, .data$spec)
    readr::write_csv(resumen, here::here("output","tables","29_resumen_signos.csv"))
  }
} else {
  message("No existe 26_dlog_overview.csv; omito coefplot y resumen de signos.")
}

# 2) ECM alpha
fp25 <- here::here("output","tables","25_ecm_coefs.csv")
if (file.exists(fp25)) {
  ecm <- readr::read_csv(fp25, show_col_types = FALSE) |> janitor::clean_names()
  if (!"x" %in% names(ecm)) ecm <- dplyr::mutate(ecm, x = "(X no especificada en 25)")
  need25 <- c("term","estimate","std_error","p_value","window","y","x")
  if (has_cols(ecm, need25)) {
    ecm1 <- ecm |>
      dplyr::filter(.data$term == "ecm1") |>
      dplyr::mutate(
        sig = dplyr::case_when(
          is.finite(.data$p_value) & .data$p_value < 0.01 ~ "***",
          is.finite(.data$p_value) & .data$p_value < 0.05 ~ "**",
          is.finite(.data$p_value) & .data$p_value < 0.10 ~ "*",
          TRUE ~ "ns"
        ),
        y_lab = dplyr::recode(.data$y,
          l_hc_rate="log(HC)", l_he_rate="log(HE)",
          l_det_tot_rate="log(Detenciones totales)", l_det_ext_rate="log(Detenciones extranjeros)",
          .default = .data$y),
        win_lab = dplyr::recode(.data$window,
          `2010_2023`="2010–2023", `2010_2019_pre`="2010–2019",
          `2020_2023_post`="2020–2023", sin_covid="Sin COVID",
          `2010_2023_sin_covid`="2010–2023 (sin COVID)",
          .default = .data$window)
      ) |>
      dplyr::filter(is.finite(.data$estimate), is.finite(.data$std_error)) |>
      dplyr::mutate(lo = .data$estimate - 1.96*.data$std_error,
                    hi = .data$estimate + 1.96*.data$std_error)
    if (nrow(ecm1) > 0) {
      xs <- unique(ecm1$x)
      col_map_x <- stats::setNames(OI[pmin(seq_along(xs), length(OI))], xs)
      g_ecm <- ggplot2::ggplot(ecm1, ggplot2::aes(x = win_lab, y = estimate, ymin = lo, ymax = hi, colour = x)) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.5, alpha = 0.7) +
        ggplot2::geom_pointrange(position = ggplot2::position_dodge(width = 0.6), size = 0.4) +
        ggplot2::facet_wrap(~ y_lab, scales = "free_y") +
        ggplot2::scale_color_manual(values = col_map_x, guide = ggplot2::guide_legend(title = "X (niveles)")) +
        ggplot2::labs(title = "Velocidad de ajuste ECM (alpha en ecm1)",
                      subtitle = "Se espera alpha < 0 si hay cointegración. IC95%.",
                      x = "Ventana", y = "alpha (ecm1)") +
        theme_pearl() + ggplot2::theme(legend.position = "bottom")
      ggsave_pearl(here::here("output","figures","29_ecm_alpha.png"), g_ecm, width = 12, height = 7)
    } else message("25_ecm_coefs.csv no tiene filas ecm1 válidas para graficar.")
  } else message("25_ecm_coefs.csv sin columnas esperadas; omito figura ECM.")
} else message("No existe 25_ecm_coefs.csv; omito figura ECM.")

# 3) IRF mosaics
make_irf_grid <- function(policy, out_png){
  patt <- sprintf("^24_irf_%s_.*\\.png$", policy)
  files <- list.files(here::here("output","figures"), pattern = patt, full.names = TRUE)
  if (!length(files)) return(FALSE)
  files <- sort(files)
  grob_from_png <- function(fp) {
    if (requireNamespace("png", quietly = TRUE)) { img <- png::readPNG(fp); grid::rasterGrob(img, interpolate = TRUE) }
    else { grid::textGrob(basename(fp)) }
  }
  grobs <- lapply(files, grob_from_png)
  n <- length(grobs); ncol <- if (n >= 9) 3 else if (n >= 4) 2 else 1; nrow <- ceiling(n / ncol)
  grDevices::png(out_png, width = 1400, height = 900, res = 120, bg = "white")
  grid::grid.newpage(); grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow, ncol)))
  k <- 1L
  for (i in seq_len(nrow)) for (j in seq_len(ncol)) {
    if (k <= n) { vp <- grid::viewport(layout.pos.row = i, layout.pos.col = j); grid::pushViewport(vp); grid::grid.draw(grobs[[k]]); grid::upViewport(); k <- k + 1L }
  }
  grid::grid.text(sprintf("IRFs (policy = %s)", policy), y = grid::unit(1, "npc") - grid::unit(10, "pt"))
  grDevices::dev.off(); TRUE
}
ok_keep   <- make_irf_grid("keep",   here::here("output","figures","29_irf_grid_keep.png"))
ok_remove <- make_irf_grid("remove", here::here("output","figures","29_irf_grid_remove.png"))
if (!ok_keep && !ok_remove) {
  files_legacy <- list.files(here::here("output","figures"), pattern = "^24_irf_.*\\.png$", full.names = TRUE)
  if (length(files_legacy)) {
    message("IRFs legacy detectadas (sin 'policy' en el nombre). Genero 29_irf_grid_legacy.png")
    make_irf_grid_from_files <- function(files, out_png){
      files <- sort(files)
      grob_from_png <- function(fp) {
        if (requireNamespace("png", quietly = TRUE)) { img <- png::readPNG(fp); grid::rasterGrob(img, interpolate = TRUE) }
        else { grid::textGrob(basename(fp)) }
      }
      grobs <- lapply(files, grob_from_png)
      n <- length(grobs); ncol <- if (n >= 9) 3 else if (n >= 4) 2 else 1; nrow <- ceiling(n / ncol)
      grDevices::png(out_png, width = 1400, height = 900, res = 120, bg = "white")
      grid::grid.newpage(); grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow, ncol)))
      k <- 1L
      for (i in seq_len(nrow)) for (j in seq_len(ncol)) {
        if (k <= n) { vp <- grid::viewport(layout.pos.row = i, layout.pos.col = j); grid::pushViewport(vp); grid::grid.draw(grobs[[k]]); grid::upViewport(); k <- k + 1L }
      }
      grid::grid.text("IRFs (legacy)", y = grid::unit(1, "npc") - grid::unit(10, "pt"))
      grDevices::dev.off(); TRUE
    }
    make_irf_grid_from_files(files_legacy, here::here("output","figures","29_irf_grid_legacy.png"))
  } else message("No hay PNGs de IRFs; omito mosaicos.")
}
message("✅ 29 listo: coefplot, ECM, mosaicos IRF (si procede) y resúmenes.")