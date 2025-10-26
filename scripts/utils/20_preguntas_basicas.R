#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
################################################################################
# 20_preguntas_basicas.R — Correlaciones básicas y parciales (robusto)
# Proyecto   : Delitos e Inmigración en España (2010–2023)
# Autor      : Jesús Castro · JESUSCASTRODATA
# Licencia   : MIT (código) · CC BY 4.0 (docs/figuras)
# Fecha      : 2025-09-28
# Descripción:
#   - Correlaciones en NIVELES y en Δ (Pearson/Spearman/Kendall*),
#     y PARCIALES (niveles: pib_pc_real, paro_joven, arope; Δ: dln_pib_pc).
#   - Ventanas: 2010–2023 y sin COVID (2010–2019 & 2022–2023).
#   - Reconstrucción robusta de pct_extranjeros si falta.
#   - Exporta CSVs, figuras y resumen Markdown (“20_pb_summary.md”).
#   * Kendall solo en simples (no parciales).
################################################################################

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(tidyr); library(janitor)
  library(stringr); library(here); library(fs); library(ggplot2); library(purrr)
  library(rlang)
})

`%||%` <- function(x, y) if (is.null(x) || is.na(x)) y else x
msg <- function(...) message("[20] ", paste0(...))

YEAR_MIN <- 2010L; YEAR_MAX <- 2023L

# -------------------- Rutas --------------------
fp_core <- here::here("data","processed","core_indicadores.csv")
fp_der  <- here::here("data","processed","core_indicadores_derivadas.csv")
stopifnot(file.exists(fp_core))
if (!file.exists(fp_der)) msg("⚠ No existe core_indicadores_derivadas.csv; se omiten Δ y parciales en Δ.")

out_tab <- here::here("output","tables");  fs::dir_create(out_tab)
out_fig <- here::here("output","figures"); fs::dir_create(out_fig)

# -------------------- Tema figuras (pearl + Okabe–Ito) -----------------------
theme_fp <- here::here("scripts","00_theme_figuras.R")
if (file.exists(theme_fp)) {
  source(theme_fp)
  msg("✓ Tema figuras cargado (00_theme_figuras.R).")
} else {
  msg("⚠ No se encontró 00_theme_figuras.R; uso theme_minimal().")
}

# -------------------- Carga datos --------------------
core <- readr::read_csv(fp_core, show_col_types = FALSE) |> clean_names()
der  <- if (file.exists(fp_der)) readr::read_csv(fp_der, show_col_types = FALSE) |> clean_names() else tibble()

msg("Columnas en core: ", paste(names(core), collapse=", "))
if (nrow(der)) msg("Columnas en derivadas: ", paste(names(der), collapse=", "))

# -------------------- Detectores flexibles --------------------
pick_one <- function(nms, patterns) {
  hit <- nms[Reduce("|", lapply(patterns, function(p) grepl(p, nms, ignore.case = TRUE)))]
  if (length(hit)) hit[1] else NA_character_
}

N <- names(core)
col_share_det <- pick_one(N, c("^share_extran", "share.*deten", "porc.*deten.*extran"))
col_tasa_hc   <- pick_one(N, c("^tasa.*hc", "hc_.*100k", "conoc.*100"))
col_tasa_he   <- pick_one(N, c("^tasa.*he", "esclarec.*100k"))
col_pib       <- pick_one(N, c("^pib.*real", "pib_pc"))
col_paro      <- pick_one(N, c("paro.*(joven|15.*29|16.*29)"))
col_arope     <- pick_one(N, c("^arope", "riesgo.*pobreza|exclusion"))

# --------- pct_extranjeros (0–1) con fallback robusto ----------
find_pct_extran <- function(core, der) {
  cand_core <- c("pct_extranjeros","pct_extranjeros_poblacion","pct_extranjeros_total","porcentaje_extranjeros")
  hit <- intersect(cand_core, names(core))
  if (length(hit)) return(core |> select(ano, pct_extranjeros = any_of(hit[1])))
  if (nrow(der)) {
    cand_der <- c("pct_extranjeros","pct_extranjeros_poblacion")
    hit2 <- intersect(cand_der, names(der))
    if (length(hit2)) return(der |> select(ano, pct_extranjeros = any_of(hit2[1])))
  }
  fpp <- here::here("data","processed","pct_extranjeros_poblacion.csv")
  if (file.exists(fpp)) {
    df <- readr::read_csv(fpp, show_col_types = FALSE) |> clean_names()
    cand_file <- intersect(c("pct_extranjeros","pct_extranjeros_poblacion"), names(df))
    if (length(cand_file)) return(df |> select(ano, pct_extranjeros = any_of(cand_file[1])))
  }
  f_ext <- here::here("data","processed","poblacion_extranjera_total.csv")
  f_tot <- here::here("data","processed","poblacion_total_nacional.csv")
  if (file.exists(f_ext) && file.exists(f_tot)) {
    E  <- readr::read_csv(f_ext, show_col_types = FALSE) |> clean_names()
    Tt <- readr::read_csv(f_tot, show_col_types = FALSE) |> clean_names()
    nmE <- pick_one(names(E),  c("poblacion_extranjera","extranjera"))
    nmT <- pick_one(names(Tt), c("poblacion_total","total"))
    if (!is.na(nmE) && !is.na(nmT)) {
      return(E |> select(ano, extranjera = any_of(nmE)) |>
               left_join(Tt |> select(ano, total = any_of(nmT)), by="ano") |>
               mutate(pct_extranjeros = ifelse(total > 0, extranjera/total, NA_real_)) |>
               select(ano, pct_extranjeros))
    }
  }
  tibble()
}

pct_df <- find_pct_extran(core, der)
if (!nrow(pct_df)) {
  msg("⚠ No se pudo obtener pct_extranjeros; se omitirán análisis que lo requieran.")
} else {
  msg("✓ pct_extranjeros disponible (", nrow(pct_df), " filas).")
}

# -------------------- DF estándar --------------------
std_map <- c(
  share_extranjeros = col_share_det,
  tasa_hc_100k      = col_tasa_hc,
  tasa_he_100k      = col_tasa_he,
  pib_pc_real       = col_pib,
  paro_joven        = col_paro,
  arope             = col_arope
)

DF <- core |> transmute(
  ano,
  !!!setNames(lapply(names(std_map), function(k) {
    v <- std_map[[k]]
    if (is.na(v) || !(v %in% names(core))) NULL else rlang::sym(v)
  }), names(std_map))
)

if (nrow(pct_df)) DF <- DF |> left_join(pct_df, by = "ano")

drv <- if (nrow(der)) der |> select(any_of(c(
  "ano",
  "dln_pib_pc","dln_hc","dln_he",
  "pct_extranjeros","pct_extranjeros_l1","d_pct_extran",
  "d_share_ext","d_tasa_hc_100k","d_tasa_he_100k"
))) |> clean_names() else tibble()

# Evitar colisión si ya añadimos pct_extranjeros desde pct_df
if (nrow(drv) && "pct_extranjeros" %in% names(drv) && "pct_extranjeros" %in% names(DF)) {
  drv <- drv |> select(-pct_extranjeros)
}

DF <- DF |> left_join(drv, by = "ano") |>
  arrange(ano) |>
  filter(dplyr::between(ano, YEAR_MIN, YEAR_MAX))

# ===== Alias de Δ y construcción automática de deltas si faltan =====
if ("d_pct_extran" %in% names(DF) && !"d_pct_extranjeros" %in% names(DF)) {
  DF <- DF |> mutate(d_pct_extranjeros = .data[["d_pct_extran"]])
}
if ("d_share_ext" %in% names(DF) && !"d_share_extranjeros" %in% names(DF)) {
  DF <- DF |> mutate(d_share_extranjeros = .data[["d_share_ext"]])
}
if (!"d_tasa_hc_100k" %in% names(DF) && "tasa_hc_100k" %in% names(DF)) {
  DF <- DF |> arrange(ano) |> mutate(d_tasa_hc_100k = tasa_hc_100k - dplyr::lag(tasa_hc_100k))
}
if (!"d_tasa_he_100k" %in% names(DF) && "tasa_he_100k" %in% names(DF)) {
  DF <- DF |> arrange(ano) |> mutate(d_tasa_he_100k = tasa_he_100k - dplyr::lag(tasa_he_100k))
}

# -------------------- Variables presentes --------------------
present <- function(vars, df) intersect(na.omit(vars), names(df))

level_X     <- present(c("pct_extranjeros"), DF)
level_Y     <- present(c("share_extranjeros","tasa_hc_100k","tasa_he_100k"), DF)
delta_X     <- present(c("d_pct_extranjeros"), DF)
delta_Y     <- present(c("d_share_extranjeros","d_tasa_hc_100k","d_tasa_he_100k"), DF)
ctrl_levels <- present(c("pib_pc_real","paro_joven","arope"), DF)
ctrl_deltas <- present(c("dln_pib_pc"), DF)

msg("Vars nivel X: ", paste(level_X, collapse=", "))
msg("Vars nivel Y: ", paste(level_Y, collapse=", "))
msg("Controles niveles: ", paste(ctrl_levels, collapse=", "))
msg("Vars Δ X: ", paste(delta_X, collapse=", "))
msg("Vars Δ Y: ", paste(delta_Y, collapse=", "))
msg("Controles Δ: ", paste(ctrl_deltas, collapse=", "))

# -------------------- Helpers correlación con p-val --------------------
one_cor <- function(x, y, method = "pearson") {
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]; y <- y[ok]
  if (length(x) < 3) return(c(r=NA_real_, p=NA_real_, n=length(x)))
  ct <- suppressWarnings(cor.test(x, y, method = method, exact = FALSE))
  c(r = unname(ct$estimate), p = unname(ct$p.value), n = length(x))
}

corr_grid <- function(df, Xs, Ys, methods = c("pearson","spearman","kendall")) {
  if (!length(Xs) || !length(Ys)) return(tibble())
  purrr::map_dfr(Xs, function(xv) {
    purrr::map_dfr(Ys, function(yv) {
      out <- tibble(x = xv, y = yv)
      for (m in methods) {
        s <- one_cor(df[[xv]], df[[yv]], method = m)
        out[[paste0(m,"_r")]] <- s["r"]; out[[paste0(m,"_p")]] <- s["p"]; out[["n"]] <- s["n"]
      }
      out
    })
  })
}

partial_two <- function(df, Xs, Ys, controls, methods = c("pearson","spearman")) {
  if (!length(Xs) || !length(Ys) || !length(controls)) return(tibble())
  controls <- intersect(controls, names(df)); if (!length(controls)) return(tibble())
  purrr::map_dfr(Xs, function(xv) {
    purrr::map_dfr(Ys, function(yv) {
      d <- df |> select(any_of(c(xv, yv, controls))) |> drop_na()
      if (nrow(d) < (length(controls) + 3))
        return(tibble(x=xv, y=yv, method="pearson", r=NA_real_, p=NA_real_, n=nrow(d)))
      # Residualización
      rX <- lm(d[[xv]] ~ ., data = d[, controls, drop=FALSE])$residuals
      rY <- lm(d[[yv]] ~ ., data = d[, controls, drop=FALSE])$residuals
      tibble(
        x = xv, y = yv, n = length(rX),
        method = c("pearson","spearman"),
        r = c(one_cor(rX, rY, "pearson")["r"], one_cor(rank(rX), rank(rY), "pearson")["r"]),
        p = c(one_cor(rX, rY, "pearson")["p"], one_cor(rank(rX), rank(rY), "pearson")["p"])
      )
    })
  })
}

# -------------------- Subsets temporales --------------------
no_covid <- function(df) dplyr::filter(df, ano <= 2019 | ano >= 2022)

# -------------------- Bloques de análisis --------------------
run_block <- function(df, label) {
  # Simples: niveles
  if (length(level_X) && length(level_Y)) {
    out <- corr_grid(df, level_X, level_Y, methods = c("pearson","spearman","kendall"))
    write_csv(out, file.path(out_tab, sprintf("20_corr_niveles_%s.csv", label)))
  } else msg("⚠ Omito correlaciones en niveles (faltan X o Y).")
  # Parciales: niveles
  if (length(level_X) && length(level_Y) && length(ctrl_levels)) {
    p <- partial_two(df, level_X, level_Y, controls = ctrl_levels, methods = c("pearson","spearman"))
    write_csv(p, file.path(out_tab, sprintf("20_pcor_niveles_%s.csv", label)))
  } else msg("⚠ Omito parciales en niveles (faltan controles o X/Y).")
  
  # Simples: deltas
  if (length(delta_X) && length(delta_Y)) {
    outd <- corr_grid(df, delta_X, delta_Y, methods = c("pearson","spearman","kendall"))
    write_csv(outd, file.path(out_tab, sprintf("20_corr_deltas_%s.csv", label)))
  } else msg("⚠ Omito correlaciones en Δ (faltan X o Y).")
  # Parciales: deltas
  if (length(delta_X) && length(delta_Y) && length(ctrl_deltas)) {
    pd <- partial_two(df, delta_X, delta_Y, controls = ctrl_deltas, methods = c("pearson","spearman"))
    write_csv(pd, file.path(out_tab, sprintf("20_pcor_deltas_%s.csv", label)))
  } else msg("⚠ Omito parciales en Δ (faltan controles o X/Y).")
  
  # Gráficos scatter (tema pearl + guardado pearl si está disponible)
  plot_pairs <- function(df, xs, ys, prefix) {
    xs <- intersect(xs, names(df)); ys <- intersect(ys, names(df))
    if (!length(xs) || !length(ys)) return(invisible(NULL))
    for (xv in xs) for (yv in ys) {
      d2 <- df %>%
        transmute(x = .data[[xv]], y = .data[[yv]]) %>%
        dplyr::filter(is.finite(x) & is.finite(y)) %>%
        tidyr::drop_na()
      if (nrow(d2) < 2) next
      p <- ggplot(d2, aes(x, y)) +
        geom_point() +
        { if (nrow(d2) >= 3) geom_smooth(method = "lm", se = FALSE) else NULL } +
        labs(title = sprintf("%s vs %s (%s)", yv, xv, label),
             x = xv, y = yv,
             caption = "Delitos e Inmigración en España (2010–2023)") +
        (if (exists("theme_pearl_lightgrid")) theme_pearl_lightgrid() else theme_minimal(base_size = 11))
      out_file <- file.path(out_fig, sprintf("20_%s_%s_vs_%s_%s.png", prefix, yv, xv, label))
      suppressMessages(
        (if (exists("ggsave_pearl")) ggsave_pearl else ggsave)(
          filename = out_file, plot = p, width = 6, height = 4, dpi = 120
        )
      )
    }
  }
  plot_pairs(df, level_X, level_Y, "niv")
  plot_pairs(df, delta_X, delta_Y, "del")
}

msg("Ventanas: all (2010–2023) y sin COVID (2010–2019 & 2022–2023).")
DF_all  <- DF |> dplyr::filter(dplyr::between(ano, YEAR_MIN, YEAR_MAX))
DF_ncvd <- DF_all |> no_covid()
run_block(DF_all,  "all")
run_block(DF_ncvd, "sin_covid")

# -------------------- Resumen directo desde memoria (evita descuadres con CSV) --------------------
mk_pair <- function(df, x, y, label, method="pearson") {
  if (!all(c(x,y) %in% names(df))) return(sprintf("- %s: N/D", label))
  ok <- is.finite(df[[x]]) & is.finite(df[[y]])
  xv <- df[[x]][ok]; yv <- df[[y]][ok]
  if (length(xv) < 3) return(sprintf("- %s: N/D", label))
  ct <- suppressWarnings(cor.test(xv, yv, method = method, exact = FALSE))
  r <- unname(ct$estimate); p <- unname(ct$p.value); n <- length(xv)
  strength <- function(r) { a <- abs(r); if (a >= .70) "alta" else if (a >= .40) "moderada" else if (a >= .20) "baja" else "muy baja" }
  sigflag  <- function(p) if (p <= 0.001) "***" else if (p <= 0.01) "**" else if (p <= 0.05) "*" else ""
  sprintf("- %s: %s (r=%.3f, p=%.3g, n=%d)%s", label, strength(r), r, p, n, sigflag(p))
}

lines <- c(
  "# 20 · Preguntas básicas — Resumen",
  "",
  "## Niveles (2010–2023)",
  mk_pair(DF_all,  "pct_extranjeros","tasa_hc_100k",  "¿Cómo coevoluciona % de extranjeros con la **tasa de hechos conocidos**?"),
  mk_pair(DF_all,  "pct_extranjeros","tasa_he_100k",  "¿Y con la **tasa de hechos esclarecidos**?"),
  mk_pair(DF_all,  "pct_extranjeros","share_extranjeros", "¿Se mueve el **share de detenciones de extranjeros** con el % de extranjeros?"),
  "",
  "## Niveles (sin COVID: 2010–2019 & 2022–2023)",
  mk_pair(DF_ncvd, "pct_extranjeros","tasa_hc_100k",  "Robustez sin COVID — % extranjeros vs **tasa HC**"),
  mk_pair(DF_ncvd, "pct_extranjeros","tasa_he_100k",  "Robustez sin COVID — % extranjeros vs **tasa HE**"),
  mk_pair(DF_ncvd, "pct_extranjeros","share_extranjeros", "Robustez sin COVID — % extranjeros vs **share detenciones extranjeros**"),
  "",
  "## Variaciones (Δ, 2010–2023)",
  mk_pair(DF_all,  "d_pct_extranjeros","d_tasa_hc_100k",  "¿Cambios en % extranjeros se asocian con cambios en **tasa HC**?"),
  mk_pair(DF_all,  "d_pct_extranjeros","d_tasa_he_100k",  "¿…y con **tasa HE**?"),
  mk_pair(DF_all,  "d_pct_extranjeros","d_share_extranjeros", "¿…y con **share detenciones extranjeros**?"),
  "",
  "## Variaciones (Δ, sin COVID)",
  mk_pair(DF_ncvd,"d_pct_extranjeros","d_tasa_hc_100k",  "Robustez sin COVID — Δ% extranjeros vs **Δ tasa HC**"),
  mk_pair(DF_ncvd,"d_pct_extranjeros","d_tasa_he_100k",  "Robustez sin COVID — Δ% extranjeros vs **Δ tasa HE**"),
  mk_pair(DF_ncvd,"d_pct_extranjeros","d_share_extranjeros", "Robustez sin COVID — Δ% extranjeros vs **Δ share detenciones**"),
  "",
  "_Notas: fuerza = |r| alta ≥ 0.70; moderada ≥ 0.40; baja ≥ 0.20. Asteriscos: * p≤0.05, ** p≤0.01, *** p≤0.001._"
)

writeLines(lines, file.path(out_tab, "20_pb_summary.md"))

# -------------------- Añadir correlaciones parciales al Markdown --------------------
mk_partial <- function(df, x, y, controls, label, method="pearson") {
  controls <- intersect(controls, names(df))
  if (!all(c(x,y) %in% names(df)) || !length(controls)) return(sprintf("- %s: N/D", label))
  d <- df |> dplyr::select(any_of(c(x, y, controls))) |> tidyr::drop_na()
  if (nrow(d) < (length(controls) + 3)) return(sprintf("- %s: N/D", label))
  rX <- lm(d[[x]] ~ ., data = d[, controls, drop=FALSE])$residuals
  rY <- lm(d[[y]] ~ ., data = d[, controls, drop=FALSE])$residuals
  ct <- suppressWarnings(cor.test(rX, rY, method = method, exact = FALSE))
  r <- unname(ct$estimate); p <- unname(ct$p.value); n <- length(rX)
  strength <- function(r) { a <- abs(r); if (a >= .70) "alta" else if (a >= .40) "moderada" else if (a >= .20) "baja" else "muy baja" }
  sigflag  <- function(p) if (p <= 0.001) "***" else if (p <= 0.01) "**" else if (p <= 0.05) "*" else ""
  sprintf("- %s: %s (r=%.3f, p=%.3g, n=%d)%s", label, strength(r), r, p, n, sigflag(p))
}

partial_lines <- c(
  "",
  "## Correlaciones parciales (niveles)",
  mk_partial(DF_all,  "pct_extranjeros","tasa_hc_100k",  c("pib_pc_real","paro_joven","arope"),
             "Parcial (controles: PIB pc real, paro joven, AROPE) — % extranjeros vs **tasa HC**"),
  mk_partial(DF_all,  "pct_extranjeros","tasa_he_100k",  c("pib_pc_real","paro_joven","arope"),
             "Parcial (controles: PIB pc real, paro joven, AROPE) — % extranjeros vs **tasa HE**"),
  mk_partial(DF_all,  "pct_extranjeros","share_extranjeros",  c("pib_pc_real","paro_joven","arope"),
             "Parcial (controles: PIB pc real, paro joven, AROPE) — % extranjeros vs **share detenciones extranjeros**"),
  "",
  "## Correlaciones parciales (Δ)",
  mk_partial(DF_all,  "d_pct_extranjeros","d_tasa_hc_100k",  c("dln_pib_pc"),
             "Parcial Δ (control: Δ ln PIB pc) — Δ% extranjeros vs **Δ tasa HC**"),
  mk_partial(DF_all,  "d_pct_extranjeros","d_tasa_he_100k",  c("dln_pib_pc"),
             "Parcial Δ (control: Δ ln PIB pc) — Δ% extranjeros vs **Δ tasa HE**"),
  mk_partial(DF_all,  "d_pct_extranjeros","d_share_extranjeros",  c("dln_pib_pc"),
             "Parcial Δ (control: Δ ln PIB pc) — Δ% extranjeros vs **Δ share detenciones**")
)

md_path <- file.path(out_tab, "20_pb_summary.md")
cat(paste0("\n", paste(partial_lines, collapse="\n")), file = md_path, append = TRUE)

msg("✅ Tablas: output/tables ; Figuras: output/figures ; Resumen: output/tables/20_pb_summary.md (incluye parciales)")
