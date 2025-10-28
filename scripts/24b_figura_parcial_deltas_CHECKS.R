#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
# =============================================================================
# 24b_figura_parcial_deltas_CHECKS.R  (versión corregida p.p.)
# Autor: Jesús Castro · JESUSCASTRODATA
# Proyecto: Delitos e Inmigración en España (2010–2023)
# Objetivo: Generar figura parcial (Δ% extranjeros vs ΔHC/100k, en p.p.)
#           con validaciones completas y log de QC.
# Salidas:
#   - output/figures/24_partial_scatter_deltas_sin_covid.png
#   - output/logs/24_partial_scatter_qc.txt
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(here)
  library(rlang)
  library(tidyr)
  library(glue)
})

qc_msg <- c()
log_qc <- function(...) {
  msg <- glue::glue(...)
  message(msg)
  assign("qc_msg", c(get("qc_msg", inherits = TRUE), as.character(msg)), envir = .GlobalEnv)
}

# --------------------------- 0) Paths/Dirs -----------------------------------
csv_path <- here("data/processed/core_indicadores_con_pib.csv")
fig_path <- here("output/figures/24_partial_scatter_deltas_sin_covid.png")
log_path <- here("output/logs/24_partial_scatter_qc.txt")

dir.create(dirname(fig_path), showWarnings = FALSE, recursive = TRUE)
dir.create(dirname(log_path), showWarnings = FALSE, recursive = TRUE)

# --------------------------- 1) Existencia de archivo ------------------------
if (!file.exists(csv_path)) {
  stop(glue("No existe el CSV requerido: {csv_path}"))
} else {
  log_qc("✔ CSV localizado: {csv_path}")
}

# --------------------------- 2) Lectura de datos -----------------------------
panel <- read_csv(csv_path, show_col_types = FALSE)
log_qc("✔ CSV leído. Filas: {nrow(panel)} · Columnas: {ncol(panel)}")

# --------------------------- 3) Columnas requeridas --------------------------
req <- c("ano", "tasa_hc_100k", "pct_extranjeros", "arope", "pib_pc_real", "paro_joven")
missing <- setdiff(req, names(panel))
if (length(missing) > 0) {
  stop(glue("Faltan columnas requeridas: {paste(missing, collapse=', ')}"))
} else {
  log_qc("✔ Columnas requeridas presentes: {paste(req, collapse=', ')}")
}

# --------------------------- 4) Tipos y rangos básicos -----------------------
non_numeric <- req[vapply(panel[req], function(x) !is.numeric(x), logical(1))]
if (length(non_numeric) > 0) {
  stop(glue("Estas columnas no son numéricas: {paste(non_numeric, collapse=', ')}"))
}

# pct_extranjeros puede venir en proporción [0–1] o en % [0–100]
max_pct <- max(panel$pct_extranjeros, na.rm = TRUE)
if (max_pct > 110 || min(panel$pct_extranjeros, na.rm = TRUE) < -1) {
  stop("pct_extranjeros fuera de rango plausible. Esperado ∈ [0,1] o [0,100].")
}
if (any(panel$tasa_hc_100k < 0, na.rm = TRUE)) {
  stop("tasa_hc_100k negativa. Revisa conteos/población.")
}
log_qc("✔ Tipos numéricos y rangos plausibles OK.")

# --------------------- 5) Normalizar escala y calcular Δ ---------------------
# Detectar escala de pct_extranjeros y convertir a % (puntos porcentuales)
if (max_pct <= 1.5) {
  panel <- panel %>% mutate(pct_pp = pct_extranjeros * 100)
  log_qc("ℹ Escala: pct_extranjeros detectado como proporción [0–1] → convertido a % (p.p.).")
} else {
  panel <- panel %>% mutate(pct_pp = pct_extranjeros)
  log_qc("ℹ Escala: pct_extranjeros detectado en % [0–100] (p.p.).")
}

panel_d <- panel %>%
  arrange(ano) %>%
  mutate(
    d_tasa_hc_100k = tasa_hc_100k - dplyr::lag(tasa_hc_100k),
    d_pct_pp       = pct_pp        - dplyr::lag(pct_pp),        # Δ en p.p.
    d_arope        = arope         - dplyr::lag(arope),
    d_pib_pc_real  = pib_pc_real   - dplyr::lag(pib_pc_real),
    d_paro_joven   = paro_joven    - dplyr::lag(paro_joven)
  ) %>%
  filter(!ano %in% c(2020, 2021))  # Ventana sin COVID

log_qc("✔ Δ calculadas (en p.p. para % extranjeros) y COVID excluido (2020–2021). Filas ahora: {nrow(panel_d)}")

# --------------------------- 6) Muestra completa para modelo -----------------
panel_cc <- panel_d %>% tidyr::drop_na(d_tasa_hc_100k, d_pct_pp, d_arope, d_pib_pc_real, d_paro_joven)

n_all <- nrow(panel_d)
n_cc  <- nrow(panel_cc)
log_qc("ℹ Filas totales post-Δ (sin COVID): {n_all}; filas completas para el modelo: {n_cc}")

if (n_cc < 8) {
  stop(glue("Muestra insuficiente para un ajuste estable (n={n_cc}). Se requieren al menos 8 años completos."))
}

vars_to_check <- c("d_tasa_hc_100k", "d_pct_pp", "d_arope", "d_pib_pc_real", "d_paro_joven")
nz <- vapply(panel_cc[vars_to_check], function(x) stats::var(x, na.rm = TRUE) > 0, logical(1))
if (!all(nz)) {
  bad <- paste(vars_to_check[!nz], collapse=", ")
  stop(glue("Varianza nula en: {bad}. No se puede ajustar un modelo válido."))
}
log_qc("✔ Varianzas de Δ > 0 para todas las variables usadas.")

# --------------------------- 7) Ajustes y residuos parciales -----------------
fit_y <- lm(d_tasa_hc_100k ~ d_arope + d_pib_pc_real + d_paro_joven, data = panel_cc)
fit_x <- lm(d_pct_pp       ~ d_arope + d_pib_pc_real + d_paro_joven, data = panel_cc)

panel_cc <- panel_cc %>%
  mutate(
    resid_y = resid(fit_y),  # ΔHC/100k tras controles
    resid_x = resid(fit_x)   # Δ% (p.p.) tras controles
  )

# Regresión parcial (resid_y ~ resid_x)
fit_partial <- lm(resid_y ~ resid_x, data = panel_cc)
s <- summary(fit_partial)

slope_pp <- unname(coef(fit_partial)[2])      # pendiente por 1 p.p.
pvalue   <- unname(coef(s)[2, 4])
r2       <- s$r.squared
n_used   <- nobs(fit_partial)

log_qc("✔ Ajuste parcial (p.p.) OK: pendiente={signif(slope_pp,3)}, p={signif(pvalue,3)}, R2={round(r2,3)}, n={n_used}")

# --------------------------- 8) Tema visual ---------------------------------
# Carga el tema pearl si existe el script (no falla si no está)
if (file.exists(here::here("scripts/00_theme_figuras.R"))) {
  source(here::here("scripts/00_theme_figuras.R"))
}
get_theme <- function() if (exists("theme_pearl_lightgrid")) theme_pearl_lightgrid() else theme_minimal()
# Paleta Okabe–Ito si está definida
oi_orange <- if (exists("oi_colors")) oi_colors[["orange"]] else "#E69F00"

# --------------------------- 9) Figura --------------------------------------
lab_anno <- glue::glue("pendiente (por p.p.) = {signif(slope_pp,3)}\nR² = {round(r2,3)} · n = {n_used}")

p <- ggplot(panel_cc, aes(x = resid_x, y = resid_y)) +
  geom_point(size = 2.6, alpha = 0.9) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.0, color = oi_orange) +
  annotate("label", x = -Inf, y = Inf, label = lab_anno, hjust = -0.02, vjust = 1.1,
           size = 3.3, label.size = 0.2) +
  labs(
    title = "Relación parcial: Δ% extranjeros (p.p.) vs ΔHC (100k)",
    subtitle = "Controlando ΔAROPE, ΔPIB pc real y ΔParo 15–29 — Ventana sin COVID (2010–2019 & 2022–2023)",
    x = "Residuos de Δ% extranjeros (p.p., tras controles)",
    y = "Residuos de ΔHC por 100k (tras controles)"
  ) +
  get_theme() +
  coord_cartesian(clip = "off") +
  theme(
    plot.margin = margin(10, 12, 10, 12)
  )

# Guardado con fondo PEARL (usa tus helpers si existen)
if (exists("ggsave_pearl_png")) {
  w <- if (exists("fig_sizes")) fig_sizes$wide["w"] else 8
  h <- if (exists("fig_sizes")) fig_sizes$wide["h"] else 5
  ggsave_pearl_png(fig_path, plot = p, width = w, height = h, dpi = 300)
} else {
  ggsave(fig_path, plot = p, width = 8, height = 5, dpi = 300,
         bg = if (exists("PEARL_BG")) PEARL_BG else "white")
}
log_qc("✔ Figura guardada en: {fig_path}")


# --------------------------- 10) Guardar QC log ------------------------------
years_all <- paste(panel_d$ano, collapse = ", ")
years_cc  <- paste(panel_cc$ano, collapse = ", ")

qc_block <- c(
  "== 24_partial_scatter_deltas_sin_covid · QC ==",
  paste("CSV:", csv_path),
  paste("Filas post-Δ (sin COVID):", n_all),
  paste("Filas completas usadas (n):", n_cc),
  paste("Años disponibles (post-Δ, sin COVID):", years_all),
  paste("Años usados en el ajuste:", years_cc),
  paste("Pendiente parcial (por p.p.):", signif(slope_pp, 5)),
  paste("p-valor:", signif(pvalue, 5)),
  paste("R2:", round(r2, 4)),
  paste("Figura:", fig_path),
  "",
  "Mensajes:",
  qc_msg
)
writeLines(qc_block, con = log_path)
message("✔ QC log escrito en: ", log_path)
