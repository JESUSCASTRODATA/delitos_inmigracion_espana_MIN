#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
###############################################################################
# 23b_base100_hc_poblaciones.R
#
# Índices base-100 para:
#   - Hechos conocidos (HC total)
#   - Población extranjera
#   - Población española
#
# Características:
#   - Fondo "blanco perla" (#FAF9F6) y paleta Okabe–Ito (colorblind-safe)
#   - Lectura robusta (separadores ES, columnas alternativas)
#   - Tablas: índices (FULL y NO_COVID) y correlaciones (Pearson/Spearman/Kendall)
#   - Figuras: líneas base-100 (FULL y NO_COVID)
#
# Entradas (en processed/ o data/processed/):
#   hechos_conocidos_total_nacional.csv   (ano, hc_total) o (ano, tipo, valor)
#   poblacion_extranjera_total.csv        (ano, poblacion_extranjera)
#   poblacion_espanola_total.csv          (ano, poblacion_espanola)
#
# Salidas:
#   output/tables/base100_hc_poblaciones_full.csv
#   output/tables/base100_hc_poblaciones_no_covid.csv
#   output/tables/corr_base100_full.csv
#   output/tables/corr_base100_no_covid.csv
#   output/figures/23_01_base100_full.png
#   output/figures/23_02_base100_no_covid.png
###############################################################################

suppressPackageStartupMessages({
  library(here); library(readr); library(dplyr); library(tidyr)
  library(janitor); library(stringr); library(fs); library(tibble)
  library(ggplot2); library(scales)
})

options(scipen = 999)

# ----------------- Helpers generales -----------------
dir_exists <- function(p) fs::dir_exists(p)

read_std <- function(path) {
  readr::read_csv(path, show_col_types = FALSE) |> janitor::clean_names()
}

parse_num_safe <- function(x) {
  if (is.numeric(x) || is.integer(x)) return(as.numeric(x))
  readr::parse_number(
    as.character(x),
    locale = readr::locale(decimal_mark = ",", grouping_mark = ".")
  )
}

# base100(x, base) — devuelve 100*x/base (NA si faltan)
base100 <- function(x, base) ifelse(is.na(x) | is.na(base), NA_real_, 100 * x / base)

# Correlación segura para un par
cor_pair <- function(df, x, y, method = "pearson") {
  ok <- is.finite(df[[x]]) & is.finite(df[[y]])
  if (sum(ok) < 3) {
    return(tibble(var1 = x, var2 = y, metodo = method, n = sum(ok),
                  estimate = NA_real_, p_value = NA_real_))
  }
  ct <- suppressWarnings(cor.test(df[[x]][ok], df[[y]][ok], method = method, exact = FALSE))
  tibble(var1 = x, var2 = y, metodo = method, n = sum(ok),
         estimate = unname(ct$estimate), p_value = ct$p.value)
}

cor_all_pairs <- function(df, vars, methods = c("pearson","spearman","kendall")) {
  prs <- t(combn(vars, 2))
  out <- vector("list", nrow(prs) * length(methods)); k <- 0L
  for (i in seq_len(nrow(prs))) {
    v1 <- prs[i,1]; v2 <- prs[i,2]
    for (m in methods) {
      k <- k + 1L
      out[[k]] <- cor_pair(df, v1, v2, m)
    }
  }
  bind_rows(out)
}

# ----------------- Tema + paleta ---------------------------------------------
col_perla <- "#FAF9F6"  # fondo blanco perla
# Paleta Okabe–Ito (colorblind-safe)
OI <- c("#000000","#E69F00","#56B4E9","#009E73",
        "#F0E442","#0072B2","#D55E00","#CC79A7")

# Orden y colores fijos por serie (consistentes entre plots)
niveles_serie <- c(
  "HC total (base 100)",
  "Población extranjera (base 100)",
  "Población española (base 100)"
)

pal_series <- c(
  "HC total (base 100)"              = OI[6], # azul
  "Población extranjera (base 100)"  = OI[4], # verde
  "Población española (base 100)"    = OI[2]  # naranja
)

theme_perla <- function() {
  theme_minimal(base_size = 12) +
    theme(
      panel.background = element_rect(fill = col_perla, color = NA),
      plot.background  = element_rect(fill = col_perla, color = NA),
      legend.background = element_rect(fill = col_perla, color = NA),
      legend.key       = element_rect(fill = col_perla, color = NA),
      strip.background = element_rect(fill = col_perla, color = NA),
      legend.position  = "top",
      panel.grid.major = element_line(color = "#EBE9E4", linewidth = 0.3),
      panel.grid.minor = element_line(color = "#EBE9E4", linewidth = 0.15)
    )
}

# ----------------- Localiza base dir -----------------
base1 <- here("processed")
base2 <- here("data","processed")
BASE  <- if (dir_exists(base1)) base1 else if (dir_exists(base2)) base2 else stop("No existe 'processed/' ni 'data/processed/'")

# ----------------- Entradas -----------------
fp_hc   <- fs::path(BASE, "hechos_conocidos_total_nacional.csv")
fp_ext  <- fs::path(BASE, "poblacion_extranjera_total.csv")
fp_esp  <- fs::path(BASE, "poblacion_espanola_total.csv")

if (!file.exists(fp_hc))  stop("Falta: ", fp_hc)
if (!file.exists(fp_ext)) stop("Falta: ", fp_ext)
if (!file.exists(fp_esp)) stop("Falta: ", fp_esp)

# ----------------- Lecturas -----------------
hc_raw <- read_std(fp_hc)
if ("hc_total" %in% names(hc_raw)) {
  hc <- hc_raw |> mutate(hc_total = parse_num_safe(hc_total)) |> select(ano, hc_total)
} else if (all(c("ano","valor") %in% names(hc_raw))) {
  # formato largo: sumar todo por año
  hc <- hc_raw |>
    mutate(valor = parse_num_safe(valor)) |>
    group_by(ano) |> summarise(hc_total = sum(valor, na.rm = TRUE), .groups = "drop")
} else {
  stop("El fichero de HC debe traer 'hc_total' o ('ano','valor'). Revisa columnas.")
}

pop_ext <- read_std(fp_ext) |> mutate(poblacion_extranjera = parse_num_safe(poblacion_extranjera)) |> select(ano, poblacion_extranjera)
pop_esp <- read_std(fp_esp) |> mutate(poblacion_espanola   = parse_num_safe(poblacion_espanola))   |> select(ano, poblacion_espanola)

# ----------------- Dataset maestro 2010–2023 -----------------
df <- hc |>
  inner_join(pop_ext, by="ano") |>
  inner_join(pop_esp, by="ano") |>
  filter(ano >= 2010, ano <= 2023) |>
  arrange(ano)

if (nrow(df) == 0) stop("Tras filtrar 2010–2023 no quedan filas. Revisa las fuentes.")

# ----------------- Base 100 -----------------
# Intentamos base en 2010; si falta, usamos el primer año disponible y lo indicamos en el título
base_year <- if (any(df$ano == 2010)) 2010L else df$ano[1]

b_hc  <- df$hc_total[df$ano == base_year][1]
b_ext <- df$poblacion_extranjera[df$ano == base_year][1]
b_esp <- df$poblacion_espanola[df$ano == base_year][1]

base_label <- if (base_year == 2010) "base 100" else paste0("base 100 (", base_year, ")")

base100_full <- df |>
  transmute(
    ano,
    hc_b100   = base100(hc_total, b_hc),
    pext_b100 = base100(poblacion_extranjera, b_ext),
    pesc_b100 = base100(poblacion_espanola,   b_esp)
  )

base100_nc <- base100_full |> filter(!(ano %in% c(2020, 2021)))

# ----------------- Correlaciones en base-100 -----------------
vars_idx <- c("hc_b100","pext_b100","pesc_b100")
corr_full <- cor_all_pairs(base100_full, vars_idx) |>
  mutate(estimate = round(estimate, 4), p_value = signif(p_value, 3), muestra = "FULL")
corr_nc   <- cor_all_pairs(base100_nc,   vars_idx) |>
  mutate(estimate = round(estimate, 4), p_value = signif(p_value, 3), muestra = "NO_COVID")

# ----------------- Salidas (tablas) -----------------
fs::dir_create(here("output","tables"))
readr::write_csv(base100_full, here("output","tables","base100_hc_poblaciones_full.csv"), na = "")
readr::write_csv(base100_nc,   here("output","tables","base100_hc_poblaciones_no_covid.csv"), na = "")
readr::write_csv(corr_full,    here("output","tables","corr_base100_full.csv"), na = "")
readr::write_csv(corr_nc,      here("output","tables","corr_base100_no_covid.csv"), na = "")

# ----------------- Gráficos (perla + colores) --------------------------------
fs::dir_create(here("output","figures"))

plot_long <- function(df_idx) {
  df_idx |>
    pivot_longer(-ano, names_to = "serie", values_to = "index") |>
    mutate(serie = recode(serie,
                          "hc_b100"   = "HC total (base 100)",
                          "pext_b100" = "Población extranjera (base 100)",
                          "pesc_b100" = "Población española (base 100)"),
           # orden fijo para mantener colores consistentes
           serie = factor(serie, levels = niveles_serie))
}

p_full <- ggplot(plot_long(base100_full), aes(ano, index, color = serie)) +
  geom_hline(yintercept = 100, linetype = "dashed", linewidth = 0.3) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.2) +
  scale_color_manual(values = pal_series, drop = FALSE) +
  scale_y_continuous(paste0("Índice (", base_label, ")"), labels = label_number(accuracy = 1)) +
  scale_x_continuous(breaks = seq(min(base100_full$ano), max(base100_full$ano), by = 1)) +
  labs(
    title = sprintf("Índices %s: HC, población extranjera y española — FULL", base_label),
    x = "Año", color = NULL,
    caption = "Línea discontinua = 100; fondo blanco perla."
  ) +
  theme_perla()

ggsave(here("output","figures","23_01_base100_full.png"),
       p_full, width = 12, height = 7, dpi = 300, bg = col_perla)

p_nc <- ggplot(plot_long(base100_nc), aes(ano, index, color = serie)) +
  geom_hline(yintercept = 100, linetype = "dashed", linewidth = 0.3) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.2) +
  scale_color_manual(values = pal_series, drop = FALSE) +
  scale_y_continuous(paste0("Índice (", base_label, ")"), labels = label_number(accuracy = 1)) +
  scale_x_continuous(breaks = seq(min(base100_nc$ano), max(base100_nc$ano), by = 1)) +
  labs(
    title = sprintf("Índices %s: HC, población extranjera y española — NO_COVID", base_label),
    x = "Año", color = NULL,
    caption = "Línea discontinua = 100; fondo blanco perla."
  ) +
  theme_perla()

ggsave(here("output","figures","23_02_base100_no_covid.png"),
       p_nc, width = 12, height = 7, dpi = 300, bg = col_perla)

# ----------------- Mensaje final ---------------------------------------------
message("✅ Listo. Tablas en output/tables y gráficos en output/figures.")
message("ℹ Nota: la correlación no cambia por base-100; se usa para lectura visual.")
if (base_year != 2010) {
  message("⚠️  Aviso: no había 2010; se usó ", base_year, " como año base para el índice.")
}
