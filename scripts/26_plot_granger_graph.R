#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(here); library(readr); library(dplyr); library(tidyr); library(janitor)
  library(ggplot2); library(glue); library(stringr)
})
if (!requireNamespace("ragg", quietly = TRUE)) stop("Instala 'ragg' (install.packages('ragg')).")

# Tema pearl (si existe)
.have_theme <- FALSE
for (p in c(here::here("scripts","00_theme_figuras.R"), here::here("R","00_theme_figuras.R"))) {
  if (file.exists(p)) { source(p); .have_theme <- TRUE; break }
}
if (!.have_theme) {
  theme_pearl_lightgrid <- function(base_size=12, base_family=NULL){
    theme_minimal(base_size=base_size, base_family=base_family) +
      theme(plot.background=element_rect(fill="#FAF9F6", colour=NA),
            panel.background=element_rect(fill="#FAF9F6", colour=NA),
            legend.background=element_rect(fill="#FAF9F6", colour=NA),
            legend.key=element_rect(fill="#FAF9F6", colour=NA),
            panel.grid.major=element_line(colour="#E5E2DA", linewidth=0.4),
            panel.grid.minor=element_line(colour="#EEECE6", linewidth=0.25),
            plot.title=element_text(face="bold", size=base_size*1.15),
            legend.title=element_text(face="bold"))
  }
}

lab_var <- function(x){
  x <- as.character(x)
  dplyr::case_when(
    x %in% c("pct_extran","pct_extranjeros") ~ "% extranjeros",
    x %in% c("share_extranjeros","share_ext","share_ext_total","d_share_ext") ~ "share det. extranjeros",
    x %in% c("tasa_hc_100k","d_tasa_hc_100k") ~ "tasa HC (100k)",
    x %in% c("tasa_he_100k","d_tasa_he_100k") ~ "tasa HE (100k)",
    x == "d_pct_extran" ~ "Δ % extranjeros",
    x == "d_share_ext" ~ "Δ share det. extranjeros",
    x == "d_tasa_hc_100k" ~ "Δ tasa HC (100k)",
    x == "d_tasa_he_100k" ~ "Δ tasa HE (100k)",
    TRUE ~ x
  )
}

p24_csv <- here::here("output","tables","24_granger_results.csv")
if (!file.exists(p24_csv)) stop("No existe: ", p24_csv)
message("Leyendo: ", p24_csv)
res24 <- suppressMessages(readr::read_csv(p24_csv, show_col_types = FALSE)) |> janitor::clean_names()

fmt_year_range <- function(df){
  if (all(c("start_year","end_year") %in% names(df))){
    a <- suppressWarnings(min(df$start_year, na.rm=TRUE))
    b <- suppressWarnings(max(df$end_year, na.rm=TRUE))
    if (is.finite(a) && is.finite(b)) return(glue("{a}–{b}"))
  }
  NULL
}
range24 <- fmt_year_range(res24)

edges <- res24 |>
  transmute(pair,
            src = str_trim(sub("^(.*) ~ (.*)$", "\\2", pair)),
            dst = str_trim(sub("^(.*) ~ (.*)$", "\\1", pair)),
            p = granger_v2_to_v1_p, dir = "v2→v1",
            lag_p, stable_roots, port = portmanteau_p) |>
  bind_rows(
    res24 |>
      transmute(pair,
                src = str_trim(sub("^(.*) ~ (.*)$", "\\1", pair)),
                dst = str_trim(sub("^(.*) ~ (.*)$", "\\2", pair)),
                p = granger_v1_to_v2_p, dir = "v1→v2",
                lag_p, stable_roots, port = portmanteau_p)
  ) |>
  mutate(p_fdr = p.adjust(p, method="BH"),
         sig_cat = case_when(p_fdr <= 0.01 ~ "≤ 1%",
                             p_fdr <= 0.05 ~ "≤ 5%",
                             p_fdr <= 0.10 ~ "≤ 10%",
                             TRUE ~ "> 10%"),
         alpha_diag = ifelse(!is.na(port) & port > 0.05 & stable_roots, 1.0, 0.45),
         src_lab = lab_var(src), dst_lab = lab_var(dst)) |>
  filter(p_fdr <= 0.10)

message("Aristas Δ con FDR≤10%: ", nrow(edges))
if (nrow(edges) == 0) stop("No hay aristas significativas (FDR≤10%) en Δ.")

nodes <- tibble::tibble(name = unique(c(edges$src_lab, edges$dst_lab)))
nodes$x <- ifelse(nodes$name == "Δ % extranjeros", 0, 1)
y_map <- setNames(rep(NA_real_, nrow(nodes)), nodes$name)
y_map["Δ % extranjeros"] <- 0
y_map["Δ share det. extranjeros"] <- 0.4
y_map["Δ tasa HC (100k)"] <- 0.0
y_map["Δ tasa HE (100k)"] <- -0.4
missing <- names(y_map)[is.na(y_map)]
if (length(missing) > 0) y_map[missing] <- seq(-0.6, 0.6, length.out = length(missing))
nodes$y <- as.numeric(y_map[nodes$name])

message("Construyendo plot Δ ...")
p <- ggplot() +
  geom_curve(data = edges,
             aes(x = nodes$x[match(src_lab, nodes$name)],
                 y = nodes$y[match(src_lab, nodes$name)],
                 xend = nodes$x[match(dst_lab, nodes$name)],
                 yend = nodes$y[match(dst_lab, nodes$name)],
                 linetype = dir, linewidth = sig_cat, alpha = alpha_diag),
             curvature = 0.1, arrow = arrow(length = unit(6, "pt"), type = "closed")) +
  geom_point(data = nodes, aes(x, y), size = 3.8) +
  geom_label(data = nodes, aes(x, y, label = name),
             size = 3, label.size = 0.2, label.padding = unit(3, "pt")) +
  scale_linewidth_manual(values = c("≤ 1%"=1.4, "≤ 5%"=1.2, "≤ 10%"=1.0)) +
  scale_linetype_manual(values = c("v2→v1"="solid", "v1→v2"="dashed")) +
  scale_alpha_identity() +
  labs(title = glue("Precedencia temporal en diferencias (Δ/VAR){if(!is.null(range24)) glue(' · {range24}') else ''}"),
       subtitle = "Selección por estabilidad + Portmanteau; dummy COVID exógena. Flechas con FDR ≤ 10%.",
       caption = "Sólida: v2→v1 · Discontinua: v1→v2 · Opacidad: PASS (estable & Port>0.05)\nFuente: 24_granger_results.csv",
       linetype="Dirección", linewidth="FDR (p)") +
  theme_pearl_lightgrid() +
  theme(legend.position="top", panel.grid = element_blank())

ggplot_build(p); invisible(gc())
out <- here::here("output","figures","granger_delta.png")
dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
message("Guardando PNG Δ en: ", out)
ggsave(out, p, width=9, height=6, dpi=300, bg="#FAF9F6", device=ragg::agg_png)
message("✔ Δ listo.")
