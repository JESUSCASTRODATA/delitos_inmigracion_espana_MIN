# 16_qc_console.R — Smoke check en consola para detenciones extranjeros (script 16)
qc16_console <- function(strict = FALSE,
                         year_min = 2010L, year_max = 2023L,
                         tol_rel = 0.005, tol_abs = 100) {
  suppressPackageStartupMessages({
    library(here); library(readr); library(dplyr); library(janitor); library(tidyr)
  })
  msg <- function(...) cat(paste0(..., "\n"))
  
  # Paths salidas de 16
  p_sex   <- here::here("data","processed","detenciones_extranjeros_total_nacional_por_sexo.csv")
  p_nat   <- here::here("data","processed","detenciones_extranjeros_total_nacional.csv")
  p_tot   <- here::here("data","processed","detenciones_extranjeros_total.csv")
  p_share <- here::here("data","processed","detenciones_share_extranjeros.csv")
  p_det   <- here::here("data","processed","detenciones_totales_total.csv")
  
  # Existencia
  files_ok <- c(file.exists(p_sex), file.exists(p_nat), file.exists(p_tot), file.exists(p_share), file.exists(p_det))
  if (!all(files_ok)) {
    msg("❌ Faltan archivos:")
    if (!file.exists(p_sex))   msg("   - detenciones_extranjeros_total_nacional_por_sexo.csv")
    if (!file.exists(p_nat))   msg("   - detenciones_extranjeros_total_nacional.csv")
    if (!file.exists(p_tot))   msg("   - detenciones_extranjeros_total.csv")
    if (!file.exists(p_share)) msg("   - detenciones_share_extranjeros.csv")
    if (!file.exists(p_det))   msg("   - detenciones_totales_total.csv (para contrastar share)")
    if (strict) stop("Falta(n) salidas del 16.", call. = FALSE) else return(invisible(FALSE))
  }
  
  # Lecturas
  sex   <- read_csv(p_sex,   show_col_types = FALSE) |> clean_names()
  nat   <- read_csv(p_nat,   show_col_types = FALSE) |> clean_names()
  tot   <- read_csv(p_tot,   show_col_types = FALSE) |> clean_names()
  share <- read_csv(p_share, show_col_types = FALSE) |> clean_names()
  det   <- read_csv(p_det,   show_col_types = FALSE) |> clean_names()
  
  # Esquema básico
  req1 <- all(c("ano","tipo","sexo","valor","metric") %in% names(sex))
  req2 <- all(c("ano","tipo","valor") %in% names(nat))
  req3 <- all(c("ano","det_ext") %in% names(tot))
  req4 <- all(c("ano","share_extranjeros") %in% names(share))
  req5 <- all(c("ano","det_tot") %in% names(det))
  if (!(req1 && req2 && req3 && req4 && req5)) {
    msg("❌ Esquema inesperado en alguna salida (sex/nat/tot/share/det_tot).")
    if (strict) stop("Esquema inesperado.", call. = FALSE) else return(invisible(FALSE))
  }
  
  YEARS <- year_min:year_max
  
  # Tipos / limpieza numérica
  sex$valor  <- suppressWarnings(as.numeric(sex$valor))
  nat$valor  <- suppressWarnings(as.numeric(nat$valor))
  tot$det_ext <- suppressWarnings(as.numeric(tot$det_ext))
  det$det_tot <- suppressWarnings(as.numeric(det$det_tot))
  share$share_extranjeros <- suppressWarnings(as.numeric(share$share_extranjeros))
  
  # Cobertura
  cov_tot   <- sort(unique(tot$ano)); miss_tot <- setdiff(YEARS, cov_tot)
  cov_share <- sort(unique(share$ano)); miss_share <- setdiff(YEARS, cov_share)
  
  # Duplicados/NA/negativos
  dup_tot <- tot |> count(ano) |> filter(n>1) |> nrow()
  na_tot  <- sum(!is.finite(tot$det_ext))
  neg_tot <- sum(tot$det_ext < 0, na.rm = TRUE)
  
  # Consistencia 1: det_ext (serie) = suma de nat por año (excluye filas "total ..." si existieran ya agregadas)
  nat_sum <- nat |>
    mutate(tipo_norm = tolower(tipo)) |>
    filter(!grepl("^total(\\b| )", tipo_norm)) |>
    group_by(ano) |> summarise(det_ext_nat = sum(valor, na.rm = TRUE), .groups="drop")
  cmp1 <- tot |> left_join(nat_sum, by = "ano") |>
    mutate(diff = det_ext - det_ext_nat,
           pass = is.na(det_ext_nat) | abs(diff) <= tol_abs | abs(diff) <= tol_rel * pmax(1, det_ext_nat))
  fails_cmp1 <- sum(!cmp1$pass, na.rm = TRUE)
  
  # Consistencia 2: share = 100 * det_ext / det_tot
  cmp2 <- share |>
    left_join(tot, by="ano") |>
    left_join(det, by="ano") |>
    mutate(share_calc = if_else(det_tot > 0, 100 * det_ext / det_tot, NA_real_),
           err = abs(share_extranjeros - share_calc),
           pass = is.na(share_calc) | err <= 0.05)  # 0.05 p.p. tolerancia
  fails_cmp2 <- sum(!cmp2$pass, na.rm = TRUE)
  
  # Consistencia 3: sexo total ≈ H+M
  sex_sum <- sex |> filter(sexo %in% c("hombres","mujeres")) |>
    group_by(ano, tipo) |> summarise(sum_hm = sum(valor, na.rm = TRUE), .groups="drop")
  sex_tot <- sex |> filter(sexo == "total") |> select(ano, tipo, total_decl = valor)
  cmp3 <- sex_sum |> left_join(sex_tot, by = c("ano","tipo")) |>
    mutate(diff = total_decl - sum_hm,
           pass = is.na(total_decl) | abs(diff) <= max(tol_abs, tol_rel * pmax(1, sum_hm)))
  fails_cmp3 <- sum(!cmp3$pass, na.rm = TRUE)
  
  # Reglas de rango
  share_bad_rng <- sum(share$share_extranjeros < 0 | share$share_extranjeros > 100, na.rm = TRUE)
  det_viol      <- tot |> left_join(det, by="ano") |> summarise(viol = sum(det_ext > det_tot, na.rm = TRUE)) |> pull(viol)
  
  # Preview último año
  last <- max(cov_tot)
  prev <- nat |> group_by(ano) |> filter(ano == last) |>
    arrange(desc(valor)) |> slice_head(n = 10) |>
    mutate(pct = 100 * valor / sum(valor, na.rm = TRUE)) |>
    select(ano, tipo, valor, pct)
  
  # ---- Salida consola ----
  cat("\n🧪 QC16 — detenciones extranjeros\n")
  cat(sprintf("• Cobertura det_ext: %d/%d (%s–%s) faltan: %s\n",
              length(cov_tot), length(YEARS), min(cov_tot), max(cov_tot),
              ifelse(length(miss_tot)==0, "ninguno", paste(miss_tot, collapse=", "))))
  cat(sprintf("• Cobertura share:   %d/%d (%s–%s) faltan: %s\n",
              length(cov_share), length(YEARS), min(cov_share), max(cov_share),
              ifelse(length(miss_share)==0, "ninguno", paste(miss_share, collapse=", "))))
  cat(sprintf("• Duplicados/NAs/negativos (det_ext): %d / %d / %d\n", dup_tot, na_tot, neg_tot))
  cat(sprintf("• Consistencia det_ext vs nat-sum — fallos: %d (tol rel=%.3f, abs=%d)\n", fails_cmp1, tol_rel, tol_abs))
  cat(sprintf("• Consistencia share (=100*det_ext/det_tot) — fallos: %d\n", fails_cmp2))
  cat(sprintf("• Consistencia sexo total ≈ H+M — fallos: %d\n", fails_cmp3))
  cat(sprintf("• share ∈ [0,100]: %s | det_ext ≤ det_tot (años con violación): %d\n",
              ifelse(share_bad_rng==0,"OK","NO"), det_viol))
  cat(sprintf("\n• Preview %d (top 10 por valor):\n", last))
  print(prev, n = nrow(prev))
  
  hard_fail <- any(c(length(miss_tot)>0, dup_tot>0, na_tot>0, neg_tot>0,
                     fails_cmp1>0, fails_cmp2>0, fails_cmp3>0,
                     share_bad_rng>0, det_viol>0))
  
  if (hard_fail) {
    cat("\n❌ FAIL — revisa los puntos rojos.\n\n")
    if (strict) stop("QC16 falló (modo estricto).", call. = FALSE)
    invisible(FALSE)
  } else {
    cat("\n✅ PASS — 16 (extranjeros) OK\n\n")
    invisible(TRUE)
  }
}

# Uso:
# qc16_console()               # resumen
# qc16_console(strict = TRUE)  # corta si hay fallos
qc16_console()