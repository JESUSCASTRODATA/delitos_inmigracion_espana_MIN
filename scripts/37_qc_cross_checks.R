#!/usr/bin/env Rscript
# -*- coding: UTF-8 -*-
################################################################################
# 37_qc_cross_checks.R  (versión con dplyr::select calificado)
################################################################################

suppressPackageStartupMessages({
  library(here); library(readr); library(dplyr); library(tidyr)
  library(janitor); library(stringr); library(tibble)
})

dir.create(here::here("output","tables"), recursive = TRUE, showWarnings = FALSE)

read_csv_auto <- function(fp){
  out <- tryCatch(suppressMessages(readr::read_csv(fp, show_col_types = FALSE)),
                  error = function(e) NULL)
  if (is.null(out)){
    out <- tryCatch(suppressMessages(readr::read_delim(fp, delim = ";", show_col_types = FALSE)),
                    error = function(e) NULL)
  }
  out
}

num_es <- function(x){
  readr::parse_number(as.character(x),
                      locale = readr::locale(decimal_mark = ",", grouping_mark = "."))
}

# Archivos base
fp_hc   <- here::here("data","processed","hechos_conocidos_total_nacional.csv")
fp_dt   <- here::here("data","processed","detenciones_totales_total_nacional.csv")
fp_de   <- here::here("data","processed","detenciones_extranjeros_total_nacional.csv")
fp_pop  <- here::here("data","processed","poblacion_total_nacional.csv")
fp_tasa <- here::here("data","processed","tasas","tasas_totales_anuales.csv")
fp_share<- here::here("data","processed","detenciones_share_extranjeros.csv")

hc <- read_csv_auto(fp_hc); dt <- read_csv_auto(fp_dt); de <- read_csv_auto(fp_de)
pp <- read_csv_auto(fp_pop); tt <- read_csv_auto(fp_tasa); sh <- read_csv_auto(fp_share)

# ---- 1) tasas vs reconstrucción ----
tasas_chk <- tibble()
if (!is.null(pp)){
  if (!is.null(hc)){
    hc2 <- hc %>% janitor::clean_names() %>% dplyr::mutate(valor=num_es(valor), tipo=as.character(tipo))
    hc_tot <- hc2 %>% dplyr::filter(str_detect(tolower(tipo), "total")) %>%
      dplyr::transmute(ano=as.integer(ano), hc_total=valor)
    if (nrow(hc_tot)==0) {
      hc_tot <- hc2 %>% dplyr::group_by(ano) %>% dplyr::summarise(hc_total=sum(valor,na.rm=TRUE), .groups="drop") %>%
        dplyr::mutate(ano=as.integer(ano))
    }
  }
  if (!is.null(dt)){
    dt2 <- dt %>% janitor::clean_names() %>% dplyr::mutate(valor=num_es(valor), tipo=as.character(tipo))
    dt_tot <- dt2 %>% dplyr::filter(str_detect(tolower(tipo), "total")) %>%
      dplyr::transmute(ano=as.integer(ano), det_tot=valor)
    if (nrow(dt_tot)==0) {
      dt_tot <- dt2 %>% dplyr::group_by(ano) %>% dplyr::summarise(det_tot=sum(valor,na.rm=TRUE), .groups="drop") %>%
        dplyr::mutate(ano=as.integer(ano))
    }
  }
  if (!is.null(de)){
    de2 <- de %>% janitor::clean_names() %>% dplyr::mutate(valor=num_es(valor), tipo=as.character(tipo))
    de_tot <- de2 %>% dplyr::filter(str_detect(tolower(tipo), "total")) %>%
      dplyr::transmute(ano=as.integer(ano), det_ext=valor)
    if (nrow(de_tot)==0) {
      de_tot <- de2 %>% dplyr::group_by(ano) %>% dplyr::summarise(det_ext=sum(valor,na.rm=TRUE), .groups="drop") %>%
        dplyr::mutate(ano=as.integer(ano))
    }
  }
  pp2 <- pp %>% janitor::clean_names() %>% dplyr::transmute(ano=as.integer(ano), poblacion_total = num_es(poblacion_total))
  
  base <- Reduce(function(x,y) dplyr::inner_join(x,y,by="ano"),
                 Filter(Negate(is.null), list(hc_tot, dt_tot, de_tot, pp2)))
  if (!is.null(base) && nrow(base)>0){
    base <- base %>% dplyr::mutate(
      tasa_hc_100k_rebuilt      = 1e5 * hc_total / poblacion_total,
      tasa_det_tot_100k_rebuilt = 1e5 * det_tot / poblacion_total,
      tasa_det_ext_100k_rebuilt = 1e5 * det_ext / poblacion_total
    )
    if (!is.null(tt)){
      tt2 <- tt %>% janitor::clean_names()
      col_hc <- names(tt2)[stringr::str_detect(names(tt2), "hc")  & stringr::str_detect(names(tt2), "100k")]
      col_dt <- names(tt2)[stringr::str_detect(names(tt2), "det") & stringr::str_detect(names(tt2), "tot") & stringr::str_detect(names(tt2), "100k")]
      col_de <- names(tt2)[stringr::str_detect(names(tt2), "det") & stringr::str_detect(names(tt2), "ext") & stringr::str_detect(names(tt2), "100k")]
      cmp <- tt2 %>% dplyr::transmute(
        ano = as.integer(ano),
        tasa_hc_100k      = if (length(col_hc)>0) num_es(.[[col_hc[1]]]) else NA_real_,
        tasa_det_tot_100k = if (length(col_dt)>0) num_es(.[[col_dt[1]]]) else NA_real_,
        tasa_det_ext_100k = if (length(col_de)>0) num_es(.[[col_de[1]]]) else NA_real_
      ) %>%
        dplyr::inner_join(base, by="ano") %>%
        dplyr::mutate(
          diff_hc_rel = abs(tasa_hc_100k - tasa_hc_100k_rebuilt)/pmax(1e-9, tasa_hc_100k),
          diff_dt_rel = abs(tasa_det_tot_100k - tasa_det_tot_100k_rebuilt)/pmax(1e-9, tasa_det_tot_100k),
          diff_de_rel = abs(tasa_det_ext_100k - tasa_det_ext_100k_rebuilt)/pmax(1e-9, tasa_det_ext_100k),
          flag_hc = diff_hc_rel > 0.01,
          flag_dt = diff_dt_rel > 0.01,
          flag_de = diff_de_rel > 0.01
        )
      tasas_chk <- cmp
    } else {
      tasas_chk <- base %>% dplyr::select(ano, dplyr::starts_with("tasa_"))
    }
  }
}
if (nrow(tasas_chk) > 0) readr::write_csv(tasas_chk, here::here("output","tables","qa37_tasas_vs_reconstruccion.csv"))

# ---- 2) share extranjeros vs reconstrucción ----
share_chk <- tibble()
if (!is.null(dt) && !is.null(de)){
  base <- dt %>% janitor::clean_names() %>% dplyr::mutate(valor=num_es(valor), tipo=as.character(tipo)) %>%
    { if (any(stringr::str_detect(tolower(.$tipo), "total"))) dplyr::filter(., stringr::str_detect(tolower(tipo), "total")) else . } %>%
    dplyr::group_by(ano) %>% dplyr::summarise(det_tot = sum(valor, na.rm=TRUE), .groups="drop") %>% dplyr::mutate(ano=as.integer(ano))
  ext  <- de %>% janitor::clean_names() %>% dplyr::mutate(valor=num_es(valor), tipo=as.character(tipo)) %>%
    { if (any(stringr::str_detect(tolower(.$tipo), "total"))) dplyr::filter(., stringr::str_detect(tolower(tipo), "total")) else . } %>%
    dplyr::group_by(ano) %>% dplyr::summarise(det_ext = sum(valor, na.rm=TRUE), .groups="drop") %>% dplyr::mutate(ano=as.integer(ano))
  sc <- base %>% dplyr::inner_join(ext, by="ano") %>% dplyr::mutate(share_det_ext_rebuilt = ifelse(det_tot>0, det_ext/det_tot, NA_real_))
  if (!is.null(sh)){
    sh2 <- sh %>% janitor::clean_names()
    share_col <- names(sh2)[stringr::str_detect(names(sh2), "share|propor")]
    sh3 <- sh2 %>% dplyr::transmute(ano=as.integer(ano),
                                    share_archivo = if (length(share_col)>0) num_es(.[[share_col[1]]]) else NA_real_) %>%
      dplyr::mutate(share_archivo = ifelse(share_archivo>1.2, share_archivo/100, share_archivo))
    share_chk <- sc %>% dplyr::inner_join(sh3, by="ano") %>%
      dplyr::mutate(diff_abs = abs(share_det_ext_rebuilt - share_archivo), flag = diff_abs > 0.005)
  } else {
    share_chk <- sc
  }
}
if (nrow(share_chk) > 0) readr::write_csv(share_chk, here::here("output","tables","qa37_share_extranjeros_vs_reconstruccion.csv"))

# ---- 3) sumas por sexo = TOTAL ----
sexo_chk <- tibble()
sex_files <- list.files(here::here("data","processed"), pattern="detenciones_.*_por_sexo.*\\.csv$", full.names = TRUE, recursive = TRUE)
for (f in sex_files){
  df <- read_csv_auto(f); if (is.null(df)) next
  df <- df %>% janitor::clean_names()
  if (!all(c("ano","tipo","sexo","valor") %in% names(df))) next
  tmp <- df %>%
    dplyr::mutate(valor=num_es(valor), sexo = toupper(trimws(as.character(sexo)))) %>%
    dplyr::filter(sexo %in% c("H","M","TOTAL","HOMBRE","MUJER")) %>%
    dplyr::mutate(sexo = dplyr::recode(sexo, "HOMBRE"="H", "MUJER"="M", .default=sexo))
  agg <- tmp %>%
    dplyr::group_by(ano, tipo) %>%
    dplyr::summarise(h = sum(valor[sexo=="H"], na.rm=TRUE),
                     m = sum(valor[sexo=="M"], na.rm=TRUE),
                     total = sum(valor[sexo=="TOTAL"], na.rm=TRUE), .groups="drop") %>%
    dplyr::mutate(sum_hm = h + m,
                  diff = abs(total - sum_hm),
                  flag = diff > 1) %>%
    dplyr::mutate(file = gsub(paste0("^", gsub("\\\\","/", here::here("data","processed")), "/?"), "", gsub("\\\\","/", f))) %>%
    dplyr::select(file, ano, tipo, h, m, total, sum_hm, diff, flag)
  sexo_chk <- dplyr::bind_rows(sexo_chk, agg %>% dplyr::filter(flag))
}
if (nrow(sexo_chk) > 0) readr::write_csv(sexo_chk, here::here("output","tables","qa37_sexo_suma_total.csv"))

message("✅ QC 37 terminado. Revisa:",
        "\n - output/tables/qa37_tasas_vs_reconstruccion.csv",
        "\n - output/tables/qa37_share_extranjeros_vs_reconstruccion.csv",
        "\n - output/tables/qa37_sexo_suma_total.csv (si existe)")
