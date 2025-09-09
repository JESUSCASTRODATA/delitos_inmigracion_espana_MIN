# 11a_build_pesos_EPA_v2.R — arregla encoding y mapeo de edades por dígitos
suppressPackageStartupMessages({
  library(dplyr); library(readr); library(janitor); library(stringr); library(here)
})

fp_raw <- here("data","raw","epa_activos_16_29_trimestral.csv")

# ⚠️ JAXI suele venir en Latin-1 / Windows-1252
epa <- read_delim(
  fp_raw, delim = ";",
  locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "Latin1"),
  show_col_types = FALSE
) |>
  clean_names()

# Localiza columnas aunque cambien los encabezados
pick_col <- function(pats, nms) {
  hit <- Reduce(`|`, lapply(pats, \(p) grepl(p, nms, ignore.case = TRUE)))
  if (!any(hit)) NA_character_ else nms[which(hit)[1]]
}
nms <- names(epa)
col_sexo    <- pick_col(c("^sexo$"), nms)
col_edad    <- pick_col(c("grupo.*edad","^edad$"), nms)
col_periodo <- pick_col(c("^periodo$"), nms)
col_valor   <- pick_col(c("^valor$","^total$","^value$"), nms)
stopifnot(all(!is.na(c(col_sexo, col_edad, col_periodo, col_valor))))

epa2 <- epa |>
  transmute(
    sexo    = .data[[col_sexo]],
    edad    = .data[[col_edad]],
    periodo = .data[[col_periodo]],
    valor   = .data[[col_valor]]
  ) |>
  filter(grepl("^ambos", tolower(sexo))) |>
  mutate(
    # ✅ mapeo por dígitos (independiente de “años/ñ”)
    edad_band = case_when(
      str_detect(edad, "16\\D*19") ~ "16-19",
      str_detect(edad, "20\\D*24") ~ "20-24",
      str_detect(edad, "25\\D*29") ~ "25-29",
      TRUE ~ NA_character_
    ),
    ano       = readr::parse_number(periodo),
    trimestre = readr::parse_number(str_extract(periodo, "(?i)T\\s*([1-4])")),
    # Convierte "1.234.567" y coma decimal si la hubiera
    valor_num = readr::parse_number(as.character(valor),
                                    locale = locale(decimal_mark = ",", grouping_mark = "."))
  ) |>
  filter(!is.na(edad_band), !is.na(ano), !is.na(trimestre)) |>
  filter(ano %in% 2010:2023)

# Media anual por banda (activos EPA)
pesos <- epa2 |>
  group_by(ano, edad_band) |>
  summarise(poblacion = mean(valor_num, na.rm = TRUE), .groups = "drop") |>
  arrange(ano, edad_band)

# QC & logs útiles
if (nrow(pesos) < 42) {
  warning("Cobertura < 42 filas (14 años x 3 bandas). Revisa filtros o el CSV.")
  message("Valores únicos en 'edad' tras filtrar sexo=Ambos:\n",
          paste(utils::head(sort(unique(epa2$edad)), 20), collapse = " | "))
  message("Rango de años detectado: ", paste(range(pesos$ano), collapse = "–"))
}

# A lo que espera tu script 11
out <- pesos |> transmute(ano, edad_bin = edad_band, poblacion)

dir.create(here("data","processed"), recursive = TRUE, showWarnings = FALSE)
write_csv(out, here("data","processed","poblacion_15_29_bins_anual.csv"))

message("✅ Generado: ", here("data","processed","poblacion_15_29_bins_anual.csv"))
print(out |> count(ano, name="filas_por_ano") |> arrange(ano), n = 50)
