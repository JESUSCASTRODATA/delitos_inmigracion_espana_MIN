# Delitos e Inmigración en España (2010–2023)

*Análisis reproducible de la relación entre población extranjera y criminalidad agregada en España.*

[![Code: MIT](https://img.shields.io/badge/Code-MIT-blue.svg)](LICENSE)
[![Docs: CC BY 4.0](https://img.shields.io/badge/Docs-CC%20BY%204.0-brightgreen.svg)](LICENSE-DOCS-CC-BY-4.0.md)
![R >= 4.3](https://img.shields.io/badge/R-%3E%3D%204.3-276DC3.svg)
![Quarto/RMarkdown](https://img.shields.io/badge/Quarto%2FRMarkdown-supported-lightgrey.svg)
![Reproducibility: renv](https://img.shields.io/badge/reproducibility-renv-success.svg)

| Autor            | Organización    | Versión | Estado | Periodo   | Última actualización |
| ---------------- | --------------- | ------- | ------ | --------- | -------------------- |
| **Jesús Castro** | JESUSCASTRODATA | dev     | Activo | 2010–2023 | 2025-09-08           |

---

## Resumen ejecutivo

Estudio sobre la relación entre **población extranjera** y **criminalidad agregada** en España (2010–2023), con foco en **tasas por 100.000**, transformaciones **Δlog**, y pruebas de **precedencia temporal** (VAR/Granger) y **robustez**.

* En **niveles** (HC vs población extranjera base-100) las correlaciones son **muy altas** incluso **sin COVID**.
* En **Δlog** (crecimientos), varias asociaciones se mantienen **moderadas-altas** **sin COVID**; la ventana **2021–2023** es **frágil** por **n=3**.
* Para **detenciones** vs **% población extranjera**, la correlación es **similar** al **excluir 2020–2021**.
* Los **tests de Granger** **no** son **robustos** al **excluir COVID** (fallos de estabilidad) → **no** se confirma una **causalidad estable** inmigración→criminalidad.

Este proyecto **no re-licencia** datos originales de terceros; ver [DATA\_LICENSE.md](DATA_LICENSE.md).

---

## Contenidos

* [Estructura del repositorio](#estructura-del-repositorio-resumen)
* [Datos y cobertura](#datos-y-cobertura)
* [Metodología](#metodología)
* [Reproducibilidad](#reproducibilidad)
* [Cómo ejecutar](#cómo-ejecutar)
* [Salidas (tablas/figuras)](#salidas-tablasfiguras)
* [Resultados clave](#resultados-clave-hechos)
* [Licencia](#licencia) · [Citación](#citación) · [Contacto](#contacto)

---

## Estructura del repositorio (resumen)

```
.
├─ config/           # mapeos, esquemas y excepciones de QA
├─ data/
│  ├─ raw/           # datos originales
│  └─ processed/     # derivados/limpios
├─ docs/             # informe Rmd/HTML, bibliografía, CSL
├─ output/
│  ├─ figures/       # gráficos
│  └─ tables/        # tablas
├─ diagnostics/      # perfiles y previsualizaciones (csv)
├─ scripts/          # ETL, análisis, modelos y QA
│  ├─ utils/
│  └─ 80_run_all.R   # orquestador
├─ LICENSE, LICENSE-DOCS-CC-BY-4.0.md, DATA_LICENSE.md, CITATION.cff
└─ README.md
```

---

## Datos y cobertura

* **Ámbito**: España, Total Nacional, anual **2010–2023**.
* **Fuentes**: INE (Padrón; AROPE; EPA), Ministerio del Interior (HC/HE/Detenciones), Eurostat (PIB pc real encadenado).
* **Definiciones clave**: “Extranjero” = nacionalidad no española; **HE ≤ HC** como regla de consistencia.
* **Licenciamiento de datos**: ver [DATA\_LICENSE.md](DATA_LICENSE.md) (los datos **mantienen** las condiciones de sus titulares).

**Nota de cobertura**: el análisis **comienza en 2010**. No se incluyen años anteriores por **falta de series nacionales comparables y continuas** de **Hechos Conocidos (HC)** y **Hechos Esclarecidos (HE)**.

### Cómo obtener `data/raw`

> Mapeo de ficheros locales a sus fuentes oficiales. Algunas fuentes permiten **CSV directo**; otras requieren **filtrar en la página** y exportar.

| archivo local                           | fuente oficial                                                     | URL                                                                                                                                                                                                                                              |
| --------------------------------------- | ------------------------------------------------------------------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `arope.csv`                             | INE · AROPE (Riesgo de pobreza o exclusión)                        | [https://www.ine.es/jaxiT3/files/t/csv\_bd/60261.csv](https://www.ine.es/jaxiT3/files/t/csv_bd/60261.csv)                                                                                                                                        |
| `gini.csv`                              | INE · Índice de Gini (ECV)                                         | [https://www.ine.es/jaxiT3/files/t/csv\_bd/28184.csv](https://www.ine.es/jaxiT3/files/t/csv_bd/28184.csv)                                                                                                                                        |
| `umbral_pobreza.csv`                    | INE · Umbral de pobreza (ECV)                                      | [https://www.ine.es/jaxiT3/files/t/csv\_bd/59968.csv](https://www.ine.es/jaxiT3/files/t/csv_bd/59968.csv)                                                                                                                                        |
| `tasas_paro.csv`                        | INE · EPA, tasas de paro (trimestral)                              | Página: [https://www.ine.es/jaxiT3/Tabla.htm?t=4247](https://www.ine.es/jaxiT3/Tabla.htm?t=4247) · CSV: [https://www.ine.es/jaxiT3/files/t/csv\_bd/4247.csv](https://www.ine.es/jaxiT3/files/t/csv_bd/4247.csv)                                  |
| `epa_activos_16_29_trimestral.csv`      | INE · EPA, población activa por edad (trimestral)                  | [https://www.ine.es/dyngs/INEbase/es/operacion.htm?c=Estadistica\_C\&cid=1254736176918\&idp=1254735976595\&menu=ultiDatos](https://www.ine.es/dyngs/INEbase/es/operacion.htm?c=Estadistica_C&cid=1254736176918&idp=1254735976595&menu=ultiDatos) |
| `poblacion_espana.csv`                  | INE · Padrón continuo (española/extranjera)                        | CSV combinado: [https://www.ine.es/jaxiT3/files/t/es/csv\_bd/t20/e245/p08/l0/01006.csv](https://www.ine.es/jaxiT3/files/t/es/csv_bd/t20/e245/p08/l0/01006.csv)                                                                                   |
| `hechos_conocidos.csv`                  | Ministerio del Interior · Portal de Criminalidad                   | Punto de entrada: [https://estadisticasdecriminalidad.ses.mir.es/sec/es/operaciones/operaciones\_tablas.htm](https://estadisticasdecriminalidad.ses.mir.es/sec/es/operaciones/operaciones_tablas.htm)                                            |
| `hechos_esclarecidos.csv`               | Ministerio del Interior · Hechos esclarecidos                      | [https://estadisticasdecriminalidad.ses.mir.es/sec/jaxiPx/files/\_px/es/csv/Datos2/l0/02002.csv](https://estadisticasdecriminalidad.ses.mir.es/sec/jaxiPx/files/_px/es/csv/Datos2/l0/02002.csv)                                                  |
| `detenciones_tipologia.csv`             | Ministerio del Interior · Detenciones por tipología                | CSV: [https://estadisticasdecriminalidad.ses.mir.es/infocrim/datoscrim/Datos6/l0/06006.csv](https://estadisticasdecriminalidad.ses.mir.es/infocrim/datoscrim/Datos6/l0/06006.csv)                                                                |
| `detenciones_extranjeros_tipologia.csv` | Ministerio del Interior · Detenciones por tipología y nacionalidad | CSV: [https://estadisticasdecriminalidad.ses.mir.es/sec/jaxiPx/files/\_px/es/csv/Datos6/l0/06008.csv](https://estadisticasdecriminalidad.ses.mir.es/sec/jaxiPx/files/_px/es/csv/Datos6/l0/06008.csv)                                             |
| `estat_nama_10_pc_filtered_en.csv`      | Eurostat · nama\_10\_pc (GDP per capita)                           | Data Browser: [https://ec.europa.eu/eurostat/databrowser/view/NAMA\_10\_PC/](https://ec.europa.eu/eurostat/databrowser/view/NAMA_10_PC/)                                                                                                         |
| `crecimiento_pib.csv`                   | Eurostat · tec00115 (Real GDP growth rate)                         | Página del indicador: [https://ec.europa.eu/eurostat/databrowser/product/page/TEC00115](https://ec.europa.eu/eurostat/databrowser/product/page/TEC00115)                                                                                         |

**Notas**

* Portal de Criminalidad: cada tabla tiene diálogo de exportación (`dlgExport.htm?file=...`) y, cuando procede, **CSV directo**. Verifica ámbito “Total nacional”.
* INE jaxiT3: el patrón `.../files/t/csv_bd/<ID>.csv` descarga con filtros por defecto; para cortes, entra en la **Tabla** y exporta.
* Eurostat: suele requerir selección en **Data Browser**; descarga en **SDMX-CSV/TSV**.

#### Comprobación rápida en R

```r
expected <- c(
  "data/raw/arope.csv",
  "data/raw/crecimiento_pib.csv",
  "data/raw/detenciones_extranjeros_tipologia.csv",
  "data/raw/detenciones_tipologia.csv",
  "data/raw/epa_activos_16_29_trimestral.csv",
  "data/raw/estat_nama_10_pc_filtered_en.csv",
  "data/raw/gini.csv",
  "data/raw/hechos_conocidos.csv",
  "data/raw/hechos_esclarecidos.csv",
  "data/raw/poblacion_espana.csv",
  "data/raw/tasas_paro.csv",
  "data/raw/umbral_pobreza.csv"
)
print(data.frame(archivo = expected, existe = file.exists(expected)))
```

---

## Metodología

* **ETL/QA**: estandarización (`ano`, `tipo`, `valor`, `dataset`), control **HE ≤ HC**, cobertura anual, duplicados, *outliers* (IQR).
* **Transformaciones**: **tasas por 100.000**, logs y **Δlog**.
* **Análisis**: descriptivo (tendencias/base-100), correlaciones (Pearson/Spearman/Kendall), **ADF**, **VAR/Granger** y **ARDL (Bounds)** con **FDR**.
* **Ventanas**: 2010–2019 (pre), 2020–2021 (COVID), 2021–2023 (post), **sin COVID**.
* Las series en **niveles** no son estacionarias (ADF) → se priorizan **Δlog** y modelos con diagnósticos.

---

## Reproducibilidad

* R ≥ 4.3; **renv** para el entorno.
* Rutas con `here::here()`; el Rmd asume ejecutar desde `docs/`.
* `.bib` y `apa.csl` en `docs/`.

### Requisitos del sistema

* R ≥ 4.3, pandoc ≥ 2.19.
* TinyTeX/TeXLive para PDF (opcional).
* Linux, macOS o Windows.
* Espacio en disco: ≥ 1–2 GB si se conservan `data/raw`.

### Preparación del entorno

```r
install.packages("renv")
renv::restore()              # instala versiones reproducibles de paquetes
source("scripts/60_reproducibility_env.R")  # utilidades de entorno/repro
```

---

## Cómo ejecutar

A) **Pipeline completo**

```r
source("scripts/80_run_all.R")
```

B) **Render del informe**

```r
# Opción 1: script dedicado
source("scripts/98_render_report.R")

# Opción 2: llamada directa
rmarkdown::render("docs/informe_delitos_inmigracion_espana_2010_2023.Rmd")
```

C) **Exportación de figuras** (si no se lanzó desde el pipeline)

```r
source("scripts/70_export_figs.R")
```

**Enlaces rápidos**: [informe](docs/informe_delitos_inmigracion_espana_2010_2023.Rmd), [pipeline](scripts/80_run_all.R), [licencias](DATA_LICENSE.md).

Si usas Windows y Quarto, instala **TinyTeX** para PDF o renderiza solo a **HTML**.

---

## Salidas (tablas/figuras)

**Figuras**

* `output/figures/22_series_base100.png`
* `output/figures/22_trend_det_rates_2010_2023.png`
* `output/figures/22_cor_2010_2019_pre_grid.png` · `..._post_...` · `..._2010_2023_...`
* `output/figures/correlaciones_heatmap_niveles.png` · `..._dlog.png`
* `output/figures/24_irf_2010_2023_d_l_det_ext_rate_covid0.png`

**Tablas**

* `output/tables/22_cor_totales_stats.csv`
* `output/tables/19_stationarity_adf.csv`
* `output/tables/24_var_granger_resultados(_fdr).csv`
* `output/tables/25_ardl_bounds_results.csv` · `25_ardl_summary.csv`

---

## Resultados clave (hechos)

* **Niveles**: HC y población extranjera muestran **correlación muy alta** **sin COVID**.
* **Δlog**: señal **moderada-alta** sin COVID; **post (2021–2023)** **inestable** por **n=3**.
* **Detenciones vs % extranjeros**: correlación **similar** al excluir **2020–2021**.
* **Granger**: **no robusto** al excluir COVID → **no evidencia** de **causalidad estable** inmigración→criminalidad.

### Top-lines (cifras clave)

| Métrica                                             | Ventana               | Valor              | Fuente                                            |
| --------------------------------------------------- | --------------------- | ------------------ | ------------------------------------------------- |
| Pearson (HC vs pob. extranjera, niveles)            | sin COVID             | 0.993              | `output/tables/_debug_cor_hc_pop_prepost.csv`     |
| Spearman (HC vs pob. extranjera, niveles)           | sin COVID             | 1.000              | `output/tables/_debug_cor_hc_pop_prepost.csv`     |
| Pearson (Δlog, HC vs pob. extranjera)               | sin COVID             | 0.831              | `output/tables/_debug_cor_hc_pop_prepost.csv`     |
| Spearman (Δlog, HC vs pob. extranjera)              | sin COVID             | 0.909              | `output/tables/_debug_cor_hc_pop_prepost.csv`     |
| Pearson (det\_tot vs % extranjeros)                 | sin COVID             | 0.663              | `output/tables/22_cor_totales_stats.csv`          |
| Granger p\_FDR: det\_tot\_rate ← share\_extranjeros | 2010–2023 (con COVID) | 0.011 (pasa)       | `output/tables/24_var_granger_resultados_fdr.csv` |
| Granger p\_FDR: det\_tot\_rate ← share\_extranjeros | sin COVID             | 0.004 (no robusto) | `output/tables/24_var_granger_resultados_fdr.csv` |

**Interpretación prudente**: parte del co-movimiento puede ser **coyuntural** (movilidad/actividad, cambios de registro).

### Problemas conocidos

* Ventana **2021–2023** con **n=3**: inestabilidad de correlaciones en Δlog.
* Series en **niveles no estacionarias** (ADF): riesgo de correlación por **tendencia común**.
* Señal **Granger** depende de incluir **COVID**; sin COVID falla **diagnósticos de estabilidad**.

---

## Licencia

* **Código**: [MIT](LICENSE) — © 2025 Jesús Castro · JESUSCASTRODATA.
* **Documentos y figuras propias**: [CC BY 4.0](LICENSE-DOCS-CC-BY-4.0.md).
* **Datos originales**: ver [DATA\_LICENSE.md](DATA_LICENSE.md) (INE, Ministerio del Interior, Eurostat, etc.).

---

## Citación

Este repositorio incluye **CITATION.cff**. GitHub genera citas desde “Cite this repository”.

**APA (sugerido)**

> Castro, J. (2025). *Delitos e Inmigración en España (2010–2023).* Código: MIT. Documentos y figuras: CC BY 4.0.

**BibTeX (plantilla)**

```bibtex
@software{castro2025-delitos-inmigracion-espana,
  author    = {Jesús Castro},
  title     = {Delitos e Inmigración en España (2010--2023)},
  year      = {2025},
  url       = {<URL-del-repo>},
  version   = {v1.0},
  license   = {MIT},
  note      = {Documentos y figuras: CC BY 4.0},
  organization = {JESUSCASTRODATA}
}
```

---

## Contacto

Autor: **Jesús Castro** · **JESUSCASTRODATA**
Issues y PRs bienvenidos.

---

## Transparencia y neutralidad (antisesgo)

Este trabajo separa **hechos** (resultados empíricos) de **interpretaciones**; reporta **ventanas alternativas**, **FDR** y **limitaciones** de identificación. Las conclusiones evitan afirmaciones de **causalidad** sin diagnósticos robustos.
