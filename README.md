
# Delitos e Inmigración en España (2010–2023)

[![Code: MIT](https://img.shields.io/badge/Code-MIT-blue.svg)](LICENSE)
[![Docs: CC BY 4.0](https://img.shields.io/badge/Docs-CC%20BY%204.0-brightgreen.svg)](LICENSE-DOCS-CC-BY-4.0.md)
![Reproducibility: renv](https://img.shields.io/badge/reproducibility-renv-success.svg)
![R >= 4.3](https://img.shields.io/badge/R-%3E%3D%204.3-276DC3.svg)
![Quarto/RMarkdown](https://img.shields.io/badge/Quarto%2FRMarkdown-supported-lightgrey.svg)

> Análisis reproducible de la relación entre **población extranjera** y **criminalidad agregada** en España (2010–2023).

<p align="center">
  <img src="output/figures/20_niv_tasa_hc_100k_vs_pct_extranjeros_all.png" alt="Scatter: tasa HC vs % extranjeros (2010–2023)" width="720">
</p>

**Autor:** Jesús Castro (JESUSCASTRODATA) · **Periodo:** 2010–2023 · **Versión:** dev · **Estado:** Activo · **Últ. actualización:** 2025-09-28 · **Contacto:** [GitHub](https://github.com/JESUSCASTRODATA)

---

## Contenido

* [Resumen ejecutivo](#resumen-ejecutivo)
* [Preguntas de investigación](#preguntas-de-investigación)
* [Datos y cobertura](#datos-y-cobertura)
* [Metodología](#metodología)
* [Cómo reproducir](#cómo-reproducir)
* [Estructura del repositorio](#estructura-del-repositorio)
* [Productos de datos](#productos-de-datos-derivados-principales)
* [Resultados clave](#resultados-clave-hechos)
* [Supuestos, límites y uso responsable](#supuestos-límites-y-uso-responsable)
* [Licencia](#licencia) · [Citación](#citación)
* [Bitácora](#bitácora)

---

## Resumen ejecutivo

Estudio de la coevolución entre **% de población extranjera** y **criminalidad agregada** (hechos conocidos/ esclarecidos y detenciones) en España, 2010–2023.
Se reportan correlaciones **simples** (Pearson/Spearman/Kendall) y **parciales** (controles: PIB pc real, paro joven, AROPE; y en Δ: Δ ln PIB pc), con ventanas **completas** y **sin COVID** (2010–2019 & 2022–2023).

**Hechos principales (descriptivos, no causales):**

* **% extranjeros ↔ share de detenciones de extranjeros:** relación **fuerte y positiva** (mecánica de composición). Persiste al excluir COVID.
* **% extranjeros ↔ tasas HC/HE:** asociación **positiva en niveles**; en **variaciones** la señal es **más débil/inestable**.
* **Parciales:** al controlar por ciclo y condiciones sociales, las relaciones en **niveles** se **atenúan**; en **Δ** no hay evidencia estable de relación fuerte.

## Preguntas de investigación

* **Composición–composición:** ¿Cómo coevoluciona el **% de población extranjera** con el **share de detenciones de extranjeros** (niveles y Δ)?
* **Composición–tasas:** ¿Se asocia el **% de extranjeros** con **tasas de HC/HE por 100.000** (niveles y Δ)?
* **Rol del ciclo:** ¿Se mantiene el vínculo al controlar por **PIB pc real**, **paro joven** y **AROPE**?
* **COVID como quiebre:** ¿Las asociaciones en Δ sobreviven al excluir **2020–2021**?

## Datos y cobertura

* **Ámbito:** España · Total nacional · **2010–2023** (anual).
* **Fuentes:** INE (Padrón; AROPE; EPA), Ministerio del Interior (HC/HE; Detenciones), Eurostat (PIB pc real).
* **Reglas de consistencia:** **HE ≤ HC**, **det_ext ≤ det_tot** (excepciones documentadas).
* **Licenciamiento:** ver `DATA_LICENSE.md` (los datos conservan las condiciones de sus titulares).

## Metodología

* **ETL/QA:** estandarización (`ano`, tasas por 100.000), control de duplicados, y guardas de esquema.
* **Transformaciones:** Δ simples y **Δ ln** (p.ej., `dln_pib_pc`), y construcción robusta de `pct_extranjeros`.
* **Análisis:** correlaciones **simples** y **parciales**, ventanas **all** y **sin COVID**.
* **Gráficos:** tema **pearl** + paleta **Okabe–Ito** (`scripts/00_theme_figuras.R`), guardado con fondo homogéneo (`ggsave_pearl`).

## Cómo reproducir

**Entorno**

```r
install.packages("renv")
renv::restore()                 # paquetes reproducibles
```

**Pipeline mínimo**

```r
# 1) Construir core + derivadas (compacto, sin duplicados por ano)
source("scripts/20b_sync_core_from_inventory.R")   # genera data/processed/core_*.csv

# 2) Validación de IO/Schema (avisos de columnas/duplicados)
source("scripts/20_io_guard.R")                    # tablas QC en output/tables/

# 3) Análisis descriptivo + figuras + resumen
source("scripts/20_preguntas_basicas.R")           # output/tables/20_* y 20_pb_summary.md
```

**Informe (opcional)**

```r
# Quarto/Rmd si mantienes un informe
# rmarkdown::render("docs/informe_delitos_inmigracion_espana_2010_2023.Rmd")
```

**Requisitos:** R ≥ 4.3, pandoc ≥ 2.19; TinyTeX para PDF (opcional).

## Estructura del repositorio

```
.
├─ config/            # mapeos y whitelists de QA (HE≤HC, YoY)
├─ data/
│  ├─ raw/            # datos originales
│  └─ processed/      # derivados/limpios (core, derivadas, tasas, etc.)
├─ docs/              # informe Rmd/Quarto (opcional)
├─ output/
│  ├─ figures/        # gráficos (tema pearl + Okabe–Ito)
│  └─ tables/         # tablas y diagnósticos (QC y resultados)
├─ scripts/
│  ├─ 00_theme_figuras.R
│  ├─ 20b_sync_core_from_inventory.R
│  ├─ 20_io_guard.R
│  └─ 20_preguntas_basicas.R
└─ LICENSE · LICENSE-DOCS-CC-BY-4.0.md · DATA_LICENSE.md · CITATION.cff
```

## Productos de datos (derivados principales)

* `data/processed/core_indicadores.csv`
* `data/processed/core_indicadores_derivadas.csv`
* `output/tables/20_corr_niveles_{all|sin_covid}.csv`
* `output/tables/20_corr_deltas_{all|sin_covid}.csv`
* `output/tables/20_pcor_niveles_{all|sin_covid}.csv`
* `output/tables/20_pcor_deltas_{all|sin_covid}.csv`
* `output/tables/20_pb_summary.md` (resumen legible)

**Figuras destacadas (ejemplos)**

* `output/figures/20_niv_tasa_hc_100k_vs_pct_extranjeros_all.png`
* `output/figures/20_del_d_tasa_he_100k_vs_d_pct_extranjeros_sin_covid.png`

## Resultados clave (hechos)

* **Niveles:** % extranjeros co-mueve con **share de detenciones de extranjeros** (composición). Señal positiva con **tasas HC/HE**.
* **Variaciones:** la relación con **tasas** es **más débil/inestable** año a año; con **share** se mantiene.
* **Parciales:** controlando por **PIB pc**, **paro joven** y **AROPE**, la señal en niveles se **atenúa**; en Δ no hay evidencia fuerte y estable.

Para cifras y tablas exactas ver `output/tables/20_*` y el Markdown `20_pb_summary.md`.

## Supuestos, límites y uso responsable

* Las correlaciones **no** implican **causalidad**. Evitar inferencias individuales a partir de relaciones agregadas (*ecological fallacy*).
* Series en niveles pueden contener **tendencias comunes**; por eso se reportan **Δ** y **ventanas sin COVID**.
* **COVID** se trata como choque; resultados se reportan con y sin 2020–2021.

## Licencia

* **Código**: [MIT](LICENSE) — © 2025 Jesús Castro · JESUSCASTRODATA.
* **Documentos y figuras**: [CC BY 4.0](LICENSE-DOCS-CC-BY-4.0.md).
* **Datos originales**: ver `DATA_LICENSE.md` (condiciones de los titulares).

## Citación

Incluye `CITATION.cff`.

**APA (sugerido)**  
Castro, J. (2025). *Delitos e Inmigración en España (2010–2023).* Código: MIT. Documentos y figuras: CC BY 4.0.

**BibTeX**
```bibtex
@software{castro2025-delitos-inmigracion-espana,
  author       = {Jesús Castro},
  title        = {Delitos e Inmigración en España (2010--2023)},
  year         = {2025},
  url          = {https://github.com/JESUSCASTRODATA/crime-migration-spain},
  version      = {dev},
  license      = {MIT},
  note         = {Docs \& figures: CC BY 4.0}
}
```

