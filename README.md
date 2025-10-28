# Delitos e Inmigración en España (2010–2023)

[![Code: MIT](https://img.shields.io/badge/Code-MIT-blue.svg)](LICENSE)
[![Docs: CC BY 4.0](https://img.shields.io/badge/Docs-CC%20BY%204.0-brightgreen.svg)](LICENSE-DOCS-CC-BY-4.0.md)
![Reproducibility: renv](https://img.shields.io/badge/reproducibility-renv-success.svg)
![R >= 4.3](https://img.shields.io/badge/R-%3E%3D%204.3-276DC3.svg)
![Quarto/RMarkdown](https://img.shields.io/badge/Quarto%2FRMarkdown-supported-lightgrey.svg)

> 🔍 Proyecto de análisis reproducible (R · 2010–2023) sobre la relación entre *población extranjera* y *criminalidad agregada* en España. Datos: INE, Ministerio del Interior y Eurostat.

<p align="center">
  <img src="output/figures/20_niv_tasa_hc_100k_vs_pct_extranjeros_all.png"
       alt="Scatter: tasa HC vs % extranjeros (2010–2023)" width="720">
</p>

**Autor:** Jesús Castro · [JESUSCASTRODATA](https://github.com/JESUSCASTRODATA)
**Periodo:** 2010–2023 · **Versión:** dev · **Estado:** Activo · **Últ. actualización:** 2025-09-28
**Contacto:** [LinkedIn](https://www.linkedin.com/in/jesuscastrodata/) · [GitHub](https://github.com/JESUSCASTRODATA)

---

## Contenido

* [Resumen ejecutivo](#resumen-ejecutivo)
* [Preguntas de investigación](#preguntas-de-investigación)
* [Datos y cobertura](#datos-y-cobertura)
* [Metodología](#metodología)
* [Cómo reproducir](#cómo-reproducir)
* [Estructura del repositorio](#estructura-del-repositorio)
* [Productos de datos](#productos-de-datos)
* [Resultados clave](#resultados-clave)
* [Supuestos y límites](#supuestos-y-límites)
* [Licencia y citación](#licencia-y-citación)
* [Bitácora](#bitácora)

---

## Resumen ejecutivo

Estudio de la coevolución entre el **porcentaje de población extranjera** y la **criminalidad agregada** (hechos conocidos, esclarecidos y detenciones) en España entre 2010–2023.

Se reportan correlaciones simples (Pearson, Spearman, Kendall) y parciales (controlando por PIB per cápita real, paro juvenil y AROPE), con ventanas completa y sin COVID‑19 (2010–2019 & 2022–2023).

### Principales hallazgos (descriptivos, no causales)

* Fuerte relación positiva entre % de extranjeros y share de detenciones de extranjeros (efecto mecánico de composición).
* En variaciones (Δ), la relación con las tasas se debilita.
* Al controlar por ciclo y condiciones sociales, las correlaciones se reducen y pierden estabilidad.

---

## Preguntas de investigación

1. ¿Cómo coevoluciona el % de población extranjera con el share de detenciones de extranjeros (niveles y variaciones)?
2. ¿Se asocia el % de extranjeros con las tasas de delitos (HC/HE) por 100.000 habitantes?
3. ¿Se mantiene el vínculo al controlar por PIB pc real, paro juvenil y AROPE?
4. ¿Qué ocurre al excluir los años de COVID‑19 (2020–2021)?

---

## Datos y cobertura

* **Ámbito:** España · Total nacional · Periodo 2010–2023 (anual).
* **Fuentes:** INE (Padrón, AROPE, EPA), Ministerio del Interior (HC/HE, Detenciones), Eurostat (PIB pc real).
* **Reglas de consistencia:** HE ≤ HC, det_ext ≤ det_tot (excepciones documentadas).
* **Licenciamiento:** ver [`docs/DATOS_Y_LICENCIAS.md`](docs/DATOS_Y_LICENCIAS.md).

---

## Metodología

* **ETL / QA:** estandarización (`ano`, tasas por 100k), control de duplicados, validación de esquema.
* **Transformaciones:** diferencias (Δ), tasas logarítmicas (Δ ln), construcción robusta de `pct_extranjeros`.
* **Análisis:** correlaciones simples y parciales, ventanas completas y sin COVID.
* **Visualización:** tema pearl + paleta Okabe–Ito (`00_theme_figuras.R`), exportación con `ggsave_pearl()`.

---

## Cómo reproducir

**Clonar e instalar entorno reproducible:**

```bash
git clone https://github.com/JESUSCASTRODATA/crime-migration-spain.git
cd crime-migration-spain
Rscript -e "install.packages('renv'); renv::restore()"
```

**Pipeline mínimo:**

```r
source("scripts/20b_sync_core_from_inventory.R")
source("scripts/20_io_guard.R")
source("scripts/20_preguntas_basicas.R")
```

**Render del informe:**

```r
rmarkdown::render("docs/informe_final.Rmd")
```

> Requisitos: R ≥ 4.3 · pandoc ≥ 2.19 · TinyTeX (para PDF opcional)

---

## Estructura del repositorio

```
.
├─ config/            # mapeos y whitelists de QA
├─ data/
│  ├─ raw/            # datos originales
│  └─ processed/      # derivados/limpios (core, tasas, etc.)
├─ docs/              # informe Rmd/Quarto
├─ output/
│  ├─ figures/        # gráficos (tema pearl + Okabe–Ito)
│  └─ tables/         # tablas y diagnósticos (QC + resultados)
├─ scripts/           # pipeline modular (00–80)
└─ LICENSE · LICENSE-DOCS-CC-BY-4.0.md · CITATION.cff
```

---

## Productos de datos

**Principales archivos:**

* `data/processed/core_indicadores.csv`
* `data/processed/core_indicadores_derivadas.csv`
* `output/tables/20_corr_*_{all|sin_covid}.csv`
* `output/tables/20_pcor_*_{all|sin_covid}.csv`

**Figuras destacadas:**

* `output/figures/20_niv_tasa_hc_100k_vs_pct_extranjeros_all.png`
* `output/figures/20_del_d_tasa_he_100k_vs_d_pct_extranjeros_sin_covid.png`
* `output/figures/granger_toda.png` (Toda–Yamamoto p‑values)

---

## Resultados clave

* En niveles, el % de población extranjera co‑mueve con el share de detenciones de extranjeros.
* En variaciones, la relación con las tasas se debilita; con *share* se mantiene.
* Al controlar por PIB, paro joven y AROPE, las correlaciones se reducen y se vuelven menos estables.

> Ver los CSV en `output/tables/20_*` y el resumen `20_pb_summary.md`.

---

## Supuestos y límites

* Las correlaciones no implican causalidad.
* Posible falacia ecológica si se extrapola a nivel individual.
* Las series en niveles pueden reflejar tendencias comunes; se contrasta con Δ.
* 2020–2021 tratado como choque exógeno; se comparan ventanas con y sin COVID.

---

## Licencia y citación

* **Código:** [MIT](LICENSE) © 2025 Jesús Castro · JESUSCASTRODATA
* **Documentos y figuras:** [CC BY 4.0](LICENSE-DOCS-CC-BY-4.0.md)
* **Datos originales:** según fuente (`docs/DATOS_Y_LICENCIAS.md`)

### Citación (APA)

> Castro, J. (2025). *Delitos e Inmigración en España (2010–2023).* Código MIT. Documentos y figuras CC BY 4.0.
> [https://github.com/JESUSCASTRODATA/crime-migration-spain](https://github.com/JESUSCASTRODATA/crime-migration-spain)

### BibTeX

```bibtex
@software{castro2025-delitos-inmigracion-espana,
  author       = {Jesús Castro},
  title        = {Delitos e Inmigración en España (2010--2023)},
  year         = {2025},
  url          = {https://github.com/JESUSCASTRODATA/crime-migration-spain},
  version      = {dev},
  license      = {MIT},
  note         = {Documentos y figuras CC BY 4.0}
}
```

---

## Bitácora

* **2025‑10‑27:** Pipeline completo (80_run_all.R) → 0 errores, estable.
* **2025‑10‑06:** Implementación de tests Toda–Yamamoto (55–57).
* **2025‑09‑28:** Limpieza final del core y QA.
* **2025‑09‑18:** Consolidación scripts 10–20 y control de esquema.
* **2025‑06‑03:** Inicio del proyecto.

---

<p align="center">
  <sub>Desarrollado por <b>Jesús Castro · JESUSCASTRODATA</b> — R, ggplot2 y Quarto  
  <br><i>Open Science · Reproducibility · Neutrality</i></sub>
</p>
