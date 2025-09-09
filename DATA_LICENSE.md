# Datos y licencias — Delitos e Inmigración en España (2010–2023)

Este proyecto reutiliza **datos oficiales** de terceros. Salvo que se indique lo contrario, los **datos originales** mantienen sus **términos de uso** y **licencias** de los titulares correspondientes. **Este repositorio no re-licencia los datos originales**.

## Fuentes principales (cite-as)
- **INE (España)** — Población residente por nacionalidad y otras series.
  - Cita sugerida: *Instituto Nacional de Estadística (INE). Padrón continuo, varias ediciones (2010–2025). Reutilización de la información del sector público conforme a sus condiciones.*
- **Ministerio del Interior (España)** — Hechos conocidos/esclarecidos y detenciones.
  - Cita sugerida: *Ministerio del Interior. Estadística de Criminalidad, 2010–2025. Reutilización de la información del sector público conforme a sus condiciones.*
- **Eurostat** — PIB per cápita en volumen encadenado (SDMX).
  - Cita sugerida: *Eurostat. GDP per capita in chain-linked volumes (extraction 2025).*

> Verifica y respeta siempre los **términos de reutilización** vigentes de cada fuente. Este repositorio ofrece **derivados** (tablas/figuras procesadas) bajo CC BY 4.0 **solo** cuando son **creación propia**. Los CSV procesados que provienen directamente de los datos de terceros heredan las condiciones de las fuentes.

## Mapa de licencias dentro del repo
- **Código** → `LICENSE` (MIT).  
- **Documentos/figuras propias** → `LICENSE-DOCS-CC-BY-4.0.md` (CC BY 4.0).  
- **Datos originales de terceros** → términos de sus propietarios (ver enlaces y avisos de cada fuente).

## Recomendaciones de atribución en informes
Incluye en “Fuentes y cobertura” una tabla con **fuente**, **periodo**, **enlace** y **condiciones de uso** de cada dataset. Cita explícitamente a INE, Ministerio del Interior y Eurostat cuando uses sus datos.

## Plantilla de tabla de procedencia (provenance)
| dataset                         | periodo     | fuente                          | URL/ID (si aplica) | licencia/uso             |
|---------------------------------|-------------|----------------------------------|---------------------|--------------------------|
| población_residente_nacionalidad| 2010–2025 | INE                              |                     | condiciones INE          |
| hechos_conocidos_total          | 2010–2025 | Ministerio del Interior          |                     | condiciones Min. Interior|
| hechos_esclarecidos_total       | 2010–2025 | Ministerio del Interior          |                     | condiciones Min. Interior|
| detenciones_totales             | 2010–2025 | Ministerio del Interior          |                     | condiciones Min. Interior|
| detenciones_extranjeros         | 2010–2025 | Ministerio del Interior          |                     | condiciones Min. Interior|
| pib_pc_volumen_encadenado       | 2010–2025 | Eurostat                         |                     | condiciones Eurostat     |

