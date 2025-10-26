# Datos y licencias — Delitos e Inmigración en España (2010–2023)

Este proyecto reutiliza **datos oficiales** de terceros. Salvo indicación expresa, los **datos originales** mantienen sus **términos de uso** y licencias de sus titulares. **Este repositorio no re‑licencia los datos originales**.

## Fuentes principales (cite‑as)

* **INE (España)** — Padrón y series demográficas.

  *Cita sugerida:* Instituto Nacional de Estadística (INE). *Padrón continuo*, varias ediciones (2010–2025). Reutilización permitida con cita de fuente y sin desnaturalizar la información (véase el Aviso legal del INE).

* **Ministerio del Interior (España)** — Hechos conocidos/esclarecidos y detenciones/investigados (SEC).

  *Cita sugerida:* Ministerio del Interior. *Portal Estadístico de Criminalidad*, series 2010–2025. Reutilización permitida con la mención “Origen de los datos: Portal Estadístico de Criminalidad”, indicando fecha de última actualización y sin desnaturalizar la información (véase el Aviso legal del portal).

* **Eurostat (Comisión Europea)** — PIB per cápita en volumen encadenado (SDMX).

  *Cita sugerida:* Eurostat. *GDP per capita in chain‑linked volumes* (extracción 2025). Reutilización autorizada con reconocimiento de la fuente conforme a la política de licencia de la Comisión Europea.

> Nota marco (España): La reutilización de información del sector público se rige por la **Ley 37/2007** y sus condiciones generales (citar la fuente, no desnaturalizar, conservar metadatos/fecha de actualización).

## Mapa de licencias dentro del repo

* **Código** → `LICENSE` (**MIT**).
* **Documentos y figuras propias** → `LICENSE-DOCS-CC-BY-4.0.md` (**CC BY 4.0**).
* **Datos originales de terceros** → términos/licencias de sus propietarios (ver enlaces y avisos de cada fuente).
* **Derivados** (tablas agregadas y figuras en `output/`) se publican bajo **CC BY 4.0** **solo** cuando constituyan creación/transformación propia y **no** reproduzcan íntegramente datasets de terceros sin cambios.

## Recomendaciones de atribución en informes

Incluye una sección “Fuentes y cobertura” con **fuente**, **periodo**, **URL** y **condiciones de uso**. Para Interior, añade literalmente: *“Origen de los datos: Portal Estadístico de Criminalidad”* y la **fecha de última actualización** cuando conste.

## Plantilla de tabla de procedencia (provenance)

| dataset                          | periodo   | fuente                  | URL/ID (si aplica) | condiciones/licencia                                  |
| -------------------------------- | --------- | ----------------------- | ------------------ | ----------------------------------------------------- |
| poblacion_residente_nacionalidad | 2010–2025 | INE                     |                    | Reutilización con cita y sin desnaturalizar           |
| hechos_conocidos_total           | 2010–2025 | Min. del Interior (SEC) |                    | Citar origen, fecha última actualización, sin alterar |
| hechos_esclarecidos_total        | 2010–2025 | Min. del Interior (SEC) |                    | Ídem anterior                                         |
| detenciones_totales/nacionalidad | 2010–2025 | Min. del Interior (SEC) |                    | Ídem anterior                                         |
| pib_pc_volumen_encadenado        | 2010–2025 | Eurostat                |                    | Reutilización con reconocimiento de fuente            |

> Sustituye las URLs por páginas exactas de extracción cuando cites tablas concretas.

## Nota sobre derivados

Los ficheros en `output/` (por ejemplo, `tables/` y `figures/`) son **derivados** generados por los scripts. Salvo que contengan material de terceros no transformado, se publican bajo **CC BY 4.0** (ver `LICENSE-DOCS-CC-BY-4.0.md`). Cuando un CSV procesado **sea esencialmente una copia** del dato original (solo limpieza mínima), **hereda** las condiciones de la fuente.

---

## Checklist de cumplimiento

* [ ] **Citar la fuente exacta** (INE / Portal SEC / Eurostat) en leyendas y pies de figura.
* [ ] **No desnaturalizar**: no presentar fuera de contexto; documentar exclusiones (p. ej., *“excluye 2020–2021”*).
* [ ] **Indicar la fecha de última actualización** si está disponible en la tabla/portal.
* [ ] Mantener en `README`/`docs` un **provenance** por dataset con enlace a sus términos.
* [ ] Publicar **código bajo MIT** y **docs/figuras bajo CC BY 4.0**.
