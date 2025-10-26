# Cuaderno de Bitácora · Delitos e Inmigración en España (2010–2023)

**Propósito.** Registrar, de forma concisa y verificable, las ejecuciones que generan **tablas** y **figuras**. Anotar **solo hechos operativos y resultados directos**, **explicando por qué se hizo** cada ejecución (motivación breve).

**Alcance.** Entradas para scripts `scripts/*.R` que produzcan salidas en `output/` o `data/processed/`. No incluye interpretación causal ni veredictos.

**Convenciones.**
- Fecha en formato `YYYY-MM-DD`.
- Rutas **relativas** al repositorio.
- Parámetros clave siempre explícitos (p. ej., ventanas, α, lags).
- “Resultado (hechos)” en 2–5 líneas, sin interpretación.

---

## Plantilla de entrada

### [YYYY-MM-DD] Título breve
**Por qué (motivación breve).** …  
**Objetivo.** …  
**Datos.** … (rutas/archivos de entrada)  
**Script.** `scripts/...`  
**Parámetros.** … (ventanas, α, lags, opciones relevantes)  
**Salidas.** … (rutas a tablas/figuras generadas)  
**Resultado (hechos).** … (2–5 líneas; qué se escribió, cuántos registros/series, etc.)  
**Notas.** … (pendientes, decisiones operativas, riesgos)

---

### 2025-09-19 Estacionariedad (ADF/PP/KPSS/ERS/NgP/ZA) — niveles y Δ
**Por qué.** Determinar la transformación adecuada (nivel vs. Δ) antes de correlaciones y modelos dinámicos; detectar posibles quiebres estructurales.

**Objetivo.** Clasificar la estacionariedad por ventana temporal (con/sin COVID y pre) usando baterías de tests y generar tabla ejecutiva + heatmaps.

**Datos.** `data/processed/core_indicadores_con_pib.csv`

**Script.** `scripts/18_stationarity_checks.R` (versión con ADF/PP/KPSS + ERS, Ng–Perron y Zivot–Andrews)

**Parámetros.**
- Ventanas: `con_covid_2010_2023`, `sin_covid_2010_2023` (excluye 2020–2021), `2010_2019_pre`, `2022_2023_post` (omitida si `N<8`).
- Modos: `level`, `diff` (Δ).
- Tests: ADF (drift/trend), PP (const/trend), KPSS (mu/tau), ERS (DF-GLS), Ng–Perron (MZt), Zivot–Andrews (quiebre).
- Umbrales: α = 5% y α = 10%.
- `min_n = 8`.

**Salidas.**
- Detalle pruebas: `output/tables/18_stationarity.csv`
- Resumen por var/ventana/modo: `output/tables/18_stationarity_summary.csv`
- Tabla ejecutiva (α=5%): `output/tables/18_stationarity_summary_table.csv`
- Tabla ejecutiva (α=10%): `output/tables/18_stationarity_summary_table_a10.csv`
- Heatmap (α=5%): `output/figures/18_stationarity_heatmap.png`, `..._heatmap.svg`
- Heatmap (α=10%): `output/figures/18_stationarity_heatmap_a10.png`, `..._heatmap_a10.svg`

**Resultado (hechos).**
- Entrada localizada y leída correctamente.
- Ventana `2022_2023_post` **omitida** automáticamente por `N<8`.
- Archivos **escritos** en `output/tables/` y `output/figures/` sin errores.
- Heatmaps generados con 4 clases: `Non-stationary`, `Level-stationary`, `Trend-stationary`, `Break-stationary` (si ZA rechaza).

**Notas.**
- Mantener ambos cortes (α=5% y α=10%) para reportar sensibilidad.
- Si se añaden nuevas variables o ventanas, re-ejecutar y registrar cambios aquí.
