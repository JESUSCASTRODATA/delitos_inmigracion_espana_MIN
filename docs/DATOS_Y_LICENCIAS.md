<!-- SPDX-License-Identifier: MIT AND CC-BY-4.0 -->

# Datos y licencias — Proyecto "Delitos e Inmigración en España (2010–2023)"

**Autor:** Jesús Castro · JESUSCASTRODATA  
**Última actualización:** 2025-10-27  
**Licencias aplicables:** MIT (código) · CC BY 4.0 (documentación y figuras)  
**Este documento:** CC BY 4.0

---

## 1. Fuentes de datos utilizadas

| Fuente | Descripción | Enlace oficial | Licencia / Uso |
|--------|--------------|----------------|----------------|
| **Instituto Nacional de Estadística (INE)** | Cifras de población residente por nacionalidad, edad y sexo (2010–2023). | https://www.ine.es | Datos públicos · © INE · Reutilización permitida con cita. |
| **Ministerio del Interior (España)** | Hechos conocidos, esclarecidos y detenciones (Anuario Estadístico de Criminalidad, 2010–2023). | https://estadisticasdecriminalidad.ses.mir.es | Datos públicos · © Ministerio del Interior. |
| **Eurostat** | Indicadores AROPE, Gini y PIB per cápita en volumen encadenado (2010–2023). | https://ec.europa.eu/eurostat | © European Union · Reuse permitted with attribution (*Decision 2011/833/EU*). |

---

## 2. Condiciones de uso de los datos

Los datos empleados son de **fuentes oficiales públicas** y se redistribuyen **solo con fines académicos, educativos o de investigación**, conforme a sus respectivas políticas de reutilización.

> ⚠️ Ninguno de los conjuntos de datos originales es de propiedad del autor de este proyecto.  
> Cualquier uso de los datos debe citar las fuentes originales (**INE**, **Ministerio del Interior**, **Eurostat**).

---

## 3. Transformaciones y derivadas

Los archivos en `data/processed/` y `output/` son **versiones derivadas** de los datos oficiales, procesadas para análisis estadístico reproducible:

- Estandarización de nombres y formatos (`UTF-8`, delimitador `,` o `;`, punto decimal).
- Agregación a nivel nacional (**TOTAL NACIONAL**).
- Cálculo de tasas por 100.000 habitantes y diferencias anuales (Δ).
- Limpieza y control de calidad (**HE ≤ HC**, sin duplicados, cobertura 2010–2023).
- Integración de indicadores socioeconómicos armonizados (PIB, AROPE, Paro 15–29).

> **Formato de salida:** CSV (UTF-8, delimitador `,`, punto decimal, encabezados en minúsculas).  
> **Propósito:** reproducibilidad, trazabilidad y compatibilidad con software estadístico (R, Python, Stata).

Estas derivadas se publican bajo **CC BY 4.0**, con la obligación de **mantener atribución al autor y a las fuentes originales**.

---

## 4. Resumen legal

| Elemento | Licencia | Atribución requerida |
|-----------|-----------|-----------------------|
| **Código (scripts R)** | MIT | “© 2025 Jesús Castro · JESUSCASTRODATA” |
| **Documentación y figuras** | CC BY 4.0 | “Castro, J. (2025). *Delitos e Inmigración en España (2010–2023)*” |
| **Datos procesados** | CC BY 4.0 + cita de fuente original | INE · Ministerio del Interior · Eurostat |
| **Datos brutos (raw)** | Propiedad de sus titulares | Consulta condiciones de cada fuente |

---

## 5. Contacto

Para aclaraciones sobre licencias, reutilización o solicitudes de colaboración:

**Jesús Castro** · [JESUSCASTRODATA](https://github.com/JESUSCASTRODATA)  
✉️ [Perfil profesional en LinkedIn](https://www.linkedin.com/in/jesuscastrodata/)

---

© 2024–2025 **Jesús Castro · JESUSCASTRODATA**  
Código: **MIT** · Documentación/Figuras: **CC BY 4.0** · Datos: según fuente original.

---

