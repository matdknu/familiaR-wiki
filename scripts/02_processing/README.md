# Scripts de Procesamiento y Análisis

Pipeline completo para procesar datos de Wikipedia infoboxes JSON y generar análisis.

## Estructura de Scripts

### 1. `01_parse_and_normalize.R`
**Propósito:** Parsing y normalización de datos

- Lee archivos CSV con `infobox_json`
- Parsea JSON de infoboxes de Wikipedia
- Genera tablas normalizadas:
  - `persons_all`: Información personal
  - `positions_all`: Cargos políticos con fechas, predecesores, sucesores
  - `family_relations_all`: Relaciones familiares
  - `education_all`: Educación
  - `affiliations_all`: Afiliaciones

**Funciones principales:**
- `parse_infobox_one()`: Parsea un infobox individual
- `parse_all_infoboxes()`: Procesamiento en lote

**Output:** Tablas CSV en `data/processed/`

---

### 2. `02_descriptive_analysis.R`
**Propósito:** Análisis descriptivo y estadísticas

- Resúmenes generales (conteos, porcentajes)
- Análisis de personas (nacionalidades, religiones, ocupaciones)
- Análisis de posiciones políticas (tipos, distribución temporal)
- Análisis de relaciones familiares
- Análisis de educación e instituciones
- Análisis de afiliaciones
- Resúmenes de carreras políticas

**Output:** Objetos R con estadísticas descriptivas (no guarda archivos)

---

### 3. `02_model_and_analyze.R`
**Propósito:** Modelado avanzado

- **Redes familiares:** Construcción de grafos (igraph), centralidad
- **Trayectorias políticas:** Análisis longitudinal de carreras
- **Patrones dinásticos:** Detección de overlap parent-child en cargos

**Output:** Objetos R:
- `g_family`: Grafo de red familiar
- `top_central`: Personas más centrales
- `career_summary`: Resumen de carreras
- `dynastic_overlap`: Patrones dinásticos

---

### 4. `03_visualizations.R`
**Propósito:** Generación de visualizaciones

- Gráficos de barras (tipos de cargo, relaciones, instituciones)
- Gráficos temporales (distribución por década/siglo)
- Red familiar (PNG estático + HTML interactivo si `visNetwork` disponible)
- Gráficos de centralidad y carreras

**Output:** Archivos PNG y HTML en `outputs/figures/`

---

### 5. `run_pipeline.R`
**Propósito:** Orquestador del pipeline completo

Ejecuta en orden:
1. Parsing y normalización
2. Análisis descriptivo
3. Modelado avanzado
4. Visualizaciones

**Uso:**
```r
Rscript scripts/02_processing/run_pipeline.R
```

---

## Flujo de Trabajo

```
CSV con infobox_json
    ↓
01_parse_and_normalize.R
    ↓
Tablas normalizadas (CSV)
    ↓
02_descriptive_analysis.R → Estadísticas descriptivas
    ↓
02_model_and_analyze.R → Modelos avanzados
    ↓
03_visualizations.R → Gráficos
```

---

## Requisitos

**Paquetes R:**
- `tidyverse` (dplyr, tidyr, stringr, purrr)
- `readr`
- `jsonlite`
- `ggplot2`
- `igraph`
- `visNetwork` (opcional, para red interactiva)

**Estructura de datos:**
- Archivos CSV en `data/raw/chile/familias/` con columna `infobox_json`
- Formato JSON: `[{"label": "...", "value_text": "...", "value_with_links": "..."}, ...]`

---

## Outputs

**Tablas normalizadas (`data/processed/`):**
- `persons_normalized.csv`
- `positions_normalized.csv`
- `family_relations_normalized.csv`
- `education_normalized.csv`
- `affiliations_normalized.csv`

**Visualizaciones (`outputs/figures/`):**
- `01_tipos_cargo.png`
- `02_distribucion_temporal.png`
- `03_top_cargos.png`
- `04_tipos_relacion.png`
- `05_red_familiar.png`
- `06_carreras_largas.png`
- `07_posiciones_por_siglo.png`
- `08_instituciones.png`
- `09_organizaciones.png`
- `10_centralidad.png`
- `red_familiar_interactiva.html` (si `visNetwork` disponible)

---

## Notas

- Los scripts están diseñados para ejecutarse en secuencia
- `run_pipeline.R` ejecuta todo el flujo automáticamente
- Los scripts individuales pueden ejecutarse por separado si se cargan las dependencias previas
- Los objetos intermedios se mantienen en memoria entre scripts cuando se ejecuta `run_pipeline.R`
