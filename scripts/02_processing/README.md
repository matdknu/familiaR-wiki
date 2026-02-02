# 02_PROCESSING - Procesamiento y Normalización de Datos

Este directorio contiene scripts para limpiar, normalizar y consolidar los datos extraídos de Wikipedia.

## 📁 Estructura

```
02_processing/
├── README.md                      # Este archivo
├── run_pipeline.R                 # Script principal que ejecuta todo el pipeline
├── 01_parse_and_normalize.R       # Parseo y normalización de datos
├── 02_descriptive_analysis.R      # Análisis descriptivo básico
├── 02_model_and_analyze.R         # Modelado y análisis avanzado
├── 03_visualizations.R            # Visualizaciones de datos procesados
└── enrich_infobox.R               # Enriquecimiento de datos de infobox
```

## 🚀 Uso

### Ejecutar el pipeline completo

```r
source("scripts/02_processing/run_pipeline.R")
```

### Ejecutar scripts individuales

```r
# 1. Normalizar datos
source("scripts/02_processing/01_parse_and_normalize.R")

# 2. Análisis descriptivo
source("scripts/02_processing/02_descriptive_analysis.R")

# 3. Generar visualizaciones
source("scripts/02_processing/03_visualizations.R")
```

## 📊 Pipeline de Procesamiento

```
┌─────────────────────────────────────────────────────────────────┐
│  ENTRADA: data/raw/<pais>/familias/_CONSOLIDADO_todas_familias.csv  │
└─────────────────────────────────┬───────────────────────────────┘
                                  │
                                  ▼
┌─────────────────────────────────────────────────────────────────┐
│  01_parse_and_normalize.R                                       │
│  - Limpieza de texto                                            │
│  - Normalización de nombres                                     │
│  - Estandarización de fechas                                    │
│  - Extracción de relaciones familiares                          │
└─────────────────────────────────┬───────────────────────────────┘
                                  │
                                  ▼
┌─────────────────────────────────────────────────────────────────┐
│  02_descriptive_analysis.R                                      │
│  - Conteos por familia                                          │
│  - Distribución temporal                                        │
│  - Análisis de ocupaciones                                      │
│  - Análisis de cargos políticos                                 │
└─────────────────────────────────┬───────────────────────────────┘
                                  │
                                  ▼
┌─────────────────────────────────────────────────────────────────┐
│  SALIDA: data/processed/familias/<pais>/consolidado.csv         │
└─────────────────────────────────────────────────────────────────┘
```

## 📋 Transformaciones Principales

### 1. Normalización de nombres
- Eliminar acentos inconsistentes
- Estandarizar mayúsculas/minúsculas
- Separar nombres compuestos

### 2. Parseo de relaciones
- Extraer links de padres, cónyuges, hijos
- Crear columnas separadas para cada tipo de relación
- Resolver referencias cruzadas

### 3. Estandarización de fechas
- Convertir formatos de fecha variados
- Extraer año, mes, día
- Calcular edades aproximadas

### 4. Limpieza de texto
- Eliminar referencias [1], [2], etc.
- Eliminar notas de Wikipedia
- Normalizar espacios y caracteres especiales

## 📂 Salida

Los datos procesados se guardan en:

```
data/processed/familias/
├── chile/consolidado.csv
├── argentina/consolidado.csv
├── colombia/consolidado.csv
├── venezuela/consolidado.csv
├── mexico/consolidado.csv
├── peru/consolidado.csv
├── ecuador/consolidado.csv
├── bolivia/consolidado.csv
├── uruguay/consolidado.csv
├── paraguay/consolidado.csv
└── _CONSOLIDADO_familias_latam.csv    # Todos los países combinados
```

## ⚙️ Dependencias R

```r
install.packages(c(
  "tidyverse",
  "readr", 
  "janitor",
  "lubridate",
  "stringr",
  "jsonlite"
))
```
