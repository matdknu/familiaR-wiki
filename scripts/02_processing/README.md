# 02_PROCESSING - Procesamiento y Normalización de Datos

Este directorio contiene scripts para limpiar, normalizar y consolidar los datos extraídos de Wikipedia.

## 📁 Estructura

```
02_processing/
├── README.md                      # Este archivo
├── 00_consolidar_familias.R       # Une todos los consolidados por país → _CONSOLIDADO_familias_latam.csv
├── leer_consolidado.R             # Lee el consolidado LATAM y opcionalmente parsea infobox_json
├── 02_api_gpt.R                   # (vacío; ver enriquecer_con_gpt.py)
├── COSTOS_API_GPT.md              # Estimación costos API OpenAI para enriquecimiento
├── enriquecer_con_gpt.py          # Enriquecer con GPT: cargos limpios, partido, fechas, redes
├── 03_red_enriquecido_gpt.R       # Red desde output enriquecido (complicaciones documentadas)
├── run_pipeline.R                 # Script principal que ejecuta todo el pipeline
├── 01_parse_and_normalize.R      # Parseo y normalización de datos
├── 02_descriptive_analysis.R     # Análisis descriptivo básico
├── 02_model_and_analyze.R       # Modelado y análisis avanzado
├── 03_visualizations.R           # Visualizaciones de datos procesados
└── enrich_infobox.R               # Enriquecimiento de datos de infobox
```

## 🚀 Uso

### Ejecutar el pipeline completo

```r
source("scripts/02_processing/run_pipeline.R")
```

### Consolidar todas las familias (data única LATAM)

```bash
# Desde la raíz del proyecto: une data/processed/familias/<pais>/consolidado.csv
# en data/processed/familias/_CONSOLIDADO_familias_latam.csv
Rscript scripts/02_processing/00_consolidar_familias.R
```

### Leer el consolidado (y opcionalmente el JSON)

```r
# Solo cargar el CSV
source("scripts/02_processing/leer_consolidado.R")
d <- leer_consolidado()  # tibble con todas las columnas

# Cargar y parsear la columna infobox_json → tablas normalizadas
datos <- leer_consolidado_y_parsear_json()
# datos$consolidado, datos$personas, datos$family_relations, etc.
```

```bash
# Por línea de comandos
Rscript scripts/02_processing/leer_consolidado.R        # resumen del CSV
Rscript scripts/02_processing/leer_consolidado.R --json  # resumen tras parsear JSON
```

### Enriquecer con API GPT (cargos limpios, partido, fechas, redes)

Para obtener **cargo_1, cargo_2, partido_limpio, nacimiento_ano, fallecimiento_ano, hijos_lista, conexion_otros_paises, redes_dentro_pais, redes_entre_paises**:

1. **Coste estimado:** ver `scripts/02_processing/COSTOS_API_GPT.md` (~1,50–3 USD con GPT-5 mini para ~6.700 filas; Batch API −50 %).
2. Definir `OPENAI_API_KEY` y ejecutar:
   ```bash
   pip install openai
   python scripts/02_processing/enriquecer_con_gpt.py --muestra 50   # prueba
   python scripts/02_processing/enriquecer_con_gpt.py                # todo
   python scripts/02_processing/enriquecer_con_gpt.py --resumir      # reanudar
   ```
3. Salida: `outputs/df_consolidado_enriquecido_gpt.csv`.

### Red desde datos enriquecidos (GPT)

Tras tener `outputs/df_consolidado_enriquecido_gpt.csv`, puedes generar una red como las de análisis pero usando ese output y documentando las **complicaciones de la data** (cobertura parcial, relaciones solo donde hay URLs, redes_* en texto libre, etc.):

```r
source("scripts/02_processing/03_red_enriquecido_gpt.R")
# O: Rscript scripts/02_processing/03_red_enriquecido_gpt.R
```

- **Entrada:** `outputs/df_consolidado_enriquecido_gpt.csv`
- **Salida:** `outputs/figures/red_enriquecido_gpt.png`
- En el script se explican cobertura parcial, extracción de aristas desde padres/conyuge/hijos, y uso de partido_limpio/cargo donde hay dato.

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
