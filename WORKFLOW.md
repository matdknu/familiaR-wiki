# 🔄 WORKFLOW - Flujo de Trabajo Completo

Este documento describe el flujo completo del proyecto, desde la extracción de datos hasta el análisis de redes.

## 📊 Diagrama General

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                           FUENTES DE DATOS                                  │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                             │
│   ┌───────────────────┐    ┌───────────────────┐    ┌──────────────────┐   │
│   │    WIKIPEDIA      │    │     MANUAL        │    │   NOTEBOOKS      │   │
│   │    (Scraping)     │    │   (CSV directo)   │    │  (Interactivo)   │   │
│   └─────────┬─────────┘    └─────────┬─────────┘    └────────┬─────────┘   │
│             │                        │                       │              │
│             ▼                        ▼                       ▼              │
│   ┌─────────────────────────────────────────────────────────────────────┐  │
│   │                     data/raw/<pais>/familias/                        │  │
│   │                   familia_<nombre>_completo.csv                      │  │
│   └─────────────────────────────────┬───────────────────────────────────┘  │
│                                     │                                       │
│                                     ▼                                       │
│   ┌─────────────────────────────────────────────────────────────────────┐  │
│   │               data/raw/<pais>/familias/                              │  │
│   │              _CONSOLIDADO_todas_familias.csv                         │  │
│   └─────────────────────────────────┬───────────────────────────────────┘  │
│                                     │                                       │
│                        ┌────────────┴────────────┐                         │
│                        │  02_PROCESSING          │                         │
│                        │  (R Scripts)            │                         │
│                        └────────────┬────────────┘                         │
│                                     │                                       │
│                                     ▼                                       │
│   ┌─────────────────────────────────────────────────────────────────────┐  │
│   │              data/processed/familias/<pais>/                         │  │
│   │                       consolidado.csv                                │  │
│   └─────────────────────────────────┬───────────────────────────────────┘  │
│                                     │                                       │
│                                     ▼                                       │
│   ┌─────────────────────────────────────────────────────────────────────┐  │
│   │              data/processed/familias/                                │  │
│   │              _CONSOLIDADO_familias_latam.csv                         │  │
│   └─────────────────────────────────┬───────────────────────────────────┘  │
│                                     │                                       │
│                        ┌────────────┴────────────┐                         │
│                        │  03_ANALYSIS            │                         │
│                        │  (R Scripts)            │                         │
│                        └────────────┬────────────┘                         │
│                                     │                                       │
│                                     ▼                                       │
│   ┌─────────────────────────────────────────────────────────────────────┐  │
│   │                      outputs/                                        │  │
│   │              figures/ tables/ reports/                               │  │
│   └─────────────────────────────────────────────────────────────────────┘  │
│                                                                             │
└─────────────────────────────────────────────────────────────────────────────┘
```

---

## 📝 PASO 1: Extracción de Datos (Scraping)

### Opción A: Script automatizado (RECOMENDADO)

```bash
# Desde la raíz del proyecto
cd wiki-chile_project

# Listar familias disponibles para un país
python scripts/01_scraping/scraper_wikipedia_familias.py --pais chile --listar

# Scrapear todas las familias de Chile
python scripts/01_scraping/scraper_wikipedia_familias.py --pais chile

# Scrapear una familia específica
python scripts/01_scraping/scraper_wikipedia_familias.py --pais chile --familia "Familia Balmaceda"
```

**Salida:**
```
data/raw/chile/familias/
├── familia_balmaceda_completo.csv
├── familia_edwards_completo.csv
├── familia_alessandri_completo.csv
├── ...
└── _CONSOLIDADO_todas_familias.csv
```

### Opción B: Notebooks interactivos

```
notebooks/02_scraping_paises/
├── familias-chile.ipynb           # Chile
├── familias-argentina_general.ipynb   # Argentina
├── familias-colombia.ipynb        # Colombia
├── familias-venezuela.ipynb       # Venezuela
└── ...
```

Útiles para:
- Debugging paso a paso
- Personalizar extracción
- Ver datos en tiempo real

### Opción C: Datos manuales

Para familias no disponibles en Wikipedia o datos adicionales:

1. Crear archivo CSV en `data/manual/`
2. Ejecutar script de integración:

```bash
python scripts/03_analysis/agregar_familias_manual.py
```

---

## 🔧 PASO 2: Procesamiento

### Ejecutar pipeline completo

```r
# En R
setwd("wiki-chile_project")
source("scripts/02_processing/run_pipeline.R")
```

### Scripts individuales

```r
# 1. Parseo y normalización
source("scripts/02_processing/01_parse_and_normalize.R")

# 2. Análisis descriptivo
source("scripts/02_processing/02_descriptive_analysis.R")

# 3. Visualizaciones básicas
source("scripts/02_processing/03_visualizations.R")
```

**Salida:**
```
data/processed/familias/
├── chile/consolidado.csv
├── argentina/consolidado.csv
├── venezuela/consolidado.csv
├── ...
└── _CONSOLIDADO_familias_latam.csv  # TODOS los países
```

---

## 📈 PASO 3: Análisis de Redes

### Análisis principales

```r
# Red general de todos los países
source("scripts/03_analysis/red_general_todos.R")

# Red multi-país con clusters
source("scripts/03_analysis/red_familias_multipais_v2.R")

# Análisis de endogamia política
source("scripts/03_analysis/analisis_endogamia_politica_multipais.R")

# Análisis de familias específicas
source("scripts/03_analysis/red_familias_especificas.R")
```

### Análisis especializados

```r
# Cambio de élites post-1973 (Chile)
source("scripts/03_analysis/cambio_elites_1973.R")

# Redes Chile-Argentina
source("scripts/03_analysis/red_chile_argentina.R")

# Tendencias de apellidos y cargos
source("scripts/03_analysis/tendencias_apellidos_cargos.R")
```

**Salida:**
```
outputs/
├── figures/
│   ├── red_familias_latam.png
│   ├── red_general_todos.png
│   ├── red_general_todos_interactiva.html
│   └── ...
├── tables/
│   ├── centralidad_familias.csv
│   ├── conexiones_transnacionales.csv
│   └── ...
└── reports/
    └── paper_redes_multipais.html
```

---

## 🗂️ Estructura de Carpetas

```
wiki-chile_project/
│
├── 📂 data/
│   ├── raw/                    # Datos crudos del scraping
│   │   ├── chile/familias/
│   │   ├── argentina/familias/
│   │   ├── colombia/familias/
│   │   └── ...
│   ├── processed/              # Datos procesados y consolidados
│   │   └── familias/
│   │       ├── chile/consolidado.csv
│   │       └── _CONSOLIDADO_familias_latam.csv
│   └── manual/                 # Datos agregados manualmente
│
├── 📂 scripts/
│   ├── 01_scraping/            # Extracción de Wikipedia
│   │   └── scraper_wikipedia_familias.py
│   ├── 02_processing/          # Limpieza y normalización
│   │   └── run_pipeline.R
│   └── 03_analysis/            # Análisis de redes
│       └── red_familias_multipais_v2.R
│
├── 📂 notebooks/
│   ├── 01_exploracion/         # Notebooks exploratorios
│   └── 02_scraping_paises/     # Notebooks de scraping por país
│
├── 📂 outputs/
│   ├── figures/                # Gráficos y visualizaciones
│   ├── tables/                 # Tablas de resultados
│   └── reports/                # Informes generados
│
├── 📂 archive/
│   └── deprecated_scripts/     # Scripts obsoletos
│
├── README.md                   # Descripción del proyecto
├── WORKFLOW.md                 # Este archivo
└── requirements.txt            # Dependencias Python
```

---

## 📋 Resumen de Fuentes de Datos

### Por país y origen

| País | Scraping Auto | Notebook | Manual | Total Aprox |
|------|---------------|----------|--------|-------------|
| 🇨🇱 Chile | ✅ | ✅ | ✅ | ~1,400 |
| 🇦🇷 Argentina | ✅ | ✅ | ✅ | ~1,200 |
| 🇨🇴 Colombia | ✅ | ✅ | - | ~1,400 |
| 🇻🇪 Venezuela | ✅ | ✅ | ✅ | ~280 |
| 🇲🇽 México | ✅ | ✅ | - | ~500 |
| 🇵🇪 Perú | ✅ | ✅ | - | ~300 |
| 🇪🇨 Ecuador | ✅ | - | - | ~200 |
| 🇧🇴 Bolivia | ✅ | - | - | ~100 |
| 🇺🇾 Uruguay | ✅ | - | - | ~150 |
| 🇵🇾 Paraguay | ✅ | - | - | ~50 |

### Familias agregadas manualmente

| País | Familia | Descripción | Fecha |
|------|---------|-------------|-------|
| Venezuela | Familia Tovar | María Corina Machado y conexiones históricas | 2026-01 |
| Chile | Familia Bello | Andrés Bello y descendientes | Existente |

---

## ⚡ Comandos Rápidos

### Setup inicial
```bash
# Clonar e instalar
git clone https://github.com/matdknu/familiaR-wiki.git
cd wiki-chile_project
pip install -r requirements.txt
```

### Scraping completo de un país
```bash
python scripts/01_scraping/scraper_wikipedia_familias.py --pais chile
```

### Procesar y consolidar
```r
source("scripts/02_processing/run_pipeline.R")
```

### Generar red principal
```r
source("scripts/03_analysis/red_familias_multipais_v2.R")
```

---

## ❓ FAQ

### ¿Cuánto tarda el scraping de un país?

Depende del número de familias:
- Chile (~100 familias): 2-4 horas
- Argentina (~165 familias): 3-5 horas
- Colombia (~150 familias): 3-4 horas

Usa `--max-familias 10` para pruebas rápidas.

### ¿Cómo agregar una persona que no está en Wikipedia?

1. Crea un archivo CSV en `data/manual/`
2. Sigue el formato de los consolidados
3. Ejecuta `python scripts/03_analysis/agregar_familias_manual.py`

### ¿Cómo actualizar datos existentes?

1. Re-ejecuta el scraping para el país
2. Los archivos se sobrescriben automáticamente
3. Vuelve a ejecutar el pipeline de procesamiento

### ¿Dónde están los datos finales?

```
data/processed/familias/_CONSOLIDADO_familias_latam.csv
```

Este archivo contiene TODOS los países combinados y es el input principal para los análisis.
