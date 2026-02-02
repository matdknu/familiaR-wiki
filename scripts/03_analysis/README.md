# 03_ANALYSIS - Análisis de Redes Familiares

Este directorio contiene scripts para el análisis de redes sociales de las familias de élite latinoamericanas.

## 📁 Estructura

```
03_analysis/
├── README.md                              # Este archivo
│
├── # ═══════════════════════════════════════════════════════════════
├── # ANÁLISIS PRINCIPALES (R)
├── # ═══════════════════════════════════════════════════════════════
├── red_general_todos.R                    # Red general de todas las familias
├── red_familias_multipais_v2.R            # Red multi-país con clusters
├── red_familias_multipais.R               # Versión anterior
├── network_analysis.R                     # Análisis de métricas de red
│
├── # ═══════════════════════════════════════════════════════════════
├── # ANÁLISIS ESPECIALIZADOS (R)
├── # ═══════════════════════════════════════════════════════════════
├── analisis_endogamia_politica_multipais.R    # Endogamia política
├── analisis_familias_clusters.R               # Análisis de clusters
├── cambio_elites_1973.R                       # Cambio de élites post-1973
├── tendencias_apellidos_cargos.R              # Tendencias históricas
│
├── # ═══════════════════════════════════════════════════════════════
├── # REDES BINACIONALES (R)
├── # ═══════════════════════════════════════════════════════════════
├── red_chile_argentina.R                  # Red Chile-Argentina básica
├── red_chile_argentina_enriquecida.R      # Red Chile-Argentina enriquecida
├── red_dos_globos_chile_argentina.R       # Visualización de dos globos
├── red_casos_especiales_chile_argentina.R # Casos especiales
├── red_multipais_latinoamerica.R          # Red multi-país general
├── red_globos_multipais.R                 # Visualización globos múltiples
│
├── # ═══════════════════════════════════════════════════════════════
├── # REDES ESPECÍFICAS (R)
├── # ═══════════════════════════════════════════════════════════════
├── red_familias_especificas.R             # Familias seleccionadas
│
├── # ═══════════════════════════════════════════════════════════════
├── # UTILIDADES PYTHON
├── # ═══════════════════════════════════════════════════════════════
├── agregar_familias_manual.py             # Agregar familias manuales
├── add_tovar_family.py                    # Ejemplo: agregar familia Tovar
├── extract_infobox_labels.py              # Extraer etiquetas de infobox
├── organizar_repo.py                      # Utilidad de organización
│
├── # ═══════════════════════════════════════════════════════════════
├── # EXPORTACIÓN
├── # ═══════════════════════════════════════════════════════════════
├── export_docs.R                          # Exportar documentación
└── paper_redes_multipais.R                # Generar paper
```

## 🚀 Uso

### Análisis principales

```r
# Red general de todos los países (el más completo)
source("scripts/03_analysis/red_general_todos.R")

# Red multi-país con clusters por país
source("scripts/03_analysis/red_familias_multipais_v2.R")

# Análisis de métricas de red
source("scripts/03_analysis/network_analysis.R")
```

### Análisis especializados

```r
# Análisis de endogamia política por país
source("scripts/03_analysis/analisis_endogamia_politica_multipais.R")

# Cambio de élites en Chile post-1973
source("scripts/03_analysis/cambio_elites_1973.R")

# Tendencias históricas de apellidos y cargos
source("scripts/03_analysis/tendencias_apellidos_cargos.R")
```

### Agregar datos manuales (Python)

```bash
# Agregar familias desde data/manual/familias_extra_*.csv
python scripts/03_analysis/agregar_familias_manual.py

# El script add_tovar_family.py es un ejemplo de cómo agregar una familia específica
python scripts/03_analysis/add_tovar_family.py
```

## 📊 Salidas Principales

### Figuras

| Script | Salida |
|--------|--------|
| `red_general_todos.R` | `outputs/figures/red_general_todos.png` |
| `red_familias_multipais_v2.R` | `outputs/figures/red_familias_latam.png` |
| `red_chile_argentina.R` | `outputs/figures/red_chile_argentina.png` |
| `analisis_endogamia_politica_multipais.R` | `outputs/figures/endogamia_por_pais.png` |

### Tablas

| Script | Salida |
|--------|--------|
| `network_analysis.R` | `outputs/tables/centralidad_*.csv` |
| `analisis_familias_clusters.R` | `outputs/tables/clusters_*.csv` |
| `tendencias_apellidos_cargos.R` | `outputs/tables/tendencias_*.csv` |

### Interactivos

| Script | Salida |
|--------|--------|
| `red_general_todos.R` | `outputs/figures/red_general_todos_interactiva.html` |

## 📋 Descripción de Scripts

### red_general_todos.R
Genera la red completa de todas las familias de todos los países.
- Input: `data/processed/familias/_CONSOLIDADO_familias_latam.csv`
- Output: PNG estático + HTML interactivo

### red_familias_multipais_v2.R
Genera una red multi-país con clusters separados por país.
- Usa layout Fruchterman-Reingold
- Colorea por país
- Destaca conexiones transnacionales

### analisis_endogamia_politica_multipais.R
Analiza patrones de endogamia (matrimonio dentro del mismo grupo) en familias políticas.
- Calcula índices de endogamia por país
- Compara patrones entre países
- Genera visualizaciones comparativas

### agregar_familias_manual.py
Agrega datos de familias ingresados manualmente al consolidado.
- Lee archivos de `data/manual/familias_extra_*.csv`
- Los integra a `data/processed/familias/<pais>/consolidado.csv`
- Regenera `_CONSOLIDADO_familias_latam.csv`

## ⚙️ Dependencias

### R
```r
install.packages(c(
  "tidyverse",
  "igraph",
  "tidygraph",
  "ggraph",
  "viridis",
  "ggrepel",
  "visNetwork",  # Para redes interactivas
  "htmlwidgets"
))
```

### Python
```bash
pip install pandas
```

## 📚 Referencias Metodológicas

- **Análisis de redes**: Wasserman, S., & Faust, K. (1994). Social Network Analysis.
- **Visualización**: Fruchterman, T. M., & Reingold, E. M. (1991). Graph drawing by force-directed placement.
- **Élites**: Padgett, J. F., & Ansell, C. K. (1993). Robust Action and the Rise of the Medici.
