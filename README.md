# Redes y reproducción de la elite chilena. Aproximación desde Wikipedia

¿Te sorprendería si te digo que existe una relación clara entre el Mio Cid Campeador y Vicente Huidobro? ¿Y Atahualpa y Piñera?

El presente proyecto busca describir las redes y estrategias de reproducción de la elite chilena usando Wikipedia como fuente de datos. Para ello, se utilizan técnicas de análisis de redes sociales y web scraping para analizar las relaciones familiares documentadas en artículos de Wikipedia.

![Red Familiar](outputs/figures/red_familiar.png)

---

## 📁 Estructura del Proyecto

```
wiki-chile_project/
├── data/                          # Datos del proyecto (NO se suben a git)
│   ├── raw/                       # Datos crudos de scraping por país
│   │   ├── chile/
│   │   ├── argentina/
│   │   ├── mexico/
│   │   └── otros_paises/
│   ├── processed/                 # Datos procesados y limpios
│   │   ├── personas/
│   │   └── relaciones/
│   └── manual/                    # Datos ingresados manualmente
│
├── scripts/                       # Scripts de análisis
│   ├── 01_scraping/              # Extracción de datos
│   │   ├── scraper_main.py       # Script principal de scraping
│   │   ├── scraper_utils.py      # Funciones auxiliares
│   │   └── config.py             # Configuración
│   ├── 02_processing/            # Limpieza de datos
│   │   └── clean_data.R
│   └── 03_analysis/              # Análisis y visualización
│       └── network_analysis.R
│
├── notebooks/                     # Notebooks exploratorios
│   ├── 01_exploracion/
│   ├── 02_scraping_paises/       # Notebooks de scraping por país
│   └── 03_analisis_redes/
│
├── outputs/                       # Resultados finales
│   ├── figures/                   # Gráficos y visualizaciones
│   ├── tables/                    # Tablas procesadas
│   └── reports/                   # Reportes y documentos
│
├── bibliography/                  # Referencias bibliográficas
├── archive/                       # Archivos obsoletos
└── README.md                      # Este archivo
```

---

## 🚀 Inicio Rápido

### Requisitos previos

**Python 3.8+** y **R 4.0+**

### Instalación

1. **Clonar el repositorio**
```bash
git clone <tu-repo>
cd wiki-chile_project
```

2. **Instalar dependencias Python**
```bash
pip install -r requirements.txt
```

3. **Instalar dependencias R**
```R
install.packages(c("readr", "tidyverse", "janitor", "ggraph", "tidygraph", "viridis"))
```

---

## 📊 Flujo de Trabajo

### 1. Scraping de Datos

Hay 3 formas de scrapear datos desde Wikipedia:

#### **Opción A: Scrapear una familia específica desde su categoría** (⭐ Recomendado)
```bash
cd scripts/01_scraping
python scraper_categories.py --category "Familia_Alessandri"
```

Esta opción extrae TODA la información del infobox de cada miembro de la familia:
- Datos biográficos completos
- Relaciones familiares con enlaces
- Cargos políticos
- Educación y ocupación

**Salida:** `data/raw/chile/familias/familia_alessandri_completo.csv`

#### **Opción B: Scrapear TODAS las familias chilenas**
```bash
cd scripts/01_scraping
python scraper_all_families.py
# O para testing: python scraper_all_families.py --limit 5
```

Extrae automáticamente +100 familias desde [Categoría:Familias de Chile](https://es.wikipedia.org/wiki/Categoría:Familias_de_Chile).

**Salida:** 
- Un archivo CSV por familia en `data/raw/chile/familias/`
- Archivo consolidado: `_CONSOLIDADO_todas_familias.csv`

#### **Opción C: Scraper recursivo desde URLs iniciales**
```bash
cd scripts/01_scraping
python scraper_main.py --country chile --depth 1
# O con Excel: python scraper_main.py --manual ../../data/manual/familia_link_manual2.xlsx
```

**Parámetros:**
- `--country`: País a scrapear (`chile`, `argentina`, `mexico`)
- `--manual`: Ruta a archivo Excel con columna 'URL'
- `--depth`: Profundidad de búsqueda (0-2 recomendado)

**Salida:**
- `data/raw/{país}/personas/{nombre}_personas.csv`
- `data/raw/{país}/relaciones/{nombre}_relaciones.csv`

### 2. Procesamiento de Datos

Limpiar y estructurar los datos crudos:

```R
cd scripts/02_processing
Rscript clean_data.R
```

### 3. Análisis de Redes

Generar visualizaciones de redes familiares:

```R
cd scripts/03_analysis
Rscript network_analysis.R
```

Los gráficos se guardan en `outputs/figures/`

---

## 📝 Notebooks Exploratorios

Los notebooks Jupyter se organizan por etapa:

1. **`notebooks/01_exploracion/`**: Análisis exploratorio inicial
2. **`notebooks/02_scraping_paises/`**: Notebooks de scraping específicos por país
3. **`notebooks/03_analisis_redes/`**: Análisis de redes y visualizaciones

---

## 🔐 Privacidad y Git

Los datos de scraping **NO se suben** al repositorio por privacidad y tamaño. El `.gitignore` está configurado para excluir:

- `data/raw/**/*.csv`
- `data/processed/**/*.csv`
- `data/manual/*.xlsx`

Para compartir datos, usar un servicio externo (Google Drive, etc.)

---

## 📚 Metodología

### Fuentes de Datos
- Wikipedia (español): Infoboxes de biografías
- Campos extraídos: nombre, fechas, ocupación, partido político, relaciones familiares

### Tipos de Relaciones
- Padre/Madre
- Cónyuge
- Hijo/Hija
- Hermano/Hermana
- Familia (genérico)

### Análisis de Redes
- Detección de comunidades (algoritmo Infomap)
- Visualización con layout Fruchterman-Reingold
- Análisis de centralidad y estructura

---

## 📖 Referencias

- Padgett, J. F., & Ansell, C. K. (1993). Robust Action and the Rise of the Medici, 1400-1434. *American Journal of Sociology*, 98(6), 1259-1319.

---

## 🤝 Contribuciones

Para contribuir:
1. Crear una rama: `git checkout -b feature/nueva-funcionalidad`
2. Hacer commit: `git commit -m "Descripción"`
3. Push: `git push origin feature/nueva-funcionalidad`
4. Crear Pull Request

---

## 📧 Contacto

Para preguntas o sugerencias, abrir un issue en el repositorio.

---

## 📄 Licencia

[Especificar licencia del proyecto]
