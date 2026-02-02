# 📓 NOTEBOOKS - Notebooks Interactivos

Este directorio contiene Jupyter Notebooks para exploración de datos y scraping interactivo.

## 📁 Estructura

```
notebooks/
├── README.md                    # Este archivo
├── 01_exploracion/              # Notebooks de exploración de datos
│   └── 1.clean-data.ipynb
└── 02_scraping_paises/          # Notebooks de scraping por país
    ├── familias_argentina_manual.ipynb
    ├── familias_chilenas_manual.ipynb
    ├── familias-argentina_general.ipynb
    ├── familias-argentina_politica.ipynb
    ├── familias-colombia_anexo.ipynb
    ├── familias-colombia.ipynb
    ├── familias-mexico_general.ipynb
    ├── familias-peru.ipynb
    └── familias-venezuela.ipynb
```

## 📋 Descripción

### 01_exploracion/

Notebooks para explorar y entender los datos procesados.

| Notebook | Descripción |
|----------|-------------|
| `1.clean-data.ipynb` | Exploración inicial y limpieza de datos |

### 02_scraping_paises/

Notebooks que contienen el código de scraping para cada país. Son útiles para:

- **Debugging**: Ver paso a paso qué datos se extraen
- **Personalización**: Modificar la lógica de extracción
- **Pruebas**: Probar con familias específicas antes de hacer scraping masivo
- **Documentación**: Entender cómo funciona el scraping

| Notebook | País | Descripción |
|----------|------|-------------|
| `familias-argentina_general.ipynb` | 🇦🇷 Argentina | Scraping general |
| `familias-argentina_politica.ipynb` | 🇦🇷 Argentina | Enfoque en familias políticas |
| `familias_argentina_manual.ipynb` | 🇦🇷 Argentina | Datos manuales |
| `familias_chilenas_manual.ipynb` | 🇨🇱 Chile | Datos manuales |
| `familias-colombia.ipynb` | 🇨🇴 Colombia | Scraping principal |
| `familias-colombia_anexo.ipynb` | 🇨🇴 Colombia | Familias adicionales |
| `familias-mexico_general.ipynb` | 🇲🇽 México | Scraping general |
| `familias-peru.ipynb` | 🇵🇪 Perú | Scraping principal |
| `familias-venezuela.ipynb` | 🇻🇪 Venezuela | Scraping principal |

## 🚀 Uso

### Ejecutar notebooks

```bash
# Iniciar Jupyter
jupyter notebook notebooks/

# O con JupyterLab
jupyter lab notebooks/
```

### En VS Code / Cursor

Los notebooks se pueden abrir y ejecutar directamente en el IDE.

## ⚠️ Notas

1. **Alternativa al script**: Los notebooks contienen la misma lógica que `scripts/01_scraping/scraper_wikipedia_familias.py`, pero de forma interactiva.

2. **Rate limiting**: Los notebooks incluyen delays para no sobrecargar Wikipedia. No los elimines.

3. **Datos de salida**: Los notebooks guardan los datos en `data/raw/<pais>/familias/`.

4. **Versión recomendada**: Para scraping masivo, preferir el script `scraper_wikipedia_familias.py`. Los notebooks son mejores para pruebas y debugging.

## 🔧 Dependencias

```bash
pip install jupyter pandas requests beautifulsoup4 lxml
```
