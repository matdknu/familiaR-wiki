# 📦 ARCHIVE - Archivos Obsoletos y Deprecated

Este directorio contiene archivos que ya no se usan en el proyecto pero se mantienen por referencia histórica.

## 📁 Estructura

```
archive/
├── README.md                    # Este archivo
├── deprecated_scripts/          # Scripts obsoletos
│   ├── familias_argentinas.py
│   ├── wikipedia_familias_mexico.py
│   ├── wikipedia_info.py        # Scraper básico original
│   ├── wikipedia_name.py
│   ├── script-inicial.R
│   └── no-function/
│       └── api-gpt.R
├── letras_jose.csv              # Datos de prueba
├── letras.csv
├── personas.csv
├── usuarios_kast.csv
└── musica.py
```

## ⚠️ Advertencia

**NO USAR** estos archivos para el proyecto actual. Han sido reemplazados por:

| Archivo obsoleto | Reemplazado por |
|------------------|-----------------|
| `wikipedia_info.py` | `scripts/01_scraping/scraper_wikipedia_familias.py` |
| `familias_argentinas.py` | `notebooks/02_scraping_paises/familias-argentina_general.ipynb` |
| `wikipedia_familias_mexico.py` | `notebooks/02_scraping_paises/familias-mexico_general.ipynb` |
| `script-inicial.R` | `scripts/02_processing/run_pipeline.R` |

## 📚 Historial

Estos archivos fueron utilizados en versiones anteriores del proyecto durante la fase de desarrollo inicial. Se mantienen archivados por si se necesita referencia al código original.

## 🗑️ Política de Limpieza

Archivos en este directorio pueden ser eliminados después de:
1. Verificar que no hay código único no migrado
2. Confirmar que los reemplazos funcionan correctamente
3. Documentar cualquier lógica especial que se haya perdido
