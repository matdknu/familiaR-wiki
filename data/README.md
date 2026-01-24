# Carpeta de Datos

Esta carpeta contiene los datos del proyecto. **Los archivos de datos NO se suben al repositorio** (ver `.gitignore`).

## Estructura

- `raw/`: Datos crudos de scraping organizados por país
- `processed/`: Datos procesados y limpios
- `manual/`: Datos ingresados manualmente (archivos Excel, etc.)

## Generar Datos

Para generar los datos, ejecuta:

```bash
cd scripts/01_scraping
python scraper_main.py --country chile --depth 1
```

Los datos se guardarán automáticamente en las carpetas correspondientes.
