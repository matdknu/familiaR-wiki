# Estructura propuesta (ordenada)

- `data/`
  - `raw/<pais>/familias/_CONSOLIDADO_todas_familias.csv` (fuente original por país, no tocar)
  - `processed/familias/<pais>/consolidado.csv` (copia limpia ordenada)
  - `processed/familias/_CONSOLIDADO_familias_latam.csv` (merge LATAM)
  - `processed/` (usar para salidas intermedias de limpieza)
- `outputs/`
  - `figures/` (todas las figuras finales)
  - `tables/`
    - `familias/` (resúmenes y tablas de familias, p.ej. `resumen_conteo_filas.csv`, `infobox_labels_*`)
    - otras tablas específicas por análisis
  - `reports/` (reportes markdown/pdf, p.ej. `paper_redes_multipais.md`)
- `scripts/`
  - `01_scraping/` (scrapers y runners por país; `scraper_all_families.py`, scripts *.sh)
  - `02_processing/` (ETL/limpieza en R)
  - `03_analysis/` (análisis, visualizaciones y utilidades en R)
- `notebooks/` (exploración)
- `docs/` (documentación y assets estáticos)

## Flujo sugerido
1) Scraping: `scripts/01_scraping/scraper_all_families.py` (o los *.sh por país) → escribe en `data/raw/<pais>/familias/`.
2) Organizar/copiar: `Rscript scripts/02_processing/organizar_repo.R` → copia a `data/processed/familias/<pais>/` y genera el LATAM.
3) Análisis/figuras/tablas: guardar siempre en `outputs/figures` y `outputs/tables` (subcarpetas por tema si aplica).
4) Reportes finales: `outputs/reports/`.

## Utilidades clave
- `scripts/02_processing/organizar_repo.R`: copia y consolida familias (por país y LATAM) y genera `outputs/tables/familias/resumen_conteo_filas.csv`.
- `scripts/02_processing/extract_infobox_labels.R`: extrae etiquetas de `infobox_json` a tablas en `outputs/tables/`.
