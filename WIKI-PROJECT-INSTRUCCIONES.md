# Wiki Project – Instrucciones rápidas

## 1) Scraping de familias
- Requisitos: `python3`, `requests`, `beautifulsoup4`, `pandas`.
- Variables (recomendado para rate-limit):
  ```bash
  export WIKI_USER_AGENT="familiares/1.0"
  # opcional HTML bearer si Wikipedia lo requiere:
  # export WIKI_ACCESS_TOKEN="..."
  # export WIKI_BEARER_HTML=true
  ```
- Comando genérico:
  ```bash
  python scripts/01_scraping/scraper_all_families.py \
    --country <pais> \
    --workers 1 \
    --resume
  ```
  Países soportados en el repo: chile, argentina, mexico, peru, colombia, bolivia, uruguay, ecuador, paraguay, venezuela (si existe su consolidado).

- Categoría custom (ej. familias políticas de Argentina):
  ```bash
  python scripts/01_scraping/scraper_all_families.py \
    --country argentina \
    --category-custom "Familias_políticas_de_Argentina" \
    --resume --workers 1
  ```

- Scripts rápidos por país: ver `scripts/01_scraping/scrape_*.sh` (uno por país) y `scrape_todos_paises.sh` para secuencial.

## 2) Organizar consolidado
Tras el scraping, copiar y consolidar:
```bash
python scripts/03_analysis/organizar_repo.py
```
Esto genera:
- Copias ordenadas en `data/processed/familias/<pais>/consolidado.csv`.
- Merge LATAM en `data/processed/familias/_CONSOLIDADO_familias_latam.csv`.
- Resumen de filas por país en `outputs/tables/familias/resumen_conteo_filas.csv`.

## 2.1) Agregar familias manuales (extras)
- Coloca extras por país en `data/manual/familias_extra_<pais>.csv` usando el mismo header de los consolidado (`;` como separador). Ya hay un ejemplo para Chile con García-Huidobro y Saavedra.
- Ejecuta:
  ```bash
  python scripts/03_analysis/agregar_familias_manual.py
  ```
- Esto inyecta los extras en `data/processed/familias/<pais>/consolidado.csv` y recompone el LATAM en `data/processed/familias/_CONSOLIDADO_familias_latam.csv` (sin borrar nada de raw).

## 3) Extraer categorías de infobox
```bash
python scripts/03_analysis/extract_infobox_labels.py
```
Salidas en `outputs/tables/`:
- `infobox_labels_global.csv`
- `infobox_labels_por_pais.csv`
- `infobox_labels_top20_por_pais.csv`

## 4) Dónde quedan los datos
- RAW (original scraping): `data/raw/<pais>/familias/_CONSOLIDADO_todas_familias.csv`
- Procesado: `data/processed/familias/<pais>/consolidado.csv`
- LATAM combinado: `data/processed/familias/_CONSOLIDADO_familias_latam.csv`
- Figuras: `outputs/figures/`
- Tablas: `outputs/tables/`
- Reportes: `outputs/reports/`

## 5) Notas
- No se borran los RAW; los scripts crean copias ordenadas.
- Usa `--resume` para evitar repetir páginas ya scrapadas.
- Si hay problemas de red/403, sube el `WIKI_USER_AGENT`, baja `--workers` a 1 y considera `WIKI_BEARER_HTML=true` con token válido.
