# WORKFLOW - Flujo vigente y reproducible

Este documento resume el flujo operativo actual del proyecto, priorizando scripts activos y salidas trazables.

## 1) Scraping por país (fuente primaria)

Comando recomendado:

```bash
export WIKI_USER_AGENT="familiares/1.0"
python scripts/01_scraping/scraper_all_families.py \
  --country <pais> \
  --workers 1 \
  --resume
```

Notas:
- `--resume` evita repetir páginas ya extraídas.
- `--workers 1` minimiza bloqueos y mejora estabilidad.
- Para categorías específicas (ej. Argentina política), usar `--category-custom`.

Salida esperada:
- `data/raw/<pais>/familias/familia_<nombre>_completo.csv`
- `data/raw/<pais>/familias/_CONSOLIDADO_todas_familias.csv`

## 2) Organización y consolidación LATAM

```bash
python scripts/03_analysis/organizar_repo.py
```

Este paso:
- copia consolidados a `data/processed/familias/<pais>/consolidado.csv`
- recompone `data/processed/familias/_CONSOLIDADO_familias_latam.csv`
- genera `outputs/tables/familias/resumen_conteo_filas.csv`

## 3) Incorporación de casos manuales (opcional)

Agregar extras en:
- `data/manual/familias_extra_<pais>.csv`

Luego ejecutar:

```bash
python scripts/03_analysis/agregar_familias_manual.py
```

## 4) Extracción de variables de infobox (apoyo comparativo)

```bash
python scripts/03_analysis/extract_infobox_labels.py
```

Salida esperada en `outputs/tables/`:
- `infobox_labels_global.csv`
- `infobox_labels_por_pais.csv`
- `infobox_labels_top20_por_pais.csv`

## 5) Análisis de redes (R)

Scripts principales del flujo de análisis:

```r
source("scripts/03_analysis/red_general_todos.R")
source("scripts/03_analysis/red_familias_multipais_v2.R")
source("scripts/03_analysis/analisis_endogamia_politica_multipais.R")
source("scripts/03_analysis/red_familias_especificas.R")
```

Scripts especializados (según pregunta de investigación):

```r
source("scripts/03_analysis/cambio_elites_1973.R")
source("scripts/03_analysis/red_chile_argentina.R")
source("scripts/03_analysis/tendencias_apellidos_cargos.R")
```

## 6) Control de calidad mínimo (antes de reportar)

- Duplicados de personas por URL canónica: deben ser cero tras consolidación.
- Auto-loops de red: remover en construcción de aristas.
- Cobertura por país: validar contra `resumen_conteo_filas.csv`.
- Trazabilidad: toda figura final debe tener script fuente explícito.

## 7) Limpieza recomendada (lo que no aporta)

Aplicar en este orden:
1. Mover scripts legacy no usados a `archive/`.
2. Eliminar referencias a scripts inexistentes en documentación.
3. Borrar outputs antiguos sin script de reproducción.
4. Mantener `data/raw/` como histórico; no borrar sin backup.

## 8) Artefactos principales del proyecto

- Datos integrados: `data/processed/familias/_CONSOLIDADO_familias_latam.csv`
- Figuras: `outputs/figures/`
- Tablas: `outputs/tables/`
- Reportes: `outputs/reports/`

Para instrucciones rápidas de ejecución, ver también `WIKI-PROJECT-INSTRUCCIONES.md`.
