# 📊 DATA - Datos del Proyecto

Este directorio contiene todos los datos del proyecto, organizados en tres niveles:

## 📁 Estructura

```
data/
├── raw/                    # Datos crudos (output del scraping)
│   ├── chile/familias/
│   ├── argentina/familias/
│   ├── colombia/familias/
│   ├── venezuela/familias/
│   ├── mexico/familias/
│   ├── peru/familias/
│   ├── ecuador/familias/
│   ├── bolivia/familias/
│   ├── uruguay/familias/
│   └── paraguay/familias/
│
├── processed/              # Datos procesados y consolidados
│   └── familias/
│       ├── chile/consolidado.csv
│       ├── argentina/consolidado.csv
│       ├── ...
│       └── _CONSOLIDADO_familias_latam.csv  ← ARCHIVO PRINCIPAL
│
└── manual/                 # Datos ingresados manualmente
    └── familia_tovar_venezuela_manual.csv
```

## 🔄 Flujo de Datos

```
┌─────────────────────────┐
│       SCRAPING          │
│  (Wikipedia/Manual)     │
└───────────┬─────────────┘
            │
            ▼
┌─────────────────────────┐
│       data/raw/         │
│  familia_*_completo.csv │
│  _CONSOLIDADO_*.csv     │
└───────────┬─────────────┘
            │
            ▼
┌─────────────────────────┐
│    02_PROCESSING        │
│   (Normalización)       │
└───────────┬─────────────┘
            │
            ▼
┌─────────────────────────┐
│    data/processed/      │
│    consolidado.csv      │
│  _CONSOLIDADO_LATAM.csv │ ← USAR ESTE
└─────────────────────────┘
```

## 📂 Detalle por Carpeta

### data/raw/ - Datos Crudos

Contiene los datos directamente extraídos de Wikipedia, sin procesar.

**Estructura por país:**
```
data/raw/<pais>/familias/
├── familia_alessandri_completo.csv
├── familia_balmaceda_completo.csv
├── familia_edwards_completo.csv
├── ...
└── _CONSOLIDADO_todas_familias.csv
```

**Origen de los datos:**
- `scripts/01_scraping/scraper_wikipedia_familias.py`
- `notebooks/02_scraping_paises/*.ipynb`

### data/processed/ - Datos Procesados

Contiene los datos limpios, normalizados y listos para análisis.

**Archivos principales:**
```
data/processed/familias/
├── chile/consolidado.csv        # Chile procesado
├── argentina/consolidado.csv    # Argentina procesado
├── colombia/consolidado.csv     # Colombia procesado
├── venezuela/consolidado.csv    # Venezuela procesado
├── mexico/consolidado.csv       # México procesado
├── peru/consolidado.csv         # Perú procesado
├── ecuador/consolidado.csv      # Ecuador procesado
├── bolivia/consolidado.csv      # Bolivia procesado
├── uruguay/consolidado.csv      # Uruguay procesado
├── paraguay/consolidado.csv     # Paraguay procesado
└── _CONSOLIDADO_familias_latam.csv  # ⭐ TODOS LOS PAÍSES
```

**⭐ El archivo `_CONSOLIDADO_familias_latam.csv` es el input principal para todos los análisis.**

### data/manual/ - Datos Manuales

Contiene datos agregados manualmente para familias o personas no disponibles en Wikipedia.

**Ver [data/manual/README.md](manual/README.md) para instrucciones.**

## 📋 Formato de los Datos

Todos los CSV usan:
- **Separador**: `;` (punto y coma)
- **Encoding**: UTF-8
- **Quote**: Comillas dobles para campos con texto largo

### Columnas Principales

| Columna | Tipo | Descripción |
|---------|------|-------------|
| nombre | string | Nombre completo |
| url | string | URL de Wikipedia |
| biografia_inicial | string | Primer párrafo |
| biografia | string | Biografía extendida |
| fecha_nacimiento | string | Fecha de nacimiento |
| lugar_nacimiento | string | Lugar de nacimiento |
| fecha_fallecimiento | string | Fecha de fallecimiento |
| nacionalidad | string | Nacionalidad |
| ocupacion | string | Ocupación principal |
| partido_politico | string | Partido político |
| padres | string | Nombres de padres |
| conyuge | string | Cónyuge(s) |
| hijos | string | Hijos |
| hermanos | string | Hermanos |
| familia | string | Familia asignada |
| cargos_politicos | string | Cargos políticos |
| infobox_json | JSON | Infobox completa |
| perfiles_relacionados | string | Links a relacionados |
| timestamp | datetime | Fecha del scraping |
| categoria_origen | string | Categoría Wikipedia |
| pais_origen | string | País |

## 📊 Estadísticas Actuales

| País | Familias | Personas |
|------|----------|----------|
| Chile | ~100 | ~1,400 |
| Argentina | ~165 | ~1,200 |
| Colombia | ~150 | ~1,400 |
| Venezuela | ~30 | ~280 |
| México | ~50 | ~500 |
| Perú | ~30 | ~300 |
| Ecuador | ~10 | ~200 |
| Bolivia | ~10 | ~100 |
| Uruguay | ~15 | ~150 |
| Paraguay | ~5 | ~50 |
| **TOTAL** | **~565** | **~6,700** |

## 🔧 Comandos Útiles

### Ver resumen de un consolidado
```bash
head -1 data/processed/familias/chile/consolidado.csv | tr ';' '\n' | nl
```

### Contar personas por país
```bash
wc -l data/processed/familias/*/consolidado.csv
```

### Buscar una persona
```bash
grep -i "balmaceda" data/processed/familias/chile/consolidado.csv
```

### Ver familias disponibles
```bash
cut -d';' -f21 data/processed/familias/chile/consolidado.csv | sort -u
```

## ⚠️ Notas Importantes

1. **No editar manualmente los archivos en `raw/`** - Son regenerados por el scraping
2. **Para correcciones, usar `data/manual/`** - Y ejecutar el script de integración
3. **El archivo LATAM es regenerado** - Cada vez que se procesa un país
4. **Backup antes de re-scrapear** - El scraping sobrescribe los archivos existentes
