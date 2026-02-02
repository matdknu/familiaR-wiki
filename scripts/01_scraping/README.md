# Scripts de Scraping

Scripts para extraer datos de relaciones familiares desde Wikipedia.

## Scripts Disponibles

### 1. `scraper_main.py` - Scraper General
Scraper recursivo que parte de URLs iniciales y navega las relaciones familiares.

**Uso:**
```bash
# Scrapear por país con URLs predefinidas
python scraper_main.py --country chile --depth 1

# Usar archivo Excel con URLs manuales
python scraper_main.py --manual ../../data/manual/familia_link_manual2.xlsx --depth 1
```

**Parámetros:**
- `--country`: País a scrapear (`chile`, `argentina`, `mexico`)
- `--manual`: Ruta a archivo Excel con columna 'URL'
- `--depth`: Profundidad de búsqueda (0-2 recomendado)
- `--output`: Nombre personalizado para archivos de salida

---

### 2. `scraper_categories.py` - Scraper por Familia
Scraper específico que extrae TODA una familia desde su categoría de Wikipedia.

**Uso:**
```bash
# Por nombre de categoría
python scraper_categories.py --category "Familia_Alessandri"

# Por URL completa
python scraper_categories.py --url "https://es.wikipedia.org/wiki/Categoría:Familia_Alessandri"

# Con nombre personalizado de salida
python scraper_categories.py --category "Familia_Montt" --output "montt_2024"
```

**Parámetros:**
- `--category`: Nombre de la categoría (ej: "Familia_Alessandri")
- `--url`: URL completa de la categoría
- `--output`: Nombre personalizado para archivos de salida
- `--depth`: Profundidad máxima de scraping recursivo (default: 2)
- `--resume`: Reanudar desde archivo existente si está disponible

**Características:**
- ✅ Extrae TODOS los miembros listados en la categoría
- ✅ Obtiene información completa del infobox estructurada
- ✅ Incluye: fechas, lugares, ocupación, educación, familia, cargos políticos, etc.
- ✅ Mantiene enlaces a familiares para análisis de redes

**Ejemplo de uso para Familia Alessandri:**
```bash
cd scripts/01_scraping
python scraper_categories.py --category "Familia_Alessandri"
```

Esto generará: `data/raw/chile/familias/familia_alessandri_completo.csv`

---

### 3. `scraper_all_families.py` - Scraper Masivo
Scraper que extrae TODAS las familias de un país (por categoría principal).

**Uso:**
```bash
# Scrapear todas las familias (chile por defecto)
python scraper_all_families.py

# Scrapear familias de Argentina
python scraper_all_families.py --country argentina

# Scrapear solo las primeras 5 (para testing)
python scraper_all_families.py --limit 5
```

**Parámetros:**
- `--country`: País a scrapear (`chile`, `argentina`, `mexico`)
- `--category-custom`: Categoría personalizada (ej: `"Familias_políticas_de_Argentina"`)
- `--limit`: Número máximo de familias a scrapear (útil para testing)
- `--workers`: Número de procesos en paralelo (default: 1)
- `--resume`: Reanudar saltando familias ya descargadas

**Características:**
- ✅ Scraping automático de +100 familias chilenas
- ✅ Genera un archivo por familia
- ✅ Crea archivo consolidado con todas las familias
- ✅ Elimina duplicados (personas en múltiples familias)

**Salida:**
- `data/raw/chile/familias/familia_XXX_completo.csv` (uno por familia)
- `data/raw/chile/familias/_CONSOLIDADO_todas_familias.csv` (todos juntos)

---

## Archivos de Configuración

### `config.py`
Configuración centralizada:
- URLs iniciales por país
- Profundidad máxima de scraping (por defecto 1)
- Delay entre requests (por defecto 3s)
- Palabras clave para relaciones
- Prefijos de URLs a excluir
- Headers base y variables de entorno:
  - `WIKI_USER_AGENT` sobreescribe el UA para HTML/API
  - `WIKI_ACCESS_TOKEN` se usa solo si exportas además `WIKI_BEARER_HTML=true` (Bearer en HTML puede devolver 400)

### `scraper_utils.py`
Funciones auxiliares compartidas:
- `get_soup()`: Obtener HTML con reintentos
- `extract_person_data()`: Extraer datos de infobox
- `is_valid_person_url()`: Validar URLs

---

## Datos Extraídos

### Información de Personas
Cada persona incluye (cuando está disponible):
- Nombre completo
- URL de Wikipedia
- Fecha y lugar de nacimiento
- Fecha y lugar de fallecimiento
- Residencia
- Nacionalidad
- Religión
- Ocupación
- Partido político
- Educación / Alma mater
- Padres (con enlaces)
- Cónyuge (con enlaces)
- Pareja (con enlaces)
- Hijos (con enlaces)
- Hermanos (con enlaces)
- Familia extendida (con enlaces)
- Cargos políticos
- Período en cargo
- Predecesor/Sucesor
- Distinciones y premios
- Sitio web
- Categoría de origen
- Timestamp de extracción

---

## Ejemplos Prácticos

### Ejemplo 1: Scrapear Familia Alessandri
```bash
cd scripts/01_scraping
python scraper_categories.py --category "Familia_Alessandri"
```

### Ejemplo 2: Scrapear múltiples familias específicas
```bash
# Crear un script bash
for familia in "Familia_Alessandri" "Familia_Piñera" "Familia_Bachelet" "Familia_Montt"
do
    python scraper_categories.py --category "$familia"
    sleep 5  # Delay entre familias
done
```

### Ejemplo 3: Scrapear top 10 familias (testing)
```bash
python scraper_all_families.py --limit 10
```

### Ejemplo 4: Scraping completo de todas las familias
```bash
# ADVERTENCIA: Esto puede tomar varias horas
python scraper_all_families.py
```

### Ejemplo 5: Reanudar y paralelizar scraping masivo
```bash
python scraper_all_families.py --resume --workers 3
```

### Ejemplo 6: Scrapear Argentina (limit 5 para probar)
```bash
python scraper_all_families.py --country argentina --limit 5 --resume
```

### Ejemplo 7: Scrapear México (limit 5 para probar)
```bash
python scraper_all_families.py --country mexico --limit 5 --resume
```

### Ejemplo 8: Scrapear Familias Políticas de Argentina
```bash
# Scrapear todas las familias políticas de Argentina
export WIKI_USER_AGENT='familiares/1.0'
python scraper_all_families.py --country argentina --category-custom "Familias_políticas_de_Argentina" --resume --workers 1

# O usar el script helper:
./scripts/01_scraping/scrape_familias_politicas_argentina.sh
```

---

## Guía rápida multi-país

1) Preparar entorno
- Recomendado: setear un User-Agent propio:
  ```bash
  export WIKI_USER_AGENT="familiares/1.0"
  ```
- No uses Bearer en HTML salvo que lo necesites: por defecto el token solo se envía a la API; si exportas `WIKI_BEARER_HTML=true` se añadirá Authorization a HTML (puede causar 400).
- Si tienes token API: `export WIKI_ACCESS_TOKEN="..."` (se usará en API; en HTML solo con `WIKI_BEARER_HTML=true`).

2) Scraper masivo por país
- Chile (por defecto):
  ```bash
  python scraper_all_families.py --resume --workers 1
  ```
- Argentina:
  ```bash
  python scraper_all_families.py --country argentina --resume --workers 1
  ```
- México:
  ```bash
  python scraper_all_families.py --country mexico --resume --workers 1
  ```
- Usa `--limit` para pruebas pequeñas y `--workers N` para paralelizar (cuidado con rate limits).

3) Scraper por familia (categoría)
- Chile:
  ```bash
  python scraper_categories.py --category "Familia_Alessandri"
  ```
- Argentina (especificando país para la ruta de salida):
  ```bash
  python scraper_categories.py --category "Familia_Macri" --country argentina
  ```
- México:
  ```bash
  python scraper_categories.py --url "https://es.wikipedia.org/wiki/Categoría:Familia_X" --country mexico
  ```
- Flags útiles: `--depth` (profundidad recursiva), `--no-relatives` (sin recursión), `--resume`.

4) Scraper general por URLs
- Arranque rápido:
  ```bash
  python scraper_main.py --country chile --depth 1
  ```
- Con URLs manuales (Excel con columna URL):
  ```bash
  python scraper_main.py --manual ../../data/manual/familia_link_manual2.xlsx --depth 1
  ```
- Nota: `scraper_main` usa `data/raw/<pais>/{personas,relaciones}` como salida.

5) Salidas
- Masivo/categorías: `data/raw/<pais>/familias/familia_xxx_completo.csv` y `_CONSOLIDADO_todas_familias.csv`.
- General: `data/raw/<pais>/personas/<output>_personas.csv` y `.../relaciones/<output>_relaciones.csv`.

6) Manejo de errores comunes
- 403 Too many requests: sube `DELAY_SECONDS` (config.py), usa `WIKI_USER_AGENT`, baja `--workers`, y evita `WIKI_BEARER_HTML`.
- 400 con token: quita `WIKI_BEARER_HTML` para no mandar Bearer en HTML; deja solo `WIKI_USER_AGENT`.
- Categoría vacía: algunas categorías son contenedores sin páginas; prueba otra familia o subcategoría específica.

---

## Notas Importantes

1. **Rate Limiting**: Los scripts incluyen delays automáticos para respetar Wikipedia
2. **Datos en `.gitignore`**: Los CSVs generados NO se suben al repositorio
3. **Reintentos**: Todos los scripts reintentan automáticamente en caso de errores
4. **Enlaces preservados**: Los datos familiares incluyen URLs para análisis de redes
5. **Duplicados**: El script masivo elimina personas duplicadas automáticamente
6. **Token opcional**: Si tienes acceso vía API, exporta `WIKI_ACCESS_TOKEN` y se usará en los requests
   - `WIKI_USER_AGENT` para un User-Agent personalizado
   - Solo se envía el Bearer en HTML si defines `WIKI_BEARER_HTML=true`
   - Si ves 400 al usar token, quita `WIKI_BEARER_HTML` y deja solo `WIKI_USER_AGENT`

---

## Troubleshooting

**Error: No se encontró infobox**
- Algunas páginas no tienen infobox estructurado
- El script continúa con la siguiente persona

**Error: Connection timeout**
- Verifica tu conexión a internet
- Los scripts reintentan automáticamente
**Error: 403 / Too many requests**
- Sube el delay (`config.py`), define `WIKI_USER_AGENT`, y evita Bearer en HTML (no exportes `WIKI_BEARER_HTML`)
**Error: 400 con token**
- No envíes Bearer en HTML: elimina `WIKI_BEARER_HTML` o deja solo `WIKI_USER_AGENT`

**Error: Categoría vacía**
- Verifica que la categoría exista en Wikipedia
- Algunas categorías pueden no tener páginas asociadas

---

## Próximos Pasos

Después de scrapear:
1. Procesar datos: `cd ../02_processing && Rscript clean_data.R`
2. Analizar redes: `cd ../03_analysis && Rscript network_analysis.R`
