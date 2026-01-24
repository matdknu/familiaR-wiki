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
Scraper que extrae TODAS las familias chilenas desde la categoría principal.

**Uso:**
```bash
# Scrapear todas las familias
python scraper_all_families.py

# Scrapear solo las primeras 5 (para testing)
python scraper_all_families.py --limit 5
```

**Parámetros:**
- `--limit`: Número máximo de familias a scrapear (útil para testing)

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
- Profundidad máxima de scraping
- Delays entre requests
- Palabras clave para relaciones
- Prefijos de URLs a excluir

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

---

## Notas Importantes

1. **Rate Limiting**: Los scripts incluyen delays automáticos para respetar Wikipedia
2. **Datos en `.gitignore`**: Los CSVs generados NO se suben al repositorio
3. **Reintentos**: Todos los scripts reintentan automáticamente en caso de errores
4. **Enlaces preservados**: Los datos familiares incluyen URLs para análisis de redes
5. **Duplicados**: El script masivo elimina personas duplicadas automáticamente

---

## Troubleshooting

**Error: No se encontró infobox**
- Algunas páginas no tienen infobox estructurado
- El script continúa con la siguiente persona

**Error: Connection timeout**
- Verifica tu conexión a internet
- Los scripts reintentan automáticamente

**Error: Categoría vacía**
- Verifica que la categoría exista en Wikipedia
- Algunas categorías pueden no tener páginas asociadas

---

## Próximos Pasos

Después de scrapear:
1. Procesar datos: `cd ../02_processing && Rscript clean_data.R`
2. Analizar redes: `cd ../03_analysis && Rscript network_analysis.R`
