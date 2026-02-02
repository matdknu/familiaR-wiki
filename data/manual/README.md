# 📝 DATA/MANUAL - Datos Ingresados Manualmente

Este directorio contiene datos de familias y personas que fueron agregados **manualmente** al proyecto, ya sea porque:

1. No existía una categoría de Wikipedia para la familia
2. Las relaciones familiares no estaban bien documentadas en Wikipedia
3. Se necesitaba agregar información adicional no disponible en Wikipedia
4. Se corrigieron errores en los datos scrapeados

## 📁 Estructura

```
data/manual/
├── README.md                              # Este archivo
├── familia_tovar_venezuela_manual.csv     # Familia Tovar de Venezuela (2026-01)
└── familias_extra_<pais>.csv              # Archivos adicionales por país
```

## 📋 Archivos Actuales

### Venezuela
| Archivo | Contenido | Fecha | Notas |
|---------|-----------|-------|-------|
| `familia_tovar_venezuela_manual.csv` | María Corina Machado, Martín Tovar Ponte, Manuel Felipe de Tovar, Martín Tovar y Tovar, Simón Planas, Simón Planas Suárez, Francisco y Fernando Rodríguez del Toro | 2026-01-25 | Conexiones familiares de María Corina Machado con próceres venezolanos |

### Chile
Los datos manuales de Chile están en `data/raw/chile/`:
| Archivo | Contenido |
|---------|-----------|
| `datos_chile_manual.csv` | Datos manuales iniciales |
| `datos_chile_manual2.csv` | Datos manuales adicionales |

### Argentina
| Archivo | Contenido |
|---------|-----------|
| `datos_argentina_manual2.csv` | Datos manuales de Argentina |

## 🔧 Cómo Agregar Datos Manuales

### Opción 1: Crear archivo CSV directamente

1. Crear un archivo CSV con el mismo formato que los consolidados:
   ```
   data/manual/familias_extra_<pais>.csv
   ```

2. Asegurarse de incluir todas las columnas requeridas (ver sección "Formato")

3. Ejecutar el script de integración:
   ```bash
   python scripts/03_analysis/agregar_familias_manual.py
   ```

### Opción 2: Usar el script add_tovar_family.py como plantilla

```bash
python scripts/add_tovar_family.py
```

Este script sirve como ejemplo de cómo agregar familias manualmente usando Python.

## 📊 Formato del CSV

El archivo debe usar `;` como separador y UTF-8 como encoding.

### Columnas requeridas

| Columna | Descripción | Ejemplo |
|---------|-------------|---------|
| nombre | Nombre completo | María Corina Machado |
| url | URL de Wikipedia | https://es.wikipedia.org/wiki/... |
| biografia_inicial | Primer párrafo | Política venezolana... |
| biografia | Biografía extendida | ... |
| fecha_nacimiento | Fecha de nacimiento | 7 de octubre de 1967 |
| lugar_nacimiento | Lugar de nacimiento | Caracas, Venezuela |
| nacionalidad | Nacionalidad | Venezolana |
| ocupacion | Ocupación | Ingeniera, política |
| familia | Nombre de la familia | Familia Tovar |
| perfiles_relacionados | Personas relacionadas | Simón Planas, Ricardo Zuloaga |
| timestamp | Fecha de ingreso | 2026-01-25T00:00:00 |
| categoria_origen | Origen del dato | manual |
| pais_origen | País | venezuela |

### Columnas opcionales

- `padres`, `conyuge`, `pareja`, `hijos`, `hermanos`
- `partido_politico`, `cargos_politicos`, `periodo`
- `educacion`, `alma_mater`
- `distinciones`, `premios`
- `infobox_json`, `infobox_completa`

## 🔄 Integración con el Pipeline

Los datos manuales se integran automáticamente al ejecutar:

```bash
# Agregar datos manuales a los consolidados
python scripts/03_analysis/agregar_familias_manual.py

# O al regenerar el consolidado LATAM
Rscript scripts/02_processing/run_pipeline.R
```

## ⚠️ Notas Importantes

1. **Evitar duplicados**: Verificar que la persona no exista ya en los datos scrapeados
2. **Consistencia**: Usar el mismo formato de fechas y nombres que el resto del proyecto
3. **URLs válidas**: Las URLs deben ser de Wikipedia en español
4. **Documentar**: Agregar una entrada en este README cuando se agreguen nuevos datos

## 📚 Historial de Cambios

| Fecha | Usuario | Cambio |
|-------|---------|--------|
| 2026-01-25 | Sistema | Agregada familia Tovar de Venezuela con María Corina Machado |
| ... | ... | ... |
