# Paper Workspace

Esta carpeta ordena el material del paper en un solo lugar: figuras, tablas, reportes, inventarios de variables y una proyección de lo que falta para cerrar el manuscrito.

## Estructura

```text
paper/
├── figures/      # figuras copiadas desde outputs y priorizadas para paper
├── tables/       # tablas copiadas desde outputs y priorizadas para paper
├── reports/      # reportes y artefactos textuales relevantes
├── manifests/    # inventarios de outputs, datasets y variables
├── manuscript/   # esquema del paper y organización narrativa
└── notes/        # hoja de ruta y faltantes
```

## Archivos clave

- `manifests/paper_outputs_inventory.csv`: qué outputs ya están listos, cuáles fueron copiados y qué rol cumplen en el paper.
- `manifests/paper_datasets_inventory.csv`: datasets base, nivel de análisis y estado.
- `manifests/paper_variable_dictionary.csv`: diccionario de variables disponibles, parciales y faltantes/proyectadas.
- `manuscript/outline.md`: estructura sugerida del paper conectada a outputs concretos.
- `notes/proyeccion_faltantes.md`: qué falta producir y en qué orden conviene hacerlo.
- `notes/imputacion_pais_y_senales.md`: guía metodológica de señales para imputar país, justificar decisiones y detectar puentes entre países.

## Cómo regenerar esta carpeta

```bash
Rscript scripts/03_analysis/reporting/setup_paper_folder.R
```

## Criterio

La lógica de esta carpeta no reemplaza `outputs/`; más bien selecciona y organiza lo necesario para escribir un paper reproducible sin tener que volver a recorrer todo el proyecto.
