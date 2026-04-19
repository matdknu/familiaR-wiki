# Proyección de lo que falta

## Estado actual

Ya existe una base razonable para escribir un paper centrado en:

- comparación entre países
- cierre familiar y endogamia
- centralidad inter-familiar
- resultados H1 y H4

## Faltantes críticos

### 1. Revisar métricas de red por país

`paper/tables/T07_metricas_red_por_pais.csv` parece repetir exactamente los mismos valores para varios países. Antes de usar esa tabla en el paper, conviene revisar el script que la genera.

### 2. Cerrar una tabla de muestra comparable

Falta una tabla más paper-friendly con:

- número de personas por país
- número de familias por país
- número de vínculos por país
- cobertura de `cargos_politicos`
- cobertura de `partido_politico`
- cobertura de `infobox_json`

### 3. Homologar mejor la dimensión temporal

Hoy el argumento es fuerte en estructura relacional, pero todavía débil en tiempo histórico comparable. Lo más útil sería consolidar:

- `nacimiento_ano`
- `fallecimiento_ano`
- una variable simple de periodo histórico

### 4. Terminar la capa político-institucional

Falta correr de forma sistemática la clasificación LLM para producir:

- ideología
- bloque de cargo normalizado
- cargos diplomáticos
- vínculos transnacionales por pareja

## Faltantes deseables

- Robustez por umbral mínimo de tamaño familiar.
- Robustez excluyendo familias con muy baja cobertura política.
- Tabla corta de casos emblemáticos por país.
- Una figura-resumen conceptual del mecanismo: cierre, concentración y puente.

## Orden recomendado

### Fase 1: cerrar paper corto

Objetivo: sacar una versión escribible con lo que ya está.

1. Usar H1 y H4 como núcleo.
2. Agregar una tabla de muestra y calidad de datos.
3. Revisar o reemplazar `metricas_red_por_pais.csv`.
4. Escribir borrador corto comparado.

### Fase 2: extender paper

Objetivo: hacer el paper más fuerte y más original.

1. Correr clasificación LLM en R.
2. Construir variables de diplomacia e ideología.
3. Agregar una capa de vínculos transnacionales más precisa.
4. Reestimar contrastes incluyendo bloque diplomático y parejas entre países.

## Cómo proyectar lo que falta

La forma más ordenada de proyectarlo es pensar el paper en tres capas:

- `capa 1, ya disponible`: parentesco, cierre, endogamia, centralidad, comparación por país.
- `capa 2, parcialmente disponible`: partido limpio, cargos normalizados, años de nacimiento/fallecimiento.
- `capa 3, faltante pero preparada`: ideología, diplomacia, pertenencia élite refinada y vínculos transnacionales por LLM.

Eso permite escribir desde ya una versión publicable y, al mismo tiempo, dejar una agenda muy clara para una segunda versión más ambiciosa.
