# Outline del paper

## Manuscrito modular (PDF único)

- **Archivo maestro:** `elite_networks_paper.qmd`
- **Capítulos:** `capitulos/01_introduccion.qmd`, `02_marco_teorico.qmd`, `03_diseno.qmd`, `04_resultados.qmd`, `05_discusion_y_conclusion.qmd`, `06_referencias.md`
- **Compilar:** desde `paper/manuscript/`, `quarto render elite_networks_paper.qmd --to pdf` → `elite_networks_paper.pdf`
- El borrador monolítico `draft_elite_family_networks.qmd` queda como referencia; las tablas y figuras están integradas en el modular.

## 1. Pregunta

¿Hasta qué punto las élites familiares en América Latina concentran poder político mediante cierre familiar, endogamia y posiciones de intermediación entre familias y países?

## 2. Argumento central

- H1: familias con mayor cierre familiar tienden a concentrar más homogéneamente pertenencia partidaria y/o bloques de cargo.
- H4: familias más centrales en la red inter-familiar no necesariamente son las más cerradas; esa tensión es sustantiva y comparativa.

## 3. Estructura sugerida

### Introducción

- Problema: cómo identificar y comparar reproducción de élites familiares a escala LATAM.
- Aporte: combinar scraping de Wikipedia, redes familiares y clasificación político-institucional.

### Datos

- Base principal: `data/processed/familias/_CONSOLIDADO_familias_latam.csv`
- Cobertura y calidad: `paper/tables/T06_json_limpieza_resumen.csv`
- Universo analítico para H1/H4: `paper/tables/T01_h1_h4_familias_pais.csv`

### Estrategia analítica

- Nodo persona y agregación posterior a familia.
- Extracción de parentesco desde `padres`, `conyuge`, `pareja`, `hijos`, `hermanos`.
- Construcción de métricas:
  - cierre familiar
  - endogamia matrimonial
  - centralidad inter-familiar
  - concentración partidaria
  - concentración por bloque de cargo

### Descripción comparada

- Figura sugerida: `paper/figures/F01_exploracion_redes_facet.png`
- Tabla sugerida: `paper/tables/T05_elites_por_pais.csv`
- Complemento transnacional: `paper/figures/F02_union_entre_paises.png`

### Resultados H1

- Figura principal: `paper/figures/F03_h1_cierre_vs_concentracion.png`
- Tabla principal: `paper/tables/T02_h1_h4_resumen_pais.csv`
- Apéndice: `paper/tables/T03_endogamia_por_pais.csv`

### Resultados H4

- Figura principal: `paper/figures/F04_h4_centralidad_vs_cierre.png`
- Tabla principal: `paper/tables/T01_h1_h4_familias_pais.csv`

### Extensión internacional

- Tabla sugerida: `paper/tables/T04_conexiones_entre_paises.csv`
- Punto a desarrollar: vínculos diplomáticos y de pareja transnacional cuando estén enriquecidos vía LLM.

### Discusión

- Qué revela el cierre familiar.
- Cuándo la centralidad supone apertura estratégica y no clausura.
- Diferencias entre países con familias densas versus familias puente.

## 4. Lo que hoy ya permite escribir

- Una versión sólida del paper enfocada en H1 y H4.
- Un componente descriptivo comparado entre países.
- Una discusión preliminar sobre transnacionalización familiar.

## 5. Lo que conviene no sobrerreclamar todavía

- Ideología comparada fina.
- Diplomacia como capa robusta de red.
- Tendencias temporales fuertes, mientras no se normalice mejor el componente histórico.
