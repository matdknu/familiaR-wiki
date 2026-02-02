# Redes familiares multi-país: endogamia y métricas

## Resumen
- País más endogámico (matrimonios dentro de la familia): **Argentina** (91.8%).
- Total de personas analizadas: 5867.
- Conexiones entre países: 9.

## Datos y limpieza
- Fuentes: consolidados por país en `data/raw/<pais>/familias/_CONSOLIDADO_todas_familias.csv`.
- Limpieza de `infobox_json`: remoción de caracteres de control y validación JSON.

### Resumen de limpieza JSON

Ver tabla: `outputs/tables/json_limpieza_resumen.csv`.

## Metodología de redes
- Vínculos: padres, cónyuge, pareja, hijos, hermanos.
- Endogamia familiar: vínculos con misma familia (extracto desde `categoria_origen` o `familia`).
- Conexiones inter-país: vínculos entre personas de distintos países.

## Principales métricas de red por país

Ver tabla: `outputs/tables/metricas_red_por_pais.csv`.

## Endogamia por país

Figura: `outputs/figures/endogamia_por_pais_paper.png`.

## Puestos políticos por país

Ver tabla: `outputs/tables/puestos_politicos_por_pais_paper.csv`.

## Red de globos entre países

Figura: `outputs/figures/red_globos_multipais_paper.png`.

## Conexiones inter-país

Ver tabla: `outputs/tables/matriz_inter_pais.csv`.
