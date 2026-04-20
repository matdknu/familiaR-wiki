# DIAGNÓSTICO COMPLETO DEL PIPELINE — Élites Latinoamericanas

**Fecha:** 2026-04-15  
**Consolidado:** 6,731 filas × 42 columnas  
**Scripts auditados:** `02_leer_data.R`, `01_exploracion.R`, `02–07_*.R` (core), `reporting/*.R`

---

## Resumen Ejecutivo

1. **466 URLs duplicadas** en el consolidado (personas scrapeadas múltiples veces), inflando conteos y generando aristas duplicadas en el grafo.
2. **Asignación de país rota para ~2,600 personas**: `pais_base` usa la familia de origen (CSV), no la nacionalidad real. Personas paraguayas, bolivianas, chilenas aparecen como "argentinas" porque entraron por familias argentinas.
3. **Aristas "misma_familia" causan explosión combinatoria**: 287,439 aristas totales, la mayoría son de tipo familia (O(n²) por familia). Las 10 familias más grandes generan >100,000 aristas solo entre sí.
4. **Alianzas matrimoniales infladas**: pares como Moctezuma-Pizarro con 21 "matrimonios" son cross-joins de relaciones, no 21 matrimonios reales.
5. **elite_familiar = 100%**: toda persona en el dataset es "élite familiar", lo cual no discrimina. Solo 4 conexiones "transnacionales" en clasificación de élites vs ~1,400 reales.

---

## Tabla de Hallazgos

| ID | Severidad | Sección | Descripción | Archivo afectado | Acción sugerida |
|----|-----------|---------|-------------|------------------|-----------------|
| H01 | 🔴 CRÍTICO | 1. Consolidado | 466 URLs duplicadas (hasta 4×). Personas duplicadas inflan conteos y generan aristas espurias. | `_CONSOLIDADO_familias_latam.csv` | Deduplicar por URL antes de procesar. Mantener la fila más completa. |
| H02 | 🟡 IMPORTANTE | 1. Consolidado | 2 filas sin nombre. | `_CONSOLIDADO_familias_latam.csv` | Filtrar filas con `nombre` vacío. |
| H03 | 🔴 CRÍTICO | 2. Fechas | Año 3060 en Andrés de Santa Cruz (familia Hurtado, Colombia). Error en el scraping. | `02_leer_data.R` | Filtrar `anio_nacimiento > 2026` → NA. Rascrapear esa persona. |
| H04 | 🟡 IMPORTANTE | 2. Fechas | Pero Niño con año 1378 (< 1400 cutoff). Persona real del siglo XIV. | `02_leer_data.R` | Ajustar cutoff a 1300 o mantener. No es error, es persona medieval real. |
| H05 | 🟡 IMPORTANTE | 2. Fechas | 699 fechas de nacimiento no parseadas (10.4%). Formatos: "Abril de 1762", "Buenos Aires , 28...", "ca 1803", "Siglo XVII". | `02_leer_data.R` → `parse_fecha_lugar()` | Agregar regex para: "Mes de YYYY", "Ciudad , DD de mes de YYYY", "ca/circa YYYY", "Siglo XX". |
| H06 | 🟡 IMPORTANTE | 2. Fechas | "Siglo XIX" se clasifica como `causa_muerte` en vez de período temporal. | `02_leer_data.R` → `parse_fecha_lugar()` | No contiene `\d{4}` → cae en rama de "causa". Agregar detección de "Siglo" como período, no causa. |
| H07 | 🔴 CRÍTICO | 3. País | 2,602 personas con `pais_base ≠ pais_inferido`. La cascada actual es: `pais > pais_origen > pais_inferido`. Pero `pais` y `pais_origen` vienen del CSV (familia de entrada), no de la persona. | `02_leer_data.R` línea 280 | Invertir: `coalesce(pais_inferido, pais, pais_origen)`. O usar `01_exploracion.R` que ya tiene `pais_persona = coalesce(override, inferido, pais)`. |
| H08 | 🔴 CRÍTICO | 3. País | "Virreinato del Perú" en `lugar_nacimiento` asigna `peru` a personas nacidas en Argentina, Chile, etc. 601 personas con entidades coloniales. | `02_leer_data.R` → `inferir_pais()` | Virreinato del Perú cubría Argentina, Chile, Bolivia, Perú, etc. No se puede asignar un solo país. Usar `NA` o cruzar con ciudad + entidad colonial. |
| H09 | 🟡 IMPORTANTE | 3. País | Solo 4 overrides manuales. Debería haber más (ej: todas las personas de Paraguay que entraron por familias argentinas). | `url_pais_extra.csv` | Generar lista de candidatos: `pais != pais_inferido` y completar manualmente. |
| H10 | 🟡 IMPORTANTE | 3. País | `01_exploracion.R` tiene su propia lógica de `pais_persona` (override > inferido > pais) que DIFIERE de `02_leer_data.R` (`pais_base = pais > pais_origen > pais_inferido`). Inconsistencia entre scripts. | `01_exploracion.R`, `02_leer_data.R` | Unificar: que `02_leer_data.R` produzca un `pais_efectivo` confiable y todos los scripts lo usen. |
| H11 | 🟡 IMPORTANTE | 4. Relaciones | 125 relaciones duplicadas (mismo persona_id + persona_relacionada_id + tipo). | `02_leer_data.R` | Agregar `distinct()` final en relaciones. |
| H12 | 🟡 IMPORTANTE | 4. Relaciones | 44 auto-referencias (persona_id == persona_relacionada_id). | `02_leer_data.R` | Filtrar `persona_id != persona_relacionada_id`. |
| H13 | 🟢 MENOR | 4. Relaciones | `perfiles_relacionados_hermanos` y `perfiles_relacionados_familia` tienen 0% datos (0/6,731). | scraper | No se extraen. Verificar si el scraper los captura o si el campo simplemente no existe en Wikipedia. |
| H14 | 🟡 IMPORTANTE | 4. Relaciones | Many-to-many warning en `left_join(wiki_slug)`. Un slug mapea a múltiples `persona_id` (por duplicados del H01). | `02_leer_data.R` línea 323 | Deduplicar consolidado primero (H01). |
| H15 | 🟡 IMPORTANTE | 5. Infobox | `infobox_json` existe (6,724 filas con datos) pero NO se usa. Solo se usa `infobox_completa` (texto plano) con regex frágil. | `02_leer_data.R` | Usar `infobox_json` (JSON parseado) en vez de regex sobre texto plano. Más robusto. |
| H16 | 🟡 IMPORTANTE | 5. Infobox | 32 partidos con nombre normalizado duplicado (ej: "Liberal" aparece como 3 variantes). "Partido Liberal" no se distingue por país. | `02_leer_data.R` → `extraer_partidos()` | Normalizar partidos: `str_remove("^Partido\\s+")`. Para homónimos, concatenar con país. |
| H17 | 🟢 MENOR | 5. Infobox | Sucesiones: solo 22.3% de matching (2,848 de 12,793). | `02_leer_data.R` | Muchos predecesores/sucesores no están en el dataset. Esperable dado que el dataset son solo élites familiares. |
| H18 | 🔴 CRÍTICO | 6. Grafo | 287,439 aristas, la mayoría de tipo "misma_familia". Una familia con 100 miembros genera 4,950 aristas. Las 10 mayores familias (>100 miembros) dominan el grafo. | `03_grafo_parentesco.R` | Eliminar aristas "misma_familia" del grafo, o ponderarlas mucho más bajo. Distorsionan todas las métricas. |
| H19 | 🟡 IMPORTANTE | 6. Grafo | Aristas del grafo guardado no tienen columna `categoria` (el grafo .rds fue generado sin ese atributo en las aristas). | `03_grafo_parentesco.R` | Regenerar grafo para que las aristas contengan `tipo` y `categoria`. |
| H20 | 🔴 CRÍTICO | 7. Endogamia | Paraguay: 20 total, 20 endogámicos, 10 transnacionales. Las categorías NO son mutuamente excluyentes (endogámico + transnacional posible). | `04_endogamia_matrimonial.R` línea 53 | Revisar la lógica de clasificación. `fam1 == fam2` se evalúa primero → siempre "endogámico", nunca llega a "transnacional" incluso si cruza países. |
| H21 | 🔴 CRÍTICO | 7. Endogamia | Moctezuma-Pizarro = 21 "matrimonios". García_de_Zúñiga-Warnes = 21. Estos son cross-joins: si hay 6 personas Warnes y 6 García_de_Zúñiga con relaciones cruzadas, `count(from, to)` genera n×m combinaciones. | `04_endogamia_matrimonial.R` | El bug está en cómo se cuentan matrimonios: son relaciones (A cónyuge de B + B cónyuge de A = 2), y personas duplicadas amplifican el efecto. Deduplicar uniones: `min(persona_id, persona_relacionada_id)` para contar una vez. |
| H22 | 🟡 IMPORTANTE | 7. Endogamia | 1,743 uniones "endogámicas" = misma `familia_norm`. Pero `familia_norm` agrupa por apellido, no por parentesco real. Dos personas llamadas "Errázuriz" de ramas distintas cuentan como endogámico. | `04_endogamia_matrimonial.R` | Documentar esta limitación. Endogamia basada en `familia_norm` es un proxy, no una medida exacta. |
| H23 | 🟡 IMPORTANTE | 8. Dinastías | Familia Hurtado: rango 1,235 años (1825–3060). Año 3060 es de H03. | `05_dinastias_temporales.R` | Mismo fix que H03: filtrar años > 2026 antes de calcular rango. |
| H24 | 🟡 IMPORTANTE | 8. Dinastías | López Carrillo = 28 sucesiones internas. Puede ser inflado por personas duplicadas y múltiples cargos. | `05_dinastias_temporales.R` | Verificar con datos: ¿son sucesiones únicas o repetidas por persona duplicada? |
| H25 | 🟢 MENOR | 9. Transnacional | Señales biográficas: 699 de `lugar_nacimiento_parseado`. Muchas son ruido colonial ("Virreinato del Perú" en Argentina). | `06_conexiones_transnacionales.R` | Filtrar entidades coloniales del detector de países en texto. |
| H26 | 🔴 CRÍTICO | 10. Élites | `elite_familiar = 100%` (6,170 de 6,171). Toda persona es "élite familiar" por definición, la variable no discrimina. | `reporting/red_elites_motivos.R` | Redefinir: "élite familiar" debería significar persona de familia con >N miembros o >N conexiones, no simplemente estar en el dataset. |
| H27 | 🔴 CRÍTICO | 10. Élites | Solo 4 conexiones "transnacionales" en clasificación de élites vs ~1,400 en análisis transnacional. Bug en la clasificación de motivos. | `reporting/red_elites_motivos.R` | El clasificador solo cuenta tipo "transnacional" explícito, no relaciones cruzadas entre países que están clasificadas como "parentesco". Integrar lógica de país. |
| H28 | 🟡 IMPORTANTE | 11. Paper | T03 solo tiene 5 países (de 10). | `reporting/setup_paper_folder.R` | Filtro de ≥N matrimonios probablemente excluye 5 países. Incluir todos con nota. |
| H29 | 🟢 MENOR | 12. Consistencia | IDs consistentes entre tablas (0 huérfanos). ✓ | — | — |
| H30 | 🟢 MENOR | 12. Figuras | `grafo_componente_principal.png` no existe. Probablemente el subgrafo filtrado tenía >2,000 nodos. | `03_grafo_parentesco.R` línea 182 | Aumentar threshold o samplear nodos para la visualización. |

---

## Detalle por Sección

### 1. Lectura del Consolidado

**Datos:** 6,731 filas × 42 columnas. Delimitador `;` → se lee correctamente.

**URLs duplicadas (H01):** 466 URLs aparecen más de una vez. Las peores tienen 4 duplicados (Atala Sarmiento, varios Ponce de León, Bolívar). Esto multiplica las personas en el dataset y sus relaciones.

```
Atala_Sarmiento                          4
Carlos_Horacio_Ponce_de_León             4
Diego_de_Alvear_y_Ponce_de_León          4
Facundo_Ponce_de_León                    4
Feliciano_Palacios_y_Sojo               4
```

**Impacto:** Una persona con 4 duplicados genera 4× sus relaciones, 4× aristas en el grafo, y aparece en 4 familias distintas (inflando alianzas).

**Filas sin nombre (H02):** 2 filas. Probablemente errores de scraping.

### 2. Parsing de Fechas

**Tasas:**
- Parseadas exitosamente: 6,032 / 6,731 (89.6%)
- Fallidas: 699 (10.4%)

**Años imposibles (H03, H04):**

| persona_id | nombre | familia | año | problema |
|---|---|---|---|---|
| 3453 | Andrés de Santa Cruz | Hurtado | 3060 | "96 de diciembre de 3060" — error de scraping |
| 3669 | Pero Niño | Niño | 1378 | Persona real del siglo XIV. No es error. |

**Formatos no reconocidos (H05):** Los más comunes:
- `"Abril de 1762 Bogotá"` — falta regex para "Mes de YYYY"
- `"Buenos Aires , 28 de junio de 1928"` — ciudad antes de fecha
- `"ca 1803"` / `"circa 1795"` — formato aproximado
- `"Siglo XVII"` / `"Siglo XIX"` — período sin año

**Causa de muerte mal clasificada (H06):** `"Siglo XIX"` en `fecha_fallecimiento` → `causa_muerte = "Siglo XIX"`. No es una causa de muerte, es un período.

### 3. Asignación de País (BUG CRÍTICO)

**El problema fundamental (H07):** En `02_leer_data.R`, la línea 280 hace:
```r
pais_base = coalesce(pais, pais_origen, pais_inferido)
```

`pais` y `pais_origen` vienen del CSV (la familia por la que se scrapeó). Si la familia López Carrillo entró por Argentina, TODAS las personas de esa familia (incluyendo Carlos Antonio López, presidente de Paraguay) quedan como `pais_base = "argentina"`.

`pais_inferido` usa `inferir_pais()` que busca texto en `lugar_nacimiento` + `infobox_completa`. Pero como `pais` siempre tiene valor, `coalesce()` nunca llega a `pais_inferido`.

**En cambio, `01_exploracion.R` usa:**
```r
pais_persona = coalesce(pais_override, pais_persona_inferido, pais)
```
Esto es mejor (inferido antes que CSV), pero solo aplica dentro de `01_exploracion.R`. Los scripts 02–07 usan `pais_base` de `02_leer_data.R` que tiene el bug.

**Entidades coloniales (H08):** 601 personas con "Virreinato del Perú", "Virreinato del Río de la Plata", etc. en `lugar_nacimiento`. El regex `"virreinato del perú"` → `"peru"` asigna Perú a personas nacidas en Argentina colonial.

Ejemplo: Cornelio Saavedra, nacido en "Otuyo, corregimiento de Potosí, Virreinato del Perú" → `pais_inferido = "peru"`, pero su país real es Bolivia/Argentina.

**Overrides manuales (H09):** Solo 4 entradas. La tabla de `pais_inconsistente.csv` tiene 2,602 candidatos.

### 4. Relaciones de Parentesco

| Métrica | Valor |
|---|---|
| Total relaciones | 18,248 |
| Con match (persona_relacionada_id) | 11,493 (63%) |
| Sin match | 6,755 (37%) |
| Duplicadas | 125 |
| Auto-referencias | 44 |

**Tasa de matching:** 63% es razonable. El 37% sin match son personas mencionadas en relaciones pero que no están en el dataset (ej: cónyuge que no es de familia de élite).

**Campos con 0% datos (H13):**
- `perfiles_relacionados_hermanos`: 0/6,731
- `perfiles_relacionados_familia`: 0/6,731

Estos campos nunca se llenan en el scraping.

**Many-to-many (H14):** El join por `wiki_slug` genera duplicados porque las personas duplicadas (H01) comparten el mismo slug.

### 5. Extracción del Infobox

| Tabla | Registros | Personas | Distintos |
|---|---|---|---|
| Partidos | 3,093 | 2,271 | 466 |
| Educación | 3,443 | 2,476 | 703 |
| Sucesiones | 12,793 | — | — (22.3% match) |

**infobox_json vs infobox_completa (H15):** 6,724 personas tienen `infobox_json` (JSON parseado por el scraper), pero `02_leer_data.R` solo usa `infobox_completa` (texto plano) con regex como `"Partido político:\\s*(.+?)(?:\\s*\\||\\s*$)"`. El JSON ya tiene los campos separados.

**Partidos duplicados (H16):** 32 variantes normalizadas se repiten. Ejemplo: "Liberal" aparece en Chile, Colombia, México, Argentina, Perú sin distinguir.

### 6. Grafo

| Métrica | Valor |
|---|---|
| Nodos | 6,731 |
| Aristas | 287,439 |
| Nodos aislados | 16 (0.2%) |
| Componentes | 83 |
| Componente mayor | 6,359 (94.5%) |

**Explosión combinatoria (H18):** Las 10 familias más grandes:

| Familia | Miembros | Aristas potenciales "misma_familia" |
|---|---|---|
| bolívar | 276 | 37,950 |
| ponce_de_león | 253 | 31,878 |
| garcía_de_zúñiga | 177 | 15,576 |
| mosquera | 173 | 14,878 |
| prieto | 157 | 12,246 |
| warnes | 152 | 11,476 |
| errázuriz | 130 | 8,385 |
| lópez_carrillo | 119 | 7,021 |
| urquiza | 116 | 6,670 |
| caro | 107 | 5,671 |

**Total solo estas 10: ~151,751 aristas.** Estas aristas dominan el grafo y distorsionan betweenness, pagerank y comunidades Louvain.

### 7. Endogamia

**Bug de conteo (H20):** En `04_endogamia_matrimonial.R` línea 53:
```r
tipo_union = case_when(
  fam1 == fam2 ~ "endogámico (misma familia)",
  pais1 != pais2 ~ "exogámico transnacional",
  TRUE ~ "exogámico (distinta familia, mismo país)"
)
```
Si `fam1 == fam2` Y `pais1 != pais2`, se clasifica solo como "endogámico". Paraguay tiene 20 uniones endogámicas y 10 transnacionales de 20 totales — 10 son ambas cosas pero se contaron en ambas columnas (en `endogamia_pais`, `transnacionales` se calcula independientemente con `sum(pais1 != pais2)`).

**Inflación de alianzas (H21):**
| Par | "Matrimonios" | Explicación probable |
|---|---|---|
| Moctezuma-Pizarro | 21 | Personas duplicadas + conteo bidireccional |
| García_de_Zúñiga-Warnes | 21 | 177 miembros × 152 miembros en esas familias |
| Prieto-Warnes | 20 | Same issue |

### 8. Dinastías

- Familia Hurtado: rango 1,235 años → causado por H03 (año 3060).
- López Carrillo: 28 sucesiones internas → posiblemente inflado por personas duplicadas.

### 9. Conexiones Transnacionales

Red entre países operativa. Top conexiones:
- Argentina ↔ Perú: 413 (214 parentesco + 199 biográfico)
- Colombia ↔ Venezuela: 399
- Colombia ↔ Ecuador: 376
- Argentina ↔ Uruguay: 358

**Ruido colonial (H25):** 699 señales de `lugar_nacimiento_parseado` incluyen referencias a virreinatos que no indican conexión transnacional real.

### 10. Clasificación de Élites

| Tipo | N | % |
|---|---|---|
| elite_familiar | 6,170 | 100.0% |
| elite_economica | 595 | 9.6% |
| elite_politica | 370 | 6.0% |
| elite_diplomatica | 76 | 1.2% |

**`elite_familiar = 100%` (H26):** Inútil como variable discriminante.

**Solo 4 transnacionales (H27):** El clasificador de motivos en `red_elites_motivos.R` marca "transnacional" solo si el campo `tipo` lo dice explícitamente. Las ~1,400 relaciones de parentesco que cruzan países no se detectan como transnacionales.

### 11. Tablas para Paper

Todas las 7 tablas existen. T03 solo tiene 5 países (filtro de ≥N).

### 12. Consistencia

- **IDs:** 0 huérfanos entre tablas. ✓
- **Figuras:** 14/15 esperadas existen. Falta: `grafo_componente_principal.png` (subgrafo > 2,000 nodos).

---

## Priorización de Correcciones

### Fase 1: Críticos (afectan resultados del paper)

1. **Deduplicar consolidado por URL** (H01) — afecta todo downstream
2. **Corregir cascada de país** (H07) — `pais_base = coalesce(pais_inferido, pais, pais_origen)` o mejor aún, unificar con `01_exploracion.R`
3. **Filtrar años > 2026** (H03) — afecta dinastías
4. **Eliminar o ponderar aristas "misma_familia"** (H18) — afecta grafo y métricas
5. **Deduplicar matrimonios** (H21) — afecta endogamia
6. **Arreglar clasificación transnacional en élites** (H27)
7. **Redefinir elite_familiar** (H26)

### Fase 2: Importantes (mejoran calidad)

8. Mejorar parsing de fechas (H05, H06)
9. Usar `infobox_json` en vez de regex sobre texto plano (H15)
10. Normalizar partidos (H16)
11. Corregir inferencia de país con entidades coloniales (H08)
12. Ampliar overrides manuales de país (H09)
13. Eliminar relaciones duplicadas y auto-referencias (H11, H12)

### Fase 3: Menores (cosmético)

14. Filtrar filas sin nombre (H02)
15. Generar `grafo_componente_principal.png` con threshold más alto (H30)

---

## Tablas Auxiliares Generadas

```
diagnostico/tablas/
├── urls_duplicadas.csv              # H01: 466 URLs con duplicados
├── anos_fuera_rango.csv             # H03: años nacimiento > 2026 o < 1400
├── anos_fallecimiento_fuera_rango.csv  # (vacío — no hay)
├── fechas_no_parseadas.csv          # H05: 30 ejemplos de fechas que fallan
├── pais_inconsistente.csv           # H07: 2,602 personas con pais_base ≠ pais_inferido
├── entidades_coloniales_mal_asignadas.csv  # H08: 601 personas con virreinatos
└── relaciones_duplicadas.csv        # H11: 125 relaciones duplicadas
```
