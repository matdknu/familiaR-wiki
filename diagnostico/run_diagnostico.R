#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(purrr)
})

dir.create("diagnostico/tablas", recursive = TRUE, showWarnings = FALSE)
cat("═══ DIAGNÓSTICO COMPLETO DEL PIPELINE ═══\n\n")

# ═══════════════════════════════════════════════════════════════════════════════
# 1. LECTURA DEL CONSOLIDADO
# ═══════════════════════════════════════════════════════════════════════════════
cat("══ 1. LECTURA DEL CONSOLIDADO ══\n")
ruta <- "data/processed/familias/_CONSOLIDADO_familias_latam.csv"
raw <- read_delim(ruta, delim = ";", show_col_types = FALSE, escape_double = FALSE, trim_ws = TRUE)
cat("Filas:", nrow(raw), "\n")
cat("Columnas:", ncol(raw), "\n")
cat("Nombres columnas:", paste(names(raw), collapse = ", "), "\n")

# Duplicados por URL
dup_url <- raw %>% filter(!is.na(url)) %>% count(url, sort = TRUE) %>% filter(n > 1)
cat("URLs duplicadas:", nrow(dup_url), "\n")
if (nrow(dup_url) > 0) {
  cat("Top 10 duplicadas:\n")
  print(head(dup_url, 10))
  write_csv(dup_url, "diagnostico/tablas/urls_duplicadas.csv")
}

# Nombre vacío
sin_nombre <- raw %>% filter(is.na(nombre) | trimws(nombre) == "")
cat("Filas sin nombre:", nrow(sin_nombre), "\n")

# ═══════════════════════════════════════════════════════════════════════════════
# 2. PARSING DE FECHAS
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n══ 2. PARSING DE FECHAS ══\n")

source("scripts/02_processing/02_leer_data.R")
datos <- leer_data(guardar = FALSE)
personas <- datos$personas

cat("Personas:", nrow(personas), "\n")

# Años de nacimiento
cat("\nanio_nacimiento parseados:", sum(!is.na(personas$anio_nacimiento)), "de", nrow(personas), "\n")
cat("anio_nacimiento NA:", sum(is.na(personas$anio_nacimiento)), "\n")

# Años imposibles
anos_imposibles <- personas %>%
  filter(!is.na(anio_nacimiento)) %>%
  filter(anio_nacimiento < 1400 | anio_nacimiento > 2026) %>%
  select(persona_id, nombre, familia, pais_base, anio_nacimiento, fecha_nacimiento)
cat("Años de nacimiento fuera de rango (<1400 o >2026):", nrow(anos_imposibles), "\n")
if (nrow(anos_imposibles) > 0) {
  print(anos_imposibles)
  write_csv(anos_imposibles, "diagnostico/tablas/anos_fuera_rango.csv")
}

# Años de fallecimiento imposibles
anos_fall_imposibles <- personas %>%
  filter(!is.na(anio_fallecimiento)) %>%
  filter(anio_fallecimiento < 1400 | anio_fallecimiento > 2026) %>%
  select(persona_id, nombre, familia, pais_base, anio_fallecimiento, fecha_fallecimiento)
cat("Años de fallecimiento fuera de rango:", nrow(anos_fall_imposibles), "\n")
if (nrow(anos_fall_imposibles) > 0) {
  print(head(anos_fall_imposibles, 20))
  write_csv(anos_fall_imposibles, "diagnostico/tablas/anos_fallecimiento_fuera_rango.csv")
}

# Fechas que no parsearon (tienen texto pero no año)
no_parsearon <- personas %>%
  filter(!is.na(fecha_nacimiento), is.na(anio_nacimiento)) %>%
  select(persona_id, nombre, fecha_nacimiento) %>%
  head(30)
cat("\nFechas de nacimiento no parseadas (top 30):\n")
print(no_parsearon, n = 30)
write_csv(no_parsearon, "diagnostico/tablas/fechas_no_parseadas.csv")

# Causa de muerte en fecha
causa_en_fecha <- personas %>%
  filter(!is.na(causa_muerte)) %>%
  select(persona_id, nombre, fecha_fallecimiento, causa_muerte) %>%
  head(20)
cat("\nCausa de muerte detectada (top 20):\n")
print(causa_en_fecha, n = 20)

# ═══════════════════════════════════════════════════════════════════════════════
# 3. ASIGNACIÓN DE PAÍS
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n══ 3. ASIGNACIÓN DE PAÍS ══\n")

cat("Distribución pais_base:\n")
personas %>% count(pais_base, sort = TRUE) %>% print(n = 20)

cat("\npais vs pais_origen:\n")
diff_pais <- personas %>%
  filter(!is.na(pais), !is.na(pais_origen), pais != pais_origen) %>%
  select(persona_id, nombre, pais, pais_origen, pais_inferido, pais_base)
cat("pais != pais_origen:", nrow(diff_pais), "\n")
if (nrow(diff_pais) > 0) print(head(diff_pais, 20))

cat("\npais_base vs pais_inferido:\n")
diff_inf <- personas %>%
  filter(!is.na(pais_base), !is.na(pais_inferido), pais_base != pais_inferido) %>%
  select(persona_id, nombre, pais, pais_origen, pais_inferido, pais_base, lugar_nacimiento_parseado)
cat("pais_base != pais_inferido:", nrow(diff_inf), "\n")
write_csv(diff_inf, "diagnostico/tablas/pais_inconsistente.csv")
cat("Top 30 sospechosos:\n")
print(head(diff_inf, 30), n = 30)

# Virreinato del Perú → ¿personas de Argentina?
virreinato <- personas %>%
  filter(str_detect(tolower(coalesce(lugar_nacimiento_parseado, "")), "virreinato|nueva granada|r.o de la plata")) %>%
  select(persona_id, nombre, pais_base, pais_inferido, lugar_nacimiento_parseado)
cat("\nCasos con entidades coloniales en lugar_nacimiento:\n")
print(virreinato, n = 40)
write_csv(virreinato, "diagnostico/tablas/entidades_coloniales_mal_asignadas.csv")

# URL override manual
url_extra_path <- "data/manual/url_pais_extra.csv"
if (file.exists(url_extra_path)) {
  overrides <- read_csv(url_extra_path, show_col_types = FALSE)
  cat("\nOverrides manuales de país:", nrow(overrides), "filas\n")
  print(overrides)
} else {
  cat("\nArchivo url_pais_extra.csv NO ENCONTRADO\n")
}

# ═══════════════════════════════════════════════════════════════════════════════
# 4. RELACIONES DE PARENTESCO
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n══ 4. RELACIONES DE PARENTESCO ══\n")
relaciones <- datos$relaciones

cat("Total relaciones:", nrow(relaciones), "\n")
cat("Con persona_relacionada_id:", sum(!is.na(relaciones$persona_relacionada_id)), "\n")
cat("Sin persona_relacionada_id (NA):", sum(is.na(relaciones$persona_relacionada_id)), "\n")
tasa_match <- round(mean(!is.na(relaciones$persona_relacionada_id)) * 100, 1)
cat("Tasa de matching:", tasa_match, "%\n")

cat("\nRelaciones por tipo:\n")
relaciones %>% count(tipo_relacion, sort = TRUE) %>% print()

# Duplicados
dup_rel <- relaciones %>%
  filter(!is.na(persona_relacionada_id)) %>%
  count(persona_id, persona_relacionada_id, tipo_relacion) %>%
  filter(n > 1)
cat("\nRelaciones duplicadas (misma persona_id + persona_relacionada_id + tipo):", nrow(dup_rel), "\n")
if (nrow(dup_rel) > 0) {
  write_csv(dup_rel, "diagnostico/tablas/relaciones_duplicadas.csv")
  print(head(dup_rel, 20))
}

# Auto-referencia
autoref <- relaciones %>% filter(!is.na(persona_relacionada_id), persona_id == persona_relacionada_id)
cat("Auto-referencias (persona_id == persona_relacionada_id):", nrow(autoref), "\n")

# Campos perfiles_relacionados con datos
for (campo in c("perfiles_relacionados_padres", "perfiles_relacionados_conyuge",
                 "perfiles_relacionados_pareja", "perfiles_relacionados_hijos",
                 "perfiles_relacionados_hermanos", "perfiles_relacionados_familia")) {
  if (campo %in% names(raw)) {
    n_data <- sum(!is.na(raw[[campo]]) & trimws(raw[[campo]]) != "")
    cat(sprintf("  %s: %d/%d con datos (%.1f%%)\n", campo, n_data, nrow(raw), 100*n_data/nrow(raw)))
  } else {
    cat(sprintf("  %s: columna NO existe\n", campo))
  }
}

# ═══════════════════════════════════════════════════════════════════════════════
# 5. EXTRACCIÓN DEL INFOBOX
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n══ 5. EXTRACCIÓN DEL INFOBOX ══\n")

cat("Personas con infobox_completa:", sum(!is.na(personas$infobox_completa)), "\n")
cat("Personas con infobox_json (si existe):", 
    if ("infobox_json" %in% names(personas)) sum(!is.na(personas$infobox_json)) else "columna no presente", "\n")

# Partidos
partidos <- datos$partidos
cat("\nPartidos extraídos:", nrow(partidos), "\n")
cat("Personas con partido:", n_distinct(partidos$persona_id), "\n")
cat("Partidos distintos:", n_distinct(partidos$partido), "\n")

# Partidos sospechosos de duplicado
partidos_similares <- partidos %>%
  distinct(partido) %>%
  mutate(partido_lower = tolower(str_remove(partido, "^Partido\\s+"))) %>%
  count(partido_lower, sort = TRUE) %>%
  filter(n > 1)
cat("Partidos con nombre normalizado duplicado:", nrow(partidos_similares), "\n")
if (nrow(partidos_similares) > 0) print(head(partidos_similares, 20))

# Partido Liberal por país
pl <- partidos %>%
  filter(str_detect(tolower(partido), "liberal")) %>%
  left_join(personas %>% select(persona_id, pais_base), by = "persona_id") %>%
  count(partido, pais_base)
cat("\n'Partido Liberal' por país:\n")
print(pl)

# Educación
educacion <- datos$educacion
cat("\nEducación extraída:", nrow(educacion), "\n")
cat("Personas con educación:", n_distinct(educacion$persona_id), "\n")
cat("Instituciones distintas:", n_distinct(educacion$institucion), "\n")

# Sucesiones
sucesiones <- datos$sucesiones
cat("\nSucesiones extraídas:", nrow(sucesiones), "\n")
cat("Con persona_relacionada_id:", sum(!is.na(sucesiones$persona_relacionada_id)), "\n")
cat("Sin persona_relacionada_id:", sum(is.na(sucesiones$persona_relacionada_id)), "\n")
tasa_suc <- if (nrow(sucesiones) > 0) round(mean(!is.na(sucesiones$persona_relacionada_id)) * 100, 1) else 0
cat("Tasa matching sucesiones:", tasa_suc, "%\n")

# ═══════════════════════════════════════════════════════════════════════════════
# 6. GRAFO
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n══ 6. GRAFO ══\n")
grafo_path <- "data/processed/02_leer_data/grafo.rds"
if (file.exists(grafo_path)) {
  library(igraph)
  library(tidygraph)
  grafo <- read_rds(grafo_path)
  cat("Nodos:", vcount(grafo), "\n")
  cat("Aristas:", ecount(grafo), "\n")
  cat("Nodos aislados (grado 0):", sum(degree(grafo) == 0), "\n")
  cat("% nodos aislados:", round(100*sum(degree(grafo)==0)/vcount(grafo), 1), "%\n")
  cat("Componentes:", components(grafo)$no, "\n")
  comp_sizes <- sort(components(grafo)$csize, decreasing = TRUE)
  cat("Tamaño componente mayor:", comp_sizes[1], "(", round(100*comp_sizes[1]/vcount(grafo),1), "%)\n")
  cat("Dirigido:", is.directed(grafo), "\n")
  
  edge_df <- grafo %>% activate(edges) %>% as_tibble()
  cat("Columnas en aristas:", paste(names(edge_df), collapse = ", "), "\n")
  if ("categoria" %in% names(edge_df)) {
    edge_types <- edge_df %>% count(categoria)
    cat("\nAristas por categoría:\n")
    print(edge_types)
    fam_edges <- edge_types %>% filter(categoria == "familia") %>% pull(n)
    par_edges <- edge_types %>% filter(categoria == "parentesco") %>% pull(n)
  } else {
    cat("Columna 'categoria' no encontrada en aristas del grafo guardado.\n")
    cat("(El grafo fue construido antes de agregar la columna 'categoria'?)\n")
    fam_edges <- 0; par_edges <- 0
  }
  cat("\nAristas 'misma_familia':", if (length(fam_edges)>0) fam_edges else 0, "\n")
  cat("Aristas 'parentesco':", if (length(par_edges)>0) par_edges else 0, "\n")
  
  # Familias grandes → aristas
  fam_sizes <- personas %>% filter(!is.na(familia_norm)) %>% count(familia_norm, sort = TRUE)
  cat("\nTop 10 familias por tamaño (generan n*(n-1)/2 aristas 'misma_familia'):\n")
  fam_sizes %>% head(10) %>% mutate(aristas_potenciales = n*(n-1)/2) %>% print()
} else {
  cat("Grafo NO ENCONTRADO en", grafo_path, "\n")
  cat("Ejecutar 03_grafo_parentesco.R primero\n")
}

# ═══════════════════════════════════════════════════════════════════════════════
# 7. ENDOGAMIA
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n══ 7. ENDOGAMIA ══\n")
endo_path <- "outputs/tables/endogamia_por_pais.csv"
if (file.exists(endo_path)) {
  endo <- read_csv(endo_path, show_col_types = FALSE)
  cat("Endogamia por país:\n")
  print(endo)
  
  # Paraguay: ¿números que no suman?
  py <- endo %>% filter(grepl("paraguay", pais1, ignore.case = TRUE))
  if (nrow(py) > 0) {
    cat("\nParaguay detalle:\n")
    cat("  total:", py$total, "endogámicos:", py$endogamicos, "transnacionales:", py$transnacionales, "\n")
    if (py$endogamicos + py$transnacionales > py$total) {
      cat("  ⚠️ endogámicos + transnacionales > total. Las categorías NO son mutuamente excluyentes.\n")
    }
  }
  
  # Alianzas sospechosas
  alianzas_path <- "outputs/tables/endogamia_alianzas_interfamilia.csv"
  if (file.exists(alianzas_path)) {
    alianzas <- read_csv(alianzas_path, show_col_types = FALSE)
    cat("\nTop 10 alianzas interfamilia:\n")
    print(head(alianzas, 10))
    sospechosas <- alianzas %>% filter(matrimonios >= 10)
    if (nrow(sospechosas) > 0) {
      cat("\n⚠️ Alianzas con 10+ matrimonios (posible cross-join):\n")
      print(sospechosas)
    }
  }
  
  # Duplicados en matrimonios
  resumen_tipos <- read_csv("outputs/tables/endogamia_resumen_tipos.csv", show_col_types = FALSE)
  cat("\nResumen tipos de unión:\n")
  print(resumen_tipos)
} else {
  cat("Archivo endogamia_por_pais.csv NO ENCONTRADO. Ejecutar 04_endogamia_matrimonial.R\n")
}

# ═══════════════════════════════════════════════════════════════════════════════
# 8. DINASTÍAS
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n══ 8. DINASTÍAS ══\n")
dinas_path <- "outputs/tables/dinastias_persistencia.csv"
if (file.exists(dinas_path)) {
  dinas <- read_csv(dinas_path, show_col_types = FALSE)
  
  # Años imposibles en dinastías
  dinas_imp <- dinas %>%
    filter(primer_miembro < 1400 | ultimo_miembro > 2026)
  cat("Familias con años imposibles en rango temporal:", nrow(dinas_imp), "\n")
  if (nrow(dinas_imp) > 0) print(dinas_imp)
  
  # Sucesiones
  suc_path <- "outputs/tables/dinastias_sucesion_interna.csv"
  if (file.exists(suc_path)) {
    suc <- read_csv(suc_path, show_col_types = FALSE)
    cat("\nTop 10 familias con sucesiones internas:\n")
    print(head(suc, 10))
    
    suc_alta <- suc %>% filter(n_sucesiones >= 20)
    if (nrow(suc_alta) > 0) {
      cat("\n⚠️ Familias con 20+ sucesiones (revisar duplicados):\n")
      print(suc_alta)
    }
  }
} else {
  cat("Archivo dinastias_persistencia.csv NO ENCONTRADO\n")
}

# ═══════════════════════════════════════════════════════════════════════════════
# 9. CONEXIONES TRANSNACIONALES
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n══ 9. CONEXIONES TRANSNACIONALES ══\n")
trans_path <- "outputs/tables/transnacional_red_paises.csv"
if (file.exists(trans_path)) {
  trans <- read_csv(trans_path, show_col_types = FALSE)
  cat("Red entre países:\n")
  print(trans)
  
  # Simetría
  for (i in seq_len(nrow(trans))) {
    reverso <- trans %>% filter(from == trans$to[i], to == trans$from[i])
    if (nrow(reverso) > 0) {
      cat(sprintf("⚠️ Par duplicado: %s↔%s aparece en ambas direcciones\n", trans$from[i], trans$to[i]))
    }
  }
}

# Señales biográficas
sen_path <- "outputs/tables/transnacional_senales_biograficas.csv"
if (file.exists(sen_path)) {
  senales <- read_csv(sen_path, show_col_types = FALSE)
  cat("\nSeñales biográficas:", nrow(senales), "\n")
  cat("Por campo:\n")
  senales %>% count(campo, sort = TRUE) %>% print()
}

# ═══════════════════════════════════════════════════════════════════════════════
# 10. CLASIFICACIÓN DE ÉLITES
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n══ 10. CLASIFICACIÓN DE ÉLITES ══\n")
elites_path <- "outputs/tables/red_elites_nodos.csv"
if (file.exists(elites_path)) {
  elites <- read_csv(elites_path, show_col_types = FALSE)
  cat("Nodos de élites:", nrow(elites), "\n")
  
  for (col in c("elite_politica", "elite_familiar", "elite_economica", "elite_diplomatica")) {
    if (col %in% names(elites)) {
      n_true <- sum(elites[[col]] == TRUE | elites[[col]] == 1, na.rm = TRUE)
      cat(sprintf("  %s: %d (%.1f%%)\n", col, n_true, 100*n_true/nrow(elites)))
    }
  }
  
  motivos_path <- "outputs/tables/red_elites_motivos_resumen.csv"
  if (file.exists(motivos_path)) {
    motivos <- read_csv(motivos_path, show_col_types = FALSE)
    cat("\nMotivos de conexión entre élites:\n")
    print(motivos)
  }
} else {
  cat("Archivo red_elites_nodos.csv NO ENCONTRADO\n")
}

# ═══════════════════════════════════════════════════════════════════════════════
# 11. TABLAS PARA PAPER
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n══ 11. TABLAS PARA PAPER ══\n")
paper_tables <- c(
  "paper/tables/T01_h1_h4_familias_pais.csv",
  "paper/tables/T02_h1_h4_resumen_pais.csv",
  "paper/tables/T03_endogamia_por_pais.csv",
  "paper/tables/T04_conexiones_entre_paises.csv",
  "paper/tables/T05_elites_por_pais.csv",
  "paper/tables/T06_json_limpieza_resumen.csv",
  "paper/tables/T07_metricas_red_por_pais.csv"
)
for (p in paper_tables) {
  if (file.exists(p)) {
    df <- read_csv(p, show_col_types = FALSE)
    cat(sprintf("  ✓ %s — %d filas × %d cols\n", basename(p), nrow(df), ncol(df)))
  } else {
    cat(sprintf("  ✗ %s — NO ENCONTRADO\n", basename(p)))
  }
}

# Diccionario de variables
dict_path <- "paper/manifests/paper_variable_dictionary.csv"
if (file.exists(dict_path)) {
  dict <- read_csv(dict_path, show_col_types = FALSE)
  cat("\nDiccionario de variables:", nrow(dict), "entradas\n")
}

# ═══════════════════════════════════════════════════════════════════════════════
# 12. CONSISTENCIA
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n══ 12. CONSISTENCIA ══\n")

# IDs consistentes
rel_ids <- unique(c(relaciones$persona_id, relaciones$persona_relacionada_id[!is.na(relaciones$persona_relacionada_id)]))
persona_ids <- personas$persona_id
huerfanos_en_relaciones <- setdiff(rel_ids, persona_ids)
cat("IDs en relaciones que no están en personas:", length(huerfanos_en_relaciones), "\n")

part_ids <- unique(partidos$persona_id)
huerfanos_partidos <- setdiff(part_ids, persona_ids)
cat("IDs en partidos que no están en personas:", length(huerfanos_partidos), "\n")

edu_ids <- unique(educacion$persona_id)
huerfanos_edu <- setdiff(edu_ids, persona_ids)
cat("IDs en educación que no están en personas:", length(huerfanos_edu), "\n")

# Figuras esperadas
figuras_esperadas <- c(
  "outputs/figures/descriptivo_panel_resumen.png",
  "outputs/figures/descriptivo_personas_por_pais.png",
  "outputs/figures/grafo_top_grado.png",
  "outputs/figures/grafo_componente_principal.png",
  "outputs/figures/endogamia_tasas_por_pais.png",
  "outputs/figures/dinastias_rango_temporal.png",
  "outputs/figures/transnacional_red_paises.png",
  "outputs/figures/instituciones_top_universidades.png",
  "outputs/figures/exploracion_redes_facet.png",
  "outputs/figures/red_elites_motivos_top.png",
  "outputs/figures/red_paises_elites_motivos.png",
  "paper/figures/F01_exploracion_redes_facet.png",
  "paper/figures/F02_union_entre_paises.png",
  "paper/figures/F03_h1_cierre_vs_concentracion.png",
  "paper/figures/F04_h4_centralidad_vs_cierre.png"
)

cat("\nFiguras esperadas:\n")
for (f in figuras_esperadas) {
  status <- if (file.exists(f)) "✓" else "✗"
  cat(sprintf("  %s %s\n", status, f))
}

cat("\n═══ DIAGNÓSTICO COMPLETADO ═══\n")
