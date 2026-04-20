#!/usr/bin/env Rscript
# ============================================================================
# 03_grafo_parentesco.R
# Construye el grafo de parentesco desde las tablas normalizadas.
# Calcula métricas de centralidad y detecta comunidades.
# Lee: data/processed/02_leer_data/*.rds
# Escribe: data/processed/02_leer_data/grafo.rds
#          outputs/tables/metricas_centralidad.csv
#          outputs/figures/grafo_*.png
# ============================================================================

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(igraph)
library(tidygraph)
library(ggraph)
library(ggplot2)
library(ggrepel)
library(forcats)

DATA_DIR  <- "data/processed/02_leer_data"
OUT_FIG   <- "outputs/figures"
OUT_TABLE <- "outputs/tables"
dir.create(OUT_FIG, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_TABLE, recursive = TRUE, showWarnings = FALSE)

personas   <- read_rds(file.path(DATA_DIR, "personas.rds"))
relaciones <- read_rds(file.path(DATA_DIR, "relaciones.rds"))
partidos   <- read_rds(file.path(DATA_DIR, "partidos.rds"))
educacion  <- read_rds(file.path(DATA_DIR, "educacion.rds"))
sucesiones <- read_rds(file.path(DATA_DIR, "sucesiones.rds"))

# ═══════════════════════════════════════════════════════════════════════════════
# ARISTAS
# ═══════════════════════════════════════════════════════════════════════════════

aristas_parentesco <- relaciones %>%
  filter(!is.na(persona_relacionada_id)) %>%
  transmute(from = persona_id, to = persona_relacionada_id,
            tipo = tipo_relacion, categoria = "parentesco", peso = 1.0)

# H18: "misma_familia" edges cause combinatorial explosion (O(n²) per family).
# Only create same-family edges for SMALL families (≤ 15 members).
# Larger families are already connected via kinship edges.
fam_sizes <- personas %>%
  filter(!is.na(familia_norm)) %>%
  count(familia_norm, name = "n_miembros")
small_fams <- fam_sizes %>% filter(n_miembros <= 15) %>% pull(familia_norm)

aristas_familia <- personas %>%
  select(persona_id, familia_norm) %>%
  filter(!is.na(familia_norm), familia_norm %in% small_fams) %>%
  inner_join(
    personas %>% select(persona_id2 = persona_id, familia_norm) %>%
      filter(!is.na(familia_norm), familia_norm %in% small_fams),
    by = "familia_norm", relationship = "many-to-many"
  ) %>%
  filter(persona_id < persona_id2) %>%
  transmute(from = persona_id, to = persona_id2,
            tipo = "misma_familia", categoria = "familia", peso = 0.3)

aristas_sucesion <- sucesiones %>%
  filter(!is.na(persona_relacionada_id)) %>%
  transmute(from = persona_id, to = persona_relacionada_id,
            tipo = paste0("sucesion_", rol), categoria = "politica", peso = 0.8)

aristas_partido <- partidos %>%
  inner_join(partidos, by = "partido", relationship = "many-to-many", suffix = c("_1", "_2")) %>%
  filter(persona_id_1 < persona_id_2) %>%
  transmute(from = persona_id_1, to = persona_id_2,
            tipo = paste0("mismo_partido:", str_trunc(partido, 30)),
            categoria = "politica", peso = 0.3) %>%
  distinct(from, to, .keep_all = TRUE)

aristas_educacion <- educacion %>%
  inner_join(educacion, by = "institucion", relationship = "many-to-many", suffix = c("_1", "_2")) %>%
  filter(persona_id_1 < persona_id_2) %>%
  transmute(from = persona_id_1, to = persona_id_2,
            tipo = paste0("misma_universidad:", str_trunc(institucion, 30)),
            categoria = "educacion", peso = 0.3) %>%
  distinct(from, to, .keep_all = TRUE)

todas_aristas <- bind_rows(
  aristas_parentesco, aristas_familia, aristas_sucesion,
  aristas_partido, aristas_educacion
)

cat("Aristas por categoría:\n")
todas_aristas %>% count(categoria) %>% print()

# ═══════════════════════════════════════════════════════════════════════════════
# GRAFO
# ═══════════════════════════════════════════════════════════════════════════════

nodos <- personas %>%
  select(persona_id, nombre, familia_norm, pais_base, anio_nacimiento, ocupacion)

grafo <- tbl_graph(
  nodes = nodos,
  edges = todas_aristas,
  directed = FALSE
) %>%
  mutate(
    grado = centrality_degree(),
    betweenness = centrality_betweenness(),
    pagerank = centrality_pagerank(),
    comunidad = as.factor(group_louvain()),
    componente = group_components()
  )

write_rds(grafo, file.path(DATA_DIR, "grafo.rds"))
cat("Grafo guardado:", vcount(grafo), "nodos,", ecount(grafo), "aristas\n")

# ═══════════════════════════════════════════════════════════════════════════════
# MÉTRICAS DE CENTRALIDAD
# ═══════════════════════════════════════════════════════════════════════════════

metricas <- grafo %>%
  as_tibble() %>%
  filter(grado > 0) %>%
  # One Wikipedia scrape occasionally yields duplicate rows for the same person
  # (distinct persona_id, same name/year); keep highest betweenness for exports/tables
  group_by(nombre, familia_norm, pais_base, anio_nacimiento) %>%
  slice_max(betweenness, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(desc(betweenness))

write_csv(metricas, file.path(OUT_TABLE, "metricas_centralidad.csv"))
cat("\nTop 15 por betweenness:\n")
metricas %>%
  select(nombre, familia_norm, pais_base, grado, betweenness, comunidad) %>%
  head(15) %>%
  print()

# ═══════════════════════════════════════════════════════════════════════════════
# VISUALIZACIONES
# ═══════════════════════════════════════════════════════════════════════════════

# 1. Top 20 por grado
p_grado <- metricas %>%
  slice_max(grado, n = 20) %>%
  mutate(nombre = fct_reorder(nombre, grado)) %>%
  ggplot(aes(grado, nombre, fill = pais_base)) +
  geom_col(alpha = 0.85) +
  geom_text(aes(label = grado), hjust = -0.1, size = 3) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Top 20 individuals by degree (number of connections)",
       x = "Connections", y = NULL, fill = "Country") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

ggsave(file.path(OUT_FIG, "grafo_top_grado.png"), p_grado,
       width = 10, height = 7, dpi = 150)

# 2. Top 20 por betweenness
p_btw <- metricas %>%
  slice_max(betweenness, n = 20) %>%
  mutate(nombre = fct_reorder(nombre, betweenness)) %>%
  ggplot(aes(betweenness, nombre, fill = pais_base)) +
  geom_col(alpha = 0.85) +
  labs(title = "Top 20 structural brokers (betweenness centrality)",
       x = "Betweenness centrality", y = NULL, fill = "Country") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

ggsave(file.path(OUT_FIG, "grafo_top_betweenness.png"), p_btw,
       width = 10, height = 7, dpi = 150)

# 3. Distribución de comunidades por país
p_com <- metricas %>%
  filter(!is.na(pais_base)) %>%
  count(comunidad, pais_base) %>%
  group_by(comunidad) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  filter(total >= 10) %>%
  ggplot(aes(fct_reorder(comunidad, -total), n, fill = pais_base)) +
  geom_col() +
  labs(title = "Louvain community composition by country",
       subtitle = "Communities with 10+ members only",
       x = "Community", y = "Persons", fill = "Country") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(OUT_FIG, "grafo_comunidades_por_pais.png"), p_com,
       width = 12, height = 6, dpi = 150)

# 4. Red del componente más grande
mayor_comp <- which.max(table(V(grafo)$componente))
subgrafo <- grafo %>%
  filter(componente == mayor_comp, grado >= 3)

if (vcount(subgrafo) > 5 && vcount(subgrafo) <= 5000) {
  p_red <- subgrafo %>%
    mutate(es_top = betweenness >= quantile(betweenness, 0.95, na.rm = TRUE)) %>%
    ggraph(layout = "fr") +
    geom_edge_link(alpha = 0.08, color = "gray50") +
    geom_node_point(aes(color = pais_base, size = grado), alpha = 0.7) +
    geom_node_text(aes(label = ifelse(es_top, str_trunc(nombre, 20), "")),
                   size = 2.2, repel = TRUE) +
    scale_size_continuous(range = c(0.5, 5)) +
    labs(title = "Main component — nodes with degree ≥ 3",
         color = "Country", size = "Degree") +
    theme_void(base_size = 11) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5))

  ggsave(file.path(OUT_FIG, "grafo_componente_principal.png"), p_red,
         width = 14, height = 10, dpi = 150)
}

message("Grafo y métricas guardados.")
