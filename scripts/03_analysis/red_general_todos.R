# ============================================================================
# red_general_todos.R
# Red general de todos los personajes (simple, tidyverse)
# ============================================================================


getwd()


system("cat ~/.ssh/id_ed25519.pub")

system("ls -la ~/.ssh | grep id_ed25519")


library(readr)
library(dplyr)
library(igraph)
library(ggraph)
library(tidygraph)
library(ggplot2)
library(RColorBrewer)

cat("📊 Cargando datos...\n")
persons_all <- read_csv("data/processed/persons_normalized.csv", show_col_types = FALSE)
family_relations_all <- read_csv("data/processed/family_relations_normalized.csv", show_col_types = FALSE)
derived_all <- read_csv("data/processed/derived_normalized.csv", show_col_types = FALSE)

cat("✅ Personas:", nrow(persons_all), "\n")
cat("✅ Relaciones:", nrow(family_relations_all), "\n")

# Aristas simples (persona -> familiar mencionado)
edges <- family_relations_all %>%
  filter(!is.na(person_id), !is.na(related_name_raw), related_name_raw != "") %>%
  transmute(from = person_id, to = related_name_raw)

# Nodos (todos los IDs y nombres mencionados)
nodes <- tibble(node_id = unique(c(edges$from, edges$to))) %>%
  left_join(derived_all %>% select(person_id, canonical_name, surname_1), 
            by = c("node_id" = "person_id")) %>%
  mutate(
    label = ifelse(is.na(canonical_name), node_id, canonical_name),
    surname_1 = ifelse(is.na(surname_1), "Otro", surname_1)
  )

cat("✅ Nodos:", nrow(nodes), "\n")
cat("✅ Aristas:", nrow(edges), "\n")

# Identificar apellidos más importantes (top 10 por cantidad de nodos)
top_surnames <- nodes %>%
  filter(surname_1 != "Otro") %>%
  count(surname_1, sort = TRUE) %>%
  slice_head(n = 10) %>%
  pull(surname_1)

cat("✅ Top apellidos:", paste(top_surnames, collapse = ", "), "\n")

# Grafo completo inicial con todos los datos
g_tbl <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE) %>%
  activate(nodes) %>%
  mutate(
    degree = centrality_degree(),
    surname_group = ifelse(surname_1 %in% top_surnames, surname_1, "Otro")
  )

cat("   - Grafo original:", nrow(g_tbl %>% activate(nodes) %>% as_tibble()), "nodos\n")

# Detectar comunidades: convertir a no dirigido temporalmente usando igraph
g_igraph <- as.igraph(g_tbl)
g_undirected_igraph <- as.undirected(g_igraph, mode = "mutual")
community_result <- cluster_louvain(g_undirected_igraph)

# Crear tabla de comunidades
node_names_all <- V(g_igraph)$name
if (is.null(node_names_all)) {
  node_names_all <- as.character(seq_len(vcount(g_igraph)))
}
communities_temp <- tibble(
  node_id = node_names_all,
  community = community_result$membership
)

# Agregar comunidades al grafo dirigido original
g_tbl <- g_tbl %>%
  activate(nodes) %>%
  left_join(communities_temp, by = "node_id")

# Analizar comunidades
nodes_df <- g_tbl %>%
  activate(nodes) %>%
  as_tibble()

cat("   - Nodos con comunidad:", sum(!is.na(nodes_df$community)), "\n")
cat("   - Comunidades únicas:", length(unique(nodes_df$community[!is.na(nodes_df$community)])), "\n")

community_info <- nodes_df %>%
  filter(!is.na(community)) %>%
  count(community, sort = TRUE) %>%
  mutate(community_label = ifelse(n >= 10, paste0("Clúster ", community), "Pequeño"))

# Agregar labels de comunidades grandes
g_tbl <- g_tbl %>%
  activate(nodes) %>%
  left_join(community_info %>% select(community, n, community_label), by = "community") %>%
  mutate(
    label_text = ifelse(
      surname_1 %in% top_surnames, 
      surname_1,
      ifelse(n >= 10, paste0("C", community), "")
    )
  )

cat("✅ Comunidades detectadas:", nrow(community_info), "\n")
cat("✅ Comunidades grandes (≥10 nodos):", sum(community_info$n >= 10), "\n")

# Colores para comunidades (para ver polarización)
communities_unique <- g_tbl %>%
  activate(nodes) %>%
  as_tibble() %>%
  filter(!is.na(community)) %>%
  distinct(community) %>%
  pull(community) %>%
  sort()

n_communities <- length(communities_unique)
if (n_communities <= 12) {
  colors_comm <- RColorBrewer::brewer.pal(max(3, n_communities), "Set3")[1:n_communities]
} else {
  colors_comm <- c(RColorBrewer::brewer.pal(12, "Set3"), 
                   rep("gray70", n_communities - 12))
}
names(colors_comm) <- as.character(communities_unique[1:length(colors_comm)])

cat("🎨 Generando red con clústeres y flechas...\n")

# Red coloreada por comunidad (para ver polarización)
p_red_clusters <- ggraph(g_tbl, layout = "fr") +
  geom_edge_link(
    alpha = 0.15, 
    colour = "gray70",
    arrow = arrow(length = unit(2, "mm"), type = "closed")
  ) +
  geom_node_point(aes(size = degree, color = as.character(community)), alpha = 0.8) +
  geom_node_text(aes(label = label_text), size = 2.5, repel = TRUE, fontface = "bold", max.overlaps = 20) +
  scale_size_continuous(range = c(1, 8), name = "Grado") +
  scale_color_manual(values = colors_comm, name = "Clúster", na.value = "gray90") +
  labs(
    title = "Red General con Clústeres y Polarización",
    subtitle = paste(
      "Flechas muestran dirección de relaciones. ",
      nrow(community_info), " clústeres detectados (Louvain)"
    )
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    legend.position = "bottom"
  )

# También crear versión por apellido para comparar
p_red_surnames <- ggraph(g_tbl, layout = "fr") +
  geom_edge_link(
    alpha = 0.1, 
    colour = "gray80",
    arrow = arrow(length = unit(2, "mm"), type = "closed")
  ) +
  geom_node_point(aes(size = degree, color = surname_group), alpha = 0.7) +
  geom_node_text(
    aes(label = ifelse(surname_1 %in% top_surnames, surname_1, "")), 
    size = 3, 
    repel = TRUE, 
    fontface = "bold"
  ) +
  scale_size_continuous(range = c(1, 8), name = "Grado") +
  scale_color_manual(
    values = c(
      setNames(RColorBrewer::brewer.pal(min(10, length(top_surnames)), "Set3")[1:length(top_surnames)], 
               top_surnames),
      "Otro" = "lightgray"
    ),
    name = "Apellido"
  ) +
  labs(
    title = "Red General por Apellidos",
    subtitle = paste("Top", length(top_surnames), "apellidos más importantes")
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "bottom"
  )

dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs/tables", showWarnings = FALSE, recursive = TRUE)

# Guardar red por clústeres (polarización)
ggsave("outputs/figures/red_general_todos_clusters.png", p_red_clusters, 
       width = 18, height = 14, dpi = 300)
cat("✅ Guardado: outputs/figures/red_general_todos_clusters.png\n")

# Guardar red por apellidos
ggsave("outputs/figures/red_general_todos.png", p_red_surnames, 
       width = 18, height = 14, dpi = 300)
cat("✅ Guardado: outputs/figures/red_general_todos.png\n")

# Guardar tabla de comunidades para análisis
write_csv(community_info, "outputs/tables/comunidades_detectadas.csv")
cat("✅ Guardado: outputs/tables/comunidades_detectadas.csv\n")

cat("\n📊 Resumen de polarización:\n")
cat("   - Si los clústeres están separados espacialmente = hay polarización\n")
cat("   - Si los clústeres se mezclan = redes más integradas\n")
cat("   - Revisa el gráfico de clústeres para ver la estructura\n")
