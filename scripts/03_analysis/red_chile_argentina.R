# ============================================================================
# red_chile_argentina.R
# Red combinada Chile-Argentina para detectar conexiones entre países
# ============================================================================

library(readr)
library(dplyr)
library(igraph)
library(ggraph)
library(tidygraph)
library(ggplot2)
library(RColorBrewer)
library(stringr)
library(purrr)
library(tidyr)

cat("📊 Cargando datos de Chile y Argentina...\n")

# Cargar datos consolidados (separados por ;)
chile_data <- read_delim("data/raw/chile/familias/_CONSOLIDADO_todas_familias.csv", 
                         delim = ";", show_col_types = FALSE, locale = locale(encoding = "UTF-8"))
argentina_data <- read_delim("data/raw/argentina/familias/_CONSOLIDADO_todas_familias.csv", 
                             delim = ";", show_col_types = FALSE, locale = locale(encoding = "UTF-8"))

cat("✅ Chile:", nrow(chile_data), "personas\n")
cat("✅ Argentina:", nrow(argentina_data), "personas\n")

# Agregar columna de país
chile_data <- chile_data %>%
  mutate(pais = "Chile") %>%
  filter(!is.na(url), !is.null(url), as.character(url) != "")

argentina_data <- argentina_data %>%
  mutate(pais = "Argentina") %>%
  filter(!is.na(url), !is.null(url), as.character(url) != "")

# Combinar datos
all_data <- bind_rows(chile_data, argentina_data) %>%
  distinct(url, .keep_all = TRUE)  # Eliminar duplicados por URL

cat("✅ Total único:", nrow(all_data), "personas\n")

# Función para extraer URLs de relaciones familiares
extract_relation_urls <- function(relation_field) {
  if (is.na(relation_field) || relation_field == "") {
    return(character(0))
  }
  # Buscar URLs en formato "Nombre (URL)"
  urls <- str_extract_all(relation_field, "https://es\\.wikipedia\\.org/wiki/[^\\)]+")[[1]]
  return(unique(urls))
}

# Extraer todas las relaciones familiares
cat("\n🔗 Extrayendo relaciones familiares...\n")

edges_list <- list()

for (i in 1:nrow(all_data)) {
  person_url <- all_data$url[i]
  person_name <- all_data$nombre[i]
  
  # Campos de relaciones
  relation_fields <- c("padres", "conyuge", "pareja", "hijos", "hermanos", "familia")
  
  for (field in relation_fields) {
    if (field %in% colnames(all_data)) {
      field_value <- all_data[[field]][i]
      related_urls <- extract_relation_urls(field_value)
      
      if (length(related_urls) > 0) {
        for (related_url in related_urls) {
          edges_list[[length(edges_list) + 1]] <- tibble(
            from = person_url,
            from_name = person_name,
            to = related_url,
            to_name = NA_character_,
            relation_type = field
          )
        }
      }
    }
  }
}

edges <- bind_rows(edges_list)

# Agregar nombres de destino si están en nuestros datos
edges <- edges %>%
  left_join(all_data %>% select(url, nombre) %>% rename(to_name = nombre), 
            by = c("to" = "url")) %>%
  mutate(to_name = coalesce(to_name.x, to_name.y)) %>%
  select(-to_name.x, -to_name.y)

cat("✅ Relaciones extraídas:", nrow(edges), "\n")

# Crear nodos (todas las personas y sus familiares mencionados)
all_urls <- unique(c(edges$from, edges$to))
nodes <- tibble(url = all_urls) %>%
  left_join(all_data %>% select(url, nombre, pais, familia), by = "url") %>%
  mutate(
    nombre = ifelse(is.na(nombre), str_extract(url, "(?<=wiki/)[^$]+"), nombre),
    nombre = str_replace_all(nombre, "_", " "),
    nombre = str_replace_all(nombre, "%C3%A1", "á"),
    nombre = str_replace_all(nombre, "%C3%A9", "é"),
    nombre = str_replace_all(nombre, "%C3%AD", "í"),
    nombre = str_replace_all(nombre, "%C3%B3", "ó"),
    nombre = str_replace_all(nombre, "%C3%BA", "ú"),
    nombre = str_replace_all(nombre, "%C3%B1", "ñ"),
    pais = ifelse(is.na(pais), "Desconocido", pais),
    familia = ifelse(is.na(familia), "Sin familia", familia)
  )

cat("✅ Nodos totales:", nrow(nodes), "\n")
cat("   - Chile:", sum(nodes$pais == "Chile", na.rm = TRUE), "\n")
cat("   - Argentina:", sum(nodes$pais == "Argentina", na.rm = TRUE), "\n")
cat("   - Desconocido:", sum(nodes$pais == "Desconocido", na.rm = TRUE), "\n")

# Detectar conexiones entre países
cat("\n🌎 Detectando conexiones entre países...\n")

cross_country_edges <- edges %>%
  left_join(nodes %>% select(url, pais) %>% rename(from_pais = pais), by = c("from" = "url")) %>%
  left_join(nodes %>% select(url, pais) %>% rename(to_pais = pais), by = c("to" = "url")) %>%
  filter(
    !is.na(from_pais), !is.na(to_pais),
    from_pais != "Desconocido", to_pais != "Desconocido",
    from_pais != to_pais
  )

cat("✅ Conexiones entre países:", nrow(cross_country_edges), "\n")
if (nrow(cross_country_edges) > 0) {
  cat("\n🔗 Ejemplos de conexiones:\n")
  print(cross_country_edges %>%
    select(from_name, to_name, from_pais, to_pais, relation_type) %>%
    head(10))
}

# Crear grafo
cat("\n📊 Construyendo red...\n")

g_tbl <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE) %>%
  activate(nodes) %>%
  mutate(
    degree = centrality_degree(mode = "all"),
    in_degree = centrality_degree(mode = "in"),
    out_degree = centrality_degree(mode = "out")
  )

# Detectar comunidades (usando versión no dirigida)
g_igraph <- as.igraph(g_tbl)
g_undirected <- as.undirected(g_igraph, mode = "mutual")
community_result <- cluster_louvain(g_undirected)

node_names <- V(g_igraph)$name
if (is.null(node_names)) {
  node_names <- as.character(seq_len(vcount(g_igraph)))
}

communities_temp <- tibble(
  url = node_names,
  community = community_result$membership
)

g_tbl <- g_tbl %>%
  activate(nodes) %>%
  left_join(communities_temp, by = "url")

# Identificar comunidades que conectan ambos países
nodes_df <- g_tbl %>%
  activate(nodes) %>%
  as_tibble()

communities_with_both <- nodes_df %>%
  filter(!is.na(community), pais %in% c("Chile", "Argentina")) %>%
  group_by(community) %>%
  summarise(
    has_chile = any(pais == "Chile"),
    has_argentina = any(pais == "Argentina"),
    n_chile = sum(pais == "Chile"),
    n_argentina = sum(pais == "Argentina"),
    total = n()
  ) %>%
  filter(has_chile & has_argentina) %>%
  arrange(desc(total))

cat("\n🔗 Comunidades que conectan ambos países:", nrow(communities_with_both), "\n")
if (nrow(communities_with_both) > 0) {
  cat("\nTop comunidades conectadas:\n")
  print(communities_with_both %>% head(10))
}

# Agregar información de comunidades conectadas
g_tbl <- g_tbl %>%
  activate(nodes) %>%
  left_join(
    communities_with_both %>% 
      select(community, total) %>% 
      rename(community_size = total),
    by = "community"
  ) %>%
  mutate(
    connects_countries = !is.na(community_size),
    pais_color = case_when(
      pais == "Chile" ~ "Chile",
      pais == "Argentina" ~ "Argentina",
      TRUE ~ "Otro/Desconocido"
    )
  )

# Colores para países
country_colors <- c(
  "Chile" = "#0033A0",  # Azul chileno
  "Argentina" = "#6CACE4",  # Celeste argentino
  "Otro/Desconocido" = "gray70"
)

# Visualización 1: Red completa coloreada por país
cat("\n🎨 Generando visualizaciones...\n")

p1 <- ggraph(g_tbl, layout = "fr") +
  geom_edge_link(
    alpha = 0.1,
    colour = "gray70",
    arrow = arrow(length = unit(1.5, "mm"), type = "closed")
  ) +
  geom_node_point(aes(size = degree, color = pais_color), alpha = 0.7) +
  scale_size_continuous(range = c(1, 6), name = "Grado") +
  scale_color_manual(values = country_colors, name = "País") +
  labs(
    title = "Red Familiar Chile-Argentina",
    subtitle = paste(
      "Nodos:", nrow(nodes), 
      "| Aristas:", nrow(edges),
      "| Conexiones entre países:", nrow(cross_country_edges)
    )
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "bottom"
  )

# Visualización 2: Solo nodos que conectan países (si existen)
if (nrow(cross_country_edges) > 0) {
  # Nodos involucrados en conexiones entre países
  connecting_nodes <- unique(c(cross_country_edges$from, cross_country_edges$to))
  
  # Subgrafo de conexiones entre países
  g_connecting <- g_tbl %>%
    activate(nodes) %>%
    filter(url %in% connecting_nodes) %>%
    activate(edges) %>%
    filter(
      .N()$url[from] %in% connecting_nodes & 
      .N()$url[to] %in% connecting_nodes
    )
  
  if (nrow(g_connecting %>% activate(nodes) %>% as_tibble()) > 0) {
    p2 <- ggraph(g_connecting, layout = "fr") +
      geom_edge_link(
        alpha = 0.3,
        colour = "gray50",
        arrow = arrow(length = unit(2, "mm"), type = "closed")
      ) +
      geom_node_point(aes(size = degree, color = pais_color), alpha = 0.8) +
      geom_node_text(
        aes(label = ifelse(degree >= 2, nombre, "")), 
        size = 2.5, 
        repel = TRUE,
        max.overlaps = 15
      ) +
      scale_size_continuous(range = c(2, 8), name = "Grado") +
      scale_color_manual(values = country_colors, name = "País") +
      labs(
        title = "Conexiones entre Chile y Argentina",
        subtitle = paste(
          "Nodos conectados:", nrow(g_connecting %>% activate(nodes) %>% as_tibble()),
          "| Relaciones:", nrow(g_connecting %>% activate(edges) %>% as_tibble())
        )
      ) +
      theme_void() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 11),
        legend.position = "bottom"
      )
  } else {
    p2 <- NULL
  }
} else {
  p2 <- NULL
}

# Guardar resultados
dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs/tables", showWarnings = FALSE, recursive = TRUE)

ggsave("outputs/figures/red_chile_argentina_completa.png", p1, 
       width = 20, height = 16, dpi = 300)
cat("✅ Guardado: outputs/figures/red_chile_argentina_completa.png\n")

if (!is.null(p2)) {
  ggsave("outputs/figures/red_chile_argentina_conexiones.png", p2, 
         width = 18, height = 14, dpi = 300)
  cat("✅ Guardado: outputs/figures/red_chile_argentina_conexiones.png\n")
}

# Guardar tabla de conexiones entre países
if (nrow(cross_country_edges) > 0) {
  write_csv(
    cross_country_edges %>% 
      select(from_name, to_name, from_pais, to_pais, relation_type),
    "outputs/tables/conexiones_chile_argentina.csv"
  )
  cat("✅ Guardado: outputs/tables/conexiones_chile_argentina.csv\n")
}

# Guardar tabla de comunidades conectadas
if (nrow(communities_with_both) > 0) {
  write_csv(communities_with_both, "outputs/tables/comunidades_conectadas_chile_argentina.csv")
  cat("✅ Guardado: outputs/tables/comunidades_conectadas_chile_argentina.csv\n")
}

# Análisis adicional: Apellidos comunes
cat("\n🔍 Buscando apellidos comunes entre países...\n")

# Extraer apellidos de nombres (últimas 1-2 palabras)
nodes_with_surnames <- nodes %>%
  filter(pais %in% c("Chile", "Argentina")) %>%
  mutate(
    name_parts = str_split(nombre, "\\s+"),
    surname_1 = map_chr(name_parts, ~ if(length(.x) >= 1) tail(.x, 1)[1] else ""),
    surname_2 = map_chr(name_parts, ~ if(length(.x) >= 2) tail(.x, 2)[1] else "")
  ) %>%
  filter(surname_1 != "", !is.na(surname_1))

surnames_by_country <- nodes_with_surnames %>%
  select(url, pais, surname_1, surname_2) %>%
  pivot_longer(cols = c(surname_1, surname_2), names_to = "pos", values_to = "surname") %>%
  filter(surname != "", !is.na(surname)) %>%
  group_by(surname, pais) %>%
  summarise(count = n(), .groups = "drop")

common_surnames <- surnames_by_country %>%
  group_by(surname) %>%
  summarise(
    has_chile = any(pais == "Chile"),
    has_argentina = any(pais == "Argentina"),
    n_chile = sum(ifelse(pais == "Chile", count, 0)),
    n_argentina = sum(ifelse(pais == "Argentina", count, 0)),
    total = sum(count)
  ) %>%
  filter(has_chile & has_argentina) %>%
  arrange(desc(total))

cat("✅ Apellidos comunes encontrados:", nrow(common_surnames), "\n")
if (nrow(common_surnames) > 0) {
  cat("\nTop apellidos comunes:\n")
  print(common_surnames %>% head(20))
  
  # Guardar apellidos comunes
  write_csv(common_surnames, "outputs/tables/apellidos_comunes_chile_argentina.csv")
  cat("✅ Guardado: outputs/tables/apellidos_comunes_chile_argentina.csv\n")
} else {
  common_surnames <- tibble(surname = character(), has_chile = logical(), 
                            has_argentina = logical(), n_chile = integer(), 
                            n_argentina = integer(), total = integer())
}

# Resumen final
cat("\n", strrep("=", 80), "\n")
cat("📊 RESUMEN FINAL\n")
cat(strrep("=", 80), "\n")
cat("   Total de personas:", nrow(nodes), "\n")
cat("   - Chile:", sum(nodes$pais == "Chile"), "\n")
cat("   - Argentina:", sum(nodes$pais == "Argentina"), "\n")
cat("   - Desconocido:", sum(nodes$pais == "Desconocido"), "\n")
cat("   Total de relaciones:", nrow(edges), "\n")
cat("   Conexiones directas entre países:", nrow(cross_country_edges), "\n")
cat("   Comunidades que conectan ambos países:", nrow(communities_with_both), "\n")
cat("   Apellidos comunes:", nrow(common_surnames), "\n")
cat(strrep("=", 80), "\n")
