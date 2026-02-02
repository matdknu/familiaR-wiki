# ============================================================================
# red_dos_globos_chile_argentina.R  
# Visualización con dos clusters separados (Chile y Argentina) 
# y conexiones cruzadas en el medio
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
library(jsonlite)

cat("📊 Cargando datos de Chile y Argentina...\n")

# Cargar datos
chile_data <- read_delim("data/raw/chile/familias/_CONSOLIDADO_todas_familias.csv", 
                         delim = ";", show_col_types = FALSE, locale = locale(encoding = "UTF-8"))
argentina_data <- read_delim("data/raw/argentina/familias/_CONSOLIDADO_todas_familias.csv", 
                             delim = ";", show_col_types = FALSE, locale = locale(encoding = "UTF-8"))

chile_data <- chile_data %>% mutate(pais = "Chile") %>% filter(!is.na(url), as.character(url) != "")
argentina_data <- argentina_data %>% mutate(pais = "Argentina") %>% filter(!is.na(url), as.character(url) != "")

all_data <- bind_rows(chile_data, argentina_data) %>% distinct(url, .keep_all = TRUE)

cat("✅ Chile:", sum(all_data$pais == "Chile"), "personas\n")
cat("✅ Argentina:", sum(all_data$pais == "Argentina"), "personas\n")

# ============================================================================
# DETECCIÓN DE CONEXIONES DIPLOMÁTICAS/CARGOS EN EL OTRO PAÍS
# ============================================================================

cat("\n🔍 Detectando conexiones diplomáticas y menciones del otro país...\n")

# Detectar menciones de Argentina en personas chilenas y viceversa
detect_country_connections <- function(row_data, target_country) {
  text_fields <- c(
    row_data$cargos_politicos,
    row_data$ocupacion,
    row_data$biografia,
    row_data$biografia_inicial,
    row_data$infobox_completa
  )
  
  combined_text <- paste(text_fields[!is.na(text_fields)], collapse = " ")
  combined_text_lower <- tolower(combined_text)
  
  # Palabras clave que indican conexión
  keywords <- if(target_country == "Argentina") {
    c("embajador.*argentina", "ministro.*argentina", "diplomático.*argentina", 
      "argentino", "argentina", "buenos aires", "menem", "perón")
  } else {
    c("embajador.*chile", "ministro.*chile", "diplomático.*chile",
      "chileno", "chilena", "santiago.*chile")
  }
  
  has_connection <- any(sapply(keywords, function(k) str_detect(combined_text_lower, k)))
  
  # Extraer contexto
  context <- ""
  if (has_connection) {
    for (kw in keywords) {
      if (str_detect(combined_text_lower, kw)) {
        # Buscar contexto alrededor de la palabra clave
        matches <- str_extract_all(combined_text, paste0(".{0,50}", kw, ".{0,50}"))[[1]]
        if (length(matches) > 0) {
          context <- paste(matches, collapse = " | ")
          break
        }
      }
    }
  }
  
  list(
    has_connection = has_connection,
    context = context
  )
}

# Aplicar detección
cat("Analizando conexiones diplomáticas...\n")
chile_connections_to_arg <- all_data %>%
  filter(pais == "Chile") %>%
  rowwise() %>%
  do({
    conn <- detect_country_connections(., "Argentina")
    tibble(
      url = .$url,
      nombre = .$nombre,
      pais = .$pais,
      conecta_con = if(conn$has_connection) "Argentina" else NA_character_,
      contexto_conexion = if(conn$has_connection) conn$context else NA_character_
    )
  }) %>%
  filter(!is.na(conecta_con))

argentina_connections_to_chile <- all_data %>%
  filter(pais == "Argentina") %>%
  rowwise() %>%
  do({
    conn <- detect_country_connections(., "Chile")
    tibble(
      url = .$url,
      nombre = .$nombre,
      pais = .$pais,
      conecta_con = if(conn$has_connection) "Chile" else NA_character_,
      contexto_conexion = if(conn$has_connection) conn$context else NA_character_
    )
  }) %>%
  filter(!is.na(conecta_con))

all_country_connections <- bind_rows(chile_connections_to_arg, argentina_connections_to_chile)

cat("✅ Chilenos con conexiones a Argentina:", nrow(chile_connections_to_arg), "\n")
cat("✅ Argentinos con conexiones a Chile:", nrow(argentina_connections_to_chile), "\n")

if (nrow(all_country_connections) > 0) {
  cat("\n📋 Ejemplos de conexiones detectadas:\n")
  print(all_country_connections %>% select(nombre, pais, conecta_con) %>% head(20))
}

# ============================================================================
# EXTRAER RELACIONES FAMILIARES
# ============================================================================

cat("\n🔗 Extrayendo relaciones familiares...\n")

extract_relations <- function(relation_field, relation_type, source_url, source_pais) {
  if (is.na(relation_field) || relation_field == "") return(tibble())
  
  pattern <- "([^(;]+?)\\s*\\((https://es\\.wikipedia\\.org/wiki/[^)]+)\\)"
  matches <- str_match_all(relation_field, pattern)[[1]]
  
  if (nrow(matches) == 0) return(tibble())
  
  tibble(
    from = source_url,
    to = matches[, 3],
    relation_type = relation_type,
    from_pais = source_pais,
    related_name = str_trim(matches[, 2])
  )
}

edges_list <- list()
for (i in 1:nrow(all_data)) {
  for (field in c("padres", "conyuge", "pareja", "hijos", "hermanos", "familia")) {
    if (field %in% colnames(all_data) && !is.na(all_data[[field]][i])) {
      edges_list[[length(edges_list) + 1]] <- extract_relations(
        all_data[[field]][i], field, all_data$url[i], all_data$pais[i]
      )
    }
  }
}

edges_all <- bind_rows(edges_list)

# Crear nodos
all_urls <- unique(c(edges_all$from, edges_all$to))
nodes <- tibble(url = all_urls) %>%
  left_join(all_data %>% select(url, nombre, pais), by = "url") %>%
  left_join(all_country_connections %>% select(url, conecta_con, contexto_conexion), by = "url") %>%
  mutate(
    nombre = ifelse(is.na(nombre), str_replace_all(str_extract(url, "(?<=wiki/).+"), "_", " "), nombre),
    pais = ifelse(is.na(pais), "Desconocido", pais),
    tipo_nodo = case_when(
      !is.na(conecta_con) ~ "Puente",
      pais %in% c("Chile", "Argentina") ~ "Nacional",
      TRUE ~ "Desconocido"
    )
  )

# Enriquecer aristas con país destino
edges_enriched <- edges_all %>%
  left_join(nodes %>% select(url, pais) %>% rename(to_pais = pais), by = c("to" = "url"))

# Detectar conexiones directas entre países
cross_country_direct <- edges_enriched %>%
  filter(
    !is.na(from_pais), !is.na(to_pais),
    from_pais != "Desconocido", to_pais != "Desconocido",
    from_pais != to_pais
  )

cat("✅ Nodos totales:", nrow(nodes), "\n")
cat("   - Chile:", sum(nodes$pais == "Chile"), "\n")
cat("   - Argentina:", sum(nodes$pais == "Argentina"), "\n")
cat("   - Desconocidos:", sum(nodes$pais == "Desconocido"), "\n")
cat("   - Nodos puente:", sum(nodes$tipo_nodo == "Puente"), "\n")
cat("✅ Conexiones familiares directas entre países:", nrow(cross_country_direct), "\n")

# ============================================================================
# CREAR GRAFO CON LAYOUT ESPECIAL
# ============================================================================

cat("\n🎨 Creando visualización con dos globos...\n")

# Crear grafo
g_tbl <- tbl_graph(nodes = nodes, edges = edges_enriched, directed = TRUE) %>%
  activate(nodes) %>%
  mutate(
    degree = centrality_degree(mode = "all"),
    betweenness = centrality_betweenness()
  )

# Crear layout manual: dos clusters separados
g_igraph <- as.igraph(g_tbl)

# Separar nodos por país
chile_nodes_idx <- which(V(g_igraph)$pais == "Chile")
argentina_nodes_idx <- which(V(g_igraph)$pais == "Argentina")
unknown_nodes_idx <- which(V(g_igraph)$pais == "Desconocido")
bridge_nodes_idx <- which(V(g_igraph)$tipo_nodo == "Puente")

# Crear subgrafos para cada país
if (length(chile_nodes_idx) > 0) {
  g_chile <- induced_subgraph(g_igraph, chile_nodes_idx)
  layout_chile <- layout_with_fr(g_chile)
  # Escalar y mover a la izquierda
  layout_chile <- layout_chile * 0.8
  layout_chile[, 1] <- layout_chile[, 1] - max(layout_chile[, 1]) - 2
} else {
  layout_chile <- matrix(0, 0, 2)
}

if (length(argentina_nodes_idx) > 0) {
  g_argentina <- induced_subgraph(g_igraph, argentina_nodes_idx)
  layout_argentina <- layout_with_fr(g_argentina)
  # Escalar y mover a la derecha
  layout_argentina <- layout_argentina * 0.8
  layout_argentina[, 1] <- layout_argentina[, 1] - min(layout_argentina[, 1]) + 2
} else {
  layout_argentina <- matrix(0, 0, 2)
}

# Layout para nodos puente (en el medio)
if (length(bridge_nodes_idx) > 0) {
  layout_bridge <- matrix(0, length(bridge_nodes_idx), 2)
  layout_bridge[, 1] <- runif(length(bridge_nodes_idx), -1, 1)
  layout_bridge[, 2] <- seq(-2, 2, length.out = length(bridge_nodes_idx))
} else {
  layout_bridge <- matrix(0, 0, 2)
}

# Layout para desconocidos (esparcidos en el medio)
if (length(unknown_nodes_idx) > 0) {
  layout_unknown <- matrix(0, length(unknown_nodes_idx), 2)
  layout_unknown[, 1] <- runif(length(unknown_nodes_idx), -0.5, 0.5)
  layout_unknown[, 2] <- runif(length(unknown_nodes_idx), -3, 3)
} else {
  layout_unknown <- matrix(0, 0, 2)
}

# Combinar layouts
layout_combined <- matrix(0, vcount(g_igraph), 2)
if (length(chile_nodes_idx) > 0) layout_combined[chile_nodes_idx, ] <- layout_chile
if (length(argentina_nodes_idx) > 0) layout_combined[argentina_nodes_idx, ] <- layout_argentina
if (length(bridge_nodes_idx) > 0) layout_combined[bridge_nodes_idx, ] <- layout_bridge
if (length(unknown_nodes_idx) > 0) layout_combined[unknown_nodes_idx, ] <- layout_unknown

# Agregar layout al grafo tidygraph
g_tbl <- g_tbl %>%
  activate(nodes) %>%
  mutate(
    x = layout_combined[, 1],
    y = layout_combined[, 2]
  )

# Colores
country_colors <- c(
  "Chile" = "red",
  "Argentina" = "#6CACE4",
  "Desconocido" = "gray70"
)

tipo_colors <- c(
  "Puente" = "#FF6B6B",
  "Nacional" = "#4ECDC4",
  "Desconocido" = "gray70"
)

# Visualización
p <- ggraph(g_tbl, layout = "manual", x = x, y = y) +
  # Aristas normales
  geom_edge_link(
    aes(color = ifelse(from_pais != to_pais & !is.na(to_pais), "cross", "normal")),
    alpha = 0.15,
    arrow = arrow(length = unit(1, "mm"), type = "closed")
  ) +
  scale_edge_color_manual(
    values = c("cross" = "#FF6B6B", "normal" = "gray70"),
    name = "Tipo de conexión",
    labels = c("cross" = "Entre países", "normal" = "Dentro del país")
  ) +
  # Nodos
  geom_node_point(aes(size = degree, color = pais, shape = tipo_nodo), alpha = 0.7) +
  # Etiquetas para nodos puente
  geom_node_text(
    aes(label = ifelse(tipo_nodo == "Puente", nombre, "")),
    size = 2.5,
    repel = TRUE,
    max.overlaps = 20,
    fontface = "bold",
    color = "#FF6B6B"
  ) +
  # Etiquetas para nodos con alto grado
  geom_node_text(
    aes(label = ifelse(degree >= 10 & tipo_nodo != "Puente", nombre, "")),
    size = 2,
    repel = TRUE,
    max.overlaps = 15
  ) +
  scale_size_continuous(range = c(0.5, 5), name = "Grado") +
  scale_color_manual(values = country_colors, name = "País") +
  scale_shape_manual(
    values = c("Puente" = 17, "Nacional" = 19, "Desconocido" = 4),
    name = "Tipo"
  ) +
  # Anotaciones
  annotate("text", x = min(layout_combined[, 1]) - 1, y = max(layout_combined[, 2]) + 1, 
           label = "CHILE", size = 8, fontface = "bold", color = "red", alpha = 0.6) +
  annotate("text", x = max(layout_combined[, 1]) + 1, y = max(layout_combined[, 2]) + 1, 
           label = "ARGENTINA", size = 8, fontface = "bold", color = "#6CACE4", alpha = 0.6) +
  labs(
    title = "Red Familiar Chile-Argentina: Dos Globos Conectados",
    subtitle = paste(
      "Chile:", sum(nodes$pais == "Chile"),
      "| Argentina:", sum(nodes$pais == "Argentina"),
      "| Nodos puente:", sum(nodes$tipo_nodo == "Puente"),
      "| Conexiones entre países:", nrow(cross_country_direct)
    ),
    caption = "Nodos rojos (triángulos) = personas con conexiones diplomáticas/profesionales al otro país"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    plot.caption = element_text(hjust = 0.5, size = 9, color = "gray50"),
    legend.position = "right"
  )

p

# Guardar
dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs/tables", showWarnings = FALSE, recursive = TRUE)

ggsave("outputs/figures/red_dos_globos_chile_argentina.png", p, 
       width = 24, height = 16, dpi = 300)
cat("✅ Guardado: outputs/figures/red_dos_globos_chile_argentina.png\n")

# Guardar tablas
if (nrow(all_country_connections) > 0) {
  write_csv(all_country_connections, "outputs/tables/nodos_puente_chile_argentina.csv")
  cat("✅ Guardado: outputs/tables/nodos_puente_chile_argentina.csv\n")
}

if (nrow(cross_country_direct) > 0) {
  write_csv(cross_country_direct, "outputs/tables/conexiones_familiares_directas.csv")
  cat("✅ Guardado: outputs/tables/conexiones_familiares_directas.csv\n")
}

# Resumen
cat("\n", strrep("=", 80), "\n")
cat("📊 RESUMEN: DOS GLOBOS CONECTADOS\n")
cat(strrep("=", 80), "\n")
cat("   Nodos totales:", nrow(nodes), "\n")
cat("   - Chile:", sum(nodes$pais == "Chile"), "\n")
cat("   - Argentina:", sum(nodes$pais == "Argentina"), "\n")
cat("   - Desconocidos:", sum(nodes$pais == "Desconocido"), "\n")
cat("   Nodos puente (conexiones diplomáticas):", sum(nodes$tipo_nodo == "Puente"), "\n")
cat("   Conexiones familiares directas:", nrow(cross_country_direct), "\n")
cat(strrep("=", 80), "\n")

