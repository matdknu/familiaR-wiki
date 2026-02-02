# ============================================================================
# red_casos_especiales_chile_argentina.R
# Detectar casos especiales: personas de un país en familias del otro
# (Bolocco-Menem, Bielsa, diplomáticos, etc.)
# ============================================================================

library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(ggraph)
library(tidygraph)
library(igraph)

cat("📊 Cargando y analizando casos especiales...\n")

# Cargar datos
chile_data <- read_delim("data/raw/chile/familias/_CONSOLIDADO_todas_familias.csv", 
                         delim = ";", show_col_types = FALSE, locale = locale(encoding = "UTF-8"))
argentina_data <- read_delim("data/raw/argentina/familias/_CONSOLIDADO_todas_familias.csv", 
                             delim = ";", show_col_types = FALSE, locale = locale(encoding = "UTF-8"))

# Detectar nacionalidad real de las personas
detect_nationality <- function(row_data) {
  text_sources <- c(
    row_data$nacionalidad,
    row_data$lugar_nacimiento,
    row_data$biografia,
    row_data$biografia_inicial
  )
  
  combined <- tolower(paste(text_sources[!is.na(text_sources)], collapse = " "))
  
  # Priorizar nacionalidad declarada
  if (!is.na(row_data$nacionalidad) && nchar(row_data$nacionalidad) > 0) {
    nac <- tolower(row_data$nacionalidad)
    if (str_detect(nac, "chilen")) return("Chile")
    if (str_detect(nac, "argentin")) return("Argentina")
  }
  
  # Luego lugar de nacimiento
  if (!is.na(row_data$lugar_nacimiento) && nchar(row_data$lugar_nacimiento) > 0) {
    lugar <- tolower(row_data$lugar_nacimiento)
    if (str_detect(lugar, "santiago|chile|valparaíso|concepción")) return("Chile")
    if (str_detect(lugar, "buenos aires|córdoba|rosario|argentina")) return("Argentina")
  }
  
  # Finalmente biografía
  if (str_detect(combined, "chileno|chilena")) return("Chile")
  if (str_detect(combined, "argentino|argentina")) return("Argentina")
  
  return("Desconocido")
}

# Procesar Chile
cat("Procesando datos de Chile...\n")
chile_processed <- chile_data %>%
  filter(!is.na(url), as.character(url) != "") %>%
  rowwise() %>%
  mutate(
    pais_familia = "Chile",
    pais_nacionalidad = detect_nationality(cur_data())
  ) %>%
  ungroup()

# Procesar Argentina
cat("Procesando datos de Argentina...\n")
argentina_processed <- argentina_data %>%
  filter(!is.na(url), as.character(url) != "") %>%
  rowwise() %>%
  mutate(
    pais_familia = "Argentina",
    pais_nacionalidad = detect_nationality(cur_data())
  ) %>%
  ungroup()

# Combinar
all_data <- bind_rows(chile_processed, argentina_processed) %>%
  distinct(url, .keep_all = TRUE) %>%
  mutate(
    es_caso_especial = (pais_familia != pais_nacionalidad & pais_nacionalidad != "Desconocido"),
    tipo_caso = case_when(
      pais_familia == "Chile" & pais_nacionalidad == "Argentina" ~ "Argentino en familia chilena",
      pais_familia == "Argentina" & pais_nacionalidad == "Chile" ~ "Chileno en familia argentina",
      TRUE ~ "Normal"
    )
  )

# Casos especiales
casos_especiales <- all_data %>%
  filter(es_caso_especial) %>%
  select(nombre, url, pais_familia, pais_nacionalidad, tipo_caso, ocupacion, cargos_politicos)

cat("\n🔍 CASOS ESPECIALES DETECTADOS\n")
cat(strrep("=", 80), "\n")
cat("Total de casos:", nrow(casos_especiales), "\n\n")

if (nrow(casos_especiales) > 0) {
  cat("Top casos especiales:\n")
  print(casos_especiales %>% select(nombre, pais_familia, pais_nacionalidad, tipo_caso) %>% head(30))
  
  # Buscar específicamente Bolocco y Menem
  bolocco_menem <- casos_especiales %>%
    filter(str_detect(tolower(nombre), "bolocco|menem"))
  
  if (nrow(bolocco_menem) > 0) {
    cat("\n⭐ CASO BOLOCCO-MENEM DETECTADO:\n")
    print(bolocco_menem %>% select(nombre, pais_familia, pais_nacionalidad))
  }
}

# Extraer relaciones
extract_relations <- function(relation_field, relation_type, source_url, source_pais_fam, source_pais_nac) {
  if (is.na(relation_field) || relation_field == "") return(tibble())
  
  pattern <- "([^(;]+?)\\s*\\((https://es\\.wikipedia\\.org/wiki/[^)]+)\\)"
  matches <- str_match_all(relation_field, pattern)[[1]]
  
  if (nrow(matches) == 0) return(tibble())
  
  tibble(
    from = source_url,
    to = matches[, 3],
    relation_type = relation_type,
    from_pais_familia = source_pais_fam,
    from_pais_nacionalidad = source_pais_nac,
    related_name = str_trim(matches[, 2])
  )
}

# Crear aristas
cat("\n🔗 Extrayendo relaciones...\n")
edges_list <- list()
for (i in 1:nrow(all_data)) {
  for (field in c("padres", "conyuge", "pareja", "hijos", "hermanos")) {
    if (field %in% colnames(all_data) && !is.na(all_data[[field]][i])) {
      edges_list[[length(edges_list) + 1]] <- extract_relations(
        all_data[[field]][i], field, 
        all_data$url[i], 
        all_data$pais_familia[i],
        all_data$pais_nacionalidad[i]
      )
    }
  }
}

edges_all <- bind_rows(edges_list)

# Enriquecer aristas con info del destino
edges_enriched <- edges_all %>%
  left_join(
    all_data %>% select(url, pais_familia, pais_nacionalidad, es_caso_especial, nombre),
    by = c("to" = "url"),
    suffix = c("_from", "_to")
  ) %>%
  rename(
    to_pais_familia = pais_familia,
    to_pais_nacionalidad = pais_nacionalidad,
    to_es_caso_especial = es_caso_especial,
    to_nombre = nombre
  )

# Conexiones cruzadas (por nacionalidad real)
conexiones_cruzadas <- edges_enriched %>%
  filter(
    !is.na(from_pais_nacionalidad), !is.na(to_pais_nacionalidad),
    from_pais_nacionalidad != "Desconocido", to_pais_nacionalidad != "Desconocido",
    from_pais_nacionalidad != to_pais_nacionalidad
  )

cat("✅ Conexiones cruzadas (por nacionalidad real):", nrow(conexiones_cruzadas), "\n")

if (nrow(conexiones_cruzadas) > 0) {
  cat("\n🌎 Ejemplos de conexiones cruzadas:\n")
  sample_connections <- conexiones_cruzadas %>%
    select(from, to, relation_type, from_pais_nacionalidad, to_pais_nacionalidad, related_name, to_nombre) %>%
    head(20)
  print(sample_connections)
  
  # Buscar Bolocco-Menem específicamente
  bolocco_menem_conn <- conexiones_cruzadas %>%
    filter(str_detect(tolower(from), "bolocco|menem") | str_detect(tolower(to), "bolocco|menem"))
  
  if (nrow(bolocco_menem_conn) > 0) {
    cat("\n⭐ CONEXIÓN BOLOCCO-MENEM:\n")
    print(bolocco_menem_conn %>% 
          select(from, to, relation_type, from_pais_nacionalidad, to_pais_nacionalidad))
  }
}

# Crear nodos para visualización
all_urls <- unique(c(edges_enriched$from, edges_enriched$to))
nodes <- tibble(url = all_urls) %>%
  left_join(
    all_data %>% select(url, nombre, pais_familia, pais_nacionalidad, es_caso_especial, tipo_caso),
    by = "url"
  ) %>%
  mutate(
    nombre = ifelse(is.na(nombre), str_replace_all(str_extract(url, "(?<=wiki/).+"), "_", " "), nombre),
    pais_familia = ifelse(is.na(pais_familia), "Desconocido", pais_familia),
    pais_nacionalidad = ifelse(is.na(pais_nacionalidad), "Desconocido", pais_nacionalidad),
    es_caso_especial = ifelse(is.na(es_caso_especial), FALSE, es_caso_especial),
    # Color por nacionalidad
    color_nodo = case_when(
      es_caso_especial ~ "Caso Especial",
      pais_nacionalidad == "Chile" ~ "Chile",
      pais_nacionalidad == "Argentina" ~ "Argentina",
      TRUE ~ "Desconocido"
    )
  )

cat("\n📊 Resumen de nodos:\n")
cat("   Total:", nrow(nodes), "\n")
cat("   Casos especiales:", sum(nodes$es_caso_especial, na.rm = TRUE), "\n")
cat("   Chile:", sum(nodes$pais_nacionalidad == "Chile", na.rm = TRUE), "\n")
cat("   Argentina:", sum(nodes$pais_nacionalidad == "Argentina", na.rm = TRUE), "\n")

# Crear grafo
g_tbl <- tbl_graph(nodes = nodes, edges = edges_enriched, directed = TRUE) %>%
  activate(nodes) %>%
  mutate(degree = centrality_degree(mode = "all"))

# Layout: dos clusters separados
g_igraph <- as.igraph(g_tbl)

chile_nodes_idx <- which(V(g_igraph)$pais_nacionalidad == "Chile")
argentina_nodes_idx <- which(V(g_igraph)$pais_nacionalidad == "Argentina")
especial_nodes_idx <- which(V(g_igraph)$es_caso_especial)
unknown_nodes_idx <- which(V(g_igraph)$pais_nacionalidad == "Desconocido")

# Layouts para cada grupo
layout_combined <- matrix(0, vcount(g_igraph), 2)

if (length(chile_nodes_idx) > 0) {
  g_chile <- induced_subgraph(g_igraph, chile_nodes_idx)
  layout_chile <- layout_with_fr(g_chile)
  layout_chile <- layout_chile * 0.6
  layout_chile[, 1] <- layout_chile[, 1] - max(layout_chile[, 1]) - 3
  layout_combined[chile_nodes_idx, ] <- layout_chile
}

if (length(argentina_nodes_idx) > 0) {
  g_argentina <- induced_subgraph(g_igraph, argentina_nodes_idx)
  layout_argentina <- layout_with_fr(g_argentina)
  layout_argentina <- layout_argentina * 0.6
  layout_argentina[, 1] <- layout_argentina[, 1] - min(layout_argentina[, 1]) + 3
  layout_combined[argentina_nodes_idx, ] <- layout_argentina
}

# Casos especiales en el medio
if (length(especial_nodes_idx) > 0) {
  layout_combined[especial_nodes_idx, 1] <- runif(length(especial_nodes_idx), -1, 1)
  layout_combined[especial_nodes_idx, 2] <- seq(-3, 3, length.out = length(especial_nodes_idx))
}

# Desconocidos dispersos
if (length(unknown_nodes_idx) > 0) {
  layout_combined[unknown_nodes_idx, 1] <- runif(length(unknown_nodes_idx), -0.5, 0.5)
  layout_combined[unknown_nodes_idx, 2] <- runif(length(unknown_nodes_idx), -4, 4)
}

g_tbl <- g_tbl %>%
  activate(nodes) %>%
  mutate(x = layout_combined[, 1], y = layout_combined[, 2])

# Visualización
color_palette <- c(
  "Chile" = "#0033A0",
  "Argentina" = "#6CACE4",
  "Caso Especial" = "#FF6B6B",
  "Desconocido" = "gray70"
)

p <- ggraph(g_tbl, layout = "manual", x = x, y = y) +
  geom_edge_link(
    aes(color = ifelse(
      !is.na(from_pais_nacionalidad) & !is.na(to_pais_nacionalidad) &
      from_pais_nacionalidad != to_pais_nacionalidad &
      from_pais_nacionalidad != "Desconocido" & to_pais_nacionalidad != "Desconocido",
      "Cruzada", "Normal"
    )),
    alpha = 0.2,
    arrow = arrow(length = unit(1, "mm"), type = "closed")
  ) +
  scale_edge_color_manual(
    values = c("Cruzada" = "#FF6B6B", "Normal" = "gray70"),
    name = "Conexión"
  ) +
  geom_node_point(aes(size = degree, color = color_nodo, shape = es_caso_especial), alpha = 0.7) +
  geom_node_text(
    aes(label = ifelse(es_caso_especial | degree >= 15, nombre, "")),
    size = 2.5,
    repel = TRUE,
    fontface = "bold",
    max.overlaps = 30
  ) +
  scale_size_continuous(range = c(0.5, 6), name = "Grado") +
  scale_color_manual(values = color_palette, name = "Nacionalidad") +
  scale_shape_manual(values = c("TRUE" = 17, "FALSE" = 19), guide = "none") +
  annotate("text", x = -5, y = 5, label = "CHILE", size = 10, fontface = "bold", color = "#0033A0", alpha = 0.5) +
  annotate("text", x = 5, y = 5, label = "ARGENTINA", size = 10, fontface = "bold", color = "#6CACE4", alpha = 0.5) +
  labs(
    title = "Red Chile-Argentina: Casos Especiales",
    subtitle = paste(
      "Triángulos rojos = Personas de un país en familias del otro |",
      "Casos especiales:", sum(nodes$es_caso_especial, na.rm = TRUE),
      "| Conexiones cruzadas:", nrow(conexiones_cruzadas)
    ),
    caption = "Incluye: Cecilia Bolocco (chilena en Familia Menem), Benjamín Vicuña (chileno con parejas argentinas), etc."
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    plot.caption = element_text(hjust = 0.5, size = 8, color = "gray50"),
    legend.position = "right"
  )


p

# Guardar
dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs/tables", showWarnings = FALSE, recursive = TRUE)

ggsave("outputs/figures/red_casos_especiales_chile_argentina.png", p, 
       width = 26, height = 18, dpi = 300)
cat("\n✅ Guardado: outputs/figures/red_casos_especiales_chile_argentina.png\n")

# Guardar tablas
write_csv(casos_especiales, "outputs/tables/casos_especiales_nacionalidad.csv")
write_csv(conexiones_cruzadas, "outputs/tables/conexiones_cruzadas_nacionalidad.csv")

cat("\n", strrep("=", 80), "\n")
cat("📊 RESUMEN FINAL\n")
cat(strrep("=", 80), "\n")
cat("   Casos especiales (nacionalidad ≠ familia):", nrow(casos_especiales), "\n")
cat("   Conexiones cruzadas:", nrow(conexiones_cruzadas), "\n")
cat(strrep("=", 80), "\n")

