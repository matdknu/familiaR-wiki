# ============================================================================
# red_familias_especificas.R
# Red de familias específicas: Aylwin, Saavedra, García-Huidobro, Bello
# ============================================================================

library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggraph)
library(tidygraph)
library(igraph)

cat("📊 Red de Familias Específicas: Aylwin, Saavedra, Huidobro, Bello\n")
cat(strrep("=", 80), "\n")

# Cargar datos de Chile
chile_data <- read_delim(
  "data/processed/familias/chile/consolidado.csv",
  delim = ";",
  show_col_types = FALSE,
  locale = locale(encoding = "UTF-8")
)

cat("✅ Datos Chile cargados:", nrow(chile_data), "personas\n")

# Filtrar familias de interés
familias_interes <- c("aylwin", "saavedra", "huidobro", "bello", "garcía-huidobro", "garcia-huidobro")

datos_filtrados <- chile_data %>%
  filter(
    str_detect(tolower(familia), paste(familias_interes, collapse = "|")) |
    str_detect(tolower(categoria_origen), paste(familias_interes, collapse = "|")) |
    str_detect(tolower(nombre), paste(familias_interes, collapse = "|"))
  ) %>%
  filter(!is.na(url), url != "")

cat("✅ Personas en familias de interés:", nrow(datos_filtrados), "\n")

# Función para extraer URLs de relaciones
extract_relation_urls <- function(relation_field) {
  if (is.na(relation_field) || relation_field == "") return(character())
  pattern <- "\\(https://es\\.wikipedia\\.org/wiki/([^)]+)\\)"
  matches <- str_match_all(relation_field, pattern)[[1]]
  if (nrow(matches) > 0) {
    return(paste0("https://es.wikipedia.org/wiki/", matches[, 2]))
  }
  return(character())
}

# Extraer familia principal
extract_familia <- function(familia_field, categoria_origen) {
  if (!is.na(familia_field) && familia_field != "") {
    return(str_replace_all(familia_field, "Familia ", ""))
  }
  if (!is.na(categoria_origen) && categoria_origen != "") {
    match <- str_match(categoria_origen, "Familia_([^/]+)")
    if (!is.na(match[1, 2])) {
      return(str_replace_all(match[1, 2], "_", " "))
    }
  }
  return("Desconocida")
}

# Procesar datos
datos_filtrados <- datos_filtrados %>%
  rowwise() %>%
  mutate(
    familia_nombre = extract_familia(familia, categoria_origen)
  ) %>%
  ungroup()

# Normalizar nombres de familia
datos_filtrados <- datos_filtrados %>%
  mutate(
    familia_grupo = case_when(
      str_detect(tolower(familia_nombre), "aylwin") ~ "Aylwin",
      str_detect(tolower(familia_nombre), "huidobro") ~ "García-Huidobro",
      str_detect(tolower(familia_nombre), "bello") ~ "Bello",
      str_detect(tolower(familia_nombre), "saavedra") ~ "Saavedra",
      str_detect(tolower(nombre), "saavedra") ~ "Saavedra",
      TRUE ~ familia_nombre
    )
  )

# Construir edges
cat("\n🔗 Extrayendo relaciones...\n")
edges_list <- list()
for (i in 1:nrow(datos_filtrados)) {
  source_url <- datos_filtrados$url[i]
  source_familia <- datos_filtrados$familia_grupo[i]
  
  for (field in c("padres", "conyuge", "pareja", "hijos", "hermanos")) {
    if (field %in% colnames(datos_filtrados) && !is.na(datos_filtrados[[field]][i])) {
      target_urls <- extract_relation_urls(datos_filtrados[[field]][i])
      for (target_url in target_urls) {
        edges_list[[length(edges_list) + 1]] <- tibble(
          from = source_url,
          to = target_url,
          relation_type = field,
          from_familia = source_familia
        )
      }
    }
  }
}

if (length(edges_list) > 0) {
  edges_all <- bind_rows(edges_list)
  
  # Enriquecer con info del destino
  edges_enriched <- edges_all %>%
    left_join(
      datos_filtrados %>% select(url, nombre, familia_grupo),
      by = c("to" = "url")
    ) %>%
    rename(to_nombre = nombre, to_familia = familia_grupo) %>%
    left_join(
      datos_filtrados %>% select(url, nombre),
      by = c("from" = "url")
    ) %>%
    rename(from_nombre = nombre)
  
  cat("✅ Relaciones extraídas:", nrow(edges_enriched), "\n")
} else {
  edges_enriched <- tibble()
}

# Crear nodos
all_urls <- unique(c(edges_enriched$from, edges_enriched$to))
nodes <- tibble(url = all_urls) %>%
  left_join(
    datos_filtrados %>% select(url, nombre, familia_grupo, cargos_politicos, ocupacion),
    by = "url"
  ) %>%
  mutate(
    nombre = ifelse(is.na(nombre), str_replace_all(str_extract(url, "(?<=wiki/).+"), "_", " "), nombre),
    familia_grupo = ifelse(is.na(familia_grupo), "Otra", familia_grupo),
    tiene_cargo = !is.na(cargos_politicos) & cargos_politicos != ""
  )

cat("✅ Nodos totales:", nrow(nodes), "\n")
cat("\n📊 Distribución por familia:\n")
print(nodes %>% count(familia_grupo, sort = TRUE))

# Crear grafo
g_tbl <- tbl_graph(nodes = nodes, edges = edges_enriched, directed = FALSE) %>%
  activate(nodes) %>%
  mutate(
    degree = centrality_degree(mode = "all"),
    betweenness = centrality_betweenness()
  )

# Colores por familia
familia_colors <- c(
  "Aylwin" = "#0033A0",
  "García-Huidobro" = "#D91023",
  "Bello" = "#006847",
  "Saavedra" = "#FCD116",
  "Otra" = "gray70"
)

# Visualización
cat("\n🎨 Generando visualización...\n")

p <- ggraph(g_tbl, layout = "fr") +
  geom_edge_link(aes(color = relation_type), alpha = 0.5, width = 0.8) +
  scale_edge_color_brewer(palette = "Set2", name = "Relación") +
  geom_node_point(
    aes(size = degree, color = familia_grupo, shape = tiene_cargo),
    alpha = 0.8
  ) +
  geom_node_text(
    aes(label = ifelse(degree >= 3 | tiene_cargo, nombre, "")),
    size = 2.5,
    repel = TRUE,
    max.overlaps = 25
  ) +
  scale_color_manual(values = familia_colors, name = "Familia") +
  scale_size_continuous(range = c(2, 10), name = "Conexiones") +
  scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 17), 
                     labels = c("FALSE" = "Sin cargo", "TRUE" = "Con cargo"),
                     name = "Cargo político") +
  labs(
    title = "Red de Familias: Aylwin, García-Huidobro, Bello, Saavedra",
    subtitle = paste("Nodos:", nrow(nodes), "| Conexiones:", nrow(edges_enriched)),
    caption = "Triángulos = personas con cargos políticos"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    plot.caption = element_text(hjust = 0.5, size = 9, color = "gray50"),
    legend.position = "right"
  )

# Mostrar plot
print(p)

# Guardar
dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE)
ggsave("outputs/figures/red_familias_aylwin_huidobro_bello_saavedra.png", p, 
       width = 20, height = 14, dpi = 300)
cat("\n✅ Guardado: outputs/figures/red_familias_aylwin_huidobro_bello_saavedra.png\n")

# Resumen de personas destacadas
cat("\n", strrep("=", 80), "\n")
cat("📊 PERSONAS DESTACADAS (por centralidad)\n")
cat(strrep("=", 80), "\n")

top_personas <- nodes %>%
  left_join(
    as_tibble(g_tbl) %>% select(url, degree, betweenness),
    by = c("url" = "url")
  ) %>%
  arrange(desc(degree.y)) %>%
  select(nombre, familia_grupo, degree.y, tiene_cargo) %>%
  head(15)

print(top_personas)
