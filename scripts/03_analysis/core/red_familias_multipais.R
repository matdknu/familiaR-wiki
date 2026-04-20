# ============================================================================
# red_familias_multipais.R
# Red de familias específicas multi-país con clusters por país
# Chile: Aylwin, García-Huidobro, Bello, Saavedra, Balmaceda
# Argentina: Otálora, Saavedra
# Colombia: Familia notable
# ============================================================================

library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggraph)
library(tidygraph)
library(igraph)
library(ggrepel)

cat("📊 Red Multi-País: Chile, Argentina, Colombia\n")
cat(strrep("=", 80), "\n")

# ============================================================================
# CARGAR DATOS
# ============================================================================

load_country_data <- function(country) {
  path <- paste0("data/processed/familias/", country, "/consolidado.csv")
  if (file.exists(path)) {
    df <- read_delim(path, delim = ";", show_col_types = FALSE, 
                     locale = locale(encoding = "UTF-8"))
    df$pais <- country
    return(df)
  }
  return(NULL)
}

chile_data <- load_country_data("chile")
argentina_data <- load_country_data("argentina")
colombia_data <- load_country_data("colombia")

cat("✅ Chile:", nrow(chile_data), "personas\n")
cat("✅ Argentina:", nrow(argentina_data), "personas\n")
cat("✅ Colombia:", nrow(colombia_data), "personas\n")

# ============================================================================
# FILTRAR FAMILIAS DE INTERÉS
# ============================================================================

# Chile: Aylwin, García-Huidobro, Bello, Saavedra, Balmaceda
familias_chile <- c("aylwin", "huidobro", "bello", "saavedra", "balmaceda", 
                    "garcía-huidobro", "garcia-huidobro")

chile_filtrado <- chile_data %>%
  filter(
    str_detect(tolower(familia), paste(familias_chile, collapse = "|")) |
    str_detect(tolower(categoria_origen), paste(familias_chile, collapse = "|")) |
    str_detect(tolower(nombre), paste(familias_chile, collapse = "|"))
  ) %>%
  filter(!is.na(url), url != "")

cat("\n✅ Chile filtrado:", nrow(chile_filtrado), "personas\n")

# Argentina: Otálora, Saavedra (conexión con Cornelio Saavedra)
familias_argentina <- c("otálora", "otalora", "saavedra")

argentina_filtrado <- argentina_data %>%
  filter(
    str_detect(tolower(familia), paste(familias_argentina, collapse = "|")) |
    str_detect(tolower(categoria_origen), paste(familias_argentina, collapse = "|")) |
    str_detect(tolower(nombre), paste(familias_argentina, collapse = "|"))
  ) %>%
  filter(!is.na(url), url != "")

cat("✅ Argentina filtrado:", nrow(argentina_filtrado), "personas\n")

# Colombia: Algunas familias políticas notables
familias_colombia <- c("restrepo", "lleras", "santos", "lópez", "ospina")

colombia_filtrado <- colombia_data %>%
  filter(
    str_detect(tolower(familia), paste(familias_colombia, collapse = "|")) |
    str_detect(tolower(categoria_origen), paste(familias_colombia, collapse = "|"))
  ) %>%
  filter(!is.na(url), url != "") %>%
  slice_head(n = 50)  # Limitar para visualización

cat("✅ Colombia filtrado:", nrow(colombia_filtrado), "personas\n")

# Combinar todos
all_data <- bind_rows(chile_filtrado, argentina_filtrado, colombia_filtrado)
cat("\n📊 Total combinado:", nrow(all_data), "personas\n")

# ============================================================================
# FUNCIONES AUXILIARES
# ============================================================================

extract_relation_urls <- function(relation_field) {
  if (is.na(relation_field) || relation_field == "") return(character())
  pattern <- "\\(https://es\\.wikipedia\\.org/wiki/([^)]+)\\)"
  matches <- str_match_all(relation_field, pattern)[[1]]
  if (nrow(matches) > 0) {
    return(paste0("https://es.wikipedia.org/wiki/", matches[, 2]))
  }
  return(character())
}

extract_familia_grupo <- function(nombre, familia, categoria_origen) {
  nombre_lower <- tolower(nombre)
  familia_lower <- tolower(familia)
  cat_lower <- tolower(categoria_origen)
  
  # Chile
  if (str_detect(familia_lower, "aylwin") || str_detect(cat_lower, "aylwin")) return("Aylwin")
  if (str_detect(familia_lower, "huidobro") || str_detect(cat_lower, "huidobro")) return("García-Huidobro")
  if (str_detect(familia_lower, "bello") || str_detect(cat_lower, "bello")) return("Bello")
  if (str_detect(familia_lower, "balmaceda") || str_detect(cat_lower, "balmaceda")) return("Balmaceda")
  
  # Argentina
  if (str_detect(familia_lower, "otálora|otalora") || str_detect(cat_lower, "otálora|otalora")) return("Otálora")
  if (str_detect(familia_lower, "saavedra") || str_detect(cat_lower, "saavedra")) return("Saavedra")
  
  # Colombia
  if (str_detect(familia_lower, "restrepo") || str_detect(cat_lower, "restrepo")) return("Restrepo")
  if (str_detect(familia_lower, "lleras") || str_detect(cat_lower, "lleras")) return("Lleras")
  if (str_detect(familia_lower, "santos") || str_detect(cat_lower, "santos")) return("Santos")
  if (str_detect(familia_lower, "lópez") || str_detect(cat_lower, "lópez")) return("López")
  if (str_detect(familia_lower, "ospina") || str_detect(cat_lower, "ospina")) return("Ospina")
  
  # Por nombre
  if (str_detect(nombre_lower, "saavedra")) return("Saavedra")
  if (str_detect(nombre_lower, "balmaceda")) return("Balmaceda")
  
  return("Otra")
}

# Aplicar clasificación
all_data <- all_data %>%
  rowwise() %>%
  mutate(
    familia_grupo = extract_familia_grupo(
      ifelse(is.na(nombre), "", nombre),
      ifelse(is.na(familia), "", familia),
      ifelse(is.na(categoria_origen), "", categoria_origen)
    )
  ) %>%
  ungroup()

cat("\n📊 Distribución por familia y país:\n")
print(all_data %>% count(pais, familia_grupo, sort = TRUE) %>% head(20))

# ============================================================================
# CONSTRUIR EDGES
# ============================================================================

cat("\n🔗 Extrayendo relaciones...\n")
edges_list <- list()

for (i in 1:nrow(all_data)) {
  source_url <- all_data$url[i]
  source_pais <- all_data$pais[i]
  source_familia <- all_data$familia_grupo[i]
  
  for (field in c("padres", "conyuge", "pareja", "hijos", "hermanos")) {
    if (field %in% colnames(all_data) && !is.na(all_data[[field]][i])) {
      target_urls <- extract_relation_urls(all_data[[field]][i])
      for (target_url in target_urls) {
        edges_list[[length(edges_list) + 1]] <- tibble(
          from = source_url,
          to = target_url,
          relation_type = field,
          from_pais = source_pais,
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
      all_data %>% select(url, nombre, pais, familia_grupo),
      by = c("to" = "url")
    ) %>%
    rename(to_nombre = nombre, to_pais = pais, to_familia = familia_grupo) %>%
    left_join(
      all_data %>% select(url, nombre),
      by = c("from" = "url")
    ) %>%
    rename(from_nombre = nombre)
  
  # Detectar conexiones transnacionales
  edges_enriched <- edges_enriched %>%
    mutate(
      es_transnacional = !is.na(to_pais) & from_pais != to_pais
    )
  
  cat("✅ Relaciones extraídas:", nrow(edges_enriched), "\n")
  cat("✅ Conexiones transnacionales:", sum(edges_enriched$es_transnacional, na.rm = TRUE), "\n")
} else {
  edges_enriched <- tibble()
}

# ============================================================================
# AÑADIR CONEXIÓN MANUAL: Cornelio Saavedra ↔ Otálora
# ============================================================================

# Buscar URLs de Cornelio Saavedra y María Saturnina Otálora
cornelio_urls <- all_data %>% 
  filter(str_detect(tolower(nombre), "cornelio saavedra")) %>% 
  pull(url)

saturnina_url <- all_data %>%
  filter(str_detect(tolower(nombre), "saturnina.*otálora|saturnina.*otalora")) %>%
  pull(url)

jose_otalora_url <- all_data %>%
  filter(str_detect(tolower(nombre), "josé antonio gregorio de otálora|jose antonio gregorio de otalora")) %>%
  pull(url)

# Añadir conexiones manuales si existen las personas
manual_edges <- tibble()

if (length(cornelio_urls) > 0 && length(saturnina_url) > 0) {
  manual_edges <- bind_rows(manual_edges, tibble(
    from = cornelio_urls[1],
    to = saturnina_url[1],
    relation_type = "conyuge",
    from_pais = "chile",
    from_familia = "Saavedra",
    to_nombre = "María Saturnina de Otálora",
    to_pais = "argentina",
    to_familia = "Otálora",
    from_nombre = "Cornelio Saavedra",
    es_transnacional = TRUE
  ))
  cat("✅ Añadida conexión manual: Cornelio Saavedra ↔ María Saturnina Otálora\n")
}

if (length(saturnina_url) > 0 && length(jose_otalora_url) > 0) {
  manual_edges <- bind_rows(manual_edges, tibble(
    from = saturnina_url[1],
    to = jose_otalora_url[1],
    relation_type = "padres",
    from_pais = "argentina",
    from_familia = "Otálora",
    to_nombre = "José Antonio Gregorio de Otálora",
    to_pais = "argentina", 
    to_familia = "Otálora",
    from_nombre = "María Saturnina de Otálora",
    es_transnacional = FALSE
  ))
}

if (nrow(manual_edges) > 0) {
  edges_enriched <- bind_rows(edges_enriched, manual_edges)
}

# ============================================================================
# CREAR NODOS
# ============================================================================

all_urls <- unique(c(edges_enriched$from, edges_enriched$to))
nodes <- tibble(url = all_urls) %>%
  left_join(
    all_data %>% select(url, nombre, pais, familia_grupo, cargos_politicos, ocupacion),
    by = "url"
  ) %>%
  mutate(
    nombre = ifelse(is.na(nombre), str_replace_all(str_extract(url, "(?<=wiki/).+"), "_", " "), nombre),
    pais = ifelse(is.na(pais), "desconocido", pais),
    familia_grupo = ifelse(is.na(familia_grupo), "Otra", familia_grupo),
    tiene_cargo = !is.na(cargos_politicos) & cargos_politicos != ""
  )

cat("\n✅ Nodos totales:", nrow(nodes), "\n")
cat("\n📊 Distribución por país:\n")
print(nodes %>% count(pais, sort = TRUE))

# ============================================================================
# CREAR GRAFO Y VISUALIZAR
# ============================================================================

# Filtrar edges válidos
edges_valid <- edges_enriched %>%
  filter(from %in% nodes$url, to %in% nodes$url) %>%
  mutate(es_transnacional = ifelse(is.na(es_transnacional), FALSE, es_transnacional))

g_tbl <- tbl_graph(nodes = nodes, edges = edges_valid, directed = FALSE) %>%
  activate(nodes) %>%
  mutate(
    degree = centrality_degree(mode = "all"),
    betweenness = centrality_betweenness()
  )

# Colores por país
pais_colors <- c(
  "chile" = "#0033A0",
  "argentina" = "#75AADB", 
  "colombia" = "#FCD116",
  "desconocido" = "gray70"
)

# Colores por familia
familia_colors <- c(
  "Aylwin" = "#1E88E5",
  "García-Huidobro" = "#D81B60",
  "Bello" = "#00897B",
  "Balmaceda" = "#43A047",
  "Saavedra" = "#FB8C00",
  "Otálora" = "#8E24AA",
  "Restrepo" = "#F4511E",
  "Lleras" = "#00ACC1",
  "Santos" = "#5E35B1",
  "López" = "#FFB300",
  "Ospina" = "#C0CA33",
  "Otra" = "gray60"
)

cat("\n🎨 Generando visualización con clusters por país...\n")

# Layout con agrupación por país
set.seed(42)

# Obtener layout base
layout_matrix <- create_layout(g_tbl, layout = "fr")

# Ajustar posiciones por país (separar clusters)
layout_df <- as_tibble(layout_matrix)

# Offsets por país
offsets <- list(
  "chile" = c(x = 0, y = 0),
  "argentina" = c(x = 25, y = 0),
  "colombia" = c(x = 12.5, y = 25),
  "desconocido" = c(x = 12.5, y = -15)
)

layout_df <- layout_df %>%
  mutate(
    x_adj = x + sapply(pais, function(p) offsets[[p]][["x"]]),
    y_adj = y + sapply(pais, function(p) offsets[[p]][["y"]])
  )

# Separar edges transnacionales
edges_trans <- edges_valid %>% filter(es_transnacional == TRUE)
edges_normal <- edges_valid %>% filter(is.na(es_transnacional) | es_transnacional == FALSE)

# Crear grafos separados
g_normal <- tbl_graph(nodes = nodes, edges = edges_normal, directed = FALSE)
g_trans <- tbl_graph(nodes = nodes, edges = edges_trans, directed = FALSE)

# Visualización principal
p_main <- ggraph(g_tbl, layout = "manual", x = layout_df$x_adj, y = layout_df$y_adj) +
  # Edges normales
  geom_edge_link(
    aes(color = relation_type),
    alpha = 0.4,
    linewidth = 0.5
  ) +
  scale_edge_color_manual(
    values = c(
      "padres" = "#66C2A5",
      "conyuge" = "#FC8D62",
      "pareja" = "#8DA0CB",
      "hijos" = "#E78AC3",
      "hermanos" = "#A6D854"
    ),
    name = "Tipo de relación"
  ) +
  # Nodos
  geom_node_point(
    aes(size = degree, fill = pais, color = familia_grupo, shape = tiene_cargo),
    stroke = 1.5
  ) +
  scale_fill_manual(values = pais_colors, name = "País") +
  scale_color_manual(values = familia_colors, name = "Familia") +
  scale_size_continuous(range = c(3, 12), name = "Conexiones") +
  scale_shape_manual(
    values = c("FALSE" = 21, "TRUE" = 24),
    labels = c("FALSE" = "Sin cargo", "TRUE" = "Con cargo"),
    name = "Cargo político"
  ) +
  # Labels para personas importantes
  geom_node_text(
    aes(label = ifelse(degree >= 3 | tiene_cargo | betweenness > 50, nombre, "")),
    size = 2.2,
    repel = TRUE,
    max.overlaps = 30,
    segment.color = "gray50",
    segment.size = 0.2
  ) +
  # Etiquetas de país
  annotate("text", x = 0, y = -12, label = "CHILE", size = 6, fontface = "bold", color = "#0033A0") +
  annotate("text", x = 25, y = -12, label = "ARGENTINA", size = 6, fontface = "bold", color = "#75AADB") +
  annotate("text", x = 12.5, y = 37, label = "COLOMBIA", size = 6, fontface = "bold", color = "#FCD116") +
  labs(
    title = "Red de Familias: Chile, Argentina, Colombia",
    subtitle = paste(
      "Familias: Aylwin, García-Huidobro, Bello, Balmaceda, Saavedra (Chile) |",
      "Otálora, Saavedra (Argentina) | Restrepo, Lleras, Santos (Colombia)\n",
      "Nodos:", nrow(nodes), "| Conexiones:", nrow(edges_valid),
      "| Transnacionales:", sum(edges_enriched$es_transnacional, na.rm = TRUE)
    ),
    caption = "Líneas rojas curvas = conexiones transnacionales | Triángulos = personas con cargos políticos"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    plot.caption = element_text(hjust = 0.5, size = 9, color = "gray50"),
    legend.position = "right",
    legend.box = "vertical"
  ) +
  guides(
    fill = guide_legend(override.aes = list(size = 5, shape = 21)),
    color = guide_legend(override.aes = list(size = 5)),
    size = guide_legend(override.aes = list(shape = 21))
  )

print(p_main)

# Guardar
dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE)
ggsave("outputs/figures/red_familias_multipais_clusters.png", p_main, 
       width = 24, height = 18, dpi = 300)
cat("\n✅ Guardado: outputs/figures/red_familias_multipais_clusters.png\n")

# ============================================================================
# RESUMEN DE CONEXIONES TRANSNACIONALES
# ============================================================================

cat("\n", strrep("=", 80), "\n")
cat("📊 CONEXIONES TRANSNACIONALES\n")
cat(strrep("=", 80), "\n")

conexiones_trans <- edges_enriched %>%
  filter(es_transnacional == TRUE) %>%
  select(from_nombre, from_pais, to_nombre, to_pais, relation_type)

if (nrow(conexiones_trans) > 0) {
  print(conexiones_trans)
} else {
  cat("No se encontraron conexiones transnacionales explícitas en los datos.\n")
  cat("Nota: La conexión Cornelio Saavedra - Otálora existe históricamente.\n")
}

# ============================================================================
# PERSONAS MÁS CENTRALES POR PAÍS
# ============================================================================

cat("\n", strrep("=", 80), "\n")
cat("📊 TOP 5 PERSONAS MÁS CONECTADAS POR PAÍS\n")
cat(strrep("=", 80), "\n")

top_por_pais <- as_tibble(g_tbl) %>%
  group_by(pais) %>%
  arrange(desc(degree)) %>%
  slice_head(n = 5) %>%
  select(nombre, pais, familia_grupo, degree, tiene_cargo)

print(top_por_pais)
