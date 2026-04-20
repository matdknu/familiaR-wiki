# ============================================================================
# red_familias_multipais_v2.R
# Visualización profesional de redes familiares multi-país
# ============================================================================

library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggraph)
library(tidygraph)
library(igraph)
library(ggrepel)
library(scales)

cat("📊 Red Multi-País: Visualización Profesional\n")
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

# Argentina: Otálora, Saavedra
familias_argentina <- c("otálora", "otalora", "saavedra")

argentina_filtrado <- argentina_data %>%
  filter(
    str_detect(tolower(familia), paste(familias_argentina, collapse = "|")) |
    str_detect(tolower(categoria_origen), paste(familias_argentina, collapse = "|")) |
    str_detect(tolower(nombre), paste(familias_argentina, collapse = "|"))
  ) %>%
  filter(!is.na(url), url != "")

# Colombia: Familias políticas notables
familias_colombia <- c("restrepo", "lleras", "santos", "lópez", "ospina")

colombia_filtrado <- colombia_data %>%
  filter(
    str_detect(tolower(familia), paste(familias_colombia, collapse = "|")) |
    str_detect(tolower(categoria_origen), paste(familias_colombia, collapse = "|"))
  ) %>%
  filter(!is.na(url), url != "") %>%
  slice_head(n = 50)

# Combinar
all_data <- bind_rows(chile_filtrado, argentina_filtrado, colombia_filtrado)

cat("✅ Chile:", nrow(chile_filtrado), "| Argentina:", nrow(argentina_filtrado), 
    "| Colombia:", nrow(colombia_filtrado), "\n")

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
  
  if (str_detect(familia_lower, "aylwin") || str_detect(cat_lower, "aylwin")) return("Aylwin")
  if (str_detect(familia_lower, "huidobro") || str_detect(cat_lower, "huidobro")) return("García-Huidobro")
  if (str_detect(familia_lower, "bello") || str_detect(cat_lower, "bello")) return("Bello")
  if (str_detect(familia_lower, "balmaceda") || str_detect(cat_lower, "balmaceda")) return("Balmaceda")
  if (str_detect(familia_lower, "otálora|otalora") || str_detect(cat_lower, "otálora|otalora")) return("Otálora")
  if (str_detect(familia_lower, "saavedra") || str_detect(cat_lower, "saavedra")) return("Saavedra")
  if (str_detect(familia_lower, "restrepo") || str_detect(cat_lower, "restrepo")) return("Restrepo")
  if (str_detect(familia_lower, "lleras") || str_detect(cat_lower, "lleras")) return("Lleras")
  if (str_detect(familia_lower, "santos") || str_detect(cat_lower, "santos")) return("Santos")
  if (str_detect(familia_lower, "lópez") || str_detect(cat_lower, "lópez")) return("López")
  if (str_detect(familia_lower, "ospina") || str_detect(cat_lower, "ospina")) return("Ospina")
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

# ============================================================================
# CONSTRUIR EDGES
# ============================================================================

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

edges_all <- bind_rows(edges_list)

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
  rename(from_nombre = nombre) %>%
  mutate(
    es_transnacional = !is.na(to_pais) & from_pais != to_pais
  )

# Añadir conexión manual Cornelio Saavedra ↔ Otálora
cornelio_url <- all_data %>% 
  filter(str_detect(tolower(nombre), "cornelio saavedra"), pais == "chile") %>% 
  slice_head(n = 1) %>%
  pull(url)

saturnina_url <- all_data %>%
  filter(str_detect(tolower(nombre), "saturnina.*otálora|saturnina.*otalora")) %>%
  pull(url)

if (length(cornelio_url) > 0 && length(saturnina_url) > 0) {
  edges_enriched <- bind_rows(edges_enriched, tibble(
    from = cornelio_url[1],
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
  cat("✅ Conexión transnacional: Cornelio Saavedra ↔ María Saturnina Otálora\n")
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
    nombre = ifelse(is.na(nombre), 
                    URLdecode(str_replace_all(str_extract(url, "(?<=wiki/).+"), "_", " ")), 
                    nombre),
    pais = ifelse(is.na(pais), "otro", pais),
    familia_grupo = ifelse(is.na(familia_grupo), "Otra", familia_grupo),
    tiene_cargo = !is.na(cargos_politicos) & cargos_politicos != ""
  ) %>%
  # Eliminar nodos "otro" para limpieza visual
  filter(pais != "otro")

# Filtrar edges para que solo incluyan nodos válidos
edges_valid <- edges_enriched %>%
  filter(from %in% nodes$url, to %in% nodes$url) %>%
  mutate(es_transnacional = ifelse(is.na(es_transnacional), FALSE, es_transnacional))

cat("✅ Nodos:", nrow(nodes), "| Edges:", nrow(edges_valid), "\n")

# ============================================================================
# CREAR GRAFO CON LAYOUT POR PAÍS
# ============================================================================

g_tbl <- tbl_graph(nodes = nodes, edges = edges_valid, directed = FALSE) %>%
  activate(nodes) %>%
  mutate(
    degree = centrality_degree(mode = "all"),
    betweenness = centrality_betweenness()
  )

# Definir centros de países (distribución más amplia)
pais_centers <- list(
  "chile" = c(x = -20, y = 0),
  "argentina" = c(x = 20, y = 0),
  "colombia" = c(x = 0, y = 25)
)

# Layout FR por subgrafo de país y combinar
set.seed(123)
nodes_df <- as_tibble(g_tbl)

layout_coords <- data.frame(x = numeric(nrow(nodes_df)), y = numeric(nrow(nodes_df)))

for (p in unique(nodes_df$pais)) {
  idx <- which(nodes_df$pais == p)
  if (length(idx) > 1) {
    subg <- induced_subgraph(as.igraph(g_tbl), idx)
    sub_layout <- layout_with_fr(subg, niter = 500)
    # Escalar y centrar
    sub_layout <- scale(sub_layout) * 8
    layout_coords$x[idx] <- sub_layout[, 1] + pais_centers[[p]]["x"]
    layout_coords$y[idx] <- sub_layout[, 2] + pais_centers[[p]]["y"]
  } else if (length(idx) == 1) {
    layout_coords$x[idx] <- pais_centers[[p]]["x"]
    layout_coords$y[idx] <- pais_centers[[p]]["y"]
  }
}

# ============================================================================
# PALETAS DE COLORES
# ============================================================================

pais_fills <- c(
  "chile" = "#1E3A5F",
  "argentina" = "#5DADE2",
  "colombia" = "#F4D03F"
)

familia_colors <- c(
  "Aylwin" = "#2980B9",
  "García-Huidobro" = "#E74C3C",
  "Bello" = "#27AE60",
  "Balmaceda" = "#16A085",
  "Saavedra" = "#E67E22",
  "Otálora" = "#9B59B6",
  "Restrepo" = "#C0392B",
  "Lleras" = "#2ECC71",
  "Santos" = "#3498DB",
  "López" = "#F39C12",
  "Ospina" = "#1ABC9C",
  "Otra" = "#95A5A6"
)

relation_colors <- c(
  "padres" = "#3498DB",
  "conyuge" = "#E74C3C",
  "pareja" = "#9B59B6",
  "hijos" = "#2ECC71",
  "hermanos" = "#F39C12"
)

# ============================================================================
# VISUALIZACIÓN PRINCIPAL
# ============================================================================

cat("🎨 Generando visualización...\n")

# Estadísticas para subtítulo
n_chile <- sum(nodes_df$pais == "chile")
n_argentina <- sum(nodes_df$pais == "argentina")
n_colombia <- sum(nodes_df$pais == "colombia")
n_trans <- sum(edges_valid$es_transnacional, na.rm = TRUE)

p_main <- ggraph(g_tbl, layout = "manual", x = layout_coords$x, y = layout_coords$y) +
  # Fondo con círculos de país
  annotate("point", x = -20, y = 0, size = 85, color = "#1E3A5F", alpha = 0.08) +
  annotate("point", x = 20, y = 0, size = 70, color = "#5DADE2", alpha = 0.08) +
  annotate("point", x = 0, y = 25, size = 70, color = "#F4D03F", alpha = 0.08) +
  
  # Edges normales (intra-país)
  geom_edge_link(
    aes(color = relation_type, 
        filter = !es_transnacional),
    alpha = 0.35,
    width = 0.4
  ) +
  
  # Edges transnacionales (curvas destacadas)
  geom_edge_arc(
    aes(filter = es_transnacional),
    color = "#C0392B",
    alpha = 0.9,
    width = 1.8,
    strength = 0.5
  ) +
  
  scale_edge_color_manual(
    values = relation_colors,
    name = "Vínculo familiar",
    labels = c("padres" = "Padres", "conyuge" = "Cónyuge", 
               "pareja" = "Pareja", "hijos" = "Hijos", "hermanos" = "Hermanos")
  ) +
  
  # Nodos
  geom_node_point(
    aes(fill = pais, 
        color = familia_grupo,
        size = degree,
        shape = tiene_cargo),
    stroke = 1.8,
    alpha = 0.9
  ) +
  
  scale_fill_manual(
    values = pais_fills, 
    name = "País",
    labels = c("chile" = "Chile", "argentina" = "Argentina", "colombia" = "Colombia")
  ) +
  scale_color_manual(values = familia_colors, name = "Familia") +
  scale_size_continuous(range = c(4, 16), name = "Conexiones", breaks = c(2, 5, 8, 10)) +
  scale_shape_manual(
    values = c("FALSE" = 21, "TRUE" = 24),
    labels = c("FALSE" = "Sin cargo", "TRUE" = "Cargo político"),
    name = "Rol"
  ) +
  
  # Labels para personas importantes
  geom_node_text(
    aes(label = ifelse(degree >= 4 | tiene_cargo | betweenness > 100, nombre, "")),
    size = 2.8,
    fontface = "bold",
    color = "gray20",
    repel = TRUE,
    max.overlaps = 35,
    segment.color = "gray60",
    segment.size = 0.3,
    segment.alpha = 0.6,
    box.padding = 0.4,
    point.padding = 0.3
  ) +
  
  # Etiquetas de país
  annotate("text", x = -20, y = -18, label = "CHILE", 
           size = 8, fontface = "bold", color = "#1E3A5F", alpha = 0.9) +
  annotate("text", x = -20, y = -20.5, 
           label = paste0("Aylwin · García-Huidobro · Bello\nBalmaceda · Saavedra"),
           size = 3, color = "#1E3A5F", alpha = 0.7, lineheight = 0.9) +
  
  annotate("text", x = 20, y = -15, label = "ARGENTINA", 
           size = 8, fontface = "bold", color = "#2471A3", alpha = 0.9) +
  annotate("text", x = 20, y = -17.5, 
           label = "Otálora · Saavedra",
           size = 3, color = "#2471A3", alpha = 0.7) +
  
  annotate("text", x = 0, y = 40, label = "COLOMBIA", 
           size = 8, fontface = "bold", color = "#B7950B", alpha = 0.9) +
  annotate("text", x = 0, y = 37.5, 
           label = "López · Lleras · Ospina",
           size = 3, color = "#B7950B", alpha = 0.7) +
  
  # Indicador de conexión transnacional
  annotate("segment", x = -5, xend = 5, y = -25, yend = -25, 
           color = "#C0392B", linewidth = 1.5, alpha = 0.8) +
  annotate("text", x = 0, y = -27, 
           label = "Conexión transnacional",
           size = 3, color = "#C0392B", fontface = "italic") +
  
  labs(
    title = "Redes Familiares de Élites Latinoamericanas",
    subtitle = paste0(
      "Chile (", n_chile, " personas) · Argentina (", n_argentina, 
      " personas) · Colombia (", n_colombia, " personas)\n",
      "Total: ", nrow(nodes), " nodos · ", nrow(edges_valid), " conexiones · ",
      n_trans, " vínculos transnacionales"
    ),
    caption = paste0(
      "Fuente: Wikipedia | Familias destacadas: Aylwin, García-Huidobro, Bello, Balmaceda (Chile); ",
      "Otálora, Saavedra (Argentina); López, Lleras, Ospina (Colombia)\n",
      "Conexión histórica: Cornelio Saavedra (prócer chileno) casado con María Saturnina de Otálora (Argentina, 1801)"
    )
  ) +
  
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#FAFAFA", color = NA),
    panel.background = element_rect(fill = "#FAFAFA", color = NA),
    plot.title = element_text(
      hjust = 0.5, face = "bold", size = 22, color = "#2C3E50",
      margin = margin(t = 15, b = 5)
    ),
    plot.subtitle = element_text(
      hjust = 0.5, size = 12, color = "#5D6D7E",
      margin = margin(b = 10), lineheight = 1.2
    ),
    plot.caption = element_text(
      hjust = 0.5, size = 9, color = "#7F8C8D",
      margin = margin(t = 15), lineheight = 1.3
    ),
    legend.position = "right",
    legend.box = "vertical",
    legend.title = element_text(face = "bold", size = 10, color = "#2C3E50"),
    legend.text = element_text(size = 9, color = "#5D6D7E"),
    legend.key.size = unit(0.8, "cm"),
    legend.spacing.y = unit(0.3, "cm"),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  guides(
    fill = guide_legend(override.aes = list(size = 6, shape = 21, stroke = 0), order = 1),
    color = guide_legend(override.aes = list(size = 4), order = 2, ncol = 2),
    size = guide_legend(override.aes = list(shape = 21, fill = "gray70"), order = 3),
    shape = guide_legend(override.aes = list(size = 5, fill = "gray70"), order = 4)
  )

# Guardar
dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE)
ggsave("outputs/figures/red_familias_latam.png", p_main, 
       width = 22, height = 16, dpi = 300, bg = "#FAFAFA")

cat("\n✅ Guardado: outputs/figures/red_familias_latam.png\n")

# También guardar versión para README (más pequeña)
ggsave("outputs/figures/red_familias_latam_readme.png", p_main, 
       width = 16, height = 12, dpi = 200, bg = "#FAFAFA")

cat("✅ Guardado versión README: outputs/figures/red_familias_latam_readme.png\n")

# ============================================================================
# RESUMEN FINAL
# ============================================================================

cat("\n", strrep("=", 80), "\n")
cat("📊 RESUMEN DE LA RED\n")
cat(strrep("=", 80), "\n")
cat("Nodos por país:\n")
print(nodes_df %>% count(pais, name = "personas") %>% arrange(desc(personas)))

cat("\nFamilias más representadas:\n
")
print(nodes_df %>% count(familia_grupo, name = "miembros") %>% arrange(desc(miembros)) %>% head(10))

cat("\nPersonas más centrales:\n")
print(nodes_df %>% arrange(desc(degree)) %>% select(nombre, pais, familia_grupo, degree) %>% head(10))
