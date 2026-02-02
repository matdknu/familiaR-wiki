# ============================================================================
# red_familias_clusters.R
# Red familiar con CLUSTERS POR FAMILIA
# 
# NUEVA LÓGICA:
# - Los miembros de cada familia siempre están agrupados en un cluster
# - Se agregan "edges de cohesión familiar" (invisibles en plot pero usados para layout)
# - Las conexiones inter-familiares (Bello-Edwards, etc.) son edges reales
# ============================================================================

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(ggraph)
library(tidygraph)
library(igraph)
library(ggrepel)
library(scales)

cat("╔══════════════════════════════════════════════════════════════════════╗\n")
cat("║     RED FAMILIAR CON CLUSTERS POR FAMILIA                           ║\n")
cat("╚══════════════════════════════════════════════════════════════════════╝\n\n")

# ============================================================================
# CARGAR DATOS
# ============================================================================

cat("📖 Cargando datos...\n")

# Cargar consolidado LATAM
latam_path <- "data/processed/familias/_CONSOLIDADO_familias_latam.csv"
if (!file.exists(latam_path)) {
  stop("No se encontró el archivo consolidado LATAM")
}

all_data <- read_delim(latam_path, delim = ";", show_col_types = FALSE,
                       locale = locale(encoding = "UTF-8"))

cat("   Total personas en LATAM:", nrow(all_data), "\n")

# ============================================================================
# FILTRAR FAMILIAS DE INTERÉS (configurable)
# ============================================================================

# Familias a incluir por país
familias_config <- list(
  chile = c("aylwin", "huidobro", "bello", "saavedra", "balmaceda", 
            "garcía-huidobro", "garcia-huidobro", "edwards", "alessandri"),
  argentina = c("otálora", "otalora", "saavedra", "anchorena"),
  colombia = c("restrepo", "lleras", "santos", "lópez", "ospina"),
  venezuela = c("tovar", "toro", "bolívar", "bolivar", "machado", "zuloaga", "planas")
)

# Función para filtrar datos
filter_families <- function(df, country, families) {
  pattern <- paste(families, collapse = "|")
  df %>%
    filter(pais_origen == country | pais == country) %>%
    filter(
      str_detect(tolower(coalesce(familia, "")), pattern) |
      str_detect(tolower(coalesce(categoria_origen, "")), pattern) |
      str_detect(tolower(coalesce(nombre, "")), pattern)
    ) %>%
    filter(!is.na(url), url != "")
}

# Aplicar filtros
chile_data <- filter_families(all_data, "chile", familias_config$chile)
argentina_data <- filter_families(all_data, "argentina", familias_config$argentina)
colombia_data <- filter_families(all_data, "colombia", familias_config$colombia)
venezuela_data <- filter_families(all_data, "venezuela", familias_config$venezuela)

# Combinar
filtered_data <- bind_rows(
  chile_data %>% mutate(pais = "chile"),
  argentina_data %>% mutate(pais = "argentina"),
  colombia_data %>% mutate(pais = "colombia"),
  venezuela_data %>% mutate(pais = "venezuela")
) %>%
  distinct(url, .keep_all = TRUE)

cat("\n📊 Datos filtrados:\n")
cat("   Chile:", nrow(chile_data), "| Argentina:", nrow(argentina_data), "\n")
cat("   Colombia:", nrow(colombia_data), "| Venezuela:", nrow(venezuela_data), "\n")
cat("   Total:", nrow(filtered_data), "\n")

# ============================================================================
# CLASIFICAR FAMILIAS
# ============================================================================

extract_familia_grupo <- function(nombre, familia, categoria_origen) {
  nombre_lower <- tolower(coalesce(nombre, ""))
  familia_lower <- tolower(coalesce(familia, ""))
  cat_lower <- tolower(coalesce(categoria_origen, ""))
  combined <- paste(nombre_lower, familia_lower, cat_lower)
  
  # Chile
  if (str_detect(combined, "aylwin")) return("Aylwin")
  if (str_detect(combined, "huidobro|garcía-huidobro|garcia-huidobro")) return("García-Huidobro")
  if (str_detect(combined, "bello")) return("Bello")
  if (str_detect(combined, "balmaceda")) return("Balmaceda")
  if (str_detect(combined, "edwards")) return("Edwards")
  if (str_detect(combined, "alessandri")) return("Alessandri")
  if (str_detect(combined, "saavedra")) return("Saavedra")
  
  # Argentina
  if (str_detect(combined, "otálora|otalora")) return("Otálora")
  if (str_detect(combined, "anchorena")) return("Anchorena")
  
  # Colombia
  if (str_detect(combined, "restrepo")) return("Restrepo")
  if (str_detect(combined, "lleras")) return("Lleras")
  if (str_detect(combined, "santos")) return("Santos")
  if (str_detect(combined, "lópez")) return("López")
  if (str_detect(combined, "ospina")) return("Ospina")
  
  # Venezuela
  if (str_detect(combined, "tovar")) return("Tovar")
  if (str_detect(combined, "toro(?!nis)")) return("Toro")
  if (str_detect(combined, "bolívar|bolivar")) return("Bolívar")
  if (str_detect(combined, "machado")) return("Machado")
  if (str_detect(combined, "zuloaga")) return("Zuloaga")
  if (str_detect(combined, "planas")) return("Planas")
  
  return("Otra")
}

filtered_data <- filtered_data %>%
  rowwise() %>%
  mutate(
    familia_grupo = extract_familia_grupo(nombre, familia, categoria_origen)
  ) %>%
  ungroup() %>%
  filter(familia_grupo != "Otra")  # Excluir "Otra" para claridad

cat("\n📋 Familias identificadas:\n")
print(filtered_data %>% count(familia_grupo, pais) %>% arrange(desc(n)))

# ============================================================================
# CREAR EDGES REALES (relaciones familiares documentadas)
# ============================================================================

cat("\n🔗 Construyendo edges de relaciones documentadas...\n")

extract_relation_urls <- function(relation_field) {
  if (is.na(relation_field) || relation_field == "") return(character())
  pattern <- "\\(https://es\\.wikipedia\\.org/wiki/([^)]+)\\)"
  matches <- str_match_all(relation_field, pattern)[[1]]
  if (nrow(matches) > 0) {
    return(paste0("https://es.wikipedia.org/wiki/", matches[, 2]))
  }
  return(character())
}

edges_real <- list()

for (i in 1:nrow(filtered_data)) {
  source_url <- filtered_data$url[i]
  source_familia <- filtered_data$familia_grupo[i]
  
  for (field in c("padres", "conyuge", "pareja", "hijos", "hermanos",
                  "perfiles_relacionados_padres", "perfiles_relacionados_conyuge",
                  "perfiles_relacionados_hijos")) {
    if (field %in% colnames(filtered_data) && !is.na(filtered_data[[field]][i])) {
      target_urls <- extract_relation_urls(filtered_data[[field]][i])
      for (target_url in target_urls) {
        relation_type <- str_replace(field, "perfiles_relacionados_", "")
        edges_real[[length(edges_real) + 1]] <- tibble(
          from = source_url,
          to = target_url,
          relation_type = relation_type,
          edge_type = "real"
        )
      }
    }
  }
}

edges_df <- bind_rows(edges_real) %>%
  distinct(from, to, .keep_all = TRUE)

cat("   Edges reales encontrados:", nrow(edges_df), "\n")

# ============================================================================
# CREAR EDGES DE COHESIÓN FAMILIAR (mantienen juntos los clusters)
# ============================================================================

cat("\n🔗 Creando edges de cohesión familiar (para clustering)...\n")

edges_cohesion <- list()

for (fam in unique(filtered_data$familia_grupo)) {
  miembros <- filtered_data %>% filter(familia_grupo == fam) %>% pull(url)
  
  if (length(miembros) > 1) {
    # Crear un nodo central virtual por familia o conectar en cadena
    # Opción: conectar cada miembro con el siguiente (cadena)
    for (i in 1:(length(miembros) - 1)) {
      edges_cohesion[[length(edges_cohesion) + 1]] <- tibble(
        from = miembros[i],
        to = miembros[i + 1],
        relation_type = "cohesion_familiar",
        edge_type = "cohesion"
      )
    }
    # Cerrar el ciclo para mejor cohesión
    if (length(miembros) > 2) {
      edges_cohesion[[length(edges_cohesion) + 1]] <- tibble(
        from = miembros[length(miembros)],
        to = miembros[1],
        relation_type = "cohesion_familiar",
        edge_type = "cohesion"
      )
    }
  }
}

edges_cohesion_df <- bind_rows(edges_cohesion)
cat("   Edges de cohesión creados:", nrow(edges_cohesion_df), "\n")

# Combinar todos los edges
all_edges <- bind_rows(edges_df, edges_cohesion_df)

# ============================================================================
# CREAR NODOS
# ============================================================================

cat("\n📍 Creando nodos...\n")

all_urls <- unique(c(all_edges$from, all_edges$to))

nodes <- tibble(url = all_urls) %>%
  left_join(
    filtered_data %>% select(url, nombre, pais, familia_grupo, cargos_politicos, ocupacion),
    by = "url"
  ) %>%
  filter(!is.na(familia_grupo)) %>%  # Solo nodos con familia conocida
  mutate(
    nombre = ifelse(is.na(nombre), 
                    URLdecode(str_replace_all(str_extract(url, "(?<=wiki/).+"), "_", " ")), 
                    nombre),
    pais = ifelse(is.na(pais), "otro", pais),
    tiene_cargo = !is.na(cargos_politicos) & cargos_politicos != ""
  )

# Filtrar edges para nodos válidos
edges_final <- all_edges %>%
  filter(from %in% nodes$url, to %in% nodes$url)

# Enriquecer edges con info de familia
edges_final <- edges_final %>%
  left_join(nodes %>% select(url, familia_grupo), by = c("from" = "url")) %>%
  rename(from_familia = familia_grupo) %>%
  left_join(nodes %>% select(url, familia_grupo), by = c("to" = "url")) %>%
  rename(to_familia = familia_grupo) %>%
  mutate(
    es_interfamiliar = from_familia != to_familia & edge_type == "real"
  )

cat("   Nodos:", nrow(nodes), "\n")
cat("   Edges totales:", nrow(edges_final), "\n")
cat("   Edges reales:", sum(edges_final$edge_type == "real"), "\n")
cat("   Edges cohesión:", sum(edges_final$edge_type == "cohesion"), "\n")
cat("   Edges inter-familiares:", sum(edges_final$es_interfamiliar, na.rm = TRUE), "\n")

# ============================================================================
# CREAR GRAFO Y CALCULAR LAYOUT POR FAMILIA
# ============================================================================

cat("\n📐 Calculando layout por clusters familiares...\n")

g <- graph_from_data_frame(edges_final, directed = FALSE, vertices = nodes)

# Usar el campo familia_grupo para agrupar
V(g)$familia_grupo <- nodes$familia_grupo
V(g)$pais <- nodes$pais

# Calcular posiciones de clusters
familia_counts <- nodes %>% count(familia_grupo, pais) %>% arrange(pais, desc(n))

# Asignar posiciones a cada familia (en círculo por país)
set.seed(42)

# Centros de país
pais_centers <- list(
  chile = c(x = 0, y = 0),
  argentina = c(x = 60, y = 0),
  colombia = c(x = 30, y = 50),
  venezuela = c(x = 90, y = 50)
)

pais_radius <- 25  # Radio del círculo de familias alrededor del centro del país

# Calcular posición de cada familia
familia_positions <- list()

for (p in names(pais_centers)) {
  familias_pais <- familia_counts %>% filter(pais == p) %>% pull(familia_grupo)
  n_familias <- length(familias_pais)
  
  if (n_familias > 0) {
    angles <- seq(0, 2 * pi, length.out = n_familias + 1)[1:n_familias]
    for (i in seq_along(familias_pais)) {
      fam <- familias_pais[i]
      familia_positions[[fam]] <- c(
        x = pais_centers[[p]]["x"] + pais_radius * cos(angles[i]),
        y = pais_centers[[p]]["y"] + pais_radius * sin(angles[i])
      )
    }
  }
}

# Calcular layout dentro de cada familia usando FR
layout_final <- matrix(0, nrow = vcount(g), ncol = 2)
colnames(layout_final) <- c("x", "y")

for (fam in unique(V(g)$familia_grupo)) {
  idx <- which(V(g)$familia_grupo == fam)
  
  if (length(idx) > 1) {
    # Subgrafo de la familia
    subg <- induced_subgraph(g, idx)
    sub_layout <- layout_with_fr(subg, niter = 500)
    
    # Escalar el layout
    if (nrow(sub_layout) > 1 && sd(sub_layout[,1]) > 0) {
      sub_layout <- scale(sub_layout) * 6  # Radio del cluster familiar
    } else {
      sub_layout <- sub_layout * 0  # Si solo hay un punto o todos iguales
    }
    
    # Reemplazar NA con 0
    sub_layout[is.na(sub_layout)] <- 0
    
    fam_center <- familia_positions[[fam]]
    if (!is.null(fam_center)) {
      layout_final[idx, 1] <- sub_layout[, 1] + fam_center["x"]
      layout_final[idx, 2] <- sub_layout[, 2] + fam_center["y"]
    } else {
      # Si no hay posición definida, poner en el centro
      layout_final[idx, 1] <- sub_layout[, 1] + 50
      layout_final[idx, 2] <- sub_layout[, 2] + 25
    }
  } else if (length(idx) == 1) {
    fam_center <- familia_positions[[fam]]
    if (!is.null(fam_center)) {
      layout_final[idx, 1] <- fam_center["x"]
      layout_final[idx, 2] <- fam_center["y"]
    } else {
      layout_final[idx, 1] <- 50
      layout_final[idx, 2] <- 25
    }
  }
}

# Asegurar que no hay NA en layout
layout_final[is.na(layout_final)] <- 0

# Convertir a tidygraph
g_tbl <- as_tbl_graph(g) %>%
  activate(nodes) %>%
  mutate(
    x = layout_final[, 1],
    y = layout_final[, 2],
    degree = centrality_degree(mode = "all")
  )

# ============================================================================
# PALETAS DE COLORES
# ============================================================================

familia_colors <- c(
  # Chile
  "Aylwin" = "#2980B9",
  "García-Huidobro" = "#E74C3C",
  "Bello" = "#27AE60",
  "Balmaceda" = "#16A085",
  "Edwards" = "#8E44AD",
  "Alessandri" = "#D35400",
  "Saavedra" = "#E67E22",
  # Argentina
  "Otálora" = "#9B59B6",
  "Anchorena" = "#3498DB",
  # Colombia
  "Restrepo" = "#C0392B",
  "Lleras" = "#2ECC71",
  "Santos" = "#3498DB",
  "López" = "#F39C12",
  "Ospina" = "#1ABC9C",
  # Venezuela
  "Tovar" = "#E91E63",
  "Toro" = "#9C27B0",
  "Bolívar" = "#FF5722",
  "Machado" = "#00BCD4",
  "Zuloaga" = "#4CAF50",
  "Planas" = "#FF9800",
  # Otros
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
# VISUALIZACIÓN
# ============================================================================

cat("\n🎨 Generando visualización...\n")

# Agregar coordenadas a los nodos
nodes$x <- layout_final[, 1]
nodes$y <- layout_final[, 2]

# Obtener edges para ggraph (solo los reales para visualizar)
edges_plot <- edges_final %>%
  filter(edge_type == "real") %>%
  # Agregar coordenadas from/to
  left_join(nodes %>% select(url, x, y), by = c("from" = "url")) %>%
  rename(x_from = x, y_from = y) %>%
  left_join(nodes %>% select(url, x, y), by = c("to" = "url")) %>%
  rename(x_to = x, y_to = y) %>%
  filter(!is.na(x_from) & !is.na(x_to))

# Calcular grado
node_degrees <- edges_plot %>%
  select(from, to) %>%
  pivot_longer(cols = c(from, to), values_to = "url") %>%
  count(url, name = "degree")

nodes <- nodes %>%
  left_join(node_degrees, by = "url") %>%
  mutate(degree = coalesce(degree, 0))

# Estadísticas
n_por_pais <- nodes %>% count(pais)
n_interfam <- sum(edges_plot$es_interfamiliar, na.rm = TRUE)

# Usar ggplot2 base en vez de ggraph para más control
p_main <- ggplot() +
  
  # Círculos de fondo por país
  annotate("point", x = 0, y = 0, size = 100, color = "#1E3A5F", alpha = 0.06) +
  annotate("point", x = 60, y = 0, size = 80, color = "#5DADE2", alpha = 0.06) +
  annotate("point", x = 30, y = 50, size = 80, color = "#F4D03F", alpha = 0.06) +
  annotate("point", x = 90, y = 50, size = 80, color = "#E74C3C", alpha = 0.06) +
  
  # Edges intra-familiares (más tenues)
  geom_segment(
    data = edges_plot %>% filter(!es_interfamiliar),
    aes(x = x_from, y = y_from, xend = x_to, yend = y_to, color = relation_type),
    alpha = 0.4,
    linewidth = 0.4
  ) +
  
  # Edges inter-familiares (destacados en rojo)
  geom_segment(
    data = edges_plot %>% filter(es_interfamiliar),
    aes(x = x_from, y = y_from, xend = x_to, yend = y_to),
    color = "#C0392B",
    alpha = 0.85,
    linewidth = 1.2
  ) +
  
  scale_color_manual(
    values = relation_colors,
    name = "Tipo de relación",
    labels = c("padres" = "Padres", "conyuge" = "Cónyuge", 
               "pareja" = "Pareja", "hijos" = "Hijos", "hermanos" = "Hermanos"),
    na.value = "gray70"
  ) +
  
  # Nodos
  geom_point(
    data = nodes,
    aes(x = x, y = y, fill = familia_grupo, size = degree + 2),
    shape = 21,
    color = "white",
    stroke = 0.7,
    alpha = 0.9
  ) +
  
  scale_fill_manual(values = familia_colors, name = "Familia") +
  scale_size_continuous(range = c(3, 12), name = "Conexiones", guide = "none") +
  
  # Labels para nodos importantes
  geom_text_repel(
    data = nodes %>% filter(degree >= 2 | tiene_cargo),
    aes(x = x, y = y, label = str_trunc(nombre, 20)),
    size = 2.3,
    fontface = "bold",
    color = "gray25",
    max.overlaps = 40,
    segment.color = "gray60",
    segment.size = 0.2,
    box.padding = 0.25,
    point.padding = 0.2,
    min.segment.length = 0.3
  ) +
  
  # Etiquetas de país
  annotate("text", x = 0, y = -35, label = "CHILE",
           size = 6, fontface = "bold", color = "#1E3A5F", alpha = 0.9) +
  annotate("text", x = 60, y = -35, label = "ARGENTINA",
           size = 6, fontface = "bold", color = "#2471A3", alpha = 0.9) +
  annotate("text", x = 30, y = 80, label = "COLOMBIA",
           size = 6, fontface = "bold", color = "#B7950B", alpha = 0.9) +
  annotate("text", x = 90, y = 80, label = "VENEZUELA",
           size = 6, fontface = "bold", color = "#C0392B", alpha = 0.9) +
  
  # Leyenda de conexión inter-familiar
  annotate("segment", x = 35, xend = 55, y = -40, yend = -40,
           color = "#C0392B", linewidth = 1.5) +
  annotate("text", x = 45, y = -43, label = "Conexión inter-familiar",
           size = 3, color = "#C0392B", fontface = "italic") +
  
  labs(
    title = "Redes Familiares de Élites Latinoamericanas",
    subtitle = paste0(
      "Cada cluster agrupa a los miembros de una familia • ",
      "Conexiones inter-familiares destacadas en rojo\n",
      "Total: ", nrow(nodes), " personas de ", 
      length(unique(nodes$familia_grupo)), " familias • ",
      n_interfam, " conexiones inter-familiares"
    ),
    caption = paste0(
      "Fuente: Wikipedia | Familias incluidas: ",
      paste(unique(nodes$familia_grupo), collapse = ", ")
    )
  ) +
  
  coord_fixed(ratio = 1) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#FAFAFA", color = NA),
    panel.background = element_rect(fill = "#FAFAFA", color = NA),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18, color = "#2C3E50",
                              margin = margin(t = 15, b = 5)),
    plot.subtitle = element_text(hjust = 0.5, size = 10, color = "#5D6D7E",
                                 margin = margin(b = 10), lineheight = 1.2),
    plot.caption = element_text(hjust = 0.5, size = 8, color = "#7F8C8D",
                                margin = margin(t = 15)),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 9),
    legend.text = element_text(size = 7),
    plot.margin = margin(15, 15, 15, 15)
  ) +
  guides(
    fill = guide_legend(override.aes = list(size = 4), ncol = 2, order = 1),
    color = guide_legend(override.aes = list(linewidth = 1), order = 2)
  )

# Guardar
dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE)
ggsave("outputs/figures/red_familias_clusters.png", p_main,
       width = 24, height = 18, dpi = 300, bg = "#FAFAFA")

cat("\n✅ Guardado: outputs/figures/red_familias_clusters.png\n")

# ============================================================================
# RESUMEN
# ============================================================================

cat("\n", strrep("=", 70), "\n")
cat("📊 RESUMEN DE LA RED CON CLUSTERS\n")
cat(strrep("=", 70), "\n")

cat("\nMiembros por familia:\n")
print(nodes %>% count(familia_grupo, pais) %>% arrange(pais, desc(n)))

cat("\nConexiones inter-familiares:\n")
interfam <- edges_plot %>%
  filter(es_interfamiliar) %>%
  select(from_familia, to_familia) %>%
  distinct()
if (nrow(interfam) > 0) {
  print(interfam)
} else {
  cat("   (Ninguna encontrada con los filtros actuales)\n")
}

cat("\n✅ Script completado!\n")
