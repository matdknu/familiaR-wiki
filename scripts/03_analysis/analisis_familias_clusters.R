# ============================================================================
# analisis_familias_clusters.R
# Análisis de familias como clusters con conexiones intra e inter-familiares
# Implementa 5 hipótesis sobre élites y reproducción social
# ============================================================================

library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(ggraph)
library(tidygraph)
library(igraph)
library(jsonlite)
library(lubridate)

cat("📊 Análisis de Familias como Clusters\n")
cat(strrep("=", 80), "\n")

# Cargar datos
cat("📂 Cargando datos...\n")
chile_data <- read_delim("data/raw/chile/familias/_CONSOLIDADO_todas_familias.csv", 
                         delim = ";", show_col_types = FALSE, locale = locale(encoding = "UTF-8"))

cat("✅ Datos cargados:", nrow(chile_data), "personas\n")

# Función para extraer nombre de familia
extract_family_name <- function(categoria_origen, familia_field) {
  # Primero intentar desde categoria_origen (URL)
  if (!is.na(categoria_origen) && categoria_origen != "") {
    # Extraer "Familia_XXX" de URL
    match <- str_match(categoria_origen, "Categoría:Familia_([^/]+)")
    if (!is.na(match[1, 2])) {
      return(str_replace_all(match[1, 2], "_", " "))
    }
  }
  
  # Si no, usar campo familia directamente
  if (!is.na(familia_field) && familia_field != "") {
    # Limpiar y extraer nombre
    familia_clean <- str_replace_all(familia_field, "Familia ", "")
    familia_clean <- str_replace_all(familia_clean, "familia ", "")
    if (nchar(familia_clean) > 0) {
      return(familia_clean)
    }
  }
  
  return(NA_character_)
}

# Extraer nombres de familia
chile_data <- chile_data %>%
  rowwise() %>%
  mutate(
    familia_nombre = extract_family_name(categoria_origen, familia)
  ) %>%
  ungroup() %>%
  filter(!is.na(familia_nombre) & familia_nombre != "")

cat("✅ Personas con familia identificada:", nrow(chile_data), "\n")
cat("✅ Familias únicas:", n_distinct(chile_data$familia_nombre), "\n")

# Función para extraer apellidos
extract_surnames <- function(nombre) {
  if (is.na(nombre) || nombre == "") return(list(surname_1 = NA, surname_2 = NA))
  tokens <- str_split(nombre, "\\s+")[[1]]
  tokens <- tokens[tokens != ""]
  preposiciones <- c("de", "del", "la", "los", "las", "y", "e")
  tokens_clean <- tokens[!tolower(tokens) %in% preposiciones]
  if (length(tokens_clean) == 0) return(list(surname_1 = NA, surname_2 = NA))
  surname_1 <- tokens_clean[length(tokens_clean)]
  surname_2 <- if (length(tokens_clean) >= 2) tokens_clean[length(tokens_clean) - 1] else NA_character_
  list(surname_1 = surname_1, surname_2 = surname_2)
}

chile_data <- chile_data %>%
  rowwise() %>%
  mutate(
    surnames = list(extract_surnames(nombre)),
    surname_1 = surnames$surname_1,
    surname_2 = surnames$surname_2
  ) %>%
  ungroup()

# Extraer relaciones familiares
extract_relation_urls <- function(relation_field) {
  if (is.na(relation_field) || relation_field == "") return(character())
  pattern <- "\\(https://es\\.wikipedia\\.org/wiki/([^)]+)\\)"
  matches <- str_match_all(relation_field, pattern)[[1]]
  if (nrow(matches) > 0) {
    return(paste0("https://es.wikipedia.org/wiki/", matches[, 2]))
  }
  return(character())
}

cat("\n🔗 Extrayendo relaciones familiares...\n")
edges_list <- list()
for (i in 1:nrow(chile_data)) {
  source_url <- chile_data$url[i]
  source_familia <- chile_data$familia_nombre[i]
  
  for (field in c("padres", "conyuge", "pareja", "hijos", "hermanos")) {
    if (field %in% colnames(chile_data) && !is.na(chile_data[[field]][i])) {
      target_urls <- extract_relation_urls(chile_data[[field]][i])
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
  
  # Enriquecer con información del destino
  edges_enriched <- edges_all %>%
    left_join(
      chile_data %>% select(url, familia_nombre, nombre, surname_1),
      by = c("to" = "url"),
      suffix = c("_from", "_to")
    ) %>%
    rename(
      to_familia = familia_nombre,
      to_nombre = nombre,
      to_surname = surname_1
    ) %>%
    left_join(
      chile_data %>% select(url, nombre, surname_1),
      by = c("from" = "url")
    ) %>%
    rename(
      from_nombre = nombre,
      from_surname = surname_1
    )
  
  # Clasificar conexiones
  edges_enriched <- edges_enriched %>%
    mutate(
      connection_type = case_when(
        !is.na(to_familia) & from_familia == to_familia ~ "Intra-familia",
        !is.na(to_familia) & from_familia != to_familia ~ "Inter-familia",
        TRUE ~ "Desconocido"
      )
    )
  
  cat("✅ Relaciones extraídas:", nrow(edges_enriched), "\n")
  cat("   - Intra-familia:", sum(edges_enriched$connection_type == "Intra-familia"), "\n")
  cat("   - Inter-familia:", sum(edges_enriched$connection_type == "Inter-familia"), "\n")
} else {
  edges_enriched <- tibble()
  cat("⚠️ No se encontraron relaciones\n")
}

# Crear nodos
all_urls <- unique(c(edges_enriched$from, edges_enriched$to))
nodes <- tibble(url = all_urls) %>%
  left_join(
    chile_data %>% select(url, nombre, familia_nombre, surname_1, fecha_nacimiento, 
                         lugar_nacimiento, ocupacion, cargos_politicos, conyuge, pareja),
    by = "url"
  ) %>%
  mutate(
    nombre = ifelse(is.na(nombre), str_replace_all(str_extract(url, "(?<=wiki/).+"), "_", " "), nombre),
    familia_nombre = ifelse(is.na(familia_nombre), "Sin familia", familia_nombre)
  )

cat("\n📊 Nodos totales:", nrow(nodes), "\n")
cat("   Familias representadas:", n_distinct(nodes$familia_nombre), "\n")

# ============================================================================
# HIPÓTESIS 1: Persistencia de elites y reproducción social
# ============================================================================
cat("\n", strrep("=", 80), "\n")
cat("HIPÓTESIS 1: Persistencia de elites y reproducción social\n")
cat(strrep("=", 80), "\n")

# Extraer años de nacimiento
nodes <- nodes %>%
  mutate(
    birth_year = as.integer(str_extract(fecha_nacimiento, "\\b(1[789]\\d{2}|20[0-2]\\d)\\b")),
    generacion = case_when(
      !is.na(birth_year) & birth_year < 1900 ~ "Pre-1900",
      !is.na(birth_year) & birth_year >= 1900 & birth_year < 1950 ~ "1900-1950",
      !is.na(birth_year) & birth_year >= 1950 & birth_year < 1980 ~ "1950-1980",
      !is.na(birth_year) & birth_year >= 1980 ~ "1980+",
      TRUE ~ "Desconocido"
    )
  )

# Análisis por familia y generación
persistencia_familias <- nodes %>%
  filter(familia_nombre != "Sin familia") %>%
  group_by(familia_nombre, generacion) %>%
  summarise(
    n_personas = n(),
    n_con_cargos = sum(!is.na(cargos_politicos) & cargos_politicos != "", na.rm = TRUE),
    n_con_ocupacion = sum(!is.na(ocupacion) & ocupacion != "", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(familia_nombre) %>%
  mutate(
    total_personas = sum(n_personas),
    total_generaciones = n_distinct(generacion),
    persistencia_score = total_generaciones * log(total_personas + 1)
  ) %>%
  ungroup() %>%
  arrange(desc(persistencia_score))

cat("\nTop 15 familias por persistencia (múltiples generaciones):\n")
print(persistencia_familias %>% 
      distinct(familia_nombre, total_personas, total_generaciones, persistencia_score) %>%
      head(15))

# Matrimonios endogámicos (dentro de la misma familia)
matrimonios_endogamicos <- edges_enriched %>%
  filter(relation_type %in% c("conyuge", "pareja")) %>%
  filter(connection_type == "Intra-familia") %>%
  group_by(from_familia) %>%
  summarise(n_matrimonios_endogamicos = n(), .groups = "drop") %>%
  arrange(desc(n_matrimonios_endogamicos))

cat("\nTop 10 familias con más matrimonios endogámicos:\n")
print(matrimonios_endogamicos %>% head(10))

# ============================================================================
# HIPÓTESIS 2: Geografía del poder familiar
# ============================================================================
cat("\n", strrep("=", 80), "\n")
cat("HIPÓTESIS 2: Geografía del poder familiar\n")
cat(strrep("=", 80), "\n")

# Clasificar lugares de nacimiento
nodes <- nodes %>%
  mutate(
    lugar_clean = tolower(str_replace_all(lugar_nacimiento, "[^a-zA-ZáéíóúñÁÉÍÓÚÑ\\s]", "")),
    region_poder = case_when(
      str_detect(lugar_clean, "santiago|valparaíso|viña|concepción") ~ "Capital/Región Central",
      str_detect(lugar_clean, "valdivia|temuco|puerto montt|osorno") ~ "Sur",
      str_detect(lugar_clean, "la serena|coquimbo|antofagasta|iquique|arica") ~ "Norte",
      !is.na(lugar_nacimiento) & lugar_nacimiento != "" ~ "Otras provincias",
      TRUE ~ "Desconocido"
    )
  )

geografia_familias <- nodes %>%
  filter(familia_nombre != "Sin familia") %>%
  group_by(familia_nombre, region_poder) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(familia_nombre) %>%
  mutate(
    total = sum(n),
    pct = round(100 * n / total, 1),
    region_dominante = region_poder[which.max(n)]
  ) %>%
  ungroup()

cat("\nDistribución geográfica de familias (top 15):\n")
print(geografia_familias %>%
      distinct(familia_nombre, region_dominante, total) %>%
      arrange(desc(total)) %>%
      head(15))

# ============================================================================
# HIPÓTESIS 3: Género y visibilidad histórica
# ============================================================================
cat("\n", strrep("=", 80), "\n")
cat("HIPÓTESIS 3: Género y visibilidad histórica\n")
cat(strrep("=", 80), "\n")

# Detectar género (heurística simple basada en nombres y ocupaciones)
nodes <- nodes %>%
  mutate(
    # Nombres típicamente femeninos
    nombre_femenino = str_detect(tolower(nombre), "\\b(maría|rosa|ana|elena|patricia|carmen|teresa|andrea|paula|carolina|josefina|isabel|luisa|mercedes|dolores|francisca|claudia|mónica|verónica|natalia)\\b"),
    # Ocupaciones típicamente femeninas históricamente
    ocupacion_femenina = str_detect(tolower(ocupacion), "\\b(primera dama|esposa|ama de casa|dama)\\b"),
    # Si aparece solo como "esposa de" o similar
    solo_como_esposa = str_detect(tolower(ocupacion), "\\besposa de\\b") & 
                       !str_detect(tolower(ocupacion), "\\b(política|abogada|médica|profesora|escritora|artista)\\b"),
    genero_probable = case_when(
      nombre_femenino | ocupacion_femenina ~ "Femenino",
      TRUE ~ "Masculino/Desconocido"
    ),
    visibilidad_independiente = !solo_como_esposa & 
                               (!is.na(ocupacion) & ocupacion != "") &
                               !str_detect(tolower(ocupacion), "\\besposa de\\b")
  )

genero_por_periodo <- nodes %>%
  filter(!is.na(birth_year)) %>%
  mutate(
    periodo = case_when(
      birth_year < 1950 ~ "Pre-1950",
      birth_year >= 1950 & birth_year < 1980 ~ "1950-1980",
      birth_year >= 1980 ~ "1980+"
    )
  ) %>%
  filter(!is.na(periodo)) %>%
  group_by(periodo, genero_probable) %>%
  summarise(
    n_total = n(),
    n_visibilidad_independiente = sum(visibilidad_independiente, na.rm = TRUE),
    pct_visibilidad = round(100 * n_visibilidad_independiente / n_total, 1),
    .groups = "drop"
  )

cat("\nVisibilidad de mujeres por período:\n")
print(genero_por_periodo %>% filter(genero_probable == "Femenino"))

# ============================================================================
# HIPÓTESIS 4: Patrones de nomenclatura y capital simbólico
# ============================================================================
cat("\n", strrep("=", 80), "\n")
cat("HIPÓTESIS 4: Patrones de nomenclatura y capital simbólico\n")
cat(strrep("=", 80), "\n")

# Extraer nombres de pila
nodes <- nodes %>%
  rowwise() %>%
  mutate(
    nombre_pila = ifelse(
      !is.na(nombre) && nombre != "",
      str_split(nombre, "\\s+")[[1]][1],
      NA_character_
    )
  ) %>%
  ungroup()

# Nombres más frecuentes por familia
nombres_por_familia <- nodes %>%
  filter(familia_nombre != "Sin familia" & !is.na(nombre_pila)) %>%
  group_by(familia_nombre, nombre_pila) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(familia_nombre) %>%
  mutate(
    total_familia = sum(n),
    pct = round(100 * n / total_familia, 1)
  ) %>%
  arrange(familia_nombre, desc(n)) %>%
  filter(n >= 2)  # Nombres que aparecen al menos 2 veces en la familia

cat("\nTop 20 patrones de nombres por familia (nombres repetidos):\n")
print(nombres_por_familia %>% head(20))

# ============================================================================
# HIPÓTESIS 5: Redes matrimoniales entre familias de elite
# ============================================================================
cat("\n", strrep("=", 80), "\n")
cat("HIPÓTESIS 5: Redes matrimoniales entre familias de elite\n")
cat(strrep("=", 80), "\n")

# Crear red de familias conectadas por matrimonios
matrimonios_inter_familia <- edges_enriched %>%
  filter(relation_type %in% c("conyuge", "pareja")) %>%
  filter(connection_type == "Inter-familia") %>%
  filter(!is.na(to_familia) & !is.na(from_familia)) %>%
  select(from_familia, to_familia) %>%
  count(from_familia, to_familia, sort = TRUE)

cat("\nTop 20 conexiones matrimoniales entre familias:\n")
print(matrimonios_inter_familia %>% head(20))

# Crear grafo de familias
if (nrow(matrimonios_inter_familia) > 0) {
  g_familias <- tbl_graph(
    nodes = tibble(
      familia = unique(c(matrimonios_inter_familia$from_familia, 
                        matrimonios_inter_familia$to_familia))
    ),
    edges = matrimonios_inter_familia %>% rename(from = from_familia, to = to_familia, weight = n),
    directed = FALSE
  ) %>%
    activate(nodes) %>%
    mutate(
      degree = centrality_degree(),
      betweenness = centrality_betweenness(),
      familia_central = degree >= quantile(degree, 0.8, na.rm = TRUE)
    )
  
  cat("\nFamilias más centrales en la red matrimonial (top 15):\n")
  familias_centrales <- g_familias %>%
    activate(nodes) %>%
    as_tibble() %>%
    arrange(desc(degree)) %>%
    head(15)
  print(familias_centrales)
}

# ============================================================================
# VISUALIZACIÓN: Familias como Clusters
# ============================================================================
cat("\n", strrep("=", 80), "\n")
cat("🎨 Generando visualizaciones...\n")
cat(strrep("=", 80), "\n")

dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs/tables", showWarnings = FALSE, recursive = TRUE)

if (nrow(edges_enriched) > 0 && nrow(nodes) > 0) {
  # Crear grafo completo
  g_completo <- tbl_graph(nodes = nodes, edges = edges_enriched, directed = TRUE) %>%
    activate(nodes) %>%
    mutate(degree = centrality_degree(mode = "all"))
  
  # Calcular tamaño de familia por separado
  familia_sizes <- nodes %>%
    filter(familia_nombre != "Sin familia") %>%
    count(familia_nombre, name = "familia_size")
  
  g_completo <- g_completo %>%
    activate(nodes) %>%
    left_join(familia_sizes, by = "familia_nombre") %>%
    mutate(familia_size = ifelse(is.na(familia_size), 1, familia_size))
  
  # Layout con clusters por familia
  g_igraph <- as.igraph(g_completo)
  
  # Identificar familias grandes (top 20)
  familias_top <- nodes %>%
    filter(familia_nombre != "Sin familia") %>%
    count(familia_nombre, sort = TRUE) %>%
    slice_head(n = 20) %>%
    pull(familia_nombre)
  
  # Layout usando comunidades (familias)
  V(g_igraph)$familia_cluster <- ifelse(
    V(g_igraph)$familia_nombre %in% familias_top,
    V(g_igraph)$familia_nombre,
    "Otras"
  )
  
  # Layout con fuerza dirigida por familia
  layout_familias <- layout_with_fr(g_igraph, niter = 1000)
  
  # Convertir de nuevo a tidygraph
  g_tbl <- g_completo %>%
    activate(nodes) %>%
    mutate(
      x = layout_familias[, 1],
      y = layout_familias[, 2],
      familia_es_top = familia_nombre %in% familias_top
    )
  
  # Visualización 1: Red completa con familias como clusters
  p1 <- ggraph(g_tbl, layout = "manual", x = x, y = y) +
    geom_edge_link(
      aes(color = connection_type, alpha = ifelse(connection_type == "Intra-familia", 0.8, 0.2)),
      arrow = arrow(length = unit(1, "mm"), type = "closed")
    ) +
    scale_edge_color_manual(
      values = c("Intra-familia" = "#0033A0", "Inter-familia" = "#FF6B6B", "Desconocido" = "gray70"),
      name = "Tipo conexión"
    ) +
    geom_node_point(
      aes(size = degree, color = ifelse(familia_es_top, familia_nombre, "Otras")),
      alpha = 0.7
    ) +
    geom_node_text(
      aes(label = ifelse(familia_es_top & degree >= 10, familia_nombre, "")),
      size = 2.5,
      repel = TRUE,
      fontface = "bold",
      max.overlaps = 30
    ) +
    scale_size_continuous(range = c(1, 8), name = "Grado") +
    labs(
      title = "Red de Familias: Clusters y Conexiones",
      subtitle = paste("Top 20 familias como clusters |", 
                       "Azul = conexiones intra-familia | Rojo = conexiones inter-familia"),
      caption = "Tamaño del nodo = grado de conexión | Color = familia"
    ) +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
      plot.subtitle = element_text(hjust = 0.5, size = 12),
      legend.position = "right"
    )
  
  ggsave("outputs/figures/red_familias_clusters.png", p1, width = 24, height = 18, dpi = 300)
  cat("✅ Guardado: outputs/figures/red_familias_clusters.png\n")
  
  # Visualización 2: Red de familias (meta-red)
  if (exists("g_familias") && vcount(as.igraph(g_familias)) > 0) {
    p2 <- ggraph(g_familias, layout = "fr") +
      geom_edge_link(aes(width = weight), alpha = 0.6, color = "#FF6B6B") +
      geom_node_point(aes(size = degree, color = familia_central), alpha = 0.8) +
      geom_node_text(
        aes(label = ifelse(familia_central, familia, "")),
        size = 3,
        repel = TRUE,
        fontface = "bold"
      ) +
      scale_size_continuous(range = c(3, 15), name = "Grado") +
      scale_color_manual(values = c("TRUE" = "#0033A0", "FALSE" = "gray70"), name = "Familia central") +
      labs(
        title = "Red Matrimonial entre Familias de Elite",
        subtitle = "Conexiones = matrimonios entre familias | Tamaño = número de conexiones",
        caption = "Familias centrales (azul) = top 20% por grado"
      ) +
      theme_void() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        plot.subtitle = element_text(hjust = 0.5, size = 12)
      )
    
    ggsave("outputs/figures/red_matrimonial_familias.png", p2, width = 20, height = 16, dpi = 300)
    cat("✅ Guardado: outputs/figures/red_matrimonial_familias.png\n")
  }
}

# Guardar tablas
write_csv(persistencia_familias, "outputs/tables/persistencia_elites_familias.csv")
write_csv(matrimonios_endogamicos, "outputs/tables/matrimonios_endogamicos.csv")
write_csv(geografia_familias, "outputs/tables/geografia_poder_familias.csv")
write_csv(genero_por_periodo, "outputs/tables/genero_visibilidad_historica.csv")
write_csv(nombres_por_familia, "outputs/tables/patrones_nomenclatura_familias.csv")
if (exists("matrimonios_inter_familia") && nrow(matrimonios_inter_familia) > 0) {
  write_csv(matrimonios_inter_familia, "outputs/tables/red_matrimonial_familias.csv")
}
if (exists("familias_centrales")) {
  write_csv(familias_centrales, "outputs/tables/familias_centrales_red.csv")
}

cat("\n✅ Tablas guardadas en outputs/tables/\n")

cat("\n", strrep("=", 80), "\n")
cat("📊 RESUMEN FINAL\n")
cat(strrep("=", 80), "\n")
cat("   Total personas:", nrow(nodes), "\n")
cat("   Familias identificadas:", n_distinct(nodes$familia_nombre), "\n")
cat("   Conexiones totales:", nrow(edges_enriched), "\n")
cat("   - Intra-familia:", sum(edges_enriched$connection_type == "Intra-familia", na.rm = TRUE), "\n")
cat("   - Inter-familia:", sum(edges_enriched$connection_type == "Inter-familia", na.rm = TRUE), "\n")
cat(strrep("=", 80), "\n")
