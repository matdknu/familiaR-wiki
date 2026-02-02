# ============================================================================
# red_multipais_latinoamerica.R
# Análisis de conexiones entre todos los países latinoamericanos
# Detecta conexiones familiares, casos especiales y redes entre países
# ============================================================================

library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(ggraph)
library(tidygraph)
library(igraph)

cat("📊 Análisis de Conexiones Multi-País Latinoamérica\n")
cat(strrep("=", 80), "\n")

# Países disponibles
paises_disponibles <- c("chile", "argentina", "mexico", "peru")
nombres_paises <- c("Chile", "Argentina", "México", "Perú")

# Función para detectar nacionalidad
detect_nationality <- function(row_data, paises_list) {
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
    for (pais in paises_list) {
      if (str_detect(nac, tolower(pais))) return(pais)
    }
  }
  
  # Luego lugar de nacimiento
  if (!is.na(row_data$lugar_nacimiento) && nchar(row_data$lugar_nacimiento) > 0) {
    lugar <- tolower(row_data$lugar_nacimiento)
    # Patrones por país
    patterns <- list(
      "Chile" = c("santiago", "chile", "valparaíso", "concepción", "viña del mar"),
      "Argentina" = c("buenos aires", "córdoba", "rosario", "argentina", "mendoza"),
      "México" = c("ciudad de méxico", "méxico", "guadalajara", "monterrey", "puebla"),
      "Perú" = c("lima", "perú", "cusco", "arequipa", "trujillo")
    )
    
    for (pais in names(patterns)) {
      if (any(str_detect(lugar, patterns[[pais]]))) return(pais)
    }
  }
  
  # Finalmente biografía
  patterns_bio <- list(
    "Chile" = c("chileno", "chilena"),
    "Argentina" = c("argentino", "argentina"),
    "México" = c("mexicano", "mexicana"),
    "Perú" = c("peruano", "peruana")
  )
  
  for (pais in names(patterns_bio)) {
    if (str_detect(combined, paste(patterns_bio[[pais]], collapse = "|"))) return(pais)
  }
  
  return("Desconocido")
}

# Cargar datos de todos los países
cat("\n📂 Cargando datos de todos los países...\n")
all_countries_data <- list()

for (i in seq_along(paises_disponibles)) {
  pais <- paises_disponibles[i]
  nombre_pais <- nombres_paises[i]
  file_path <- paste0("data/raw/", pais, "/familias/_CONSOLIDADO_todas_familias.csv")
  
  if (file.exists(file_path)) {
    cat("  ✓ Cargando", nombre_pais, "...\n")
    tryCatch({
      data <- read_delim(file_path, delim = ";", show_col_types = FALSE, 
                        locale = locale(encoding = "UTF-8"))
      data <- data %>%
        filter(!is.na(url), as.character(url) != "") %>%
        rowwise() %>%
        mutate(
          pais_familia = nombre_pais,
          pais_nacionalidad = detect_nationality(cur_data(), nombres_paises)
        ) %>%
        ungroup()
      
      all_countries_data[[nombre_pais]] <- data
      cat("    →", nrow(data), "personas cargadas\n")
    }, error = function(e) {
      cat("    ⚠️ Error cargando", nombre_pais, ":", e$message, "\n")
    })
  } else {
    cat("  ⚠️ No se encontró:", file_path, "\n")
  }
}

# Combinar todos los datos
if (length(all_countries_data) > 0) {
  all_data <- bind_rows(all_countries_data) %>%
    distinct(url, .keep_all = TRUE)
  
  cat("\n✅ Total de personas:", nrow(all_data), "\n")
  cat("   Países:", paste(names(all_countries_data), collapse = ", "), "\n")
} else {
  stop("❌ No se encontraron datos de ningún país")
}

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
for (i in 1:nrow(all_data)) {
  source_url <- all_data$url[i]
  source_pais_fam <- all_data$pais_familia[i]
  source_pais_nac <- all_data$pais_nacionalidad[i]
  
  for (field in c("padres", "conyuge", "pareja", "hijos", "hermanos")) {
    if (field %in% colnames(all_data) && !is.na(all_data[[field]][i])) {
      target_urls <- extract_relation_urls(all_data[[field]][i])
      for (target_url in target_urls) {
        edges_list[[length(edges_list) + 1]] <- tibble(
          from = source_url,
          to = target_url,
          relation_type = field,
          from_pais_familia = source_pais_fam,
          from_pais_nacionalidad = source_pais_nac
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
      all_data %>% select(url, pais_familia, pais_nacionalidad, nombre),
      by = c("to" = "url"),
      suffix = c("_from", "_to")
    ) %>%
    rename(
      to_pais_familia = pais_familia,
      to_pais_nacionalidad = pais_nacionalidad,
      to_nombre = nombre
    ) %>%
    left_join(
      all_data %>% select(url, nombre),
      by = c("from" = "url")
    ) %>%
    rename(from_nombre = nombre)
  
  # Clasificar conexiones
  edges_enriched <- edges_enriched %>%
    mutate(
      connection_type = case_when(
        !is.na(to_pais_familia) & from_pais_familia == to_pais_familia ~ "Intra-país",
        !is.na(to_pais_familia) & from_pais_familia != to_pais_familia ~ "Inter-país",
        TRUE ~ "Desconocido"
      ),
      # Conexión por nacionalidad real
      conexion_nacionalidad = case_when(
        !is.na(from_pais_nacionalidad) & !is.na(to_pais_nacionalidad) &
        from_pais_nacionalidad != "Desconocido" & to_pais_nacionalidad != "Desconocido" &
        from_pais_nacionalidad != to_pais_nacionalidad ~ "Cruzada",
        TRUE ~ "Normal"
      )
    )
  
  cat("✅ Relaciones extraídas:", nrow(edges_enriched), "\n")
  cat("   - Intra-país:", sum(edges_enriched$connection_type == "Intra-país", na.rm = TRUE), "\n")
  cat("   - Inter-país:", sum(edges_enriched$connection_type == "Inter-país", na.rm = TRUE), "\n")
  cat("   - Conexiones cruzadas (nacionalidad):", sum(edges_enriched$conexion_nacionalidad == "Cruzada", na.rm = TRUE), "\n")
} else {
  edges_enriched <- tibble()
  cat("⚠️ No se encontraron relaciones\n")
}

# Análisis de conexiones entre países
cat("\n", strrep("=", 80), "\n")
cat("📊 ANÁLISIS DE CONEXIONES ENTRE PAÍSES\n")
cat(strrep("=", 80), "\n")

if (nrow(edges_enriched) > 0) {
  # Conexiones inter-país por tipo de relación
  conexiones_inter_pais <- edges_enriched %>%
    filter(connection_type == "Inter-país") %>%
    count(from_pais_familia, to_pais_familia, relation_type, sort = TRUE)
  
  cat("\nTop 30 conexiones inter-país:\n")
  print(conexiones_inter_pais %>% head(30))
  
  # Matriz de conexiones entre países
  matriz_conexiones <- edges_enriched %>%
    filter(connection_type == "Inter-país") %>%
    count(from_pais_familia, to_pais_familia) %>%
    complete(from_pais_familia, to_pais_familia, fill = list(n = 0)) %>%
    pivot_wider(names_from = to_pais_familia, values_from = n, values_fill = 0)
  
  cat("\n📊 Matriz de conexiones entre países:\n")
  print(matriz_conexiones)
  
  # Casos especiales (personas de un país en familias de otro)
  casos_especiales <- all_data %>%
    filter(pais_familia != pais_nacionalidad & pais_nacionalidad != "Desconocido") %>%
    count(pais_familia, pais_nacionalidad, sort = TRUE)
  
  cat("\n🔍 Casos especiales (nacionalidad ≠ familia):\n")
  print(casos_especiales)
}

# Crear nodos
all_urls <- unique(c(edges_enriched$from, edges_enriched$to))
nodes <- tibble(url = all_urls) %>%
  left_join(
    all_data %>% select(url, nombre, pais_familia, pais_nacionalidad, ocupacion, cargos_politicos),
    by = "url"
  ) %>%
  mutate(
    nombre = ifelse(is.na(nombre), str_replace_all(str_extract(url, "(?<=wiki/).+"), "_", " "), nombre),
    pais_familia = ifelse(is.na(pais_familia), "Desconocido", pais_familia),
    pais_nacionalidad = ifelse(is.na(pais_nacionalidad), "Desconocido", pais_nacionalidad)
  )

cat("\n📊 Nodos totales:", nrow(nodes), "\n")
cat("   Distribución por país (familia):\n")
print(nodes %>% count(pais_familia, sort = TRUE))

# Visualizaciones
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
  
  # Layout con clusters por país
  g_igraph <- as.igraph(g_completo)
  
  # Colores por país
  country_colors <- c(
    "Chile" = "#0033A0",
    "Argentina" = "#6CACE4",
    "México" = "#006847",
    "Perú" = "#D91023",
    "Desconocido" = "gray70"
  )
  
  # Layout manual: posicionar países en círculo
  paises_unicos <- unique(nodes$pais_familia[nodes$pais_familia != "Desconocido"])
  n_paises <- length(paises_unicos)
  
  # Crear layout circular para países
  layout_combined <- matrix(0, vcount(g_igraph), 2)
  
  for (i in seq_along(paises_unicos)) {
    pais <- paises_unicos[i]
    pais_nodes_idx <- which(V(g_igraph)$pais_familia == pais)
    
    if (length(pais_nodes_idx) > 0) {
      # Subgrafo del país
      g_pais <- induced_subgraph(g_igraph, pais_nodes_idx)
      layout_pais <- layout_with_fr(g_pais)
      layout_pais <- layout_pais * 0.8
      
      # Posición en círculo
      angle <- 2 * pi * (i - 1) / n_paises
      center_x <- 5 * cos(angle)
      center_y <- 5 * sin(angle)
      
      layout_pais[, 1] <- layout_pais[, 1] + center_x
      layout_pais[, 2] <- layout_pais[, 2] + center_y
      
      layout_combined[pais_nodes_idx, ] <- layout_pais
    }
  }
  
  # Desconocidos en el centro
  unknown_nodes_idx <- which(V(g_igraph)$pais_familia == "Desconocido")
  if (length(unknown_nodes_idx) > 0) {
    layout_combined[unknown_nodes_idx, 1] <- runif(length(unknown_nodes_idx), -2, 2)
    layout_combined[unknown_nodes_idx, 2] <- runif(length(unknown_nodes_idx), -2, 2)
  }
  
  g_tbl <- g_completo %>%
    activate(nodes) %>%
    mutate(x = layout_combined[, 1], y = layout_combined[, 2])
  
  # Visualización 1: Red completa multi-país
  p1 <- ggraph(g_tbl, layout = "manual", x = x, y = y) +
    geom_edge_link(
      aes(color = ifelse(connection_type == "Inter-país", "Inter-país", "Intra-país"),
          alpha = ifelse(connection_type == "Inter-país", 0.6, 0.1)),
      arrow = arrow(length = unit(1, "mm"), type = "closed")
    ) +
    scale_edge_color_manual(
      values = c("Inter-país" = "#FF6B6B", "Intra-país" = "#0033A0", "Desconocido" = "gray70"),
      name = "Tipo conexión"
    ) +
    geom_node_point(
      aes(size = degree, color = pais_familia),
      alpha = 0.7
    ) +
    geom_node_text(
      aes(label = ifelse(degree >= 15, nombre, "")),
      size = 2,
      repel = TRUE,
      max.overlaps = 20
    ) +
    scale_size_continuous(range = c(1, 8), name = "Grado") +
    scale_color_manual(values = country_colors, name = "País (familia)") +
    labs(
      title = "Red Multi-País: Conexiones entre Familias Latinoamericanas",
      subtitle = paste("Países:", paste(paises_unicos, collapse = ", "), "|",
                       "Conexiones inter-país:", sum(edges_enriched$connection_type == "Inter-país", na.rm = TRUE)),
      caption = "Rojo = conexiones entre países | Azul = conexiones dentro del mismo país"
    ) +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
      plot.subtitle = element_text(hjust = 0.5, size = 12),
      legend.position = "right"
    )
  
  ggsave("outputs/figures/red_multipais_latinoamerica.png", p1, width = 28, height = 20, dpi = 300)
  cat("✅ Guardado: outputs/figures/red_multipais_latinoamerica.png\n")
  
  # Visualización 2: Red de países (meta-red)
  if (exists("matriz_conexiones") && nrow(matriz_conexiones) > 0) {
    # Crear grafo de países
    pais_edges <- edges_enriched %>%
      filter(connection_type == "Inter-país") %>%
      count(from_pais_familia, to_pais_familia) %>%
      filter(from_pais_familia != to_pais_familia)
    
    if (nrow(pais_edges) > 0) {
      g_paises <- tbl_graph(
        nodes = tibble(pais = unique(c(pais_edges$from_pais_familia, pais_edges$to_pais_familia))),
        edges = pais_edges %>% rename(from = from_pais_familia, to = to_pais_familia, weight = n),
        directed = TRUE
      ) %>%
        activate(nodes) %>%
        mutate(
          degree_in = centrality_degree(mode = "in"),
          degree_out = centrality_degree(mode = "out"),
          degree_total = centrality_degree(mode = "all")
        )
      
      p2 <- ggraph(g_paises, layout = "fr") +
        geom_edge_link(aes(width = weight), alpha = 0.7, color = "#FF6B6B",
                       arrow = arrow(length = unit(2, "mm"), type = "closed")) +
        geom_node_point(aes(size = degree_total, color = pais), alpha = 0.8, size = 15) +
        geom_node_text(aes(label = pais), size = 5, fontface = "bold", color = "white") +
        scale_size_continuous(range = c(10, 30), name = "Conexiones") +
        scale_color_manual(values = country_colors, name = "País") +
        labs(
          title = "Red de Conexiones entre Países",
          subtitle = "Grosor de línea = número de conexiones familiares",
          caption = "Flechas indican dirección de las conexiones"
        ) +
        theme_void() +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
          plot.subtitle = element_text(hjust = 0.5, size = 12)
        )
      
      ggsave("outputs/figures/red_paises_latinoamerica.png", p2, width = 16, height = 12, dpi = 300)
      cat("✅ Guardado: outputs/figures/red_paises_latinoamerica.png\n")
    }
  }
}

# Guardar tablas
if (exists("conexiones_inter_pais") && nrow(conexiones_inter_pais) > 0) {
  write_csv(conexiones_inter_pais, "outputs/tables/conexiones_inter_pais_latinoamerica.csv")
}
if (exists("matriz_conexiones") && nrow(matriz_conexiones) > 0) {
  write_csv(matriz_conexiones, "outputs/tables/matriz_conexiones_paises.csv")
}
if (exists("casos_especiales") && nrow(casos_especiales) > 0) {
  write_csv(casos_especiales, "outputs/tables/casos_especiales_multipais.csv")
}

cat("\n✅ Tablas guardadas en outputs/tables/\n")

cat("\n", strrep("=", 80), "\n")
cat("📊 RESUMEN FINAL\n")
cat(strrep("=", 80), "\n")
cat("   Total personas:", nrow(all_data), "\n")
cat("   Países analizados:", length(all_countries_data), "\n")
cat("   Conexiones totales:", nrow(edges_enriched), "\n")
if (nrow(edges_enriched) > 0) {
  cat("   - Inter-país:", sum(edges_enriched$connection_type == "Inter-país", na.rm = TRUE), "\n")
  cat("   - Intra-país:", sum(edges_enriched$connection_type == "Intra-país", na.rm = TRUE), "\n")
}
cat(strrep("=", 80), "\n")
