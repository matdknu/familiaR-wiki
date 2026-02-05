# ============================================================================
# red_globos_multipais.R
# Visualización con múltiples globos (uno por país) y conexiones cruzadas
# Similar a red_dos_globos pero para todos los países latinoamericanos
# ============================================================================

library(readr)
library(dplyr)
library(igraph)
library(ggraph)
library(tidygraph)
library(ggplot2)
library(stringr)
library(tidyr)
library(jsonlite)

cat("📊 Visualización Multi-Globos: Todos los Países Latinoamericanos\n")
cat(strrep("=", 80), "\n")

# Países disponibles (todos los del consolidado LATAM)
paises_disponibles <- c("chile", "argentina", "mexico", "peru", "colombia", "venezuela",
                       "bolivia", "paraguay", "uruguay", "ecuador")
nombres_paises <- c("Chile", "Argentina", "México", "Perú", "Colombia", "Venezuela",
                    "Bolivia", "Paraguay", "Uruguay", "Ecuador")

# Función para detectar nacionalidad
detect_nationality <- function(row_data, paises_list) {
  text_sources <- c(
    row_data$nacionalidad,
    row_data$lugar_nacimiento,
    row_data$biografia,
    row_data$biografia_inicial
  )
  
  combined <- tolower(paste(text_sources[!is.na(text_sources)], collapse = " "))
  
  if (!is.na(row_data$nacionalidad) && nchar(row_data$nacionalidad) > 0) {
    nac <- tolower(row_data$nacionalidad)
    for (pais in paises_list) {
      if (str_detect(nac, tolower(pais))) return(pais)
    }
  }
  
  if (!is.na(row_data$lugar_nacimiento) && nchar(row_data$lugar_nacimiento) > 0) {
    lugar <- tolower(row_data$lugar_nacimiento)
    patterns <- list(
      "Chile" = c("santiago", "chile", "valparaíso", "concepción", "viña del mar"),
      "Argentina" = c("buenos aires", "córdoba", "rosario", "argentina", "mendoza"),
      "México" = c("ciudad de méxico", "méxico", "guadalajara", "monterrey", "puebla"),
      "Perú" = c("lima", "perú", "cusco", "arequipa", "trujillo"),
      "Colombia" = c("bogotá", "colombia", "medellín", "cali", "cartagena", "barranquilla"),
      "Venezuela" = c("caracas", "venezuela", "maracaibo", "valencia"),
      "Bolivia" = c("la paz", "bolivia", "santa cruz", "sucre", "cochabamba"),
      "Paraguay" = c("asunción", "paraguay", "encarnación"),
      "Uruguay" = c("montevideo", "uruguay", "paysandú"),
      "Ecuador" = c("quito", "guayaquil", "ecuador", "cuenca")
    )
    for (pais in names(patterns)) {
      if (any(str_detect(lugar, patterns[[pais]]))) return(pais)
    }
  }

  patterns_bio <- list(
    "Chile" = c("chileno", "chilena"),
    "Argentina" = c("argentino", "argentina"),
    "México" = c("mexicano", "mexicana"),
    "Perú" = c("peruano", "peruana"),
    "Colombia" = c("colombiano", "colombiana"),
    "Venezuela" = c("venezolano", "venezolana"),
    "Bolivia" = c("boliviano", "boliviana"),
    "Paraguay" = c("paraguayo", "paraguaya"),
    "Uruguay" = c("uruguayo", "uruguaya"),
    "Ecuador" = c("ecuatoriano", "ecuatoriana")
  )
  
  for (pais in names(patterns_bio)) {
    if (str_detect(combined, paste(patterns_bio[[pais]], collapse = "|"))) return(pais)
  }
  
  return("Desconocido")
}

# Cargar datos de todos los países
cat("\n📂 Cargando datos...\n")
all_countries_data <- list()

for (i in seq_along(paises_disponibles)) {
  pais <- paises_disponibles[i]
  nombre_pais <- nombres_paises[i]
  file_path <- paste0("data/raw/", pais, "/familias/_CONSOLIDADO_todas_familias.csv")
  if (!file.exists(file_path)) {
    file_path <- paste0("data/processed/familias/", pais, "/consolidado.csv")
  }
  if (file.exists(file_path)) {
    cat("  ✓ Cargando", nombre_pais, "...\n")
    tryCatch({
      data <- read_delim(file_path, delim = ";", show_col_types = FALSE,
                        locale = locale(encoding = "UTF-8"))
      data <- data %>%
        filter(!is.na(url), as.character(url) != "") %>%
        rowwise() %>%
        mutate(
          pais = nombre_pais,
          pais_nacionalidad = detect_nationality(pick(everything()), nombres_paises)
        ) %>%
        ungroup()
      all_countries_data[[nombre_pais]] <- data
      cat("    →", nrow(data), "personas\n")
    }, error = function(e) {
      cat("    ⚠️ Error:", e$message, "\n")
    })
  }
}

# Combinar todos los datos
if (length(all_countries_data) > 0) {
  all_data <- bind_rows(all_countries_data) %>%
    distinct(url, .keep_all = TRUE)
  
  cat("\n✅ Total:", nrow(all_data), "personas de", length(all_countries_data), "países\n")
} else {
  stop("❌ No se encontraron datos")
}

# Detectar conexiones diplomáticas/profesionales entre países
detect_country_connections <- function(row_data, target_countries) {
  text_fields <- c(
    row_data$cargos_politicos,
    row_data$ocupacion,
    row_data$biografia,
    row_data$biografia_inicial
  )
  
  combined_text <- tolower(paste(text_fields[!is.na(text_fields)], collapse = " "))
  
  connections <- list()
  for (target in target_countries) {
    keywords <- switch(target,
      "Argentina" = c("embajador.*argentina", "ministro.*argentina", "diplomático.*argentina", 
                      "argentino", "buenos aires", "menem", "perón"),
      "Chile" = c("embajador.*chile", "ministro.*chile", "diplomático.*chile",
                  "chileno", "chilena", "santiago.*chile"),
      "México" = c("embajador.*méxico", "ministro.*méxico", "diplomático.*méxico",
                   "mexicano", "mexicana", "ciudad de méxico"),
      "Perú" = c("embajador.*perú", "ministro.*perú", "diplomático.*perú",
                 "peruano", "peruana", "lima")
    )
    
    has_connection <- any(sapply(keywords, function(k) str_detect(combined_text, k)))
    if (has_connection) {
      connections[[target]] <- TRUE
    }
  }
  
  list(
    connections = names(connections),
    has_any = length(connections) > 0
  )
}

# Aplicar detección de conexiones
cat("\n🔍 Detectando conexiones diplomáticas/profesionales...\n")
country_connections_list <- list()


xd <- all_data |> select(infobox_json)

for (pais_actual in names(all_countries_data)) {
  otros_paises <- setdiff(nombres_paises, pais_actual)
  
  connections <- all_data %>%
    filter(pais == pais_actual) %>%
    rowwise() %>%
    do({
      conn <- detect_country_connections(., otros_paises)
      if (conn$has_any) {
        tibble(
          url = .$url,
          nombre = .$nombre,
          pais = .$pais,
          conecta_con = paste(conn$connections, collapse = ", ")
        )
      } else {
        tibble()
      }
    })
  
  if (nrow(connections) > 0) {
    country_connections_list[[pais_actual]] <- connections
  }
}

all_country_connections <- bind_rows(country_connections_list)
cat("✅ Personas con conexiones detectadas:", nrow(all_country_connections), "\n")

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
  source_pais <- all_data$pais[i]
  
  for (field in c("padres", "conyuge", "pareja", "hijos", "hermanos")) {
    if (field %in% colnames(all_data) && !is.na(all_data[[field]][i])) {
      target_urls <- extract_relation_urls(all_data[[field]][i])
      for (target_url in target_urls) {
        edges_list[[length(edges_list) + 1]] <- tibble(
          from = source_url,
          to = target_url,
          relation_type = field,
          from_pais = source_pais
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
      all_data %>% select(url, pais, nombre),
      by = c("to" = "url"),
      suffix = c("_from", "_to")
    ) %>%
    rename(
      to_pais = pais,
      to_nombre = nombre
    ) %>%
    left_join(
      all_data %>% select(url, nombre),
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
    all_data %>% select(url, nombre, pais, pais_nacionalidad),
    by = "url"
  ) %>%
  mutate(
    nombre = ifelse(is.na(nombre), str_replace_all(str_extract(url, "(?<=wiki/).+"), "_", " "), nombre),
    pais = ifelse(is.na(pais), "Desconocido", pais),
    # Identificar nodos puente
    tipo_nodo = case_when(
      url %in% all_country_connections$url ~ "Puente",
      pais != "Desconocido" ~ "Nacional",
      TRUE ~ "Desconocido"
    )
  )

# Conexiones directas entre países
if (nrow(edges_enriched) > 0) {
  cross_country_direct <- edges_enriched %>%
    filter(!is.na(to_pais) & from_pais != to_pais & from_pais != "Desconocido" & to_pais != "Desconocido")
  
  cat("✅ Conexiones familiares directas entre países:", nrow(cross_country_direct), "\n")
} else {
  cross_country_direct <- tibble()
}

# Crear grafo
cat("\n🎨 Creando visualización con múltiples globos...\n")

g_tbl <- tbl_graph(nodes = nodes, edges = edges_enriched, directed = TRUE) %>%
  activate(nodes) %>%
  mutate(
    degree = centrality_degree(mode = "all"),
    betweenness = centrality_betweenness()
  )

# Layout manual: globos por país en círculo
g_igraph <- as.igraph(g_tbl)

paises_unicos <- unique(nodes$pais[nodes$pais != "Desconocido"])
n_paises <- length(paises_unicos)

# Colores por país (todos los países + Desconocido)
country_colors <- c(
  "Chile" = "#0033A0",
  "Argentina" = "#6CACE4",
  "México" = "#006847",
  "Perú" = "#D91023",
  "Peru" = "#D91023",
  "Colombia" = "#FCD116",
  "Venezuela" = "#CF142B",
  "Bolivia" = "#007A33",
  "Paraguay" = "#D52B1E",
  "Uruguay" = "#0038A8",
  "Ecuador" = "#FFD100",
  "Desconocido" = "gray70"
)

# Crear layout: cada país en posiciones específicas
layout_combined <- matrix(0, vcount(g_igraph), 2)

# Posiciones en círculo/elipse para 10 países (mejor visualización de conexiones cruzadas)
# Orden aproximado geográfico: Chile, Argentina, Uruguay, Paraguay, Bolivia, Perú, Ecuador, Colombia, Venezuela, México
posiciones_paises <- list(
  "Chile"    = c(8, -6),
  "Argentina" = c(10, 2),
  "Uruguay"  = c(10, -2),
  "Paraguay" = c(6, 4),
  "Bolivia"  = c(2, 6),
  "Perú"     = c(-4, 6),
  "Peru"     = c(-4, 6),
  "Ecuador"  = c(-8, 4),
  "Colombia" = c(-10, 0),
  "Venezuela" = c(-8, -4),
  "México"   = c(-4, -8)
)

# Posicionar cada país en su ubicación específica
for (pais in paises_unicos) {
  pais_nodes_idx <- which(V(g_igraph)$pais == pais)
  
  if (length(pais_nodes_idx) > 0) {
    # Subgrafo del país
    g_pais <- induced_subgraph(g_igraph, pais_nodes_idx)
    layout_pais <- layout_with_fr(g_pais)
    layout_pais <- layout_pais * 0.8  # Escalar el layout
    
    # Obtener posición central del país
    center_pos <- if (pais %in% names(posiciones_paises)) {
      posiciones_paises[[pais]]
    } else if (pais == "Peru" && "Perú" %in% names(posiciones_paises)) {
      # Manejar variante sin tilde
      posiciones_paises[["Perú"]]
    } else {
      # Si el país no está en la lista, usar posición por defecto
      c(0, 0)
    }
    
    # Centrar el layout del país en su posición
    layout_pais[, 1] <- layout_pais[, 1] + center_pos[1]
    layout_pais[, 2] <- layout_pais[, 2] + center_pos[2]
    
    layout_combined[pais_nodes_idx, ] <- layout_pais
  }
}

# Nodos puente distribuidos entre los países (no solo en el centro)
bridge_nodes_idx <- which(V(g_igraph)$tipo_nodo == "Puente")
if (length(bridge_nodes_idx) > 0) {
  # Distribuir nodos puente en el área central para conectar mejor los países
  layout_bridge <- matrix(0, length(bridge_nodes_idx), 2)
  # Crear una distribución más amplia en el centro
  layout_bridge[, 1] <- runif(length(bridge_nodes_idx), -4, 4)
  layout_bridge[, 2] <- runif(length(bridge_nodes_idx), -4, 4)
  layout_combined[bridge_nodes_idx, ] <- layout_bridge
}

# Desconocidos dispersos
unknown_nodes_idx <- which(V(g_igraph)$pais == "Desconocido")
if (length(unknown_nodes_idx) > 0) {
  layout_unknown <- matrix(0, length(unknown_nodes_idx), 2)
  layout_unknown[, 1] <- runif(length(unknown_nodes_idx), -1, 1)
  layout_unknown[, 2] <- runif(length(unknown_nodes_idx), -1, 1)
  layout_combined[unknown_nodes_idx, ] <- layout_unknown
}

# Agregar layout al grafo
g_tbl <- g_tbl %>%
  activate(nodes) %>%
  mutate(
    x = layout_combined[, 1],
    y = layout_combined[, 2]
  )


g_tbl

# Visualización
p <- ggraph(g_tbl, layout = "manual", x = x, y = y) +
  # Aristas - destacar conexiones entre países
  geom_edge_link(
    aes(color = ifelse(from_pais != to_pais & !is.na(to_pais), "cross", "normal"),
        alpha = ifelse(from_pais != to_pais & !is.na(to_pais), 0.6, 0.1),
        width = ifelse(from_pais != to_pais & !is.na(to_pais), 0.8, 0.2)),
    arrow = arrow(length = unit(1, "mm"), type = "closed")
  ) +
  scale_edge_color_manual(
    values = c("cross" = "#FF6B6B", "normal" = "gray70"),
    name = "Conexión",
    labels = c("cross" = "Entre países", "normal" = "Dentro del país")
  ) +
  scale_edge_alpha_identity(guide = "none") +
  scale_edge_width_continuous(range = c(0.1, 1), guide = "none") +
  # Nodos
  geom_node_point(
    aes(size = degree, color = pais, shape = tipo_nodo),
    alpha = 0.7
  ) +
  # Asegurar que todos los países tengan su color (incluyendo Perú)
  scale_color_manual(
    values = country_colors,
    name = "País",
    na.value = "gray50",
    drop = FALSE,
    guide = guide_legend(override.aes = list(size = 4))
  ) +
  # Etiquetas para nodos puente
  geom_node_text(
    aes(label = ifelse(tipo_nodo == "Puente", nombre, "")),
    size = 2.5,
    repel = TRUE,
    max.overlaps = 30,
    fontface = "bold",
    color = "#FF6B6B"
  ) +
  # Etiquetas para nodos importantes
  geom_node_text(
    aes(label = ifelse(degree >= 15 & tipo_nodo != "Puente", nombre, "")),
    size = 2,
    repel = TRUE,
    max.overlaps = 20
  ) +
  scale_size_continuous(range = c(0.5, 6), name = "Grado") +
  scale_shape_manual(
    values = c("Puente" = 17, "Nacional" = 19, "Desconocido" = 4),
    name = "Tipo"
  ) +
  # Anotaciones de países en sus posiciones
  {
    annots <- list()
    for (pais in paises_unicos) {
      if (pais %in% names(posiciones_paises)) {
        center_pos <- posiciones_paises[[pais]]
        # Posicionar etiqueta ligeramente fuera del globo
        offset <- 2.5
        x_pos <- center_pos[1] + ifelse(center_pos[1] < 0, -offset, offset)
        y_pos <- center_pos[2] + ifelse(center_pos[2] < 0, -offset, offset)
        
        # Obtener color del país (manejar variantes)
        pais_color <- if (pais %in% names(country_colors)) {
          country_colors[[pais]]
        } else if (pais == "Peru" && "Perú" %in% names(country_colors)) {
          country_colors[["Perú"]]
        } else {
          "gray50"
        }
        
        annots[[length(annots) + 1]] <- annotate("text", x = x_pos, y = y_pos,
                                label = toupper(pais), size = 12, fontface = "bold",
                                color = pais_color, alpha = 0.8)
      }
    }
    annots
  } +
  labs(
    title = "Red Multi-País: Globos Conectados de Latinoamérica",
    subtitle = paste(
      paste(paises_unicos, collapse = " | "),
      "| Nodos puente:", sum(nodes$tipo_nodo == "Puente"),
      "| Conexiones entre países:", nrow(cross_country_direct)
    ),
    caption = "Triángulos rojos = personas con conexiones diplomáticas/profesionales a otros países"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    plot.caption = element_text(hjust = 0.5, size = 9, color = "gray50"),
    legend.position = "right"
  )

p

# Guardar
dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs/tables", showWarnings = FALSE, recursive = TRUE)

ggsave("outputs/figures/red_globos_multipais.png", p, width = 30, height = 24, dpi = 300)
cat("✅ Guardado: outputs/figures/red_globos_multipais.png\n")

# Guardar tablas
if (nrow(all_country_connections) > 0) {
  write_csv(all_country_connections, "outputs/tables/nodos_puente_multipais.csv")
  cat("✅ Guardado: outputs/tables/nodos_puente_multipais.csv\n")
}

if (nrow(cross_country_direct) > 0) {
  write_csv(cross_country_direct, "outputs/tables/conexiones_familiares_multipais.csv")
  cat("✅ Guardado: outputs/tables/conexiones_familiares_multipais.csv\n")
}

# Resumen
cat("\n", strrep("=", 80), "\n")
cat("📊 RESUMEN: MÚLTIPLES GLOBOS CONECTADOS\n")
cat(strrep("=", 80), "\n")
cat("   Nodos totales:", nrow(nodes), "\n")
for (pais in paises_unicos) {
  cat("   -", pais, ":", sum(nodes$pais == pais), "\n")
}
cat("   - Desconocidos:", sum(nodes$pais == "Desconocido"), "\n")
cat("   Nodos puente:", sum(nodes$tipo_nodo == "Puente"), "\n")
cat("   Conexiones familiares directas entre países:", nrow(cross_country_direct), "\n")
cat(strrep("=", 80), "\n")
