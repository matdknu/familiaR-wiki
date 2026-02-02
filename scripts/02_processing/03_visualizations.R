# ============================================================================
# 03_visualizations.R
# Visualizaciones de los datos normalizados
# ============================================================================
# Input: Tablas normalizadas y análisis descriptivo
# Requiere: 01_parse_and_normalize.R y 02_descriptive_analysis.R
# ============================================================================

library(ggplot2)
library(readr)
library(dplyr)
library(stringr)
library(igraph)

# Cargar datos si no están en memoria
if (!exists("persons_all") || !exists("positions_all")) {
  cat("Cargando datos normalizados...\n")
  persons_all <- read_csv("data/processed/persons_normalized.csv", show_col_types = FALSE)
  positions_all <- read_csv("data/processed/positions_normalized.csv", show_col_types = FALSE)
  family_relations_all <- read_csv("data/processed/family_relations_normalized.csv", show_col_types = FALSE)
  education_all <- read_csv("data/processed/education_normalized.csv", show_col_types = FALSE)
  affiliations_all <- read_csv("data/processed/affiliations_normalized.csv", show_col_types = FALSE)
  
  # Ejecutar análisis descriptivo si no existe
  if (!exists("position_type_summary")) {
    source("scripts/02_processing/02_descriptive_analysis.R")
  }
  
  # Ejecutar modelado si no existe g_family
  if (!exists("g_family")) {
    source("scripts/02_processing/02_model_and_analyze.R")
  }
}

# Crear directorio de salida
dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE)

# ============================================================================
# 1. GRÁFICOS DE POSICIONES POLÍTICAS
# ============================================================================

# Gráfico 1: Tipos de cargo más comunes
if (exists("position_type_summary") && nrow(position_type_summary) > 0) {
  p1 <- position_type_summary %>%
    head(10) %>%
    ggplot(aes(x = reorder(position_type, n), y = n)) +
    geom_col(fill = "steelblue", alpha = 0.8) +
    coord_flip() +
    labs(
      title = "Tipos de Cargo Político Más Comunes",
      x = "Tipo de Cargo",
      y = "Número de Posiciones"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  
  ggsave("outputs/figures/01_tipos_cargo.png", p1, width = 10, height = 6, dpi = 300)
  cat("✅ Guardado: outputs/figures/01_tipos_cargo.png\n")
}

# Gráfico 2: Distribución temporal de posiciones
if (exists("temporal_distribution") && nrow(temporal_distribution) > 0) {
  p2 <- temporal_distribution %>%
    filter(decade >= 1800) %>%
    ggplot(aes(x = decade, y = n)) +
    geom_line(color = "steelblue", linewidth = 1.2) +
    geom_point(color = "steelblue", size = 2) +
    labs(
      title = "Distribución Temporal de Posiciones Políticas",
      subtitle = "Número de cargos por década",
      x = "Década",
      y = "Número de Posiciones"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  
  ggsave("outputs/figures/02_distribucion_temporal.png", p2, width = 12, height = 6, dpi = 300)
  cat("✅ Guardado: outputs/figures/02_distribucion_temporal.png\n")
}

# Gráfico 3: Top personas con más cargos
if (exists("top_office_holders") && nrow(top_office_holders) > 0) {
  p3 <- top_office_holders %>%
    head(15) %>%
    ggplot(aes(x = reorder(canonical_name, n), y = n)) +
    geom_col(fill = "darkgreen", alpha = 0.8) +
    coord_flip() +
    labs(
      title = "Personas con Más Cargos Políticos",
      x = "Persona",
      y = "Número de Cargos"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  
  ggsave("outputs/figures/03_top_cargos.png", p3, width = 10, height = 8, dpi = 300)
  cat("✅ Guardado: outputs/figures/03_top_cargos.png\n")
}

# ============================================================================
# 2. GRÁFICOS DE RELACIONES FAMILIARES
# ============================================================================

# Gráfico 4: Tipos de relación familiar
if (exists("relation_type_summary") && nrow(relation_type_summary) > 0) {
  p4 <- relation_type_summary %>%
    ggplot(aes(x = reorder(relation_type, n), y = n)) +
    geom_col(fill = "purple", alpha = 0.8) +
    coord_flip() +
    labs(
      title = "Distribución de Tipos de Relación Familiar",
      x = "Tipo de Relación",
      y = "Número de Relaciones"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  
  ggsave("outputs/figures/04_tipos_relacion.png", p4, width = 10, height = 6, dpi = 300)
  cat("✅ Guardado: outputs/figures/04_tipos_relacion.png\n")
}

# Gráfico 5: Red familiar (si existe g_family)
if (exists("g_family") && vcount(g_family) > 0) {
  # Simplificar grafo (remover loops y múltiples aristas)
  g_simple <- simplify(g_family, remove.multiple = TRUE, remove.loops = TRUE)
  
  # Filtrar nodos aislados
  g_simple <- delete.vertices(g_simple, degree(g_simple) == 0)
  
  if (vcount(g_simple) > 0) {
    # Calcular layout
    if (vcount(g_simple) > 100) {
      # Para grafos grandes, usar layout más rápido
      layout <- layout_with_fr(g_simple, niter = 50)
    } else {
      layout <- layout_with_kk(g_simple)
    }
    
    # Guardar como PNG
    png("outputs/figures/05_red_familiar.png", width = 2000, height = 2000, res = 300)
    par(mar = c(0, 0, 2, 0))
    plot(g_simple,
         layout = layout,
         vertex.size = 3,
         vertex.label = NA,
         vertex.color = "lightblue",
         edge.arrow.size = 0.3,
         edge.color = "gray50",
         main = "Red Familiar de Familias Chilenas")
    dev.off()
    cat("✅ Guardado: outputs/figures/05_red_familiar.png\n")
    
    # Guardar versión interactiva HTML (si es posible)
    if (requireNamespace("visNetwork", quietly = TRUE)) {
      library(visNetwork)
      
      nodes <- data.frame(
        id = V(g_simple)$name,
        label = V(g_simple)$name,
        value = degree(g_simple),
        title = paste("ID:", V(g_simple)$name, "<br>Degree:", degree(g_simple))
      )
      
      edges <- get.data.frame(g_simple, what = "edges")
      if (nrow(edges) > 0) {
        edges <- data.frame(
          from = edges$from,
          to = edges$to,
          arrows = "to"
        )
        
        vis <- visNetwork(nodes, edges, width = "100%", height = "800px") %>%
          visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
          visLayout(randomSeed = 42)
        
        htmlwidgets::saveWidget(vis, "outputs/figures/red_familiar_interactiva.html")
        cat("✅ Guardado: outputs/figures/red_familiar_interactiva.html\n")
      }
    }
  }
}

# ============================================================================
# 3. GRÁFICOS DE CARRERAS POLÍTICAS
# ============================================================================

# Gráfico 6: Carreras más largas
if (exists("career_lengths") && nrow(career_lengths) > 0) {
  p6 <- career_lengths %>%
    filter(!is.na(total_years) & total_years > 0) %>%
    head(15) %>%
    ggplot(aes(x = reorder(canonical_name, total_years), y = total_years)) +
    geom_col(fill = "darkorange", alpha = 0.8) +
    coord_flip() +
    labs(
      title = "Carreras Políticas Más Largas",
      subtitle = "Años entre primer y último cargo",
      x = "Persona",
      y = "Años de Carrera"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  
  ggsave("outputs/figures/06_carreras_largas.png", p6, width = 10, height = 8, dpi = 300)
  cat("✅ Guardado: outputs/figures/06_carreras_largas.png\n")
}

# Gráfico 7: Distribución por siglo
if (exists("positions_by_century") && nrow(positions_by_century) > 0) {
  p7 <- positions_by_century %>%
    ggplot(aes(x = factor(century), y = n)) +
    geom_col(fill = "coral", alpha = 0.8) +
    labs(
      title = "Distribución de Posiciones por Siglo",
      x = "Siglo",
      y = "Número de Posiciones"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  
  ggsave("outputs/figures/07_posiciones_por_siglo.png", p7, width = 10, height = 6, dpi = 300)
  cat("✅ Guardado: outputs/figures/07_posiciones_por_siglo.png\n")
}

# ============================================================================
# 4. GRÁFICOS DE EDUCACIÓN Y AFILIACIONES
# ============================================================================

# Gráfico 8: Top instituciones educativas
if (exists("top_institutions") && nrow(top_institutions) > 0) {
  p8 <- top_institutions %>%
    head(15) %>%
    ggplot(aes(x = reorder(institution_raw, n), y = n)) +
    geom_col(fill = "darkblue", alpha = 0.8) +
    coord_flip() +
    labs(
      title = "Instituciones Educativas Más Frecuentes",
      x = "Institución",
      y = "Número de Personas"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          axis.text.y = element_text(size = 8))
  
  ggsave("outputs/figures/08_instituciones.png", p8, width = 10, height = 8, dpi = 300)
  cat("✅ Guardado: outputs/figures/08_instituciones.png\n")
}

# Gráfico 9: Top organizaciones
if (exists("top_organizations") && nrow(top_organizations) > 0 && "affiliation_raw" %in% names(top_organizations)) {
  p9 <- top_organizations %>%
    head(15) %>%
    ggplot(aes(x = reorder(affiliation_raw, n), y = n)) +
    geom_col(fill = "darkred", alpha = 0.8) +
    coord_flip() +
    labs(
      title = "Afiliaciones Más Frecuentes",
      x = "Afiliación",
      y = "Número de Personas"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          axis.text.y = element_text(size = 8))
  
  ggsave("outputs/figures/09_organizaciones.png", p9, width = 10, height = 8, dpi = 300)
  cat("✅ Guardado: outputs/figures/09_organizaciones.png\n")
}

# ============================================================================
# 5. GRÁFICO DE CENTRALIDAD (si existe top_central)
# ============================================================================

if (exists("top_central") && nrow(top_central) > 0) {
  p10 <- top_central %>%
    head(15) %>%
    ggplot(aes(x = reorder(node, degree), y = degree)) +
    geom_col(fill = "darkviolet", alpha = 0.8) +
    coord_flip() +
    labs(
      title = "Personas Más Centrales en la Red Familiar",
      subtitle = "Por grado de conexión",
      x = "Persona",
      y = "Grado de Centralidad"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  
  ggsave("outputs/figures/10_centralidad.png", p10, width = 10, height = 8, dpi = 300)
  cat("✅ Guardado: outputs/figures/10_centralidad.png\n")
}

cat("\n✅ Visualizaciones completadas.\n")


p1


p1
p2
p3
p4
p5
p6
p7

