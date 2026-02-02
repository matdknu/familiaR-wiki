# ============================================================================
# analisis_endogamia_politica_multipais.R
# Análisis completo de redes, endogamia y puestos políticos por país
# ============================================================================

library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(ggraph)
library(tidygraph)
library(igraph)

cat("📊 Análisis de Endogamia y Puestos Políticos Multi-País\n")
cat(strrep("=", 80), "\n")

# Países disponibles
paises_disponibles <- c("chile", "argentina", "mexico", "peru")
nombres_paises <- c("Chile", "Argentina", "México", "Perú")

# Cargar datos de todos los países
cat("\n📂 Cargando datos...\n")
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
        mutate(pais = nombre_pais)
      
      all_countries_data[[nombre_pais]] <- data
      cat("    →", nrow(data), "personas\n")
    }, error = function(e) {
      cat("    ⚠️ Error:", e$message, "\n")
    })
  }
}

if (length(all_countries_data) == 0) {
  stop("❌ No se encontraron datos de ningún país")
}

# Función para extraer nombre de familia
extract_family_name <- function(categoria_origen, familia_field) {
  if (!is.na(categoria_origen) && categoria_origen != "") {
    match <- str_match(categoria_origen, "Categoría:Familia_([^/]+)")
    if (!is.na(match[1, 2])) {
      return(str_replace_all(match[1, 2], "_", " "))
    }
  }
  if (!is.na(familia_field) && familia_field != "") {
    familia_clean <- str_replace_all(familia_field, "Familia ", "")
    familia_clean <- str_replace_all(familia_clean, "familia ", "")
    if (nchar(familia_clean) > 0) {
      return(familia_clean)
    }
  }
  return(NA_character_)
}

# Función para extraer relaciones
extract_relation_urls <- function(relation_field) {
  if (is.na(relation_field) || relation_field == "") return(character())
  pattern <- "\\(https://es\\.wikipedia\\.org/wiki/([^)]+)\\)"
  matches <- str_match_all(relation_field, pattern)[[1]]
  if (nrow(matches) > 0) {
    return(paste0("https://es.wikipedia.org/wiki/", matches[, 2]))
  }
  return(character())
}

# ============================================================================
# ANÁLISIS POR PAÍS
# ============================================================================

cat("\n", strrep("=", 80), "\n")
cat("📊 ANÁLISIS DE REDES POR PAÍS\n")
cat(strrep("=", 80), "\n")

resultados_por_pais <- list()

for (nombre_pais in names(all_countries_data)) {
  cat("\n🔍 Analizando", nombre_pais, "...\n")
  
  data_pais <- all_countries_data[[nombre_pais]]
  
  # Extraer familias
  data_pais <- data_pais %>%
    rowwise() %>%
    mutate(
      familia_nombre = extract_family_name(categoria_origen, familia)
    ) %>%
    ungroup() %>%
    filter(!is.na(familia_nombre) & familia_nombre != "")
  
  # Extraer relaciones
  edges_list <- list()
  for (i in 1:nrow(data_pais)) {
    source_url <- data_pais$url[i]
    source_familia <- data_pais$familia_nombre[i]
    
    for (field in c("padres", "conyuge", "pareja", "hijos", "hermanos")) {
      if (field %in% colnames(data_pais) && !is.na(data_pais[[field]][i])) {
        target_urls <- extract_relation_urls(data_pais[[field]][i])
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
        data_pais %>% select(url, familia_nombre),
        by = c("to" = "url"),
        suffix = c("_from", "_to")
      ) %>%
      rename(to_familia = familia_nombre) %>%
      mutate(
        connection_type = case_when(
          !is.na(to_familia) & from_familia == to_familia ~ "Intra-familia",
          !is.na(to_familia) & from_familia != to_familia ~ "Inter-familia",
          TRUE ~ "Desconocido"
        )
      )
    
    # Calcular métricas de endogamia
    total_edges <- nrow(edges_enriched)
    intra_familia <- sum(edges_enriched$connection_type == "Intra-familia", na.rm = TRUE)
    inter_familia <- sum(edges_enriched$connection_type == "Inter-familia", na.rm = TRUE)
    
    # Matrimonios endogámicos (dentro de la misma familia)
    matrimonios_endogamicos <- edges_enriched %>%
      filter(relation_type %in% c("conyuge", "pareja")) %>%
      filter(connection_type == "Intra-familia") %>%
      nrow()
    
    # Matrimonios totales
    matrimonios_totales <- edges_enriched %>%
      filter(relation_type %in% c("conyuge", "pareja")) %>%
      nrow()
    
    # Tasa de endogamia familiar
    tasa_endogamia_familiar <- ifelse(matrimonios_totales > 0, 
                                       matrimonios_endogamicos / matrimonios_totales, 0)
    
    # Tasa de endogamia general (conexiones intra vs inter familia)
    tasa_endogamia_general <- ifelse(total_edges > 0, 
                                     intra_familia / total_edges, 0)
    
    # Análisis de puestos políticos
    data_pais <- data_pais %>%
      mutate(
        tiene_cargo_politico = !is.na(cargos_politicos) & cargos_politicos != "",
        cargo_text = tolower(cargos_politicos)
      )
    
    # Extraer tipos de cargos
    tipos_cargo <- data_pais %>%
      filter(tiene_cargo_politico) %>%
      mutate(
        es_presidente = str_detect(cargo_text, "presidente"),
        es_ministro = str_detect(cargo_text, "ministro"),
        es_senador = str_detect(cargo_text, "senador"),
        es_diputado = str_detect(cargo_text, "diputado"),
        es_gobernador = str_detect(cargo_text, "gobernador"),
        es_alcalde = str_detect(cargo_text, "alcalde"),
        es_embajador = str_detect(cargo_text, "embajador"),
        es_militar = str_detect(cargo_text, "general|almirante|militar")
      ) %>%
      summarise(
        n_presidentes = sum(es_presidente, na.rm = TRUE),
        n_ministros = sum(es_ministro, na.rm = TRUE),
        n_senadores = sum(es_senador, na.rm = TRUE),
        n_diputados = sum(es_diputado, na.rm = TRUE),
        n_gobernadores = sum(es_gobernador, na.rm = TRUE),
        n_alcaldes = sum(es_alcalde, na.rm = TRUE),
        n_embajadores = sum(es_embajador, na.rm = TRUE),
        n_militares = sum(es_militar, na.rm = TRUE),
        n_con_cargos = sum(tiene_cargo_politico, na.rm = TRUE)
      )
    
    # Guardar resultados
    resultados_por_pais[[nombre_pais]] <- list(
      pais = nombre_pais,
      n_personas = nrow(data_pais),
      n_familias = n_distinct(data_pais$familia_nombre),
      total_edges = total_edges,
      intra_familia = intra_familia,
      inter_familia = inter_familia,
      matrimonios_endogamicos = matrimonios_endogamicos,
      matrimonios_totales = matrimonios_totales,
      tasa_endogamia_familiar = tasa_endogamia_familiar,
      tasa_endogamia_general = tasa_endogamia_general,
      tipos_cargo = tipos_cargo
    )
    
    cat("  ✅", nrow(data_pais), "personas,", n_distinct(data_pais$familia_nombre), "familias\n")
    cat("     Endogamia familiar:", round(tasa_endogamia_familiar * 100, 1), "%\n")
    cat("     Endogamia general:", round(tasa_endogamia_general * 100, 1), "%\n")
  }
}

# ============================================================================
# COMPARACIÓN ENTRE PAÍSES
# ============================================================================

cat("\n", strrep("=", 80), "\n")
cat("📊 COMPARACIÓN ENTRE PAÍSES\n")
cat(strrep("=", 80), "\n")

# Crear tabla comparativa
tabla_comparativa <- bind_rows(lapply(resultados_por_pais, function(x) {
  tibble(
    Pais = x$pais,
    Personas = x$n_personas,
    Familias = x$n_familias,
    Conexiones_Total = x$total_edges,
    Conexiones_Intra_Familia = x$intra_familia,
    Conexiones_Inter_Familia = x$inter_familia,
    Matrimonios_Endogamicos = x$matrimonios_endogamicos,
    Matrimonios_Total = x$matrimonios_totales,
    Tasa_Endogamia_Familiar = round(x$tasa_endogamia_familiar * 100, 2),
    Tasa_Endogamia_General = round(x$tasa_endogamia_general * 100, 2),
    Personas_por_Familia = round(x$n_personas / x$n_familias, 2)
  )
}))

cat("\n📋 Tabla Comparativa:\n")
print(tabla_comparativa)

# País más endogámico
pais_mas_endogamico <- tabla_comparativa %>%
  arrange(desc(Tasa_Endogamia_Familiar)) %>%
  slice_head(n = 1)

cat("\n🏆 PAÍS MÁS ENDOGÁMICO (por matrimonios dentro de la misma familia):\n")
print(pais_mas_endogamico %>% select(Pais, Tasa_Endogamia_Familiar, Matrimonios_Endogamicos, Matrimonios_Total))

# ============================================================================
# ANÁLISIS DE PUESTOS POLÍTICOS POR PAÍS
# ============================================================================

cat("\n", strrep("=", 80), "\n")
cat("📊 ANÁLISIS DE PUESTOS POLÍTICOS POR PAÍS\n")
cat(strrep("=", 80), "\n")

puestos_por_pais <- bind_rows(lapply(resultados_por_pais, function(x) {
  x$tipos_cargo %>%
    mutate(Pais = x$pais) %>%
    select(Pais, everything())
}))

cat("\n📋 Puestos Políticos por País:\n")
print(puestos_por_pais)

# Calcular porcentajes
puestos_por_pais_pct <- puestos_por_pais %>%
  mutate(
    Pct_Presidentes = round(100 * n_presidentes / n_con_cargos, 1),
    Pct_Ministros = round(100 * n_ministros / n_con_cargos, 1),
    Pct_Senadores = round(100 * n_senadores / n_con_cargos, 1),
    Pct_Diputados = round(100 * n_diputados / n_con_cargos, 1),
    Pct_Con_Cargos = round(100 * n_con_cargos / resultados_por_pais[[Pais]]$n_personas, 1),
    .by = Pais
  )

cat("\n📋 Puestos Políticos (% del total con cargos):\n")
print(puestos_por_pais_pct %>% select(Pais, Pct_Presidentes, Pct_Ministros, Pct_Senadores, Pct_Diputados, Pct_Con_Cargos))

# ============================================================================
# ANÁLISIS DE CONEXIONES ENTRE PAÍSES
# ============================================================================

cat("\n", strrep("=", 80), "\n")
cat("📊 ANÁLISIS DE CONEXIONES ENTRE PAÍSES\n")
cat(strrep("=", 80), "\n")

# Combinar todos los datos
all_data <- bind_rows(all_countries_data) %>%
  distinct(url, .keep_all = TRUE)

# Extraer relaciones entre países
edges_inter_pais_list <- list()
for (i in 1:nrow(all_data)) {
  source_url <- all_data$url[i]
  source_pais <- all_data$pais[i]
  
  for (field in c("padres", "conyuge", "pareja", "hijos", "hermanos")) {
    if (field %in% colnames(all_data) && !is.na(all_data[[field]][i])) {
      target_urls <- extract_relation_urls(all_data[[field]][i])
      for (target_url in target_urls) {
        edges_inter_pais_list[[length(edges_inter_pais_list) + 1]] <- tibble(
          from = source_url,
          to = target_url,
          relation_type = field,
          from_pais = source_pais
        )
      }
    }
  }
}

if (length(edges_inter_pais_list) > 0) {
  edges_inter_pais <- bind_rows(edges_inter_pais_list) %>%
    left_join(
      all_data %>% select(url, pais),
      by = c("to" = "url")
    ) %>%
    rename(to_pais = pais) %>%
    filter(!is.na(to_pais) & from_pais != to_pais)
  
  # Matriz de conexiones entre países
  matriz_conexiones <- edges_inter_pais %>%
    count(from_pais, to_pais, relation_type, sort = TRUE)
  
  cat("\n📊 Conexiones entre países por tipo de relación:\n")
  print(matriz_conexiones %>% head(30))
  
  # Resumen por país (cuántas conexiones salen y entran)
  conexiones_salientes <- edges_inter_pais %>%
    count(from_pais, name = "conexiones_salientes") %>%
    rename(pais = from_pais)
  
  conexiones_entrantes <- edges_inter_pais %>%
    count(to_pais, name = "conexiones_entrantes") %>%
    rename(pais = to_pais)
  
  resumen_conexiones <- full_join(conexiones_salientes, conexiones_entrantes, by = "pais") %>%
    mutate(
      conexiones_salientes = ifelse(is.na(conexiones_salientes), 0, conexiones_salientes),
      conexiones_entrantes = ifelse(is.na(conexiones_entrantes), 0, conexiones_entrantes),
      total_conexiones = conexiones_salientes + conexiones_entrantes
    ) %>%
    arrange(desc(total_conexiones))
  
  cat("\n📊 Resumen de conexiones inter-país:\n")
  print(resumen_conexiones)
}

# ============================================================================
# VISUALIZACIONES
# ============================================================================

cat("\n", strrep("=", 80), "\n")
cat("🎨 Generando visualizaciones...\n")
cat(strrep("=", 80), "\n")

dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs/tables", showWarnings = FALSE, recursive = TRUE)

# Gráfico 1: Comparación de endogamia
p1 <- tabla_comparativa %>%
  pivot_longer(cols = c(Tasa_Endogamia_Familiar, Tasa_Endogamia_General),
               names_to = "Tipo", values_to = "Porcentaje") %>%
  ggplot(aes(x = reorder(Pais, Porcentaje), y = Porcentaje, fill = Tipo)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(
    values = c("Tasa_Endogamia_Familiar" = "#D91023", "Tasa_Endogamia_General" = "#0033A0"),
    labels = c("Tasa_Endogamia_Familiar" = "Endogamia Familiar", "Tasa_Endogamia_General" = "Endogamia General")
  ) +
  labs(
    title = "Comparación de Endogamia por País",
    subtitle = "Endogamia Familiar = % matrimonios dentro de la misma familia | Endogamia General = % conexiones intra-familia",
    x = "País",
    y = "Porcentaje (%)",
    fill = "Tipo"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("outputs/figures/comparacion_endogamia_paises.png", p1, width = 14, height = 8, dpi = 300)
cat("✅ Guardado: outputs/figures/comparacion_endogamia_paises.png\n")

# Gráfico 2: Puestos políticos por país
p2 <- puestos_por_pais %>%
  select(Pais, n_presidentes, n_ministros, n_senadores, n_diputados, n_gobernadores) %>%
  pivot_longer(cols = -Pais, names_to = "Cargo", values_to = "Cantidad") %>%
  mutate(Cargo = str_replace_all(Cargo, "n_", "")) %>%
  ggplot(aes(x = reorder(Pais, Cantidad), y = Cantidad, fill = Cargo)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_brewer(palette = "Set2", name = "Tipo de Cargo") +
  labs(
    title = "Distribución de Puestos Políticos por País",
    x = "País",
    y = "Número de Personas",
    fill = "Cargo"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("outputs/figures/puestos_politicos_por_pais.png", p2, width = 14, height = 8, dpi = 300)
cat("✅ Guardado: outputs/figures/puestos_politicos_por_pais.png\n")

# Gráfico 3: Red de conexiones entre países
if (exists("resumen_conexiones") && nrow(resumen_conexiones) > 0) {
  # Crear grafo de países
  if (exists("matriz_conexiones") && nrow(matriz_conexiones) > 0) {
    pais_edges <- matriz_conexiones %>%
      group_by(from_pais, to_pais) %>%
      summarise(weight = sum(n), .groups = "drop")
    
    g_paises <- tbl_graph(
      nodes = tibble(pais = unique(c(pais_edges$from_pais, pais_edges$to_pais))),
      edges = pais_edges %>% rename(from = from_pais, to = to_pais),
      directed = TRUE
    ) %>%
      activate(nodes) %>%
      mutate(
        degree_in = centrality_degree(mode = "in"),
        degree_out = centrality_degree(mode = "out"),
        degree_total = centrality_degree(mode = "all")
      ) %>%
      left_join(resumen_conexiones, by = "pais")
    
    p3 <- ggraph(g_paises, layout = "fr") +
      geom_edge_link(aes(width = weight), alpha = 0.6, color = "#FF6B6B",
                     arrow = arrow(length = unit(2, "mm"), type = "closed")) +
      geom_node_point(aes(size = degree_total), alpha = 0.8, color = "#0033A0") +
      geom_node_text(aes(label = pais), size = 5, fontface = "bold") +
      scale_size_continuous(range = c(10, 30), name = "Conexiones") +
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
    
    ggsave("outputs/figures/red_conexiones_entre_paises.png", p3, width = 16, height = 12, dpi = 300)
    cat("✅ Guardado: outputs/figures/red_conexiones_entre_paises.png\n")
  }
}

# Guardar tablas
write_csv(tabla_comparativa, "outputs/tables/comparacion_endogamia_paises.csv")
write_csv(puestos_por_pais, "outputs/tables/puestos_politicos_por_pais.csv")
if (exists("resumen_conexiones")) {
  write_csv(resumen_conexiones, "outputs/tables/resumen_conexiones_inter_pais.csv")
}
if (exists("matriz_conexiones")) {
  write_csv(matriz_conexiones, "outputs/tables/matriz_conexiones_entre_paises.csv")
}

cat("\n✅ Tablas guardadas en outputs/tables/\n")

# Resumen final
cat("\n", strrep("=", 80), "\n")
cat("📊 RESUMEN FINAL\n")
cat(strrep("=", 80), "\n")
cat("\n🏆 PAÍS MÁS ENDOGÁMICO:\n")
print(pais_mas_endogamico %>% select(Pais, Tasa_Endogamia_Familiar))
cat("\n📊 Total países analizados:", length(resultados_por_pais), "\n")
cat(strrep("=", 80), "\n")
