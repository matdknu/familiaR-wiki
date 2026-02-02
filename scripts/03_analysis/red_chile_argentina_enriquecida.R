# ============================================================================
# red_chile_argentina_enriquecida.R
# Análisis enriquecido con cargos políticos, tipos de relación y contexto
# ============================================================================

library(readr)
library(dplyr)
library(igraph)
library(ggraph)
library(tidygraph)
library(ggplot2)
library(RColorBrewer)
library(stringr)
library(purrr)
library(tidyr)
library(jsonlite)

cat("📊 Cargando datos de Chile y Argentina...\n")

# Cargar datos consolidados (separados por ;)
chile_data <- read_delim("data/raw/chile/familias/_CONSOLIDADO_todas_familias.csv", 
                         delim = ";", show_col_types = FALSE, locale = locale(encoding = "UTF-8"))
argentina_data <- read_delim("data/raw/argentina/familias/_CONSOLIDADO_todas_familias.csv", 
                             delim = ";", show_col_types = FALSE, locale = locale(encoding = "UTF-8"))

cat("✅ Chile:", nrow(chile_data), "personas\n")
cat("✅ Argentina:", nrow(argentina_data), "personas\n")

# Agregar columna de país
chile_data <- chile_data %>%
  mutate(pais = "Chile") %>%
  filter(!is.na(url), !is.null(url), as.character(url) != "")

argentina_data <- argentina_data %>%
  mutate(pais = "Argentina") %>%
  filter(!is.na(url), !is.null(url), as.character(url) != "")

# Combinar datos
all_data <- bind_rows(chile_data, argentina_data) %>%
  distinct(url, .keep_all = TRUE)

cat("✅ Total único:", nrow(all_data), "personas\n")

# ============================================================================
# ANÁLISIS 1: Investigar "Desconocidos"
# ============================================================================

cat("\n🔍 ANÁLISIS 1: Investigando personas 'Desconocidas'...\n")
cat(strrep("=", 80), "\n")

# Función mejorada para extraer relaciones con más contexto
extract_relation_urls_enriched <- function(relation_field, relation_type, source_url, source_pais) {
  if (is.na(relation_field) || relation_field == "") {
    return(tibble(
      from = character(), to = character(), relation_type = character(),
      from_pais = character(), context = character()
    ))
  }
  
  # Extraer URLs y nombres
  pattern <- "([^(;]+?)\\s*\\((https://es\\.wikipedia\\.org/wiki/[^)]+)\\)"
  matches <- str_match_all(relation_field, pattern)[[1]]
  
  if (nrow(matches) == 0) {
    return(tibble(
      from = character(), to = character(), relation_type = character(),
      from_pais = character(), context = character()
    ))
  }
  
  tibble(
    from = source_url,
    to = matches[, 3],
    relation_type = relation_type,
    from_pais = source_pais,
    related_name = matches[, 2]
  )
}

# Extraer todas las relaciones con contexto
cat("🔗 Extrayendo relaciones enriquecidas...\n")

edges_enriched <- list()

for (i in 1:nrow(all_data)) {
  person_url <- all_data$url[i]
  person_pais <- all_data$pais[i]
  
  relation_fields <- c("padres", "conyuge", "pareja", "hijos", "hermanos", "familia")
  
  for (field in relation_fields) {
    if (field %in% colnames(all_data)) {
      field_value <- all_data[[field]][i]
      relations <- extract_relation_urls_enriched(field_value, field, person_url, person_pais)
      if (nrow(relations) > 0) {
        edges_enriched[[length(edges_enriched) + 1]] <- relations
      }
    }
  }
}

edges_all <- bind_rows(edges_enriched)

# Identificar nodos "desconocidos" (mencionados pero sin datos completos)
all_mentioned_urls <- unique(edges_all$to)
known_urls <- all_data$url
unknown_urls <- setdiff(all_mentioned_urls, known_urls)

cat("✅ URLs mencionadas:", length(all_mentioned_urls), "\n")
cat("✅ URLs con datos completos:", length(known_urls), "\n")
cat("⚠️  URLs sin datos (Desconocidos):", length(unknown_urls), "\n")

# Analizar de dónde vienen los desconocidos
unknown_analysis <- edges_all %>%
  filter(to %in% unknown_urls) %>%
  group_by(from_pais, relation_type) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(desc(count))

cat("\n📊 Origen de personas Desconocidas:\n")
print(unknown_analysis)

# Ejemplos de desconocidos
cat("\n📋 Ejemplos de personas Desconocidas:\n")
unknown_examples <- edges_all %>%
  filter(to %in% unknown_urls) %>%
  select(related_name, to, relation_type, from_pais) %>%
  distinct() %>%
  head(20)
print(unknown_examples)

# ============================================================================
# ANÁLISIS 2: Enriquecer nodos con información de cargos políticos
# ============================================================================

cat("\n📋 ANÁLISIS 2: Extrayendo cargos políticos del JSON...\n")
cat(strrep("=", 80), "\n")

# Función para extraer cargos del JSON
extract_political_info <- function(infobox_json_str, cargos_str, ocupacion_str) {
  result <- list(
    cargo_tipo = NA_character_,
    cargo_nivel = NA_character_,  # Nacional, Internacional, Local
    cargo_es_embajador = FALSE,
    cargo_es_ministro = FALSE,
    cargo_es_presidente = FALSE,
    cargo_es_diplomatico = FALSE,
    cargos_raw = NA_character_
  )
  
  # De los campos directos
  if (!is.na(cargos_str) && cargos_str != "") {
    result$cargos_raw <- cargos_str
    cargos_lower <- tolower(cargos_str)
    
    if (str_detect(cargos_lower, "embajador|ambassador")) {
      result$cargo_es_embajador <- TRUE
      result$cargo_nivel <- "Internacional"
    }
    if (str_detect(cargos_lower, "ministro|minister")) {
      result$cargo_es_ministro <- TRUE
      result$cargo_nivel <- "Nacional"
    }
    if (str_detect(cargos_lower, "presidente|president")) {
      result$cargo_es_presidente <- TRUE
      result$cargo_nivel <- "Nacional"
    }
  }
  
  # De ocupación
  if (!is.na(ocupacion_str) && ocupacion_str != "") {
    ocup_lower <- tolower(ocupacion_str)
    if (str_detect(ocup_lower, "diplomático|diplomatico|diplomat")) {
      result$cargo_es_diplomatico <- TRUE
      if (is.na(result$cargo_nivel)) {
        result$cargo_nivel <- "Internacional"
      }
    }
    if (str_detect(ocup_lower, "político|politico|politic")) {
      if (is.na(result$cargo_nivel)) {
        result$cargo_nivel <- "Nacional"
      }
    }
  }
  
  # Del JSON
  if (!is.na(infobox_json_str) && infobox_json_str != "") {
    tryCatch({
      json_data <- fromJSON(infobox_json_str)
      if (is.data.frame(json_data)) {
        labels <- tolower(json_data$label)
        
        if (any(str_detect(labels, "embajador|ambassador"))) {
          result$cargo_es_embajador <- TRUE
          result$cargo_nivel <- "Internacional"
        }
        if (any(str_detect(labels, "ministro|minister"))) {
          result$cargo_es_ministro <- TRUE
          result$cargo_nivel <- "Nacional"
        }
        if (any(str_detect(labels, "presidente|president"))) {
          result$cargo_es_presidente <- TRUE
          result$cargo_nivel <- "Nacional"
        }
      }
    }, error = function(e) NULL)
  }
  
  # Determinar tipo principal
  if (result$cargo_es_presidente) {
    result$cargo_tipo <- "Presidente"
  } else if (result$cargo_es_embajador) {
    result$cargo_tipo <- "Embajador"
  } else if (result$cargo_es_ministro) {
    result$cargo_tipo <- "Ministro"
  } else if (result$cargo_es_diplomatico) {
    result$cargo_tipo <- "Diplomático"
  } else if (!is.na(result$cargos_raw)) {
    result$cargo_tipo <- "Otro político"
  } else {
    result$cargo_tipo <- "Sin cargo político"
  }
  
  return(as_tibble(result))
}

# Aplicar extracción a todos los datos
cat("Extrayendo información política...\n")
political_info <- all_data %>%
  rowwise() %>%
  do(cbind(
    url = .$url,
    pais = .$pais,
    nombre = .$nombre,
    extract_political_info(.$infobox_json, .$cargos_politicos, .$ocupacion)
  )) %>%
  ungroup()

cat("✅ Información política extraída para", nrow(political_info), "personas\n")

# Resumen de cargos por país
cat("\n📊 Distribución de cargos por país:\n")
cargos_summary <- political_info %>%
  group_by(pais, cargo_tipo) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(pais, desc(count))
print(cargos_summary)

# ============================================================================
# ANÁLISIS 3: Crear nodos enriquecidos
# ============================================================================

cat("\n🎨 ANÁLISIS 3: Creando red enriquecida...\n")
cat(strrep("=", 80), "\n")

# Crear nodos completos (incluyendo desconocidos con info parcial)
nodes_complete <- tibble(url = unique(c(edges_all$from, edges_all$to))) %>%
  left_join(all_data %>% select(url, nombre, pais, familia), by = "url") %>%
  left_join(political_info, by = c("url", "nombre", "pais")) %>%
  mutate(
    nombre = ifelse(is.na(nombre), str_extract(url, "(?<=wiki/)[^$]+"), nombre),
    nombre = str_replace_all(nombre, "_", " "),
    nombre = str_replace_all(nombre, "%C3%A1", "á"),
    nombre = str_replace_all(nombre, "%C3%A9", "é"),
    nombre = str_replace_all(nombre, "%C3%AD", "í"),
    nombre = str_replace_all(nombre, "%C3%B3", "ó"),
    nombre = str_replace_all(nombre, "%C3%BA", "ú"),
    nombre = str_replace_all(nombre, "%C3%B1", "ñ"),
    pais = ifelse(is.na(pais), "Desconocido", pais),
    cargo_tipo = ifelse(is.na(cargo_tipo), "Sin información", cargo_tipo),
    cargo_nivel = ifelse(is.na(cargo_nivel), "Desconocido", cargo_nivel)
  )

# Agregar información de país destino a las aristas
edges_enriched_final <- edges_all %>%
  left_join(nodes_complete %>% select(url, pais, cargo_tipo) %>% 
              rename(to_pais = pais, to_cargo = cargo_tipo), 
            by = c("to" = "url"))

cat("✅ Nodos totales:", nrow(nodes_complete), "\n")
cat("   - Chile:", sum(nodes_complete$pais == "Chile"), "\n")
cat("   - Argentina:", sum(nodes_complete$pais == "Argentina"), "\n")
cat("   - Desconocido:", sum(nodes_complete$pais == "Desconocido"), "\n")

# ============================================================================
# ANÁLISIS 4: Conexiones entre países con contexto
# ============================================================================

cat("\n🌎 ANÁLISIS 4: Detectando conexiones enriquecidas entre países...\n")
cat(strrep("=", 80), "\n")

# Conexiones directas entre países
cross_country_enriched <- edges_enriched_final %>%
  filter(
    !is.na(from_pais), !is.na(to_pais),
    from_pais != "Desconocido", to_pais != "Desconocido",
    from_pais != to_pais
  )

cat("✅ Conexiones directas entre países:", nrow(cross_country_enriched), "\n")

if (nrow(cross_country_enriched) > 0) {
  cat("\n🔗 Análisis de conexiones por tipo de relación:\n")
  conexiones_tipo <- cross_country_enriched %>%
    group_by(from_pais, to_pais, relation_type) %>%
    summarise(count = n(), .groups = "drop") %>%
    arrange(desc(count))
  print(conexiones_tipo)
  
  cat("\n🔗 Análisis de conexiones por cargo político:\n")
  conexiones_cargo <- cross_country_enriched %>%
    filter(!is.na(to_cargo), to_cargo != "Sin información") %>%
    group_by(from_pais, to_pais, to_cargo) %>%
    summarise(count = n(), .groups = "drop") %>%
    arrange(desc(count))
  print(conexiones_cargo)
}

# Conexiones políticas/diplomáticas especiales
cat("\n👔 Análisis de conexiones políticas/diplomáticas:\n")

political_connections <- edges_enriched_final %>%
  left_join(political_info %>% select(url, cargo_tipo, cargo_es_embajador, cargo_es_diplomatico),
            by = c("from" = "url")) %>%
  filter(
    (cargo_es_embajador == TRUE | cargo_es_diplomatico == TRUE) &
    from_pais != to_pais &
    to_pais != "Desconocido"
  )

if (nrow(political_connections) > 0) {
  cat("✅ Conexiones de embajadores/diplomáticos entre países:", nrow(political_connections), "\n")
  print(political_connections %>%
    select(from_pais, to_pais, relation_type, cargo_tipo) %>%
    head(20))
}

# ============================================================================
# ANÁLISIS 5: Apellidos y familias cruzadas
# ============================================================================

cat("\n👨‍👩‍👧‍👦 ANÁLISIS 5: Familias y apellidos comunes...\n")
cat(strrep("=", 80), "\n")

# Extraer apellidos
nodes_surnames <- nodes_complete %>%
  filter(pais %in% c("Chile", "Argentina")) %>%
  mutate(
    name_parts = str_split(nombre, "\\s+"),
    surname_1 = map_chr(name_parts, ~ if(length(.x) >= 1) tail(.x, 1)[1] else ""),
    surname_2 = map_chr(name_parts, ~ if(length(.x) >= 2) tail(.x, 2)[1] else "")
  ) %>%
  filter(surname_1 != "", !is.na(surname_1))

# Apellidos comunes por cargo
apellidos_con_cargo <- nodes_surnames %>%
  filter(cargo_tipo != "Sin información") %>%
  select(url, pais, surname_1, surname_2, cargo_tipo) %>%
  pivot_longer(cols = c(surname_1, surname_2), names_to = "pos", values_to = "surname") %>%
  filter(surname != "", !is.na(surname), nchar(surname) > 2) %>%
  group_by(surname, pais, cargo_tipo) %>%
  summarise(count = n(), .groups = "drop")

# Apellidos en ambos países
apellidos_compartidos <- apellidos_con_cargo %>%
  group_by(surname) %>%
  summarise(
    has_chile = any(pais == "Chile"),
    has_argentina = any(pais == "Argentina"),
    n_chile = sum(ifelse(pais == "Chile", count, 0)),
    n_argentina = sum(ifelse(pais == "Argentina", count, 0)),
    cargos = paste(unique(cargo_tipo), collapse = ", "),
    total = sum(count)
  ) %>%
  filter(has_chile & has_argentina, nchar(surname) > 2, !surname %in% c("de", "del", "la", "los", "las")) %>%
  arrange(desc(total))

cat("✅ Apellidos políticos compartidos:", nrow(apellidos_compartidos), "\n")
if (nrow(apellidos_compartidos) > 0) {
  cat("\nTop apellidos compartidos:\n")
  print(apellidos_compartidos %>% head(20))
}

# ============================================================================
# VISUALIZACIÓN
# ============================================================================

cat("\n🎨 Generando visualizaciones enriquecidas...\n")
cat(strrep("=", 80), "\n")

# Crear grafo
g_tbl <- tbl_graph(nodes = nodes_complete, edges = edges_enriched_final, directed = TRUE) %>%
  activate(nodes) %>%
  mutate(
    degree = centrality_degree(mode = "all"),
    in_degree = centrality_degree(mode = "in"),
    out_degree = centrality_degree(mode = "out"),
    pais_color = case_when(
      pais == "Chile" ~ "Chile",
      pais == "Argentina" ~ "Argentina",
      TRUE ~ "Otro/Desconocido"
    ),
    node_size = case_when(
      cargo_tipo == "Presidente" ~ 3,
      cargo_tipo == "Embajador" ~ 2.5,
      cargo_tipo == "Ministro" ~ 2.5,
      cargo_tipo == "Diplomático" ~ 2,
      TRUE ~ 1
    )
  )

# Colores
country_colors <- c(
  "Chile" = "#0033A0",
  "Argentina" = "#6CACE4",
  "Otro/Desconocido" = "gray70"
)

# Visualización 1: Red completa con nodos por cargo
p1 <- ggraph(g_tbl, layout = "fr") +
  geom_edge_link(
    aes(color = relation_type),
    alpha = 0.15,
    arrow = arrow(length = unit(1, "mm"), type = "closed")
  ) +
  geom_node_point(aes(size = degree * node_size, color = pais_color, shape = cargo_tipo), alpha = 0.7) +
  scale_size_continuous(range = c(0.5, 5), name = "Grado") +
  scale_color_manual(values = country_colors, name = "País") +
  scale_shape_manual(
    values = c("Presidente" = 17, "Embajador" = 15, "Ministro" = 16, 
               "Diplomático" = 18, "Otro político" = 3, 
               "Sin cargo político" = 1, "Sin información" = 4),
    name = "Cargo"
  ) +
  labs(
    title = "Red Familiar Chile-Argentina Enriquecida",
    subtitle = paste(
      "Nodos:", nrow(nodes_complete),
      "| Relaciones:", nrow(edges_enriched_final),
      "| Conexiones entre países:", nrow(cross_country_enriched)
    )
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    legend.position = "right",
    legend.box = "vertical"
  )

p1

# Guardar
dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs/tables", showWarnings = FALSE, recursive = TRUE)

ggsave("outputs/figures/red_chile_argentina_enriquecida.png", p1, 
       width = 24, height = 18, dpi = 300)
cat("✅ Guardado: outputs/figures/red_chile_argentina_enriquecida.png\n")

# Guardar tablas
write_csv(unknown_analysis, "outputs/tables/analisis_desconocidos.csv")
write_csv(unknown_examples, "outputs/tables/ejemplos_desconocidos.csv")
write_csv(cargos_summary, "outputs/tables/cargos_por_pais.csv")

if (nrow(cross_country_enriched) > 0) {
  write_csv(cross_country_enriched, "outputs/tables/conexiones_enriquecidas_chile_argentina.csv")
}

if (nrow(apellidos_compartidos) > 0) {
  write_csv(apellidos_compartidos, "outputs/tables/apellidos_politicos_compartidos.csv")
}

# Resumen final
cat("\n", strrep("=", 80), "\n")
cat("📊 RESUMEN FINAL ENRIQUECIDO\n")
cat(strrep("=", 80), "\n")
cat("   Total de nodos (incluye desconocidos):", nrow(nodes_complete), "\n")
cat("   - Chile:", sum(nodes_complete$pais == "Chile"), "\n")
cat("   - Argentina:", sum(nodes_complete$pais == "Argentina"), "\n")
cat("   - Desconocidos:", sum(nodes_complete$pais == "Desconocido"), "\n")
cat("   Total de relaciones:", nrow(edges_enriched_final), "\n")
cat("   Conexiones directas entre países:", nrow(cross_country_enriched), "\n")
cat("   Conexiones políticas/diplomáticas:", nrow(political_connections), "\n")
cat("   Apellidos políticos compartidos:", nrow(apellidos_compartidos), "\n")
cat(strrep("=", 80), "\n")
