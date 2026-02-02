# ============================================================================
# paper_redes_multipais.R
# Reporte tipo paper: redes multi-país, endogamia, métricas y limpieza JSON
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

cat("📄 Paper: Redes Multi-País y Endogamia\n")
cat(strrep("=", 80), "\n")

# Configuración de países (prioridad solicitada por el usuario)
paises_disponibles <- c("chile", "argentina", "mexico", "peru", "colombia", "venezuela")
nombres_paises <- c("Chile", "Argentina", "México", "Perú", "Colombia", "Venezuela")

outputs_dir <- "outputs"
figures_dir <- file.path(outputs_dir, "figures")
tables_dir <- file.path(outputs_dir, "tables")
reports_dir <- file.path(outputs_dir, "reports")
dir.create(figures_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(tables_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(reports_dir, showWarnings = FALSE, recursive = TRUE)

# ----------------------------------------------------------------------------
# Utilidades de limpieza JSON
# ----------------------------------------------------------------------------
clean_json_text <- function(x) {
  if (is.na(x)) return(NA_character_)
  x <- str_replace_all(x, "[\\x00-\\x1F\\x7F]", " ")
  x <- str_squish(x)
  if (x == "") return(NA_character_)
  if (jsonlite::validate(x)) return(x)
  # Intento simple de limpieza adicional
  x2 <- str_replace_all(x, "\\\\'", "'")
  x2 <- str_replace_all(x2, "\\\\\"", "\"")
  x2 <- str_replace_all(x2, "\\\\n", " ")
  x2 <- str_squish(x2)
  if (jsonlite::validate(x2)) return(x2)
  return(NA_character_)
}

safe_parse_json <- function(x) {
  if (is.na(x) || x == "") return(NULL)
  tryCatch(fromJSON(x), error = function(e) NULL)
}

# ----------------------------------------------------------------------------
# Cargar datos
# ----------------------------------------------------------------------------
cat("\n📂 Cargando datos...\n")
all_countries_data <- list()

for (i in seq_along(paises_disponibles)) {
  pais <- paises_disponibles[i]
  nombre_pais <- nombres_paises[i]
  file_path <- paste0("data/raw/", pais, "/familias/_CONSOLIDADO_todas_familias.csv")

  if (file.exists(file_path)) {
    cat("  ✓ Cargando", nombre_pais, "...\n")
    data <- read_delim(
      file_path,
      delim = ";",
      show_col_types = FALSE,
      locale = locale(encoding = "UTF-8")
    )
    data <- data %>%
      filter(!is.na(url), as.character(url) != "") %>%
      mutate(
        pais = nombre_pais,
        infobox_json_clean = vapply(infobox_json, clean_json_text, character(1)),
        infobox_json_valido = !is.na(infobox_json_clean)
      )

    all_countries_data[[nombre_pais]] <- data
    cat("    →", nrow(data), "personas\n")
  } else {
    cat("  ⚠️ No se encontró:", file_path, "\n")
  }
}

if (length(all_countries_data) == 0) {
  stop("❌ No se encontraron datos de ningún país")
}

all_data <- bind_rows(all_countries_data) %>%
  distinct(url, .keep_all = TRUE)

# Filtrar solo países solicitados y disponibles
paises_disponibles_nombres <- intersect(nombres_paises, names(all_countries_data))
all_data <- all_data %>% filter(pais %in% paises_disponibles_nombres)

# Paleta de colores y centros aproximados por país
country_colors <- c(
  "Chile" = "#0033A0",
  "Argentina" = "#6CACE4",
  "México" = "#006847",
  "Perú" = "#D91023",
  "Colombia" = "#FCD116",
  "Venezuela" = "#CF142B",
  "Uruguay" = "#0038A8",
  "Bolivia" = "#007A33",
  "Ecuador" = "#FFD100",
  "Paraguay" = "#D52B1E",
  "Peru" = "#D91023",
  "Desconocido" = "gray70"
)

country_centers <- tibble::tribble(
  ~pais,       ~lat,   ~lon,
  "México",     23.6,  -102.5,
  "Colombia",    4.6,   -74.1,
  "Venezuela",   6.4,   -66.6,
  "Perú",      -9.2,   -75.0,
  "Chile",    -33.4,   -70.7,
  "Argentina",-34.6,   -58.4
) %>%
  mutate(
    x = (lon + 60) / 10,
    y = lat / 5
  )

# ----------------------------------------------------------------------------
# Limpieza JSON: resumen
# ----------------------------------------------------------------------------
json_limpieza_resumen <- all_data %>%
  group_by(pais) %>%
  summarise(
    n_total = n(),
    n_json_valido = sum(infobox_json_valido, na.rm = TRUE),
    pct_json_valido = round(100 * n_json_valido / n_total, 1),
    .groups = "drop"
  )

write_csv(json_limpieza_resumen, file.path(tables_dir, "json_limpieza_resumen.csv"))

# ----------------------------------------------------------------------------
# Funciones de relaciones y familia
# ----------------------------------------------------------------------------
extract_relation_urls <- function(relation_field) {
  if (is.na(relation_field) || relation_field == "") return(character())
  pattern <- "\\(https://es\\.wikipedia\\.org/wiki/([^)]+)\\)"
  matches <- str_match_all(relation_field, pattern)[[1]]
  if (nrow(matches) > 0) {
    return(paste0("https://es.wikipedia.org/wiki/", matches[, 2]))
  }
  return(character())
}

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
    if (nchar(familia_clean) > 0) return(familia_clean)
  }
  return(NA_character_)
}

extract_family_name_vec <- function(categoria_origen, familia_field) {
  mapply(
    extract_family_name,
    categoria_origen,
    familia_field,
    USE.NAMES = FALSE
  )
}

# ----------------------------------------------------------------------------
# Construcción de edges y nodos
# ----------------------------------------------------------------------------
cat("\n🔗 Extrayendo relaciones familiares...\n")
edges_list <- list()
for (i in seq_len(nrow(all_data))) {
  source_url <- all_data$url[i]
  source_pais <- all_data$pais[i]
  source_familia <- extract_family_name(all_data$categoria_origen[i], all_data$familia[i])

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

edges_all <- if (length(edges_list) > 0) bind_rows(edges_list) else tibble()

edges_enriched <- edges_all %>%
  left_join(
    all_data %>% select(url, pais, nombre, categoria_origen, familia),
    by = c("to" = "url")
  ) %>%
  mutate(
    to_pais = pais,
    to_familia = extract_family_name_vec(categoria_origen, familia),
    connection_type = case_when(
      !is.na(to_pais) & from_pais == to_pais ~ "Intra-país",
      !is.na(to_pais) & from_pais != to_pais ~ "Inter-país",
      TRUE ~ "Desconocido"
    ),
    endogamia_familiar = case_when(
      !is.na(to_familia) & !is.na(from_familia) & from_familia == to_familia ~ "Intra-familia",
      !is.na(to_familia) & !is.na(from_familia) & from_familia != to_familia ~ "Inter-familia",
      TRUE ~ "Desconocido"
    )
  )

all_urls <- unique(c(edges_enriched$from, edges_enriched$to))
nodes <- tibble(url = all_urls) %>%
  left_join(all_data %>% select(url, nombre, pais, categoria_origen, familia, cargos_politicos), by = "url") %>%
  mutate(
    nombre = ifelse(is.na(nombre), str_replace_all(str_extract(url, "(?<=wiki/).+"), "_", " "), nombre),
    familia = extract_family_name_vec(categoria_origen, familia),
    pais = ifelse(is.na(pais), "Desconocido", pais)
  )

# ----------------------------------------------------------------------------
# Endogamia por país
# ----------------------------------------------------------------------------
endogamia_por_pais <- edges_enriched %>%
  filter(relation_type %in% c("conyuge", "pareja")) %>%
  group_by(from_pais) %>%
  summarise(
    matrimonios_total = n(),
    matrimonios_endogamicos = sum(endogamia_familiar == "Intra-familia", na.rm = TRUE),
    tasa_endogamia_familiar = round(100 * matrimonios_endogamicos / matrimonios_total, 1),
    .groups = "drop"
  ) %>%
  rename(pais = from_pais)

write_csv(endogamia_por_pais, file.path(tables_dir, "endogamia_por_pais.csv"))

# ----------------------------------------------------------------------------
# Puestos políticos por país (top)
# ----------------------------------------------------------------------------
puestos_por_pais <- all_data %>%
  mutate(
    cargo_text = tolower(cargos_politicos),
    es_presidente = str_detect(cargo_text, "presidente"),
    es_ministro = str_detect(cargo_text, "ministro"),
    es_senador = str_detect(cargo_text, "senador"),
    es_diputado = str_detect(cargo_text, "diputado"),
    es_gobernador = str_detect(cargo_text, "gobernador"),
    es_alcalde = str_detect(cargo_text, "alcalde"),
    es_embajador = str_detect(cargo_text, "embajador")
  ) %>%
  group_by(pais) %>%
  summarise(
    n_presidentes = sum(es_presidente, na.rm = TRUE),
    n_ministros = sum(es_ministro, na.rm = TRUE),
    n_senadores = sum(es_senador, na.rm = TRUE),
    n_diputados = sum(es_diputado, na.rm = TRUE),
    n_gobernadores = sum(es_gobernador, na.rm = TRUE),
    n_alcaldes = sum(es_alcalde, na.rm = TRUE),
    n_embajadores = sum(es_embajador, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(puestos_por_pais, file.path(tables_dir, "puestos_politicos_por_pais_paper.csv"))

# ----------------------------------------------------------------------------
# Elites: personas con cargos políticos (intra e inter país)
# ----------------------------------------------------------------------------
elites_all <- all_data %>%
  mutate(es_elite = !is.na(cargos_politicos) & cargos_politicos != "")

elites_por_pais <- elites_all %>%
  group_by(pais) %>%
  summarise(
    n_total = n(),
    n_elite = sum(es_elite, na.rm = TRUE),
    pct_elite = round(100 * n_elite / n_total, 1),
    .groups = "drop"
  )

write_csv(elites_por_pais, file.path(tables_dir, "elites_por_pais.csv"))

# Aristas solo entre élites
elite_urls <- elites_all %>%
  filter(es_elite) %>%
  pull(url)

edges_elite <- edges_enriched %>%
  filter(from %in% elite_urls, to %in% elite_urls)

edges_elite_pais <- edges_elite %>%
  count(from_pais, to_pais, name = "n") %>%
  arrange(desc(n))

write_csv(edges_elite_pais, file.path(tables_dir, "elites_conexiones_inter_pais.csv"))

# Meta red de élites entre países
if (nrow(edges_elite_pais) > 0) {
  country_nodes_elite <- country_centers %>%
    filter(pais %in% unique(c(edges_elite_pais$from_pais, edges_elite_pais$to_pais)))

  g_country_elite <- tbl_graph(
    nodes = country_nodes_elite %>% select(pais, x, y),
    edges = edges_elite_pais %>% rename(from = from_pais, to = to_pais, weight = n),
    directed = TRUE
  ) %>%
    activate(nodes) %>%
    mutate(degree_total = centrality_degree(mode = "all"))

  p_globos_elite <- ggraph(g_country_elite, layout = "manual", x = x, y = y) +
    geom_edge_arc(
      aes(width = weight, color = "Inter-país"),
      strength = 0.35,
      alpha = 0.8,
      arrow = arrow(length = unit(2.2, "mm"), type = "closed")
    ) +
    scale_edge_width(range = c(1, 3.5), guide = "none") +
    scale_edge_color_manual(values = c("Inter-país" = "#FF6B6B"), name = "Conexión élite") +
    geom_node_point(aes(color = pais, size = degree_total), alpha = 0.9) +
    geom_node_text(aes(label = pais), size = 5, fontface = "bold", vjust = -1) +
    scale_color_manual(values = country_colors, drop = FALSE, na.value = "gray50") +
    scale_size_continuous(range = c(6, 14), name = "Grado total élite") +
    labs(
      title = "Red de élites entre países",
      subtitle = paste("Enlaces entre personas con cargos políticos. Total aristas:", nrow(edges_elite_pais)),
      caption = "Solo conexiones élite ↔ élite entre países"
    ) +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
      plot.subtitle = element_text(hjust = 0.5, size = 12),
      plot.caption = element_text(hjust = 0.5, size = 9, color = "gray50")
    )

  ggsave(file.path(figures_dir, "red_globos_elites_multipais.png"), p_globos_elite, width = 18, height = 12, dpi = 300)
} else {
  cat("⚠️ No hay aristas entre élites para dibujar meta-red.\n")
}

# ----------------------------------------------------------------------------
# Métricas de red por país
# ----------------------------------------------------------------------------
network_metrics <- list()

for (pais in unique(nodes$pais)) {
  if (pais == "Desconocido") next

  edges_pais <- edges_enriched %>%
    filter(from_pais == pais, to_pais == pais)

  nodes_pais <- nodes %>%
    filter(pais == pais)

  if (nrow(edges_pais) == 0 || nrow(nodes_pais) == 0) next

  g <- graph_from_data_frame(edges_pais, vertices = nodes_pais, directed = TRUE)
  g_und <- as.undirected(g, mode = "collapse")
  comp <- components(g_und)
  giant_idx <- which(comp$membership == which.max(comp$csize))
  g_giant <- induced_subgraph(g_und, V(g_und)[giant_idx])

  avg_path <- if (vcount(g_giant) > 1 && ecount(g_giant) > 0) {
    suppressWarnings(average.path.length(g_giant))
  } else {
    NA_real_
  }

  network_metrics[[pais]] <- tibble(
    pais = pais,
    n_nodes = vcount(g),
    n_edges = ecount(g),
    density = round(edge_density(g, loops = FALSE), 4),
    avg_degree = round(mean(degree(g_und)), 2),
    transitivity = round(transitivity(g_und, type = "global"), 4),
    n_componentes = comp$no,
    giant_size = max(comp$csize),
    avg_path_giant = round(avg_path, 3)
  )
}

network_metrics_tbl <- bind_rows(network_metrics)
write_csv(network_metrics_tbl, file.path(tables_dir, "metricas_red_por_pais.csv"))

# ----------------------------------------------------------------------------
# Matriz inter-país
# ----------------------------------------------------------------------------
matriz_inter_pais <- edges_enriched %>%
  filter(connection_type == "Inter-país") %>%
  count(from_pais, to_pais, sort = TRUE)

write_csv(matriz_inter_pais, file.path(tables_dir, "matriz_inter_pais.csv"))

# ----------------------------------------------------------------------------
# Visualización: red de globos entre países
# ----------------------------------------------------------------------------
cat("\n🎨 Generando red de globos...\n")

country_colors <- c(
  "Chile" = "#0033A0",
  "Argentina" = "#6CACE4",
  "México" = "#006847",
  "Perú" = "#D91023",
  "Colombia" = "#FCD116",
  "Venezuela" = "#CF142B",
  "Uruguay" = "#0038A8",
  "Bolivia" = "#007A33",
  "Ecuador" = "#FFD100",
  "Paraguay" = "#D52B1E",
  "Peru" = "#D91023",
  "Desconocido" = "gray70"
)

# Centros aproximados por país (lat, lon) para separar en "mapa"
country_centers <- tibble::tribble(
  ~pais,       ~lat,   ~lon,
  "México",     23.6,  -102.5,
  "Colombia",    4.6,   -74.1,
  "Venezuela",   6.4,   -66.6,
  "Perú",      -9.2,   -75.0,
  "Chile",    -33.4,   -70.7,
  "Argentina",-34.6,   -58.4
) %>%
  mutate(
    x = (lon + 60) / 10,
    y = lat / 5
  )

# Países presentes en datos (para filtrar centros)
paises_unicos <- unique(all_data$pais[all_data$pais != "Desconocido"])

# --- Meta red por país con curvas (claridad de conexiones) ------------------
country_nodes <- country_centers %>%
  filter(pais %in% paises_unicos)

country_edges <- edges_enriched %>%
  filter(from_pais != to_pais & !is.na(to_pais)) %>%
  count(from_pais, to_pais, name = "weight")

if (nrow(country_edges) > 0 && nrow(country_nodes) > 0) {
  g_country <- tbl_graph(
    nodes = country_nodes %>% select(pais, x, y),
    edges = country_edges %>% rename(from = from_pais, to = to_pais),
    directed = TRUE
  ) %>%
    activate(nodes) %>%
    mutate(
      degree_total = centrality_degree(mode = "all")
    )

  p_globos <- ggraph(g_country, layout = "manual", x = x, y = y) +
    geom_edge_arc(
      aes(width = weight, color = "Inter-país"),
      strength = 0.3,
      alpha = 0.75,
      arrow = arrow(length = unit(2.2, "mm"), type = "closed")
    ) +
    scale_edge_width(range = c(0.8, 3), guide = "none") +
    scale_edge_color_manual(values = c("Inter-país" = "#FF6B6B"), name = "Conexión") +
    geom_node_point(aes(color = pais, size = degree_total), alpha = 0.9) +
    geom_node_text(aes(label = pais), size = 5, fontface = "bold", vjust = -1) +
    scale_color_manual(values = country_colors, drop = FALSE, na.value = "gray50") +
    scale_size_continuous(range = c(6, 14), name = "Grado total") +
    labs(
      title = "Red Multi-País (meta): puentes entre países",
      subtitle = paste("Curvas = conexiones entre países (familiares / parentesco). Total:", nrow(country_edges)),
      caption = "Posiciones aproximadas en mapa; solo se muestran enlaces entre países (puentes)"
    ) +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
      plot.subtitle = element_text(hjust = 0.5, size = 12),
      plot.caption = element_text(hjust = 0.5, size = 9, color = "gray50")
    )

  ggsave(file.path(figures_dir, "red_globos_multipais_paper.png"), p_globos, width = 18, height = 12, dpi = 300)
} else {
  cat("⚠️ No hay conexiones entre países para dibujar meta-red.\n")
}

# ----------------------------------------------------------------------------
# Visualización: endogamia por país
# ----------------------------------------------------------------------------
p_endogamia <- endogamia_por_pais %>%
  ggplot(aes(x = reorder(pais, tasa_endogamia_familiar), y = tasa_endogamia_familiar)) +
  geom_col(fill = "#D91023", alpha = 0.8) +
  coord_flip() +
  labs(
    title = "Endogamia Familiar por País",
    x = "País",
    y = "Tasa de endogamia (%)"
  ) +
  theme_minimal()

ggsave(file.path(figures_dir, "endogamia_por_pais_paper.png"), p_endogamia, width = 12, height = 8, dpi = 300)

# ----------------------------------------------------------------------------
# Reporte tipo paper (Markdown)
# ----------------------------------------------------------------------------
cat("\n📝 Generando reporte...\n")

pais_mas_endogamico <- endogamia_por_pais %>%
  arrange(desc(tasa_endogamia_familiar)) %>%
  slice_head(n = 1)

report_path <- file.path(reports_dir, "paper_redes_multipais.md")

report_lines <- c(
  "# Redes familiares multi-país: endogamia y métricas",
  "",
  "## Resumen",
  paste0("- País más endogámico (matrimonios dentro de la familia): **", pais_mas_endogamico$pais, "** (", pais_mas_endogamico$tasa_endogamia_familiar, "%)."),
  paste0("- Total de personas analizadas: ", nrow(all_data), "."),
  paste0("- Conexiones entre países: ", nrow(matriz_inter_pais), "."),
  "",
  "## Datos y limpieza",
  "- Fuentes: consolidados por país en `data/raw/<pais>/familias/_CONSOLIDADO_todas_familias.csv`.",
  "- Limpieza de `infobox_json`: remoción de caracteres de control y validación JSON.",
  "",
  "### Resumen de limpieza JSON",
  "",
  paste0("Ver tabla: `", file.path(tables_dir, "json_limpieza_resumen.csv"), "`."),
  "",
  "## Metodología de redes",
  "- Vínculos: padres, cónyuge, pareja, hijos, hermanos.",
  "- Endogamia familiar: vínculos con misma familia (extracto desde `categoria_origen` o `familia`).",
  "- Conexiones inter-país: vínculos entre personas de distintos países.",
  "",
  "## Principales métricas de red por país",
  "",
  paste0("Ver tabla: `", file.path(tables_dir, "metricas_red_por_pais.csv"), "`."),
  "",
  "## Endogamia por país",
  "",
  paste0("Figura: `", file.path(figures_dir, "endogamia_por_pais_paper.png"), "`."),
  "",
  "## Puestos políticos por país",
  "",
  paste0("Ver tabla: `", file.path(tables_dir, "puestos_politicos_por_pais_paper.csv"), "`."),
  "",
  "## Red de globos entre países",
  "",
  paste0("Figura: `", file.path(figures_dir, "red_globos_multipais_paper.png"), "`."),
  "",
  "## Conexiones inter-país",
  "",
  paste0("Ver tabla: `", file.path(tables_dir, "matriz_inter_pais.csv"), "`.")
)

writeLines(report_lines, report_path)
cat("✅ Reporte guardado:", report_path, "\n")

cat("\n", strrep("=", 80), "\n")
cat("✅ Paper generado\n")
cat(strrep("=", 80), "\n")
