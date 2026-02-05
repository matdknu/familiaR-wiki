# ============================================================================
# red_globos_todos_pares.R
# Genera todas las combinaciones de pares de países (Chile–X, Venezuela–Y, etc.)
# y grafica cada par como dos globos conectados.
# Salida: outputs/figures/pares/{pais1}_{pais2}.png
# ============================================================================

library(readr)
library(dplyr)
library(igraph)
library(ggraph)
library(tidygraph)
library(ggplot2)
library(stringr)
library(tidyr)

cat("📊 Red globos: TODAS las combinaciones de pares de países\n")
cat(strrep("=", 80), "\n")

# Países (mismo orden que red_globos_multipais)
paises_disponibles <- c("chile", "argentina", "mexico", "peru", "colombia", "venezuela",
                       "bolivia", "paraguay", "uruguay", "ecuador")
nombres_paises <- c("Chile", "Argentina", "México", "Perú", "Colombia", "Venezuela",
                    "Bolivia", "Paraguay", "Uruguay", "Ecuador")

# Slug para nombres de archivo (sin espacios, minúscula, sin tildes)
pais_to_slug <- function(nombre) {
  nombre %>%
    tolower() %>%
    str_replace_all("é", "e") %>%
    str_replace_all("ú", "u") %>%
    str_replace_all("í", "i") %>%
    str_replace_all("ó", "o") %>%
    str_replace_all("á", "a") %>%
    str_replace_all(" ", "_")
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
    tryCatch({
      data <- read_delim(file_path, delim = ";", show_col_types = FALSE,
                        locale = locale(encoding = "UTF-8"))
      data <- data %>%
        filter(!is.na(url), as.character(url) != "") %>%
        mutate(pais = nombre_pais)
      all_countries_data[[nombre_pais]] <- data
    }, error = function(e) NULL)
  }
}

if (length(all_countries_data) == 0) stop("❌ No se encontraron datos.")

all_data <- bind_rows(all_countries_data) %>% distinct(url, .keep_all = TRUE)
cat("✅ Total:", nrow(all_data), "personas de", length(all_countries_data), "países\n")

# Extraer relaciones familiares (URLs)
extract_relation_urls <- function(relation_field) {
  if (is.na(relation_field) || relation_field == "") return(character())
  x <- as.character(relation_field)
  pattern <- "\\(https://es\\.wikipedia\\.org/wiki/([^)]+)\\)"
  matches <- str_match_all(x, pattern)[[1]]
  if (nrow(matches) > 0) return(paste0("https://es.wikipedia.org/wiki/", matches[, 2]))
  pattern2 <- "https://es\\.wikipedia\\.org/wiki/[^\\s)]+"
  urls <- str_extract_all(x, pattern2)[[1]]
  unique(trimws(urls))
}

rel_fields <- c("padres", "conyuge", "pareja", "hijos", "hermanos",
                "perfiles_relacionados_padres", "perfiles_relacionados_conyuge",
                "perfiles_relacionados_hijos", "perfiles_relacionados_familia")

edges_list <- list()
for (i in seq_len(nrow(all_data))) {
  source_url <- all_data$url[i]
  source_pais <- all_data$pais[i]
  for (field in rel_fields) {
    if (!field %in% names(all_data)) next
    target_urls <- extract_relation_urls(all_data[[field]][i])
    for (target_url in target_urls) {
      if (target_url != source_url && nchar(trimws(target_url)) > 0) {
        relation_type <- str_replace(field, "perfiles_relacionados_", "")
        edges_list[[length(edges_list) + 1]] <- tibble(
          from = source_url, to = target_url,
          relation_type = relation_type, from_pais = source_pais
        )
      }
    }
  }
}

edges_all <- bind_rows(edges_list) %>% distinct(from, to, .keep_all = TRUE)
cat("✅ Aristas totales:", nrow(edges_all), "\n")

# Enriquecer con país destino
edges_enriched <- edges_all %>%
  left_join(all_data %>% select(url, pais, nombre), by = c("to" = "url")) %>%
  rename(to_pais = pais, to_nombre = nombre) %>%
  left_join(all_data %>% select(url, nombre), by = c("from" = "url")) %>%
  rename(from_nombre = nombre)

# Nodos globales (todos los que aparecen en alguna arista)
all_urls <- unique(c(edges_enriched$from, edges_enriched$to))
nodes_full <- tibble(url = all_urls) %>%
  left_join(all_data %>% select(url, nombre, pais), by = "url") %>%
  mutate(
    nombre = if_else(is.na(nombre), str_replace_all(URLdecode(str_replace(str_replace(url, ".*wiki/", ""), "_", " ")), "%", " "), nombre),
    pais = if_else(is.na(pais) | trimws(pais) == "", "Desconocido", pais),
    tipo_nodo = if_else(pais == "Desconocido", "Desconocido", "Nacional")
  )

# Países que tienen al menos un nodo en la red
paises_con_datos <- unique(nodes_full$pais)
paises_con_datos <- setdiff(paises_con_datos, "Desconocido")
if (length(paises_con_datos) < 2) stop("❌ Se necesitan al menos 2 países con datos.")

# Todas las combinaciones de pares (sin orden: Chile–Argentina = Argentina–Chile)
pares <- t(combn(paises_con_datos, 2))
colnames(pares) <- c("pais_a", "pais_b")

dir.create("outputs/figures/pares", showWarnings = FALSE, recursive = TRUE)

# Colores por país (mismos que red_globos_multipais)
country_colors <- c(
  "Chile" = "#0033A0", "Argentina" = "#6CACE4", "México" = "#006847",
  "Perú" = "#D91023", "Peru" = "#D91023", "Colombia" = "#FCD116",
  "Venezuela" = "#CF142B", "Bolivia" = "#007A33", "Paraguay" = "#D52B1E",
  "Uruguay" = "#0038A8", "Ecuador" = "#FFD100", "Desconocido" = "gray70"
)

# Función: graficar un par de países y guardar
graficar_par <- function(pais_a, pais_b) {
  # Aristas dentro del “mundo” A–B: ambos extremos en A, B o Desconocido
  edges_ab <- edges_enriched %>%
    filter(from_pais %in% c(pais_a, pais_b, "Desconocido")) %>%
    filter(is.na(to_pais) | to_pais %in% c(pais_a, pais_b, "Desconocido"))
  urls_ab <- unique(c(edges_ab$from, edges_ab$to))
  nodes_ab <- nodes_full %>% filter(url %in% urls_ab)
  if (nrow(nodes_ab) < 2) return(invisible(NULL))
  # Solo aristas cuyos extremos están en nodes_ab
  edges_ab <- edges_ab %>% filter(from %in% nodes_ab$url, to %in% nodes_ab$url)
  if (nrow(edges_ab) == 0) return(invisible(NULL))
  # Índices por país para layout
  idx_a <- which(nodes_ab$pais == pais_a)
  idx_b <- which(nodes_ab$pais == pais_b)
  idx_unk <- which(nodes_ab$pais == "Desconocido")
  if (length(idx_a) == 0 && length(idx_b) == 0) return(invisible(NULL))
  g_igraph <- graph_from_data_frame(edges_ab, directed = TRUE,
                                   vertices = nodes_ab %>% select(url, nombre, pais, tipo_nodo))
  g_tbl <- as_tbl_graph(g_igraph) %>%
    activate(nodes) %>%
    mutate(degree = centrality_degree(mode = "all"))
  g_igraph <- as.igraph(g_tbl)
  nv <- vcount(g_igraph)
  layout_combined <- matrix(0, nv, 2)
  # Globo izquierdo = pais_a
  if (length(idx_a) > 0) {
    sub_a <- induced_subgraph(g_igraph, idx_a)
    lay_a <- layout_with_fr(sub_a, niter = 200)
    lay_a <- scale(lay_a) * 0.8
    layout_combined[idx_a, 1] <- lay_a[, 1] - 4
    layout_combined[idx_a, 2] <- lay_a[, 2]
  }
  # Globo derecho = pais_b
  if (length(idx_b) > 0) {
    sub_b <- induced_subgraph(g_igraph, idx_b)
    lay_b <- layout_with_fr(sub_b, niter = 200)
    lay_b <- scale(lay_b) * 0.8
    layout_combined[idx_b, 1] <- lay_b[, 1] + 4
    layout_combined[idx_b, 2] <- lay_b[, 2]
  }
  # Desconocidos en el centro
  if (length(idx_unk) > 0) {
    layout_combined[idx_unk, 1] <- runif(length(idx_unk), -1.5, 1.5)
    layout_combined[idx_unk, 2] <- runif(length(idx_unk), -2, 2)
  }
  cross_n <- edges_ab %>%
    filter(from_pais != to_pais, from_pais %in% c(pais_a, pais_b), to_pais %in% c(pais_a, pais_b)) %>%
    nrow()
  g_tbl <- g_tbl %>% activate(nodes) %>% mutate(x = layout_combined[, 1], y = layout_combined[, 2])
  col_ab <- country_colors[names(country_colors) %in% c(pais_a, pais_b, "Desconocido")]
  p <- ggraph(g_tbl, layout = "manual", x = x, y = y) +
    geom_edge_link(
      aes(color = ifelse(from_pais != to_pais & !is.na(to_pais) & from_pais %in% c(pais_a, pais_b) & to_pais %in% c(pais_a, pais_b), "cross", "normal")),
      alpha = 0.35, linewidth = 0.4
    ) +
    scale_edge_color_manual(
      values = c("cross" = "#C0392B", "normal" = "gray75"),
      name = "Conexión",
      labels = c("cross" = "Entre países", "normal" = "Dentro del país")
    ) +
    geom_node_point(aes(size = degree + 0.5, fill = pais), shape = 21, colour = "gray25", stroke = 0.3) +
    scale_fill_manual(values = col_ab, name = "País", na.value = "gray70") +
    scale_size_continuous(range = c(1.5, 6), name = "Grado") +
    annotate("text", x = min(layout_combined[, 1]) - 0.8, y = max(layout_combined[, 2]) + 0.5,
             label = toupper(pais_a), size = 6, fontface = "bold",
             color = if (pais_a %in% names(country_colors)) country_colors[[pais_a]] else "gray40") +
    annotate("text", x = max(layout_combined[, 1]) + 0.8, y = max(layout_combined[, 2]) + 0.5,
             label = toupper(pais_b), size = 6, fontface = "bold",
             color = if (pais_b %in% names(country_colors)) country_colors[[pais_b]] else "gray40") +
    labs(
      title = paste0("Conexión ", pais_a, " — ", pais_b),
      subtitle = sprintf("Nodos: %d (%s: %d | %s: %d) | Conexiones entre países: %d",
        nrow(nodes_ab), pais_a, length(idx_a), pais_b, length(idx_b), cross_n)
    ) +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      plot.subtitle = element_text(hjust = 0.5, size = 9),
      legend.position = "right"
    )
  slug_a <- pais_to_slug(pais_a)
  slug_b <- pais_to_slug(pais_b)
  fname <- paste0("outputs/figures/pares/", slug_a, "_", slug_b, ".png")
  ggsave(fname, p, width = 12, height = 8, dpi = 150, bg = "white")
  list(
    pais_a = pais_a, pais_b = pais_b,
    nodos = nrow(nodes_ab), n_a = length(idx_a), n_b = length(idx_b),
    cross = cross_n, file = fname
  )
}

# Generar todas las combinaciones y graficar
cat("\n🎨 Generando gráficos por par de países...\n")
resultados <- list()
for (k in seq_len(nrow(pares))) {
  pais_a <- pares[k, "pais_a"]
  pais_b <- pares[k, "pais_b"]
  cat("  ", pais_a, " — ", pais_b, " ... ", sep = "")
  res <- graficar_par(pais_a, pais_b)
  if (!is.null(res)) {
    resultados[[length(resultados) + 1]] <- res
    cat("OK →", res$nodos, "nodos,", res$cross, "entre países\n")
  } else {
    cat("omitido (pocos nodos)\n")
  }
}

# Resumen
cat("\n", strrep("=", 80), "\n")
cat("📊 RESUMEN: COMBINACIONES DE PARES\n")
cat(strrep("=", 80), "\n")
cat("   Pares generados:", length(resultados), "de", nrow(pares), "combinaciones\n")
cat("   Figuras guardadas en: outputs/figures/pares/\n")
if (length(resultados) > 0) {
  res_df <- bind_rows(resultados)
  cat("\n   Por par:\n")
  for (i in seq_len(nrow(res_df))) {
    cat("   -", res_df$pais_a[i], "+", res_df$pais_b[i], ":", res_df$nodos[i], "nodos,", res_df$cross[i], "conexiones entre países\n")
  }
}
cat(strrep("=", 80), "\n")
cat("✅ Listo.\n")
