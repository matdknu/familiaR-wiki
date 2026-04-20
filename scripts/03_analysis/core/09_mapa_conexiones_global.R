#!/usr/bin/env Rscript
# ============================================================================
# 09_mapa_conexiones_global.R
# Mapa global de conexiones:
#   - Familiar: cónyuge/pareja/padre/hijo (`conexiones_consolidadas.csv`).
#   - Texto/diplomático: menciones de otros países en biografía, residencia,
#     cargos, etc. (`conexiones_diplomaticas_entre_paises.csv`, mismo criterio
#     que en 01_exploracion.R) → aristas persona → nodo "país (mención)".
# - Filtra por familias con más peso en la red familiar.
# - Figuras principales: SIN diagrama de red (evita “hairball”): matriz país×país
#   (heatmap) + barras de los pares con más vínculos. HTML opcional con plotly.
# Produce:
#   - outputs/tables/mapa_global_nodos.csv, mapa_global_aristas.csv, mapa_global_pares_paises.csv
#   - outputs/figures/mapa_global_conexiones.png (matriz + top pares)
#   - outputs/figures/mapa_global_red_paises_circulo.png (red agregada país–país, layout circular)
#   - outputs/figures/mapa_global_conexiones.html (heatmap interactivo si plotly está instalado)
# Requiere: igraph; ggplot2; ggraph (solo el gráfico circular país–país); tidyr; patchwork opcional; plotly opcional
# ============================================================================

# Paso 0: cargar librerías y paths
library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(igraph)
library(ggraph)
library(tidyr)
library(htmlwidgets)
library(htmltools)

DATA_CONEX   <- "outputs/tables/conexiones_consolidadas.csv"
DATA_DIPLO   <- "outputs/tables/conexiones_diplomaticas_entre_paises.csv"
DATA_PERSONAS <- "data/processed/02_leer_data/personas.rds"
OUT_FIG    <- "outputs/figures"
OUT_TABLE  <- "outputs/tables"
INCLUDE_VINCULOS_TEXTO <- TRUE
# Etiqueta visible en nodos (persona): familia truncada; nodos país: un poco más largo
MAX_CHARS_FAMILIA_LABEL <- 10L
MAX_CHARS_NODO_PAIS_LABEL <- 14L
dir.create(OUT_FIG, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_TABLE, recursive = TRUE, showWarnings = FALSE)

# --- Parámetros de foco (familias + tamaño) ---
N_TOP_FAMILIAS <- 45L
# "ambos": solo aristas entre dos familias del ranking (más denso entre élites).
# "al_menos_uno": cualquier vínculo donde al menos un lado sea familia top.
FILTRO_FAMILIAS <- "al_menos_uno"
MAX_NODOS       <- 500L

# Orden visual de países (columnas de izquierda a derecha). El resto se añade al final.
PAISES_ORDEN <- c(
  "mexico", "colombia", "venezuela", "peru", "ecuador", "bolivia",
  "chile", "argentina", "uruguay", "paraguay", "brazil", "brasil",
  "spain", "españa", "france", "francia", "italy", "italia",
  "usa", "united states", "united kingdom", "uk", "germany", "alemania"
)

# Paso 1: leer datos consolidados
con <- read_csv(DATA_CONEX, show_col_types = FALSE)

# Paso 2: filtrar tipos de relación clave
tipos_keep <- c("Cónyuge", "Pareja", "Padre/Madre", "Hijo/a")
con <- con %>% filter(tipo_label %in% tipos_keep)

# Paso 2b: ranking de familias por peso en la tabla (apariciones en ambos lados)
fam_w <- bind_rows(
  con %>% count(familia, name = "w") %>%
    filter(!is.na(familia), nzchar(trimws(familia))),
  con %>% count(relacionado_familia, name = "w") %>%
    filter(!is.na(relacionado_familia), nzchar(trimws(relacionado_familia))) %>%
    rename(familia = relacionado_familia)
) %>%
  group_by(familia) %>%
  summarise(peso = sum(w), .groups = "drop") %>%
  arrange(desc(peso))

top_fams <- head(fam_w$familia, N_TOP_FAMILIAS)

if (FILTRO_FAMILIAS == "ambos") {
  con <- con %>%
    filter(
      !is.na(familia), familia %in% top_fams,
      !is.na(relacionado_familia), relacionado_familia %in% top_fams
    )
} else {
  con <- con %>% filter(
    (familia %in% top_fams | relacionado_familia %in% top_fams)
  )
}

cat(sprintf(
  "Filtro familias: top %d | modo=%s | filas conexiones: %d\n",
  N_TOP_FAMILIAS, FILTRO_FAMILIAS, nrow(con)
))

# Paso 3: normalizar nombres para colapsar duplicados visibles
con <- con %>%
  mutate(
    nombre_norm = case_when(
      grepl("Menem Jr", nombre, ignore.case = TRUE) ~ "Carlos Menem Jr.",
      TRUE ~ nombre
    ),
    relacionado_nombre_norm = case_when(
      grepl("Menem Jr", relacionado_nombre, ignore.case = TRUE) ~ "Carlos Menem Jr.",
      TRUE ~ relacionado_nombre
    )
  )

# Paso 4: construir aristas (from_id, to_id) y nodos
edges <- con %>%
  transmute(
    from_id = persona_id,
    to_id   = relacionado_id,
    from_nombre = nombre_norm,
    to_nombre   = relacionado_nombre_norm,
    tipo = tipo_label,
    cross_country,
    cross_family
  )

# Paso 5: excluir filas sin ninguno de los dos nombres (para estabilidad)
edges <- edges %>% filter(!is.na(from_nombre) | !is.na(to_nombre))

# Paso 6: crear tabla de nodos (unión de lados)
nodos_from <- con %>%
  transmute(
    persona_id,
    nombre = nombre_norm,
    familia,
    pais,
    origen = "izq"
  )
nodos_to <- con %>%
  transmute(
    persona_id = relacionado_id,
    nombre = relacionado_nombre_norm,
    familia = relacionado_familia,
    pais = relacionado_pais,
    origen = "der"
  )
nodos <- bind_rows(nodos_from, nodos_to) %>%
  filter(!is.na(persona_id) | !is.na(nombre)) %>%
  mutate(persona_id = ifelse(is.na(persona_id), -row_number() - 1e6, persona_id)) %>%
  group_by(persona_id, nombre) %>%
  summarise(
    familia = first(na.omit(familia)),
    pais = first(na.omit(pais)),
    .groups = "drop"
  )

# Paso 7: recalcular aristas usando IDs virtuales ya asignados a faltantes
lookup_nombre <- nodos %>%
  filter(is.na(persona_id) | persona_id < 0) %>%
  select(persona_id, nombre)

edges <- edges %>%
  mutate(
    from_id_final = ifelse(is.na(from_id),
                           lookup_nombre$persona_id[match(from_nombre, lookup_nombre$nombre)],
                           from_id),
    to_id_final = ifelse(is.na(to_id),
                         lookup_nombre$persona_id[match(to_nombre, lookup_nombre$nombre)],
                         to_id)
  ) %>%
  filter(!is.na(from_id_final), !is.na(to_id_final))

# Paso 8: construir grafo y calcular grado para priorizar nodos
g <- graph_from_data_frame(
  d = edges %>% select(from = from_id_final, to = to_id_final, tipo, cross_country, cross_family),
  directed = FALSE,
  vertices = nodos %>% rename(name = persona_id)
)
deg <- degree(g)

# Paso 9: limitar tamaño (después del filtro por familia)
keep_nodes <- names(sort(deg, decreasing = TRUE))
if (length(keep_nodes) > MAX_NODOS) keep_nodes <- keep_nodes[seq_len(MAX_NODOS)]
g_sub <- induced_subgraph(g, keep_nodes)
deg_sub <- degree(g_sub)
V(g_sub)$grado <- deg_sub

# Paso 10: extraer tablas finales para exportar
nombre_attr <- V(g_sub)$nombre
familia_attr <- V(g_sub)$familia
pais_attr <- V(g_sub)$pais
if (is.null(nombre_attr)) nombre_attr <- rep(NA_character_, vcount(g_sub))
if (is.null(familia_attr)) familia_attr <- rep(NA_character_, vcount(g_sub))
if (is.null(pais_attr)) pais_attr <- rep(NA_character_, vcount(g_sub))

nodos_final <- tibble(
  persona_id = as.integer(V(g_sub)$name),
  nombre = nombre_attr,
  familia = familia_attr,
  pais = pais_attr,
  grado = as.integer(deg_sub)
)

edges_fam_export <- as_data_frame(g_sub, what = "edges") %>%
  transmute(
    from = as.character(from),
    to = as.character(to),
    tipo = E(g_sub)$tipo,
    cross_country = E(g_sub)$cross_country,
    cross_family = E(g_sub)$cross_family,
    edge_kind = "familiar",
    campo = NA_character_
  )

norm_pais <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x[is.na(x) | !nzchar(x)] <- "desconocido"
  x
}

# --- Vínculos texto / diplomáticos (persona → país mencionado en campos) ---
edges_diplo_export <- tibble(
  from = character(),
  to = character(),
  tipo = character(),
  cross_country = logical(),
  cross_family = logical(),
  edge_kind = character(),
  campo = character()
)

if (INCLUDE_VINCULOS_TEXTO && file.exists(DATA_DIPLO)) {
  personas_url <- readRDS(DATA_PERSONAS) %>%
    transmute(persona_id, url = as.character(url))
  diplo_raw <- read_csv(DATA_DIPLO, show_col_types = FALSE)
  diplo_j <- diplo_raw %>%
    inner_join(personas_url, by = "url") %>%
    filter(persona_id %in% nodos_final$persona_id) %>%
    mutate(
      pais_v_key = norm_pais(pais_vinculo),
      to_node = paste0("pais:", pais_v_key)
    ) %>%
    distinct(persona_id, to_node, tipo_vinculo, campo_detectado)

  if (nrow(diplo_j) > 0L) {
    edges_diplo_export <- diplo_j %>%
      transmute(
        from = as.character(persona_id),
        to = to_node,
        tipo = paste0("Texto (", tipo_vinculo, ")"),
        cross_country = TRUE,
        cross_family = NA,
        edge_kind = "texto",
        campo = as.character(campo_detectado)
      )
    cat(sprintf("Vínculos texto/diplomático (en subred familiar): %d aristas\n", nrow(edges_diplo_export)))
  }
} else if (INCLUDE_VINCULOS_TEXTO) {
  message("No se encontró ", DATA_DIPLO, " — solo red familiar.")
}

# --- Nodos persona + nodos país (ancla) para layout y export ---
nodos_personas <- nodos_final %>%
  mutate(
    pais_key = norm_pais(pais),
    es_nodo_pais = FALSE,
    node_key = as.character(persona_id)
  )

country_rows <- edges_diplo_export %>%
  distinct(to) %>%
  transmute(
    node_key = to,
    pais_key = str_remove(to, "^pais:"),
    nombre = paste0("→ ", str_to_title(str_replace_all(pais_key, "_", " "))),
    persona_id = NA_integer_,
    familia = NA_character_,
    pais = pais_key,
    grado = 0L,
    es_nodo_pais = TRUE
  )

paises_presentes <- sort(unique(c(nodos_personas$pais_key, country_rows$pais_key)))
ord_pais <- c(
  intersect(PAISES_ORDEN, paises_presentes),
  sort(setdiff(paises_presentes, PAISES_ORDEN))
)
idx_p <- match(nodos_personas$pais_key, ord_pais)
idx_p[is.na(idx_p)] <- length(ord_pais) + 1L

nodos_personas <- nodos_personas %>%
  mutate(
    col_idx = idx_p,
    x_layout = (col_idx - 1L) * 420,
    seq_en_pais = ave(seq_len(n()), pais_key, FUN = seq_along),
    n_en_pais = ave(seq_len(n()), pais_key, FUN = length),
    y_layout = (seq_en_pais - (n_en_pais + 1) / 2) * 38
  ) %>%
  select(-seq_en_pais, -n_en_pais)

max_y_pais <- nodos_personas %>%
  group_by(pais_key) %>%
  summarise(y_base = max(y_layout), .groups = "drop")

country_rows <- country_rows %>%
  mutate(
    col_idx = match(pais_key, ord_pais),
    col_idx = ifelse(is.na(col_idx), length(ord_pais) + 1L, col_idx),
    x_layout = (col_idx - 1L) * 420
  ) %>%
  left_join(max_y_pais, by = "pais_key") %>%
  mutate(y_layout = coalesce(y_base, 0) + 300) %>%
  select(-y_base)

nodos_out <- bind_rows(
  nodos_personas,
  country_rows %>% select(-col_idx)
)

edges_out <- bind_rows(edges_fam_export, edges_diplo_export) %>%
  distinct(from, to, tipo, edge_kind, campo, .keep_all = TRUE)

write_csv(nodos_out, file.path(OUT_TABLE, "mapa_global_nodos.csv"))
write_csv(edges_out, file.path(OUT_TABLE, "mapa_global_aristas.csv"))

# --- Aristas resumidas + tabla país–país (sin grafo “red”) ---
edges_simplify <- edges_out %>%
  select(from, to, tipo, cross_country, cross_family, edge_kind) %>%
  distinct(from, to, tipo, .keep_all = TRUE) %>%
  left_join(nodos_out %>% select(node_key, pais_from = pais_key), by = c("from" = "node_key")) %>%
  left_join(nodos_out %>% select(node_key, pais_to_n = pais_key), by = c("to" = "node_key")) %>%
  mutate(
    pais_to = case_when(
      str_detect(to, "^pais:") ~ norm_pais(str_remove(to, "^pais:")),
      TRUE ~ pais_to_n
    ),
    es_transnacional = edge_kind == "texto" |
      (!is.na(pais_from) & !is.na(pais_to) & pais_from != pais_to)
  ) %>%
  select(from, to, tipo, cross_country, cross_family, edge_kind, es_transnacional, pais_from, pais_to)

pares_paises <- edges_simplify %>%
  filter(es_transnacional, !is.na(pais_from), !is.na(pais_to)) %>%
  transmute(
    a = pmin(pais_from, pais_to),
    b = pmax(pais_from, pais_to)
  ) %>%
  count(a, b, name = "n_aristas") %>%
  arrange(desc(n_aristas))
write_csv(pares_paises, file.path(OUT_TABLE, "mapa_global_pares_paises.csv"))

# --- Figuras: matriz + barras (no diagrama de nodos/aristas) ---
paises_en_red <- sort(unique(c(pares_paises$a, pares_paises$b)))
orden_paises <- c(
  intersect(PAISES_ORDEN, paises_en_red),
  sort(setdiff(paises_en_red, PAISES_ORDEN))
)

mat_tiles <- if (length(orden_paises) >= 2L && nrow(pares_paises) > 0L) {
  tidyr::crossing(
    fila = factor(orden_paises, levels = orden_paises),
    col = factor(orden_paises, levels = orden_paises)
  ) %>%
    mutate(
      a = pmin(as.character(fila), as.character(col)),
      b = pmax(as.character(fila), as.character(col))
    ) %>%
    left_join(pares_paises, by = c("a", "b")) %>%
    mutate(
      n = if_else(as.character(fila) == as.character(col), NA_real_, replace_na(n_aristas, 0))
    )
} else {
  tibble()
}

p_mat <- if (nrow(mat_tiles) > 0L) {
  ggplot(mat_tiles, aes(x = col, y = fila, fill = n)) +
    geom_tile(color = "white", linewidth = 0.35) +
    geom_text(
      aes(label = if_else(is.na(n) | n == 0, "", as.character(n))),
      size = 2.8,
      color = "gray15"
    ) +
    scale_fill_viridis_c(
      option = "C",
      na.value = "grey92",
      trans = "sqrt",
      name = "N connections\n(between countries)"
    ) +
    labs(
      title = "Cross-national connections (aggregated matrix)",
      subtitle = paste0(
        "Subgraph: top ", N_TOP_FAMILIAS, " families (", FILTRO_FAMILIAS, "). ",
        "Each off-diagonal cell = kinship or text-based edges for that pair. Diagonal empty. ",
        "Persons in subgraph: ", sum(!nodos_out$es_nodo_pais), "."
      ),
      x = "Country (destination)",
      y = "Country (origin)"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(color = "gray35", hjust = 0.5, size = 9),
      panel.grid = element_blank(),
      axis.text.x = element_text(angle = 40, hjust = 1)
    )
} else {
  ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = "Sin pares transnacionales en esta subred") +
    theme_void()
}

p_bar <- if (nrow(pares_paises) > 0L) {
  pares_paises %>%
    slice_head(n = 28L) %>%
    mutate(
      par = paste(str_to_title(str_replace_all(a, "_", " ")), str_to_title(str_replace_all(b, "_", " ")), sep = " ↔ ")
    ) %>%
    ggplot(aes(x = reorder(par, n_aristas), y = n_aristas)) +
    geom_col(fill = "#2c3e50", width = 0.85) +
    coord_flip() +
    labs(
      title = "Country pairs with most connections",
      x = NULL,
      y = "Number of edges (same definition as matrix)"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0),
      axis.text.y = element_text(size = 8)
    )
} else {
  ggplot() + theme_void()
}

png_main <- file.path(OUT_FIG, "mapa_global_conexiones.png")
if (requireNamespace("patchwork", quietly = TRUE)) {
  suppressPackageStartupMessages(library(patchwork))
  combined <- p_mat / p_bar + plot_layout(heights = c(1.35, 1))
  ggsave(png_main, combined, width = 11, height = 14, dpi = 200)
} else {
  ggsave(png_main, p_mat, width = 11, height = 9, dpi = 200)
  ggsave(file.path(OUT_FIG, "mapa_global_top_pares_paises.png"), p_bar, width = 9, height = 10, dpi = 200)
}

# --- Red solo país–país, layout circular (pocos nodos; no es la red de personas) ---
png_circ <- file.path(OUT_FIG, "mapa_global_red_paises_circulo.png")
if (nrow(pares_paises) > 0L) {
  g_circ <- graph_from_data_frame(
    pares_paises %>% transmute(from = a, to = b, weight = n_aristas),
    directed = FALSE
  )
  lay_circ <- layout_in_circle(g_circ)
  p_circ <- ggraph(g_circ, layout = lay_circ) +
    geom_edge_link(
      aes(width = weight),
      alpha = 0.5,
      color = "#34495e",
      lineend = "round"
    ) +
    scale_edge_width(range = c(0.35, 5.5), guide = "none") +
    geom_node_point(size = 15, fill = "white", color = "#1a252f", shape = 21, stroke = 1) +
    geom_node_text(
      aes(label = str_to_title(str_replace_all(name, "_", " "))),
      size = 3.4,
      fontface = "bold",
      color = "gray15"
    ) +
    labs(
      title = "Country network (circular layout)",
      subtitle = "One node = country. Edge width = aggregated connections (same logic as matrix)."
    ) +
    theme_void() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
      plot.subtitle = element_text(color = "gray40", hjust = 0.5, size = 9.5),
      plot.margin = margin(16, 16, 16, 16)
    )
  ggsave(png_circ, p_circ, width = 10, height = 10, dpi = 200)
}

# --- HTML: heatmap interactivo (sin red) o página simple ---
html_out <- file.path(OUT_FIG, "mapa_global_conexiones.html")
if (requireNamespace("plotly", quietly = TRUE) && nrow(mat_tiles) > 0L) {
  p_inter <- plotly::ggplotly(
    ggplot(mat_tiles, aes(x = col, y = fila, fill = n, text = paste0("n = ", n))) +
      geom_tile(color = "white") +
      scale_fill_viridis_c(na.value = "grey92", name = "N connections") +
      theme_minimal() +
      labs(title = "Country × country matrix (zoom / pan)", x = NULL, y = NULL),
    tooltip = "text"
  ) %>%
    plotly::layout(margin = list(l = 120, b = 120))
  htmlwidgets::saveWidget(
    htmlwidgets::prependContent(
      p_inter,
      htmltools::tags$p(
        style = "font-family:system-ui;padding:12px;background:#f8f9fa;",
        "Vista tipo tabla/mapa de calor (no diagrama de red). Misma lógica que el PNG."
      )
    ),
    html_out,
    selfcontained = FALSE,
    libdir = file.path(OUT_FIG, "mapa_global_conexiones_files")
  )
} else {
  htmltools::save_html(
    htmltools::tagList(
      htmltools::tags$h2("Conexión entre países (sin red)"),
      htmltools::tags$p(
        "Figura principal: ", htmltools::tags$code("mapa_global_conexiones.png"),
        " (matriz + barras). Instala ",
        htmltools::tags$code("plotly"),
        " para un HTML interactivo."
      ),
      htmltools::tags$p(
        "CSV: ",
        htmltools::tags$code("mapa_global_pares_paises.csv")
      )
    ),
    html_out
  )
}

cat("\nMapa global generado:\n")
cat("  PNG (matriz+barras): ", png_main, "\n")
if (file.exists(png_circ)) {
  cat("  PNG (red circular países): ", png_circ, "\n")
}
if (!requireNamespace("patchwork", quietly = TRUE)) {
  cat("  PNG (solo barras): ", file.path(OUT_FIG, "mapa_global_top_pares_paises.png"), " (instala patchwork para un solo PNG combinado)\n")
}
cat("  HTML: ", html_out, "\n")
cat("  CSV:  mapa_global_nodos.csv, mapa_global_aristas.csv, mapa_global_pares_paises.csv\n")
