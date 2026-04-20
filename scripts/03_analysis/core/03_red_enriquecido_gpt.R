# =============================================================================
# 03_red_enriquecido_gpt.R — Red de familias tipo GLOBOS (como las iniciales)
# =============================================================================
# Sigue la misma lógica que red_familias_clusters / redes de familias generales:
# - Un globo por familia, colocados por país; nodos dentro de cada globo (FR).
# - Aristas reales (padres, conyuge, hijos...) e interconexiones entre familias
#   (en rojo) para que se vean claramente las conexiones entre globos.
#
# FUENTES (en orden de preferencia):
# 1) Consolidado LATAM (data/processed/familias/_CONSOLIDADO_familias_latam.csv)
#    + join con enriquecido por url para cargo_1, partido_limpio, etc.
# 2) Solo output enriquecido (outputs/df_consolidado_enriquecido_gpt.csv)
#
# SALIDA: outputs/figures/red_enriquecido_gpt.png
# =============================================================================

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(igraph)
library(scales)

cat("╔══════════════════════════════════════════════════════════════════════╗\n")
cat("║     RED FAMILIAS (GLOBOS) — Misma lógica que redes iniciales       ║\n")
cat("╚══════════════════════════════════════════════════════════════════════╝\n\n")

# =============================================================================
# CARGA: consolidado LATAM + enriquecido (si existen)
# =============================================================================

path_latam <- "data/processed/familias/_CONSOLIDADO_familias_latam.csv"
path_enriquecido <- "outputs/df_consolidado_enriquecido_gpt.csv"

if (!file.exists(path_latam) && !file.exists(path_enriquecido)) {
  stop("No se encontró ni consolidado LATAM ni CSV enriquecido.")
}

# Preferir consolidado LATAM como base (mismas familias/países que las redes iniciales)
if (file.exists(path_latam)) {
  cat("📖 Cargando consolidado LATAM (base como redes de familias generales)...\n")
  all_data <- read_delim(path_latam, delim = ";", show_col_types = FALSE, locale = locale(encoding = "UTF-8"))
  all_data <- all_data %>% filter(!is.na(url), trimws(url) != "")
  cat("   Filas:", nrow(all_data), "\n")
  # Merge enriquecido si existe (cargo_1, partido_limpio, etc.)
  if (file.exists(path_enriquecido)) {
    enq <- read_csv(path_enriquecido, show_col_types = FALSE)
    cols_extra <- intersect(c("cargo_1", "cargo_2", "partido_limpio", "nacimiento_ano", "fallecimiento_ano", "redes_dentro_pais", "redes_entre_paises"), names(enq))
    if (length(cols_extra) > 0) {
      enq <- enq %>% select(url, all_of(cols_extra)) %>% distinct(url, .keep_all = TRUE)
      all_data <- all_data %>% select(-any_of(cols_extra)) %>% left_join(enq, by = "url")
      cat("   Enriquecido unido por url:", length(cols_extra), "columnas extra.\n")
    }
  }
} else {
  cat("📖 Cargando solo CSV enriquecido...\n")
  all_data <- read_csv(path_enriquecido, show_col_types = FALSE)
  all_data <- all_data %>% filter(!is.na(url), trimws(url) != "")
}

# Asegurar y normalizar país (consolidado ya trae pais por fila; no reemplazar por "otro" si hay valor)
if (!"pais" %in% names(all_data)) all_data$pais <- all_data$pais_origen
all_data <- all_data %>%
  mutate(
    pais = trimws(coalesce(as.character(pais), as.character(pais_origen), "")),
    pais = if_else(pais == "" | is.na(pais), "otro", tolower(pais))
  )

# =============================================================================
# CLASIFICAR FAMILIAS (misma lógica que red_familias_clusters + argentinas)
# =============================================================================

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
  # Argentina (incl. familias de la lista de URLs)
  if (str_detect(combined, "alpoim|alpoin")) return("Alpoim")
  if (str_detect(combined, "zuviría|zuviria|gorriti")) return("Zuviría/Gorriti")
  if (str_detect(combined, "belzu|belzú")) return("Belzu")
  if (str_detect(combined, "fortabat|lacroze")) return("Fortabat")
  if (str_detect(combined, "amoedo")) return("Amoedo")
  if (str_detect(combined, "lafuente")) return("Lafuente")
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

# Asegurar columna para clasificación
if (!"categoria_origen" %in% names(all_data)) all_data$categoria_origen <- NA_character_
if (!"familia" %in% names(all_data)) all_data$familia <- NA_character_

# Usar TODA la data: no filtrar por familias de interés; asignar familia_grupo a todos
filtered_data <- all_data %>%
  rowwise() %>%
  mutate(
    familia_grupo = extract_familia_grupo(nombre, familia, categoria_origen)
  ) %>%
  ungroup() %>%
  distinct(url, .keep_all = TRUE)

cat("   Nodos (toda la data):", nrow(filtered_data), "\n")
print(filtered_data %>% count(pais) %>% arrange(desc(n)))

# =============================================================================
# EXTRAER ARISTAS (mismo patrón que red_familias_clusters; normalizar URL)
# =============================================================================

extract_relation_urls <- function(relation_field) {
  if (is.na(relation_field) || as.character(relation_field) == "") return(character(0))
  x <- as.character(relation_field)
  # Patrón: (https://es.wikipedia.org/wiki/SLUG)
  pattern <- "\\(https://es\\.wikipedia\\.org/wiki/([^)]+)\\)"
  matches <- str_match_all(x, pattern)[[1]]
  if (nrow(matches) > 0) {
    slugs <- matches[, 2]
    return(paste0("https://es.wikipedia.org/wiki/", slugs))
  }
  # Fallback: cualquier URL de wiki en el texto
  pattern2 <- "https://es\\.wikipedia\\.org/wiki/[^\\s)]+"
  urls <- str_extract_all(x, pattern2)[[1]]
  unique(trimws(urls))
}

rel_fields <- c("padres", "conyuge", "pareja", "hijos", "hermanos",
               "perfiles_relacionados_padres", "perfiles_relacionados_conyuge",
               "perfiles_relacionados_hijos", "perfiles_relacionados_familia")

edges_real <- list()
for (i in seq_len(nrow(filtered_data))) {
  source_url <- filtered_data$url[i]
  for (field in rel_fields) {
    if (!field %in% names(filtered_data)) next
    target_urls <- extract_relation_urls(filtered_data[[field]][i])
    for (target_url in target_urls) {
      if (target_url != source_url && nchar(trimws(target_url)) > 0) {
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

edges_df <- bind_rows(edges_real) %>% distinct(from, to, .keep_all = TRUE)
cat("   Aristas reales (relaciones documentadas):", nrow(edges_df), "\n")

# =============================================================================
# ARISTAS DE COHESIÓN (globos: solo familias nombradas, máximo por grupo para no saturar)
# =============================================================================

edges_cohesion <- list()
max_cohesion_per_family <- 80L  # anillo máximo por familia para que el layout no explote
for (fam in unique(filtered_data$familia_grupo)) {
  if (fam == "Otra") next  # no cohesion para "Otra" (miles de nodos)
  miembros <- filtered_data %>% filter(familia_grupo == fam) %>% pull(url)
  if (length(miembros) < 2) next
  if (length(miembros) > max_cohesion_per_family) miembros <- miembros[1:max_cohesion_per_family]
  for (i in seq_len(length(miembros) - 1)) {
    edges_cohesion[[length(edges_cohesion) + 1]] <- tibble(
      from = miembros[i], to = miembros[i + 1],
      relation_type = "cohesion_familiar", edge_type = "cohesion"
    )
  }
  edges_cohesion[[length(edges_cohesion) + 1]] <- tibble(
    from = miembros[length(miembros)], to = miembros[1],
    relation_type = "cohesion_familiar", edge_type = "cohesion"
  )
}
edges_cohesion_df <- bind_rows(edges_cohesion)
all_edges <- bind_rows(edges_df, edges_cohesion_df)

# =============================================================================
# NODOS: toda la data (todos los URLs del consolidado + los que solo aparecen en aristas)
# =============================================================================

all_urls <- unique(c(all_edges$from, all_edges$to, filtered_data$url))
nodes <- filtered_data %>%
  filter(url %in% all_urls) %>%
  select(url, nombre, pais, familia_grupo, cargo_1, cargo_2, partido_limpio, ocupacion) %>%
  distinct(url, .keep_all = TRUE)

# Quienes solo aparecen como "to" (enlazados desde otro): añadirlos; asignar país desde all_data si existe
missing_urls <- setdiff(all_urls, nodes$url)
if (length(missing_urls) > 0) {
  lookup_pais <- all_data %>% filter(url %in% missing_urls) %>% select(url, pais) %>% distinct(url, .keep_all = TRUE)
  missing_tbl <- tibble(
    url = missing_urls,
    nombre = str_replace_all(URLdecode(str_replace(str_replace(missing_urls, ".*wiki/", ""), "_", " ")), "%", " "),
    familia_grupo = "Otra",
    cargo_1 = NA_character_, cargo_2 = NA_character_, partido_limpio = NA_character_, ocupacion = NA_character_
  ) %>%
    left_join(lookup_pais, by = "url") %>%
    mutate(pais = if_else(is.na(pais) | trimws(pais) == "", "otro", tolower(trimws(pais))))
  nodes <- bind_rows(nodes, missing_tbl)
  cat("   Nodos añadidos (solo enlazados):", length(missing_urls), "→ interconexiones visibles.\n")
}

nodes <- nodes %>%
  filter(!is.na(url), trimws(url) != "", url != "NA") %>%
  mutate(
    pais = tolower(trimws(if_else(is.na(pais) | pais == "", "otro", pais))),
    familia_para_layout = if_else(familia_grupo == "Otra", paste0("Otra_", pais), familia_grupo)
  )
all_edges <- all_edges %>% filter(from %in% nodes$url, to %in% nodes$url)

edges_final <- all_edges %>%
  left_join(nodes %>% select(url, familia_grupo, pais), by = c("from" = "url")) %>%
  rename(from_familia = familia_grupo, from_pais = pais) %>%
  left_join(nodes %>% select(url, familia_grupo, pais), by = c("to" = "url")) %>%
  rename(to_familia = familia_grupo, to_pais = pais) %>%
  mutate(
    es_interfamiliar = (from_familia != to_familia) & (edge_type == "real"),
    es_entre_paises  = (from_pais != to_pais) & (edge_type == "real")
  )

cat("   Nodos totales:", nrow(nodes), "| Aristas:", nrow(edges_final),
    "| Reales:", sum(edges_final$edge_type == "real"),
    "| Entre países:", sum(edges_final$es_entre_paises, na.rm = TRUE), "\n")

# =============================================================================
# LAYOUT GLOBOS (igual que red_familias_clusters)
# =============================================================================

g <- graph_from_data_frame(edges_final, directed = FALSE, vertices = nodes)
set.seed(42)

# Todos los países del consolidado (y "otro" solo si aparece): posiciones y colores
paises_presentes <- unique(nodes$pais)
pais_centers <- list(
  argentina = c(x = 55, y = -5),
  bolivia   = c(x = 35, y = 25),
  chile     = c(x = 0, y = 0),
  colombia  = c(x = 45, y = 48),
  ecuador   = c(x = 28, y = 42),
  mexico    = c(x = 90, y = 22),
  paraguay  = c(x = 48, y = 18),
  peru      = c(x = 22, y = 35),
  uruguay   = c(x = 52, y = -18),
  venezuela = c(x = 72, y = 48),
  otro      = c(x = 45, y = 25)
)
# Solo mantener países que existen en los nodos
pais_centers <- pais_centers[names(pais_centers) %in% paises_presentes]
if (length(pais_centers) == 0) pais_centers <- list(otro = c(x = 45, y = 25))

pais_radius <- 22
familia_counts <- nodes %>% count(familia_para_layout, pais) %>% arrange(pais, desc(n))
familia_positions <- list()

for (p in names(pais_centers)) {
  fams <- familia_counts %>% filter(pais == p) %>% pull(familia_para_layout)
  nf <- length(unique(fams))
  fams <- unique(fams)
  if (nf > 0) {
    angles <- seq(0, 2 * pi, length.out = nf + 1)[1:nf]
    for (i in seq_along(fams)) {
      familia_positions[[fams[i]]] <- pais_centers[[p]] + c(pais_radius * cos(angles[i]), pais_radius * sin(angles[i]))
    }
  }
}

layout_final <- matrix(0, nrow = vcount(g), ncol = 2)
for (fam in unique(nodes$familia_para_layout)) {
  idx <- which(nodes$familia_para_layout == fam)
  cen <- familia_positions[[fam]]
  if (is.null(cen)) cen <- c(x = 45, y = 25)
  if (length(idx) > 1) {
    subg <- induced_subgraph(g, idx)
    nv <- vcount(subg)
    if (nv > 400) {
      # Grupos muy grandes: layout en círculo para no saturar FR
      sub_layout <- layout_in_circle(subg)
      if (nv > 1) sub_layout <- scale(sub_layout) * min(12, 3 + nv / 80)
    } else {
      sub_layout <- layout_with_fr(subg, niter = if (nv > 150) 200 else 500)
      sub_layout[is.na(sub_layout)] <- 0
      if (nrow(sub_layout) > 1 && sd(sub_layout[, 1]) > 0) sub_layout <- scale(sub_layout) * 6
    }
    layout_final[idx, 1] <- sub_layout[, 1] + cen["x"]
    layout_final[idx, 2] <- sub_layout[, 2] + cen["y"]
  } else {
    layout_final[idx, 1] <- cen["x"]
    layout_final[idx, 2] <- cen["y"]
  }
}
layout_final[is.na(layout_final)] <- 0

nodes$x <- layout_final[, 1]
nodes$y <- layout_final[, 2]

# Grado
edges_plot <- edges_final %>% filter(edge_type == "real")
node_degree <- edges_plot %>%
  pivot_longer(cols = c(from, to), values_to = "url") %>%
  count(url, name = "degree")
nodes <- nodes %>% left_join(node_degree, by = "url") %>% mutate(degree = coalesce(degree, 0))

# =============================================================================
# VISUALIZACIÓN (globos + interconexiones en rojo)
# =============================================================================

dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE)
out_plot <- "outputs/figures/red_enriquecido_gpt.png"

edges_plot_df <- edges_final %>%
  filter(edge_type == "real") %>%
  left_join(nodes %>% select(url, x, y), by = c("from" = "url")) %>%
  rename(x_from = x, y_from = y) %>%
  left_join(nodes %>% select(url, x, y), by = c("to" = "url")) %>%
  rename(x_to = x, y_to = y) %>%
  filter(!is.na(x_from), !is.na(x_to))

n_entre_paises <- sum(edges_plot_df$es_entre_paises, na.rm = TRUE)

# Recalcular es_entre_paises con país normalizado
edges_plot_df <- edges_plot_df %>%
  mutate(
    from_pais = tolower(trimws(coalesce(from_pais, "otro"))),
    to_pais   = tolower(trimws(coalesce(to_pais, "otro"))),
    es_entre_paises = from_pais != to_pais
  )

# Colores por país (todos los que pueden aparecer en el consolidado)
pais_colors <- c(
  argentina = "#5DADE2",
  bolivia   = "#16A085",
  chile     = "#1E3A5F",
  colombia  = "#F4D03F",
  ecuador   = "#D35400",
  mexico    = "#8E44AD",
  paraguay  = "#C0392B",
  peru      = "#27AE60",
  uruguay   = "#2980B9",
  venezuela = "#E74C3C",
  otro      = "gray65"
)
# Solo incluir países que existen en los nodos (evitar leyenda vacía)
paises_plot <- unique(nodes$pais)
pais_colors <- pais_colors[names(pais_colors) %in% paises_plot]

# Capas de fondo: un globo por país
p <- ggplot()
for (pk in names(pais_centers)) {
  if (pk %in% names(pais_colors)) {
    cen <- pais_centers[[pk]]
    p <- p + annotate("point", x = cen["x"], y = cen["y"], size = 70, color = pais_colors[pk], alpha = 0.08)
  }
}
p <- p +
  # Aristas dentro del mismo país (tenues)
  geom_segment(
    data = edges_plot_df %>% filter(!es_entre_paises),
    aes(x = x_from, y = y_from, xend = x_to, yend = y_to),
    color = "gray70", alpha = 0.4, linewidth = 0.4
  ) +
  # Aristas entre países (conexiones entre países, visibles)
  geom_segment(
    data = edges_plot_df %>% filter(es_entre_paises),
    aes(x = x_from, y = y_from, xend = x_to, yend = y_to),
    color = "#C0392B", alpha = 0.85, linewidth = 0.9
  ) +
  # Nodos: color por país, sin etiquetas de nombre
  geom_point(
    data = nodes,
    aes(x = x, y = y, fill = pais, size = degree + 1),
    shape = 21, colour = "gray25", stroke = 0.35
  ) +
  scale_fill_manual(values = pais_colors, na.value = "gray75") +
  theme_void() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 9)
  ) +
  labs(
    title = "Red por país — Conexiones entre países",
    subtitle = sprintf("Nodos: %d | Conexiones entre países: %d",
      nrow(nodes), n_entre_paises),
    fill = "País", size = "Grado"
  )

p

ggsave(out_plot, p, width = 12, height = 8, dpi = 150, bg = "white")
cat("   Guardado:", out_plot, "\n")
cat("\n✅ Red tipo globos con interconexiones listada.\n")
