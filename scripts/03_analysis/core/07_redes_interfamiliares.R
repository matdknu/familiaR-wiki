#!/usr/bin/env Rscript
# ============================================================================
# 07_redes_interfamiliares.R
# Redes entre familias dentro de cada país y transnacionales:
#  1. Grafo inter-familiar por país (qué familias se entrecruzan)
#  2. Ocupaciones de las familias más conectadas
#  3. Cadenas de conexión a nivel individual (ej: Menem-Bolocco-Vicuña-...)
# ============================================================================

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(forcats)
library(igraph)
library(ggraph)
library(tidygraph)
library(purrr)
library(scales)

DATA_DIR  <- "data/processed/02_leer_data"
OUT_FIG   <- "outputs/figures"
OUT_TABLE <- "outputs/tables"
dir.create(OUT_FIG, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_TABLE, recursive = TRUE, showWarnings = FALSE)

personas   <- read_rds(file.path(DATA_DIR, "personas.rds"))
relaciones <- read_rds(file.path(DATA_DIR, "relaciones.rds"))

url_extra_path <- "data/manual/url_pais_extra.csv"
if (file.exists(url_extra_path)) {
  overrides <- read_csv(url_extra_path, show_col_types = FALSE) %>%
    transmute(url = as.character(url), pais_override = tolower(trimws(pais))) %>%
    filter(!is.na(url), !is.na(pais_override))
  personas <- personas %>%
    left_join(overrides, by = "url") %>%
    mutate(pais_efectivo = coalesce(pais_override, pais_base)) %>%
    select(-pais_override)
} else {
  personas <- personas %>% mutate(pais_efectivo = pais_base)
}

cap <- function(x) str_to_title(x)

# ═══════════════════════════════════════════════════════════════════════════════
# BUILD CROSS-FAMILY EDGES
# ═══════════════════════════════════════════════════════════════════════════════

cross_fam <- relaciones %>%
  filter(!is.na(persona_relacionada_id)) %>%
  left_join(
    personas %>% select(persona_id, nombre_p = nombre, fam_p = familia_norm,
                        pais_p = pais_efectivo, ocupacion_p = ocupacion),
    by = "persona_id"
  ) %>%
  left_join(
    personas %>% select(persona_id, nombre_r = nombre, fam_r = familia_norm,
                        pais_r = pais_efectivo, ocupacion_r = ocupacion),
    by = c("persona_relacionada_id" = "persona_id")
  ) %>%
  filter(!is.na(fam_p), !is.na(fam_r), fam_p != fam_r)

cross_fam <- cross_fam %>%
  mutate(
    fam_min = pmin(fam_p, fam_r),
    fam_max = pmax(fam_p, fam_r),
    id_min  = pmin(persona_id, persona_relacionada_id),
    id_max  = pmax(persona_id, persona_relacionada_id)
  ) %>%
  distinct(id_min, id_max, tipo_relacion, .keep_all = TRUE) %>%
  select(-id_min, -id_max)

cat("Cross-family relations (deduplicated):", nrow(cross_fam), "\n")

write_csv(
  cross_fam %>% select(nombre_p, fam_p, pais_p, nombre_r, fam_r, pais_r,
                        tipo_relacion, ocupacion_p, ocupacion_r),
  file.path(OUT_TABLE, "interfamiliar_conexiones_detalle.csv")
)

# ═══════════════════════════════════════════════════════════════════════════════
# 1. INTER-FAMILY NETWORK BY COUNTRY
# ═══════════════════════════════════════════════════════════════════════════════

tipo_label <- function(t) {
  case_when(
    t %in% c("conyuge", "pareja") ~ "Matrimonio/Pareja",
    t == "padre/madre"             ~ "Filiación",
    t == "hijo/a"                  ~ "Filiación",
    TRUE                           ~ "Otro"
  )
}

fam_edges <- cross_fam %>%
  mutate(tipo = tipo_label(tipo_relacion)) %>%
  count(fam_min, fam_max, tipo, name = "peso")

fam_edges_total <- fam_edges %>%
  group_by(fam_min, fam_max) %>%
  summarise(peso = sum(peso), .groups = "drop")

fam_pais <- personas %>%
  filter(!is.na(familia_norm), !is.na(pais_efectivo)) %>%
  count(familia_norm, pais_efectivo, sort = TRUE) %>%
  group_by(familia_norm) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(familia_norm, pais_principal = pais_efectivo)

fam_sizes <- personas %>%
  filter(!is.na(familia_norm)) %>%
  count(familia_norm, name = "n_miembros")

# Global inter-family network (top connections)
g_fam <- graph_from_data_frame(
  fam_edges_total %>% filter(peso >= 2),
  directed = FALSE
)
g_fam <- igraph::simplify(g_fam, edge.attr.comb = "sum")

V(g_fam)$pais <- fam_pais$pais_principal[match(V(g_fam)$name, fam_pais$familia_norm)]
V(g_fam)$size <- fam_sizes$n_miembros[match(V(g_fam)$name, fam_sizes$familia_norm)]
V(g_fam)$size[is.na(V(g_fam)$size)] <- 1

country_pal <- c(
  "argentina" = "#74b9ff", "bolivia" = "#00b894", "chile" = "#0984e3",
  "colombia" = "#fdcb6e", "ecuador" = "#e17055", "mexico" = "#00cec9",
  "paraguay" = "#d63031", "peru" = "#e84393", "uruguay" = "#6c5ce7",
  "venezuela" = "#ff7675"
)

if (vcount(g_fam) > 0) {
  tg_fam <- as_tbl_graph(g_fam)

  set.seed(42)
  p_fam_global <- ggraph(tg_fam, layout = "fr") +
    geom_edge_link(aes(width = peso), alpha = 0.35, color = "gray50") +
    geom_node_point(aes(color = pais, size = size), alpha = 0.8) +
    geom_node_text(aes(label = cap(name)), size = 2.8, repel = TRUE,
                   max.overlaps = 20, fontface = "bold") +
    scale_edge_width_continuous(range = c(0.3, 3), name = "Conexiones") +
    scale_color_manual(values = country_pal, na.value = "gray60",
                       name = "País principal", labels = cap) +
    scale_size_continuous(range = c(2, 10), name = "Miembros") +
    labs(title = "Red inter-familiar: familias conectadas por matrimonio y parentesco",
         subtitle = "Aristas con ≥ 2 vínculos entre familias distintas. Tamaño = miembros.") +
    theme_void(base_size = 11) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
          legend.position = "right")

  ggsave(file.path(OUT_FIG, "interfamiliar_red_global.png"), p_fam_global,
         width = 16, height = 12, dpi = 200)
  cat("Guardado: interfamiliar_red_global.png\n")
}

# Per-country faceted network
paises_top <- personas %>%
  filter(!is.na(pais_efectivo)) %>%
  count(pais_efectivo, sort = TRUE) %>%
  filter(n >= 50) %>%
  pull(pais_efectivo)

plots_pais <- list()
for (pais in paises_top) {
  fams_pais <- personas %>%
    filter(pais_efectivo == pais, !is.na(familia_norm)) %>%
    distinct(familia_norm) %>%
    pull(familia_norm)

  edges_pais <- fam_edges_total %>%
    filter(fam_min %in% fams_pais | fam_max %in% fams_pais) %>%
    filter(peso >= 2)

  if (nrow(edges_pais) < 3) next

  g_p <- graph_from_data_frame(edges_pais, directed = FALSE) %>%
    igraph::simplify(., edge.attr.comb = "sum")
  V(g_p)$size <- fam_sizes$n_miembros[match(V(g_p)$name, fam_sizes$familia_norm)]
  V(g_p)$size[is.na(V(g_p)$size)] <- 1

  set.seed(123)
  tg_p <- as_tbl_graph(g_p)
  p_p <- ggraph(tg_p, layout = "fr") +
    geom_edge_link(aes(width = peso), alpha = 0.4, color = "gray50") +
    geom_node_point(aes(size = size), color = country_pal[pais], alpha = 0.8) +
    geom_node_text(aes(label = cap(name)), size = 3, repel = TRUE,
                   max.overlaps = 25, fontface = "bold") +
    scale_edge_width_continuous(range = c(0.3, 3), guide = "none") +
    scale_size_continuous(range = c(3, 12), guide = "none") +
    labs(title = cap(pais)) +
    theme_void(base_size = 10) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 14))

  plots_pais[[pais]] <- p_p
}

if (length(plots_pais) >= 1) {
  library(patchwork)
  p_facet <- wrap_plots(plots_pais, ncol = 2) +
    plot_annotation(
      title = "Redes inter-familiares por país",
      subtitle = "Familias conectadas por matrimonio/parentesco (≥ 2 vínculos). Tamaño = miembros.",
      theme = theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
                    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"))
    )
  ggsave(file.path(OUT_FIG, "interfamiliar_red_por_pais.png"), p_facet,
         width = 18, height = 5 * ceiling(length(plots_pais) / 2), dpi = 200)
  cat("Guardado: interfamiliar_red_por_pais.png\n")
}

# Top inter-family pairs table
top_pairs <- fam_edges %>%
  group_by(fam_min, fam_max) %>%
  summarise(
    total = sum(peso),
    matrimonios = sum(peso[tipo == "Matrimonio/Pareja"]),
    filiacion = sum(peso[tipo == "Filiación"]),
    .groups = "drop"
  ) %>%
  left_join(fam_pais, by = c("fam_min" = "familia_norm")) %>%
  rename(pais_fam1 = pais_principal) %>%
  left_join(fam_pais, by = c("fam_max" = "familia_norm")) %>%
  rename(pais_fam2 = pais_principal) %>%
  arrange(desc(total))

write_csv(top_pairs, file.path(OUT_TABLE, "interfamiliar_top_pares.csv"))
cat("Top 20 pares inter-familiares:\n")
print(head(top_pairs, 20))

# Heatmap top families
top_fams_set <- unique(c(
  head(top_pairs$fam_min, 15),
  head(top_pairs$fam_max, 15)
))

heat_mat <- fam_edges_total %>%
  filter(fam_min %in% top_fams_set & fam_max %in% top_fams_set) %>%
  bind_rows(
    fam_edges_total %>%
      filter(fam_min %in% top_fams_set & fam_max %in% top_fams_set) %>%
      rename(fam_min = fam_max, fam_max = fam_min)
  ) %>%
  group_by(fam_min, fam_max) %>%
  summarise(peso = sum(peso), .groups = "drop") %>%
  mutate(fam_min = cap(fam_min), fam_max = cap(fam_max))

if (nrow(heat_mat) > 0) {
  p_heat_fam <- ggplot(heat_mat, aes(fam_min, fam_max, fill = peso)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = ifelse(peso > 0, peso, "")), size = 3, fontface = "bold") +
    scale_fill_gradient(low = "#f8f9fa", high = "#d63031", name = "Conexiones") +
    labs(title = "Conexiones inter-familiares (top familias)",
         subtitle = "Vínculos por parentesco/matrimonio entre familias",
         x = NULL, y = NULL) +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
          axis.text.y = element_text(size = 9),
          panel.grid = element_blank())

  ggsave(file.path(OUT_FIG, "interfamiliar_heatmap_top.png"), p_heat_fam,
         width = 14, height = 11, dpi = 200)
  cat("Guardado: interfamiliar_heatmap_top.png\n")
}


# ═══════════════════════════════════════════════════════════════════════════════
# 2. OCUPACIONES DE LAS FAMILIAS CONECTADAS
# ═══════════════════════════════════════════════════════════════════════════════

normalizar_ocupacion <- function(ocu) {
  if (is.na(ocu)) return(NA_character_)
  o <- tolower(trimws(ocu))
  case_when(
    str_detect(o, "politic|president|senator|diputad|ministr|gobernad|congres|legislad") ~ "Político/a",
    str_detect(o, "militar|general|coronel|almirante|soldado|brigadier|rango") ~ "Militar",
    str_detect(o, "abogad|jurist|magistrad|juez") ~ "Abogado/a",
    str_detect(o, "diplom|embajad|cónsul") ~ "Diplomático/a",
    str_detect(o, "escrit|poet|nolist|literat|ensayist") ~ "Escritor/a",
    str_detect(o, "periodist|locuto") ~ "Periodista",
    str_detect(o, "empresar|industrial|comerciant|ejecutiv|negoci") ~ "Empresario/a",
    str_detect(o, "ingenier") ~ "Ingeniero/a",
    str_detect(o, "medic|cirujan|doctor") ~ "Médico/a",
    str_detect(o, "actor|actriz|model|presentad|televisión|cantante|artis|músic") ~ "Espectáculo/Media",
    str_detect(o, "profesor|académ|educad|rector|docent|investigad|sociólog|histori|cientif|filósof|economis") ~ "Académico/a",
    str_detect(o, "sacerdot|obispo|arzobispo|cardenal|papa|religios|cleri|presbi") ~ "Religioso/a",
    str_detect(o, "agricult|hacendad|terratenient|ganadero") ~ "Hacendado/a",
    str_detect(o, "pint|escult|fotógraf|diseñ") ~ "Artista visual",
    TRUE ~ "Otro"
  )
}

personas <- personas %>%
  mutate(ocupacion_norm = map_chr(ocupacion, normalizar_ocupacion))

connected_fams <- unique(c(fam_edges_total$fam_min, fam_edges_total$fam_max))

ocu_fam <- personas %>%
  filter(familia_norm %in% connected_fams, !is.na(ocupacion_norm), ocupacion_norm != "Otro") %>%
  count(familia_norm, ocupacion_norm, sort = TRUE) %>%
  left_join(fam_pais, by = "familia_norm") %>%
  left_join(fam_sizes, by = "familia_norm")

write_csv(ocu_fam, file.path(OUT_TABLE, "interfamiliar_ocupaciones_familias.csv"))

top_connected <- fam_edges_total %>%
  pivot_longer(c(fam_min, fam_max), values_to = "familia") %>%
  count(familia, wt = peso, name = "conexiones_ext", sort = TRUE) %>%
  head(20) %>%
  pull(familia)

ocu_top <- ocu_fam %>%
  filter(familia_norm %in% top_connected) %>%
  mutate(
    familia_label = paste0(cap(familia_norm), " (", cap(pais_principal), ")"),
    familia_label = fct_reorder(familia_label, n_miembros)
  )

ocu_pal <- c(
  "Político/a" = "#d63031", "Militar" = "#2d3436", "Abogado/a" = "#0984e3",
  "Diplomático/a" = "#00b894", "Escritor/a" = "#6c5ce7", "Periodista" = "#fdcb6e",
  "Empresario/a" = "#e17055", "Ingeniero/a" = "#00cec9", "Médico/a" = "#74b9ff",
  "Espectáculo/Media" = "#e84393", "Académico/a" = "#fab1a0",
  "Religioso/a" = "#636e72", "Hacendado/a" = "#b2bec3", "Artista visual" = "#a29bfe"
)

if (nrow(ocu_top) > 0) {
  p_ocu <- ggplot(ocu_top, aes(n, familia_label, fill = ocupacion_norm)) +
    geom_col(alpha = 0.85) +
    scale_fill_manual(values = ocu_pal, name = "Ocupación") +
    labs(title = "Ocupaciones de las familias más interconectadas",
         subtitle = "Top 20 familias con más vínculos inter-familiares",
         x = "Personas", y = NULL) +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold"),
          legend.position = "right",
          legend.text = element_text(size = 9))

  ggsave(file.path(OUT_FIG, "interfamiliar_ocupaciones_top.png"), p_ocu,
         width = 14, height = 10, dpi = 200)
  cat("Guardado: interfamiliar_ocupaciones_top.png\n")
}

# Aggregate: occupation composition of connected vs isolated families
personas_comp <- personas %>%
  filter(!is.na(familia_norm), !is.na(ocupacion_norm), ocupacion_norm != "Otro") %>%
  mutate(conectada = familia_norm %in% connected_fams)

comp_data <- personas_comp %>%
  count(conectada, ocupacion_norm) %>%
  group_by(conectada) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  mutate(grupo = ifelse(conectada, "Familias conectadas", "Familias aisladas"))

p_comp <- ggplot(comp_data, aes(prop, fct_reorder(ocupacion_norm, prop), fill = grupo)) +
  geom_col(position = "dodge", alpha = 0.8) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("Familias conectadas" = "#0984e3",
                                "Familias aisladas" = "#dfe6e9"), name = NULL) +
  labs(title = "Perfil ocupacional: familias inter-conectadas vs. aisladas",
       x = "Proporción", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "top")

ggsave(file.path(OUT_FIG, "interfamiliar_ocupaciones_comparacion.png"), p_comp,
       width = 11, height = 7, dpi = 200)
cat("Guardado: interfamiliar_ocupaciones_comparacion.png\n")


# ═══════════════════════════════════════════════════════════════════════════════
# 3. CADENAS DE CONEXIÓN A NIVEL INDIVIDUAL — EXPANSIÓN BFS COMPLETA
#    Incluye relaciones NO matcheadas como nodos virtuales (nombres sin ficha)
# ═══════════════════════════════════════════════════════════════════════════════

# Clean unmatched names
clean_rel_name <- function(x) {
  x <- str_remove_all(x, "\\s*\\(.*?\\)")
  x <- str_remove_all(x, "\\s*Ver y modificar.*$")
  x <- URLdecode(x)
  trimws(x)
}

# Separate matched vs unmatched
rels_matched <- relaciones %>%
  filter(!is.na(persona_relacionada_id)) %>%
  transmute(from = persona_id,
            to   = persona_relacionada_id,
            tipo_relacion,
            from_is_real = TRUE, to_is_real = TRUE)

rels_unmatched <- relaciones %>%
  filter(is.na(persona_relacionada_id),
         !is.na(nombre_relacionado),
         nchar(trimws(nombre_relacionado)) > 2,
         !grepl("^\\d+$", nombre_relacionado)) %>%
  mutate(nombre_limpio = clean_rel_name(nombre_relacionado)) %>%
  filter(nchar(nombre_limpio) > 2)

# Create virtual IDs for unmatched people (negative IDs to avoid collisions)
virtual_people <- rels_unmatched %>%
  distinct(nombre_limpio) %>%
  mutate(virtual_id = -row_number())

rels_virtual <- rels_unmatched %>%
  left_join(virtual_people, by = "nombre_limpio") %>%
  transmute(from = persona_id, to = virtual_id, tipo_relacion,
            from_is_real = TRUE, to_is_real = FALSE)

all_edges <- bind_rows(rels_matched, rels_virtual)

# Deduplicate + assign edge type label
edge_dedup <- all_edges %>%
  mutate(e_min = pmin(from, to), e_max = pmax(from, to)) %>%
  group_by(e_min, e_max) %>%
  summarise(
    tipo_label = case_when(
      any(tipo_relacion == "conyuge")      ~ "cónyuge",
      any(tipo_relacion == "pareja")       ~ "pareja",
      any(tipo_relacion == "padre/madre")  ~ "padre/madre",
      any(tipo_relacion == "hijo/a")       ~ "hijo/a",
      TRUE ~ first(tipo_relacion)
    ),
    .groups = "drop"
  ) %>%
  rename(from = e_min, to = e_max)

# Build vertex table: real personas + virtual people
real_vertices <- personas %>%
  transmute(id = persona_id, label = nombre, familia = familia_norm,
            pais = pais_efectivo, ocu = ocupacion_norm, is_virtual = FALSE)

virtual_vertices <- virtual_people %>%
  transmute(id = virtual_id, label = nombre_limpio, familia = NA_character_,
            pais = NA_character_, ocu = NA_character_, is_virtual = TRUE)

all_vertices <- bind_rows(real_vertices, virtual_vertices)

g_ind <- graph_from_data_frame(edge_dedup, directed = FALSE,
                               vertices = all_vertices %>% select(name = id))

V(g_ind)$label      <- all_vertices$label[match(as.integer(V(g_ind)$name), all_vertices$id)]
V(g_ind)$familia    <- all_vertices$familia[match(as.integer(V(g_ind)$name), all_vertices$id)]
V(g_ind)$pais       <- all_vertices$pais[match(as.integer(V(g_ind)$name), all_vertices$id)]
V(g_ind)$ocu_norm   <- all_vertices$ocu[match(as.integer(V(g_ind)$name), all_vertices$id)]
V(g_ind)$is_virtual <- all_vertices$is_virtual[match(as.integer(V(g_ind)$name), all_vertices$id)]

cat("Grafo individual:", vcount(g_ind), "nodos (", sum(V(g_ind)$is_virtual, na.rm=TRUE),
    "virtuales),", ecount(g_ind), "aristas\n")

# ───────────────────────────────────────────────────────────────────────────────
# 3a. BFS expansion from seed celebrities — ALL relations including unmatched
# ───────────────────────────────────────────────────────────────────────────────

seed_names <- c("Carlos Menem", "Cecilia Bolocco", "Benjamín Vicuña",
                "China Suárez", "Paz Bascuñán", "Pampita",
                "Patricio Aylwin", "Nicolás Cabré")

seed_ids <- personas %>%
  filter(nombre %in% seed_names) %>%
  distinct(nombre, .keep_all = TRUE)

cat("\n=== Semillas para expansión BFS ===\n")
print(seed_ids %>% select(persona_id, nombre, familia_norm, pais_efectivo) %>% as.data.frame())

seed_vids <- match(as.character(seed_ids$persona_id), V(g_ind)$name)
seed_vids <- seed_vids[!is.na(seed_vids)]

# BFS: expand 2 hops from each seed
all_ego_nodes <- integer()
for (sv in seed_vids) {
  ego_nodes <- ego(g_ind, order = 2, nodes = sv)[[1]]
  all_ego_nodes <- unique(c(all_ego_nodes, as.integer(ego_nodes)))
}

sg_full <- induced_subgraph(g_ind, all_ego_nodes)
cat("Subgrafo BFS 2-hops desde semillas:", vcount(sg_full), "personas,",
    ecount(sg_full), "aristas\n")

# Cap at 200 nodes: keep seeds + neighbors + highest-degree
if (vcount(sg_full) > 200) {
  seed_in_sg <- which(V(sg_full)$label %in% seed_names)
  deg_sg <- degree(sg_full)
  non_seed <- setdiff(seq_len(vcount(sg_full)), seed_in_sg)
  top_non_seed <- non_seed[order(deg_sg[non_seed], decreasing = TRUE)][1:(200 - length(seed_in_sg))]
  keep <- sort(unique(c(seed_in_sg, top_non_seed)))
  sg_full <- induced_subgraph(sg_full, keep)
  cat("  (podado a", vcount(sg_full), "nodos)\n")
}

# Node/edge data
node_full <- tibble(
  name       = V(sg_full)$name,
  label      = V(sg_full)$label,
  familia    = V(sg_full)$familia,
  pais       = V(sg_full)$pais,
  ocu        = V(sg_full)$ocu_norm,
  is_virtual = V(sg_full)$is_virtual,
  is_seed    = V(sg_full)$label %in% seed_names
)

edge_full <- igraph::as_data_frame(sg_full, "edges") %>%
  mutate(
    from_label = V(sg_full)$label[match(from, V(sg_full)$name)],
    to_label   = V(sg_full)$label[match(to, V(sg_full)$name)],
    from_fam   = V(sg_full)$familia[match(from, V(sg_full)$name)],
    to_fam     = V(sg_full)$familia[match(to, V(sg_full)$name)]
  )

write_csv(node_full, file.path(OUT_TABLE, "interfamiliar_cadena_celebs_nodos.csv"))
write_csv(edge_full %>% select(from_label, to_label, tipo_label, from_fam, to_fam),
          file.path(OUT_TABLE, "interfamiliar_cadena_celebs_aristas.csv"))

# --- PLOT: Full BFS network ---
tipo_edge_color <- c(
  "cónyuge"      = "#e84393",
  "pareja"       = "#fd79a8",
  "padre/madre"  = "#0984e3",
  "hijo/a"       = "#74b9ff"
)

pais_shape <- c("chile" = 16, "argentina" = 17, "colombia" = 15,
                "peru" = 18, "mexico" = 8, "venezuela" = 3,
                "uruguay" = 4, "bolivia" = 7, "ecuador" = 9)

tg_full <- as_tbl_graph(sg_full)
set.seed(42)

p_bfs <- ggraph(tg_full, layout = "fr") +
  geom_edge_link(aes(color = tipo_label), alpha = 0.6, width = 0.9) +
  geom_node_point(
    aes(color = familia,
        size  = case_when(label %in% seed_names ~ 12,
                          is_virtual == TRUE ~ 4,
                          TRUE ~ 6),
        shape = ifelse(is_virtual == TRUE, "virtual", "real")),
    alpha = 0.85
  ) +
  geom_node_text(aes(label = label), size = 2.6, repel = TRUE,
                 max.overlaps = 50, fontface = "bold", segment.color = "gray70") +
  scale_size_identity() +
  scale_shape_manual(values = c("real" = 16, "virtual" = 1),
                     name = "Tipo nodo",
                     labels = c("real" = "En dataset", "virtual" = "Solo mencionado")) +
  scale_color_viridis_d(name = "Familia", option = "turbo", na.value = "gray60") +
  scale_edge_color_manual(values = tipo_edge_color, name = "Tipo de relación",
                          na.value = "gray60") +
  labs(title = "Red completa: celebridades + padres, abuelos, parejas, hijos, cónyuges",
       subtitle = paste0("BFS 2 pasos. ", vcount(sg_full), " personas (",
                         sum(node_full$is_virtual, na.rm = TRUE), " solo mencionados), ",
                         ecount(sg_full), " vínculos. Círculos vacíos = personas fuera del dataset.")) +
  theme_void(base_size = 11) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
        plot.subtitle = element_text(hjust = 0.5, color = "gray40", size = 9),
        legend.position = "right",
        legend.text = element_text(size = 8))

ggsave(file.path(OUT_FIG, "interfamiliar_cadena_celebridades.png"), p_bfs,
       width = 20, height = 15, dpi = 200)
cat("Guardado: interfamiliar_cadena_celebridades.png\n")

# ───────────────────────────────────────────────────────────────────────────────
# 3b. ZOOMED CHAIN: seeds + 1-hop + shortest paths — ALL relations
# ───────────────────────────────────────────────────────────────────────────────

celeb_vid_full <- match(as.character(seed_ids$persona_id), V(g_ind)$name)
celeb_vid_full <- celeb_vid_full[!is.na(celeb_vid_full)]

all_on_paths <- integer()
for (i in seq_along(celeb_vid_full)) {
  for (j in seq_len(i - 1)) {
    sp <- tryCatch(
      shortest_paths(g_ind, from = celeb_vid_full[i], to = celeb_vid_full[j],
                     output = "vpath"),
      warning = function(w) list(vpath = list(integer(0)))
    )
    if (length(sp$vpath[[1]]) > 0) {
      all_on_paths <- unique(c(all_on_paths, as.integer(sp$vpath[[1]])))
    }
  }
}

# 1-hop neighbors of each seed (includes unmatched people)
for (sv in celeb_vid_full) {
  n1 <- neighbors(g_ind, sv)
  all_on_paths <- unique(c(all_on_paths, as.integer(n1), sv))
}

if (length(all_on_paths) >= 3) {
  sg_chain <- induced_subgraph(g_ind, all_on_paths)

  node_chain <- tibble(
    label      = V(sg_chain)$label,
    familia    = V(sg_chain)$familia,
    pais       = V(sg_chain)$pais,
    ocu        = V(sg_chain)$ocu_norm,
    is_virtual = V(sg_chain)$is_virtual,
    is_seed    = V(sg_chain)$label %in% seed_names
  )

  edge_chain <- igraph::as_data_frame(sg_chain, "edges") %>%
    mutate(
      from_label = V(sg_chain)$label[match(from, V(sg_chain)$name)],
      to_label   = V(sg_chain)$label[match(to, V(sg_chain)$name)]
    )

  write_csv(node_chain, file.path(OUT_TABLE, "interfamiliar_cadena_zoom_nodos.csv"))
  write_csv(edge_chain %>% select(from_label, to_label, tipo_label),
            file.path(OUT_TABLE, "interfamiliar_cadena_zoom_aristas.csv"))

  cat("\nCadena zoom:", vcount(sg_chain), "personas (",
      sum(node_chain$is_virtual, na.rm = TRUE), "virtuales),",
      ecount(sg_chain), "aristas\n")

  tg_chain <- as_tbl_graph(sg_chain)
  set.seed(77)

  p_zoom <- ggraph(tg_chain, layout = "fr") +
    geom_edge_link(aes(color = tipo_label), alpha = 0.7, width = 1.2) +
    geom_node_point(
      aes(fill   = familia,
          size   = case_when(label %in% seed_names ~ 16, is_virtual == TRUE ~ 5, TRUE ~ 8),
          shape  = ifelse(is_virtual == TRUE, "virtual", "real")),
      color = "gray30", stroke = 0.6, alpha = 0.85
    ) +
    geom_node_text(aes(label = label,
                       fontface = ifelse(label %in% seed_names, "bold", "plain")),
                   size = 3.2, repel = TRUE,
                   max.overlaps = 60, segment.color = "gray70") +
    scale_size_identity() +
    scale_shape_manual(values = c("real" = 21, "virtual" = 23),
                       name = NULL,
                       labels = c("real" = "En dataset", "virtual" = "Solo mencionado")) +
    scale_fill_viridis_d(name = "Familia", option = "turbo", na.value = "gray80") +
    scale_edge_color_manual(values = tipo_edge_color, name = "Relación",
                            na.value = "gray60") +
    guides(fill = guide_legend(override.aes = list(shape = 21, size = 5))) +
    labs(
      title = "Cadena completa: Menem ↔ Bolocco ↔ Vicuña ↔ China Suárez / Pampita / Bascuñán ↔ Aylwin",
      subtitle = paste0(vcount(sg_chain), " personas (", sum(node_chain$is_virtual, na.rm=TRUE),
                        " solo mencionados). Aristas por tipo de relación. Chile ↔ Argentina.")
    ) +
    theme_void(base_size = 11) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
          plot.subtitle = element_text(hjust = 0.5, color = "gray40", size = 10),
          legend.position = "bottom",
          legend.box = "horizontal",
          legend.text = element_text(size = 9))

  ggsave(file.path(OUT_FIG, "interfamiliar_cadena_zoom.png"), p_zoom,
         width = 20, height = 14, dpi = 200)
  cat("Guardado: interfamiliar_cadena_zoom.png\n")
}

# ───────────────────────────────────────────────────────────────────────────────
# 3c. LARGEST CROSS-FAMILY COMPONENT (all data)
# ───────────────────────────────────────────────────────────────────────────────

all_matched_only <- relaciones %>%
  filter(!is.na(persona_relacionada_id)) %>%
  select(from = persona_id, to = persona_relacionada_id, tipo_relacion)

cross_edges <- all_matched_only %>%
  left_join(personas %>% select(persona_id, fam1 = familia_norm), by = c("from" = "persona_id")) %>%
  left_join(personas %>% select(persona_id, fam2 = familia_norm), by = c("to" = "persona_id")) %>%
  filter(!is.na(fam1), !is.na(fam2), fam1 != fam2)

g_cross <- graph_from_data_frame(cross_edges %>% select(from, to), directed = FALSE)
g_cross <- igraph::simplify(g_cross, remove.multiple = TRUE, remove.loops = TRUE)

V(g_cross)$label   <- personas$nombre[match(as.integer(V(g_cross)$name), personas$persona_id)]
V(g_cross)$familia <- personas$familia_norm[match(as.integer(V(g_cross)$name), personas$persona_id)]
V(g_cross)$pais    <- personas$pais_efectivo[match(as.integer(V(g_cross)$name), personas$persona_id)]

comps <- components(g_cross)
top_comps <- order(comps$csize, decreasing = TRUE)[1:min(5, length(comps$csize))]

comp_summary <- tibble(
  componente = seq_along(top_comps),
  n_personas = comps$csize[top_comps],
  miembros   = map_chr(top_comps, ~ {
    who <- which(comps$membership == .x)
    labels <- V(g_cross)$label[who]
    paste(head(labels[order(nchar(labels))], 8), collapse = ", ")
  })
)
cat("\nTop 5 componentes inter-familiares:\n")
print(as.data.frame(comp_summary))

largest_comp <- top_comps[1]
sg_big <- induced_subgraph(g_cross, which(comps$membership == largest_comp))

cat("\nComponente conexa mayor:", vcount(sg_big), "personas,", ecount(sg_big), "vínculos\n")

chain_big_df <- tibble(
  nombre  = V(sg_big)$label,
  familia = V(sg_big)$familia,
  pais    = V(sg_big)$pais
)
write_csv(chain_big_df, file.path(OUT_TABLE, "interfamiliar_componente_mayor.csv"))

max_plot_nodes <- 200
if (vcount(sg_big) > max_plot_nodes) {
  deg <- degree(sg_big)
  top_nodes <- order(deg, decreasing = TRUE)[1:max_plot_nodes]
  sg_plot <- induced_subgraph(sg_big, top_nodes)
} else {
  sg_plot <- sg_big
}

tg_big <- as_tbl_graph(sg_plot)
set.seed(42)

p_mega <- ggraph(tg_big, layout = "fr") +
  geom_edge_link(alpha = 0.2, color = "gray60", width = 0.4) +
  geom_node_point(aes(color = familia), size = 4, alpha = 0.7) +
  geom_node_text(aes(label = label), size = 2, repel = TRUE,
                 max.overlaps = 30, fontface = "bold") +
  labs(title = "Componente conexa mayor: familias entrecruzadas",
       subtitle = paste0(vcount(sg_plot), " personas conectadas por vínculos inter-familiares"),
       color = "Familia") +
  theme_void(base_size = 10) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
        legend.position = "right",
        legend.text = element_text(size = 7))

ggsave(file.path(OUT_FIG, "interfamiliar_componente_mayor.png"), p_mega,
       width = 20, height = 15, dpi = 200)
cat("Guardado: interfamiliar_componente_mayor.png\n")

message("\nAnálisis inter-familiar completado.")
