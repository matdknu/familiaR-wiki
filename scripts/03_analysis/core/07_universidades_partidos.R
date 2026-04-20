#!/usr/bin/env Rscript
# ============================================================================
# 07_universidades_partidos.R
# Universidades y partidos políticos como nexos institucionales entre élites.
# Quién comparte universidad, quién comparte partido, y cómo eso cruza países.
# Lee: data/processed/02_leer_data/*.rds
# Escribe: outputs/tables/instituciones_*.csv
#          outputs/figures/instituciones_*.png
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

DATA_DIR  <- "data/processed/02_leer_data"
OUT_FIG   <- "outputs/figures"
OUT_TABLE <- "outputs/tables"
dir.create(OUT_FIG, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_TABLE, recursive = TRUE, showWarnings = FALSE)

personas  <- read_rds(file.path(DATA_DIR, "personas.rds"))
partidos  <- read_rds(file.path(DATA_DIR, "partidos.rds"))
educacion <- read_rds(file.path(DATA_DIR, "educacion.rds"))

# ═══════════════════════════════════════════════════════════════════════════════
# UNIVERSIDADES COMO NEXOS
# ═══════════════════════════════════════════════════════════════════════════════

uni_stats <- educacion %>%
  left_join(personas %>% select(persona_id, pais_base, familia_norm), by = "persona_id") %>%
  group_by(institucion) %>%
  summarise(
    alumnos = n(),
    paises = n_distinct(pais_base, na.rm = TRUE),
    lista_paises = paste(sort(unique(na.omit(pais_base))), collapse = ", "),
    familias = n_distinct(familia_norm, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(alumnos))

cat("Top 20 universidades que más conectan élites:\n")
print(head(uni_stats, 20))
write_csv(uni_stats, file.path(OUT_TABLE, "instituciones_universidades.csv"))

p_uni <- uni_stats %>%
  slice_head(n = 20) %>%
  mutate(institucion = fct_reorder(institucion, alumnos)) %>%
  ggplot(aes(alumnos, institucion, fill = factor(paises))) +
  geom_col(alpha = 0.85) +
  geom_text(aes(label = paste0(alumnos, " (", paises, " países)")),
            hjust = -0.05, size = 3) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(title = "Universities that most connect elites",
       subtitle = "Color = number of countries represented",
       x = "Personas", y = NULL, fill = "Países") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

ggsave(file.path(OUT_FIG, "instituciones_top_universidades.png"), p_uni,
       width = 11, height = 7, dpi = 150)

# Universidades transnacionales: conectan personas de distintos países
uni_transnacional <- educacion %>%
  left_join(personas %>% select(persona_id, pais_base, nombre), by = "persona_id") %>%
  filter(!is.na(pais_base)) %>%
  group_by(institucion) %>%
  filter(n_distinct(pais_base) > 1) %>%
  ungroup()

if (nrow(uni_transnacional) > 0) {
  uni_pares <- uni_transnacional %>%
    select(institucion, persona_id, pais_base) %>%
    inner_join(uni_transnacional %>% select(institucion, persona_id2 = persona_id, pais2 = pais_base),
               by = "institucion", relationship = "many-to-many") %>%
    filter(persona_id < persona_id2, pais_base != pais2) %>%
    mutate(par = paste(pmin(pais_base, pais2), pmax(pais_base, pais2), sep = " ↔ "))

  uni_cross <- uni_pares %>%
    count(par, institucion, sort = TRUE)

  cat("\nConexiones universitarias transnacionales (top 20):\n")
  print(head(uni_cross, 20))
  write_csv(uni_cross, file.path(OUT_TABLE, "instituciones_uni_transnacional.csv"))
}

# ═══════════════════════════════════════════════════════════════════════════════
# PARTIDOS POLÍTICOS COMO NEXOS
# ═══════════════════════════════════════════════════════════════════════════════

partido_stats <- partidos %>%
  left_join(personas %>% select(persona_id, pais_base, familia_norm), by = "persona_id") %>%
  group_by(partido) %>%
  summarise(
    miembros = n(),
    paises = n_distinct(pais_base, na.rm = TRUE),
    lista_paises = paste(sort(unique(na.omit(pais_base))), collapse = ", "),
    familias = n_distinct(familia_norm, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(miembros))

cat("\nTop 20 partidos por número de miembros de élite:\n")
print(head(partido_stats, 20))
write_csv(partido_stats, file.path(OUT_TABLE, "instituciones_partidos.csv"))

p_par <- partido_stats %>%
  slice_head(n = 20) %>%
  mutate(partido = fct_reorder(partido, miembros)) %>%
  ggplot(aes(miembros, partido, fill = factor(paises))) +
  geom_col(alpha = 0.85) +
  geom_text(aes(label = paste0(miembros, " (", familias, " fam.)")),
            hjust = -0.05, size = 3) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(title = "Parties with the most elite members",
       subtitle = "Color = countries. Label = members (families).",
       x = "Personas", y = NULL, fill = "Países") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

ggsave(file.path(OUT_FIG, "instituciones_top_partidos.png"), p_par,
       width = 11, height = 7, dpi = 150)

# ═══════════════════════════════════════════════════════════════════════════════
# RED BIPARTITA: FAMILIAS ↔ PARTIDOS (top familias y partidos)
# ═══════════════════════════════════════════════════════════════════════════════

top_partidos <- partido_stats %>% slice_head(n = 15) %>% pull(partido)
top_familias <- personas %>% count(familia_norm, sort = TRUE) %>%
  slice_head(n = 20) %>% pull(familia_norm)

bip <- partidos %>%
  left_join(personas %>% select(persona_id, familia_norm), by = "persona_id") %>%
  filter(partido %in% top_partidos, familia_norm %in% top_familias) %>%
  count(familia_norm, partido, name = "miembros") %>%
  filter(miembros >= 1)

if (nrow(bip) >= 5) {
  g_bip <- graph_from_data_frame(
    bip %>% select(from = familia_norm, to = partido, weight = miembros),
    directed = FALSE
  )
  V(g_bip)$type <- V(g_bip)$name %in% top_partidos

  p_bip <- as_tbl_graph(g_bip) %>%
    mutate(es_partido = type) %>%
    ggraph(layout = "bipartite") +
    geom_edge_link(aes(width = weight), alpha = 0.3, color = "gray50") +
    geom_node_point(aes(color = es_partido, shape = es_partido), size = 5) +
    geom_node_text(aes(label = str_trunc(name, 25)), repel = TRUE, size = 2.8) +
    scale_color_manual(values = c("FALSE" = "coral", "TRUE" = "steelblue"),
                       labels = c("Familia", "Partido")) +
    scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 17),
                       labels = c("Familia", "Partido")) +
    scale_edge_width_continuous(range = c(0.3, 2.5)) +
    labs(title = "Bipartite network: families ↔ parties",
         subtitle = "Top 20 families × top 15 parties. Edge width = shared members.",
         color = "Tipo", shape = "Tipo") +
    theme_void(base_size = 11) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5))

  ggsave(file.path(OUT_FIG, "instituciones_red_familia_partido.png"), p_bip,
         width = 14, height = 10, dpi = 150)
}

# ═══════════════════════════════════════════════════════════════════════════════
# CONCENTRACIÓN DE ÉLITE POR PAÍS: ¿pocos partidos dominan?
# ═══════════════════════════════════════════════════════════════════════════════

concentracion <- partidos %>%
  left_join(personas %>% select(persona_id, pais_base), by = "persona_id") %>%
  filter(!is.na(pais_base)) %>%
  group_by(pais_base) %>%
  summarise(
    total_miembros = n(),
    n_partidos = n_distinct(partido),
    top3_partidos = paste(
      (count(tibble(partido = partido), partido, sort = TRUE) %>% slice_head(n = 3) %>% pull(partido)),
      collapse = " | "
    ),
    share_top3 = {
      top <- count(tibble(partido = partido), partido, sort = TRUE) %>% slice_head(n = 3) %>% pull(n)
      sum(top) / n()
    },
    .groups = "drop"
  ) %>%
  arrange(desc(share_top3))

cat("\nConcentración partidaria por país:\n")
print(concentracion)
write_csv(concentracion, file.path(OUT_TABLE, "instituciones_concentracion_partidos.csv"))

message("Análisis de instituciones completado.")
