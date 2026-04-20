#!/usr/bin/env Rscript
# ============================================================================
# 06_conexiones_transnacionales.R
# Red entre países: vínculos por parentesco cruzado, lugar de muerte,
# residencia, nacionalidad múltiple y señales biográficas.
# Lee: data/processed/02_leer_data/*.rds
#      data/manual/url_pais_extra.csv
# Escribe: outputs/tables/transnacional_*.csv
#          outputs/figures/transnacional_*.png
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

personas   <- read_rds(file.path(DATA_DIR, "personas.rds"))
relaciones <- read_rds(file.path(DATA_DIR, "relaciones.rds"))

# Override manual de país
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

keywords_por_pais <- list(
  argentina = c("argentina", "buenos aires", "córdoba", "rosario", "argentino", "argentinas"),
  chile = c("chile", "santiago", "valparaíso", "chileno", "chilena"),
  mexico = c("méxico", "mexico", "ciudad de méxico", "mexicano", "mexicana"),
  peru = c("perú", "peru", "lima", "peruano", "peruana"),
  colombia = c("colombia", "bogotá", "medellín", "colombiano", "colombiana"),
  venezuela = c("venezuela", "caracas", "venezolano", "venezolana"),
  bolivia = c("bolivia", "la paz", "boliviano", "boliviana"),
  paraguay = c("paraguay", "asunción", "paraguayo", "paraguaya"),
  uruguay = c("uruguay", "montevideo", "uruguayo", "uruguaya"),
  ecuador = c("ecuador", "quito", "ecuatoriano", "ecuatoriana")
)

detectar_paises_en_texto <- function(texto, excluir = NULL) {
  if (is.na(texto) || nchar(trimws(texto)) == 0) return(character())
  t <- tolower(trimws(texto))
  encontrados <- character()
  for (pais in names(keywords_por_pais)) {
    if (!is.null(excluir) && pais %in% excluir) next
    if (any(str_detect(t, fixed(keywords_por_pais[[pais]])))) {
      encontrados <- c(encontrados, pais)
    }
  }
  encontrados
}

# ═══════════════════════════════════════════════════════════════════════════════
# 1. CONEXIONES POR PARENTESCO CRUZADO
# ═══════════════════════════════════════════════════════════════════════════════

cross_parentesco <- relaciones %>%
  filter(!is.na(persona_relacionada_id)) %>%
  left_join(personas %>% select(persona_id, pais1 = pais_efectivo, nombre1 = nombre), by = "persona_id") %>%
  left_join(personas %>% select(persona_id, pais2 = pais_efectivo, nombre2 = nombre),
            by = c("persona_relacionada_id" = "persona_id")) %>%
  filter(!is.na(pais1), !is.na(pais2), pais1 != pais2)

cat("Conexiones de parentesco entre países:", nrow(cross_parentesco), "\n")

if (nrow(cross_parentesco) > 0) {
  cross_par_agg <- cross_parentesco %>%
    mutate(par = paste(pmin(pais1, pais2), pmax(pais1, pais2), sep = " ↔ ")) %>%
    count(par, sort = TRUE)

  write_csv(cross_par_agg, file.path(OUT_TABLE, "transnacional_parentesco_pares.csv"))

  detalle <- cross_parentesco %>%
    select(nombre1, nombre2, pais1, pais2, tipo_relacion) %>%
    arrange(pais1, pais2)
  write_csv(detalle, file.path(OUT_TABLE, "transnacional_parentesco_detalle.csv"))
}

# ═══════════════════════════════════════════════════════════════════════════════
# 2. SEÑALES BIOGRÁFICAS TRANSNACIONALES
# ═══════════════════════════════════════════════════════════════════════════════

campos_texto <- c("lugar_fallecimiento", "lugar_fallecimiento_parseado",
                   "residencia", "lugar_nacimiento_parseado", "ocupacion")
campos_texto <- intersect(campos_texto, names(personas))

senales <- list()
for (i in seq_len(nrow(personas))) {
  pais_persona <- personas$pais_efectivo[i]
  if (is.na(pais_persona)) next
  for (campo in campos_texto) {
    texto <- as.character(personas[[campo]][i])
    paises_detectados <- detectar_paises_en_texto(texto, excluir = pais_persona)
    for (p in paises_detectados) {
      senales[[length(senales) + 1]] <- tibble(
        persona_id = personas$persona_id[i],
        nombre = personas$nombre[i],
        pais_persona = pais_persona,
        pais_vinculo = p,
        campo = campo,
        texto = str_trunc(as.character(texto), 100)
      )
    }
  }
}

senales_df <- bind_rows(senales) %>%
  distinct(persona_id, pais_vinculo, campo, .keep_all = TRUE)

cat("Señales biográficas transnacionales:", nrow(senales_df), "\n")
write_csv(senales_df, file.path(OUT_TABLE, "transnacional_senales_biograficas.csv"))

# ═══════════════════════════════════════════════════════════════════════════════
# 3. RED AGREGADA ENTRE PAÍSES
# ═══════════════════════════════════════════════════════════════════════════════

agg_parentesco <- if (nrow(cross_parentesco) > 0) {
  cross_parentesco %>%
    transmute(from = pmin(pais1, pais2), to = pmax(pais1, pais2)) %>%
    count(from, to, name = "n_parentesco")
} else {
  tibble(from = character(), to = character(), n_parentesco = integer())
}

agg_biografico <- senales_df %>%
  transmute(from = pmin(pais_persona, pais_vinculo), to = pmax(pais_persona, pais_vinculo)) %>%
  count(from, to, name = "n_biografico")

red_paises <- agg_parentesco %>%
  full_join(agg_biografico, by = c("from", "to")) %>%
  replace_na(list(n_parentesco = 0L, n_biografico = 0L)) %>%
  mutate(n_total = n_parentesco + n_biografico) %>%
  filter(n_total > 0) %>%
  arrange(desc(n_total))

cat("\nRed entre países:\n")
print(red_paises)
write_csv(red_paises, file.path(OUT_TABLE, "transnacional_red_paises.csv"))

# Grafo de países
if (nrow(red_paises) >= 1) {
  paises_unicos <- unique(c(red_paises$from, red_paises$to))
  g_paises <- graph_from_data_frame(
    red_paises %>% select(from, to, weight = n_total),
    directed = FALSE,
    vertices = tibble(name = paises_unicos)
  )

  set.seed(42)
  lay <- layout_with_fr(g_paises)
  v_df <- tibble(pais = V(g_paises)$name, x = lay[, 1], y = lay[, 2])
  e_df <- red_paises %>%
    left_join(v_df, by = c("from" = "pais")) %>% rename(x1 = x, y1 = y) %>%
    left_join(v_df, by = c("to" = "pais")) %>% rename(x2 = x, y2 = y)

  p_red <- ggplot() +
    geom_segment(data = e_df,
                 aes(x = x1, y = y1, xend = x2, yend = y2, linewidth = n_total),
                 color = "gray40", alpha = 0.6) +
    geom_point(data = v_df, aes(x, y), size = 14, fill = "steelblue",
               color = "white", shape = 21, stroke = 1.5) +
    geom_text(data = v_df, aes(x, y, label = str_to_title(pais)),
              size = 3, fontface = "bold") +
    scale_linewidth_continuous(range = c(0.3, 3), name = "Ties") +
    labs(title = "Transnational network between countries",
         subtitle = "Cross-border kinship + biographical signals") +
    theme_void(base_size = 11) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5, color = "gray40"))

  ggsave(file.path(OUT_FIG, "transnacional_red_paises.png"), p_red,
         width = 10, height = 8, dpi = 150)
}

# ═══════════════════════════════════════════════════════════════════════════════
# 4. FAMILIAS MULTINACIONALES
# ═══════════════════════════════════════════════════════════════════════════════

familias_multi <- personas %>%
  filter(!is.na(pais_efectivo), !is.na(familia_norm)) %>%
  group_by(familia_norm) %>%
  summarise(
    paises = paste(sort(unique(pais_efectivo)), collapse = ", "),
    n_paises = n_distinct(pais_efectivo),
    n_miembros = n(),
    .groups = "drop"
  ) %>%
  filter(n_paises > 1) %>%
  arrange(desc(n_paises), desc(n_miembros))

cat("\nFamilias transnacionales:", nrow(familias_multi), "\n")
print(head(familias_multi, 20))
write_csv(familias_multi, file.path(OUT_TABLE, "transnacional_familias_multi.csv"))

library(patchwork)
library(scales)

cap <- function(x) str_to_title(x)

country_pal <- c(
  "Argentina" = "#74b9ff", "Bolivia" = "#00b894", "Chile" = "#0984e3",
  "Colombia" = "#fdcb6e", "Ecuador" = "#e17055", "Mexico" = "#00cec9",
  "Paraguay" = "#d63031", "Peru" = "#e84393", "Uruguay" = "#6c5ce7",
  "Venezuela" = "#ff7675"
)

# ═══════════════════════════════════════════════════════════════════════════════
# 5. FAMILIAS MULTINACIONALES — TILE MAP
# ═══════════════════════════════════════════════════════════════════════════════
# Show which countries each multinational family spans

fam_pais_long <- personas %>%
  filter(!is.na(pais_efectivo), !is.na(familia_norm)) %>%
  semi_join(familias_multi, by = "familia_norm") %>%
  count(familia_norm, pais_efectivo, name = "n_miembros")

top_multi <- familias_multi %>%
  slice_head(n = 30) %>%
  pull(familia_norm)

tile_data <- fam_pais_long %>%
  filter(familia_norm %in% top_multi) %>%
  mutate(
    pais_label = cap(pais_efectivo),
    familia_label = str_replace_all(familia_norm, "_", " ") %>% str_to_title()
  )

p_tile <- tile_data %>%
  mutate(familia_label = fct_rev(fct_reorder(familia_label, n_miembros, .fun = sum))) %>%
  ggplot(aes(pais_label, familia_label, fill = n_miembros)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = n_miembros), size = 3, fontface = "bold") +
  scale_fill_gradient(low = "#dfe6e9", high = "#d63031", name = "Miembros") +
  labs(title = "Familias multinacionales — presencia por país",
       subtitle = "Top 30 familias con miembros en más de un país",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 30, hjust = 1, face = "bold"),
        axis.text.y = element_text(size = 9),
        panel.grid = element_blank())

ggsave(file.path(OUT_FIG, "transnacional_familias_tile.png"), p_tile,
       width = 12, height = 10, dpi = 200)
cat("Guardado: transnacional_familias_tile.png\n")

# ═══════════════════════════════════════════════════════════════════════════════
# 6. RED DE FAMILIAS MULTINACIONALES ↔ PAÍSES
# ═══════════════════════════════════════════════════════════════════════════════

bip_edges <- tile_data %>%
  transmute(from = familia_label, to = pais_label, weight = n_miembros)

if (nrow(bip_edges) >= 5) {
  g_bip <- graph_from_data_frame(bip_edges, directed = FALSE)
  V(g_bip)$type <- V(g_bip)$name %in% unique(tile_data$pais_label)

  p_bip <- as_tbl_graph(g_bip) %>%
    mutate(
      es_pais = type,
      grado = centrality_degree()
    ) %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(width = weight), alpha = 0.3, color = "gray50") +
    geom_node_point(aes(color = es_pais, size = grado), alpha = 0.8) +
    geom_node_text(aes(label = str_trunc(name, 22)), repel = TRUE, size = 2.8) +
    scale_color_manual(values = c("FALSE" = "#d63031", "TRUE" = "#0984e3"),
                       labels = c("Familia", "País")) +
    scale_size_continuous(range = c(3, 12)) +
    scale_edge_width_continuous(range = c(0.3, 3), name = "Miembros") +
    labs(title = "Red bipartita: Familias multinacionales ↔ Países",
         subtitle = "Grosor = miembros de esa familia en ese país",
         color = "Tipo", size = "Conexiones") +
    theme_void(base_size = 11) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5))

  ggsave(file.path(OUT_FIG, "transnacional_familias_red_bipartita.png"), p_bip,
         width = 14, height = 10, dpi = 200)
  cat("Guardado: transnacional_familias_red_bipartita.png\n")
}

# ═══════════════════════════════════════════════════════════════════════════════
# 7. RED ENTRE PAÍSES POR TIPO DE RELACIÓN
# ═══════════════════════════════════════════════════════════════════════════════
# Separate cross-country connections by: kinship type, biographical signal type

# 7a. Parentesco cruzado por tipo de relación
if (nrow(cross_parentesco) > 0) {
  cross_por_tipo <- cross_parentesco %>%
    mutate(
      par = paste(cap(pmin(pais1, pais2)), cap(pmax(pais1, pais2)), sep = " ↔ "),
      tipo_label = case_when(
        tipo_relacion %in% c("conyuge", "pareja") ~ "Pareja / Cónyuge",
        tipo_relacion == "padre/madre" ~ "Padre / Madre",
        tipo_relacion == "hijo/a" ~ "Hijo / Hija",
        tipo_relacion == "hermano/a" ~ "Hermano / Hermana",
        TRUE ~ "Otro"
      )
    ) %>%
    count(par, tipo_label, sort = TRUE)

  write_csv(cross_por_tipo, file.path(OUT_TABLE, "transnacional_parentesco_por_tipo.csv"))

  p_tipo_par <- cross_por_tipo %>%
    group_by(par) %>%
    mutate(total = sum(n)) %>%
    ungroup() %>%
    filter(total >= 3) %>%
    mutate(par = fct_reorder(par, total)) %>%
    ggplot(aes(n, par, fill = tipo_label)) +
    geom_col(alpha = 0.85) +
    scale_fill_brewer(palette = "Set2", name = "Tipo de relación") +
    labs(title = "Conexiones entre países por tipo de parentesco",
         subtitle = "Solo pares con ≥ 3 conexiones totales",
         x = "Conexiones", y = NULL) +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold"),
          legend.position = "top")

  ggsave(file.path(OUT_FIG, "transnacional_parentesco_por_tipo.png"), p_tipo_par,
         width = 11, height = 8, dpi = 200)
  cat("Guardado: transnacional_parentesco_por_tipo.png\n")
}

# 7b. Señales biográficas por tipo de campo
senales_por_tipo <- senales_df %>%
  mutate(
    par = paste(cap(pmin(pais_persona, pais_vinculo)),
                cap(pmax(pais_persona, pais_vinculo)), sep = " ↔ "),
    tipo_senal = case_when(
      campo %in% c("lugar_nacimiento_parseado") ~ "Nació allí",
      campo %in% c("lugar_fallecimiento", "lugar_fallecimiento_parseado") ~ "Murió allí",
      campo == "residencia" ~ "Residió allí",
      campo == "ocupacion" ~ "Trabajó allí",
      TRUE ~ "Otro"
    )
  ) %>%
  count(par, tipo_senal, sort = TRUE)

write_csv(senales_por_tipo, file.path(OUT_TABLE, "transnacional_senales_por_tipo.csv"))

p_tipo_bio <- senales_por_tipo %>%
  group_by(par) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  filter(total >= 3) %>%
  mutate(par = fct_reorder(par, total)) %>%
  ggplot(aes(n, par, fill = tipo_senal)) +
  geom_col(alpha = 0.85) +
  scale_fill_brewer(palette = "Dark2", name = "Tipo de señal") +
  labs(title = "Señales biográficas transnacionales por tipo",
       subtitle = "Nació, murió, residió o trabajó en otro país. Pares con ≥ 3 señales.",
       x = "Señales", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "top")

ggsave(file.path(OUT_FIG, "transnacional_senales_por_tipo.png"), p_tipo_bio,
       width = 11, height = 8, dpi = 200)
cat("Guardado: transnacional_senales_por_tipo.png\n")

# 7c. Red entre países con color por motivo dominante
red_detallada <- bind_rows(
  cross_parentesco %>%
    mutate(
      from = pmin(pais1, pais2), to = pmax(pais1, pais2),
      motivo = case_when(
        tipo_relacion %in% c("conyuge", "pareja") ~ "Matrimonio/Pareja",
        tipo_relacion %in% c("padre/madre", "hijo/a") ~ "Filiación",
        TRUE ~ "Otro parentesco"
      )
    ) %>%
    count(from, to, motivo, name = "n"),
  senales_df %>%
    mutate(
      from = pmin(pais_persona, pais_vinculo),
      to = pmax(pais_persona, pais_vinculo),
      motivo = case_when(
        campo %in% c("lugar_nacimiento_parseado") ~ "Nació allí",
        campo %in% c("lugar_fallecimiento", "lugar_fallecimiento_parseado") ~ "Murió allí",
        campo == "residencia" ~ "Residió allí",
        campo == "ocupacion" ~ "Trabajó allí",
        TRUE ~ "Otro"
      )
    ) %>%
    count(from, to, motivo, name = "n")
) %>%
  group_by(from, to, motivo) %>%
  summarise(n = sum(n), .groups = "drop")

red_agg_motivo <- red_detallada %>%
  group_by(from, to) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  filter(total >= 3)

motivo_pal <- c(
  "Matrimonio/Pareja" = "#e84393",
  "Filiación" = "#6c5ce7",
  "Otro parentesco" = "#a29bfe",
  "Nació allí" = "#00b894",
  "Murió allí" = "#d63031",
  "Residió allí" = "#0984e3",
  "Trabajó allí" = "#fdcb6e"
)

# 7c-i. Stacked bar: all motives per country pair
if (nrow(red_agg_motivo) > 0) {
  p_bar_all <- red_agg_motivo %>%
    mutate(
      par = paste(cap(from), cap(to), sep = " ↔ "),
      par = fct_reorder(par, total),
      motivo = factor(motivo, levels = names(motivo_pal))
    ) %>%
    ggplot(aes(n, par, fill = motivo)) +
    geom_col(alpha = 0.85) +
    scale_fill_manual(values = motivo_pal, name = "Motivo de conexión") +
    labs(title = "Conexiones entre países — parentesco + biográficas",
         subtitle = "Todos los motivos combinados. Pares con ≥ 3 conexiones.",
         x = "Conexiones", y = NULL) +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold"),
          legend.position = "right",
          legend.text = element_text(size = 9))

  ggsave(file.path(OUT_FIG, "transnacional_red_todos_motivos.png"), p_bar_all,
         width = 13, height = 9, dpi = 200)
  cat("Guardado: transnacional_red_todos_motivos.png\n")
}

# 7c-ii. Network graph: countries as nodes, edges colored by dominant motive
red_para_grafo <- red_detallada %>%
  group_by(from, to) %>%
  summarise(
    n_total = sum(n),
    n_parentesco = sum(n[motivo %in% c("Matrimonio/Pareja", "Filiación", "Otro parentesco")]),
    n_biografico = sum(n[motivo %in% c("Nació allí", "Murió allí", "Residió allí", "Trabajó allí")]),
    motivo_dom = motivo[which.max(n)],
    detalle = paste(paste0(motivo, "=", n), collapse = " | "),
    .groups = "drop"
  ) %>%
  filter(n_total >= 2) %>%
  mutate(
    tipo_dom = case_when(
      n_parentesco > n_biografico ~ "Parentesco",
      n_biografico > n_parentesco ~ "Biográfico",
      TRUE ~ "Mixto"
    )
  )

if (nrow(red_para_grafo) >= 1) {
  paises_g <- unique(c(red_para_grafo$from, red_para_grafo$to))
  g_mix <- graph_from_data_frame(
    red_para_grafo %>% select(from, to, weight = n_total),
    directed = FALSE,
    vertices = tibble(name = paises_g)
  )

  set.seed(42)
  lay_mix <- layout_with_fr(g_mix)
  v_mix <- tibble(pais = V(g_mix)$name, x = lay_mix[, 1], y = lay_mix[, 2])
  e_mix <- red_para_grafo %>%
    left_join(v_mix, by = c("from" = "pais")) %>% rename(x1 = x, y1 = y) %>%
    left_join(v_mix, by = c("to" = "pais")) %>% rename(x2 = x, y2 = y) %>%
    drop_na(x1, y1, x2, y2)

  tipo_col <- c("Parentesco" = "#6c5ce7", "Biográfico" = "#00b894", "Mixto" = "#fdcb6e")

  p_net_mix <- ggplot() +
    geom_segment(data = e_mix,
                 aes(x = x1, y = y1, xend = x2, yend = y2,
                     linewidth = n_total, color = tipo_dom),
                 alpha = 0.65) +
    geom_point(data = v_mix, aes(x, y), size = 16, fill = "white",
               color = "gray30", shape = 21, stroke = 1.2) +
    geom_text(data = v_mix, aes(x, y, label = cap(pais)),
              size = 3.2, fontface = "bold") +
    scale_linewidth_continuous(range = c(0.5, 4), name = "Vínculos totales") +
    scale_color_manual(values = tipo_col, name = "Tipo dominante") +
    labs(title = "Red entre países — parentesco vs. biográfico",
         subtitle = "Color de arista: tipo dominante de conexión. Grosor = total.") +
    theme_void(base_size = 11) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
          legend.position = "bottom",
          legend.box = "horizontal")

  ggsave(file.path(OUT_FIG, "transnacional_red_parentesco_vs_biografico.png"), p_net_mix,
         width = 11, height = 9, dpi = 200)
  cat("Guardado: transnacional_red_parentesco_vs_biografico.png\n")

  # 7c-iii. Faceted heatmap: motives as rows, country pairs as cols
  heat_data <- red_detallada %>%
    mutate(
      par = paste(cap(from), cap(to), sep = "\n↔ "),
      motivo = factor(motivo, levels = rev(names(motivo_pal)))
    ) %>%
    group_by(par) %>%
    mutate(total_par = sum(n)) %>%
    ungroup() %>%
    filter(total_par >= 5)

  if (nrow(heat_data) > 0) {
    p_heat <- heat_data %>%
      mutate(par = fct_reorder(par, total_par)) %>%
      ggplot(aes(par, motivo, fill = n)) +
      geom_tile(color = "white", linewidth = 0.6) +
      geom_text(aes(label = ifelse(n > 0, n, "")), size = 3, fontface = "bold") +
      scale_fill_gradient(low = "#f8f9fa", high = "#d63031", name = "Conexiones") +
      labs(title = "Motivos de conexión entre países (heatmap)",
           subtitle = "Parentesco + biográficas. Solo pares con ≥ 5 conexiones.",
           x = NULL, y = NULL) +
      theme_minimal(base_size = 11) +
      theme(plot.title = element_text(face = "bold"),
            axis.text.x = element_text(angle = 40, hjust = 1, size = 8),
            axis.text.y = element_text(size = 9),
            panel.grid = element_blank())

    ggsave(file.path(OUT_FIG, "transnacional_heatmap_motivos.png"), p_heat,
           width = 16, height = 7, dpi = 200)
    cat("Guardado: transnacional_heatmap_motivos.png\n")
  }
}

write_csv(red_detallada, file.path(OUT_TABLE, "transnacional_red_detallada_motivos.csv"))
write_csv(red_para_grafo, file.path(OUT_TABLE, "transnacional_red_paises_tipo_dom.csv"))

message("Análisis transnacional completado.")
