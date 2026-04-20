#!/usr/bin/env Rscript
# ============================================================================
# 04_endogamia_matrimonial.R
# Análisis de endogamia familiar y alianzas matrimoniales entre familias.
# Lee: data/processed/02_leer_data/*.rds
# Escribe: outputs/tables/endogamia_*.csv
#          outputs/figures/endogamia_*.png
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

# ═══════════════════════════════════════════════════════════════════════════════
# MATRIMONIOS IDENTIFICADOS
# ═══════════════════════════════════════════════════════════════════════════════

# H21: Deduplicate marriages — A↔B and B↔A count as one union
matrimonios_raw <- relaciones %>%
  filter(tipo_relacion %in% c("conyuge", "pareja"), !is.na(persona_relacionada_id)) %>%
  left_join(
    personas %>% select(persona_id, fam1 = familia_norm, pais1 = pais_base, nombre1 = nombre),
    by = "persona_id"
  ) %>%
  left_join(
    personas %>% select(persona_id, fam2 = familia_norm, pais2 = pais_base, nombre2 = nombre),
    by = c("persona_relacionada_id" = "persona_id")
  ) %>%
  filter(!is.na(fam1), !is.na(fam2))

# Canonical pair: always min_id first to deduplicate A↔B / B↔A
matrimonios <- matrimonios_raw %>%
  mutate(
    id_min = pmin(persona_id, persona_relacionada_id),
    id_max = pmax(persona_id, persona_relacionada_id)
  ) %>%
  distinct(id_min, id_max, .keep_all = TRUE) %>%
  select(-id_min, -id_max)

cat("Total matrimonios/parejas (deduplicados):", nrow(matrimonios), "\n")

# ═══════════════════════════════════════════════════════════════════════════════
# ENDOGAMIA vs EXOGAMIA
# ═══════════════════════════════════════════════════════════════════════════════

# H20: Make categories mutually exclusive and more informative
# Cross both dimensions: endogamy (same/diff family) × geography (same/diff country)
matrimonios <- matrimonios %>%
  mutate(
    es_endogamico = (fam1 == fam2),
    es_transnacional = (!is.na(pais1) & !is.na(pais2) & pais1 != pais2),
    tipo_union = case_when(
      es_endogamico & es_transnacional ~ "endogámico transnacional",
      es_endogamico & !es_transnacional ~ "endogámico (mismo país)",
      !es_endogamico & es_transnacional ~ "exogámico transnacional",
      TRUE ~ "exogámico (mismo país)"
    )
  )

resumen_tipo <- matrimonios %>% count(tipo_union, sort = TRUE)
cat("\nTipos de unión:\n")
print(resumen_tipo)

write_csv(resumen_tipo, file.path(OUT_TABLE, "endogamia_resumen_tipos.csv"))

# Tasa de endogamia por país
endogamia_pais <- matrimonios %>%
  filter(!is.na(pais1)) %>%
  group_by(pais1) %>%
  summarise(
    total = n(),
    endogamicos = sum(es_endogamico),
    tasa_endogamia = endogamicos / total,
    transnacionales = sum(es_transnacional),
    tasa_transnacional = transnacionales / total,
    exogamicos_mismo_pais = sum(!es_endogamico & !es_transnacional),
    .groups = "drop"
  ) %>%
  arrange(desc(tasa_endogamia))

cat("\nEndogamia por país:\n")
print(endogamia_pais)
write_csv(endogamia_pais, file.path(OUT_TABLE, "endogamia_por_pais.csv"))

p_endo <- endogamia_pais %>%
  filter(total >= 5) %>%
  pivot_longer(cols = c(tasa_endogamia, tasa_transnacional),
               names_to = "indicador", values_to = "tasa") %>%
  mutate(
    indicador = recode(indicador,
                       tasa_endogamia = "Endogamy rate",
                       tasa_transnacional = "Transnational marriage rate"),
    pais1 = fct_reorder(pais1, tasa, .fun = max)
  ) %>%
  ggplot(aes(tasa, pais1, fill = indicador)) +
  geom_col(position = "dodge", alpha = 0.85) +
  scale_x_continuous(labels = scales::percent_format()) +
  labs(title = "Endogamy and transnational marriage rates by country",
       subtitle = "Countries with ≥ 5 identified unions only",
       x = "Rate", y = "Country", fill = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"), legend.position = "top")

ggsave(file.path(OUT_FIG, "endogamia_tasas_por_pais.png"), p_endo,
       width = 9, height = 6, dpi = 150)

# ═══════════════════════════════════════════════════════════════════════════════
# ALIANZAS ENTRE FAMILIAS (red de matrimonios inter-familia)
# ═══════════════════════════════════════════════════════════════════════════════

# H21: Canonical family pairs (already deduplicated marriages above)
alianzas <- matrimonios %>%
  filter(fam1 != fam2) %>%
  mutate(
    from = pmin(fam1, fam2),
    to = pmax(fam1, fam2)
  ) %>%
  group_by(from, to) %>%
  summarise(
    matrimonios = n(),
    personas = paste(unique(c(nombre1, nombre2)), collapse = " | "),
    paises = paste(sort(unique(c(pais1, pais2))), collapse = ", "),
    .groups = "drop"
  ) %>%
  arrange(desc(matrimonios))

cat("\nTop 20 alianzas matrimoniales entre familias:\n")
print(head(alianzas, 20))
write_csv(alianzas, file.path(OUT_TABLE, "endogamia_alianzas_interfamilia.csv"))

# Red de alianzas (solo pares con 2+ matrimonios)
alianzas_fuertes <- alianzas %>% filter(matrimonios >= 2)

if (nrow(alianzas_fuertes) >= 3) {
  g_alianzas <- tbl_graph(edges = alianzas_fuertes, directed = FALSE)

  p_alianzas <- g_alianzas %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(width = matrimonios, alpha = matrimonios), color = "darkred") +
    geom_node_point(size = 4, color = "darkred", alpha = 0.7) +
    geom_node_text(aes(label = name), repel = TRUE, size = 3) +
    scale_edge_width_continuous(range = c(0.5, 3)) +
    labs(title = "Marriage alliances between families",
         subtitle = "Pairs with 2+ marriages. Edge width = count.") +
    theme_void(base_size = 11) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5))

  ggsave(file.path(OUT_FIG, "endogamia_red_alianzas.png"), p_alianzas,
         width = 12, height = 10, dpi = 150)
}

# ═══════════════════════════════════════════════════════════════════════════════
# MATRIMONIOS TRANSNACIONALES
# ═══════════════════════════════════════════════════════════════════════════════

transnacionales <- matrimonios %>%
  filter(!is.na(pais1), !is.na(pais2), pais1 != pais2) %>%
  select(nombre1, nombre2, fam1, fam2, pais1, pais2, tipo_relacion) %>%
  arrange(pais1, pais2)

cat("\nMatrimonios transnacionales:", nrow(transnacionales), "\n")
write_csv(transnacionales, file.path(OUT_TABLE, "endogamia_transnacionales.csv"))

if (nrow(transnacionales) > 0) {
  p_trans <- transnacionales %>%
    mutate(par_paises = paste(pmin(pais1, pais2), pmax(pais1, pais2), sep = " ↔ ")) %>%
    count(par_paises, sort = TRUE) %>%
    mutate(par_paises = fct_reorder(par_paises, n)) %>%
    ggplot(aes(n, par_paises)) +
    geom_col(fill = "steelblue", alpha = 0.85) +
    geom_text(aes(label = n), hjust = -0.1, size = 3.5) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
    labs(title = "Transnational marriages by country pair",
         x = "Unions", y = NULL) +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold"))

  ggsave(file.path(OUT_FIG, "endogamia_transnacionales_paises.png"), p_trans,
         width = 9, height = 6, dpi = 150)
}

message("Análisis de endogamia completado.")
