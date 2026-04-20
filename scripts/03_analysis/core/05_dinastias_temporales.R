#!/usr/bin/env Rscript
# ============================================================================
# 05_dinastias_temporales.R
# Persistencia dinástica: familias que persisten a través de siglos,
# sucesiones en cargos entre miembros de la misma familia, y distribución
# temporal por país.
# Lee: data/processed/02_leer_data/*.rds
# Escribe: outputs/tables/dinastias_*.csv
#          outputs/figures/dinastias_*.png
# ============================================================================

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(forcats)
library(scales)

DATA_DIR  <- "data/processed/02_leer_data"
OUT_FIG   <- "outputs/figures"
OUT_TABLE <- "outputs/tables"
dir.create(OUT_FIG, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_TABLE, recursive = TRUE, showWarnings = FALSE)

personas   <- read_rds(file.path(DATA_DIR, "personas.rds"))
sucesiones <- read_rds(file.path(DATA_DIR, "sucesiones.rds"))

# ═══════════════════════════════════════════════════════════════════════════════
# PERSISTENCIA DINÁSTICA POR FAMILIA
# ═══════════════════════════════════════════════════════════════════════════════

# H23: Defensive filter for impossible years (should be cleaned in ETL already)
dinastias <- personas %>%
  filter(!is.na(anio_nacimiento), !is.na(familia_norm),
         anio_nacimiento >= 1300, anio_nacimiento <= 2026) %>%
  group_by(familia_norm) %>%
  summarise(
    primer_miembro = min(anio_nacimiento),
    ultimo_miembro = max(anio_nacimiento),
    rango_temporal = max(anio_nacimiento) - min(anio_nacimiento),
    n_miembros = n(),
    n_siglos = n_distinct((anio_nacimiento %/% 100) + 1L),
    siglos = paste(sort(unique((anio_nacimiento %/% 100) + 1L)), collapse = ", "),
    paises = paste(sort(unique(na.omit(pais_base))), collapse = ", "),
    n_paises = n_distinct(na.omit(pais_base)),
    .groups = "drop"
  ) %>%
  arrange(desc(rango_temporal))

cat("Familias con presencia en 3+ siglos:\n")
dinastias %>% filter(n_siglos >= 3) %>% print(n = 30)
write_csv(dinastias, file.path(OUT_TABLE, "dinastias_persistencia.csv"))

# Top familias por rango temporal
p_dinas <- dinastias %>%
  filter(n_miembros >= 3) %>%
  slice_max(rango_temporal, n = 30) %>%
  mutate(familia_norm = fct_reorder(familia_norm, rango_temporal)) %>%
  ggplot(aes(y = familia_norm)) +
  geom_segment(aes(x = primer_miembro, xend = ultimo_miembro,
                   yend = familia_norm, color = factor(n_paises)),
               linewidth = 2, alpha = 0.7) +
  geom_point(aes(x = primer_miembro), size = 2) +
  geom_point(aes(x = ultimo_miembro), size = 2) +
  labs(title = "Dynastic persistence — top 30 families by birth-year span",
       subtitle = "Segment = first to last birth year. Color = number of countries.",
       x = "Birth year", y = NULL, color = "N countries") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

ggsave(file.path(OUT_FIG, "dinastias_rango_temporal.png"), p_dinas,
       width = 11, height = 9, dpi = 150)

# ═══════════════════════════════════════════════════════════════════════════════
# DISTRIBUCIÓN TEMPORAL POR PAÍS
# ═══════════════════════════════════════════════════════════════════════════════

temporal_pais <- personas %>%
  filter(!is.na(anio_nacimiento), !is.na(pais_base),
         anio_nacimiento >= 1500, anio_nacimiento <= 2010) %>%
  mutate(
    pais_label = str_to_title(pais_base),
    periodo = cut(anio_nacimiento,
                  breaks = seq(1500, 2050, 50),
                  labels = paste0(seq(1500, 2000, 50), "s"),
                  right = FALSE)
  )

library(ggridges)

country_pal <- c(
  "Argentina" = "#74b9ff", "Bolivia" = "#00b894", "Chile" = "#0984e3",
  "Colombia" = "#fdcb6e", "Ecuador" = "#e17055", "Mexico" = "#00cec9",
  "Paraguay" = "#d63031", "Peru" = "#e84393", "Uruguay" = "#6c5ce7",
  "Venezuela" = "#ff7675"
)

n_por_pais <- temporal_pais %>%
  count(pais_label, name = "n_total") %>%
  mutate(etiqueta = paste0(pais_label, " (n=", n_total, ")"))

temporal_pais <- temporal_pais %>%
  left_join(n_por_pais, by = "pais_label") %>%
  mutate(etiqueta = fct_reorder(etiqueta, anio_nacimiento, .fun = median))

p_temp <- ggplot(temporal_pais, aes(x = anio_nacimiento, y = etiqueta, fill = pais_label)) +
  geom_density_ridges(
    alpha = 0.75, scale = 1.8, bandwidth = 30,
    rel_min_height = 0.005, color = "gray30", linewidth = 0.3
  ) +
  scale_fill_manual(values = country_pal, na.value = "gray60") +
  scale_x_continuous(breaks = seq(1500, 2000, 100), limits = c(1500, 2010)) +
  labs(title = "Birth year distribution by country",
       subtitle = "Kernel density estimates, one ridge per country, ordered by median birth year.",
       x = "Birth year", y = NULL) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(color = "gray40"),
        axis.text.y = element_text(face = "bold", size = 10),
        legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank())

ggsave(file.path(OUT_FIG, "dinastias_temporal_por_pais.png"), p_temp,
       width = 12, height = 8, dpi = 200)

# ═══════════════════════════════════════════════════════════════════════════════
# SUCESIÓN DINÁSTICA (mismo cargo en la misma familia)
# ═══════════════════════════════════════════════════════════════════════════════

sucesion_dinastica <- sucesiones %>%
  filter(!is.na(persona_relacionada_id)) %>%
  left_join(
    personas %>% select(persona_id, fam1 = familia_norm, nombre1 = nombre, pais1 = pais_base),
    by = "persona_id"
  ) %>%
  left_join(
    personas %>% select(persona_id, fam2 = familia_norm, nombre2 = nombre, pais2 = pais_base),
    by = c("persona_relacionada_id" = "persona_id")
  ) %>%
  filter(!is.na(fam1), !is.na(fam2))

sucesion_misma_fam <- sucesion_dinastica %>%
  filter(fam1 == fam2)

cat("\nSucesiones dentro de la misma familia:", nrow(sucesion_misma_fam), "\n")
cat("de un total de", nrow(sucesion_dinastica), "sucesiones identificadas\n")

if (nrow(sucesion_misma_fam) > 0) {
  resumen_sucesion <- sucesion_misma_fam %>%
    group_by(fam1) %>%
    summarise(
      n_sucesiones = n(),
      miembros = paste(unique(c(nombre1, nombre2)), collapse = " | "),
      paises = paste(sort(unique(c(pais1, pais2))), collapse = ", "),
      .groups = "drop"
    ) %>%
    arrange(desc(n_sucesiones))

  cat("\nFamilias con más sucesiones internas:\n")
  print(head(resumen_sucesion, 20))
  write_csv(resumen_sucesion, file.path(OUT_TABLE, "dinastias_sucesion_interna.csv"))

  p_suc <- resumen_sucesion %>%
    slice_head(n = 20) %>%
    mutate(fam1 = fct_reorder(fam1, n_sucesiones)) %>%
    ggplot(aes(n_sucesiones, fam1)) +
    geom_col(fill = "darkorange", alpha = 0.8) +
    geom_text(aes(label = n_sucesiones), hjust = -0.1, size = 3.5) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
    labs(title = "Dynastic succession — families with internal office succession",
         subtitle = "Predecessor → successor from the same family in an office",
         x = "Sucesiones internas", y = NULL) +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold"))

  ggsave(file.path(OUT_FIG, "dinastias_sucesion_interna.png"), p_suc,
         width = 9, height = 7, dpi = 150)
}

# Sucesiones transnacionales (distinta familia, distinto país)
sucesion_trans <- sucesion_dinastica %>%
  filter(!is.na(pais1), !is.na(pais2), pais1 != pais2) %>%
  select(nombre1, nombre2, fam1, fam2, pais1, pais2, rol)

cat("\nSucesiones transnacionales:", nrow(sucesion_trans), "\n")
if (nrow(sucesion_trans) > 0) {
  write_csv(sucesion_trans, file.path(OUT_TABLE, "dinastias_sucesion_transnacional.csv"))
}

message("Análisis de dinastías completado.")

