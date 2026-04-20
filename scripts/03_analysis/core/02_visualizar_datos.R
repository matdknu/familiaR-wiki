#!/usr/bin/env Rscript
# ============================================================================
# 02_visualizar_datos.R
# Visualización descriptiva de las tablas normalizadas (02_leer_data).
# Lee: data/processed/02_leer_data/*.rds
# Escribe: outputs/figures/descriptivo_*.png
# ============================================================================

library(readr)
library(dplyr)
library(ggplot2)
library(forcats)
library(stringr)
library(tidyr)
library(patchwork)

DATA_DIR <- "data/processed/02_leer_data"
OUT_DIR  <- "outputs/figures"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

personas    <- read_rds(file.path(DATA_DIR, "personas.rds"))
ocupaciones <- read_rds(file.path(DATA_DIR, "ocupaciones.rds"))
relaciones  <- read_rds(file.path(DATA_DIR, "relaciones.rds"))
partidos    <- read_rds(file.path(DATA_DIR, "partidos.rds"))
educacion   <- read_rds(file.path(DATA_DIR, "educacion.rds"))

theme_elite <- theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "gray40", size = 10),
    panel.grid.minor = element_blank()
  )

# ─── 1. Personas por país ────────────────────────────────────────────────────
p1 <- personas %>%
  filter(!is.na(pais_base)) %>%
  count(pais_base, sort = TRUE) %>%
  mutate(pais_base = fct_reorder(pais_base, n)) %>%
  ggplot(aes(n, pais_base, fill = pais_base)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = scales::comma(n)), hjust = -0.1, size = 3.5) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Personas por país", x = "Cantidad", y = NULL) +
  theme_elite

p1

ggsave(file.path(OUT_DIR, "descriptivo_personas_por_pais.png"), p1,
       width = 8, height = 5, dpi = 150)

# ─── 2. Distribución temporal de nacimientos ─────────────────────────────────
p2 <- personas %>%
  filter(!is.na(anio_nacimiento), anio_nacimiento >= 1500, anio_nacimiento <= 2010) %>%
  mutate(decada = (anio_nacimiento %/% 50) * 50) %>%
  count(decada) %>%
  ggplot(aes(decada, n)) +
  geom_col(fill = "steelblue", alpha = 0.85) +
  labs(title = "Distribución temporal de nacimientos",
       subtitle = "Agrupados en medios siglos",
       x = "Medio siglo", y = "Personas") +
  theme_elite

p2

ggsave(file.path(OUT_DIR, "descriptivo_temporal_nacimientos.png"), p2,
       width = 9, height = 5, dpi = 150)

# ─── 3. Familias más grandes ─────────────────────────────────────────────────
p3 <- personas %>%
  filter(!is.na(familia_norm)) %>%
  count(familia_norm, sort = TRUE) %>%
  slice_head(n = 25) %>%
  mutate(familia_norm = fct_reorder(familia_norm, n)) %>%
  ggplot(aes(n, familia_norm)) +
  geom_col(fill = "coral", alpha = 0.85) +
  geom_text(aes(label = n), hjust = -0.1, size = 3) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Top 25 familias por número de miembros", x = "Miembros", y = NULL) +
  theme_elite

p3

ggsave(file.path(OUT_DIR, "descriptivo_top_familias.png"), p3,
       width = 8, height = 7, dpi = 150)

# ─── 4. Ocupaciones más frecuentes ───────────────────────────────────────────
p4 <- ocupaciones %>%
  count(ocupacion, sort = TRUE) %>%
  slice_head(n = 20) %>%
  mutate(ocupacion = fct_reorder(ocupacion, n)) %>%
  ggplot(aes(n, ocupacion)) +
  geom_col(fill = "mediumpurple", alpha = 0.85) +
  geom_text(aes(label = n), hjust = -0.1, size = 3) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Top 20 ocupaciones", x = "Personas", y = NULL) +
  theme_elite

p4

ggsave(file.path(OUT_DIR, "descriptivo_top_ocupaciones.png"), p4,
       width = 8, height = 6, dpi = 150)

# ─── 5. Relaciones por tipo ──────────────────────────────────────────────────
p5 <- relaciones %>%
  count(tipo_relacion, sort = TRUE) %>%
  mutate(tipo_relacion = fct_reorder(tipo_relacion, n)) %>%
  ggplot(aes(n, tipo_relacion)) +
  geom_col(fill = "darkgreen", alpha = 0.75) +
  geom_text(aes(label = scales::comma(n)), hjust = -0.1, size = 3.5) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Relaciones por tipo", x = "Cantidad", y = NULL) +
  theme_elite

p5

ggsave(file.path(OUT_DIR, "descriptivo_relaciones_tipo.png"), p5,
       width = 7, height = 4, dpi = 150)

# ─── 6. Top partidos políticos ────────────────────────────────────────────────
p6 <- partidos %>%
  count(partido, sort = TRUE) %>%
  slice_head(n = 20) %>%
  mutate(partido = fct_reorder(partido, n)) %>%
  ggplot(aes(n, partido)) +
  geom_col(fill = "tomato", alpha = 0.8) +
  geom_text(aes(label = n), hjust = -0.1, size = 3) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Top 20 partidos políticos", x = "Personas", y = NULL) +
  theme_elite

p6

ggsave(file.path(OUT_DIR, "descriptivo_top_partidos.png"), p6,
       width = 9, height = 6, dpi = 150)

# ─── 7. Top instituciones educativas ─────────────────────────────────────────
p7 <- educacion %>%
  count(institucion, sort = TRUE) %>%
  slice_head(n = 20) %>%
  mutate(institucion = fct_reorder(institucion, n)) %>%
  ggplot(aes(n, institucion)) +
  geom_col(fill = "dodgerblue", alpha = 0.8) +
  geom_text(aes(label = n), hjust = -0.1, size = 3) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Top 20 instituciones educativas", x = "Personas", y = NULL) +
  theme_elite

p7

ggsave(file.path(OUT_DIR, "descriptivo_top_educacion.png"), p7,
       width = 9, height = 6, dpi = 150)

# ─── 8. Panel resumen (4 gráficos en uno) ────────────────────────────────────
panel <- (p1 + p2) / (p3 + p4) +
  plot_annotation(
    title = "Élites Latinoamericanas — Resumen descriptivo",
    subtitle = paste0(nrow(personas), " personas | ",
                      length(unique(personas$pais_base)), " países | ",
                      length(unique(personas$familia_norm)), " familias"),
    theme = theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(color = "gray40", size = 12, hjust = 0.5)
    )
  )

ggsave(file.path(OUT_DIR, "descriptivo_panel_resumen.png"), panel,
       width = 16, height = 14, dpi = 150)

message("Descriptivos guardados en ", OUT_DIR)
