# ============================================================================
# tendencias_apellidos_cargos.R
# Tendencias temporales de cargos por apellido (simple tidyverse)
# ============================================================================

library(readr)
library(dplyr)
library(stringr)
library(ggplot2)

cat("📊 Cargando datos...\n")
positions_all <- read_csv("data/processed/positions_normalized.csv", show_col_types = FALSE)
derived_all <- read_csv("data/processed/derived_normalized.csv", show_col_types = FALSE)

cat("✅ Posiciones:", nrow(positions_all), "\n")
cat("✅ Personas:", nrow(derived_all), "\n")

# Parametros simples
top_n <- 12
min_decade <- 1800

# Extraer año de inicio y unir con apellido
positions_with_years <- positions_all %>%
  mutate(
    start_year = str_extract(start_date_raw, "\\b(1[789]\\d{2}|20[0-2]\\d)\\b") %>%
      as.integer()
  ) %>%
  left_join(
    derived_all %>% distinct(person_id, .keep_all = TRUE) %>%
      select(person_id, surname_1),
    by = "person_id"
  ) %>%
  filter(!is.na(start_year), !is.na(surname_1), surname_1 != "") %>%
  mutate(decade = floor(start_year / 10) * 10) %>%
  filter(decade >= min_decade)

# Apellidos mas importantes (por total de cargos)
top_surnames <- positions_with_years %>%
  count(surname_1, sort = TRUE) %>%
  slice_head(n = top_n) %>%
  pull(surname_1)

cat("✅ Top apellidos:", paste(top_surnames, collapse = ", "), "\n")

# Tendencia por decada y apellido
trend_by_surname <- positions_with_years %>%
  filter(surname_1 %in% top_surnames) %>%
  count(decade, surname_1) %>%
  arrange(decade, surname_1)

# Guardar tabla
dir.create("outputs/tables", showWarnings = FALSE, recursive = TRUE)
write_csv(trend_by_surname, "outputs/tables/tendencias_apellidos_cargos.csv")
cat("✅ Guardado: outputs/tables/tendencias_apellidos_cargos.csv\n")

# Grafico de tendencias
p_trend <- ggplot(trend_by_surname, aes(x = decade, y = n, color = surname_1)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  labs(
    title = "Tendencias de cargos por apellido (top)",
    subtitle = paste("Top", top_n, "apellidos por numero de cargos"),
    x = "Decada",
    y = "Numero de cargos",
    color = "Apellido"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE)
ggsave("outputs/figures/tendencias_apellidos_cargos.png", p_trend, width = 12, height = 7, dpi = 300)
cat("✅ Guardado: outputs/figures/tendencias_apellidos_cargos.png\n")

cat("\n✅ Tendencias generadas.\n")
