# ============================================================================
# 02_descriptive_analysis.R
# Análisis descriptivo de los datos normalizados
# ============================================================================
# Input: Tablas normalizadas de 01_parse_and_normalize.R
# - persons_all
# - positions_all
# - family_relations_all
# - education_all
# - affiliations_all
# ============================================================================

library(dplyr)
library(stringr)
library(tidyr)
library(purrr)

# ============================================================================
# 1. RESUMEN GENERAL
# ============================================================================

summary_stats <- list(
  n_persons = nrow(persons_all),
  n_positions = nrow(positions_all),
  n_family_relations = nrow(family_relations_all),
  n_education = nrow(education_all),
  n_affiliations = nrow(affiliations_all),
  
  persons_with_positions = n_distinct(positions_all$person_id),
  persons_with_family = n_distinct(family_relations_all$person_id),
  persons_with_education = n_distinct(education_all$person_id),
  persons_with_affiliations = n_distinct(affiliations_all$person_id)
)

cat("=== RESUMEN GENERAL ===\n")
cat("Total personas:", summary_stats$n_persons, "\n")
cat("Total posiciones:", summary_stats$n_positions, "\n")
cat("Total relaciones familiares:", summary_stats$n_family_relations, "\n")
cat("Total educación:", summary_stats$n_education, "\n")
cat("Total afiliaciones:", summary_stats$n_affiliations, "\n\n")

# ============================================================================
# 2. ANÁLISIS DE PERSONAS
# ============================================================================

# Nacionalidades
nationality_summary <- persons_all %>%
  filter(!is.na(nationality_raw) & nationality_raw != "") %>%
  count(nationality_raw, sort = TRUE) %>%
  mutate(pct = round(100 * n / sum(n), 1))

# Religiones
religion_summary <- persons_all %>%
  filter(!is.na(religion_raw) & religion_raw != "") %>%
  count(religion_raw, sort = TRUE) %>%
  mutate(pct = round(100 * n / sum(n), 1))

# Ocupaciones
occupation_summary <- persons_all %>%
  filter(!is.na(other_notes_raw) & other_notes_raw != "") %>%
  count(other_notes_raw, sort = TRUE) %>%
  head(20)

cat("=== NACIONALIDADES (Top 10) ===\n")
print(nationality_summary %>% head(10))
cat("\n")

cat("=== RELIGIONES (Top 10) ===\n")
print(religion_summary %>% head(10))
cat("\n")

# ============================================================================
# 3. ANÁLISIS DE POSICIONES POLÍTICAS
# ============================================================================

# Extraer años
positions_with_years <- positions_all %>%
  mutate(
    start_year = str_extract(start_date_raw, "\\b(1[789]\\d{2}|20[0-2]\\d)\\b") %>% as.integer(),
    end_year = str_extract(end_date_raw, "\\b(1[789]\\d{2}|20[0-2]\\d)\\b") %>% as.integer()
  )

# Tipos de cargo más comunes
position_types <- positions_with_years %>%
  mutate(
    position_type = case_when(
      str_detect(tolower(position_title_raw), "presidente") ~ "Presidente",
      str_detect(tolower(position_title_raw), "senador") ~ "Senador",
      str_detect(tolower(position_title_raw), "diputado") ~ "Diputado",
      str_detect(tolower(position_title_raw), "ministro") ~ "Ministro",
      str_detect(tolower(position_title_raw), "alcalde") ~ "Alcalde",
      str_detect(tolower(position_title_raw), "intendente") ~ "Intendente",
      str_detect(tolower(position_title_raw), "gobernador") ~ "Gobernador",
      str_detect(tolower(position_title_raw), "embajador") ~ "Embajador",
      TRUE ~ "Otro"
    )
  )

position_type_summary <- position_types %>%
  count(position_type, sort = TRUE) %>%
  mutate(pct = round(100 * n / sum(n), 1))

# Distribución temporal
temporal_distribution <- positions_with_years %>%
  filter(!is.na(start_year)) %>%
  mutate(
    decade = floor(start_year / 10) * 10,
    century = floor(start_year / 100) + 1
  ) %>%
  count(decade, sort = TRUE)

# Personas con más cargos
top_office_holders <- positions_all %>%
  count(person_id, sort = TRUE) %>%
  left_join(
    persons_all %>% dplyr::select(person_id, canonical_name),
    by = "person_id"
  ) %>%
  head(20)

cat("=== TIPOS DE CARGO (Top 15) ===\n")
print(position_type_summary %>% head(15))
cat("\n")

cat("=== DISTRIBUCIÓN TEMPORAL (por década) ===\n")
print(temporal_distribution %>% head(20))
cat("\n")

cat("=== PERSONAS CON MÁS CARGOS (Top 20) ===\n")
print(top_office_holders)
cat("\n")

# ============================================================================
# 4. ANÁLISIS DE RELACIONES FAMILIARES
# ============================================================================

# Tipos de relación
relation_type_summary <- family_relations_all %>%
  count(relation_type, sort = TRUE) %>%
  mutate(pct = round(100 * n / sum(n), 1))

# Personas más conectadas (por número de relaciones)
most_connected <- family_relations_all %>%
  count(person_id, sort = TRUE) %>%
  left_join(
    persons_all %>% dplyr::select(person_id, canonical_name),
    by = "person_id"
  ) %>%
  head(20)

cat("=== TIPOS DE RELACIÓN FAMILIAR ===\n")
print(relation_type_summary)
cat("\n")

cat("=== PERSONAS MÁS CONECTADAS (Top 20) ===\n")
print(most_connected)
cat("\n")

# ============================================================================
# 5. ANÁLISIS DE EDUCACIÓN
# ============================================================================

# Instituciones más frecuentes
top_institutions <- education_all %>%
  filter(!is.na(institution_raw) & institution_raw != "") %>%
  count(institution_raw, sort = TRUE) %>%
  head(20)

cat("=== INSTITUCIONES MÁS FRECUENTES (Top 20) ===\n")
print(top_institutions)
cat("\n")

# ============================================================================
# 6. ANÁLISIS DE AFILIACIONES
# ============================================================================

# Organizaciones más frecuentes
top_organizations <- affiliations_all %>%
  filter(!is.na(affiliation_raw) & affiliation_raw != "") %>%
  count(affiliation_raw, sort = TRUE) %>%
  head(20)

cat("=== ORGANIZACIONES MÁS FRECUENTES (Top 20) ===\n")
print(top_organizations)
cat("\n")

# ============================================================================
# 7. RESUMEN DE CARRERAS POLÍTICAS
# ============================================================================

career_lengths <- positions_with_years %>%
  filter(!is.na(start_year) & !is.na(end_year)) %>%
  mutate(career_length = end_year - start_year) %>%
  group_by(person_id) %>%
  summarise(
    n_positions = n(),
    earliest_year = min(start_year, na.rm = TRUE),
    latest_year = max(end_year, na.rm = TRUE),
    total_years = latest_year - earliest_year,
    avg_position_length = mean(career_length, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    persons_all %>% dplyr::select(person_id, canonical_name),
    by = "person_id"
  ) %>%
  arrange(desc(total_years))

cat("=== CARRERAS POLÍTICAS MÁS LARGAS (Top 20) ===\n")
print(career_lengths %>% head(20))
cat("\n")

# ============================================================================
# 8. PATRONES TEMPORALES
# ============================================================================

# Posiciones por siglo
positions_by_century <- positions_with_years %>%
  filter(!is.na(start_year)) %>%
  mutate(century = floor(start_year / 100) + 1) %>%
  count(century, sort = TRUE)

cat("=== POSICIONES POR SIGLO ===\n")
print(positions_by_century)
cat("\n")

# ============================================================================
# Objetos creados para uso posterior:
# - summary_stats: estadísticas generales
# - nationality_summary, religion_summary: demografía
# - position_type_summary: tipos de cargo
# - temporal_distribution: distribución temporal
# - top_office_holders: personas con más cargos
# - relation_type_summary: tipos de relación
# - most_connected: personas más conectadas
# - top_institutions, field_summary: educación
# - top_organizations: afiliaciones
# - career_lengths: longitudes de carrera
# - positions_by_century: distribución por siglo
# ============================================================================
