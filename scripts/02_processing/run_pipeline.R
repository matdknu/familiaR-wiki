# ============================================================================
# run_pipeline.R
# Ejecuta el pipeline completo: parse + normalize + analyze
# ============================================================================

library(readr)
library(jsonlite)
library(purrr)
library(dplyr)
suppressPackageStartupMessages({
  source("scripts/02_processing/01_parse_and_normalize.R")
})

# Leer todas las familias
files <- list.files("data/raw/chile/familias", pattern = "_completo.csv$", full.names = TRUE)
cat("=== Procesando", length(files), "familias ===\n\n")

all_persons <- tibble()
all_positions <- tibble()
all_family <- tibble()
all_education <- tibble()
all_affiliations <- tibble()
all_links <- tibble()
all_derived <- tibble()

for (f in files) {
  cat("Procesando:", basename(f), "\n")
  df <- read_delim(f, delim = ";", show_col_types = FALSE)
  
  if (!"infobox_json" %in% names(df)) {
    cat("  Saltando (sin infobox_json)\n")
    next
  }
  
  infobox_json <- map(seq_len(nrow(df)), function(i) {
    if (!is.na(df$infobox_json[i]) && df$infobox_json[i] != "") {
      fromJSON(df$infobox_json[i])
    } else {
      NULL
    }
  })
  
  parsed <- parse_all_infoboxes(infobox_json)
  
  all_persons <- bind_rows(all_persons, parsed$persons_all)
  all_positions <- bind_rows(all_positions, parsed$positions_all)
  all_family <- bind_rows(all_family, parsed$family_relations_all)
  all_education <- bind_rows(all_education, parsed$education_all)
  all_affiliations <- bind_rows(all_affiliations, parsed$affiliations_all)
  all_links <- bind_rows(all_links, parsed$links_all)
  all_derived <- bind_rows(all_derived, parsed$derived_all)
  
  cat("  OK:", nrow(parsed$persons_all), "personas\n")
}

cat("\n=== CONSOLIDADO ===\n")
cat("Personas:", nrow(all_persons), "\n")
cat("Posiciones:", nrow(all_positions), "\n")
cat("Relaciones:", nrow(all_family), "\n")
cat("Educación:", nrow(all_education), "\n")
cat("Afiliaciones:", nrow(all_affiliations), "\n")

# Guardar
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
write_csv(all_persons, "data/processed/persons_normalized.csv")
write_csv(all_positions, "data/processed/positions_normalized.csv")
write_csv(all_family, "data/processed/family_relations_normalized.csv")
write_csv(all_education, "data/processed/education_normalized.csv")
write_csv(all_affiliations, "data/processed/affiliations_normalized.csv")
write_csv(all_links, "data/processed/links_normalized.csv")
write_csv(all_derived, "data/processed/derived_normalized.csv")

cat("\n✅ Guardado en data/processed/\n\n")

# Ahora analizar
cat("\n=== EJECUTANDO ANÁLISIS DESCRIPTIVO ===\n")
persons_all <- all_persons
positions_all <- all_positions
family_relations_all <- all_family
education_all <- all_education
affiliations_all <- all_affiliations
links_all <- all_links
derived_all <- all_derived

suppressPackageStartupMessages({
  source("scripts/02_processing/02_descriptive_analysis.R")
})

cat("\n=== EJECUTANDO MODELADO AVANZADO ===\n")
suppressPackageStartupMessages({
  source("scripts/02_processing/02_model_and_analyze.R")
})

cat("\n=== RED FAMILIAR ===\n")
cat("Nodos:", vcount(g_family_igraph), "\n")
cat("Aristas:", ecount(g_family_igraph), "\n\n")

cat("=== TOP 10 PERSONAS CENTRALES ===\n")
if (nrow(person_metrics) > 0 && "degree" %in% names(person_metrics)) {
  print(person_metrics %>% arrange(desc(degree)) %>% head(10) %>% dplyr::select(person_id, canonical_name, degree, betweenness))
} else {
  cat("No hay métricas de centralidad disponibles.\n")
}

cat("\n=== RESUMEN CARRERAS (primeras 20) ===\n")
print(positions_summary %>% arrange(desc(positions_count)) %>% head(20))

cat("\n=== MÉTRICAS POR APELLIDO (Top 10) ===\n")
print(surname_metrics %>% arrange(desc(dynasty_score)) %>% head(10))

cat("\n=== GENERANDO VISUALIZACIONES ===\n")
suppressPackageStartupMessages({
  source("scripts/02_processing/03_visualizations.R")
})

cat("\n✅ Pipeline completado.\n")

