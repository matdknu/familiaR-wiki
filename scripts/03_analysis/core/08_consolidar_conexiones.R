#!/usr/bin/env Rscript
# ============================================================================
# 08_consolidar_conexiones.R
# Construye una tabla consolidada de TODAS las conexiones persona-a-persona:
#  - Relaciones matcheadas (ambas personas en dataset)
#  - Relaciones no matcheadas: limpia nombres, separa padre+madre, intenta
#    re-match por nombre parcial
#  - Output: una tabla donde cada fila es un vínculo con metadata completa
# ============================================================================

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

DATA_DIR  <- "data/processed/02_leer_data"
OUT_TABLE <- "outputs/tables"
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
# 1. LIMPIAR NOMBRES DE RELACIONES NO MATCHEADAS
# ═══════════════════════════════════════════════════════════════════════════════

clean_nombre <- function(x) {
  x <- str_remove_all(x, "\\s*\\[.*?\\]")
  x <- str_remove_all(x, "\\s*\\(.*?\\)")
  x <- str_remove_all(x, "Ver y modificar.*$")
  x <- str_remove_all(x, "\\s*\u200B")
  x <- URLdecode(x)
  trimws(x)
}

# Separate padre/madre entries that contain two people
split_padres <- function(nombre_raw) {
  nm <- clean_nombre(nombre_raw)
  if (is.na(nm) || nchar(nm) < 3) return(tibble(nombre_split = character()))

  if (grepl("^[0-9]+$", nm)) return(tibble(nombre_split = character()))

  # Pattern: "Nombre1 y Nombre2" with y as separator
  if (str_detect(nm, "\\b[yY]\\b") && !str_detect(nm, "^[A-Z][a-záéíóú]+ y [A-Z][a-záéíóú]+$")) {
    # Only split if "y" connects two capitalized segments
    parts <- str_split(nm, "\\s+[yY]\\s+", n = 2)[[1]]
    if (length(parts) == 2 && all(nchar(trimws(parts)) >= 3)) {
      return(tibble(nombre_split = trimws(parts)))
    }
  }

  tibble(nombre_split = nm)
}

cat("Procesando relaciones...\n")

# ═══════════════════════════════════════════════════════════════════════════════
# 2. TABLA CONSOLIDADA: MATCHED + UNMATCHED
# ═══════════════════════════════════════════════════════════════════════════════

# --- 2a. Matched relations ---
matched <- relaciones %>%
  filter(!is.na(persona_relacionada_id)) %>%
  left_join(
    personas %>% select(persona_id, nombre_p = nombre, familia_p = familia_norm,
                        pais_p = pais_efectivo, ocupacion_p = ocupacion),
    by = "persona_id"
  ) %>%
  left_join(
    personas %>% select(persona_id, nombre_r = nombre, familia_r = familia_norm,
                        pais_r = pais_efectivo, ocupacion_r = ocupacion),
    by = c("persona_relacionada_id" = "persona_id")
  ) %>%
  transmute(
    persona_id,
    nombre          = nombre_p,
    familia         = familia_p,
    pais            = pais_p,
    ocupacion       = ocupacion_p,
    tipo_relacion,
    relacionado_id  = persona_relacionada_id,
    relacionado_nombre = nombre_r,
    relacionado_familia = familia_r,
    relacionado_pais   = pais_r,
    relacionado_ocupacion = ocupacion_r,
    is_matched      = TRUE,
    nombre_raw      = nombre_relacionado
  )

cat("Matched relations:", nrow(matched), "\n")

# --- 2b. Unmatched relations: clean and split ---
unmatched_raw <- relaciones %>%
  filter(is.na(persona_relacionada_id),
         !is.na(nombre_relacionado),
         nchar(trimws(nombre_relacionado)) > 2,
         !grepl("^[0-9]+$", trimws(nombre_relacionado)))

# Split padre/madre entries
unmatched_split <- unmatched_raw %>%
  filter(tipo_relacion == "padre/madre") %>%
  rowwise() %>%
  mutate(splits = list(split_padres(nombre_relacionado))) %>%
  ungroup() %>%
  unnest(splits) %>%
  select(persona_id, tipo_relacion, fuente, nombre_split)

# Non padre/madre unmatched: just clean the name
unmatched_other <- unmatched_raw %>%
  filter(tipo_relacion != "padre/madre") %>%
  mutate(nombre_split = clean_nombre(nombre_relacionado)) %>%
  filter(nchar(nombre_split) > 2, !grepl("^[0-9]+$", nombre_split)) %>%
  select(persona_id, tipo_relacion, fuente, nombre_split)

unmatched_all <- bind_rows(unmatched_split, unmatched_other)

# Try to re-match by fuzzy name against personas
personas_lookup <- personas %>%
  select(persona_id, nombre, familia_norm, pais_efectivo, ocupacion) %>%
  mutate(nombre_lower = tolower(trimws(nombre)))

unmatched_all <- unmatched_all %>%
  mutate(nombre_lower = tolower(trimws(nombre_split)))

# Exact match first
re_matched <- unmatched_all %>%
  inner_join(
    personas_lookup %>% select(re_id = persona_id, nombre_lower,
                                re_nombre = nombre, re_familia = familia_norm,
                                re_pais = pais_efectivo, re_ocu = ocupacion),
    by = "nombre_lower"
  )

cat("Re-matched by exact name:", nrow(re_matched), "\n")

# Partial match: if unmatched name contains a persona's full name
still_unmatched <- unmatched_all %>%
  anti_join(re_matched, by = c("persona_id", "tipo_relacion", "nombre_split"))

# El texto libre (nombre_split) contiene el nombre completo de alguna persona
partial_matches <- still_unmatched %>%
  rowwise() %>%
  mutate(
    match_row = list(
      personas_lookup %>%
        filter(
          nchar(nombre) >= 10,
          str_detect(tolower(nombre_split), fixed(nombre_lower))
        ) %>%
        slice_max(nchar(nombre), n = 1, with_ties = FALSE) %>%
        transmute(
          re_id = persona_id,
          re_nombre = nombre,
          re_familia = familia_norm,
          re_pais = pais_efectivo,
          re_ocu = ocupacion
        )
    )
  ) %>%
  unnest(match_row) %>%
  ungroup()

cat("Re-matched by partial name:", nrow(partial_matches), "\n")

# Combine re-matched
all_re_matched <- bind_rows(
  re_matched %>% select(persona_id, tipo_relacion, nombre_split, re_id, re_nombre, re_familia, re_pais, re_ocu),
  partial_matches %>% select(persona_id, tipo_relacion, nombre_split, re_id, re_nombre, re_familia, re_pais, re_ocu)
)

# Final unmatched
final_unmatched <- unmatched_all %>%
  anti_join(all_re_matched, by = c("persona_id", "tipo_relacion", "nombre_split"))

cat("Still unmatched after re-matching:", nrow(final_unmatched), "\n")

# Build unmatched consolidated rows
unmatched_consolidated <- bind_rows(
  # Re-matched
  all_re_matched %>%
    left_join(personas %>% select(persona_id, nombre_p = nombre, familia_p = familia_norm,
                                   pais_p = pais_efectivo, ocupacion_p = ocupacion),
              by = "persona_id") %>%
    transmute(
      persona_id,
      nombre          = nombre_p,
      familia         = familia_p,
      pais            = pais_p,
      ocupacion       = ocupacion_p,
      tipo_relacion,
      relacionado_id  = re_id,
      relacionado_nombre = re_nombre,
      relacionado_familia = re_familia,
      relacionado_pais   = re_pais,
      relacionado_ocupacion = re_ocu,
      is_matched      = TRUE,
      nombre_raw      = nombre_split
    ),
  # Truly unmatched
  final_unmatched %>%
    left_join(personas %>% select(persona_id, nombre_p = nombre, familia_p = familia_norm,
                                   pais_p = pais_efectivo, ocupacion_p = ocupacion),
              by = "persona_id") %>%
    transmute(
      persona_id,
      nombre          = nombre_p,
      familia         = familia_p,
      pais            = pais_p,
      ocupacion       = ocupacion_p,
      tipo_relacion,
      relacionado_id  = NA_integer_,
      relacionado_nombre = nombre_split,
      relacionado_familia = NA_character_,
      relacionado_pais   = NA_character_,
      relacionado_ocupacion = NA_character_,
      is_matched      = FALSE,
      nombre_raw      = nombre_split
    )
)

# ═══════════════════════════════════════════════════════════════════════════════
# 3. CONSOLIDAR TODO
# ═══════════════════════════════════════════════════════════════════════════════

conexiones <- bind_rows(matched, unmatched_consolidated) %>%
  # Deduplicate
  mutate(
    dup_key = paste(persona_id, coalesce(as.character(relacionado_id), relacionado_nombre),
                    tipo_relacion, sep = "|")
  ) %>%
  distinct(dup_key, .keep_all = TRUE) %>%
  select(-dup_key) %>%
  # Classify
  mutate(
    misma_familia = (!is.na(familia) & !is.na(relacionado_familia) & familia == relacionado_familia),
    mismo_pais    = (!is.na(pais) & !is.na(relacionado_pais) & pais == relacionado_pais),
    tipo_label = case_when(
      tipo_relacion == "conyuge" ~ "Cónyuge",
      tipo_relacion == "pareja"  ~ "Pareja",
      tipo_relacion == "padre/madre" ~ "Padre/Madre",
      tipo_relacion == "hijo/a"  ~ "Hijo/a",
      TRUE ~ "Otro"
    ),
    cross_family = (!is.na(familia) & !is.na(relacionado_familia) & familia != relacionado_familia),
    cross_country = (!is.na(pais) & !is.na(relacionado_pais) & pais != relacionado_pais)
  ) %>%
  arrange(persona_id, tipo_relacion)

cat("\n═══════════════════════════════════════\n")
cat("TABLA CONSOLIDADA DE CONEXIONES\n")
cat("═══════════════════════════════════════\n")
cat("Total conexiones:", nrow(conexiones), "\n")
cat("  Matcheadas:", sum(conexiones$is_matched), "\n")
cat("  No matcheadas:", sum(!conexiones$is_matched), "\n")
cat("  Cross-family:", sum(conexiones$cross_family, na.rm = TRUE), "\n")
cat("  Cross-country:", sum(conexiones$cross_country, na.rm = TRUE), "\n\n")

cat("Por tipo de relación:\n")
print(conexiones %>% count(tipo_label, is_matched) %>% tidyr::pivot_wider(names_from = is_matched, values_from = n, values_fill = 0))

# ═══════════════════════════════════════════════════════════════════════════════
# 4. GUARDAR
# ═══════════════════════════════════════════════════════════════════════════════

write_csv(conexiones, file.path(OUT_TABLE, "conexiones_consolidadas.csv"))
write_rds(conexiones, file.path(OUT_TABLE, "conexiones_consolidadas.rds"))

cat("\nGuardado: conexiones_consolidadas.csv/rds\n")
cat("  Filas:", nrow(conexiones), "| Columnas:", ncol(conexiones), "\n")

# ═══════════════════════════════════════════════════════════════════════════════
# 5. RESUMEN POR PERSONA: cuántas conexiones tiene cada uno
# ═══════════════════════════════════════════════════════════════════════════════

resumen_persona <- conexiones %>%
  group_by(persona_id, nombre, familia, pais) %>%
  summarise(
    n_conexiones     = n(),
    n_matched        = sum(is_matched),
    n_unmatched      = sum(!is_matched),
    n_conyuges       = sum(tipo_label == "Cónyuge"),
    n_parejas        = sum(tipo_label == "Pareja"),
    n_padres         = sum(tipo_label == "Padre/Madre"),
    n_hijos          = sum(tipo_label == "Hijo/a"),
    n_cross_family   = sum(cross_family, na.rm = TRUE),
    n_cross_country  = sum(cross_country, na.rm = TRUE),
    familias_conectadas = paste(sort(unique(na.omit(relacionado_familia[cross_family]))), collapse = ", "),
    paises_conectados  = paste(sort(unique(na.omit(relacionado_pais[cross_country]))), collapse = ", "),
    .groups = "drop"
  ) %>%
  arrange(desc(n_conexiones))

write_csv(resumen_persona, file.path(OUT_TABLE, "resumen_conexiones_por_persona.csv"))

cat("\n═══════════════════════════════════════\n")
cat("TOP 30 PERSONAS MÁS CONECTADAS\n")
cat("═══════════════════════════════════════\n")
print(
  resumen_persona %>%
    head(30) %>%
    select(nombre, familia, pais, n_conexiones, n_matched, n_conyuges,
           n_parejas, n_hijos, n_cross_family, n_cross_country) %>%
    as.data.frame()
)

# ═══════════════════════════════════════════════════════════════════════════════
# 6. TOP CROSS-FAMILY CONNECTORS (brokers entre familias)
# ═══════════════════════════════════════════════════════════════════════════════

brokers <- resumen_persona %>%
  filter(n_cross_family >= 2) %>%
  arrange(desc(n_cross_family))

cat("\n═══════════════════════════════════════\n")
cat("TOP BROKERS INTER-FAMILIARES (≥2 conexiones cross-family)\n")
cat("═══════════════════════════════════════\n")
print(
  brokers %>%
    head(30) %>%
    select(nombre, familia, pais, n_conexiones, n_cross_family,
           n_cross_country, familias_conectadas) %>%
    as.data.frame()
)

write_csv(brokers, file.path(OUT_TABLE, "brokers_interfamiliares.csv"))

# ═══════════════════════════════════════════════════════════════════════════════
# 7. CROSS-COUNTRY CONNECTORS
# ═══════════════════════════════════════════════════════════════════════════════

cross_country_people <- resumen_persona %>%
  filter(n_cross_country >= 1) %>%
  arrange(desc(n_cross_country))

cat("\n═══════════════════════════════════════\n")
cat("TOP CONECTORES TRANSNACIONALES\n")
cat("═══════════════════════════════════════\n")
print(
  cross_country_people %>%
    head(30) %>%
    select(nombre, familia, pais, n_conexiones, n_cross_country,
           paises_conectados) %>%
    as.data.frame()
)

write_csv(cross_country_people, file.path(OUT_TABLE, "conectores_transnacionales.csv"))

# ══════════════════════════════════════════════════════════════════════════════
# 8. EJEMPLO DE CADENA: mostrar la ficha completa de celebridades
# ══════════════════════════════════════════════════════════════════════════════

celebs <- c("Carlos Menem", "Cecilia Bolocco", "Benjamín Vicuña",
            "China Suárez", "Paz Bascuñán", "Pampita",
            "Patricio Aylwin", "Nicolás Cabré")

cat("\n═══════════════════════════════════════\n")
cat("FICHAS DE CONEXIÓN: CELEBRIDADES\n")
cat("═══════════════════════════════════════\n")

for (c in celebs) {
  ficha <- conexiones %>% filter(nombre == c)
  if (nrow(ficha) == 0) next
  cat("\n┌─────────────────────────────────────\n")
  cat("│", c, " [", ficha$familia[1], "|", cap(ficha$pais[1]), "]\n")
  cat("├─────────────────────────────────────\n")
  for (i in seq_len(nrow(ficha))) {
    r <- ficha[i, ]
    status <- ifelse(r$is_matched, "✓", "○")
    fam_tag <- ifelse(!is.na(r$relacionado_familia), paste0("[", r$relacionado_familia, "]"), "[?]")
    pais_tag <- ifelse(!is.na(r$relacionado_pais), cap(r$relacionado_pais), "?")
    cross <- ""
    if (!is.na(r$cross_family) && r$cross_family) cross <- paste0(cross, " ★CROSS-FAM")
    if (!is.na(r$cross_country) && r$cross_country) cross <- paste0(cross, " ★CROSS-PAIS")
    cat("│ ", status, r$tipo_label, "→", r$relacionado_nombre, fam_tag, pais_tag, cross, "\n")
  }
  cat("└─────────────────────────────────────\n")
}

# ══════════════════════════════════════════════════════════════════════════════
# 9. RESUMEN POR PAÍS: conexiones y top conectores por país
# ══════════════════════════════════════════════════════════════════════════════

resumen_pais <- conexiones %>%
  mutate(pais = coalesce(pais, "desconocido")) %>%
  group_by(pais) %>%
  summarise(
    conexiones = n(),
    matched = sum(is_matched),
    unmatched = sum(!is_matched),
    cross_country = sum(cross_country, na.rm = TRUE),
    cross_family = sum(cross_family, na.rm = TRUE),
    personas_distintas = n_distinct(persona_id),
    familias_distintas = n_distinct(familia, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(conexiones))

write_csv(resumen_pais, file.path(OUT_TABLE, "conexiones_por_pais.csv"))

top_conectores_pais <- resumen_persona %>%
  filter(!is.na(pais)) %>%
  group_by(pais) %>%
  slice_max(n_conexiones, n = 15, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(pais, desc(n_conexiones))

write_csv(top_conectores_pais, file.path(OUT_TABLE, "top_conectores_por_pais.csv"))

cat("\nResumen por país guardado en conexiones_por_pais.csv\n")
cat("Top conectores por país guardado en top_conectores_por_pais.csv\n")

message("\nConsolidación de conexiones completada.")

