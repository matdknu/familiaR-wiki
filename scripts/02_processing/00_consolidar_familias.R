# ============================================================================
# 00_consolidar_familias.R
# 1) Primero actualiza el consolidado de Chile (incorpora familia Allende desde raw).
# 2) Luego une todos los consolidados por país en un único archivo LATAM.
# SALIDA: data/processed/familias/chile/consolidado.csv (actualizado)
#         data/processed/familias/_CONSOLIDADO_familias_latam.csv
# ============================================================================

library(readr)
library(dplyr)
library(purrr)

# Rutas (asumir ejecución desde la raíz del proyecto)
BASE_DIR <- "data/processed/familias"
CHILE_CONSOLIDADO <- file.path(BASE_DIR, "chile", "consolidado.csv")
OUTPUT_FILE <- file.path(BASE_DIR, "_CONSOLIDADO_familias_latam.csv")
ALLENDE_RAW <- "data/raw/chile/familias/familia_allende_completo.csv"

# ----------------------------------------------------------------------------
# Paso 1: Consolidar Chile (incorporar familia Allende en chile/consolidado.csv)
# ----------------------------------------------------------------------------
if (file.exists(ALLENDE_RAW) && file.exists(CHILE_CONSOLIDADO)) {
  message("Paso 1: Consolidando Chile (incorporando familia Allende)...")
  chile <- read_delim(CHILE_CONSOLIDADO, delim = ";", show_col_types = FALSE)
  allende <- read_delim(ALLENDE_RAW, delim = ";", show_col_types = FALSE)
  if (!"pais_origen" %in% names(allende)) allende$pais_origen <- "chile"
  cols <- intersect(names(chile), names(allende))
  allende <- allende %>% select(all_of(cols))
  chile <- chile %>% select(all_of(cols))
  chile <- bind_rows(chile, allende)
  if ("url" %in% names(chile)) {
    chile <- chile %>% distinct(url, .keep_all = TRUE)
  }
  dir.create(dirname(CHILE_CONSOLIDADO), showWarnings = FALSE, recursive = TRUE)
  write_delim(chile, CHILE_CONSOLIDADO, delim = ";")
  message("  OK: Chile guardado en ", CHILE_CONSOLIDADO, " (", nrow(chile), " filas)")
} else if (file.exists(ALLENDE_RAW) && !file.exists(CHILE_CONSOLIDADO)) {
  message("Paso 1: Creando consolidado de Chile solo con familia Allende...")
  chile <- read_delim(ALLENDE_RAW, delim = ";", show_col_types = FALSE)
  if (!"pais_origen" %in% names(chile)) chile$pais_origen <- "chile"
  dir.create(dirname(CHILE_CONSOLIDADO), showWarnings = FALSE, recursive = TRUE)
  write_delim(chile, CHILE_CONSOLIDADO, delim = ";")
  message("  OK: Chile guardado en ", CHILE_CONSOLIDADO, " (", nrow(chile), " filas)")
}

# ----------------------------------------------------------------------------
# Paso 2: Unir todos los consolidados por país → LATAM
# ----------------------------------------------------------------------------
country_dirs <- list.dirs(BASE_DIR, recursive = FALSE)
consolidado_paths <- file.path(country_dirs, "consolidado.csv")
existing <- consolidado_paths[file.exists(consolidado_paths)]

if (length(existing) == 0) {
  stop("No se encontró ningún consolidado en ", BASE_DIR, "/<pais>/consolidado.csv")
}

message("Paso 2: Leyendo ", length(existing), " consolidados por país...")

read_one <- function(path) {
  pais <- basename(dirname(path))
  df <- read_delim(path, delim = ";", show_col_types = FALSE)
  if (!"pais" %in% names(df)) {
    df$pais <- pais
  } else {
    df$pais <- coalesce(df$pais, pais)
  }
  df
}

listas <- map(existing, read_one)
latam <- bind_rows(listas)

# Guardar
dir.create(BASE_DIR, showWarnings = FALSE, recursive = TRUE)
write_delim(latam, OUTPUT_FILE, delim = ";")

message("OK: ", nrow(latam), " filas guardadas en ", OUTPUT_FILE)
message("Países: ", paste(unique(latam$pais), collapse = ", "))

