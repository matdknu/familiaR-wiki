# ============================================================================
# Script para enriquecer datos desde infobox_completa
# 
# Este script toma los CSVs scrapeados y extrae información estructurada
# del campo infobox_completa, creando tablas de:
# - Cargos políticos (cargo, período, predecesor, sucesor)
# - Información personal expandida
# - Relaciones familiares
#
# Uso:
#   source("enrich_infobox.R")
#   resultado <- enrich_familia("familia_aylwin_completo.csv")
# ============================================================================

library(tidyverse)
library(jsonlite)

# Ruta base de datos
DATA_DIR <- here::here("data", "raw", "chile", "familias")
OUTPUT_DIR <- here::here("data", "processed")

# ============================================================================
# Funciones de parsing
# ============================================================================

#' Parsea el campo infobox_completa separado por " | "
#' @param infobox_text String con formato "label: value | label: value | ..."
#' @return tibble con columnas label y value
parse_infobox_completa <- function(infobox_text) {
  if (is.na(infobox_text) || infobox_text == "") {
    return(tibble(label = character(), value = character()))
  }
  
  # Separar por " | "
  entries <- str_split(infobox_text, " [|] ")[[1]]
  
  # Parsear cada entrada "label: value"
  parsed <- map_dfr(entries, function(entry) {
    # Separar solo en el primer ":"
    parts <- str_split_fixed(entry, ": ", 2)
    if (ncol(parts) == 2 && parts[1, 1] != "") {
      tibble(
        label = str_trim(parts[1, 1]),
        value = str_trim(parts[1, 2])
      )
    } else {
      tibble(label = str_trim(entry), value = "")
    }
  })
  
  return(parsed)
}

#' Extrae cargos políticos del infobox parseado
#' @param parsed_infobox tibble con label y value
#' @param persona_url URL de la persona para identificar
#' @return tibble con cargos estructurados
extract_cargos <- function(parsed_infobox, persona_url = NA) {
  if (nrow(parsed_infobox) == 0) {
    return(tibble())
  }
  

  # Patrones que indican un cargo
  cargo_patterns <- c(
    "Presidente", "Senador", "Diputado", "Ministro", "Alcalde",
    "Intendente", "Gobernador", "Embajador", "Subsecretario",
    "Concejal", "Consejero", "Director", "Rector", "Decano"
  )
  
  cargos <- tibble()
  cargo_actual <- NULL
  periodo_actual <- NULL
  predecesor_actual <- NULL
  sucesor_actual <- NULL
  
  for (i in seq_len(nrow(parsed_infobox))) {
    label <- parsed_infobox$label[i]
    value <- parsed_infobox$value[i]
    
    # ¿Es un cargo nuevo?
    is_cargo <- any(str_detect(label, regex(cargo_patterns, ignore_case = TRUE)))
    
    if (is_cargo && value == "") {
      # Guardar cargo anterior si existe
      if (!is.null(cargo_actual)) {
        cargos <- bind_rows(cargos, tibble(
          persona_url = persona_url,
          cargo = cargo_actual,
          periodo = periodo_actual %||% NA_character_,
          predecesor = predecesor_actual %||% NA_character_,
          sucesor = sucesor_actual %||% NA_character_
        ))
      }
      # Iniciar nuevo cargo
      cargo_actual <- label
      periodo_actual <- NULL
      predecesor_actual <- NULL
      sucesor_actual <- NULL
    } else if (!is.null(cargo_actual)) {
      # Llenar detalles del cargo actual
      if (label == "detalle" && is.null(periodo_actual)) {
        periodo_actual <- value
      } else if (str_detect(label, regex("predecesor", ignore_case = TRUE))) {
        predecesor_actual <- value
      } else if (str_detect(label, regex("sucesor", ignore_case = TRUE))) {
        sucesor_actual <- value
      }
    }
  }
  
  # Guardar último cargo
 if (!is.null(cargo_actual)) {
    cargos <- bind_rows(cargos, tibble(
      persona_url = persona_url,
      cargo = cargo_actual,
      periodo = periodo_actual %||% NA_character_,
      predecesor = predecesor_actual %||% NA_character_,
      sucesor = sucesor_actual %||% NA_character_
    ))
  }
  
  return(cargos)
}

#' Extrae URLs de un texto con formato "Nombre (URL)"
#' @param text String con formato "Nombre (URL); Nombre2 (URL2)"
#' @return tibble con nombre y url
extract_links <- function(text) {
  if (is.na(text) || text == "") {
    return(tibble(nombre = character(), url = character()))
  }
  
  # Buscar patrón "Nombre (URL)"
  matches <- str_match_all(text, "([^;]+?)\\s*\\((https?://[^)]+)\\)")[[1]]
  
  if (nrow(matches) > 0) {
    tibble(
      nombre = str_trim(matches[, 2]),
      url = matches[, 3]
    )
  } else {
    tibble(nombre = text, url = NA_character_)
  }
}

# ============================================================================
# Función principal de enriquecimiento
# ============================================================================

#' Enriquece los datos de una familia
#' @param filename Nombre del archivo CSV en DATA_DIR
#' @return Lista con tibbles: personas, cargos, relaciones
enrich_familia <- function(filename) {
  filepath <- file.path(DATA_DIR, filename)
  
  if (!file.exists(filepath)) {
    stop(paste("Archivo no encontrado:", filepath))
  }
  
  message("Leyendo: ", filename)
  
  # Leer CSV
  df <- read_delim(filepath, delim = ";", show_col_types = FALSE)
  
  message("  - ", nrow(df), " personas encontradas")
  
  # Verificar que existe infobox_completa
  if (!"infobox_completa" %in% names(df)) {
    warning("  ! El archivo no tiene columna 'infobox_completa'")
    return(list(personas = df, cargos = tibble(), relaciones = tibble()))
  }
  
  # Extraer cargos de cada persona
  message("  - Extrayendo cargos...")
  cargos <- map_dfr(seq_len(nrow(df)), function(i) {
    parsed <- parse_infobox_completa(df$infobox_completa[i])
    extract_cargos(parsed, persona_url = df$url[i])
  })
  
  message("  - ", nrow(cargos), " cargos extraídos")
  
  # Extraer relaciones expandidas
  message("  - Extrayendo relaciones...")
  relaciones <- map_dfr(seq_len(nrow(df)), function(i) {
    persona_url <- df$url[i]
    persona_nombre <- df$nombre[i]
    
    # Campos de relación
    rel_fields <- c("padres", "conyuge", "pareja", "hijos", "hermanos", "familia")
    
    map_dfr(rel_fields, function(field) {
      if (field %in% names(df) && !is.na(df[[field]][i]) && df[[field]][i] != "") {
        links <- extract_links(df[[field]][i])
        if (nrow(links) > 0) {
          links %>%
            mutate(
              persona_origen_url = persona_url,
              persona_origen_nombre = persona_nombre,
              tipo_relacion = field
            )
        }
      }
    })
  })
  
  message("  - ", nrow(relaciones), " relaciones extraídas")
  
  return(list(
    personas = df,
    cargos = cargos,
    relaciones = relaciones
  ))
}

#' Guarda los resultados enriquecidos
#' @param resultado Lista retornada por enrich_familia
#' @param familia_nombre Nombre de la familia para los archivos
save_enriched <- function(resultado, familia_nombre) {
  dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
  
  base_name <- str_replace(familia_nombre, "_completo\\.csv$", "")
  
  # Guardar cargos
  if (nrow(resultado$cargos) > 0) {
    cargos_file <- file.path(OUTPUT_DIR, paste0(base_name, "_cargos.csv"))
    write_csv(resultado$cargos, cargos_file)
    message("  Guardado: ", cargos_file)
  }
  
  # Guardar relaciones
  if (nrow(resultado$relaciones) > 0) {
    rel_file <- file.path(OUTPUT_DIR, paste0(base_name, "_relaciones.csv"))
    write_csv(resultado$relaciones, rel_file)
    message("  Guardado: ", rel_file)
  }
}

# ============================================================================
# Ejemplo de uso
# ============================================================================

if (interactive()) {
  message("\n=== Ejemplo: Familia Aylwin ===\n")
  
  resultado <- enrich_familia("familia_aylwin_completo.csv")
  
  message("\n--- Cargos extraídos (primeros 10) ---")
  print(head(resultado$cargos, 10))
  
  message("\n--- Relaciones extraídas (primeras 10) ---")
  print(head(resultado$relaciones, 10))
  
  # Guardar
  # save_enriched(resultado, "familia_aylwin_completo.csv")
}

# ============================================================================
# Procesar todas las familias
# ============================================================================

#' Procesa todos los archivos de familias
process_all_families <- function() {
  files <- list.files(DATA_DIR, pattern = "_completo\\.csv$", full.names = FALSE)
  
  message("\n=== Procesando ", length(files), " familias ===\n")
  
  all_cargos <- tibble()
  all_relaciones <- tibble()
  
  for (f in files) {
    tryCatch({
      resultado <- enrich_familia(f)
      
      if (nrow(resultado$cargos) > 0) {
        resultado$cargos$familia_origen <- f
        all_cargos <- bind_rows(all_cargos, resultado$cargos)
      }
      
      if (nrow(resultado$relaciones) > 0) {
        resultado$relaciones$familia_origen <- f
        all_relaciones <- bind_rows(all_relaciones, resultado$relaciones)
      }
      
      save_enriched(resultado, f)
      
    }, error = function(e) {
      warning("Error procesando ", f, ": ", e$message)
    })
  }
  
  # Guardar consolidados
  if (nrow(all_cargos) > 0) {
    write_csv(all_cargos, file.path(OUTPUT_DIR, "_CONSOLIDADO_cargos.csv"))
    message("\nGuardado consolidado de cargos: ", nrow(all_cargos), " registros")
  }
  
  if (nrow(all_relaciones) > 0) {
    write_csv(all_relaciones, file.path(OUTPUT_DIR, "_CONSOLIDADO_relaciones.csv"))
    message("Guardado consolidado de relaciones: ", nrow(all_relaciones), " registros")
  }
  
  return(list(cargos = all_cargos, relaciones = all_relaciones))
}

resultado
