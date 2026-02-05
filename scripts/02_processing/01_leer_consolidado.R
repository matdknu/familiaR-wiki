# ============================================================================
# leer_consolidado.R
# Lee el consolidado LATAM, parsea infobox_json y construye un df consolidado.
# ============================================================================
# El resultado incluye datos$df_consolidado: una fila por persona con
# nombre, familia, pais, fecha_nacimiento, fecha_fallecimiento, nacionalidad,
# cargos, n_cargos, familiares, n_familiares, conexion_otros_paises, etc.
#
# Uso:
#   source("scripts/02_processing/leer_consolidado.R")
#   d <- leer_consolidado()                           # solo tibble del CSV
#   datos <- leer_consolidado_y_parsear_json()        # list con df_consolidado
#   datos <- leer_consolidado_y_parsear_json(guardar = TRUE)  # además escribe CSVs
#   df_final <- datos$df_consolidado   # ver como df
#
#   Rscript leer_consolidado.R        # lee y muestra resumen
#   Rscript leer_consolidado.R --json # lee, parsea, escribe y muestra preview del df
# ============================================================================

library(readr)
library(jsonlite)
library(purrr)
library(dplyr)
library(stringr)
library(tidyr)

#' Lee el archivo consolidado LATAM (CSV con ;).
#'
#' @param ruta Ruta al CSV. Por defecto data/processed/familias/_CONSOLIDADO_familias_latam.csv
#' @return Tibble con todas las columnas del consolidado.
leer_consolidado <- function(ruta = "data/processed/familias/_CONSOLIDADO_familias_latam.csv") {
  if (!file.exists(ruta)) {
    stop("No se encontró el consolidado: ", ruta)
  }
  read_delim(ruta, delim = ";", show_col_types = FALSE)
}

slugify_simple <- function(x) {
  x %>%
    str_replace_all("\\s+", " ") %>%
    str_trim() %>%
    iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_replace_all("^_+|_+$", "")
}

#' Construye un único data frame consolidado: una fila por persona con cargos,
#' familiares, fechas, nacionalidad y conexión con otros países.
#'
#' @param datos Lista con consolidado y opcionalmente personas, posiciones, family_relations.
#' @return Tibble con columnas: nombre, url, familia, pais, fecha_nacimiento, fecha_fallecimiento,
#'   nacionalidad, cargos, n_cargos, familiares, n_familiares, conexion_otros_paises, ...
construir_df_consolidado <- function(datos) {
  c <- datos$consolidado
  if (is.null(c) || nrow(c) == 0) return(c)

  # Clave para unir con tablas parseadas (mismo criterio que el parser)
  c$person_id <- slugify_simple(replace_na(as.character(c$nombre), ""))

  # Columnas base del consolidado
  cols_base <- c("person_id", "nombre", "url", "familia", "pais", "pais_origen",
                 "fecha_nacimiento", "lugar_nacimiento", "fecha_fallecimiento", "lugar_fallecimiento",
                 "nacionalidad", "ocupacion", "partido_politico", "religion", "residencia",
                 "padres", "conyuge", "pareja", "hijos", "hermanos", "biografia_inicial")
  cols_base <- intersect(cols_base, names(c))
  df <- c %>% dplyr::select(dplyr::all_of(cols_base))

  # Cargos (del JSON parseado)
  if (!is.null(datos$posiciones) && nrow(datos$posiciones) > 0) {
    cargos_agg <- datos$posiciones %>%
      group_by(person_id) %>%
      summarise(
        cargos = paste(unique(na.omit(position_title_raw)), collapse = " | "),
        n_cargos = n(),
        .groups = "drop"
      )
    df <- df %>% left_join(cargos_agg, by = "person_id")
  } else {
    df$cargos <- if ("cargos_politicos" %in% names(c)) c$cargos_politicos else NA_character_
    df$n_cargos <- NA_integer_
  }

  # Familiares (del JSON parseado)
  if (!is.null(datos$family_relations) && nrow(datos$family_relations) > 0) {
    fam_agg <- datos$family_relations %>%
      group_by(person_id) %>%
      summarise(
        familiares = paste(unique(na.omit(related_name_raw)), collapse = " | "),
        n_familiares = n(),
        tipos_relacion = paste(unique(na.omit(relation_type)), collapse = ", "),
        .groups = "drop"
      )
    df <- df %>% left_join(fam_agg, by = "person_id")
  } else {
    df$familiares <- NA_character_
    df$n_familiares <- NA_integer_
    df$tipos_relacion <- NA_character_
  }

  # Conexión con otros países: multinacional o nacionalidad distinta al país del registro
  pais_norm <- str_to_lower(str_trim(replace_na(as.character(df$pais), "")))
  nac_norm <- str_to_lower(str_trim(replace_na(as.character(df$nacionalidad), "")))
  df$conexion_otros_paises <- case_when(
    nac_norm == "" | is.na(nac_norm) ~ NA_character_,
    str_detect(nac_norm, ",") | str_detect(nac_norm, " y ") ~ "varias_nacionalidades",
    nac_norm != pais_norm & nchar(pais_norm) > 0 ~ "nacionalidad_distinta_al_pais",
    TRUE ~ "mismo_pais"
  )

  # Año nacimiento / fallecimiento (extracción simple)
  df$ano_nacimiento <- as.integer(str_extract(replace_na(as.character(df$fecha_nacimiento), ""), "\\b(1[0-9]{3}|20[0-2][0-9])\\b"))
  df$ano_fallecimiento <- as.integer(str_extract(replace_na(as.character(df$fecha_fallecimiento), ""), "\\b(1[0-9]{3}|20[0-2][0-9])\\b"))

  df
}

#' Convierte el resultado de fromJSON a un data frame de filas infobox (label, value_text, ...).
as_infobox_rows <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.data.frame(x)) return(x)
  if (is.list(x) && length(x) > 0) {
    # Lista de listas o de vectores
    if (is.list(x[[1]])) {
      return(dplyr::bind_rows(lapply(x, as.data.frame)))
    }
    return(as.data.frame(x))
  }
  NULL
}

#' Escribe las tablas parseadas en un directorio (CSV).
#'
#' @param datos Lista con personas, posiciones, family_relations, etc.
#' @param dir_output Directorio donde guardar. Por defecto data/processed/from_consolidado
guardar_outputs <- function(datos, dir_output = "data/processed/from_consolidado") {
  dir.create(dir_output, showWarnings = FALSE, recursive = TRUE)
  if (!is.null(datos$personas))
    write_csv(datos$personas, file.path(dir_output, "persons_normalized.csv"))
  if (!is.null(datos$posiciones))
    write_csv(datos$posiciones, file.path(dir_output, "positions_normalized.csv"))
  if (!is.null(datos$family_relations))
    write_csv(datos$family_relations, file.path(dir_output, "family_relations_normalized.csv"))
  if (!is.null(datos$education))
    write_csv(datos$education, file.path(dir_output, "education_normalized.csv"))
  if (!is.null(datos$affiliations))
    write_csv(datos$affiliations, file.path(dir_output, "affiliations_normalized.csv"))
  if (!is.null(datos$links))
    write_csv(datos$links, file.path(dir_output, "links_normalized.csv"))
  if (!is.null(datos$derived))
    write_csv(datos$derived, file.path(dir_output, "derived_normalized.csv"))
  message("Outputs guardados en ", dir_output)
  invisible(dir_output)
}

#' Lee el consolidado y parsea la columna infobox_json con el parser normalizado.
#' Requiere que exista 01_parse_and_normalize.R (parse_all_infoboxes).
#'
#' @param ruta Ruta al CSV consolidado.
#' @param guardar Si TRUE, escribe las tablas parseadas en dir_output.
#' @param dir_output Directorio donde guardar los CSV (solo si guardar = TRUE).
#' @return Lista con: consolidado (tibble), personas, posiciones, family_relations,
#'   education, affiliations, links, derived.
leer_consolidado_y_parsear_json <- function(
    ruta = "data/processed/familias/_CONSOLIDADO_familias_latam.csv",
    guardar = FALSE,
    dir_output = "data/processed/from_consolidado") {
  consolidado <- leer_consolidado(ruta)

  if (!"infobox_json" %in% names(consolidado)) {
    message("El consolidado no tiene columna 'infobox_json'. Se devuelve consolidado + df_consolidado.")
    out <- list(consolidado = consolidado, df_consolidado = construir_df_consolidado(list(consolidado = consolidado)))
    if (isTRUE(guardar)) {
      dir.create(dir_output, showWarnings = FALSE, recursive = TRUE)
      write_csv(out$df_consolidado, file.path(dir_output, "df_consolidado.csv"))
    }
    dir.create("outputs", showWarnings = FALSE, recursive = TRUE)
    write_csv(out$df_consolidado, "outputs/df_consolidado.csv")
    message("Output del df: outputs/df_consolidado.csv")
    return(out)
  }

  # Construir lista de infoboxes para el parser
  infobox_list <- map(seq_len(nrow(consolidado)), function(i) {
    raw <- consolidado$infobox_json[i]
    if (is.na(raw) || raw == "") return(NULL)
    parsed <- tryCatch(fromJSON(raw), error = function(e) NULL)
    as_infobox_rows(parsed)
  })

  # Cargar el parser (ruta relativa a la raíz del proyecto)
  parse_script <- "scripts/02_processing/01_parse_and_normalize.R"
  if (!file.exists(parse_script)) {
    message("No se encontró 01_parse_and_normalize.R. Se devuelve consolidado + df_consolidado.")
    out <- list(consolidado = consolidado, df_consolidado = construir_df_consolidado(list(consolidado = consolidado)))
    if (isTRUE(guardar)) {
      dir.create(dir_output, showWarnings = FALSE, recursive = TRUE)
      write_csv(out$df_consolidado, file.path(dir_output, "df_consolidado.csv"))
    }
    dir.create("outputs", showWarnings = FALSE, recursive = TRUE)
    write_csv(out$df_consolidado, "outputs/df_consolidado.csv")
    message("Output del df: outputs/df_consolidado.csv")
    return(out)
  }

  suppressPackageStartupMessages(source(parse_script, local = TRUE))

  parsed <- parse_all_infoboxes(infobox_list)

  out <- list(
    consolidado = consolidado,
    personas = parsed$persons_all,
    posiciones = parsed$positions_all,
    family_relations = parsed$family_relations_all,
    education = parsed$education_all,
    affiliations = parsed$affiliations_all,
    links = parsed$links_all,
    derived = parsed$derived_all
  )

  # Un solo df consolidado: cargos, familiares, fechas, nacionalidad, conexión otros países
  out$df_consolidado <- construir_df_consolidado(out)

  if (isTRUE(guardar)) {
    guardar_outputs(out, dir_output)
    if (!is.null(out$df_consolidado)) {
      write_csv(out$df_consolidado, file.path(dir_output, "df_consolidado.csv"))
      message("df_consolidado guardado: ", nrow(out$df_consolidado), " filas")
    }
  }

  # Siempre escribir output visible en outputs/ para poder abrirlo y ver el df
  if (!is.null(out$df_consolidado)) {
    dir.create("outputs", showWarnings = FALSE, recursive = TRUE)
    out_path <- "outputs/df_consolidado.csv"
    write_csv(out$df_consolidado, out_path)
    message("Output del df: ", out_path, " (", nrow(out$df_consolidado), " filas)")
  }

  out
}

# =============================================================================
# OUTPUT DEL DF: al ejecutar el script, el df se escribe y se imprime
# =============================================================================
# - Archivo: outputs/df_consolidado.csv (siempre que se construya el df)
# - Consola: se imprime abajo (primeras filas y resumen)
# =============================================================================

# Al ejecutar con Rscript o source() + llamar a la función, el df queda en datos$df_consolidado
# y en outputs/df_consolidado.csv. Para verlo en consola:
if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  if ("--parsear" %in% args || "--json" %in% args) {
    message("Leyendo consolidado y parseando infobox_json...")
    datos <- leer_consolidado_y_parsear_json(guardar = TRUE)
    message("Consolidado: ", nrow(datos$consolidado), " filas")
    if (!is.null(datos$personas)) {
      message("Personas (del JSON): ", nrow(datos$personas))
      message("Relaciones familiares: ", nrow(datos$family_relations))
    }
  } else {
    d <- leer_consolidado()
    datos <- list(consolidado = d, df_consolidado = construir_df_consolidado(list(consolidado = d)))
    dir.create("outputs", showWarnings = FALSE, recursive = TRUE)
    write_csv(datos$df_consolidado, "outputs/df_consolidado.csv")
    message("Consolidado LATAM: ", nrow(d), " filas. Output: outputs/df_consolidado.csv")
  }

  # --- OUTPUT VISIBLE DEL DF (siempre que exista df_consolidado) ---
  if (!is.null(datos$df_consolidado)) {
    message("\n=============== OUTPUT DF CONSOLIDADO ===============")
    message("Filas: ", nrow(datos$df_consolidado), "  |  Columnas: ", ncol(datos$df_consolidado))
    message("Archivo: outputs/df_consolidado.csv")
    message("------------------------------------------------------")
    message("Columnas: ", paste(names(datos$df_consolidado), collapse = " | "))
    message("------------------------------------------------------")
    message("Primeras 20 filas (nombre, pais, fecha_nac, fecha_fall, nacionalidad, cargos, conexion_otros_paises):\n")
    cols_show <- intersect(
      c("nombre", "pais", "fecha_nacimiento", "fecha_fallecimiento", "nacionalidad", "cargos", "conexion_otros_paises"),
      names(datos$df_consolidado)
    )
    print(as.data.frame(head(datos$df_consolidado[, cols_show], 20)))
    message("\n=====================================================\n")
  }
}

