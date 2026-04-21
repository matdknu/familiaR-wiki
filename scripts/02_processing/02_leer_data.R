#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(readr)
  library(jsonlite)
  library(purrr)
  library(dplyr)
  library(stringr)
  library(tidyr)
})

`%||%` <- function(x, y) if (is.null(x)) y else x

clean_text <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- NA_character_
  x <- str_replace_all(x, "[\\x00-\\x1F\\x7F]", " ")
  x <- str_squish(x)
  x[x %in% c("", "NA", "NaN", "nan", "None")] <- NA_character_
  x
}

normalizar_familia <- function(raw) {
  raw %>%
    clean_text() %>%
    str_to_lower() %>%
    str_remove("^familia[_ ]") %>%
    str_remove("\\s*\\(familia\\)") %>%
    str_replace("\\s*\\((\\w+)\\)", "_\\1") %>%
    str_replace_all("\\s+", "_")
}

slugify_simple <- function(x) {
  x %>%
    clean_text() %>%
    replace_na("") %>%
    iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_replace_all("^_+|_+$", "")
}

parse_fecha_lugar <- function(raw) {
  empty <- tibble(anio = NA_integer_, fecha_completa = NA_character_,
                  lugar = NA_character_, causa_muerte = NA_character_)
  if (is.na(raw) || raw == "NA") return(empty)

  s <- str_trim(as.character(raw))

  # H06: "Siglo XVII/XVIII/XIX/XX" is a period, not cause of death
  if (str_detect(s, "(?i)^siglo\\s+[XIVLC]+")) {
    return(tibble(anio = NA_integer_, fecha_completa = s,
                  lugar = NA_character_, causa_muerte = NA_character_))
  }

  if (!str_detect(s, "\\d{4}")) {
    return(tibble(anio = NA_integer_, fecha_completa = NA_character_,
                  lugar = NA_character_, causa_muerte = s))
  }

  s <- str_remove(s, "\\(\\d+\\s*años\\)") %>% str_trim()
  s <- str_remove(s, "\\s*jul\\.\\s*") %>% str_trim()

  # H05: "Ciudad , DD de mes de YYYY" — location before date
  match_loc_date <- str_match(
    s,
    "^(.+?)\\s*,?\\s+(\\d{1,2}\\s+de\\s+\\w+\\s+de\\s+(\\d{4}))\\s*(.*)"
  )
  if (!is.na(match_loc_date[1, 1]) && !str_detect(match_loc_date[1, 2], "^\\d{1,2}$")) {
    lugar_part <- str_trim(paste(match_loc_date[1, 2], match_loc_date[1, 5]))
    return(tibble(
      anio = as.integer(match_loc_date[1, 4]),
      fecha_completa = str_trim(match_loc_date[1, 3]),
      lugar = na_if(str_trim(match_loc_date[1, 2]), ""),
      causa_muerte = NA_character_
    ))
  }

  # Standard: "DD de mes de YYYY lugar"
  match1 <- str_match(
    s,
    "^(?:e/\\s*\\d+\\.?º?\\s*y\\s*)?(\\d{1,2}\\s+de\\s+\\w+\\s+de\\s+(\\d{4}))\\s*(.*)"
  )
  if (!is.na(match1[1, 1])) {
    return(tibble(anio = as.integer(match1[1, 3]),
                  fecha_completa = str_trim(match1[1, 2]),
                  lugar = na_if(str_trim(match1[1, 4]), ""),
                  causa_muerte = NA_character_))
  }

  # H05: "Mes de YYYY lugar"
  match_mes <- str_match(
    s,
    "(?i)^(enero|febrero|marzo|abril|mayo|junio|julio|agosto|septiembre|octubre|noviembre|diciembre)\\s+de\\s+(\\d{4})\\s*(.*)"
  )
  if (!is.na(match_mes[1, 1])) {
    return(tibble(anio = as.integer(match_mes[1, 3]),
                  fecha_completa = paste(match_mes[1, 2], "de", match_mes[1, 3]),
                  lugar = na_if(str_trim(match_mes[1, 4]), ""),
                  causa_muerte = NA_character_))
  }

  # H05: "ca/circa/hacia YYYY" or plain "YYYY lugar"
  match2 <- str_match(s, "^(?:ca\\.?|circa|hacia)?\\s*(\\d{4})\\s*(.*)")
  if (!is.na(match2[1, 1])) {
    return(tibble(anio = as.integer(match2[1, 2]),
                  fecha_completa = match2[1, 2],
                  lugar = na_if(str_trim(match2[1, 3]), ""),
                  causa_muerte = NA_character_))
  }

  # Last resort: extract any 4-digit year from the string
  any_year <- str_extract(s, "\\d{4}")
  if (!is.na(any_year)) {
    return(tibble(anio = as.integer(any_year), fecha_completa = s,
                  lugar = NA_character_, causa_muerte = NA_character_))
  }

  tibble(anio = NA_integer_, fecha_completa = s,
         lugar = NA_character_, causa_muerte = NA_character_)
}

extraer_personas_y_links <- function(campo) {
  if (is.na(campo) || campo == "NA") {
    return(tibble(nombre = character(), wiki_slug = character(), wiki_url = character()))
  }

  partes <- str_split(as.character(campo), ";")[[1]] %>% str_trim()

  map_dfr(partes, function(parte) {
    match <- str_match(
      parte,
      "^(.+?)\\s*\\(https://es\\.wikipedia\\.org/wiki/([^)]+)\\)"
    )

    if (!is.na(match[1, 1])) {
      tibble(
        nombre = str_trim(match[1, 2]),
        wiki_slug = match[1, 3],
        wiki_url = paste0("https://es.wikipedia.org/wiki/", match[1, 3])
      )
    } else if (nchar(str_trim(parte)) > 0) {
      tibble(
        nombre = str_trim(parte),
        wiki_slug = NA_character_,
        wiki_url = NA_character_
      )
    } else {
      tibble(nombre = character(), wiki_slug = character(), wiki_url = character())
    }
  })
}

extraer_campo_infobox <- function(infobox, campo) {
  infobox <- clean_text(infobox)
  if (is.na(infobox)) return(NA_character_)
  pattern <- paste0(campo, ":\\s*(.+?)(?:\\s*\\||\\s*$)")
  match <- str_match(infobox, pattern)
  if (is.na(match[1, 1])) return(NA_character_)
  str_trim(match[1, 2])
}

extraer_partidos <- function(infobox) {
  infobox <- clean_text(infobox)
  if (is.na(infobox)) return(tibble(nombre = character(), wiki_slug = character(), wiki_url = character()))
  match <- str_match(infobox, "Partido político:\\s*(.+?)(?:\\s*\\||\\s*$)")
  if (is.na(match[1, 1])) return(tibble(nombre = character(), wiki_slug = character(), wiki_url = character()))
  extraer_personas_y_links(match[1, 2])
}

extraer_educacion <- function(infobox) {
  infobox <- clean_text(infobox)
  if (is.na(infobox)) return(tibble(nombre = character(), wiki_slug = character(), wiki_url = character()))
  match <- str_match(infobox, "Educado en:\\s*(.+?)(?:\\s*\\||\\s*$)")
  if (is.na(match[1, 1])) return(tibble(nombre = character(), wiki_slug = character(), wiki_url = character()))
  extraer_personas_y_links(match[1, 2])
}

extraer_cargos <- function(infobox, persona_id) {
  infobox <- clean_text(infobox)
  if (is.na(infobox)) return(tibble())

  predecesores <- str_match_all(infobox, "Predecesor:\\s*(.+?)(?:\\s*\\||\\s*$)")[[1]]
  sucesores <- str_match_all(infobox, "Sucesora?:\\s*(.+?)(?:\\s*\\||\\s*$)")[[1]]

  pred_tibble <- if (nrow(predecesores) > 0) {
    tibble(persona_id = persona_id, raw = predecesores[, 2], rol = "predecesor") %>%
      mutate(parsed = map(raw, extraer_personas_y_links)) %>%
      unnest(parsed)
  } else {
    tibble()
  }

  suc_tibble <- if (nrow(sucesores) > 0) {
    tibble(persona_id = persona_id, raw = sucesores[, 2], rol = "sucesor") %>%
      mutate(parsed = map(raw, extraer_personas_y_links)) %>%
      unnest(parsed)
  } else {
    tibble()
  }

  bind_rows(pred_tibble, suc_tibble)
}

procesar_relacion <- function(df, campo, campo_perfil, tipo) {
  empty_rel <- tibble(
    persona_id = integer(), nombre_relacionado = character(),
    wiki_slug = character(), wiki_url = character(),
    tipo_relacion = character(), fuente = character()
  )

  desde_perfiles <- if (campo_perfil %in% names(df)) {
    sub <- df %>%
      select(persona_id, valor = all_of(campo_perfil)) %>%
      filter(!is.na(valor))
    if (nrow(sub) == 0) {
      empty_rel
    } else {
      sub %>%
        mutate(parsed = map(valor, extraer_personas_y_links)) %>%
        unnest(parsed) %>%
        mutate(tipo_relacion = tipo, fuente = "perfil_wiki") %>%
        select(persona_id, nombre_relacionado = nombre, wiki_slug, wiki_url, tipo_relacion, fuente)
    }
  } else {
    empty_rel
  }

  desde_campo <- if (campo %in% names(df)) {
    sub <- df %>%
      select(persona_id, valor = all_of(campo)) %>%
      filter(!is.na(valor))
    if (nrow(sub) == 0) {
      empty_rel
    } else {
      sub %>%
        mutate(parsed = map(valor, extraer_personas_y_links)) %>%
        unnest(parsed) %>%
        mutate(tipo_relacion = tipo, fuente = "campo_directo") %>%
        select(persona_id, nombre_relacionado = nombre, wiki_slug, wiki_url, tipo_relacion, fuente)
    }
  } else {
    empty_rel
  }

  bind_rows(desde_perfiles, desde_campo) %>%
    distinct(persona_id, nombre_relacionado, tipo_relacion, .keep_all = TRUE)
}

inferir_pais <- function(lugar, infobox, nacionalidad = NA_character_) {
  # H08: Use nationality first (most reliable), then location keywords
  # H07: Priority: nacionalidad > lugar_nacimiento > infobox
  paises_map <- c(
    "argentino" = "argentina", "argentina" = "argentina",
    "chileno" = "chile", "chilena" = "chile", "chile" = "chile",
    "colombiano" = "colombia", "colombiana" = "colombia", "colombia" = "colombia",
    "peruano" = "peru", "peruana" = "peru", "perú" = "peru", "peru" = "peru",
    "mexicano" = "mexico", "mexicana" = "mexico", "méxico" = "mexico", "mexico" = "mexico",
    "venezolano" = "venezuela", "venezolana" = "venezuela", "venezuela" = "venezuela",
    "uruguayo" = "uruguay", "uruguaya" = "uruguay", "uruguay" = "uruguay",
    "boliviano" = "bolivia", "boliviana" = "bolivia", "bolivia" = "bolivia",
    "paraguayo" = "paraguay", "paraguaya" = "paraguay", "paraguay" = "paraguay",
    "ecuatoriano" = "ecuador", "ecuatoriana" = "ecuador", "ecuador" = "ecuador"
  )

  # Check nationality first
  nac_text <- str_to_lower(clean_text(nacionalidad))
  if (!is.na(nac_text) && nac_text != "") {
    for (kw in names(paises_map)) {
      if (str_detect(nac_text, fixed(kw))) return(paises_map[[kw]])
    }
  }

  # Then check lugar (but skip colonial entities)
  lugar_text <- str_to_lower(clean_text(lugar))
  if (!is.na(lugar_text) && lugar_text != "") {
    # H08: Skip colonial entities — they span multiple modern countries
    is_colonial <- str_detect(lugar_text,
      "virreinato|nueva granada|provincias unidas|intendencia|corregimiento|capitanía general|gobernación")
    if (!is_colonial) {
      for (kw in names(paises_map)) {
        if (str_detect(lugar_text, fixed(kw))) return(paises_map[[kw]])
      }
    }
    # Even with colonial, check for specific city names
    city_map <- c(
      "buenos aires" = "argentina", "córdoba" = "argentina", "rosario" = "argentina",
      "salta" = "argentina", "mendoza" = "argentina", "tucumán" = "argentina",
      "santiago" = "chile", "valparaíso" = "chile", "concepción" = "chile",
      "bogotá" = "colombia", "medellín" = "colombia", "cartagena" = "colombia",
      "lima" = "peru", "cusco" = "peru", "arequipa" = "peru",
      "caracas" = "venezuela", "maracaibo" = "venezuela",
      "montevideo" = "uruguay", "la paz" = "bolivia", "sucre" = "bolivia",
      "asunción" = "paraguay", "quito" = "ecuador", "guayaquil" = "ecuador",
      "ciudad de méxico" = "mexico", "guadalajara" = "mexico", "puebla" = "mexico"
    )
    for (city in names(city_map)) {
      if (str_detect(lugar_text, fixed(city))) return(city_map[[city]])
    }
  }

  # Finally check infobox (nationality field already checked, so this catches other signals)
  info_text <- str_to_lower(clean_text(infobox))
  if (!is.na(info_text) && info_text != "") {
    nac_in_infobox <- str_match(info_text, "nacionalidad:\\s*([^|]+)")
    if (!is.na(nac_in_infobox[1, 2])) {
      nac_val <- str_trim(nac_in_infobox[1, 2])
      for (kw in names(paises_map)) {
        if (str_detect(nac_val, fixed(kw))) return(paises_map[[kw]])
      }
    }
  }

  NA_character_
}

leer_data <- function(
  ruta = "data/processed/familias/_CONSOLIDADO_familias_latam.csv",
  guardar = TRUE,
  dir_output = "data/processed/02_leer_data"
) {
  if (!file.exists(ruta)) {
    stop("No se encontró el consolidado: ", ruta)
  }

  raw_all <- read_delim(ruta, delim = ";", show_col_types = FALSE, escape_double = FALSE, trim_ws = TRUE) %>%
    mutate(across(everything(), clean_text))

  # H01: Deduplicate by URL — keep the row with most non-NA columns
  n_before <- nrow(raw_all)
  raw_all <- raw_all %>%
    filter(!is.na(nombre), trimws(nombre) != "") %>%   # H02: remove nameless rows
    mutate(.completeness = rowSums(!is.na(across(everything()))))
  raw <- raw_all %>%
    arrange(url, desc(.completeness)) %>%
    distinct(url, .keep_all = TRUE) %>%
    select(-.completeness) %>%
    mutate(persona_id = row_number())
  n_after <- nrow(raw)
  message(sprintf("Deduplicación: %d → %d filas (%d duplicados removidos)",
                  n_before, n_after, n_before - n_after))

  personas <- raw %>%
    mutate(
      nac = map(fecha_nacimiento, parse_fecha_lugar),
      fal = map(fecha_fallecimiento, parse_fecha_lugar)
    ) %>%
    unnest_wider(nac, names_sep = "_") %>%
    unnest_wider(fal, names_sep = "_") %>%
    rename(
      anio_nacimiento = nac_anio,
      fecha_nacimiento_parseada = nac_fecha_completa,
      lugar_nacimiento_parseado = nac_lugar,
      anio_fallecimiento = fal_anio,
      fecha_fallecimiento_parseada = fal_fecha_completa,
      lugar_fallecimiento_parseado = fal_lugar,
      causa_muerte = fal_causa_muerte
    ) %>%
    mutate(
      familia_norm = normalizar_familia(familia),
      wiki_slug_propio = str_extract(url %||% "", "(?<=wiki/).+") %>% clean_text(),
      wiki_slug_propio = coalesce(wiki_slug_propio, slugify_simple(nombre)),
      # H07: Improved country inference with nationality priority
      pais_inferido = pmap_chr(
        list(lugar_nacimiento_parseado, infobox_completa, nacionalidad),
        ~ inferir_pais(..1, ..2, ..3)
      ),
      # H07+H09: Load manual overrides and apply cascade: override > inferred > csv
      pais_csv = coalesce(pais, pais_origen),
      # H07: FIXED cascade — inferred (from nationality/location) takes priority over CSV family
      pais_base = coalesce(pais_inferido, pais_csv),
      # H03: Filter impossible years
      anio_nacimiento = if_else(
        !is.na(anio_nacimiento) & (anio_nacimiento > 2026L | anio_nacimiento < 1300L),
        NA_integer_, anio_nacimiento
      ),
      anio_fallecimiento = if_else(
        !is.na(anio_fallecimiento) & (anio_fallecimiento > 2026L | anio_fallecimiento < 1300L),
        NA_integer_, anio_fallecimiento
      ),
      siglo_nacimiento = if_else(!is.na(anio_nacimiento), (anio_nacimiento %/% 100L) + 1L, NA_integer_),
      religion_infobox = map_chr(infobox_completa, ~ extraer_campo_infobox(.x, "Religión")),
      rango_militar = map_chr(infobox_completa, ~ extraer_campo_infobox(.x, "Rango militar")),
      rama_militar = map_chr(infobox_completa, ~ extraer_campo_infobox(.x, "Rama militar"))
    ) %>%
    select(any_of(c(
      "persona_id", "nombre", "url", "wiki_slug_propio",
      "familia", "familia_norm", "pais", "pais_origen", "pais_csv", "pais_inferido", "pais_base",
      "fecha_nacimiento", "fecha_nacimiento_parseada", "anio_nacimiento", "lugar_nacimiento", "lugar_nacimiento_parseado",
      "fecha_fallecimiento", "fecha_fallecimiento_parseada", "anio_fallecimiento", "lugar_fallecimiento", "lugar_fallecimiento_parseado",
      "causa_muerte", "nacionalidad", "ocupacion", "partido_politico", "residencia",
      "religion", "religion_infobox", "rango_militar", "rama_militar",
      "educacion", "alma_mater", "infobox_completa", "infobox_json"
    )))

  # H09: Apply manual country overrides
  override_path <- "data/manual/url_pais_extra.csv"
  if (file.exists(override_path)) {
    overrides <- read_csv(override_path, show_col_types = FALSE) %>%
      transmute(url = as.character(url), pais_override = tolower(trimws(pais))) %>%
      filter(!is.na(url), !is.na(pais_override)) %>%
      distinct(url, .keep_all = TRUE)
    personas <- personas %>%
      left_join(overrides, by = "url") %>%
      mutate(pais_base = coalesce(pais_override, pais_base)) %>%
      select(-pais_override)
    message(sprintf("Overrides de país aplicados: %d", sum(!is.na(overrides$pais_override))))
  }

  ocupaciones <- raw %>%
    select(persona_id, ocupacion) %>%
    filter(!is.na(ocupacion)) %>%
    mutate(ocupacion_lista = str_split(ocupacion, ",\\s*|\\s+y\\s+")) %>%
    unnest(ocupacion_lista) %>%
    mutate(
      ocupacion_norm = ocupacion_lista %>%
        str_to_lower() %>%
        str_remove("\\s*\\(.*\\)") %>%
        str_squish()
    ) %>%
    filter(!is.na(ocupacion_norm), ocupacion_norm != "") %>%
    select(persona_id, ocupacion = ocupacion_norm) %>%
    distinct()

  relaciones_raw <- bind_rows(
    procesar_relacion(raw, "padres", "perfiles_relacionados_padres", "padre/madre"),
    procesar_relacion(raw, "hijos", "perfiles_relacionados_hijos", "hijo/a"),
    procesar_relacion(raw, "conyuge", "perfiles_relacionados_conyuge", "conyuge"),
    procesar_relacion(raw, "pareja", "perfiles_relacionados_pareja", "pareja"),
    procesar_relacion(raw, "hermanos", "perfiles_relacionados_hermanos", "hermano/a")
  )

  # H14: Use distinct slugs to avoid many-to-many joins
  personas_slugs <- personas %>%
    distinct(wiki_slug_propio, .keep_all = TRUE) %>%
    select(persona_relacionada_id = persona_id, nombre, wiki_slug_propio, familia_norm)

  relaciones <- relaciones_raw %>%
    left_join(personas_slugs, by = c("wiki_slug" = "wiki_slug_propio")) %>%
    rename(nombre_match = nombre, familia_match = familia_norm)

  sin_match <- relaciones %>% filter(is.na(persona_relacionada_id), !is.na(nombre_relacionado))

  if (nrow(sin_match) > 0) {
    nombres_dataset <- personas$nombre
    matches_fuzzy <- sin_match %>%
      mutate(
        match_idx = stringdist::amatch(nombre_relacionado, nombres_dataset, method = "jw", maxDist = 0.15),
        persona_relacionada_id_fuzzy = ifelse(!is.na(match_idx), personas$persona_id[match_idx], NA_integer_)
      ) %>%
      select(persona_id, nombre_relacionado, tipo_relacion, persona_relacionada_id_fuzzy)

    relaciones <- relaciones %>%
      left_join(matches_fuzzy, by = c("persona_id", "nombre_relacionado", "tipo_relacion")) %>%
      mutate(persona_relacionada_id = coalesce(persona_relacionada_id, persona_relacionada_id_fuzzy)) %>%
      select(-persona_relacionada_id_fuzzy)
  }

  # H12: Remove self-references
  relaciones <- relaciones %>%
    filter(is.na(persona_relacionada_id) | persona_id != persona_relacionada_id)
  # H11: Remove duplicate relations
  relaciones <- relaciones %>%
    distinct(persona_id, persona_relacionada_id, tipo_relacion, .keep_all = TRUE)

  partidos_raw <- personas %>%
    select(persona_id, infobox_completa) %>%
    filter(!is.na(infobox_completa)) %>%
    mutate(partido = map(infobox_completa, extraer_partidos))
  partidos <- if (nrow(partidos_raw) > 0) {
    partidos_raw %>%
      unnest(partido) %>%
      select(persona_id, partido = nombre, partido_wiki = wiki_slug, partido_url = wiki_url) %>%
      distinct()
  } else {
    tibble(persona_id = integer(), partido = character(), partido_wiki = character(), partido_url = character())
  }

  educacion_raw <- personas %>%
    select(persona_id, infobox_completa) %>%
    filter(!is.na(infobox_completa)) %>%
    mutate(inst = map(infobox_completa, extraer_educacion))
  educacion_tbl <- if (nrow(educacion_raw) > 0) {
    educacion_raw %>%
      unnest(inst) %>%
      select(persona_id, institucion = nombre, institucion_wiki = wiki_slug, institucion_url = wiki_url) %>%
      distinct()
  } else {
    tibble(persona_id = integer(), institucion = character(), institucion_wiki = character(), institucion_url = character())
  }

  sucesiones <- personas %>%
    select(persona_id, infobox_completa) %>%
    pmap_dfr(function(persona_id, infobox_completa) {
      extraer_cargos(infobox_completa, persona_id)
    }) %>%
    left_join(
      personas %>% select(persona_relacionada_id = persona_id, wiki_slug_propio),
      by = c("wiki_slug" = "wiki_slug_propio")
    )

  out <- list(
    personas = personas,
    ocupaciones = ocupaciones,
    relaciones = relaciones,
    partidos = partidos,
    educacion = educacion_tbl,
    sucesiones = sucesiones
  )

  if (isTRUE(guardar)) {
    dir.create(dir_output, recursive = TRUE, showWarnings = FALSE)

    write_rds(out$personas, file.path(dir_output, "personas.rds"))
    write_rds(out$ocupaciones, file.path(dir_output, "ocupaciones.rds"))
    write_rds(out$relaciones, file.path(dir_output, "relaciones.rds"))
    write_rds(out$partidos, file.path(dir_output, "partidos.rds"))
    write_rds(out$educacion, file.path(dir_output, "educacion.rds"))
    write_rds(out$sucesiones, file.path(dir_output, "sucesiones.rds"))

    write_csv(out$personas, file.path(dir_output, "personas.csv"))
    write_csv(out$ocupaciones, file.path(dir_output, "ocupaciones.csv"))
    write_csv(out$relaciones, file.path(dir_output, "relaciones.csv"))
    write_csv(out$partidos, file.path(dir_output, "partidos.csv"))
    write_csv(out$educacion, file.path(dir_output, "educacion.csv"))
    write_csv(out$sucesiones, file.path(dir_output, "sucesiones.csv"))
  }

  out
}

if (sys.nframe() == 0) {
  datos <- leer_data()
  cat("Personas:", nrow(datos$personas), "\n")
  cat("Ocupaciones:", nrow(datos$ocupaciones), "\n")
  cat("Relaciones:", nrow(datos$relaciones), "\n")
  cat("Partidos:", nrow(datos$partidos), "\n")
  cat("Educación:", nrow(datos$educacion), "\n")
  cat("Sucesiones:", nrow(datos$sucesiones), "\n")
}


