# ============================================================================
# 01_parse_and_normalize.R
# Production parser: RAW infobox JSON -> normalized relational tables
# ============================================================================
# Principles:
# - Treat infobox_json as immutable raw input.
# - Normalize outputs only; keep raw fields.
# - Strict schemas and defensive programming.
# - No I/O side effects by default.
# ============================================================================

library(jsonlite)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(tibble)

# ----------------------------------------------------------------------------
# Schema helpers
# ----------------------------------------------------------------------------

assert_columns <- function(tbl, required_cols, table_name = "table") {
  missing_cols <- setdiff(required_cols, names(tbl))
  if (length(missing_cols) > 0) {
    stop(
      sprintf("Schema error in %s. Missing columns: %s",
              table_name, paste(missing_cols, collapse = ", ")),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

coalesce_chr <- function(x, y = NA_character_) {
  ifelse(is.na(x) | x == "", y, x)
}

normalize_text <- function(x) {
  x %>%
    str_replace_all("\\s+", " ") %>%
    str_trim()
}

normalize_label <- function(label) {
  normalize_text(label) %>%
    str_to_lower()
}

slugify <- function(x) {
  x %>%
    iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_replace_all("^_+|_+$", "")
}

safe_year <- function(x) {
  if (is.na(x) || x == "") return(NA_integer_)
  year <- str_extract(x, "\\b(1[789]\\d{2}|20[0-2]\\d)\\b")
  if (!is.na(year)) return(as.integer(year))

  # Handle "Siglo XX" style (roman numerals)
  if (str_detect(str_to_lower(x), "siglo")) {
    roman <- str_match(str_to_lower(x), "siglo\\s+([ivxlcdm]+)")[, 2]
    roman_map <- c(i = 1, v = 5, x = 10, l = 50, c = 100, d = 500, m = 1000)
    if (!is.na(roman)) {
      chars <- str_split(roman, "")[[1]]
      values <- map_int(chars, ~ roman_map[.x])
      total <- sum(values) # conservative, not full roman conversion
      if (!is.na(total) && total >= 18 && total <= 21) {
        return((total - 1) * 100)
      }
    }
  }
  NA_integer_
}

era_guess_from_year <- function(year) {
  if (is.na(year)) return(NA_character_)
  if (year >= 1700 && year < 1800) return("1700s")
  if (year >= 1800 && year < 1900) return("1800s")
  if (year >= 1900 && year < 2000) return("1900s")
  if (year >= 2000 && year < 2100) return("2000s")
  NA_character_
}

generate_person_id <- function(canonical_name, fallback_index = NULL) {
  if (!is.na(canonical_name) && canonical_name != "") {
    return(slugify(canonical_name))
  }
  if (!is.null(fallback_index)) {
    return(paste0("person_", fallback_index))
  }
  return(paste0("person_", sample(10000:99999, 1)))
}

# ----------------------------------------------------------------------------
# Link extraction
# ----------------------------------------------------------------------------

extract_links <- function(value_with_links) {
  if (is.na(value_with_links) || value_with_links == "") {
    return(tibble(entity_text = character(), url = character()))
  }
  pattern <- "([^;()]+?)\\s*\\((https?://[^)]+)\\)"
  matches <- str_match_all(value_with_links, pattern)[[1]]
  if (nrow(matches) > 0) {
    return(tibble(
      entity_text = normalize_text(matches[, 2]),
      url = matches[, 3]
    ))
  }
  items <- str_split(value_with_links, ";")[[1]] %>% normalize_text()
  items <- items[items != ""]
  tibble(entity_text = items, url = NA_character_)
}

split_names <- function(value_text) {
  if (is.na(value_text) || value_text == "") return(character())
  if (str_detect(value_text, ";")) {
    return(str_split(value_text, ";")[[1]] %>% normalize_text() %>% discard(~ .x == ""))
  }
  if (str_detect(value_text, " y ")) {
    return(str_split(value_text, " y ")[[1]] %>% normalize_text() %>% discard(~ .x == ""))
  }
  if (str_detect(value_text, ",")) {
    return(str_split(value_text, ",")[[1]] %>% normalize_text() %>% discard(~ .x == ""))
  }
  normalize_text(value_text)
}

extract_parenthetical_detail <- function(text) {
  detail <- str_match(text, "\\(([^)]+)\\)")[, 2]
  ifelse(is.na(detail), NA_character_, detail)
}

is_count_only <- function(x) {
  if (is.na(x) || x == "") return(TRUE)
  x <- str_trim(str_to_lower(x))
  count_words <- c(
    "uno", "dos", "tres", "cuatro", "cinco", "seis", "siete", "ocho",
    "nueve", "diez", "once", "doce", "trece", "catorce", "quince",
    "veinte", "treinta", "cuarenta", "cincuenta", "sesenta",
    "setenta", "ochenta", "noventa", "cien"
  )
  str_detect(x, "^\\d+$") || x %in% count_words
}

# ----------------------------------------------------------------------------
# Label classification
# ----------------------------------------------------------------------------

is_section_header <- function(label_norm) {
  section_headers <- c(
    "información personal", "información profesional", "familia",
    "educación", "carrera política", "carrera militar", "carrera artística",
    "carrera deportiva", "obras", "premios", "distinciones", "firma",
    "notas", "referencias", "enlaces externos", "información personal", "biografía"
  )
  label_norm %in% section_headers
}

is_noise_label <- function(label_norm) {
  noise <- c("detalle", "editar datos en wikidata", "", "imagen", "escudo", "bandera")
  label_norm %in% noise || str_detect(label_norm, "editar datos")
}

is_position_start <- function(label_norm, value_text) {
  position_keywords <- c(
    "presidente", "senador", "diputado", "ministro", "alcalde", "intendente",
    "gobernador", "embajador", "subsecretario", "secretario", "concejal",
    "consejero", "director", "rector", "decano", "juez", "fiscal",
    "contralor", "comandante", "general", "coronel", "almirante",
    "canciller", "vicepresidente"
  )
  is_keyword <- any(str_detect(label_norm, fixed(position_keywords)))
  is_section <- is_section_header(label_norm)
  # Prefer labels that look like titles (value empty or short)
  looks_like_title <- is.na(value_text) || value_text == "" || nchar(value_text) < 5
  is_keyword && !is_section && looks_like_title
}

label_to_person_field <- function(label_norm) {
  fields <- list(
    birth_date_raw = c("nacimiento", "fecha de nacimiento", "nació"),
    birth_place_raw = c("lugar de nacimiento"),
    death_date_raw = c("fallecimiento", "fecha de fallecimiento", "muerte"),
    death_place_raw = c("lugar de fallecimiento", "lugar de muerte"),
    nationality_raw = c("nacionalidad", "ciudadanía"),
    religion_raw = c("religión", "fe"),
    residence_raw = c("residencia", "domicilio"),
    language_raw = c("lengua materna", "idioma", "lengua")
  )
  for (f in names(fields)) {
    if (label_norm %in% fields[[f]]) return(f)
  }
  NA_character_
}

label_to_family_type <- function(label_norm) {
  mapping <- list(
    parent = c("padres", "padre", "madre"),
    mother = c("madre"),
    father = c("padre"),
    spouse = c("cónyuge", "conyuge", "esposo", "esposa", "pareja"),
    child = c("hijos", "hijo", "hija"),
    sibling = c("hermanos", "hermano", "hermana"),
    in_law = c("cuñado", "cuñada", "suegro", "suegra"),
    relative_unspecified = c("familia", "familiares", "parientes")
  )
  for (k in names(mapping)) {
    if (label_norm %in% mapping[[k]]) return(k)
  }
  NA_character_
}

label_to_education_type <- function(label_norm) {
  if (label_norm %in% c("educado en", "educación", "formación", "alma mater")) return("educated_in")
  if (label_norm %in% c("tesis", "tesis doctoral")) return("thesis")
  if (label_norm %in% c("posgrado", "doctorado", "maestría", "magíster")) return("postgraduate")
  NA_character_
}

label_to_affiliation_type <- function(label_norm) {
  if (label_norm %in% c("partido político", "partido", "militancia")) return("party")
  if (label_norm %in% c("miembro de", "afiliaciones", "pertenece a")) return("member_of")
  if (label_norm %in% c("ocupación", "profesión")) return("occupation")
  if (label_norm %in% c("distinciones", "premios")) return("distinction")
  NA_character_
}

extract_date_range <- function(text) {
  if (is.na(text) || text == "") {
    return(list(start = NA_character_, end = NA_character_))
  }
  parts <- str_split(text, "[-–—]|\\s+a\\s+|\\s+al\\s+")[[1]] %>% normalize_text()
  if (length(parts) == 1) {
    return(list(start = parts[1], end = NA_character_))
  }
  list(start = parts[1], end = parts[length(parts)])
}

infer_surnames <- function(canonical_name) {
  if (is.na(canonical_name) || canonical_name == "") {
    return(list(surname_1 = NA_character_, surname_2 = NA_character_))
  }
  tokens <- canonical_name %>%
    normalize_text() %>%
    str_replace_all("[^A-Za-zÁÉÍÓÚÑáéíóúñ\\s]", "") %>%
    str_split("\\s+") %>%
    unlist()
  tokens <- tokens[tokens != ""]
  particles <- c("de", "del", "la", "las", "los", "y", "da", "do", "dos", "van", "von")
  tokens_clean <- tokens[!str_to_lower(tokens) %in% particles]
  if (length(tokens_clean) == 0) {
    return(list(surname_1 = NA_character_, surname_2 = NA_character_))
  }
  surname_1 <- tokens_clean[length(tokens_clean)]
  surname_2 <- if (length(tokens_clean) >= 2) tokens_clean[length(tokens_clean) - 1] else NA_character_
  list(surname_1 = surname_1, surname_2 = surname_2)
}

# ----------------------------------------------------------------------------
# Core parser: ONE PERSON
# ----------------------------------------------------------------------------

parse_infobox_one <- function(infobox_rows, person_id = NULL, canonical_name = NULL) {
  if (is.null(infobox_rows)) stop("infobox_rows is NULL", call. = FALSE)
  if (!is.data.frame(infobox_rows)) infobox_rows <- as_tibble(infobox_rows)
  assert_columns(infobox_rows, c("label", "value_text"), "infobox_rows")

  if (is.null(person_id) || person_id == "") {
    person_id <- generate_person_id(canonical_name)
  }

  # Initialize tables with strict schemas
  person_tbl <- tibble(
    person_id = person_id,
    canonical_name = canonical_name %||% NA_character_,
    birth_date_raw = NA_character_,
    birth_place_raw = NA_character_,
    death_date_raw = NA_character_,
    death_place_raw = NA_character_,
    nationality_raw = NA_character_,
    religion_raw = NA_character_,
    residence_raw = NA_character_,
    language_raw = NA_character_,
    other_notes_raw = NA_character_
  )

  positions_tbl <- tibble(
    person_id = character(),
    position_title_raw = character(),
    jurisdiction_raw = character(),
    start_date_raw = character(),
    end_date_raw = character(),
    predecessor_raw = character(),
    successor_raw = character(),
    president_or_superior_raw = character(),
    vice_or_deputy_raw = character(),
    cabinet_or_body_raw = character(),
    source_label_raw = character(),
    block_id = character()
  )

  family_tbl <- tibble(
    person_id = character(),
    related_name_raw = character(),
    relation_type = character(),
    relation_detail_raw = character(),
    related_url = character()
  )

  education_tbl <- tibble(
    person_id = character(),
    education_type_raw = character(),
    institution_raw = character(),
    location_raw = character(),
    date_raw = character(),
    institution_url = character()
  )

  affiliations_tbl <- tibble(
    person_id = character(),
    affiliation_type_raw = character(),
    affiliation_raw = character(),
    start_date_raw = character(),
    end_date_raw = character(),
    affiliation_url = character()
  )

  links_tbl <- tibble(
    person_id = character(),
    context_table = character(),
    context_field = character(),
    entity_text = character(),
    url = character()
  )

  current_block <- NULL
  block_index <- 0

  for (i in seq_len(nrow(infobox_rows))) {
    label_raw <- infobox_rows$label[i] %||% ""
    value_text <- infobox_rows$value_text[i] %||% ""
    value_with_links <- if ("value_with_links" %in% names(infobox_rows)) {
      infobox_rows$value_with_links[i] %||% value_text
    } else {
      value_text
    }

    label_norm <- normalize_label(label_raw)

    if (is_section_header(label_norm)) {
      if (!is.null(current_block)) {
        positions_tbl <- bind_rows(positions_tbl, as_tibble(current_block))
        current_block <- NULL
      }
      next
    }

    if (is_position_start(label_norm, value_text)) {
      if (!is.null(current_block)) {
        positions_tbl <- bind_rows(positions_tbl, as_tibble(current_block))
      }
      block_index <- block_index + 1
      current_block <- list(
        person_id = person_id,
        position_title_raw = label_raw,
        jurisdiction_raw = NA_character_,
        start_date_raw = NA_character_,
        end_date_raw = NA_character_,
        predecessor_raw = NA_character_,
        successor_raw = NA_character_,
        president_or_superior_raw = NA_character_,
        vice_or_deputy_raw = NA_character_,
        cabinet_or_body_raw = NA_character_,
        source_label_raw = label_raw,
        block_id = paste0(person_id, "_pos_", block_index)
      )
      next
    }

    if (!is.null(current_block)) {
      if (label_norm == "detalle") {
        if (str_detect(value_text, "\\d{4}")) {
          rng <- extract_date_range(value_text)
          current_block$start_date_raw <- coalesce_chr(current_block$start_date_raw, rng$start)
          current_block$end_date_raw <- coalesce_chr(current_block$end_date_raw, rng$end)
        } else {
          current_block$cabinet_or_body_raw <- coalesce_chr(current_block$cabinet_or_body_raw, value_text)
        }
        next
      }
      if (label_norm %in% c("predecesor", "antecesor")) {
        current_block$predecessor_raw <- coalesce_chr(current_block$predecessor_raw, value_text)
        next
      }
      if (label_norm == "sucesor") {
        current_block$successor_raw <- coalesce_chr(current_block$successor_raw, value_text)
        next
      }
      if (label_norm %in% c("presidente", "superior", "jefe")) {
        current_block$president_or_superior_raw <- coalesce_chr(current_block$president_or_superior_raw, value_text)
        next
      }
      if (label_norm %in% c("vicepresidente", "vice", "subrogante")) {
        current_block$vice_or_deputy_raw <- coalesce_chr(current_block$vice_or_deputy_raw, value_text)
        next
      }
      if (label_norm %in% c("gabinete", "cámara", "senado", "congreso")) {
        current_block$cabinet_or_body_raw <- coalesce_chr(current_block$cabinet_or_body_raw, value_text)
        next
      }
    }

    if (is_noise_label(label_norm)) next

    person_field <- label_to_person_field(label_norm)
    if (!is.na(person_field)) {
      person_tbl[[person_field]][1] <- coalesce_chr(person_tbl[[person_field]][1], value_text)
      next
    }

    rel_type <- label_to_family_type(label_norm)
    if (!is.na(rel_type)) {
      links <- extract_links(value_with_links)
      if (nrow(links) > 0) {
        family_tbl <- bind_rows(family_tbl, tibble(
          person_id = person_id,
          related_name_raw = links$entity_text,
          relation_type = rel_type,
          relation_detail_raw = NA_character_,
          related_url = links$url
        ))
      } else {
        names <- split_names(value_text)
        if (length(names) > 0 && !all(map_lgl(names, is_count_only))) {
          family_tbl <- bind_rows(family_tbl, tibble(
            person_id = person_id,
            related_name_raw = names,
            relation_type = rel_type,
            relation_detail_raw = map_chr(names, extract_parenthetical_detail),
            related_url = NA_character_
          ))
        }
      }
      next
    }

    edu_type <- label_to_education_type(label_norm)
    if (!is.na(edu_type)) {
      links <- extract_links(value_with_links)
      if (nrow(links) > 0) {
        education_tbl <- bind_rows(education_tbl, tibble(
          person_id = person_id,
          education_type_raw = edu_type,
          institution_raw = links$entity_text,
          location_raw = NA_character_,
          date_raw = NA_character_,
          institution_url = links$url
        ))
      } else if (!is.na(value_text) && value_text != "") {
        education_tbl <- bind_rows(education_tbl, tibble(
          person_id = person_id,
          education_type_raw = edu_type,
          institution_raw = value_text,
          location_raw = NA_character_,
          date_raw = NA_character_,
          institution_url = NA_character_
        ))
      }
      next
    }

    aff_type <- label_to_affiliation_type(label_norm)
    if (!is.na(aff_type)) {
      links <- extract_links(value_with_links)
      if (nrow(links) > 0) {
        affiliations_tbl <- bind_rows(affiliations_tbl, tibble(
          person_id = person_id,
          affiliation_type_raw = aff_type,
          affiliation_raw = links$entity_text,
          start_date_raw = NA_character_,
          end_date_raw = NA_character_,
          affiliation_url = links$url
        ))
      } else if (!is.na(value_text) && value_text != "") {
        affiliations_tbl <- bind_rows(affiliations_tbl, tibble(
          person_id = person_id,
          affiliation_type_raw = aff_type,
          affiliation_raw = value_text,
          start_date_raw = NA_character_,
          end_date_raw = NA_character_,
          affiliation_url = NA_character_
        ))
      }
      next
    }

    if (!is.na(value_text) && value_text != "") {
      person_tbl$other_notes_raw[1] <- coalesce_chr(person_tbl$other_notes_raw[1], value_text)
    }

    links <- extract_links(value_with_links)
    if (nrow(links) > 0) {
      links_tbl <- bind_rows(links_tbl, tibble(
        person_id = person_id,
        context_table = "raw",
        context_field = label_raw,
        entity_text = links$entity_text,
        url = links$url
      ))
    }
  }

  if (!is.null(current_block)) {
    positions_tbl <- bind_rows(positions_tbl, as_tibble(current_block))
  }

  assert_columns(person_tbl, c(
    "person_id", "canonical_name", "birth_date_raw", "birth_place_raw",
    "death_date_raw", "death_place_raw", "nationality_raw", "religion_raw",
    "residence_raw", "language_raw", "other_notes_raw"
  ), "person")

  assert_columns(positions_tbl, c(
    "person_id", "position_title_raw", "jurisdiction_raw", "start_date_raw",
    "end_date_raw", "predecessor_raw", "successor_raw",
    "president_or_superior_raw", "vice_or_deputy_raw",
    "cabinet_or_body_raw", "source_label_raw", "block_id"
  ), "positions")

  assert_columns(family_tbl, c(
    "person_id", "related_name_raw", "relation_type", "relation_detail_raw",
    "related_url"
  ), "family_relations")

  assert_columns(education_tbl, c(
    "person_id", "education_type_raw", "institution_raw", "location_raw",
    "date_raw", "institution_url"
  ), "education")

  assert_columns(affiliations_tbl, c(
    "person_id", "affiliation_type_raw", "affiliation_raw",
    "start_date_raw", "end_date_raw", "affiliation_url"
  ), "affiliations")

  assert_columns(links_tbl, c(
    "person_id", "context_table", "context_field", "entity_text", "url"
  ), "links")

  list(
    person = person_tbl,
    positions = positions_tbl,
    family_relations = family_tbl,
    education = education_tbl,
    affiliations = affiliations_tbl,
    links = links_tbl
  )
}

# ----------------------------------------------------------------------------
# Batch parser: MANY people
# ----------------------------------------------------------------------------

parse_all_infoboxes <- function(infobox_json) {
  if (is.null(infobox_json)) stop("infobox_json is NULL", call. = FALSE)

  persons_all <- tibble()
  positions_all <- tibble()
  family_relations_all <- tibble()
  education_all <- tibble()
  affiliations_all <- tibble()
  links_all <- tibble()
  derived_all <- tibble()

  for (i in seq_along(infobox_json)) {
    rows <- infobox_json[[i]]
    if (is.null(rows)) next

    canonical_name <- NA_character_
    if (is.data.frame(rows) && nrow(rows) > 0) {
      first_label <- rows$label[1] %||% ""
      if (!is_section_header(normalize_label(first_label)) && nchar(first_label) > 2) {
        canonical_name <- normalize_text(first_label)
      }
    }

    person_id <- generate_person_id(canonical_name, fallback_index = i)
    parsed <- parse_infobox_one(rows, person_id = person_id, canonical_name = canonical_name)

    persons_all <- bind_rows(persons_all, parsed$person)
    positions_all <- bind_rows(positions_all, parsed$positions)
    family_relations_all <- bind_rows(family_relations_all, parsed$family_relations)
    education_all <- bind_rows(education_all, parsed$education)
    affiliations_all <- bind_rows(affiliations_all, parsed$affiliations)
    links_all <- bind_rows(links_all, parsed$links)
  }

  # Derived features
  derived_all <- persons_all %>%
    mutate(
      year_birth = map_int(birth_date_raw, safe_year),
      year_death = map_int(death_date_raw, safe_year),
      era_guess = map_chr(seq_len(n()), function(i) {
        if (!is.na(year_birth[i])) {
          era_guess_from_year(year_birth[i])
        } else if (!is.na(year_death[i])) {
          era_guess_from_year(year_death[i])
        } else {
          NA_character_
        }
      })
    ) %>%
    rowwise() %>%
    mutate(
      surnames = list(infer_surnames(canonical_name)),
      surname_1 = surnames$surname_1,
      surname_2 = surnames$surname_2
    ) %>%
    ungroup() %>%
    mutate(
      surname_signature = ifelse(!is.na(surname_1) & !is.na(surname_2),
                                 paste(surname_1, surname_2), surname_1)
    ) %>%
    dplyr::select(person_id, canonical_name, surname_1, surname_2, surname_signature, era_guess)

  positions_count <- positions_all %>%
    count(person_id, name = "positions_count")
  family_count <- family_relations_all %>%
    count(person_id, name = "family_edges_count")

  derived_all <- derived_all %>%
    left_join(positions_count, by = "person_id") %>%
    left_join(family_count, by = "person_id") %>%
    mutate(
      positions_count = coalesce(positions_count, 0L),
      family_edges_count = coalesce(family_edges_count, 0L),
      has_positions = positions_count > 0,
      has_family_edges = family_edges_count > 0
    ) %>%
    dplyr::select(
      person_id, canonical_name, surname_1, surname_2, surname_signature,
      era_guess, has_positions, positions_count, has_family_edges, family_edges_count
    )

  # Schema checks
  assert_columns(persons_all, c(
    "person_id", "canonical_name", "birth_date_raw", "birth_place_raw",
    "death_date_raw", "death_place_raw", "nationality_raw", "religion_raw",
    "residence_raw", "language_raw", "other_notes_raw"
  ), "persons_all")

  assert_columns(positions_all, c(
    "person_id", "position_title_raw", "jurisdiction_raw", "start_date_raw",
    "end_date_raw", "predecessor_raw", "successor_raw",
    "president_or_superior_raw", "vice_or_deputy_raw",
    "cabinet_or_body_raw", "source_label_raw", "block_id"
  ), "positions_all")

  assert_columns(family_relations_all, c(
    "person_id", "related_name_raw", "relation_type", "relation_detail_raw",
    "related_url"
  ), "family_relations_all")

  assert_columns(education_all, c(
    "person_id", "education_type_raw", "institution_raw", "location_raw",
    "date_raw", "institution_url"
  ), "education_all")

  assert_columns(affiliations_all, c(
    "person_id", "affiliation_type_raw", "affiliation_raw",
    "start_date_raw", "end_date_raw", "affiliation_url"
  ), "affiliations_all")

  assert_columns(links_all, c(
    "person_id", "context_table", "context_field", "entity_text", "url"
  ), "links_all")

  assert_columns(derived_all, c(
    "person_id", "canonical_name", "surname_1", "surname_2",
    "surname_signature", "era_guess", "has_positions", "positions_count",
    "has_family_edges", "family_edges_count"
  ), "derived_all")

  list(
    persons_all = persons_all,
    positions_all = positions_all,
    family_relations_all = family_relations_all,
    education_all = education_all,
    affiliations_all = affiliations_all,
    links_all = links_all,
    derived_all = derived_all
  )
}

# ----------------------------------------------------------------------------
# Final objects in global environment (only if infobox_json exists)
# ----------------------------------------------------------------------------

# Note: This block only runs if infobox_json is already in memory.
# For batch processing, use parse_all_infoboxes() directly with your data.
if (exists("infobox_json")) {
  parsed_all <- parse_all_infoboxes(infobox_json)
  persons_all <- parsed_all$persons_all
  positions_all <- parsed_all$positions_all
  family_relations_all <- parsed_all$family_relations_all
  education_all <- parsed_all$education_all
  affiliations_all <- parsed_all$affiliations_all
  links_all <- parsed_all$links_all
  derived_all <- parsed_all$derived_all
}

