# ============================================================================
# 02_model_and_analyze.R
# Network-first modeling of political elites and dynasties
# ============================================================================
# Input assumptions:
# - persons_all
# - positions_all
# - family_relations_all
# - education_all
# - affiliations_all
# - links_all
# - derived_all
# ============================================================================

library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(ggplot2)
library(igraph)
library(ggraph)
library(tidygraph)
library(broom)
library(MASS)

# ----------------------------------------------------------------------------
# Helpers
# ----------------------------------------------------------------------------

normalize_name_key <- function(x) {
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
  NA_integer_
}

position_type_from_title <- function(title) {
  title_norm <- str_to_lower(title %||% "")
  case_when(
    str_detect(title_norm, "presidente") ~ "presidente",
    str_detect(title_norm, "senador") ~ "senador",
    str_detect(title_norm, "diputado") ~ "diputado",
    str_detect(title_norm, "ministro") ~ "ministro",
    str_detect(title_norm, "alcalde") ~ "alcalde",
    str_detect(title_norm, "embajador") ~ "embajador",
    str_detect(title_norm, "gobernador") ~ "gobernador",
    TRUE ~ "otro"
  )
}

high_office_flag <- function(title) {
  title_norm <- str_to_lower(title %||% "")
  str_detect(title_norm, "presidente|ministro|senador|embajador")
}

scale_safe <- function(x) {
  if (all(is.na(x))) return(rep(NA_real_, length(x)))
  as.numeric(scale(x))
}

# ----------------------------------------------------------------------------
# PART A — Family network construction
# ----------------------------------------------------------------------------

persons_index <- persons_all %>%
  mutate(name_key = normalize_name_key(canonical_name)) %>%
  dplyr::select(person_id, canonical_name, name_key)

edges_family <- family_relations_all %>%
  filter(!is.na(person_id) & !is.na(related_name_raw) & related_name_raw != "") %>%
  mutate(
    related_name_key = normalize_name_key(related_name_raw),
    is_external_to = TRUE
  ) %>%
  left_join(persons_index, by = c("related_name_key" = "name_key"), relationship = "many-to-many") %>%
  mutate(
    related_person_id = person_id.y,
    is_external_to = is.na(related_person_id),
    from_id = person_id.x,
    to_id = ifelse(is_external_to, related_name_raw, related_person_id)
  ) %>%
  filter(!is.na(from_id) & !is.na(to_id) & from_id != "" & to_id != "") %>%
  dplyr::select(from_id, to_id, relation_type, relation_detail_raw, is_external_to)

nodes_internal <- derived_all %>%
  dplyr::select(node_id = person_id, canonical_name, surname_1, era_guess,
         positions_count, is_external = has_positions) %>%
  mutate(
    era_guess = as.character(era_guess),
    is_external = FALSE
  )

nodes_external <- edges_family %>%
  filter(is_external_to) %>%
  distinct(node_id = to_id) %>%
  mutate(
    canonical_name = node_id,
    surname_1 = NA_character_,
    era_guess = NA_character_,
    positions_count = 0L,
    is_external = TRUE
  )

nodes_family <- bind_rows(nodes_internal, nodes_external) %>%
  distinct(node_id, .keep_all = TRUE)

# Debug: check data
if (nrow(edges_family) == 0) {
  warning("edges_family is empty. No family relations found.", call. = FALSE)
}
if (nrow(nodes_family) == 0) {
  warning("nodes_family is empty. No nodes found.", call. = FALSE)
}

# ----------------------------------------------------------------------------
# PART B — Network visualizations
# ----------------------------------------------------------------------------

if (nrow(edges_family) > 0 && nrow(nodes_family) > 0) {
  g_family_tbl <- tbl_graph(nodes = nodes_family, edges = edges_family, directed = TRUE)
  g_family_igraph <- as.igraph(g_family_tbl)
  
  if (vcount(g_family_igraph) == 0) {
    warning("Family graph has no vertices. Creating empty graph objects.", call. = FALSE)
    g_family_igraph <- make_empty_graph(directed = TRUE)
    g_largest_tbl <- as_tbl_graph(g_family_igraph)
    centrality_tbl <- tibble(node_id = character(), degree = numeric(), 
                             betweenness = numeric(), closeness = numeric(), eigenvector = numeric())
  } else {
    components_tbl <- components(as.undirected(g_family_igraph))
    largest_component_id <- which.max(components_tbl$csize)
    keep_nodes <- names(components_tbl$membership[components_tbl$membership == largest_component_id])
    g_largest <- induced_subgraph(g_family_igraph, vids = keep_nodes)
    g_largest_tbl <- as_tbl_graph(g_largest)
    
    node_names <- V(g_family_igraph)$name
    centrality_tbl <- tibble(
      node_id = node_names,
      degree = degree(g_family_igraph, mode = "all"),
      betweenness = betweenness(g_family_igraph, directed = TRUE, normalized = TRUE),
      closeness = closeness(g_family_igraph, mode = "all", normalized = TRUE),
      eigenvector = eigen_centrality(g_family_igraph)$vector
    )
  }
} else {
  warning("Cannot create graph: missing edges or nodes. Creating empty objects.", call. = FALSE)
  g_family_igraph <- make_empty_graph(directed = TRUE)
  g_largest_tbl <- as_tbl_graph(g_family_igraph)
  centrality_tbl <- tibble(node_id = character(), degree = numeric(), 
                           betweenness = numeric(), closeness = numeric(), eigenvector = numeric())
}

  if (nrow(centrality_tbl) > 0 && "node_id" %in% names(centrality_tbl)) {
    top_labels <- centrality_tbl %>%
      arrange(desc(degree)) %>%
      slice(1:min(10, nrow(.))) %>%
      pull(node_id)
  } else {
    top_labels <- character()
  }

if (vcount(g_family_igraph) > 0 && nrow(centrality_tbl) > 0) {
  plot_full_family <- ggraph(g_largest_tbl, layout = "fr") +
    geom_edge_link(alpha = 0.2, colour = "gray60") +
    geom_node_point(aes(size = centrality_tbl$degree[match(name, centrality_tbl$node_id)]),
                    colour = "steelblue", alpha = 0.8) +
    geom_node_text(aes(label = ifelse(name %in% top_labels, name, "")),
                   size = 2, repel = TRUE) +
    scale_size_continuous(range = c(1, 6)) +
    ggtitle("Largest Component: Family Network") +
    theme_void()
  
  if (nrow(derived_all) > 0 && "surname_1" %in% names(derived_all)) {
    surname_top <- derived_all %>%
      filter(!is.na(surname_1)) %>%
      count(surname_1, sort = TRUE) %>%
      slice(1:min(12, nrow(.))) %>%
      pull(surname_1)
  } else {
    surname_top <- character()
  }
  
  if (length(surname_top) > 0) {
    plot_surname <- ggraph(g_largest_tbl, layout = "fr") +
    geom_edge_link(alpha = 0.2, colour = "gray70") +
    geom_node_point(aes(color = ifelse(surname_1 %in% surname_top, surname_1, "Other")), size = 2) +
      ggtitle("Family Network Colored by Surname (Top 12)") +
      theme_void() +
      theme(legend.position = "bottom")
  } else {
    plot_surname <- NULL
  }
  
  plot_era <- ggraph(g_largest_tbl, layout = "fr") +
    geom_edge_link(alpha = 0.15, colour = "gray70") +
    geom_node_point(aes(color = era_guess), size = 2) +
    ggtitle("Family Network by Era Guess") +
    theme_void() +
    theme(legend.position = "bottom")
  
  if (nrow(centrality_tbl) > 0 && "node_id" %in% names(centrality_tbl)) {
    ego_ids <- centrality_tbl %>%
      arrange(desc(degree)) %>%
      slice(1:min(3, nrow(.))) %>%
      pull(node_id)
    
    if (length(ego_ids) > 0) {
      plot_ego_list <- map(ego_ids, function(ego_id) {
    ego_graph <- make_ego_graph(g_family_igraph, order = 1, nodes = ego_id)[[1]]
    ego_tbl <- as_tbl_graph(ego_graph)
    ggraph(ego_tbl, layout = "fr") +
      geom_edge_link(alpha = 0.3, colour = "gray60") +
      geom_node_point(aes(color = name == ego_id), size = 3) +
      geom_node_text(aes(label = name), size = 2, repel = TRUE) +
        ggtitle(paste("Ego Network:", ego_id)) +
        theme_void()
      })
    } else {
      plot_ego_list <- list()
    }
  } else {
    plot_ego_list <- list()
  }
  
  if (vcount(g_family_igraph) > 0) {
    community <- cluster_louvain(as.undirected(g_family_igraph))
    nodes_family <- nodes_family %>%
      mutate(community = community$membership[match(node_id, V(g_family_igraph)$name)])
    g_comm <- tbl_graph(nodes = nodes_family, edges = edges_family, directed = TRUE)
    
    plot_community <- ggraph(g_comm, layout = "fr") +
      geom_edge_link(alpha = 0.15, colour = "gray70") +
      geom_node_point(aes(color = factor(community)), size = 2) +
      ggtitle("Community Structure (Louvain)") +
      theme_void() +
      theme(legend.position = "bottom")
  } else {
    plot_community <- NULL
  }

  if (exists("community") && "community" %in% names(nodes_family)) {
    community_summary <- nodes_family %>%
      filter(!is.na(community)) %>%
      count(community, surname_1, name = "n_members") %>%
      group_by(community) %>%
      summarise(
        community_size = sum(n_members, na.rm = TRUE),
        n_surnames = n_distinct(surname_1, na.rm = TRUE),
        top_surnames = paste(head(surname_1[order(-n_members)], 5), collapse = ", "),
        .groups = "drop"
      ) %>%
      arrange(desc(community_size))
  } else {
    community_summary <- tibble(
      community = integer(),
      community_size = integer(),
      n_surnames = integer(),
      top_surnames = character()
    )
  }
  
  # Surname-to-surname meta-network
  surname_edges <- edges_family %>%
    left_join(nodes_family %>% dplyr::select(node_id, surname_1), by = c("from_id" = "node_id")) %>%
    rename(from_surname = surname_1) %>%
    left_join(nodes_family %>% dplyr::select(node_id, surname_1), by = c("to_id" = "node_id")) %>%
    rename(to_surname = surname_1) %>%
    mutate(
      from_surname = coalesce(from_surname, "External"),
      to_surname = coalesce(to_surname, "External")
    ) %>%
    count(from_surname, to_surname, name = "weight") %>%
    filter(from_surname != "" & to_surname != "")
  
  if (nrow(surname_edges) > 0) {
    g_surname <- tbl_graph(nodes = tibble(name = unique(c(surname_edges$from_surname, surname_edges$to_surname))),
                           edges = surname_edges %>% rename(from = from_surname, to = to_surname),
                           directed = TRUE)
    
    plot_surname_meta <- ggraph(g_surname, layout = "fr") +
      geom_edge_link(aes(width = weight), alpha = 0.4, colour = "gray50") +
      geom_node_point(size = 3, colour = "darkred") +
      geom_node_text(aes(label = name), size = 3, repel = TRUE) +
      ggtitle("Surname-to-Surname Meta-Network") +
      theme_void()
  } else {
    plot_surname_meta <- NULL
  }

  surname_edges_interfamily <- surname_edges %>%
    filter(from_surname != to_surname) %>%
    arrange(desc(weight))

  surname_pair_edges <- derived_all %>%
    filter(!is.na(surname_1) & !is.na(surname_2)) %>%
    mutate(
      surname_1 = str_trim(surname_1),
      surname_2 = str_trim(surname_2)
    ) %>%
    filter(surname_1 != "" & surname_2 != "" & surname_1 != surname_2) %>%
    mutate(
      pair_a = pmin(surname_1, surname_2),
      pair_b = pmax(surname_1, surname_2)
    ) %>%
    count(pair_a, pair_b, name = "weight") %>%
    arrange(desc(weight))

  shared_family_people <- derived_all %>%
    filter(!is.na(surname_1) & !is.na(surname_2)) %>%
    mutate(
      surname_1 = str_trim(surname_1),
      surname_2 = str_trim(surname_2)
    ) %>%
    filter(surname_1 != "" & surname_2 != "" & surname_1 != surname_2) %>%
    arrange(surname_1, surname_2, canonical_name)
} else {
  plot_full_family <- NULL
  plot_surname <- NULL
  plot_era <- NULL
  plot_ego_list <- list()
  plot_community <- NULL
  plot_surname_meta <- NULL
  community_summary <- tibble(
    community = integer(),
    community_size = integer(),
    n_surnames = integer(),
    top_surnames = character()
  )
  surname_edges_interfamily <- tibble(
    from_surname = character(),
    to_surname = character(),
    weight = integer()
  )
  surname_pair_edges <- tibble(
    pair_a = character(),
    pair_b = character(),
    weight = integer()
  )
  shared_family_people <- tibble(
    person_id = character(),
    canonical_name = character(),
    surname_1 = character(),
    surname_2 = character(),
    surname_signature = character(),
    era_guess = character(),
    has_positions = logical(),
    positions_count = integer(),
    has_family_edges = logical(),
    family_edges_count = integer()
  )
}

# ----------------------------------------------------------------------------
# PART C — Network metrics and dynasty influence
# ----------------------------------------------------------------------------

if (vcount(g_family_igraph) > 0 && nrow(centrality_tbl) > 0 && "node_id" %in% names(centrality_tbl)) {
  component_membership <- components(as.undirected(g_family_igraph))
  if (exists("community")) {
    person_metrics <- centrality_tbl %>%
      mutate(
        component = component_membership$membership[match(node_id, names(component_membership$membership))],
        component_size = component_membership$csize[component],
        community = community$membership[match(node_id, names(community$membership))]
      ) %>%
      rename(person_id = node_id)
  } else {
    person_metrics <- centrality_tbl %>%
      mutate(
        component = component_membership$membership[match(node_id, names(component_membership$membership))],
        component_size = component_membership$csize[component],
        community = NA_integer_
      ) %>%
      rename(person_id = node_id)
  }
} else {
  if (nrow(centrality_tbl) > 0 && "node_id" %in% names(centrality_tbl)) {
    person_metrics <- centrality_tbl %>%
      mutate(component = NA_integer_, component_size = 0L, community = NA_integer_) %>%
      rename(person_id = node_id)
  } else {
    person_metrics <- tibble(person_id = character(), degree = numeric(), betweenness = numeric(),
                            closeness = numeric(), eigenvector = numeric(),
                            component = integer(), component_size = integer(), community = integer())
  }
}

surname_size <- derived_all %>%
  count(surname_1, name = "surname_size")

person_metrics <- person_metrics %>%
  left_join(derived_all, by = "person_id") %>%
  left_join(surname_size, by = "surname_1") %>%
  mutate(surname_size = coalesce(surname_size, 1L))

surname_metrics <- person_metrics %>%
  group_by(surname_1) %>%
  summarise(
    n_individuals = n(),
    total_positions = sum(positions_count, na.rm = TRUE),
    mean_degree = mean(degree, na.rm = TRUE),
    median_degree = median(degree, na.rm = TRUE),
    mean_betweenness = mean(betweenness, na.rm = TRUE),
    dynasty_score = mean(scale_safe(positions_count) + scale_safe(degree) + scale_safe(betweenness), na.rm = TRUE),
    .groups = "drop"
  )

top_brokers <- person_metrics %>%
  arrange(desc(betweenness)) %>%
  slice(1:20)

# ----------------------------------------------------------------------------
# PART D — Political careers as longitudinal object
# ----------------------------------------------------------------------------

positions_timeline <- positions_all %>%
  mutate(
    start_year = map_int(start_date_raw, safe_year),
    end_year = map_int(end_date_raw, safe_year),
    position_type = position_type_from_title(position_title_raw),
    career_span_years = ifelse(!is.na(start_year) & !is.na(end_year),
                               end_year - start_year, NA_integer_)
  )

positions_summary <- positions_timeline %>%
  group_by(person_id) %>%
  summarise(
    positions_count = n(),
    first_year = min(start_year, na.rm = TRUE),
    last_year = max(end_year, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    first_year = ifelse(is.infinite(first_year), NA_integer_, first_year),
    last_year = ifelse(is.infinite(last_year), NA_integer_, last_year)
  )

office_type_distribution <- positions_timeline %>%
  count(position_type, sort = TRUE)

# ----------------------------------------------------------------------------
# PART E — Statistical modeling (regressions)
# ----------------------------------------------------------------------------

model_df <- person_metrics %>%
  mutate(
    has_positions = positions_count > 0,
    high_office = person_id %in% positions_all$person_id[
      high_office_flag(positions_all$position_title_raw)
    ],
    education_present = person_id %in% education_all$person_id,
    nationality_present = !is.na(persons_all$nationality_raw[match(person_id, persons_all$person_id)])
  ) %>%
  dplyr::select(
    person_id, positions_count, has_positions, high_office,
    degree, betweenness, closeness, eigenvector,
    surname_size, era_guess, education_present, nationality_present
  ) %>%
  filter(!is.na(degree) & !is.na(betweenness))

# Only fit models if we have enough data
if (nrow(model_df) >= 10) {
  # Convert era_guess to factor only if it has 2+ levels
  era_levels <- length(unique(model_df$era_guess[!is.na(model_df$era_guess)]))
  use_era <- era_levels >= 2
  
  if (use_era) {
    model_df$era_guess <- as.factor(model_df$era_guess)
  }
  
  # Model 1: positions_count (negative binomial if possible)
  if (use_era) {
    model_count <- tryCatch(
      glm.nb(positions_count ~ degree + betweenness + surname_size + era_guess +
               education_present + nationality_present, data = model_df),
      error = function(e) {
        glm(positions_count ~ degree + betweenness + surname_size + era_guess +
              education_present + nationality_present,
            data = model_df, family = poisson())
      }
    )
  } else {
    model_count <- tryCatch(
      glm.nb(positions_count ~ degree + betweenness + surname_size +
               education_present + nationality_present, data = model_df),
      error = function(e) {
        glm(positions_count ~ degree + betweenness + surname_size +
              education_present + nationality_present,
            data = model_df, family = poisson())
      }
    )
  }
  
  # Model 2: has_positions (logit)
  if (use_era) {
    model_has_positions <- tryCatch(
      glm(has_positions ~ degree + betweenness + surname_size + era_guess +
            education_present + nationality_present,
          data = model_df, family = binomial()),
      error = function(e) NULL
    )
  } else {
    model_has_positions <- tryCatch(
      glm(has_positions ~ degree + betweenness + surname_size +
            education_present + nationality_present,
          data = model_df, family = binomial()),
      error = function(e) NULL
    )
  }
  
  # Model 3: high_office (logit)
  if (use_era) {
    model_high_office <- tryCatch(
      glm(high_office ~ degree + betweenness + surname_size + era_guess +
            education_present + nationality_present,
          data = model_df, family = binomial()),
      error = function(e) NULL
    )
  } else {
    model_high_office <- tryCatch(
      glm(high_office ~ degree + betweenness + surname_size +
            education_present + nationality_present,
          data = model_df, family = binomial()),
      error = function(e) NULL
    )
  }
  
  model_count_tidy <- if (!is.null(model_count)) tidy(model_count) else tibble()
  model_has_positions_tidy <- if (!is.null(model_has_positions)) tidy(model_has_positions) else tibble()
  model_high_office_tidy <- if (!is.null(model_high_office)) tidy(model_high_office) else tibble()
} else {
  warning("Insufficient data for regression models (n < 10). Skipping models.", call. = FALSE)
  model_count_tidy <- tibble()
  model_has_positions_tidy <- tibble()
  model_high_office_tidy <- tibble()
}

# ----------------------------------------------------------------------------
# PART F — Complementary network analyses
# ----------------------------------------------------------------------------

assortativity_surname <- {
  surname_vec <- nodes_family$surname_1
  surname_vec[is.na(surname_vec)] <- "External"
  assortativity_nominal(g_family_igraph, as.numeric(factor(surname_vec)), directed = TRUE)
}

mixing_matrix_surname <- {
  if (nrow(surname_size) > 0 && "surname_1" %in% names(surname_size)) {
    top_surnames <- surname_size %>% slice(1:min(10, nrow(.))) %>% pull(surname_1)
  } else {
    top_surnames <- character()
  }
  if (length(top_surnames) > 0 && nrow(edges_family) > 0 && "node_id" %in% names(nodes_family)) {
    edges_family %>%
      left_join(nodes_family %>% dplyr::select(node_id, surname_1), by = c("from_id" = "node_id")) %>%
      rename(from_surname = surname_1) %>%
      left_join(nodes_family %>% dplyr::select(node_id, surname_1), by = c("to_id" = "node_id")) %>%
    rename(to_surname = surname_1) %>%
      mutate(
        from_surname = ifelse(from_surname %in% top_surnames, from_surname, "Other"),
        to_surname = ifelse(to_surname %in% top_surnames, to_surname, "Other")
      ) %>%
      count(from_surname, to_surname)
  } else {
    tibble(from_surname = character(), to_surname = character(), n = integer())
  }
}

centrality_by_era <- person_metrics %>%
  filter(!is.na(era_guess)) %>%
  ggplot(aes(x = era_guess, y = degree)) +
  geom_boxplot(outlier.alpha = 0.2) +
  ggtitle("Centrality Distributions by Era") +
  theme_minimal()

# ----------------------------------------------------------------------------
# Objects created:
# - edges_family, nodes_family
# - plot_full_family, plot_surname, plot_era, plot_community, plot_surname_meta
# - plot_ego_list (length 3)
# - person_metrics, surname_metrics, top_brokers
# - positions_timeline, positions_summary, office_type_distribution
# - model_count_tidy, model_has_positions_tidy, model_high_office_tidy
# - assortativity_surname, mixing_matrix_surname, centrality_by_era
# ----------------------------------------------------------------------------

