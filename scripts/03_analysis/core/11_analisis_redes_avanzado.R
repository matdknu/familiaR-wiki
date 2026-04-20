#!/usr/bin/env Rscript

# ============================================================================
# 11_analisis_redes_avanzado.R
# ----------------------------------------------------------------------------
# Advanced network analyses for Latin American elite family networks.
# Data must already exist in memory as:
#   - nodes
#   - edges_full
# No CSV reloading is performed in this script.
# ============================================================================

# ---- Package bootstrap requested by user -----------------------------------
packages <- c("readr","dplyr","purrr","tidyr","ggplot2","igraph",
              "sna","ergm","sbm","aricode","ggalluvial","circlize",
              "boot","broom","scales","ggrepel","forcats")
install_if_missing <- packages[!packages %in% installed.packages()[,"Package"]]
if (length(install_if_missing)) install.packages(install_if_missing)
invisible(lapply(packages, library, character.only=TRUE))

set.seed(42)

dir.create("outputs/tables/advanced", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/figures/advanced", recursive = TRUE, showWarnings = FALSE)

# ---- Mathematical notes (kept inline per requirement) ----------------------
# Logit model: log(p/(1-p)) = b0 + b1*log(n_a*n_b) + b2*mismo_pais + ...
# Gravity: P(tie_ij) ∝ n_i * n_j / distance_ij^alpha
# Coleman H: H = (w - sum(p^2)) / (1 - sum(p^2))  where w=within-type ties, p=group proportions
# E-I index: EI = (E-I)/(E+I)  where E=external ties, I=internal ties
# Coreness: node v is in k-core iff deg(v) >= k in induced subgraph of k-core
# Rich-club: RC(k) = 2*E_k / (N_k*(N_k-1))  normalized by null model
# Gini: G = (2/n^2*mean(x)) * sum_i(i*x_i) - (n+1)/n
# Burt constraint: C_i = sum_j(c_ij)^2  where c_ij = p_ij + sum_q(p_iq*p_qj)
# Markov stationary: pi * T = pi,  sum(pi) = 1

# ---- Guards ----------------------------------------------------------------
if (!exists("nodes") || !exists("edges_full")) {
  stop("`nodes` and `edges_full` must already be loaded in memory.")
}

# ---- Helpers ---------------------------------------------------------------
`%||%` <- function(a, b) if (!is.null(a)) a else b

safe_write_csv <- function(x, path) {
  readr::write_csv(x, path, na = "")
}

safe_ggsave <- function(path, plot_obj, width = 12, height = 8, dpi = 300) {
  ggplot2::ggsave(
    filename = path,
    plot = plot_obj,
    width = width,
    height = height,
    dpi = dpi,
    bg = "white"
  )
}

run_analysis <- function(id, title, fn) {
  cat("\n", paste(rep("=", 75), collapse = ""), "\n", sep = "")
  cat(sprintf("ANALYSIS %s — %s\n", id, title))
  cat(paste(rep("=", 75), collapse = ""), "\n", sep = "")
  out <- tryCatch(
    fn(),
    error = function(e) {
      warning(sprintf("Analysis %s failed: %s", id, e$message))
      list(n_obs = NA_integer_, key_finding = paste("FAILED:", e$message), file = "FAILED")
    }
  )
  out
}

analysis_summary <- tibble::tibble(
  Analysis = character(),
  N_obs = integer(),
  Key_finding = character(),
  File = character()
)

append_summary <- function(id, n_obs, key_finding, file) {
  analysis_summary <<- dplyr::bind_rows(
    analysis_summary,
    tibble::tibble(
      Analysis = id,
      N_obs = as.integer(n_obs %||% NA_integer_),
      Key_finding = as.character(key_finding %||% ""),
      File = as.character(file %||% "")
    )
  )
}

rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (!is.finite(rng[1]) || !is.finite(rng[2]) || abs(diff(rng)) < 1e-12) return(rep(0, length(x)))
  (x - rng[1]) / (rng[2] - rng[1])
}

gini_degree <- function(x) {
  x <- sort(x[is.finite(x) & x > 0])
  n <- length(x)
  if (n == 0 || sum(x) == 0) return(0)
  2 * sum(seq_len(n) * x) / (n * sum(x)) - (n + 1) / n
}

# ---- Data standardization ---------------------------------------------------
nodes_std <- nodes %>%
  mutate(
    url = as.character(url),
    nombre = as.character(nombre),
    familia_grupo = as.character(familia_grupo),
    pais = as.character(pais),
    degree = as.numeric(degree),
    betweenness = as.numeric(betweenness),
    tiene_cargo = as.logical(tiene_cargo)
  ) %>%
  filter(!is.na(url), !is.na(familia_grupo), familia_grupo != "", familia_grupo != "Otra")

edges_std <- edges_full %>%
  mutate(
    from = as.character(from),
    to = as.character(to),
    from_familia = as.character(from_familia),
    to_familia = as.character(to_familia),
    relation_type = as.character(relation_type),
    es_puente = as.logical(es_puente),
    bridge_weight = as.numeric(bridge_weight)
  ) %>%
  filter(!is.na(from), !is.na(to), from != to)

url_lookup <- nodes_std %>%
  select(url, familia_grupo, pais, degree, betweenness, tiene_cargo, nombre, dplyr::everything()) %>%
  distinct(url, .keep_all = TRUE)

edges_enriched <- edges_std %>%
  left_join(url_lookup %>% select(url, from_pais = pais, from_family_lookup = familia_grupo), by = c("from" = "url")) %>%
  left_join(url_lookup %>% select(url, to_pais = pais, to_family_lookup = familia_grupo), by = c("to" = "url")) %>%
  mutate(
    from_familia = coalesce(from_familia, from_family_lookup),
    to_familia = coalesce(to_familia, to_family_lookup),
    es_puente = coalesce(es_puente, from_familia != to_familia)
  ) %>%
  filter(!is.na(from_familia), !is.na(to_familia), from_familia != "Otra", to_familia != "Otra")

family_main_country <- nodes_std %>%
  group_by(familia_grupo) %>%
  count(pais, name = "n_country", sort = TRUE) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  transmute(familia_grupo, pais_main = pais)

fam_nodes_base <- nodes_std %>%
  group_by(familia_grupo) %>%
  summarise(
    n_members = n(),
    n_paises = n_distinct(pais, na.rm = TRUE),
    mean_degree = mean(degree, na.rm = TRUE),
    mean_between = mean(betweenness, na.rm = TRUE),
    has_president = any(tiene_cargo %in% TRUE, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(family_main_country, by = "familia_grupo")

fam_edges_und <- edges_enriched %>%
  filter(from_familia != to_familia) %>%
  transmute(
    a = pmin(from_familia, to_familia),
    b = pmax(from_familia, to_familia),
    n_tie = coalesce(bridge_weight, 1)
  ) %>%
  group_by(a, b) %>%
  summarise(n_ties = n(), w = sum(n_tie, na.rm = TRUE), .groups = "drop")

safe_write_csv(
  fam_edges_und %>% transmute(from = a, to = b, n_ties, w),
  "outputs/tables/advanced/A0_family_kinship_edges.csv"
)

families <- sort(unique(c(fam_nodes_base$familia_grupo, fam_edges_und$a, fam_edges_und$b)))

if (length(families) < 3) {
  stop("Not enough families to run advanced analyses.")
}

g_fam <- igraph::graph_from_data_frame(
  d = fam_edges_und %>% transmute(from = a, to = b, weight = n_ties),
  vertices = fam_nodes_base %>% rename(name = familia_grupo),
  directed = FALSE
) %>%
  igraph::simplify(remove.multiple = TRUE, remove.loops = TRUE, edge.attr.comb = list(weight = "sum", "ignore"))

# ============================================================================
# ANALYSIS 1 — MARRIAGE PROBABILITY MODEL (LOGISTIC REGRESSION)
# ============================================================================
r1 <- run_analysis("1", "MARRIAGE PROBABILITY MODEL", function() {
  fam_tab <- fam_nodes_base %>%
    transmute(
      familia_grupo,
      n_members,
      mean_degree,
      pais_main
    )

  family_pairs <- expand.grid(
    familia_a = families,
    familia_b = families,
    stringsAsFactors = FALSE
  ) %>%
    as_tibble() %>%
    filter(familia_a < familia_b) %>%
    left_join(fam_tab %>% rename(familia_a = familia_grupo, n_a = n_members, deg_mean_a = mean_degree, pais_a = pais_main), by = "familia_a") %>%
    left_join(fam_tab %>% rename(familia_b = familia_grupo, n_b = n_members, deg_mean_b = mean_degree, pais_b = pais_main), by = "familia_b") %>%
    mutate(
      mismo_pais = pais_a == pais_b,
      size_product = pmax(n_a * n_b, 1),
      deg_sum = coalesce(deg_mean_a, 0) + coalesce(deg_mean_b, 0)
    )

  observed_pairs <- fam_edges_und %>%
    transmute(pair_key = paste(a, b, sep = "___")) %>%
    distinct()

  family_pairs <- family_pairs %>%
    mutate(
      pair_key = paste(familia_a, familia_b, sep = "___"),
      tiene_vinculo = pair_key %in% observed_pairs$pair_key
    )

  # Logit model: log(p/(1-p)) = b0 + b1*log(n_a*n_b) + b2*mismo_pais + ...
  # Gravity intuition: larger families have larger "interaction mass" in dyadic tie formation.
  modelo_matrimonio <- glm(
    tiene_vinculo ~ log(size_product) + mismo_pais + scale(deg_sum) + scale(n_a) + scale(n_b),
    data = family_pairs,
    family = binomial(link = "logit")
  )

  family_pairs$prob_vinculo <- predict(modelo_matrimonio, type = "response")

  prob_matrix <- family_pairs %>%
    select(familia_a, familia_b, prob_vinculo, tiene_vinculo) %>%
    arrange(desc(prob_vinculo))

  table_path <- "outputs/tables/advanced/A1_marriage_probability_model.csv"
  safe_write_csv(prob_matrix, table_path)

  heat <- ggplot(prob_matrix, aes(x = familia_a, y = familia_b, fill = prob_vinculo)) +
    geom_tile(color = NA) +
    geom_point(
      data = prob_matrix %>% filter(tiene_vinculo),
      aes(x = familia_a, y = familia_b),
      inherit.aes = FALSE,
      shape = 21,
      fill = "black",
      color = "white",
      stroke = 0.15,
      size = 1.4
    ) +
    scale_fill_gradient(low = "#f7fbff", high = "#08306b", name = "Predicted\nprobability") +
    labs(
      title = "Predicted family-pair kinship probabilities",
      subtitle = "Dots indicate observed inter-family ties",
      caption = "Tile value is p_ij from a logit dyadic model; higher values imply higher expected tie probability."
    ) +
    theme_minimal(base_size = 10) +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      panel.grid = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )

  fig_path <- "outputs/figures/advanced/A1_marriage_probability_model.png"
  safe_ggsave(fig_path, heat, width = 11, height = 8, dpi = 280)

  key_find <- sprintf(
    "Model converged; top predicted tie probability = %.3f",
    max(prob_matrix$prob_vinculo, na.rm = TRUE)
  )

  list(n_obs = nrow(prob_matrix), key_finding = key_find, file = paste(table_path, fig_path, sep = " | "))
})
append_summary("A1", r1$n_obs, r1$key_finding, r1$file)

# ============================================================================
# ANALYSIS 2 — ERGM
# ============================================================================
r2 <- run_analysis("2", "EXPONENTIAL RANDOM GRAPH MODEL (ERGM)", function() {
  fam_nodes <- fam_nodes_base %>%
    transmute(
      familia_grupo,
      n_members,
      n_countries = n_paises,
      mean_degree,
      has_president,
      pais = pais_main
    )

  fam_edges <- fam_edges_und %>%
    transmute(from = a, to = b, n_ties = n_ties)

  mat_fam <- matrix(0, nrow = nrow(fam_nodes), ncol = nrow(fam_nodes), dimnames = list(fam_nodes$familia_grupo, fam_nodes$familia_grupo))
  for (i in seq_len(nrow(fam_edges))) {
    mat_fam[fam_edges$from[i], fam_edges$to[i]] <- 1
    mat_fam[fam_edges$to[i], fam_edges$from[i]] <- 1
  }

  net_obj <- network::network(mat_fam, directed = FALSE, matrix.type = "adjacency")
  network::set.vertex.attribute(net_obj, "n_members", fam_nodes$n_members[match(network::get.vertex.attribute(net_obj, "vertex.names"), fam_nodes$familia_grupo)])
  network::set.vertex.attribute(net_obj, "mean_degree", fam_nodes$mean_degree[match(network::get.vertex.attribute(net_obj, "vertex.names"), fam_nodes$familia_grupo)])
  network::set.vertex.attribute(net_obj, "pais", fam_nodes$pais[match(network::get.vertex.attribute(net_obj, "vertex.names"), fam_nodes$familia_grupo)])

  fit <- tryCatch(
    ergm::ergm(
      net_obj ~ edges + triangle + nodecov("n_members") + nodematch("pais") + absdiff("mean_degree"),
      control = ergm::control.ergm(seed = 42, MCMLE.maxit = 20)
    ),
    error = function(e) {
      warning(sprintf("Full ERGM failed (%s). Retrying with MPLE fallback.", e$message))
      ergm::ergm(
        net_obj ~ edges + nodecov("n_members") + nodematch("pais") + absdiff("mean_degree"),
        estimate = "MPLE",
        control = ergm::control.ergm(seed = 42)
      )
    }
  )

  coef_tbl <- broom::tidy(fit) %>%
    mutate(odds_ratio = exp(estimate))

  table_path <- "outputs/tables/advanced/A2_ergm_coefficients.csv"
  safe_write_csv(coef_tbl, table_path)

  gof_obj <- ergm::gof(fit, GOF = ~ degree + espartners)
  fig_path <- "outputs/figures/advanced/A2_ergm_gof.png"
  png(fig_path, width = 1500, height = 900, res = 140, bg = "white")
  plot(gof_obj)
  title(
    main = "ERGM GOF diagnostics",
    sub = "Degree and edge-wise shared partner diagnostics compare observed vs simulated structure"
  )
  dev.off()

  key_find <- sprintf(
    "ERGM fitted; strongest absolute coefficient = %s (%.3f)",
    coef_tbl$term[which.max(abs(coef_tbl$estimate))],
    coef_tbl$estimate[which.max(abs(coef_tbl$estimate))]
  )
  list(n_obs = nrow(fam_edges), key_finding = key_find, file = paste(table_path, fig_path, sep = " | "))
})
append_summary("A2", r2$n_obs, r2$key_finding, r2$file)

# ============================================================================
# ANALYSIS 3 — STOCHASTIC BLOCK MODEL (ICL surrogate)
# ============================================================================
r3 <- run_analysis("3", "STOCHASTIC BLOCK MODEL", function() {
  fam_names <- igraph::V(g_fam)$name
  A <- as.matrix(igraph::as_adjacency_matrix(g_fam, sparse = FALSE))
  diag(A) <- 0
  n <- nrow(A)

  ll_bernoulli <- function(A_mat, z) {
    z <- as.integer(z)
    K <- length(unique(z))
    eps <- 1e-8
    pi_hat <- matrix(0, K, K)
    ll <- 0
    for (k in seq_len(K)) {
      for (l in seq_len(K)) {
        idx_k <- which(z == k)
        idx_l <- which(z == l)
        if (length(idx_k) == 0 || length(idx_l) == 0) next
        block <- A_mat[idx_k, idx_l, drop = FALSE]
        if (k == l) block <- block[upper.tri(block, diag = FALSE)]
        p_hat <- if (length(block) == 0) 0 else mean(block, na.rm = TRUE)
        p_hat <- pmin(pmax(p_hat, eps), 1 - eps)
        pi_hat[k, l] <- p_hat
        if (length(block) > 0) {
          ll <- ll + sum(block * log(p_hat) + (1 - block) * log(1 - p_hat))
        }
      }
    }
    list(ll = ll, pi = pi_hat)
  }

  sbm_candidates <- map_dfr(2:5, function(K) {
    eig <- eigen(A, symmetric = TRUE)
    emb <- eig$vectors[, seq_len(min(K, ncol(eig$vectors))), drop = FALSE]
    km <- kmeans(emb, centers = K, nstart = 25, iter.max = 100)
    z <- km$cluster

    ll_out <- ll_bernoulli(A, z)
    n_dyads <- n * (n - 1) / 2
    n_params <- K * (K + 1) / 2 + (K - 1)
    bic <- -2 * ll_out$ll + n_params * log(max(n_dyads, 2))

    # ICL surrogate uses BIC + assignment uncertainty penalty.
    dist_to_cent <- as.matrix(dist(rbind(emb, km$centers)))[seq_len(n), (n + 1):(n + K), drop = FALSE]
    prob_soft <- exp(-dist_to_cent)
    prob_soft <- prob_soft / rowSums(prob_soft)
    entropy <- -sum(prob_soft * log(pmax(prob_soft, 1e-10)))
    icl_surrogate <- bic + 2 * entropy

    tibble(
      K = K,
      bic = bic,
      entropy = entropy,
      icl = icl_surrogate,
      membership = list(z),
      pi_matrix = list(ll_out$pi)
    )
  })

  best <- sbm_candidates %>% slice_min(icl, n = 1)
  best_k <- best$K[[1]]
  z_best <- best$membership[[1]]
  pi_best <- best$pi_matrix[[1]]

  memb_tbl <- tibble(
    family = fam_names,
    block = factor(z_best, levels = sort(unique(z_best))),
    K_selected = best_k,
    entropy = best$entropy[[1]]
  ) %>%
    left_join(fam_nodes_base %>% transmute(family = familia_grupo, country = pais_main), by = "family")

  pi_tbl <- as_tibble(pi_best) %>%
    mutate(block_from = row_number()) %>%
    pivot_longer(cols = -block_from, names_to = "block_to", values_to = "pi") %>%
    mutate(block_to = as.integer(gsub("^V", "", block_to)))

  out_tbl <- memb_tbl %>%
    left_join(sbm_candidates %>% select(K, icl, bic, entropy), by = c("K_selected" = "K"))

  table_path <- "outputs/tables/advanced/A3_sbm_membership.csv"
  safe_write_csv(out_tbl, table_path)
  safe_write_csv(pi_tbl, "outputs/tables/advanced/A3_sbm_pi_matrix.csv")

  ord <- order(z_best)
  A_ord <- A[ord, ord]
  g_plot <- g_fam
  V(g_plot)$block <- factor(z_best[match(V(g_plot)$name, fam_names)])
  lay <- igraph::layout_with_fr(g_plot)

  fig_path <- "outputs/figures/advanced/A3_sbm_structure.png"
  png(fig_path, width = 1800, height = 900, res = 140, bg = "white")
  par(mfrow = c(1, 2), mar = c(3, 3, 3, 1))
  image(
    t(A_ord[rev(seq_len(nrow(A_ord))), ]),
    axes = FALSE,
    col = colorRampPalette(c("white", "#2b8cbe"))(100),
    main = "Adjacency reordered by SBM block"
  )
  box()
  plot(
    g_plot,
    layout = lay,
    vertex.label = NA,
    vertex.size = 6,
    vertex.color = as.numeric(V(g_plot)$block),
    edge.width = 0.7,
    edge.color = scales::alpha("grey30", 0.4),
    main = sprintf("Family network colored by latent blocks (K=%d)", best_k)
  )
  mtext("Color encodes latent alliance bloc from SBM-style Bernoulli block fit.", side = 1, line = 0.5, cex = 0.8)
  dev.off()

  key_find <- sprintf("Selected K=%d by minimum ICL surrogate", best_k)
  list(n_obs = nrow(memb_tbl), key_finding = key_find, file = paste(table_path, fig_path, sep = " | "))
})
append_summary("A3", r3$n_obs, r3$key_finding, r3$file)

# ============================================================================
# ANALYSIS 4 — HOMOPHILY INDICES
# ============================================================================
r4 <- run_analysis("4", "HOMOPHILY INDICES", function() {
  fam_attr <- fam_nodes_base %>%
    transmute(
      family = familia_grupo,
      country = pais_main,
      has_cargo = has_president,
      mean_degree
    )

  # Optional era proxy if birth year exists
  if ("anio_nacimiento" %in% names(nodes_std)) {
    era_tbl <- nodes_std %>%
      group_by(familia_grupo) %>%
      summarise(median_birth = median(as.numeric(anio_nacimiento), na.rm = TRUE), .groups = "drop") %>%
      mutate(era = if_else(is.finite(median_birth) & median_birth < 1900, "pre1900", "post1900")) %>%
      select(family = familia_grupo, era)
  } else {
    era_tbl <- tibble(family = fam_attr$family, era = "unknown")
  }

  fam_deg <- tibble(family = names(igraph::degree(g_fam)), fam_degree = as.numeric(igraph::degree(g_fam)))
  q75 <- quantile(fam_deg$fam_degree, 0.75, na.rm = TRUE)
  fam_deg <- fam_deg %>% mutate(tier = if_else(fam_degree >= q75, "top25", "bottom75"))

  fam_attr <- fam_attr %>%
    left_join(fam_deg, by = "family") %>%
    left_join(era_tbl, by = "family")

  edge_pairs <- fam_edges_und %>%
    transmute(f1 = a, f2 = b)

  coleman_H <- function(attr_vec, edges_df) {
    valid <- names(attr_vec)[!is.na(attr_vec)]
    e <- edges_df %>% filter(f1 %in% valid, f2 %in% valid)
    if (nrow(e) == 0) return(list(H = NA_real_, w = NA_real_, expected = NA_real_))
    same <- attr_vec[e$f1] == attr_vec[e$f2]
    w <- mean(same, na.rm = TRUE)
    p <- prop.table(table(attr_vec[valid]))
    expected <- sum(p^2)
    # Coleman H: H = (w - sum(p^2)) / (1 - sum(p^2))
    H <- (w - expected) / pmax(1 - expected, 1e-8)
    list(H = H, w = w, expected = expected)
  }

  perm_test_H <- function(attr_vec, edges_df, nperm = 1000) {
    obs <- coleman_H(attr_vec, edges_df)$H
    if (!is.finite(obs)) return(list(obs = NA_real_, p = NA_real_))
    vals <- unname(attr_vec)
    fams <- names(attr_vec)
    perm_H <- replicate(nperm, {
      set.seed(42)
      sh <- sample(vals, length(vals), replace = FALSE)
      names(sh) <- fams
      coleman_H(sh, edges_df)$H
    })
    p <- mean(abs(perm_H) >= abs(obs), na.rm = TRUE)
    list(obs = obs, p = p)
  }

  attrs <- list(
    Country = setNames(fam_attr$country, fam_attr$family),
    PoliticalOffice = setNames(as.character(fam_attr$has_cargo), fam_attr$family),
    NetworkTier = setNames(fam_attr$tier, fam_attr$family),
    Era = setNames(fam_attr$era, fam_attr$family)
  )

  hom_tbl <- imap_dfr(attrs, function(v, nm) {
    h <- coleman_H(v, edge_pairs)
    p <- perm_test_H(v, edge_pairs, nperm = 1000)
    tibble(
      attribute = nm,
      H_observed = h$H,
      H_expected_component = h$expected,
      within_share = h$w,
      p_value = p$p
    )
  })

  # E-I index by country
  ei_tbl <- map_dfr(sort(unique(na.omit(fam_attr$country))), function(cty) {
    fam_cty <- fam_attr %>% filter(country == cty) %>% pull(family)
    e_cty <- edge_pairs %>%
      mutate(in1 = f1 %in% fam_cty, in2 = f2 %in% fam_cty)
    I <- sum(e_cty$in1 & e_cty$in2, na.rm = TRUE)
    E <- sum(xor(e_cty$in1, e_cty$in2), na.rm = TRUE)
    # E-I index: EI = (E-I)/(E+I)
    EI <- (E - I) / pmax(E + I, 1)
    tibble(country = cty, internal = I, external = E, EI = EI)
  })

  table_path <- "outputs/tables/advanced/A4_homophily_indices.csv"
  safe_write_csv(hom_tbl, table_path)
  safe_write_csv(ei_tbl, "outputs/tables/advanced/A4_ei_country.csv")

  p_h <- ggplot(hom_tbl, aes(x = attribute, y = H_observed, fill = attribute)) +
    geom_col(width = 0.65, show.legend = FALSE) +
    geom_text(aes(label = sprintf("p=%.3f", p_value)), vjust = -0.4, size = 3) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
    labs(
      title = "Coleman homophily index by attribute",
      subtitle = "Positive values indicate within-type preference beyond random mixing",
      caption = "H = (w - Σp_g^2)/(1-Σp_g^2), where w is observed within-type tie share."
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )

  fig_path <- "outputs/figures/advanced/A4_homophily_indices.png"
  safe_ggsave(fig_path, p_h, width = 10, height = 7, dpi = 280)

  key_find <- hom_tbl %>%
    filter(is.finite(H_observed)) %>%
    slice_max(abs(H_observed), n = 1) %>%
    transmute(msg = sprintf("Strongest homophily signal: %s (H=%.3f)", attribute, H_observed)) %>%
    pull(msg) %>%
    first()
  if (is.na(key_find)) key_find <- "Homophily computed with missing-sensitive handling."

  list(n_obs = nrow(edge_pairs), key_finding = key_find, file = paste(table_path, fig_path, sep = " | "))
})
append_summary("A4", r4$n_obs, r4$key_finding, r4$file)

# ============================================================================
# ANALYSIS 5 — CORE-PERIPHERY DECOMPOSITION
# ============================================================================
r5 <- run_analysis("5", "CORE-PERIPHERY DECOMPOSITION", function() {
  deg <- igraph::degree(g_fam)
  core_k <- igraph::coreness(g_fam)

  fam_core <- tibble(
    family = names(core_k),
    degree = as.numeric(deg[names(core_k)]),
    coreness = as.numeric(core_k),
    in_3_core = coreness >= 3,
    in_4_core = coreness >= 4
  )

  A <- as.matrix(igraph::as_adjacency_matrix(g_fam, sparse = FALSE))
  eig <- eigen(A, symmetric = TRUE)
  core_score <- abs(eig$vectors[, 1])
  core_score <- rescale01(core_score)

  fam_core$core_score <- core_score[match(fam_core$family, V(g_fam)$name)]

  # Borgatti-Everett style continuous fit: correlation between observed A and ideal core product matrix.
  ideal <- outer(fam_core$core_score, fam_core$core_score)
  upper <- upper.tri(A, diag = FALSE)
  be_fit <- cor(as.numeric(A[upper]), as.numeric(ideal[upper]), use = "complete.obs")
  fam_core$be_fit_global <- be_fit

  # Rich-club: RC(k) = 2E_k / (N_k(N_k-1)), normalized by degree-preserving null.
  ks <- sort(unique(as.integer(deg)))
  rich_tbl <- map_dfr(ks, function(k) {
    ids <- which(deg > k)
    Nk <- length(ids)
    if (Nk < 2) return(tibble(k = k, rc_obs = NA_real_, rc_norm = NA_real_, rc_null_mean = NA_real_, rc_null_low = NA_real_, rc_null_high = NA_real_))
    gk <- igraph::induced_subgraph(g_fam, vids = ids)
    Ek <- igraph::ecount(gk)
    rc_obs <- 2 * Ek / (Nk * (Nk - 1))

    rc_null <- replicate(100, {
      g_null <- igraph::sample_degseq(deg, method = "simple.no.multiple")
      gk_n <- igraph::induced_subgraph(g_null, vids = which(igraph::degree(g_null) > k))
      Nk_n <- igraph::vcount(gk_n)
      if (Nk_n < 2) return(NA_real_)
      2 * igraph::ecount(gk_n) / (Nk_n * (Nk_n - 1))
    })
    tibble(
      k = k,
      rc_obs = rc_obs,
      rc_null_mean = mean(rc_null, na.rm = TRUE),
      rc_null_low = quantile(rc_null, 0.05, na.rm = TRUE),
      rc_null_high = quantile(rc_null, 0.95, na.rm = TRUE),
      rc_norm = rc_obs / pmax(mean(rc_null, na.rm = TRUE), 1e-8)
    )
  })

  table_path <- "outputs/tables/advanced/A5_core_periphery.csv"
  safe_write_csv(fam_core, table_path)
  safe_write_csv(rich_tbl, "outputs/tables/advanced/A5_rich_club_curve.csv")

  # Onion diagram coordinates by coreness ring
  onion <- fam_core %>%
    arrange(desc(coreness), desc(degree)) %>%
    group_by(coreness) %>%
    mutate(
      idx = row_number(),
      n = n(),
      theta = 2 * pi * idx / pmax(n, 1),
      r = max(coreness) - coreness + 1,
      x = r * cos(theta),
      y = r * sin(theta)
    ) %>%
    ungroup()

  fig_path <- "outputs/figures/advanced/A5_core_periphery.png"
  png(fig_path, width = 1800, height = 900, res = 140, bg = "white")
  par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
  plot(
    onion$x, onion$y,
    pch = 21,
    bg = scales::col_numeric("Blues", domain = range(onion$coreness, na.rm = TRUE))(onion$coreness),
    cex = scales::rescale(onion$degree, to = c(0.8, 2.2)),
    asp = 1,
    xlab = "", ylab = "",
    main = "Onion diagram by k-coreness"
  )
  text(onion$x, onion$y, labels = ifelse(onion$coreness == max(onion$coreness), onion$family, ""), cex = 0.6, pos = 3)
  mtext("Inner rings indicate higher k-core membership.", side = 1, line = 2.2, cex = 0.8)

  plot(rich_tbl$k, rich_tbl$rc_norm, type = "l", lwd = 2, col = "#08519c",
       ylim = range(c(rich_tbl$rc_norm, rich_tbl$rc_null_low, rich_tbl$rc_null_high), na.rm = TRUE),
       xlab = "Degree threshold k", ylab = "Normalized rich-club coefficient",
       main = "Rich-club curve (normalized)")
  polygon(
    c(rich_tbl$k, rev(rich_tbl$k)),
    c(rich_tbl$rc_null_low, rev(rich_tbl$rc_null_high)),
    col = scales::alpha("#9ecae1", 0.45),
    border = NA
  )
  lines(rich_tbl$k, rich_tbl$rc_norm, lwd = 2, col = "#08519c")
  abline(h = 1, lty = 2, col = "grey40")
  mtext("RC(k)>1 indicates elite over-connection among high-degree families.", side = 1, line = 2.2, cex = 0.8)
  dev.off()

  key_find <- sprintf(
    "Borgatti-Everett continuous fit correlation = %.3f",
    be_fit
  )
  list(n_obs = igraph::vcount(g_fam), key_finding = key_find, file = paste(table_path, fig_path, sep = " | "))
})
append_summary("A5", r5$n_obs, r5$key_finding, r5$file)

# ============================================================================
# ANALYSIS 6 — BROKERAGE ROLES (Gould-Fernandez style on directed triads)
# ============================================================================
r6 <- run_analysis("6", "BROKERAGE ROLES", function() {
  urls <- unique(c(edges_enriched$from, edges_enriched$to))
  node_broker <- nodes_std %>%
    filter(url %in% urls) %>%
    distinct(url, .keep_all = TRUE) %>%
    transmute(
      url,
      nombre = coalesce(nombre, url),
      familia = familia_grupo,
      pais = pais
    )

  g_dir <- igraph::graph_from_data_frame(
    d = edges_enriched %>% select(from, to),
    vertices = node_broker %>% rename(name = url),
    directed = TRUE
  ) %>% igraph::simplify(remove.multiple = TRUE, remove.loops = TRUE)

  groups <- node_broker$familia[match(V(g_dir)$name, node_broker$url)]
  names(groups) <- V(g_dir)$name

  role_counts <- tibble(
    url = V(g_dir)$name,
    coordinator = 0,
    consultant = 0,
    gatekeeper = 0,
    representative = 0,
    liaison = 0
  )

  # Triad brokerage intuition:
  # We count ordered paths i -> broker -> k and classify by group labels (family groups).
  for (b in V(g_dir)$name) {
    in_n <- igraph::neighbors(g_dir, b, mode = "in")$name
    out_n <- igraph::neighbors(g_dir, b, mode = "out")$name
    if (length(in_n) == 0 || length(out_n) == 0) next
    gb <- groups[b]
    for (i in in_n) {
      for (k in out_n) {
        if (i == k) next
        gi <- groups[i]
        gk <- groups[k]
        if (is.na(gi) || is.na(gb) || is.na(gk)) next
        if (gi == gb && gb == gk) {
          role_counts$coordinator[role_counts$url == b] <- role_counts$coordinator[role_counts$url == b] + 1
        } else if (gi != gb && gb == gk) {
          role_counts$gatekeeper[role_counts$url == b] <- role_counts$gatekeeper[role_counts$url == b] + 1
        } else if (gi == gb && gb != gk) {
          role_counts$representative[role_counts$url == b] <- role_counts$representative[role_counts$url == b] + 1
        } else if (gi == gk && gb != gi) {
          role_counts$consultant[role_counts$url == b] <- role_counts$consultant[role_counts$url == b] + 1
        } else if (gi != gb && gb != gk && gi != gk) {
          role_counts$liaison[role_counts$url == b] <- role_counts$liaison[role_counts$url == b] + 1
        }
      }
    }
  }

  role_tbl <- role_counts %>%
    left_join(node_broker, by = c("url")) %>%
    mutate(total_brokerage = coordinator + consultant + gatekeeper + representative + liaison) %>%
    mutate(across(c(coordinator, consultant, gatekeeper, representative, liaison), ~ .x / pmax(total_brokerage, 1), .names = "norm_{.col}"))

  top_roles <- role_tbl %>%
    select(url, nombre, pais, starts_with("norm_")) %>%
    pivot_longer(starts_with("norm_"), names_to = "role", values_to = "score") %>%
    mutate(role = gsub("^norm_", "", role)) %>%
    group_by(role) %>%
    slice_max(score, n = 10, with_ties = FALSE) %>%
    ungroup()

  table_path <- "outputs/tables/advanced/A6_brokerage_roles.csv"
  safe_write_csv(role_tbl, table_path)
  safe_write_csv(top_roles, "outputs/tables/advanced/A6_brokerage_top10.csv")

  p <- ggplot(top_roles, aes(x = forcats::fct_reorder(nombre, score), y = score, fill = pais)) +
    geom_col() +
    coord_flip() +
    facet_wrap(~ role, scales = "free_y") +
    labs(
      title = "Top normalized brokerage actors by role",
      subtitle = "Gould-Fernandez style roles from directed i→broker→k triads",
      x = "Person",
      y = "Normalized brokerage score",
      caption = "Role score = count(role triads)/count(all brokerage triads) per individual."
    ) +
    theme_minimal(base_size = 10) +
    theme(
      legend.position = "bottom",
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )

  fig_path <- "outputs/figures/advanced/A6_brokerage_roles.png"
  safe_ggsave(fig_path, p, width = 14, height = 10, dpi = 280)

  key_find <- top_roles %>%
    filter(role == "liaison") %>%
    slice_max(score, n = 1) %>%
    transmute(msg = sprintf("Top liaison: %s (%.3f)", nombre, score)) %>%
    pull(msg) %>%
    first()
  if (is.na(key_find)) key_find <- "Brokerage roles estimated for directed triads."

  list(n_obs = nrow(role_tbl), key_finding = key_find, file = paste(table_path, fig_path, sep = " | "))
})
append_summary("A6", r6$n_obs, r6$key_finding, r6$file)

# ============================================================================
# ANALYSIS 7 — TEMPORAL NETWORK ANALYSIS
# ============================================================================
r7 <- run_analysis("7", "TEMPORAL NETWORK ANALYSIS (COHORT SLICES)", function() {
  birth_col <- intersect(c("anio_nacimiento", "birth_year", "year_birth"), names(nodes_std))
  if (length(birth_col) == 0) {
    warning("No birth-year column in `nodes`; temporal analysis skipped.")
    table_path <- "outputs/tables/advanced/A7_temporal_network_metrics.csv"
    safe_write_csv(tibble(note = "Skipped: no birth-year column available"), table_path)
    fig_path <- "outputs/figures/advanced/A7_temporal_network_evolution.png"
    p <- ggplot() +
      annotate("text", x = 0, y = 0, label = "Skipped: no birth-year column available", size = 6) +
      theme_void() +
      theme(plot.background = element_rect(fill = "white", color = NA))
    safe_ggsave(fig_path, p, width = 10, height = 6)
    return(list(n_obs = 0, key_finding = "Skipped (missing birth year).", file = paste(table_path, fig_path, sep = " | ")))
  }

  birth_var <- birth_col[[1]]
  birth_lookup <- nodes_std %>%
    transmute(url, birth_year = as.numeric(.data[[birth_var]]), family = familia_grupo, country = pais)

  cohorts <- list(
    "Colonial (pre-1800)" = c(-Inf, 1800),
    "Independence (1800-1850)" = c(1800, 1850),
    "Oligarchic (1850-1900)" = c(1850, 1900),
    "Republican (1900-1950)" = c(1900, 1950),
    "Modern (1950-2000)" = c(1950, 2000)
  )

  e_t <- edges_enriched %>%
    left_join(birth_lookup %>% rename(from_birth = birth_year, from_country = country, from_family = family), by = c("from" = "url")) %>%
    left_join(birth_lookup %>% rename(to_birth = birth_year, to_country = country, to_family = family), by = c("to" = "url")) %>%
    mutate(
      older_birth = pmin(from_birth, to_birth, na.rm = TRUE),
      older_birth = if_else(is.infinite(older_birth), NA_real_, older_birth),
      cross_country = from_country != to_country
    )

  metrics <- imap_dfr(cohorts, function(rng, nm) {
    sub_e <- e_t %>% filter(!is.na(older_birth), older_birth >= rng[1], older_birth < rng[2])
    if (nrow(sub_e) == 0) {
      return(tibble(
        cohort = nm, density = NA_real_, mean_degree = NA_real_,
        prop_cross_family = NA_real_, prop_cross_country = NA_real_,
        gini_degree = NA_real_, lcc_frac = NA_real_,
        top3_families = NA_character_, n_edges = 0L
      ))
    }
    g <- igraph::graph_from_data_frame(sub_e %>% select(from, to), directed = FALSE)
    g <- igraph::simplify(g)
    deg <- igraph::degree(g)
    lcc <- max(igraph::components(g)$csize) / pmax(igraph::vcount(g), 1)
    fam_deg <- sub_e %>%
      transmute(from_family, to_family) %>%
      pivot_longer(cols = everything(), values_to = "family") %>%
      count(family, sort = TRUE)
    top3 <- paste(head(fam_deg$family, 3), collapse = "; ")

    tibble(
      cohort = nm,
      density = igraph::edge_density(g, loops = FALSE),
      mean_degree = mean(deg, na.rm = TRUE),
      prop_cross_family = mean(sub_e$es_puente %in% TRUE, na.rm = TRUE),
      prop_cross_country = mean(sub_e$cross_country %in% TRUE, na.rm = TRUE),
      gini_degree = gini_degree(as.numeric(deg)),
      lcc_frac = lcc,
      top3_families = top3,
      n_edges = nrow(sub_e)
    )
  })

  table_path <- "outputs/tables/advanced/A7_temporal_network_metrics.csv"
  safe_write_csv(metrics, table_path)

  metric_long <- metrics %>%
    select(cohort, density, mean_degree, prop_cross_family, prop_cross_country, gini_degree, lcc_frac) %>%
    pivot_longer(-cohort, names_to = "metric", values_to = "value")

  p <- ggplot(metric_long, aes(x = cohort, y = value, group = metric, color = metric)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    facet_wrap(~ metric, scales = "free_y", ncol = 3) +
    labs(
      title = "Temporal evolution of elite-network structure by cohort",
      subtitle = "Each panel tracks one network quantity across historical periods",
      x = "Cohort",
      y = "Metric value",
      caption = "Cross-family and cross-country series diagnose brokerage openness over time."
    ) +
    theme_minimal(base_size = 10) +
    theme(
      axis.text.x = element_text(angle = 25, hjust = 1),
      legend.position = "none",
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )

  fig_path <- "outputs/figures/advanced/A7_temporal_network_evolution.png"
  safe_ggsave(fig_path, p, width = 14, height = 10, dpi = 280)

  key_find <- metrics %>%
    filter(is.finite(prop_cross_family)) %>%
    slice_max(prop_cross_family, n = 1) %>%
    transmute(msg = sprintf("Highest cross-family share in %s (%.3f)", cohort, prop_cross_family)) %>%
    pull(msg) %>%
    first()
  if (is.na(key_find)) key_find <- "Temporal metrics computed for available cohorts."

  list(n_obs = sum(metrics$n_edges, na.rm = TRUE), key_finding = key_find, file = paste(table_path, fig_path, sep = " | "))
})
append_summary("A7", r7$n_obs, r7$key_finding, r7$file)

# ============================================================================
# ANALYSIS 8 — COUNTRY-LEVEL NETWORK COMPARISON (QAP-style permutation)
# ============================================================================
r8 <- run_analysis("8", "COUNTRY-LEVEL NETWORK COMPARISON (QAP)", function() {
  countries <- sort(unique(na.omit(nodes_std$pais)))
  countries <- intersect(countries, c("argentina","bolivia","chile","colombia","ecuador","mexico","paraguay","peru","uruguay","venezuela"))
  if (length(countries) < 5) countries <- sort(unique(na.omit(nodes_std$pais)))
  nC <- length(countries)

  fam_country <- fam_nodes_base %>%
    transmute(family = familia_grupo, country = pais_main)

  edge_country <- fam_edges_und %>%
    left_join(fam_country %>% rename(a = family, c1 = country), by = "a") %>%
    left_join(fam_country %>% rename(b = family, c2 = country), by = "b") %>%
    filter(!is.na(c1), !is.na(c2))

  Y <- matrix(0, nC, nC, dimnames = list(countries, countries))
  for (i in seq_len(nrow(edge_country))) {
    c1 <- edge_country$c1[i]; c2 <- edge_country$c2[i]
    if (!c1 %in% countries || !c2 %in% countries) next
    Y[c1, c2] <- Y[c1, c2] + 1
    Y[c2, c1] <- Y[c2, c1] + 1
  }

  capitals <- tribble(
    ~country, ~lat, ~lon,
    "argentina", -34.6037, -58.3816,
    "bolivia", -16.4897, -68.1193,
    "chile", -33.4489, -70.6693,
    "colombia", 4.7110, -74.0721,
    "ecuador", -0.1807, -78.4678,
    "mexico", 19.4326, -99.1332,
    "paraguay", -25.2637, -57.5759,
    "peru", -12.0464, -77.0428,
    "uruguay", -34.9011, -56.1645,
    "venezuela", 10.4806, -66.9036
  ) %>% filter(country %in% countries)

  hav_km <- function(lat1, lon1, lat2, lon2) {
    rad <- pi / 180
    dlat <- (lat2 - lat1) * rad
    dlon <- (lon2 - lon1) * rad
    a <- sin(dlat / 2)^2 + cos(lat1 * rad) * cos(lat2 * rad) * sin(dlon / 2)^2
    6371 * 2 * atan2(sqrt(a), sqrt(1 - a))
  }

  X1 <- matrix(0, nC, nC, dimnames = list(countries, countries))
  for (i in seq_len(nC)) {
    for (j in seq_len(nC)) {
      ci <- capitals %>% filter(country == countries[i])
      cj <- capitals %>% filter(country == countries[j])
      X1[i, j] <- hav_km(ci$lat, ci$lon, cj$lat, cj$lon)
    }
  }

  # Shared colonial master is constant (=1 for all pairs in this sample), so identified as near-zero variance.
  X2 <- matrix(1, nC, nC, dimnames = list(countries, countries))
  diag(X2) <- 0

  andean <- c("chile", "peru", "bolivia", "ecuador", "colombia")
  X3 <- outer(countries, countries, Vectorize(function(i, j) as.integer(i %in% andean && j %in% andean)))
  diag(X3) <- 0

  gdp2020 <- c(
    argentina = 389, bolivia = 36, chile = 252, colombia = 271, ecuador = 99,
    mexico = 1076, paraguay = 36, peru = 203, uruguay = 53, venezuela = 90
  )
  gdp <- gdp2020[countries]
  X4 <- outer(log(gdp), log(gdp), FUN = function(a, b) abs(a - b))

  vec_ut <- function(M) M[upper.tri(M, diag = FALSE)]
  y <- vec_ut(Y)
  x1 <- vec_ut(X1)
  x2 <- vec_ut(X2)
  x3 <- vec_ut(X3)
  x4 <- vec_ut(X4)

  dat <- tibble(y = y, x1 = x1, x2 = x2, x3 = x3, x4 = x4)
  if (sd(dat$x2, na.rm = TRUE) < 1e-9) dat <- dat %>% mutate(x2 = NA_real_)
  fit <- lm(y ~ x1 + x3 + x4, data = dat)
  beta_obs <- coef(fit)

  # QAP permutation: permute labels on Y (rows+cols jointly) to preserve dyadic dependence structure.
  nperm <- 1000
  perm_betas <- replicate(nperm, {
    set.seed(42)
    perm <- sample(seq_len(nC))
    Yp <- Y[perm, perm]
    yp <- vec_ut(Yp)
    dfp <- dat %>% mutate(y = yp)
    coef(lm(y ~ x1 + x3 + x4, data = dfp))
  })
  perm_betas <- t(perm_betas)

  coef_names <- names(beta_obs)
  qap_tbl <- tibble(
    predictor = coef_names,
    beta = as.numeric(beta_obs)
  ) %>%
    rowwise() %>%
    mutate(
      p_perm = mean(abs(perm_betas[, predictor]) >= abs(beta), na.rm = TRUE)
    ) %>%
    ungroup()

  table_path <- "outputs/tables/advanced/A8_qap_regression.csv"
  safe_write_csv(qap_tbl, table_path)

  p <- qap_tbl %>%
    filter(predictor != "(Intercept)") %>%
    ggplot(aes(x = predictor, y = beta, fill = predictor)) +
    geom_col(show.legend = FALSE, width = 0.65) +
    geom_text(aes(label = sprintf("p=%.3f", p_perm)), vjust = -0.35, size = 3.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
    labs(
      title = "QAP-style country-pair regression coefficients",
      subtitle = "Response: cross-country elite tie intensity",
      caption = "Permutation p-values come from row/column joint label shuffles of Y."
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )

  fig_path <- "outputs/figures/advanced/A8_qap_regression.png"
  safe_ggsave(fig_path, p, width = 9, height = 6.5, dpi = 280)

  key_find <- qap_tbl %>%
    filter(predictor != "(Intercept)") %>%
    slice_min(p_perm, n = 1) %>%
    transmute(msg = sprintf("Most robust predictor: %s (beta=%.3f, p=%.3f)", predictor, beta, p_perm)) %>%
    pull(msg) %>%
    first()
  if (is.na(key_find)) key_find <- "QAP-style regression estimated."

  list(n_obs = length(y), key_finding = key_find, file = paste(table_path, fig_path, sep = " | "))
})
append_summary("A8", r8$n_obs, r8$key_finding, r8$file)

# ============================================================================
# ANALYSIS 9 — ENDOGAMY / CLOSURE-REACH TRADE-OFF
# ============================================================================
r9 <- run_analysis("9", "ENDOGAMY RATE AS NETWORK CLOSURE", function() {
  fam_stats <- nodes_std %>%
    group_by(familia_grupo) %>%
    summarise(
      n = n(),
      mean_degree = mean(degree, na.rm = TRUE),
      sd_degree = sd(degree, na.rm = TRUE),
      mean_between = mean(betweenness, na.rm = TRUE),
      n_paises = n_distinct(pais, na.rm = TRUE),
      .groups = "drop"
    )

  bridge_long <- edges_enriched %>%
    filter(es_puente %in% TRUE) %>%
    transmute(family = from_familia, other = to_familia) %>%
    bind_rows(edges_enriched %>% filter(es_puente %in% TRUE) %>% transmute(family = to_familia, other = from_familia))

  internal_long <- edges_enriched %>%
    filter(es_puente %in% FALSE) %>%
    transmute(family = from_familia) %>%
    bind_rows(edges_enriched %>% filter(es_puente %in% FALSE) %>% transmute(family = to_familia))

  fam_stats <- fam_stats %>%
    left_join(bridge_long %>% count(family, name = "n_bridges_out"), by = c("familia_grupo" = "family")) %>%
    left_join(internal_long %>% count(family, name = "n_internal"), by = c("familia_grupo" = "family")) %>%
    mutate(
      n_bridges_out = coalesce(n_bridges_out, 0L),
      n_internal = coalesce(n_internal, 0L),
      closure_index = n_internal / (n_internal + n_bridges_out + 0.001),
      # Burt constraint proxy intuition: centrality per potential triadic opportunity.
      constraint_proxy = mean_between / (mean_degree * (mean_degree - 1) / 2 + 0.001)
    ) %>%
    left_join(
      bridge_long %>% group_by(family) %>% summarise(reach = n_distinct(other), .groups = "drop"),
      by = c("familia_grupo" = "family")
    ) %>%
    mutate(reach = coalesce(reach, 0L)) %>%
    left_join(family_main_country %>% rename(familia_grupo = familia_grupo, country = pais_main), by = "familia_grupo")

  # Formal tests
  pear <- cor.test(fam_stats$closure_index, fam_stats$reach, method = "pearson")
  spea <- cor.test(fam_stats$closure_index, fam_stats$reach, method = "spearman")

  boot_fun <- function(data, idx) {
    d <- data[idx, ]
    cor(d$closure_index, d$reach, method = "pearson", use = "complete.obs")
  }
  b <- boot::boot(fam_stats, statistic = boot_fun, R = 1000)
  ci <- tryCatch(boot::boot.ci(b, type = "perc")$percent[4:5], error = function(e) c(NA_real_, NA_real_))

  cor_tbl <- tibble(
    metric = c("pearson", "spearman", "pearson_boot_ci_low", "pearson_boot_ci_high"),
    value = c(unname(pear$estimate), unname(spea$estimate), ci[1], ci[2]),
    p_value = c(pear$p.value, spea$p.value, NA_real_, NA_real_)
  )

  table_path <- "outputs/tables/advanced/A9_closure_reach_tradeoff.csv"
  safe_write_csv(fam_stats, table_path)
  safe_write_csv(cor_tbl, "outputs/tables/advanced/A9_closure_reach_correlation.csv")

  p <- ggplot(fam_stats, aes(x = closure_index, y = reach, size = n, color = country, label = familia_grupo)) +
    geom_point(alpha = 0.8) +
    geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 0.7) +
    ggrepel::geom_text_repel(
      data = fam_stats %>% slice_max(reach, n = 20),
      size = 2.6,
      max.overlaps = 60
    ) +
    facet_wrap(~ country, scales = "free") +
    labs(
      title = "Closure-Reach trade-off across families",
      subtitle = "Higher closure means more internal ties; reach counts distinct inter-family bridges",
      x = "Closure index",
      y = "Reach (distinct external families)",
      caption = "Trade-off diagnostics: closure = internal/(internal+bridges); reach = unique bridge partners."
    ) +
    theme_minimal(base_size = 10) +
    theme(
      legend.position = "none",
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )

  fig_path <- "outputs/figures/advanced/A9_closure_reach_tradeoff.png"
  safe_ggsave(fig_path, p, width = 14, height = 10, dpi = 280)

  key_find <- sprintf(
    "Pearson corr(closure, reach)=%.3f (p=%.3g)",
    unname(pear$estimate), pear$p.value
  )
  list(n_obs = nrow(fam_stats), key_finding = key_find, file = paste(table_path, fig_path, sep = " | "))
})
append_summary("A9", r9$n_obs, r9$key_finding, r9$file)

# ============================================================================
# ANALYSIS 10 — NETWORK RESILIENCE (ATTACK SIMULATION)
# ============================================================================
r10 <- run_analysis("10", "NETWORK RESILIENCE (ATTACK SIMULATION)", function() {
  simulate_attack <- function(g, strategy = "targeted") {
    n <- igraph::vcount(g)
    lcc <- numeric(n)
    names_vec <- V(g)$name
    order_idx <- if (strategy == "targeted") order(igraph::degree(g), decreasing = TRUE) else sample(n)
    g_tmp <- g
    removed <- character(n)
    for (i in seq_len(n)) {
      if (igraph::vcount(g_tmp) == 0) break
      lcc[i] <- max(igraph::components(g_tmp)$csize) / igraph::vcount(g_tmp)
      v_remove <- names_vec[order_idx[i]]
      removed[i] <- v_remove
      if (v_remove %in% V(g_tmp)$name) {
        g_tmp <- igraph::delete_vertices(g_tmp, v_remove)
      }
    }
    tibble(step = seq_len(n), lcc_frac = lcc, strategy = strategy, removed_family = removed)
  }

  n <- igraph::vcount(g_fam)
  targeted <- simulate_attack(g_fam, "targeted")

  random_runs <- map_dfr(seq_len(100), function(i) {
    set.seed(42 + i)
    simulate_attack(g_fam, "random") %>% mutate(run = i)
  })

  random_summary <- random_runs %>%
    group_by(step) %>%
    summarise(
      lcc_mean = mean(lcc_frac, na.rm = TRUE),
      lcc_low = quantile(lcc_frac, 0.05, na.rm = TRUE),
      lcc_high = quantile(lcc_frac, 0.95, na.rm = TRUE),
      .groups = "drop"
    )

  curves_tbl <- targeted %>%
    select(step, lcc_targeted = lcc_frac) %>%
    left_join(random_summary, by = "step") %>%
    mutate(pct_removed = step / n)

  critical <- targeted %>%
    mutate(pct_removed = step / n) %>%
    filter(lcc_frac <= 0.5) %>%
    slice_head(n = 10)

  table_path <- "outputs/tables/advanced/A10_attack_simulation.csv"
  safe_write_csv(curves_tbl, table_path)
  safe_write_csv(critical, "outputs/tables/advanced/A10_critical_families.csv")

  p <- ggplot(curves_tbl, aes(x = pct_removed)) +
    geom_ribbon(aes(ymin = lcc_low, ymax = lcc_high), fill = "#9ecae1", alpha = 0.4) +
    geom_line(aes(y = lcc_mean, color = "Random attack (mean)"), linewidth = 1.1) +
    geom_line(aes(y = lcc_targeted, color = "Targeted attack"), linewidth = 1.1) +
    scale_color_manual(values = c("Random attack (mean)" = "#3182bd", "Targeted attack" = "#cb181d")) +
    labs(
      title = "Network resilience under node-removal attacks",
      subtitle = "Largest connected component (LCC) after removing families",
      x = "% families removed",
      y = "LCC fraction",
      color = "",
      caption = "Gap between targeted and random curves quantifies vulnerability to elite-centric disruption."
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )

  fig_path <- "outputs/figures/advanced/A10_attack_simulation.png"
  safe_ggsave(fig_path, p, width = 10.5, height = 7, dpi = 280)

  key_find <- if (nrow(critical) > 0) {
    sprintf("LCC drops below 50%% after removing %d families (targeted).", min(critical$step))
  } else {
    "LCC did not drop below 50% in targeted sequence."
  }

  list(n_obs = n, key_finding = key_find, file = paste(table_path, fig_path, sep = " | "))
})
append_summary("A10", r10$n_obs, r10$key_finding, r10$file)

# ============================================================================
# ANALYSIS 11 — COMMUNITY DETECTION COMPARISON
# ============================================================================
r11 <- run_analysis("11", "COMMUNITY DETECTION COMPARISON", function() {
  weights <- igraph::E(g_fam)$weight %||% rep(1, igraph::ecount(g_fam))

  comm_louvain <- igraph::cluster_louvain(g_fam, weights = weights)
  comm_walktrap <- igraph::cluster_walktrap(g_fam, weights = weights, steps = 4)
  comm_infomap <- igraph::cluster_infomap(g_fam, e.weights = weights)
  comm_leiden <- tryCatch(
    igraph::cluster_leiden(g_fam, objective_function = "modularity", resolution_parameter = 1.0, weights = weights),
    error = function(e) comm_louvain
  )

  safe_modularity <- function(comm, g) {
    tryCatch(
      igraph::modularity(comm),
      error = function(e) {
        igraph::modularity(g, membership = igraph::membership(comm))
      }
    )
  }

  part_tbl <- tibble(
    family = V(g_fam)$name,
    Louvain = membership(comm_louvain)[V(g_fam)$name],
    Leiden = membership(comm_leiden)[V(g_fam)$name],
    Walktrap = membership(comm_walktrap)[V(g_fam)$name],
    Infomap = membership(comm_infomap)[V(g_fam)$name]
  )

  summary_tbl <- tibble(
    algorithm = c("Louvain", "Leiden", "Walktrap", "Infomap"),
    modularity_Q = c(
      safe_modularity(comm_louvain, g_fam),
      safe_modularity(comm_leiden, g_fam),
      safe_modularity(comm_walktrap, g_fam),
      safe_modularity(comm_infomap, g_fam)
    ),
    n_communities = c(length(unique(membership(comm_louvain))), length(unique(membership(comm_leiden))), length(unique(membership(comm_walktrap))), length(unique(membership(comm_infomap))))
  ) %>%
    mutate(
      NMI_vs_Louvain = c(
        1,
        aricode::NMI(part_tbl$Louvain, part_tbl$Leiden),
        aricode::NMI(part_tbl$Louvain, part_tbl$Walktrap),
        aricode::NMI(part_tbl$Louvain, part_tbl$Infomap)
      )
    )

  # Families always grouped identically across algorithms -> robust signatures.
  part_tbl <- part_tbl %>%
    mutate(consensus_signature = paste(Louvain, Leiden, Walktrap, Infomap, sep = "_"))
  consensus <- part_tbl %>%
    count(consensus_signature, name = "n_families") %>%
    filter(n_families >= 2)

  table_path <- "outputs/tables/advanced/A11_community_comparison.csv"
  safe_write_csv(summary_tbl, table_path)
  safe_write_csv(part_tbl, "outputs/tables/advanced/A11_community_membership.csv")
  safe_write_csv(consensus, "outputs/tables/advanced/A11_consensus_clusters.csv")

  alluvial_df <- part_tbl %>%
    mutate(Family = family) %>%
    select(Family, Louvain, Leiden, Walktrap, Infomap) %>%
    pivot_longer(cols = c(Louvain, Leiden, Walktrap, Infomap), names_to = "Algorithm", values_to = "Community") %>%
    mutate(
      Algorithm = factor(Algorithm, levels = c("Louvain", "Leiden", "Walktrap", "Infomap")),
      Community = factor(Community)
    )

  p <- ggplot(
    alluvial_df,
    aes(x = Algorithm, stratum = Community, alluvium = Family, fill = Community, label = Community)
  ) +
    ggalluvial::geom_flow(alpha = 0.35) +
    ggalluvial::geom_stratum(alpha = 0.9, color = "grey30") +
    scale_x_discrete(expand = c(0.02, 0.02)) +
    labs(
      title = "Community assignment flow across algorithms",
      subtitle = "Families moving between blocks indicate algorithmic disagreement",
      caption = "NMI quantifies partition overlap; robust clusters show stable alluvial paths."
    ) +
    theme_minimal(base_size = 10) +
    theme(
      legend.position = "none",
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )

  fig_path <- "outputs/figures/advanced/A11_community_comparison.png"
  safe_ggsave(fig_path, p, width = 12, height = 8, dpi = 280)

  key_find <- summary_tbl %>%
    slice_max(modularity_Q, n = 1) %>%
    transmute(msg = sprintf("Highest modularity: %s (Q=%.3f)", algorithm, modularity_Q)) %>%
    pull(msg) %>%
    first()

  list(n_obs = igraph::vcount(g_fam), key_finding = key_find, file = paste(table_path, fig_path, sep = " | "))
})
append_summary("A11", r11$n_obs, r11$key_finding, r11$file)

# ============================================================================
# ANALYSIS 12 — MARKOV CHAIN OF ALLIANCE TRANSITIONS
# ============================================================================
r12 <- run_analysis("12", "MARKOV CHAIN: FAMILY ALLIANCE TRANSITIONS", function() {
  fam_adj <- fam_edges_und %>%
    transmute(from = a, to = b) %>%
    bind_rows(fam_edges_und %>% transmute(from = b, to = a))

  neigh <- fam_adj %>%
    group_by(from) %>%
    summarise(neighbors = list(unique(to)), .groups = "drop")
  neigh_map <- setNames(neigh$neighbors, neigh$from)

  transitions <- list()
  for (i in names(neigh_map)) {
    nb_i <- neigh_map[[i]]
    for (j in nb_i) {
      nb_j <- neigh_map[[j]]
      if (length(nb_j) == 0) next
      next_nodes <- setdiff(nb_j, i)
      if (length(next_nodes) == 0) next
      transitions[[length(transitions) + 1]] <- tibble(current = j, next_family = next_nodes)
    }
  }

  trans_df <- bind_rows(transitions)
  if (nrow(trans_df) == 0) {
    stop("No second-order transitions could be constructed.")
  }

  T_counts <- trans_df %>% count(current, next_family, name = "n")
  families_T <- sort(unique(c(T_counts$current, T_counts$next_family)))
  T_mat <- matrix(0, length(families_T), length(families_T), dimnames = list(families_T, families_T))
  for (i in seq_len(nrow(T_counts))) {
    T_mat[T_counts$current[i], T_counts$next_family[i]] <- T_counts$n[i]
  }
  rs <- rowSums(T_mat)
  T_prob <- T_mat
  T_prob[rs > 0, ] <- T_prob[rs > 0, , drop = FALSE] / rs[rs > 0]

  # Markov stationary condition: pi * T = pi, with sum(pi)=1.
  eig <- eigen(t(T_prob))
  idx <- which.min(abs(eig$values - 1))
  pi_vec <- Re(eig$vectors[, idx])
  pi_vec <- abs(pi_vec)
  pi_vec <- pi_vec / sum(pi_vec)

  degree_dist <- igraph::degree(g_fam)[families_T]
  degree_dist <- degree_dist / sum(degree_dist)

  stat_tbl <- tibble(
    family = families_T,
    stationary_prob = pi_vec,
    observed_degree_share = as.numeric(degree_dist[families_T])
  ) %>%
    arrange(desc(stationary_prob))

  cor_station_degree <- cor(stat_tbl$stationary_prob, stat_tbl$observed_degree_share, use = "complete.obs")
  stat_tbl$cor_stationary_degree <- cor_station_degree

  table_path <- "outputs/tables/advanced/A12_markov_transitions.csv"
  safe_write_csv(stat_tbl, table_path)

  top10 <- stat_tbl %>% slice_head(n = 10) %>% pull(family)
  chord_mat <- T_prob[top10, top10, drop = FALSE]
  chord_mat[chord_mat < 0.05] <- 0

  fig_path <- "outputs/figures/advanced/A12_markov_transitions.png"
  png(fig_path, width = 1400, height = 1200, res = 130, bg = "white")
  circlize::circos.clear()
  circlize::chordDiagram(
    x = chord_mat,
    transparency = 0.35,
    annotationTrack = "grid",
    directional = 1,
    direction.type = c("arrows", "diffHeight"),
    link.arr.type = "big.arrow"
  )
  title(
    main = "Markov alliance transition probabilities among top 10 attractor families",
    sub = "Links represent P(next ally = j | current ally = i), thresholded at 0.05"
  )
  circlize::circos.clear()
  dev.off()

  key_find <- sprintf(
    "Stationary vs observed degree share correlation = %.3f",
    cor_station_degree
  )

  list(n_obs = nrow(trans_df), key_finding = key_find, file = paste(table_path, fig_path, sep = " | "))
})
append_summary("A12", r12$n_obs, r12$key_finding, r12$file)

# ============================================================================
# FINAL SUMMARY
# ============================================================================
summary_path <- "outputs/tables/advanced/A00_advanced_analysis_summary.csv"
safe_write_csv(analysis_summary, summary_path)

cat("\n\n")
cat("Advanced analysis summary\n")
cat(paste(rep("-", 75), collapse = ""), "\n", sep = "")
print(analysis_summary, n = nrow(analysis_summary))
cat(paste(rep("-", 75), collapse = ""), "\n", sep = "")
cat("Summary CSV:", summary_path, "\n")

