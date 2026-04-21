#!/usr/bin/env Rscript
# H5 manuscript figures — sources: outputs/tables/advanced A2–A10 (do not edit CSVs).
# Run from repository root: Rscript paper/figures/build_h5_figures.R

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(ggplot2)
  library(igraph)
  library(ggrepel)
  library(stringr)
})

if (!file.exists("outputs/tables/advanced/A2_ergm_coefficients.csv")) {
  stop("Run this script from the wiki-chile_project repository root.")
}

.root <- getwd()
source(file.path(.root, "paper/figures/theme_paper.R"))

adv <- function(f) file.path(.root, "outputs/tables/advanced", f)
out_fig <- function(f) file.path(.root, "outputs/figures", f)
paper_fig <- function(f) file.path(.root, "paper/figures", f)

dir.create(out_fig(""), showWarnings = FALSE, recursive = TRUE)
dir.create(paper_fig(""), showWarnings = FALSE, recursive = TRUE)

save_both <- function(plot, name_base, w_in, h_in, dpi = 300) {
  png_path <- out_fig(paste0(name_base, ".png"))
  pdf_path <- out_fig(paste0(name_base, ".pdf"))
  ggsave(png_path, plot, width = w_in, height = h_in, dpi = dpi, bg = "white")
  ggsave(pdf_path, plot, width = w_in, height = h_in, device = "pdf", bg = "white")
  file.copy(png_path, paper_fig(paste0(name_base, ".png")), overwrite = TRUE)
  file.copy(pdf_path, paper_fig(paste0(name_base, ".pdf")), overwrite = TRUE)
  message("Wrote ", png_path)
}

# ---- Figure 1: attack simulation ---------------------------------------------
d_atk <- read_csv(adv("A10_attack_simulation.csv"), show_col_types = FALSE) |>
  filter(step <= 45)

p1 <- ggplot(d_atk) +
  geom_ribbon(aes(x = step, ymin = lcc_low, ymax = lcc_high), fill = paper_colors$gray_light, alpha = 0.35) +
  geom_line(aes(x = step, y = lcc_mean), color = paper_colors$gray_mid, linewidth = 0.5, linetype = "dashed") +
  geom_line(aes(x = step, y = lcc_targeted), color = paper_colors$gray_dark, linewidth = 0.9) +
  geom_vline(xintercept = 1, linetype = "dotted", color = paper_colors$gray_mid) +
  geom_vline(xintercept = 10, linetype = "dotted", color = paper_colors$gray_mid) +
  annotate(
    "segment", x = 1.8, y = 0.975, xend = 1.0, yend = d_atk$lcc_targeted[d_atk$step == 1][1],
    color = paper_colors$gray_mid, linewidth = 0.35
  ) +
  annotate(
    "text", x = 1.85, y = 0.98,
    label = "Caicedo removed", hjust = 0, vjust = -0.2, size = 2.8, color = paper_colors$gray_mid
  ) +
  annotate(
    "text", x = 44.5, y = 0.98,
    label = "1.4% of families", hjust = 1, vjust = 1, size = 2.8, color = paper_colors$gray_mid
  ) +
  labs(
    x = "Families removed (ordered by betweenness)",
    y = "Largest connected component (fraction)",
    subtitle = "Targeted removal collapses the network within ten families; random removal degrades linearly"
  ) +
  theme_paper()

save_both(p1, "A10_attack_simulation", 8, 4.5)

# ---- Load family edges (kinship) or interfamiliar fallback -------------------
load_family_edges <- function() {
  p0 <- adv("A0_family_kinship_edges.csv")
  if (file.exists(p0)) {
    x <- read_csv(p0, show_col_types = FALSE)
    return(
      x |>
        transmute(
          from = as.character(.data$from),
          to = as.character(.data$to)
        ) |>
        distinct()
    )
  }
  message("Note: A0_family_kinship_edges.csv not found; using interfamiliar_conexiones_detalle.csv fallback for layout.")
  mem <- read_csv(adv("A3_sbm_membership.csv"), show_col_types = FALSE)
  fams <- mem$family
  ifi <- read_csv(file.path(.root, "outputs/tables/interfamiliar_conexiones_detalle.csv"), show_col_types = FALSE)
  ifi |>
    filter(.data$pais_p == .data$pais_r, .data$fam_p != .data$fam_r) |>
    transmute(
      from = pmin(fam_p, fam_r),
      to = pmax(fam_p, fam_r)
    ) |>
    distinct() |>
    filter(from %in% fams, to %in% fams)
}

# ---- Figure 2: SBM core on layout -------------------------------------------
mem <- read_csv(adv("A3_sbm_membership.csv"), show_col_types = FALSE)
el <- load_family_edges()

g <- graph_from_data_frame(
  el,
  vertices = data.frame(name = mem$family, stringsAsFactors = FALSE),
  directed = FALSE
)
g <- simplify(g, remove.multiple = TRUE, remove.loops = TRUE)
set.seed(42)
xy <- layout_with_fr(g, niter = 1500)
nm <- V(g)$name
lay_df <- tibble(
  name = nm,
  x = xy[, 1],
  y = xy[, 2]
) |>
  left_join(mem |> select(name = family, block, country), by = "name")

lay_df <- lay_df |>
  mutate(
    node_class = case_when(
      block == 2 & country == "chile" ~ "Core (Chile)",
      block == 2 ~ "Core (non-Chile)",
      TRUE ~ "Periphery"
    ),
    pt_size = if_else(block == 2, 3.2, 0.9),
    pt_alpha = if_else(block == 2, 0.95, 0.35)
  )

el_lines <- igraph::as_data_frame(g, what = "edges") |>
  left_join(lay_df |> select(name, x, y), by = c("from" = "name")) |>
  rename(xf = x, yf = y) |>
  left_join(lay_df |> select(name, x, y), by = c("to" = "name")) |>
  rename(xt = x, yt = y)

lab_fam <- c("caicedo", "cruz_chile", "pinto_chile", "vicuña")
lay_lab <- lay_df |>
  filter(name %in% lab_fam)

p2 <- ggplot() +
  geom_segment(
    data = el_lines,
    aes(x = xf, y = yf, xend = xt, yend = yt),
    color = paper_colors$gray_light,
    linewidth = 0.12,
    alpha = 0.15
  ) +
  geom_point(
    data = lay_df,
    aes(x = x, y = y, size = pt_size, alpha = pt_alpha, color = node_class),
    shape = 16
  ) +
  scale_color_manual(
    values = c(
      "Core (Chile)" = paper_colors$accent_chile,
      "Core (non-Chile)" = paper_colors$accent_core,
      "Periphery" = "gray70"
    )
  ) +
  scale_size_identity() +
  scale_alpha_identity() +
  ggrepel::geom_text_repel(
    data = lay_lab,
    aes(x = x, y = y, label = name),
    size = 2.6,
    fontface = "bold",
    color = paper_colors$gray_dark,
    min.segment.length = 0,
    max.overlaps = 20
  ) +
  labs(
    x = NULL,
    y = NULL,
    subtitle = "Twenty-one families (16 Chilean) form a dense core with 208× the internal tie probability of the periphery"
  ) +
  theme_paper() +
  theme(
    legend.position = "bottom",
    axis.text = element_blank(),
    panel.grid = element_blank()
  )

save_both(p2, "A3_sbm_core_membership", 8, 4.5)

# ---- Figure 3: temporal consolidation --------------------------------------
tmp <- read_csv(adv("A7_temporal_network_metrics.csv"), show_col_types = FALSE) |>
  mutate(cohort = factor(cohort, levels = cohort))

p_edges <- ggplot(tmp, aes(x = cohort, y = n_edges)) +
  geom_col(fill = paper_colors$gray_mid, width = 0.65) +
  labs(y = "Edge count", x = NULL) +
  theme_paper()

p_share <- ggplot(tmp) +
  geom_line(aes(x = cohort, y = prop_cross_family, group = 1), color = paper_colors$gray_dark, linewidth = 0.8) +
  geom_line(aes(x = cohort, y = prop_cross_country, group = 1), color = paper_colors$gray_light, linewidth = 0.7, linetype = "dashed") +
  labs(y = "Share", x = NULL) +
  theme_paper() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

if (!requireNamespace("patchwork", quietly = TRUE)) {
  install.packages("patchwork", repos = "https://cloud.r-project.org", quiet = TRUE)
}
library(patchwork)
p3 <- p_edges / p_share +
  plot_annotation(
    subtitle = "Edge volume rises through Republican; cross-family share falls monotonically — the signature of oligarchic consolidation"
  )

save_both(p3, "A7_temporal_consolidation", 8, 6)

# ---- Figure 4: homophily -----------------------------------------------------
hom <- read_csv(adv("A4_homophily_indices.csv"), show_col_types = FALSE) |>
  mutate(
    attribute = factor(attribute, levels = attribute[order(H_observed)])
  )

p4 <- ggplot(hom, aes(x = H_observed, y = attribute, fill = H_observed >= 0)) +
  geom_col(color = NA, width = 0.7) +
  geom_point(aes(x = H_expected_component, y = attribute), shape = 124, size = 2, color = "gray50") +
  scale_fill_manual(values = c(`TRUE` = paper_colors$accent_core, `FALSE` = paper_colors$gray_mid), guide = "none") +
  geom_text(aes(label = sprintf("%.2f", H_observed)), hjust = ifelse(hom$H_observed >= 0, -0.05, 1.05), size = 2.5, color = paper_colors$gray_dark) +
  labs(
    x = expression(paste("Coleman-like homophily index ", italic(H), " (−1 = heterophily, +1 = homophily)")),
    y = NULL,
    subtitle = "Tier and country sort the network; political office shows strong heterophily — functional complementarity"
  ) +
  theme_paper()

save_both(p4, "A4_homophily", 8, 4.5)

# ---- Figure 5: robust brokers scatter -----------------------------------------
brok <- read_csv(adv("A6_brokerage_roles.csv"), show_col_types = FALSE)
mem <- read_csv(adv("A3_sbm_membership.csv"), show_col_types = FALSE)
crit <- read_csv(adv("A10_critical_families.csv"), show_col_types = FALSE) |>
  slice_head(n = 30) |>
  distinct(removed_family) |>
  pull(removed_family)

br <- brok |>
  group_by(familia) |>
  summarise(
    coordinator = sum(coordinator, na.rm = TRUE),
    consultant = sum(consultant, na.rm = TRUE),
    gatekeeper = sum(gatekeeper, na.rm = TRUE),
    representative = sum(representative, na.rm = TRUE),
    liaison = sum(liaison, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    total_b = coordinator + consultant + gatekeeper + representative + liaison,
    cross_share = ifelse(total_b > 0, (gatekeeper + representative) / total_b, NA_real_)
  ) |>
  left_join(mem |> select(familia = family, block), by = "familia") |>
  mutate(
    block = dplyr::coalesce(block, 1L),
    is_crit = familia %in% crit,
    ptcol = dplyr::if_else(is_crit, paper_colors$accent_core, paper_colors$gray_light),
    ptshape = dplyr::if_else(block == 2L, 17L, 16L)
  ) |>
  filter(total_b > 0, is.finite(cross_share))

lab_primary <- c("caicedo", "cruz_chile", "pinto_chile", "vicuña")
lab_counter <- c(
  "garcía-mansilla", "echaurren", "ospina", "arboleda",
  "chávez", "balcarce", "podestá", "onassis"
)
br_lab <- br |>
  filter(familia %in% c(lab_primary, lab_counter))

p5 <- ggplot(br, aes(x = total_b, y = cross_share)) +
  annotate(
    "rect",
    xmin = quantile(br$total_b, 0.5, na.rm = TRUE), xmax = Inf,
    ymin = 0.12, ymax = 0.55,
    fill = paper_colors$gray_light, alpha = 0.2
  ) +
  geom_point(aes(color = ptcol, shape = ptshape), size = 2.2, show.legend = FALSE) +
  scale_color_identity() +
  scale_shape_identity() +
  scale_x_continuous(trans = "log1p") +
  ggrepel::geom_text_repel(
    data = br_lab,
    aes(label = familia, x = total_b, y = cross_share),
    size = 2.4,
    max.overlaps = 30,
    segment.color = paper_colors$gray_mid
  ) +
  labs(
    title = NULL,
    subtitle = NULL,
    x = "Total aggregate brokerage (log scale)",
    y = "Share of cross-block roles\n(gatekeeper + representative)"
  ) +
  theme_paper() +
  theme(
    plot.margin = margin(5, 15, 5, 5),
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 9),
    axis.text = element_text(size = 8)
  )

save_both(p5, "A6_A10_robust_brokers", 7.5, 5)

file.copy(out_fig("A6_A10_robust_brokers.png"), paper_fig("T_robust_brokers_comparison.png"), overwrite = TRUE)
file.copy(out_fig("A6_A10_robust_brokers.pdf"), paper_fig("T_robust_brokers_comparison.pdf"), overwrite = TRUE)
message("Wrote ", paper_fig("T_robust_brokers_comparison.png"))

message("Done.")
