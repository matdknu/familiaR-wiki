# Consistent ggplot2 theme for manuscript figures (H5 pipeline).
theme_paper <- function(base_size = 10) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      plot.subtitle = ggplot2::element_text(color = "gray35", size = base_size - 1),
      plot.margin = ggplot2::margin(8, 8, 8, 8)
    )
}

paper_colors <- list(
  gray_dark = "#2d2d2d",
  gray_mid = "#8c8c8c",
  gray_light = "#d9d9d9",
  accent_core = "#1f4e79",
  accent_chile = "#8B4513"
)
