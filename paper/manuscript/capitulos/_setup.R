# Sourced from elite_networks_paper.qmd; getwd() == paper/manuscript when rendering
.root <- normalizePath(file.path(getwd(), "..", ".."))

path_out <- function(...) file.path(.root, ...)
path_tbl <- function(f) path_out("outputs/tables", f)
path_fig <- function(f) path_out("outputs/figures", f)
path_paper_tbl <- function(f) path_out("paper/tables", f)
path_paper_fig <- function(f) path_out("paper/figures", f)

read_tbl <- function(f, subdir = "outputs/tables") {
  readr::read_csv(file.path(.root, subdir, f), show_col_types = FALSE)
}
