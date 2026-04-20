# ============================================================================
# 10_red_pais.R
# ----------------------------------------------------------------------------
# Red de élite por país: mismos datos que 01_exploracion.R (consolidado LATAM,
# aristas desde URLs en "personas relacionadas"), con figuras pensadas para
# leer bien familia + personas (aristas visibles, etiquetas con ggrepel).
#
# Salidas:
#   - outputs/figures/red_elite_por_pais/red_<pais>.png  (uno por país con aristas)
#   - outputs/figures/red_elite_por_pais/red_vinculos_entre_paises.png (sólo
#     aristas cuyo origen y destino caen en países distintos, cuando existen)
#
# Requisito: data/processed/familias/_CONSOLIDADO_familias_latam.csv
# (ejecutar antes 00_consolidar_familias si falta).
# ============================================================================

library(readr)
library(dplyr)
library(purrr)
library(stringr)
library(tidyr)
library(tibble)
library(igraph)
library(ggplot2)
library(ggrepel)
library(scales)

# --- Parámetros de lectura visual (ajustar si hace falta) -------------------
# Etiquetas: solo los nodos con más conexiones (máximo LABEL_TOP_N nombres en el gráfico).
LABEL_TOP_N <- 8L
# Por defecto NO recortamos por grado: se dibujan todos los nodos (todas las familias visibles).
# Si la red es enorme y FR es lento, pon RECORTAR_POR_GRADO <- TRUE y usa MAX_NODES_PLOT.
RECORTAR_POR_GRADO <- FALSE
MAX_NODES_PLOT <- 140L
EDGE_COLOR_WEAK   <- "#999999"
EDGE_COLOR_STRONG <- "#1a2f5e"
EDGE_ALPHA_WEAK   <- 0.18
EDGE_ALPHA_STRONG <- 0.75
EDGE_CURVATURE    <- 0.18
# Used by cross-country plots; within-country node size is adaptive in plot_red_elite_pais().
NODE_SIZE_RANGE   <- c(1.2, 6.5)

country_name_en <- function(x) {
  key <- tolower(trimws(as.character(x)))
  ref <- c(
    argentina = "Argentina", bolivia = "Bolivia", chile = "Chile", colombia = "Colombia",
    ecuador = "Ecuador", mexico = "Mexico", "méxico" = "Mexico", paraguay = "Paraguay",
    peru = "Peru", "perú" = "Peru", uruguay = "Uruguay", venezuela = "Venezuela"
  )
  out <- unname(ref[key])
  ifelse(is.na(out), stringr::str_to_title(as.character(x)), out)
}

# =============================================================================
# CARGA Y ARISTAS (misma lógica que 01_exploracion.R)
# =============================================================================

ruta_latam <- "data/processed/familias/_CONSOLIDADO_familias_latam.csv"
if (!file.exists(ruta_latam)) {
  stop("No se encontró ", ruta_latam, ". Ejecuta antes 00_consolidar_familias.R")
}

familias <- read_delim(ruta_latam, delim = ";", show_col_types = FALSE)

if (!"pais" %in% names(familias) && "pais_origen" %in% names(familias)) {
  familias$pais <- familias$pais_origen
}

normalize_country <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x[x %in% c("", "na", "null", "none")] <- NA_character_
  x
}

familias <- familias %>%
  filter(!is.na(url), trimws(url) != "") %>%
  mutate(
    pais = as.character(coalesce(pais, pais_origen)),
    pais = normalize_country(pais)
  ) %>%
  filter(!is.na(pais), trimws(pais) != "")

campos_relacion <- c(
  "padres", "conyuge", "pareja", "hijos", "hermanos",
  "perfiles_relacionados",
  "perfiles_relacionados_padres", "perfiles_relacionados_conyuge",
  "perfiles_relacionados_pareja", "perfiles_relacionados_hijos",
  "perfiles_relacionados_hermanos", "perfiles_relacionados_familia"
)
campos_relacion <- intersect(campos_relacion, names(familias))

extraer_urls <- function(x) {
  if (is.na(x) || trimws(as.character(x)) == "") return(character())
  x <- as.character(x)
  pat <- "\\(https://es\\.wikipedia\\.org/wiki/([^)]+)\\)"
  m <- str_match_all(x, pat)[[1]]
  if (nrow(m) > 0) return(unique(paste0("https://es.wikipedia.org/wiki/", m[, 2])))
  pat2 <- "https://es\\.wikipedia\\.org/wiki/[^\\s)]+"
  urls <- str_extract_all(x, pat2)[[1]]
  unique(trimws(urls))
}

keywords_por_pais <- list(
  argentina = c("argentina", "buenos aires", "córdoba", "rosario", "argentino", "argentina"),
  chile = c("chile", "santiago", "valparaíso", "chileno", "chilena"),
  mexico = c("méxico", "mexico", "ciudad de méxico", "guadalajara", "mexicano", "mexicana"),
  peru = c("perú", "peru", "lima", "peruano", "peruana"),
  colombia = c("colombia", "bogotá", "medellín", "colombiano", "colombiana"),
  venezuela = c("venezuela", "caracas", "venezolano", "venezolana"),
  bolivia = c("bolivia", "la paz", "boliviano", "boliviana"),
  paraguay = c("paraguay", "asunción", "paraguayo", "paraguaya"),
  uruguay = c("uruguay", "montevideo", "uruguayo", "uruguaya"),
  ecuador = c("ecuador", "quito", "guayaquil", "ecuatoriano", "ecuatoriana")
)

inferir_pais_en_texto <- function(texto, excluir_pais = NULL) {
  if (is.na(texto) || str_trim(as.character(texto)) == "") return(NA_character_)
  t <- tolower(str_trim(as.character(texto)))
  for (pais in names(keywords_por_pais)) {
    if (!is.null(excluir_pais) && pais == excluir_pais) next
    if (any(str_detect(t, fixed(keywords_por_pais[[pais]])))) return(pais)
  }
  NA_character_
}

url_extra_path <- "data/manual/url_pais_extra.csv"
country_overrides <- tibble(url = character(), pais_override = character())
if (file.exists(url_extra_path)) {
  country_overrides <- read_csv(url_extra_path, show_col_types = FALSE) %>%
    transmute(
      url = as.character(url),
      pais_override = normalize_country(pais)
    ) %>%
    filter(!is.na(url), !is.na(pais_override)) %>%
    distinct(url, .keep_all = TRUE)
}

familias <- familias %>%
  left_join(country_overrides, by = "url") %>%
  mutate(
    texto_pais_persona = paste(
      as.character(nacionalidad),
      as.character(lugar_nacimiento),
      as.character(biografia_inicial),
      sep = " "
    ),
    pais_persona_inferido = map_chr(texto_pais_persona, inferir_pais_en_texto, excluir_pais = NULL),
    pais_persona = coalesce(pais_override, pais_persona_inferido, pais),
    pais_persona = normalize_country(pais_persona)
  ) %>%
  select(-texto_pais_persona)

edges_global <- list()
for (i in seq_len(nrow(familias))) {
  url_origen <- familias$url[i]
  pais_origen <- familias$pais_persona[i]
  texto_origen <- paste(
    as.character(familias$biografia_inicial[i]),
    as.character(familias$biografia[i]),
    as.character(familias$nacionalidad[i]),
    as.character(familias$lugar_nacimiento[i]),
    collapse = " "
  )
  for (campo in campos_relacion) {
    texto_campo <- familias[[campo]][i]
    urls_destino <- extraer_urls(texto_campo)
    for (url_dest in urls_destino) {
      if (url_dest != url_origen && nchar(trimws(url_dest)) > 0) {
        edges_global[[length(edges_global) + 1]] <- tibble(
          from = url_origen,
          to = url_dest,
          pais = pais_origen,
          texto_relacion = paste(as.character(texto_campo), collapse = " "),
          texto_origen = texto_origen
        )
      }
    }
  }
}

if (length(edges_global) == 0) {
  stop("No se encontraron relaciones (URLs) en los campos de personas relacionadas.")
}

edges_global <- bind_rows(edges_global) %>%
  group_by(from, to, pais) %>%
  summarise(
    texto_relacion = paste(str_trim(replace_na(texto_relacion, "")), collapse = " "),
    texto_origen = paste(str_trim(replace_na(first(texto_origen), "")), collapse = " "),
    .groups = "drop"
  ) %>%
  mutate(texto_para_inferir = paste(texto_relacion, texto_origen, sep = " "))

url_a_pais <- familias %>%
  distinct(url, .keep_all = TRUE) %>%
  transmute(url, pais_destino = pais_persona)

edges_con_pais_destino <- edges_global %>%
  left_join(url_a_pais, by = c("to" = "url")) %>%
  mutate(
    pais_inferido = map2_chr(
      texto_para_inferir,
      pais,
      ~ inferir_pais_en_texto(.x, excluir_pais = NULL)
    ),
    pais_destino = coalesce(pais_destino, pais_inferido)
  ) %>%
  select(from, to, pais, pais_destino)

# =============================================================================
# REDES POR PAÍS (internas)
# =============================================================================

urls_por_pais <- familias %>% group_by(pais_persona) %>% summarise(urls = list(unique(url)), .groups = "drop")
paises <- sort(unique(familias$pais_persona))
resultados_pais <- list()

for (p in paises) {
  urls_p <- urls_por_pais %>% filter(pais_persona == p) %>% pull(urls) %>% `[[`(1)
  if (length(urls_p) == 0) next

  edges_p <- edges_global %>%
    filter(pais == p, from %in% urls_p, to %in% urls_p) %>%
    select(from, to)

  if (nrow(edges_p) == 0) {
    g <- make_empty_graph() + vertices(urls_p)
    V(g)$degree <- 0
    V(g)$betweenness <- 0
  } else {
    g <- graph_from_data_frame(edges_p, vertices = tibble(url = urls_p), directed = FALSE)
    g <- simplify(g)
    V(g)$degree <- degree(g)
    V(g)$betweenness <- betweenness(g)
  }

  nodos_p <- tibble(
    url = V(g)$name,
    grado = as.integer(V(g)$degree),
    betweenness = as.numeric(V(g)$betweenness),
    pais = p
  ) %>%
    left_join(
      familias %>% distinct(url, .keep_all = TRUE) %>% select(url, nombre, familia),
      by = "url"
    ) %>%
    mutate(
      nombre = coalesce(nombre, str_replace_all(URLdecode(str_replace(str_extract(url, "(?<=wiki/).+"), "_", " ")), "%", " ")),
      familia = replace_na(trimws(as.character(familia)), "Sin familia"),
      familia = if_else(familia == "", "Sin familia", familia)
    )

  if (ecount(g) > 0 && vcount(g) > 1) {
    set.seed(42)
    lay <- layout_with_fr(g, niter = 800)
    nodos_p$x <- lay[, 1]
    nodos_p$y <- lay[, 2]
  } else {
    nodos_p$x <- 0
    nodos_p$y <- 0
  }

  resultados_pais[[p]] <- list(grafo = g, nodos = nodos_p, n_edges = ecount(g), edges = edges_p)
}

# Paleta de familias (global, para consistencia entre países)
all_fam <- unique(bind_rows(lapply(resultados_pais, function(r) r$nodos))$familia)
base_cols <- c(
  "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf",
  "#aec7e8", "#ffbb78", "#98df8a", "#ff9896", "#c5b0d5", "#c49c94", "#f7b6d2", "#c7c7c7", "#dbdb8d", "#9edae5"
)
familia_colors <- setNames(rep(base_cols, length.out = length(all_fam)), all_fam)
familia_colors["Sin familia"] <- "gray75"

# --- Reduce densidad: subgrafo inducido por los nodos de mayor grado ---------
downsample_graph <- function(g, nodos_p, edges_p, max_nodes) {
  if (is.null(edges_p) || nrow(edges_p) == 0 || nrow(nodos_p) <= max_nodes) {
    return(list(g = g, nodos = nodos_p, edges = edges_p, recortado = FALSE))
  }
  ord <- nodos_p %>% arrange(desc(grado), url)
  keep <- ord %>% slice_head(n = max_nodes) %>% pull(url)
  edges_f <- edges_p %>% filter(from %in% keep, to %in% keep)
  verts <- sort(unique(c(edges_f$from, edges_f$to)))
  if (length(verts) < 2 || nrow(edges_f) == 0) {
    return(list(g = g, nodos = nodos_p, edges = edges_p, recortado = FALSE))
  }
  g2 <- graph_from_data_frame(edges_f, vertices = tibble(url = verts), directed = FALSE)
  g2 <- simplify(g2)
  nodos_f <- nodos_p %>% filter(url %in% verts)
  nodos_f$grado <- degree(g2)[nodos_f$url]
  nodos_f$betweenness <- betweenness(g2)[nodos_f$url]

  set.seed(42)
  lay <- layout_with_fr(g2, niter = 800)
  ix <- match(nodos_f$url, V(g2)$name)
  nodos_f$x <- lay[ix, 1]
  nodos_f$y <- lay[ix, 2]

  list(g = g2, nodos = nodos_f, edges = edges_f %>% select(from, to), recortado = TRUE)
}

plot_red_elite_pais <- function(p, r, familia_colors) {
  edges_p <- r$edges
  if (is.null(edges_p) || nrow(edges_p) == 0) return(invisible(NULL))

  if (isTRUE(RECORTAR_POR_GRADO)) {
    ds <- downsample_graph(r$grafo, r$nodos, edges_p, MAX_NODES_PLOT)
  } else {
    ds <- list(g = r$grafo, nodos = r$nodos, edges = edges_p, recortado = FALSE)
  }
  g <- ds$g
  nodos_p <- ds$nodos
  edges_use <- ds$edges
  recortado <- ds$recortado

  if (nrow(nodos_p) > 500 && ecount(r$grafo) > 0) {
    comps <- components(r$grafo)
    main_comp_id <- which.max(comps$csize)
    main_comp_urls <- names(comps$membership[comps$membership == main_comp_id])
    nodos_p <- nodos_p %>%
      filter(url %in% main_comp_urls | grado > 1)
    edges_use <- edges_use %>%
      filter(from %in% nodos_p$url, to %in% nodos_p$url)
  }

  nodos_p <- nodos_p %>%
    mutate(
      nombre_corto = if_else(nchar(nombre) > 32, paste0(str_sub(nombre, 1, 29), "..."), nombre),
      familia_label = replace_na(trimws(as.character(familia)), "Sin familia"),
      label_nodo = if_else(familia_label == "Sin familia", nombre_corto, paste0(nombre_corto, " (", familia_label, ")")),
      rank_deg = rank(-grado, ties.method = "min")
    )
  if (!any(nodos_p$grado > 0, na.rm = TRUE)) {
    label_urls <- character(0)
  } else {
    label_urls <- nodos_p %>%
      filter(grado > 0) %>%
      slice_min(rank_deg, n = min(LABEL_TOP_N, sum(nodos_p$grado > 0, na.rm = TRUE)), with_ties = FALSE) %>%
      pull(url)
  }

  nodos_p <- nodos_p %>% mutate(etiquetar = url %in% label_urls)

  pais_lab <- country_name_en(p)
  n_fam_distintas <- n_distinct(nodos_p$familia)
  subt <- paste0(
    nrow(r$nodos), " nodes, ", nrow(edges_p), " edges (within-country network). ",
    n_fam_distintas, " distinct families (node color). ",
    if (recortado) paste0("Filtered view: top-", MAX_NODES_PLOT, " nodes by degree. ") else "",
    "Labels: top ", LABEL_TOP_N, " nodes by degree."
  )

  node_alpha <- if (nrow(nodos_p) > 800) 0.22 else if (nrow(nodos_p) > 400) 0.30 else 0.42
  node_size_range <- if (nrow(nodos_p) > 800) c(0.45, 2.8) else if (nrow(nodos_p) > 400) c(0.65, 3.6) else c(0.95, 5.2)

  nod_coords <- nodos_p %>% select(url, x, y, grado)
  edges_plot <- edges_use
  edges_with_coords <- edges_plot %>%
    left_join(nod_coords, by = c("from" = "url")) %>%
    rename(x_from = x, y_from = y, deg_from = grado) %>%
    left_join(nod_coords %>% select(url, x, y, grado), by = c("to" = "url")) %>%
    rename(x_to = x, y_to = y, deg_to = grado) %>%
    left_join(nodos_p %>% select(url, familia) %>% rename(fam_from = familia), by = c("from" = "url")) %>%
    left_join(nodos_p %>% select(url, familia) %>% rename(fam_to = familia), by = c("to" = "url")) %>%
    mutate(
      edge_weight = (deg_from + deg_to) / 2,
      is_strong = edge_weight >= quantile(edge_weight, 0.55, na.rm = TRUE),
      fam_from = replace_na(trimws(as.character(fam_from)), "Sin familia"),
      fam_to = replace_na(trimws(as.character(fam_to)), "Sin familia"),
      tipo_union = if_else(fam_from == fam_to, "intrafamiliar", "interfamiliar")
    ) %>%
    drop_na(x_from, y_from, x_to, y_to)

  edges_strong <- edges_with_coords %>% filter(is_strong)
  edges_weak <- edges_with_coords %>% filter(!is_strong)
  edges_strong_intra <- edges_strong %>% filter(tipo_union == "intrafamiliar")
  edges_strong_inter <- edges_strong %>% filter(tipo_union == "interfamiliar")

  ggplot() +
    geom_curve(
      data = edges_weak,
      aes(x = x_from, y = y_from, xend = x_to, yend = y_to),
      color = EDGE_COLOR_WEAK, alpha = 0.12, linewidth = 0.22,
      curvature = 0.12, lineend = "round"
    ) +
    geom_curve(
      data = edges_strong_intra,
      aes(x = x_from, y = y_from, xend = x_to, yend = y_to, linewidth = edge_weight),
      color = EDGE_COLOR_STRONG, alpha = 0.82,
      curvature = 0.20, lineend = "round"
    ) +
    geom_curve(
      data = edges_strong_inter,
      aes(x = x_from, y = y_from, xend = x_to, yend = y_to, linewidth = edge_weight),
      color = "#c0392b", alpha = 0.9,
      curvature = 0.20, lineend = "round"
    ) +
    geom_point(
      data = nodos_p,
      aes(x = x, y = y, size = grado + 0.3, fill = familia),
      shape = 21,
      color = "#333333",
      stroke = 0.18,
      alpha = node_alpha
    ) +
    geom_text_repel(
      data = nodos_p %>% filter(etiquetar),
      aes(x = x, y = y, label = label_nodo),
      size = 3.2,
      fontface = "bold",
      color = "#0d1b2a",
      bg.color = "white",
      bg.r = 0.18,
      max.overlaps = 30,
      segment.size = 0.3,
      segment.color = "#555555",
      segment.alpha = 0.6,
      min.segment.length = 0,
      box.padding = 0.45,
      point.padding = 0.3
    ) +
    annotate(
      "text",
      x = Inf, y = -Inf,
      hjust = 1.05, vjust = -0.5,
      label = paste0(
        "n = ", nrow(nodos_p), " nodes | ", nrow(edges_use), " edges",
        "\ninter-family ties: ", nrow(edges_strong_inter)
      ),
      size = 2.8, color = "#888888", fontface = "italic"
    ) +
    scale_fill_manual(values = familia_colors, na.value = "gray50", guide = "none") +
    scale_linewidth_continuous(range = c(0.7, 3.4), guide = "none") +
    scale_size_continuous(range = node_size_range, guide = "none") +
    labs(
      title = paste0("Elite kinship network — ", pais_lab),
      subtitle = subt
    ) +
    theme_void(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
      plot.subtitle = element_text(hjust = 0.5, color = "gray35", size = 9.5, lineheight = 1.15),
      legend.position = "none",
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(12, 12, 12, 12)
    )
}

# =============================================================================
# GUARDAR: una figura por país
# =============================================================================

out_dir <- "outputs/figures/red_elite_por_pais"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

for (p in names(resultados_pais)) {
  r <- resultados_pais[[p]]
  if (is.null(r$edges) || nrow(r$edges) == 0) next
  p_gg <- plot_red_elite_pais(p, r, familia_colors)
  if (is.null(p_gg)) next
  fig_w <- if (nrow(r$nodos) > 500) 16 else 12
  fig_h <- if (nrow(r$nodos) > 500) 12 else 9
  ggsave(file.path(out_dir, paste0("red_", p, ".png")), p_gg, width = fig_w, height = fig_h, dpi = 220, bg = "white")
  message("Guardado: ", file.path(out_dir, paste0("red_", p, ".png")))
}

# =============================================================================
# Vínculos ENTRE países (unión): aristas donde origen y destino ≠ país
# =============================================================================

edges_cross <- edges_con_pais_destino %>%
  filter(
    !is.na(pais_destino),
    !is.na(pais),
    pais_destino != pais
  ) %>%
  distinct(from, to, .keep_all = TRUE)

if (nrow(edges_cross) >= 1) {
  # Mantener más aristas: no colapsar pares por dirección.
  e_cross <- edges_cross %>%
    filter(from != to) %>%
    distinct(from, to, pais, pais_destino, .keep_all = TRUE) %>%
    transmute(from, to, pais_origen = pais, pais_destino)

  all_urls <- unique(c(e_cross$from, e_cross$to))
  fam_idx <- familias %>%
    filter(url %in% all_urls) %>%
    distinct(url, .keep_all = TRUE) %>%
    select(url, nombre, familia, pais_persona)

  nodos_x <- tibble(url = all_urls) %>%
    left_join(fam_idx, by = "url") %>%
    mutate(
      nombre = coalesce(nombre, str_replace_all(URLdecode(str_replace(str_extract(url, "(?<=wiki/).+"), "_", " ")), "%", " ")),
      familia = replace_na(trimws(as.character(familia)), "Sin familia"),
      pais_node = country_name_en(coalesce(pais_persona, "—"))
    )

  g_x <- graph_from_data_frame(
    e_cross %>% select(from, to),
    vertices = tibble(url = all_urls),
    directed = TRUE
  )
  V(g_x)$degree <- degree(g_x, mode = "all")

  if (ecount(g_x) > 0 && vcount(g_x) > 1) {
    set.seed(43)
    lay_x <- layout_with_fr(g_x, niter = 1200)
    nodos_x <- nodos_x %>% filter(url %in% V(g_x)$name)
    nodos_x$x <- lay_x[match(nodos_x$url, V(g_x)$name), 1]
    nodos_x$y <- lay_x[match(nodos_x$url, V(g_x)$name), 2]

    if (nrow(nodos_x) > MAX_NODES_PLOT) {
      deg_map <- degree(g_x, mode = "all")
      nodos_x <- nodos_x %>% mutate(gr = deg_map[url])
      topu <- nodos_x %>% arrange(desc(gr)) %>% slice_head(n = MAX_NODES_PLOT) %>% pull(url)
      e_sub <- e_cross %>% filter(from %in% topu, to %in% topu)
      g_x <- graph_from_data_frame(
        e_sub,
        vertices = tibble(url = sort(unique(c(e_sub$from, e_sub$to)))),
        directed = TRUE
      )
      nodos_x <- nodos_x %>% filter(url %in% V(g_x)$name)
      set.seed(43)
      lay_x <- layout_with_fr(g_x, niter = 1200)
      nodos_x$x <- lay_x[match(nodos_x$url, V(g_x)$name), 1]
      nodos_x$y <- lay_x[match(nodos_x$url, V(g_x)$name), 2]
    }

    nodos_x <- nodos_x %>%
      mutate(
        nombre_corto = if_else(nchar(nombre) > 30, paste0(str_sub(nombre, 1, 27), "..."), nombre),
        gr = degree(g_x, mode = "all")[url]
      )
    lab_u <- nodos_x %>% arrange(desc(gr)) %>% slice_head(n = min(LABEL_TOP_N, nrow(nodos_x))) %>% pull(url)

    seg <- igraph::as_data_frame(g_x, what = "edges") %>%
      left_join(nodos_x %>% select(url, x, y), by = c("from" = "url")) %>%
      rename(x_from = x, y_from = y) %>%
      left_join(nodos_x %>% select(url, x, y), by = c("to" = "url")) %>%
      rename(x_to = x, y_to = y) %>%
      mutate(
        par_paises = paste0(country_name_en(pais_origen), " -> ", country_name_en(pais_destino))
      )

    cols_pais <- c(
      "Argentina" = "#6CACE4", "Bolivia" = "#007A33", "Chile" = "#0033A0",
      "Colombia" = "#FCD116", "Ecuador" = "#FFD100", "Mexico" = "#006847",
      "Paraguay" = "#D52B1E", "Peru" = "#D91023", "Uruguay" = "#0038A8",
      "Venezuela" = "#CF142B", "—" = "gray70"
    )

    p_cross <- ggplot() +
      geom_curve(
        data = seg,
        aes(x = x_from, y = y_from, xend = x_to, yend = y_to),
        color = "#c0392b", alpha = 0.55, linewidth = 0.65, lineend = "round", curvature = 0.22
      ) +
      geom_point(data = nodos_x, aes(x = x, y = y, size = gr + 0.3, color = pais_node), alpha = 0.9) +
      geom_text_repel(
        data = nodos_x %>% filter(url %in% lab_u),
        aes(x = x, y = y, label = nombre_corto),
        size = 2.7, fontface = "bold", max.overlaps = 35,
        segment.size = 0.25, min.segment.length = 0, box.padding = 0.35
      ) +
      scale_color_manual(values = cols_pais, name = "País (nodo)") +
      scale_size_continuous(range = NODE_SIZE_RANGE, name = "Grado") +
      labs(
        title = "Vínculos entre países (personas relacionadas, extremos en países distintos)",
        subtitle = paste0(
          nrow(nodos_x), " nodos, ", nrow(seg), " aristas dirigidas. ",
          "Más líneas para distinguir mejor la unión transnacional entre países."
        )
      ) +
      theme_void(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, color = "gray35", size = 9.5),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.position = "right"
      )

    ggsave(file.path(out_dir, "red_vinculos_entre_paises.png"), p_cross, width = 14, height = 10, dpi = 200, bg = "white")
    message("Guardado: ", file.path(out_dir, "red_vinculos_entre_paises.png"))

    # Capa agregada país -> país para ver diferenciación de uniones.
    enlaces_pais <- e_cross %>%
      mutate(
        pais_a = country_name_en(pais_origen),
        pais_b = country_name_en(pais_destino)
      ) %>%
      count(pais_a, pais_b, name = "n_rel") %>%
      arrange(desc(n_rel))

    if (nrow(enlaces_pais) > 0) {
      nodos_pais <- tibble(pais = sort(unique(c(enlaces_pais$pais_a, enlaces_pais$pais_b))))
      g_pais <- graph_from_data_frame(
        enlaces_pais %>% transmute(from = pais_a, to = pais_b, n_rel),
        vertices = nodos_pais,
        directed = TRUE
      )
      set.seed(44)
      lay_p <- layout_in_circle(g_pais)
      nodos_pais$x <- lay_p[, 1]
      nodos_pais$y <- lay_p[, 2]
      nodos_pais$gr <- degree(g_pais, mode = "all")[nodos_pais$pais]

      ed_pais <- igraph::as_data_frame(g_pais, what = "edges") %>%
        left_join(nodos_pais %>% select(pais, x, y), by = c("from" = "pais")) %>%
        rename(x_from = x, y_from = y) %>%
        left_join(nodos_pais %>% select(pais, x, y), by = c("to" = "pais")) %>%
        rename(x_to = x, y_to = y)

      p_union <- ggplot() +
        geom_curve(
          data = ed_pais,
          aes(x = x_from, y = y_from, xend = x_to, yend = y_to, linewidth = n_rel),
          color = "#2b6cb0", alpha = 0.65, curvature = 0.18,
          arrow = arrow(length = unit(0.18, "cm"), type = "closed", ends = "last")
        ) +
        geom_point(data = nodos_pais, aes(x = x, y = y, size = gr), color = "#1a365d", alpha = 0.95) +
        geom_text_repel(
          data = nodos_pais,
          aes(x = x, y = y, label = pais),
          size = 3.5, fontface = "bold", max.overlaps = 50,
          box.padding = 0.4, point.padding = 0.25, segment.size = 0.25
        ) +
        scale_linewidth_continuous(range = c(0.6, 4.5), name = "N° conexiones") +
        scale_size_continuous(range = c(3, 10), name = "Grado país") +
        labs(
          title = "Unión entre países (agregado país -> país)",
          subtitle = paste0(nrow(ed_pais), " vínculos dirigidos agregados entre países.")
        ) +
        theme_void(base_size = 12) +
        theme(
          plot.title = element_text(face = "bold", hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5, color = "gray35", size = 9.5),
          panel.background = element_rect(fill = "white", color = NA),
          plot.background = element_rect(fill = "white", color = NA),
          legend.position = "right"
        )

      ggsave(file.path(out_dir, "red_union_entre_paises_agregada.png"), p_union, width = 12, height = 9, dpi = 220, bg = "white")
      message("Guardado: ", file.path(out_dir, "red_union_entre_paises_agregada.png"))
    }
  }
}

message("Listo 10_red_pais.R → ", out_dir)
