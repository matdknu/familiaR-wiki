# ============================================================================
# 01_exploracion.R
# ----------------------------------------------------------------------------
# 1) Redes DENTRO de cada país por "personas relacionadas":
#    - Quiénes y qué FAMILIAS más unen (conexiones internas).
# 2) Unión ENTRE países: vínculos familiares y diplomáticos/profesionales
#    (lugar de fallecimiento, residencia, trabajo, biografía).
# ----------------------------------------------------------------------------
# Salidas:
#   - Gráficos de barras: personas y familias que más unen por país.
#   - Redes gráficas por país (nodos coloreados por familia).
#   - Red/graph de conexiones entre países (familiares + diplomáticas).
# ============================================================================

library(readr)
library(dplyr)
library(purrr)
library(stringr)
library(tidyr)
library(igraph)
library(ggraph)
library(tidygraph)
library(ggplot2)
library(ggrepel)
library(forcats)

# =============================================================================
# CARGA DE DATOS
# =============================================================================
# Consolidado LATAM: una fila por persona, con país, familia, perfiles
# relacionados (padres, cónyuge, hijos, etc.) y campos de texto para detectar
# vínculos con otros países (lugar_fallecimiento, residencia, cargos, etc.).

ruta_latam <- "data/processed/familias/_CONSOLIDADO_familias_latam.csv"
if (!file.exists(ruta_latam)) {
  stop("No se encontró ", ruta_latam, ". Ejecuta antes 00_consolidar_familias.R")
}

familias <- read_delim(ruta_latam, delim = ";", show_col_types = FALSE)

# Asegurar columna de país: usar 'pais' si existe, si no 'pais_origen'
if (!"pais" %in% names(familias) && "pais_origen" %in% names(familias)) {
  familias$pais <- familias$pais_origen
}
familias <- familias %>%
  filter(!is.na(url), trimws(url) != "") %>%
  mutate(pais = coalesce(pais, pais_origen), pais = as.character(pais)) %>%
  filter(!is.na(pais), trimws(pais) != "")

# =============================================================================
# CAMPOS DE "PERSONAS RELACIONADAS"
# =============================================================================
# En el consolidado, las relaciones familiares vienen como texto con URLs
# de Wikipedia (ej.: "Nombre (https://es.wikipedia.org/wiki/Nombre)").
# Extraemos esas URLs para construir aristas: persona A -> persona B.

campos_relacion <- c(
  "padres", "conyuge", "pareja", "hijos", "hermanos",
  "perfiles_relacionados",
  "perfiles_relacionados_padres", "perfiles_relacionados_conyuge",
  "perfiles_relacionados_pareja", "perfiles_relacionados_hijos",
  "perfiles_relacionados_hermanos", "perfiles_relacionados_familia"
)
campos_relacion <- intersect(campos_relacion, names(familias))

# Función que extrae todas las URLs de Wikipedia de un campo de texto
extraer_urls <- function(x) {
  if (is.na(x) || trimws(as.character(x)) == "") return(character())
  x <- as.character(x)
  # Formato: (https://es.wikipedia.org/wiki/Slug)
  pat <- "\\(https://es\\.wikipedia\\.org/wiki/([^)]+)\\)"
  m <- str_match_all(x, pat)[[1]]
  if (nrow(m) > 0) return(unique(paste0("https://es.wikipedia.org/wiki/", m[, 2])))
  pat2 <- "https://es\\.wikipedia\\.org/wiki/[^\\s)]+"
  urls <- str_extract_all(x, pat2)[[1]]
  unique(trimws(urls))
}

# Palabras/claves por país (para inferir país en texto de relaciones y en campos diplomáticos)
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

# Inferir país a partir de un texto (ej. "actriz argentina", "Santiago, Chile")
inferir_pais_en_texto <- function(texto, excluir_pais = NULL) {
  if (is.na(texto) || str_trim(as.character(texto)) == "") return(NA_character_)
  t <- tolower(str_trim(as.character(texto)))
  for (pais in names(keywords_por_pais)) {
    if (!is.null(excluir_pais) && pais == excluir_pais) next
    if (any(str_detect(t, fixed(keywords_por_pais[[pais]])))) return(pais)
  }
  NA_character_
}

# Construir aristas guardando texto de relación + biografía del origen (para inferir país del "to")
edges_global <- list()
for (i in seq_len(nrow(familias))) {
  url_origen <- familias$url[i]
  pais_origen <- familias$pais[i]
  # Texto amplio del origen (biografía, etc.) para inferir país del relacionado
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
    texto_origen  = paste(str_trim(replace_na(first(texto_origen), "")), collapse = " "),
    .groups = "drop"
  ) %>%
  mutate(
    # Unir relación + contexto del origen para inferir país del "to"
    texto_para_inferir = paste(texto_relacion, texto_origen, sep = " ")
  )

# País del destino para cada URL:
# 1) Si está en consolidado: preferir país inferido de nacionalidad/lugar_nac (si indica otro país), sino pais del consolidado.
# 2) Si no está en consolidado: inferir del texto de la relación + biografía del origen.
url_a_pais <- familias %>%
  distinct(url, .keep_all = TRUE) %>%
  select(url, pais, nacionalidad, lugar_nacimiento) %>%
  mutate(
    texto_nacionalidad = paste(
      replace_na(as.character(nacionalidad), ""),
      replace_na(as.character(lugar_nacimiento), ""),
      sep = " "
    ),
    pais_segun_nacionalidad = map_chr(texto_nacionalidad, inferir_pais_en_texto, excluir_pais = NULL)
  ) %>%
  mutate(
    # Para conexión entre países: si nacionalidad/lugar indican otro país, usarlo
    pais_destino = coalesce(pais_segun_nacionalidad, pais)
  ) %>%
  select(url, pais_destino)

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
# REDES DENTRO DE CADA PAÍS
# =============================================================================
# Por cada país: solo nodos y aristas donde AMBOS extremos están en ese país.
# Así medimos "quién une" y "qué familia une" dentro del país.

urls_por_pais <- familias %>% group_by(pais) %>% summarise(urls = list(unique(url)), .groups = "drop")
paises <- unique(familias$pais)
resultados_pais <- list()
top_conectores <- list()

for (p in paises) {
  urls_p <- urls_por_pais %>% filter(pais == p) %>% pull(urls) %>% `[[`(1)
  if (length(urls_p) == 0) next

  # Aristas internas: from y to en este país
  edges_p <- edges_global %>%
    filter(pais == p, from %in% urls_p, to %in% urls_p) %>%
    select(from, to)

  # Grafo no dirigido (las relaciones son bidireccionales para "unión")
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

  # Tabla de nodos: url, grado, betweenness, y nombre (para etiquetas)
  nodos_p <- tibble(
    url = V(g)$name,
    grado = as.integer(V(g)$degree),
    betweenness = as.numeric(V(g)$betweenness),
    pais = p
  ) %>%
    left_join(
      familias %>% distinct(url, .keep_all = TRUE) %>%
        select(url, nombre, familia),
      by = "url"
    ) %>%
    mutate(
      nombre = coalesce(nombre, str_replace_all(URLdecode(str_replace(str_extract(url, "(?<=wiki/).+"), "_", " ")), "%", " ")),
      # Familia: si no hay, dejar como "Sin familia" para agrupar
      familia = replace_na(trimws(as.character(familia)), "Sin familia"),
      familia = if_else(familia == "", "Sin familia", familia)
    )

  # Layout para dibujar la red (Fruchterman-Reingold: nodos conectados se acercan)
  if (ecount(g) > 0 && vcount(g) > 1) {
    set.seed(42)
    lay <- layout_with_fr(g, niter = 500)
    nodos_p$x <- lay[, 1]
    nodos_p$y <- lay[, 2]
  } else {
    nodos_p$x <- 0
    nodos_p$y <- 0
  }

  resultados_pais[[p]] <- list(grafo = g, nodos = nodos_p, n_edges = ecount(g), edges = edges_p)

  # Top conectores (personas con más aristas) para tablas y gráficos de barras
  top_conectores[[p]] <- nodos_p %>%
    filter(grado > 0) %>%
    distinct(url, .keep_all = TRUE) %>%
    slice_max(grado, n = 15, with_ties = TRUE) %>%
    mutate(ranking = row_number(), pais = p)
}

# Tabla única de top conectores (todas las personas que más unen por país)
top_conectores_df <- bind_rows(top_conectores)

# =============================================================================
# FAMILIAS QUE MÁS UNEN (dentro de cada país)
# =============================================================================
# Por país y familia: sumamos el grado de todos sus miembros. Las familias
# con mayor "grado total" son las que más conectan la red dentro del país.

familias_conexiones <- bind_rows(lapply(resultados_pais, function(r) r$nodos)) %>%
  filter(grado > 0) %>%
  group_by(pais, familia) %>%
  summarise(
    total_conexiones = sum(grado, na.rm = TRUE),
    n_miembros = n(),
    .groups = "drop"
  ) %>%
  group_by(pais) %>%
  slice_max(total_conexiones, n = 10, with_ties = TRUE) %>%
  ungroup() %>%
  mutate(
    pais_label = str_to_sentence(pais),
    familia_corto = if_else(nchar(familia) > 22, paste0(str_sub(familia, 1, 19), "..."), familia)
  )

# =============================================================================
# GRÁFICOS: PERSONAS Y FAMILIAS QUE MÁS UNEN (barras + redes por país)
# =============================================================================

dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs/tables", showWarnings = FALSE, recursive = TRUE)

if (nrow(top_conectores_df) > 0) {
  top_conectores_df <- top_conectores_df %>%
    mutate(
      pais_label = str_to_sentence(pais),
      nombre_corto = if_else(nchar(nombre) > 25, paste0(str_sub(nombre, 1, 22), "..."), nombre)
    )

  # --- Gráfico de barras: personas que más unen por país ---
  p_barras <- ggplot(top_conectores_df, aes(x = fct_reorder(nombre_corto, grado), y = grado, fill = pais_label)) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    facet_wrap(~ pais_label, scales = "free_y", ncol = 2) +
    labs(
      x = NULL,
      y = "Nº de conexiones (personas relacionadas)",
      title = "Quiénes más unen dentro de cada país",
      subtitle = "Red por perfiles relacionados (padres, cónyuge, hijos, hermanos, familia). Solo conexiones dentro del mismo país."
    ) +
    theme_minimal(base_size = 10) +
    theme(strip.text = element_text(face = "bold"), plot.title = element_text(face = "bold"), panel.grid.major.y = element_blank())
  ggsave("outputs/figures/exploracion_conectores_por_pais.png", p_barras, width = 10, height = 12, dpi = 150)
  message("Guardado: outputs/figures/exploracion_conectores_por_pais.png")

  # --- Gráfico de barras: FAMILIAS que más unen por país ---
  if (nrow(familias_conexiones) > 0) {
    p_familias <- ggplot(familias_conexiones, aes(x = fct_reorder(familia_corto, total_conexiones), y = total_conexiones, fill = pais_label)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      facet_wrap(~ pais_label, scales = "free_y", ncol = 2) +
      labs(
        x = NULL,
        y = "Total conexiones (suma de grados de sus miembros)",
        title = "Familias que más unen dentro de cada país",
        subtitle = "Agregado por familia: cuántas conexiones aportan sus miembros a la red de perfiles relacionados."
      ) +
      theme_minimal(base_size = 10) +
      theme(strip.text = element_text(face = "bold"), plot.title = element_text(face = "bold"), panel.grid.major.y = element_blank())
    ggsave("outputs/figures/exploracion_familias_que_mas_unen.png", p_familias, width = 10, height = 12, dpi = 150)
    message("Guardado: outputs/figures/exploracion_familias_que_mas_unen.png")
  }

  write_csv(
    top_conectores_df %>% select(pais, nombre, url, grado, betweenness, ranking) %>% arrange(pais, desc(grado)),
    "outputs/tables/top_conectores_por_pais.csv"
  )
  if (nrow(familias_conexiones) > 0) {
    write_csv(familias_conexiones %>% arrange(pais, desc(total_conexiones)), "outputs/tables/familias_que_mas_unen_por_pais.csv")
  }
}

# Resumen numérico por país
resumen <- tibble(
  pais = paises,
  n_nodos = purrr::map_int(paises, function(pp) vcount(resultados_pais[[pp]]$grafo)),
  n_aristas = purrr::map_int(paises, function(pp) resultados_pais[[pp]]$n_edges)
) %>%
  mutate(densidad = if_else(n_nodos > 1, round(2 * n_aristas / (n_nodos * (n_nodos - 1)), 4), NA_real_))
message("\nResumen redes por país:")
print(resumen)

# =============================================================================
# REDES GRÁFICAS POR PAÍS (nodos coloreados por FAMILIA)
# =============================================================================
# Cada panel muestra la red interna del país; el color del nodo indica la
# familia. Así se ve qué familias concentran más conexiones.

all_nodes_plot <- bind_rows(lapply(resultados_pais, function(r) r$nodos)) %>%
  mutate(
    pais_label = str_to_sentence(pais),
    nombre_corto = if_else(nchar(nombre) > 28, paste0(str_sub(nombre, 1, 25), "..."), nombre)
  )

# Etiquetar solo a los principales conectores por país
top_para_label <- top_conectores_df %>%
  filter(ranking <= 8) %>%
  distinct(url, .keep_all = TRUE) %>%
  select(url, nombre_corto)
all_nodes_plot <- all_nodes_plot %>%
  left_join(top_para_label %>% rename(nombre_label = nombre_corto), by = "url") %>%
  mutate(es_top = !is.na(nombre_label))

# Aristas con coordenadas para dibujar segmentos
edges_con_coords <- list()
for (p in paises) {
  ed <- resultados_pais[[p]]$edges
  if (is.null(ed) || nrow(ed) == 0) next
  nod <- resultados_pais[[p]]$nodos %>% select(url, x, y)
  ed <- ed %>%
    left_join(nod, by = c("from" = "url")) %>% rename(x_from = x, y_from = y) %>%
    left_join(nod, by = c("to" = "url"))   %>% rename(x_to = x, y_to = y) %>%
    mutate(pais = p, pais_label = str_to_sentence(p))
  edges_con_coords[[length(edges_con_coords) + 1]] <- ed
}
all_edges_plot <- bind_rows(edges_con_coords)

# Paleta de colores por país (para leyenda) y muchas familias (para color de nodo)
country_colors <- c(
  "Argentina" = "#6CACE4", "Bolivia" = "#007A33", "Chile" = "#0033A0",
  "Colombia" = "#FCD116", "Ecuador" = "#FFD100", "Mexico" = "#006847", "México" = "#006847",
  "Paraguay" = "#D52B1E", "Peru" = "#D91023", "Perú" = "#D91023", "Uruguay" = "#0038A8", "Venezuela" = "#CF142B"
)
# Familias: usar muchas categorías; las que no estén tendrán gris
familias_unicas <- unique(all_nodes_plot$familia)
n_fam <- length(familias_unicas)
# Paleta amplia para familias (repetir si hace falta)
base_cols <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf",
  "#aec7e8", "#ffbb78", "#98df8a", "#ff9896", "#c5b0d5", "#c49c94", "#f7b6d2", "#c7c7c7", "#dbdb8d", "#9edae5")
familia_colors <- setNames(rep(base_cols, length.out = n_fam), familias_unicas)
familia_colors["Sin familia"] <- "gray75"

if (nrow(all_edges_plot) > 0) {
  # Facet: una red por país; sin color por familia (solo tamaño = grado)
  nodos_con_label <- all_nodes_plot %>% filter(es_top)
  p_facet <- ggplot() +
    geom_segment(data = all_edges_plot, aes(x = x_from, y = y_from, xend = x_to, yend = y_to), color = "gray65", alpha = 0.35, linewidth = 0.25) +
    geom_point(data = all_nodes_plot, aes(x = x, y = y, size = grado + 0.5), color = "steelblue", alpha = 0.75, shape = 19)
  # Etiquetas de top conectores (geom_text con overlap para evitar error de viewport en facet)
  if (nrow(nodos_con_label) > 0) {
    p_facet <- p_facet +
      geom_text(data = nodos_con_label, aes(x = x, y = y, label = nombre_label), size = 2, check_overlap = TRUE, hjust = -0.1, vjust = 0)
  }
  p_facet <- p_facet +
    scale_size_continuous(range = c(0.8, 4), name = "Conexiones") +
    facet_wrap(~ pais_label, scales = "free", ncol = 2) +
    labs(
      title = "Redes por país (personas relacionadas)",
      subtitle = "Tamaño = número de conexiones. Color por familia en el gráfico dedicado por país."
    ) +
    theme_void(base_size = 10) +
    theme(strip.text = element_text(face = "bold", size = 11), plot.title = element_text(face = "bold", hjust = 0.5), plot.subtitle = element_text(hjust = 0.5, color = "gray40"), legend.position = "right")
  ggsave("outputs/figures/exploracion_redes_facet.png", p_facet, width = 12, height = 14, dpi = 150)
  message("Guardado: outputs/figures/exploracion_redes_facet.png")

  # Una figura por país (mismo estilo, color por familia)
  dir.create("outputs/figures/exploracion_redes_pais", showWarnings = FALSE, recursive = TRUE)
  for (p in paises) {
    r <- resultados_pais[[p]]
    if (is.null(r$edges) || nrow(r$edges) == 0) next
    nodos_p <- r$nodos
    edges_p <- r$edges
    g_tbl <- as_tbl_graph(r$grafo) %>%
      activate(nodes) %>%
      left_join(nodos_p %>% select(url, x, y, nombre, grado, familia), by = c("name" = "url")) %>%
      mutate(
        nombre_corto = if_else(nchar(nombre) > 30, paste0(str_sub(nombre, 1, 27), "..."), nombre),
        es_destacado = grado >= max(1, quantile(grado[grado > 0], 0.85, na.rm = TRUE), na.rm = TRUE)
      )
    pais_lab <- str_to_sentence(p)
    p_pais <- ggraph(g_tbl, layout = "manual", x = x, y = y) +
      geom_edge_link(alpha = 0.25, color = "gray60", linewidth = 0.3) +
      geom_node_point(aes(size = grado + 0.5, color = familia), alpha = 0.8) +
      geom_node_text(aes(label = if_else(es_destacado, nombre_corto, "")), size = 2.5, repel = FALSE) +
      scale_color_manual(values = familia_colors, name = "Familia", na.value = "gray50") +
      scale_size_continuous(range = c(1, 6), name = "Grado") +
      labs(title = paste0("Red de familias — ", pais_lab), subtitle = paste0(nrow(nodos_p), " nodos, ", nrow(edges_p), " conexiones. Color = familia.")) +
      theme_void(base_size = 11) +
      theme(plot.title = element_text(face = "bold", hjust = 0.5), plot.subtitle = element_text(hjust = 0.5, color = "gray40", size = 9), legend.position = "right")
    ggsave(paste0("outputs/figures/exploracion_redes_pais/red_", p, ".png"), p_pais, width = 10, height = 8, dpi = 150)
  }
  message("Guardado: outputs/figures/exploracion_redes_pais/red_<pais>.png")
}

# =============================================================================
# UNIÓN ENTRE PAÍSES: FAMILIAR Y DIPLOMÁTICA/PROFESIONAL
# =============================================================================
# 1) Familiar: aristas donde el origen está en un país y el destino en otro
#    (relación de parentesco que cruza frontera).
# 2) Diplomática/profesional: inferida de texto (lugar de fallecimiento,
#    residencia, lugar de nacimiento, cargos, ocupación, biografía) cuando
#    se menciona otro país.

# --- 1) Conexiones familiares entre países (100% automático) ---
# pais_destino = consolidado (con país por nacionalidad/lugar_nac si aplica) o inferido del texto.
# Si el relacionado tiene país distinto al origen → conexión entre países.
cross_familia <- edges_con_pais_destino %>%
  filter(!is.na(pais_destino), pais != pais_destino) %>%
  select(from, to, pais_origen = pais, pais_destino)

# --- 2) Conexiones diplomáticas/profesionales (texto) ---
# Usa el mismo keywords_por_pais definido arriba.
campos_vinculo <- c("lugar_fallecimiento", "residencia", "lugar_nacimiento", "cargos_politicos", "ocupacion", "biografia_inicial")
campos_vinculo <- intersect(campos_vinculo, names(familias))

# Por persona y país distinto al suyo: ¿aparece el país en algún campo de texto?
cross_diplomatica <- list()
for (i in seq_len(nrow(familias))) {
  pais_persona <- trimws(tolower(as.character(familias$pais[i])))
  url_persona <- familias$url[i]
  nombre_persona <- familias$nombre[i]
  for (campo in campos_vinculo) {
    texto <- tolower(paste(as.character(familias[[campo]][i]), collapse = " "))
    if (is.na(texto) || nchar(trimws(texto)) == 0) next
    for (pais_otro in names(keywords_por_pais)) {
      if (pais_otro == pais_persona) next
      kws <- keywords_por_pais[[pais_otro]]
      if (any(sapply(kws, function(k) str_detect(texto, fixed(k))))) {
        tipo <- case_when(
          campo == "lugar_fallecimiento" ~ "fallecimiento",
          campo == "residencia" ~ "residencia",
          campo == "lugar_nacimiento" ~ "nacimiento",
          campo %in% c("cargos_politicos", "ocupacion") ~ "trabajo",
          TRUE ~ "biografía"
        )
        cross_diplomatica[[length(cross_diplomatica) + 1]] <- tibble(
          url = url_persona,
          nombre = nombre_persona,
          pais_origen = familias$pais[i],
          pais_vinculo = pais_otro,
          tipo_vinculo = tipo,
          campo_detectado = campo
        )
      }
    }
  }
}
cross_diplomatica_df <- bind_rows(cross_diplomatica) %>%
  distinct(url, pais_origen, pais_vinculo, tipo_vinculo, .keep_all = TRUE)

# =============================================================================
# GRÁFICO: UNIÓN ENTRE PAÍSES (familiares + diplomáticas)
# =============================================================================
# Pipeline tidyverse:
#   1. count(familia) + count(diplomática) por par (origen, destino)
#   2. full_join → replace_na(0) → filter(n_total > 0)
#   3. Grafo con layout → v_df (nodos con x, y) → e_df (aristas con coords)
#   4. drop_na en aristas para no dibujar NA → ggplot

agg_familia <- cross_familia %>%
  count(pais_origen, pais_destino, name = "n_familia")

agg_diplo <- cross_diplomatica_df %>%
  count(pais_origen, pais_vinculo, name = "n_diplomatica") %>%
  rename(pais_destino = pais_vinculo)

edges_paises <- agg_familia %>%
  full_join(agg_diplo, by = c("pais_origen", "pais_destino")) %>%
  replace_na(list(n_familia = 0L, n_diplomatica = 0L)) %>%
  mutate(n_total = n_familia + n_diplomatica) %>%
  filter(n_total > 0)

# Solo graficar si hay al menos un par de países con vínculos
if (nrow(edges_paises) >= 1L) {
  # Nodos = países únicos que aparecen en origen o destino
  paises_red <- edges_paises %>%
    pull(pais_origen) %>%
    c(edges_paises %>% pull(pais_destino)) %>%
    unique()

  # Grafo solo con aristas con peso para calcular layout
  g_paises <- edges_paises %>%
    select(pais_origen, pais_destino, n_familia, n_diplomatica, n_total) %>%
    graph_from_data_frame(directed = TRUE, vertices = tibble(name = paises_red))

  # Posiciones de nodos (layout)
  set.seed(42)
  lay_paises <- g_paises %>%
    layout_with_fr()

  # Tabla de nodos con coordenadas (una fila por país)
  v_df <- tibble(
    pais = V(g_paises)$name,
    x    = lay_paises[, 1],
    y    = lay_paises[, 2]
  ) %>%
    mutate(pais_label = str_to_sentence(pais))

  # Tabla de aristas con coordenadas: join por nombre de país, eliminar NA
  e_df <- edges_paises %>%
    select(pais_origen, pais_destino, n_total, n_familia, n_diplomatica) %>%
    left_join(
      v_df %>% select(pais, x, y),
      by = c("pais_origen" = "pais")
    ) %>%
    rename(x_from = x, y_from = y) %>%
    left_join(
      v_df %>% select(pais, x, y),
      by = c("pais_destino" = "pais")
    ) %>%
    rename(x_to = x, y_to = y) %>%
    drop_na(x_from, y_from, x_to, y_to)

  # Gráfico: solo segmentos con coordenadas completas
  p_cross <- ggplot() +
    geom_segment(
      data = e_df,
      aes(x = x_from, y = y_from, xend = x_to, yend = y_to, linewidth = n_total),
      color = "gray40",
      alpha = 0.7
    ) +
    geom_point(
      data = v_df,
      aes(x = x, y = y),
      size = 12,
      fill = "steelblue",
      color = "white",
      shape = 21,
      stroke = 1.5
    ) +
    geom_text(
      data = v_df,
      aes(x = x, y = y, label = pais_label),
      size = 3.2,
      fontface = "bold"
    ) +
    scale_linewidth_continuous(range = c(0.3, 2.5), name = "Vínculos (fam. + dipl.)") +
    labs(
      title = "Unión entre países: vínculos familiares y diplomáticos/profesionales",
      subtitle = "Aristas = personas con familia en otro país o vínculo por fallecimiento, residencia, trabajo. Grosor = cantidad."
    ) +
    theme_void(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5, color = "gray40")
    )

  ggsave("outputs/figures/exploracion_union_entre_paises.png", p_cross, width = 10, height = 8, dpi = 150)
  message("Guardado: outputs/figures/exploracion_union_entre_paises.png")
}

# Tablas de salida para conexiones entre países
write_csv(cross_familia, "outputs/tables/conexiones_familiares_entre_paises.csv")
write_csv(cross_diplomatica_df, "outputs/tables/conexiones_diplomaticas_entre_paises.csv")
message("Guardado: outputs/tables/conexiones_familiares_entre_paises.csv y conexiones_diplomaticas_entre_paises.csv")
