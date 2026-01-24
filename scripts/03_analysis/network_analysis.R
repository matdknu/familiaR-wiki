pacman::p_load(ggplot2, dplyr, tidyr, cowplot, ggpubr, ggrepel, scales,
               viridis, patchwork, gridExtra, grid, gtable, stringr,
               lubridate, purrr, readr, rlang, janitor)


library(readr)

# Leer y preparar datos
datos_wikipedia <- read_csv("datos_chile_manual2.csv") |> 
  clean_names() |> 
  rename(id = url) |> 
  select(nombre, familia, id) |> 
  mutate(row_id = row_number())




datos_wikipedia |> select(nombre, familia)
df_limpio <- datos_wikipedia %>%
  select(nombre, familia) %>%
  mutate(familia = str_split(familia, ";")) %>%  # separa por punto y coma
  unnest(familia) %>%
  mutate(familia = str_squish(familia)) %>%  # quita espacios extra
  filter(familia != "")  # elimina vacíos

df_limpio <- df_limpio %>%
  mutate(familia = str_remove_all(familia, "\\(.*?\\)"),  # quita todo entre paréntesis
         familia = str_remove_all(familia, "\\[.*?\\]"),  # quita referencias tipo [9]
         familia = str_squish(familia))  # limpia espacios

red_familiar <- df_limpio %>%
  filter(familia != "") %>%
  rename(source = nombre, target = familia)


library(ggraph)
library(tidygraph)

# Convertir a objeto tidygraph
g_tidy <- as_tbl_graph(red_familiar, directed = FALSE)

# Dibujar red con layout por componentes (force-directed)
library(ggraph)
library(tidygraph)

library(ggraph)
library(tidygraph)
library(dplyr)

# Convertimos a objeto de red
g_tidy <- as_tbl_graph(red_familiar, directed = FALSE) %>%
  # Detectamos comunidades (algoritmo infomap)
  mutate(comunidad = as.factor(group_infomap())) %>%
  # Calculamos tamaño de cada comunidad
  group_by(comunidad) %>%
  mutate(tamaño_comunidad = n()) %>%
  ungroup()

# Graficamos con color según tamaño de la comunidad (tipo heatmap)
g1 <- ggraph(g_tidy, layout = "fr") +
  geom_edge_link(alpha = 0.2, color = "gray70") +
  geom_node_point(aes(color = tamaño_comunidad), size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  scale_color_viridis_c(option = "inferno", direction = -1) +
  theme_void() +
  labs(
    title = "Red Familiar de Familias en Chile",
    color = "Tamaño\nComunidad"
  )
g1
ggsave(g1, filename = "images/red_familiar.png", width = 10, height = 8, dpi = 300)
1


