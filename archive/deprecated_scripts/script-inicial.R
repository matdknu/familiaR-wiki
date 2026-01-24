#==============================
# PAQUETES NECESARIOS
#==============================
pacman::p_load(
  tidyverse, janitor, data.table, readr, stringr,
  httr, jsonlite, igraph, ggraph, RColorBrewer
)

#==============================
# (1) CARGAR Y LIMPIAR BASE
#==============================
data <- read_delim("scripts/scrapping/familias_peru.csv", 
                     delim = ";", escape_double = FALSE, trim_ws = TRUE) |> 
  clean_names() |> 
  rename(id = url) |> 
  select(nombre, familia, id) |> 
  mutate(row_id = row_number())

#==============================
# (2) PROCESAR VÍNCULOS FAMILIARES
#==============================
data_familiares <- data |> 
  select(row_id, familia) |> 
  separate_rows(familia, sep = ";") |> 
  mutate(
    familia = str_trim(familia),
    familia = str_remove_all(familia, "\\[.*?\\]"), # quitar referencias
    familia = str_remove_all(familia, "\\(https?[^\\)]*\\)"), # quitar links
    familia = if_else(str_detect(familia, regex("familia|nupcias", ignore_case = TRUE)),
                      NA_character_, familia),
    familia = str_trim(familia)
  ) |> 
  #filter(!is.na(familia), familia != "") |> 
  group_by(row_id) |> 
  mutate(fam_n = paste0("familiar", row_number())) |> 
  pivot_wider(names_from = fam_n, values_from = familia) |> 
  ungroup()

data_final <- left_join(data, data_familiares, by = "row_id") |> 
  select(-row_id)


data_final <- data_final |> 
  mutate(nombre = str_remove_all(nombre, "\\s*\\([^\\)]+\\)")) 


#familia <- read_excel("data/raw_data/familia_link_manual2.xlsx")


data_final


# Cargar paquetes necesarios
library(tidyverse)
library(igraph)

# Cargar paquetes necesarios
library(tidyverse)
library(igraph)
library(ggraph)

# Detectar columnas que contienen relaciones familiares
familiar_cols <- data_final %>% select(starts_with("familiar")) %>% names()

# Transformar a formato largo (origen - destino)
relaciones <- data_final %>%
  pivot_longer(cols = all_of(familiar_cols), names_to = "tipo", values_to = "familiar") %>%
  filter(!is.na(familiar), familiar != "") %>%
  select(from = nombre, to = familiar)

# Crear grafo
g <- graph_from_data_frame(relaciones, directed = TRUE)

# Visualizar con ggraph + ggplot2
ggraph(g, layout = "fr") +  # Layout tipo fuerza de repulsión
  geom_edge_link(alpha = 0.5, arrow = arrow(length = unit(2, 'mm')), end_cap = circle(3, 'mm')) +
  geom_node_point(size = 3, color = "steelblue") +
  geom_node_text(aes(label = name), repel = TRUE, size = 2.5) +
  theme_void()
