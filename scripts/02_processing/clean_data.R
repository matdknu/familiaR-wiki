library(readr)
library(tidyverse)
library(janitor)
library(DT)



datos_wikipedia <- read_csv("data/datos_wikipedia.csv") |> clean_names()

datos_wikipedia |> select(familia) 


library(tidyverse)

# Suponiendo que tienes un data.frame llamado df con una columna `familiares`
df <- tibble(familiares = c(
  "Ángela Jeria (https://es.wikipedia.org/wiki/%C3%81ngela_Jeria); Alberto Bachelet (https://es.wikipedia.org/wiki/Alberto_Bachelet); Michelle Bachelet Jeria",
  "Michelle Bachelet Jeria (https://es.wikipedia.org/wiki/Michelle_Bachelet); ; ;",
  "Familia Piñera; ; José Piñera (https://es.wikipedia.org/wiki/Jos%C3%A9_Pi%C3%B1era_Carvallo); José Piñera Echenique"
))




# Separar en columnas familiar1, familiar2, ..., familiarN
df_separado <- datos_wikipedia %>%
  separate_wider_delim(
    familia,
    delim = ";",
    names = paste0("familiar", 1:25), # puedes ajustar cuántas columnas máximas quieres
    too_few = "align_start",  # rellena con NA los que faltan
    too_many = "drop"         # si hay más de 10, los ignora
  ) %>% mutate(across(starts_with("familiar"), ~str_remove_all(.x, "\\s*\\(https?[^\\)]*\\)") %>% 
                        str_trim())) |> mutate(across(
  starts_with("familiar"),
  list(
    limpio = ~str_remove_all(.x, "\\s*\\([^\\)]*\\)") %>% str_trim(),
    parentesco = ~str_extract(.x, "\\(([^\\)]*)\\)") %>% str_remove_all("[\\(\\)]") %>% str_trim()
  ),
  .names = "{.col}_{.fn}"
))

library(DT)

datatable(df_separado, editable = TRUE)
