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
data <- read_csv("datos_chile_manual2.csv") |> 
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
  filter(!is.na(familia), familia != "") |> 
  group_by(row_id) |> 
  mutate(fam_n = paste0("familiar", row_number())) |> 
  pivot_wider(names_from = fam_n, values_from = familia) |> 
  ungroup()

data_final <- left_join(data, data_familiares, by = "row_id") |> 
  select(-row_id)

#==============================
# (3) FILTRAR SOLO FAMILIA KAST
#==============================
data_final <- data_final |> 
  #filter(str_detect(nombre, "Kast")) |> 
  select(nombre, id, starts_with("familiar"))

#==============================
# (4) CONFIGURAR API OPENAI
#==============================
my_API <- "TU_API_KEY_AQUI" # <- reemplaza con tu clave de OpenAI

hey_chatGPT <- function(prompt_text) {
  response <- POST(
    url = "https://api.openai.com/v1/chat/completions",
    add_headers(Authorization = paste("Bearer", my_API)),
    content_type_json(),
    encode = "json",
    body = list(
      model = "gpt-3.5-turbo",
      messages = list(
        list(role = "system", content = "Eres un experto en genealogía chilena."),
        list(role = "user", content = prompt_text)
      ),
      temperature = 0.2
    )
  )
  
  if (http_error(response)) {
    warning("Error al llamar a la API: ", content(response)$error$message)
    return(NA)
  }
  
  content(response)$choices[[1]]$message$content |> str_trim()
}

#==============================
# (5) CONSTRUIR PROMPTS Y ENVIAR A GPT
#==============================
nombres_completos <- data_final$nombre |> unique() |> str_squish()
nombres_lista <- paste("- ", nombres_completos, collapse = "\n")

resultados <- list()

for (i in 1:nrow(data_final)) {
  persona <- data_final$nombre[i]
  
  familiares <- data_final[i, str_detect(names(data_final), "^familiar")] |>
    unlist() |>
    na.omit() |>
    unique() |>
    str_remove_all("\\(.*?\\)") |>
    str_squish()
  
  if (length(familiares) == 0) next
  
  familiares_lista <- paste("- ", familiares, collapse = "\n")
  
  prompt <- paste0(
    "Actúa como un experto en genealogía chilena. Vas a procesar una fila de una base de datos. Tu tarea es:\n\n",
    "1. Identificar a quién corresponde cada nombre en la lista de familiares, usando como referencia los nombres completos disponibles.\n",
    "2. Reemplazar las menciones ambiguas (como \"Pablo\", \"Felipe\" o \"Luis\") por el nombre completo correcto.\n",
    "3. Detectar vínculos recíprocos faltantes: si A menciona a B, asegúrate de que B también mencione a A.\n\n",
    "Lista de personas conocidas:\n", nombres_lista, "\n\n",
    "Caso a procesar:\n",
    "Persona: ", persona, "\n",
    "Familiares mencionados:\n", familiares_lista, "\n\n",
    "Devuelve el resultado en este formato:\n\n",
    "Persona: [nombre completo]\n",
    "Familiares estandarizados:\n- ...\n",
    "Relaciones recíprocas necesarias:\n- ...\n"
  )
  
  cat("\n⏳ Procesando: ", persona, "\n")
  respuesta <- hey_chatGPT(prompt)
  resultados[[persona]] <- list(prompt = prompt, respuesta = respuesta)
  Sys.sleep(2)
}

 #==============================
# (6) PARSEAR RESPUESTAS DE GPT
#==============================
relaciones <- map_dfr(names(resultados), function(p) {
  texto <- resultados[[p]]$respuesta
  
  if (is.na(texto) || !str_detect(texto, "Familiares estandarizados:")) return(NULL)
  
  familiares <- str_match_all(texto, "Familiares estandarizados:\\n((?:- .+\\n)+)")[[1]][,2] |>
    str_split("\\n") |> unlist() |> str_remove("- ") |> str_squish() |> na.omit()
  
  recips <- str_match_all(texto, "Relaciones recíprocas necesarias:\\n((?:- .+\\n*)+)")[[1]][,2] |>
    str_split("\\n") |> unlist() |> str_remove("- ") |> str_squish() |> na.omit()
  
  tibble(
    persona = p,
    familiar_estandarizado = familiares
  ) |> bind_rows(
    tibble(
      persona = str_extract(recips, "(?<=→ ).+"),
      familiar_estandarizado = str_extract(recips, "^[^→]+")
    )
  )
})

#==============================
# (7) ACTUALIZAR BASE CON RELACIONES
#==============================
data_actualizada <- data_final

for (i in seq_len(nrow(relaciones))) {
  persona <- relaciones$persona[i]
  familiar <- relaciones$familiar_estandarizado[i]
  
  idx <- which(data_actualizada$nombre == persona)
  if (length(idx) == 0) next
  
  ya_existe <- familiar %in% unlist(data_actualizada[idx, str_detect(names(data_actualizada), "^familiar")])
  if (!ya_existe) {
    cols_fam <- str_detect(names(data_actualizada), "^familiar")
    pos_vacia <- which(is.na(data_actualizada[idx, cols_fam]))[1]
    if (!is.na(pos_vacia)) {
      col_name <- names(data_actualizada)[which(cols_fam)[pos_vacia]]
      data_actualizada[[col_name]][idx] <- familiar
    } else {
      warning("No hay espacio para añadir familiar a ", persona)
    }
  }
}

# Resultado final:
data_actualizada

