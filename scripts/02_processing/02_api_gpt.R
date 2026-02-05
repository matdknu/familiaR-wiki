# =============================================================================
# 02_api_gpt.R — Enriquecer df_consolidado con la API de OpenAI (GPT)
# =============================================================================
# REGLA DE CLARIDAD PARA REDES:
# - GPT solo se usa para CARGOS (y partido, fechas). El JSON pide solo:
#   cargo_1, cargo_2, cargo_3, partido_limpio, nacimiento_ano, fallecimiento_ano.
# - Las conexiones de red (hijos, padres, cónyuge, hermanos) NO se tocan:
#   se mantienen las columnas originales del consolidado (con URLs de Wikipedia).
# - Se mantiene la lógica por origen: pais, pais_origen, familia, etc. no se modifican;
#   el output tiene todas las columnas del consolidado más las columnas GPT.
#
# API key: pégala en OPENAI_API_KEY_SCRIPT abajo, o usa Sys.setenv / .env (GitHub vacío).
#
# Uso: source("scripts/02_processing/02_api_gpt.R")
#      enriquecer_con_gpt(muestra = 5)   # prueba
#      enriquecer_con_gpt()              # todo
# =============================================================================

# API key: vacía en repo. En local usa .env (OPENAI_API_KEY=...) o crea api.R en la raíz
# con la key (api.R está en .gitignore). Si existe api.R se lee desde ahí.
OPENAI_API_KEY_SCRIPT <- ""
if (file.exists("api.R")) {
  OPENAI_API_KEY_SCRIPT <- trimws(paste(readLines("api.R", warn = FALSE), collapse = ""))
}

library(httr)
library(jsonlite)
library(readr)
library(dplyr)
library(purrr)

# Si no hay key en el script, intentar .env
if (nzchar(trimws(OPENAI_API_KEY_SCRIPT))) {
  Sys.setenv(OPENAI_API_KEY = OPENAI_API_KEY_SCRIPT)
} else if (file.exists(".env")) {
  env_lines <- readLines(".env", warn = FALSE)
  for (line in env_lines) {
    line <- trimws(line)
    if (grepl("^OPENAI_API_KEY\\s*=", line)) {
      key <- sub("^OPENAI_API_KEY\\s*=\\s*['\"]?", "", line)
      key <- sub("['\"]?\\s*$", "", key)
      if (nchar(key) > 0) Sys.setenv(OPENAI_API_KEY = key)
      break
    }
  }
}

# Rutas
BASE_DIR <- "outputs"
DF_IN <- file.path(BASE_DIR, "df_consolidado.csv")
DF_OUT <- file.path(BASE_DIR, "df_consolidado_enriquecido_gpt.csv")

SYSTEM_PROMPT <- "Eres un asistente que extrae solo CARGOS y datos básicos de biografías. Responde ÚNICAMENTE con un JSON válido, sin markdown. Usa null para lo que no encuentres. Años como número (ej: 1770)."

# Solo cargos + partido + fechas. Las redes (hijos, padres, cónyuge) se mantienen del consolidado.
USER_PROMPT_TEMPLATE <- "De esta ficha, extrae ÚNICAMENTE un JSON con estas 6 claves (nada más):
- cargo_1, cargo_2, cargo_3: los tres cargos más relevantes (políticos, militares, institucionales), normalizados y breves (ej: Presidente de Chile, Senador). Si hay menos, usa null.
- partido_limpio: partido político si se menciona, sino null.
- nacimiento_ano: año de nacimiento (número) o null.
- fallecimiento_ano: año de fallecimiento (número) o null.

(No extraigas hijos ni redes: eso ya está en los datos y se usa para la red de conexiones.)

Ficha:
Nombre: %s
País: %s
Nacionalidad: %s
Biografía (inicio): %s
Cargos (crudo): %s
Partido (crudo): %s
Nacimiento: %s
Fallecimiento: %s"

safe_str <- function(x) {
  if (is.null(x) || is.na(x) || length(x) == 0 || trimws(as.character(x)) == "") return("")
  substr(trimws(as.character(x)), 1, 2000)
}

#' Llama a la API de OpenAI Chat Completions.
#' Usa, en orden: argumento api_key, variable de entorno OPENAI_API_KEY, o OPENAI_API_KEY_SCRIPT (pegada en el script).
llamar_gpt <- function(prompt_user, api_key = NULL, model = "gpt-4o-mini") {
  key <- NULL
  if (!is.null(api_key) && length(api_key) > 0 && nzchar(trimws(api_key))) {
    key <- trimws(api_key)
  } else {
    key <- Sys.getenv("OPENAI_API_KEY")
    if (is.null(key) || !nzchar(trimws(key))) {
      if (exists("OPENAI_API_KEY_SCRIPT") && nzchar(trimws(OPENAI_API_KEY_SCRIPT))) {
        key <- trimws(OPENAI_API_KEY_SCRIPT)
      }
    }
  }
  if (is.null(key) || !nzchar(key)) {
    stop("Define OPENAI_API_KEY: pégala en OPENAI_API_KEY_SCRIPT (línea ~17), o Sys.setenv('OPENAI_API_KEY'='...'), o export en la terminal.")
  }
  body <- list(
    model = model,
    messages = list(
      list(role = "system", content = SYSTEM_PROMPT),
      list(role = "user", content = prompt_user)
    ),
    temperature = 0.1,
    max_tokens = 500
  )
  r <- POST(
    "https://api.openai.com/v1/chat/completions",
    add_headers(Authorization = paste0("Bearer ", key), `Content-Type` = "application/json"),
    body = body,
    encode = "json"
  )
  if (http_error(r)) {
    msg <- content(r, "text")
    if (status_code(r) == 401) {
      msg <- paste0(msg, " [Usa una API key de https://platform.openai.com/account/api-keys, no de Cursor/sk-proj-]")
    }
    return(list(error = paste0(status_code(r), ": ", msg)))
  }
  out <- content(r, "parsed")
  text <- out$choices[[1]]$message$content
  list(content = trimws(text))
}

#' Parsea JSON desde la respuesta (puede venir en ```json ... ```)
parsear_json_respuesta <- function(text) {
  if (is.null(text) || trimws(text) == "") return(list())
  text <- trimws(text)
  text <- sub("^```(?:json)?\\s*", "", text)
  text <- sub("\\s*```$", "", text)
  text <- trimws(text)
  start <- regexpr("\\{", text)
  if (start < 0) return(list())
  txt <- substr(text, start, nchar(text))
  chars <- strsplit(txt, "")[[1]]
  depth <- 0
  end <- 0
  for (i in seq_along(chars)) {
    if (chars[i] == "{") depth <- depth + 1
    if (chars[i] == "}") {
      depth <- depth - 1
      if (depth == 0) { end <- i; break }
    }
  }
  if (end == 0) return(list())
  json_str <- paste(chars[seq_len(end)], collapse = "")
  tryCatch(fromJSON(json_str), error = function(e) list())
}

#' Enriquece una fila del consolidado vía API.
enriquecer_fila <- function(row, api_key = NULL, model = "gpt-4o-mini", delay = 0.5) {
  prompt <- sprintf(
    USER_PROMPT_TEMPLATE,
    safe_str(row$nombre),
    safe_str(row$pais),
    safe_str(row$nacionalidad),
    safe_str(row$biografia_inicial),
    safe_str(row$cargos),
    safe_str(row$partido_politico),
    safe_str(row$fecha_nacimiento),
    safe_str(row$fecha_fallecimiento)
  )
  resp <- llamar_gpt(prompt, api_key = api_key, model = model)
  if (!is.null(resp$error)) {
    return(list(`_error` = resp$error))
  }
  Sys.sleep(delay)
  parsear_json_respuesta(resp$content)
}

#' Enriquecer df_consolidado con GPT y guardar resultado.
#' @param muestra Número de filas a procesar (0 = todas)
#' @param api_key Opcional; si no, usa OPENAI_API_KEY
#' @param model Modelo OpenAI
#' @param delay Segundos entre llamadas
#' @param guardar Si TRUE, escribe DF_OUT
enriquecer_con_gpt <- function(muestra = 0, api_key = NULL, model = "gpt-4o-mini", delay = 0.5, guardar = TRUE) {
  if (!file.exists(DF_IN)) {
    stop("No existe ", DF_IN, ". Ejecuta antes leer_consolidado.R para generar el CSV.")
  }
  df <- read_csv(DF_IN, show_col_types = FALSE)
  if (muestra > 0 && muestra < nrow(df)) {
    df <- df %>% slice_head(n = muestra)
  }
  n <- nrow(df)
  message("Enriqueciendo ", n, " filas con modelo ", model, " ...")

  # Solo escribimos en el df lo que viene de GPT (cargos, partido, fechas).
  # Las columnas padres, conyuge, hijos, hermanos se mantienen del consolidado para las redes.
  extra_cols <- c("cargo_1", "cargo_2", "cargo_3", "partido_limpio", "nacimiento_ano", "fallecimiento_ano")
  for (c in extra_cols) {
    if (!c %in% names(df)) df[[c]] <- NA_character_
  }
  df$nacimiento_ano <- NA_integer_
  df$fallecimiento_ano <- NA_integer_

  for (i in seq_len(n)) {
    row <- as.list(df[i, ])
    out <- enriquecer_fila(row, api_key = api_key, model = model, delay = delay)
    if (!is.null(out$`_error`)) {
      message("Fila ", i, ": ", out$`_error`)
      next
    }
    for (k in extra_cols) {
      if (k %in% names(out) && !is.null(out[[k]])) {
        v <- out[[k]]
        if (k %in% c("nacimiento_ano", "fallecimiento_ano") && is.numeric(v)) {
          df[[k]][i] <- as.integer(v)
        } else {
          df[[k]][i] <- if (is.character(v)) v else as.character(jsonlite::toJSON(v, auto_unbox = TRUE))
        }
      }
    }
    if (i %% 10 == 0) message("  ", i, "/", n)
  }

  if (guardar) {
    write_csv(df, DF_OUT)
    message("Guardado: ", DF_OUT)
    message("Conexiones para la red (sin tocar): padres, conyuge, hijos, hermanos, perfiles_relacionados_*")
  }
  invisible(df)
}


# Ejecución con argumentos (Rscript)
#   Rscript scripts/02_processing/02_api_gpt.R           # 5 filas de prueba
#   Rscript scripts/02_processing/02_api_gpt.R 20        # 20 filas
if (!interactive() && length(commandArgs(TRUE)) > 0) {
  args <- commandArgs(TRUE)
  n <- if (length(args) >= 1) as.integer(args[1]) else 5
  if (is.na(n)) n <- 30
  enriquecer_con_gpt(muestra = n)
}

enriquecer_con_gpt(muestra = 100)
