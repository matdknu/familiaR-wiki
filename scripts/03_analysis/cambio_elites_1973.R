# ============================================================================
# cambio_elites_1973.R
# Análisis del cambio de élites políticas en Chile después de 1973
# - Cambios en apellidos dominantes
# - Cambios en tipos de cargos
# - Transición temporal
# ============================================================================

library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(lubridate)

cat("📊 Analizando cambio de élites post-1973 en Chile...\n")

# Cargar datos consolidados de Chile
chile_data <- read_delim("data/raw/chile/familias/_CONSOLIDADO_todas_familias.csv", 
                         delim = ";", show_col_types = FALSE, locale = locale(encoding = "UTF-8"))

cat("✅ Datos cargados:", nrow(chile_data), "personas\n")

# Función para extraer apellidos del nombre
extract_surnames <- function(nombre) {
  if (is.na(nombre) || nombre == "") return(list(surname_1 = NA, surname_2 = NA))
  
  # Limpiar y dividir
  tokens <- str_split(nombre, "\\s+")[[1]]
  tokens <- tokens[tokens != ""]
  
  # Filtrar preposiciones comunes
  preposiciones <- c("de", "del", "la", "los", "las", "y", "e")
  tokens_clean <- tokens[!tolower(tokens) %in% preposiciones]
  
  if (length(tokens_clean) == 0) return(list(surname_1 = NA, surname_2 = NA))
  
  surname_1 <- tokens_clean[length(tokens_clean)]
  surname_2 <- if (length(tokens_clean) >= 2) tokens_clean[length(tokens_clean) - 1] else NA_character_
  
  # Filtrar apellidos inválidos (con paréntesis, muy cortos, números, etc.)
  if (!is.na(surname_1) && (
    str_detect(surname_1, "[\\(\\)]") || 
    nchar(surname_1) < 2 || 
    str_detect(surname_1, "\\d") ||
    tolower(surname_1) %in% c("chile", "argentina", "actor", "comediante", "político")
  )) {
    surname_1 <- NA_character_
  }
  
  if (!is.na(surname_2) && (
    str_detect(surname_2, "[\\(\\)]") || 
    nchar(surname_2) < 2 || 
    str_detect(surname_2, "\\d") ||
    tolower(surname_2) %in% c("chile", "argentina", "actor", "comediante", "político")
  )) {
    surname_2 <- NA_character_
  }
  
  list(surname_1 = surname_1, surname_2 = surname_2)
}

# Extraer apellidos
chile_data <- chile_data %>%
  rowwise() %>%
  mutate(
    surnames = list(extract_surnames(nombre)),
    surname_1 = surnames$surname_1,
    surname_2 = surnames$surname_2
  ) %>%
  ungroup()

# Función para extraer años de cargos políticos
extract_years_from_cargos <- function(cargos_text, periodo_text, biografia_text = NA) {
  years <- c()
  
  # Buscar años en formato YYYY
  pattern <- "\\b(1[789]\\d{2}|20[0-2]\\d)\\b"
  
  if (!is.na(cargos_text) && cargos_text != "") {
    matches <- str_extract_all(cargos_text, pattern)[[1]]
    if (length(matches) > 0) {
      years <- c(years, as.integer(matches))
    }
  }
  
  if (!is.na(periodo_text) && periodo_text != "") {
    matches <- str_extract_all(periodo_text, pattern)[[1]]
    if (length(matches) > 0) {
      years <- c(years, as.integer(matches))
    }
  }
  
  # También buscar en biografía si está disponible
  if (!is.na(biografia_text) && biografia_text != "") {
    matches <- str_extract_all(biografia_text, pattern)[[1]]
    if (length(matches) > 0) {
      years <- c(years, as.integer(matches))
    }
  }
  
  unique(years[!is.na(years) & years >= 1800 & years <= 2025])
}

# Extraer años de cargos para cada persona
cat("📅 Extrayendo años de cargos políticos...\n")
cargos_expanded <- list()
for (i in 1:nrow(chile_data)) {
  years <- extract_years_from_cargos(
    chile_data$cargos_politicos[i], 
    chile_data$periodo[i],
    chile_data$biografia[i]
  )
  
  if (length(years) > 0) {
    for (year in years) {
      cargos_expanded[[length(cargos_expanded) + 1]] <- tibble(
        person_id = i,
        nombre = chile_data$nombre[i],
        url = chile_data$url[i],
        surname_1 = chile_data$surname_1[i],
        surname_2 = chile_data$surname_2[i],
        cargo_text = chile_data$cargos_politicos[i],
        periodo_text = chile_data$periodo[i],
        year = year,
        periodo = case_when(
          year < 1973 ~ "Pre-1973",
          year >= 1973 & year < 1990 ~ "Dictadura (1973-1990)",
          year >= 1990 ~ "Post-dictadura (1990+)"
        )
      )
    }
  }
}

if (length(cargos_expanded) > 0) {
  cargos_df <- bind_rows(cargos_expanded)
  cat("✅ Cargos extraídos:", nrow(cargos_df), "registros\n")
} else {
  cat("⚠️ No se encontraron años en cargos políticos\n")
  cargos_df <- tibble()
}

# Si no hay cargos con años, usar fecha_nacimiento para clasificar personas
if (nrow(cargos_df) == 0) {
  cat("📅 Usando fecha_nacimiento para clasificar períodos...\n")
  
  chile_data <- chile_data %>%
    mutate(
      birth_year = as.integer(str_extract(fecha_nacimiento, "\\b(1[789]\\d{2}|20[0-2]\\d)\\b")),
      periodo_persona = case_when(
        !is.na(birth_year) & birth_year < 1940 ~ "Pre-1973",
        !is.na(birth_year) & birth_year >= 1940 & birth_year < 1960 ~ "Dictadura (1973-1990)",
        !is.na(birth_year) & birth_year >= 1960 ~ "Post-dictadura (1990+)",
        TRUE ~ "Desconocido"
      )
    )
  
  # Crear cargos_df basado en personas con cargos políticos
  cargos_df <- chile_data %>%
    filter(!is.na(cargos_politicos) & cargos_politicos != "") %>%
    mutate(
      year = birth_year,
      periodo = periodo_persona
    ) %>%
    select(person_id = row_number(), nombre, url, surname_1, surname_2, 
           cargo_text = cargos_politicos, periodo_text = periodo, year, periodo)
}

# Análisis 1: Cambios en apellidos dominantes por período
cat("\n📊 ANÁLISIS 1: Cambios en apellidos dominantes\n")
cat(strrep("=", 80), "\n")

apellidos_por_periodo <- tibble()
apellidos_unicos <- tibble()
apellidos_nuevos <- tibble()
apellidos_desaparecidos <- tibble()

if (nrow(cargos_df) > 0) {
  # Top apellidos por período
  apellidos_por_periodo <- cargos_df %>%
    filter(!is.na(surname_1) & surname_1 != "") %>%
    count(periodo, surname_1, sort = TRUE) %>%
    group_by(periodo) %>%
    mutate(rank = row_number()) %>%
    ungroup()
  
  # Top 15 apellidos por período
  if (nrow(apellidos_por_periodo) > 0) {
    top_apellidos_periodo <- apellidos_por_periodo %>%
      filter(rank <= 15) %>%
      arrange(periodo, rank)
    
    cat("\nTop 15 apellidos por período:\n")
    print(top_apellidos_periodo %>% 
          pivot_wider(names_from = periodo, values_from = c(n, rank), 
                     names_sep = "_", values_fill = list(n = 0, rank = 999)) %>%
          head(20))
    
    # Apellidos que aparecen solo en un período
    apellidos_unicos <- apellidos_por_periodo %>%
      group_by(surname_1) %>%
      summarise(
        n_periodos = n_distinct(periodo),
        periodos = paste(unique(periodo), collapse = ", "),
        total_cargos = sum(n),
        .groups = "drop"
      ) %>%
      filter(n_periodos == 1) %>%
      arrange(desc(total_cargos))
    
    cat("\n📌 Apellidos que aparecen SOLO en un período:\n")
    print(apellidos_unicos %>% head(20))
    
    # Apellidos nuevos post-1973
    apellidos_nuevos <- apellidos_unicos %>%
      filter(str_detect(periodos, "Dictadura|Post-dictadura"))
    
    cat("\n🆕 Apellidos NUEVOS post-1973:", nrow(apellidos_nuevos), "\n")
    if (nrow(apellidos_nuevos) > 0) {
      print(apellidos_nuevos %>% head(15))
    }
    
    # Apellidos que desaparecen post-1973
    apellidos_desaparecidos <- apellidos_unicos %>%
      filter(periodos == "Pre-1973")
    
    cat("\n❌ Apellidos que DESAPARECEN post-1973:", nrow(apellidos_desaparecidos), "\n")
    if (nrow(apellidos_desaparecidos) > 0) {
      print(apellidos_desaparecidos %>% head(15))
    }
  }
}

# Análisis 2: Cambios en tipos de cargos
cat("\n📊 ANÁLISIS 2: Cambios en tipos de cargos\n")
cat(strrep("=", 80), "\n")

if (nrow(cargos_df) > 0) {
  # Extraer tipos de cargos
  tipos_cargo <- c(
    "Presidente", "Ministro", "Senador", "Diputado", "Gobernador", 
    "Alcalde", "Embajador", "General", "Almirante", "Juez",
    "Ministro de Estado", "Secretario", "Intendente", "Subsecretario"
  )
  
  cargos_df <- cargos_df %>%
    mutate(
      cargo_tipo = case_when(
        str_detect(tolower(cargo_text), "presidente") ~ "Presidente",
        str_detect(tolower(cargo_text), "ministro") ~ "Ministro",
        str_detect(tolower(cargo_text), "senador") ~ "Senador",
        str_detect(tolower(cargo_text), "diputado") ~ "Diputado",
        str_detect(tolower(cargo_text), "gobernador") ~ "Gobernador",
        str_detect(tolower(cargo_text), "alcalde") ~ "Alcalde",
        str_detect(tolower(cargo_text), "embajador") ~ "Embajador",
        str_detect(tolower(cargo_text), "general|almirante") ~ "Militar",
        str_detect(tolower(cargo_text), "juez|tribunal") ~ "Judicial",
        TRUE ~ "Otro"
      )
    )
  
  tipos_por_periodo <- cargos_df %>%
    count(periodo, cargo_tipo, sort = TRUE) %>%
    group_by(periodo) %>%
    mutate(pct = round(100 * n / sum(n), 1)) %>%
    ungroup()
  
  cat("\nTipos de cargos por período:\n")
  print(tipos_por_periodo %>% 
        pivot_wider(names_from = periodo, values_from = c(n, pct), 
                   names_sep = "_", values_fill = list(n = 0, pct = 0)))
}

# Análisis 3: Diversidad de apellidos (índice de diversidad)
cat("\n📊 ANÁLISIS 3: Diversidad de apellidos\n")
cat(strrep("=", 80), "\n")

diversidad <- tibble()

if (nrow(cargos_df) > 0) {
  # Calcular diversidad
  apellidos_por_periodo_with_rank <- cargos_df %>%
    filter(!is.na(surname_1) & surname_1 != "") %>%
    count(periodo, surname_1, sort = TRUE) %>%
    group_by(periodo) %>%
    mutate(rank = row_number()) %>%
    ungroup()
  
  if (nrow(apellidos_por_periodo_with_rank) > 0) {
    diversidad <- cargos_df %>%
      filter(!is.na(surname_1) & surname_1 != "") %>%
      group_by(periodo) %>%
      summarise(
        n_personas = n_distinct(person_id),
        n_apellidos_unicos = n_distinct(surname_1),
        .groups = "drop"
      ) %>%
      left_join(
        apellidos_por_periodo_with_rank %>%
          filter(rank <= 10) %>%
          group_by(periodo) %>%
          summarise(top10_n = sum(n), .groups = "drop"),
        by = "periodo"
      ) %>%
      left_join(
        apellidos_por_periodo_with_rank %>%
          group_by(periodo) %>%
          summarise(total_n = sum(n), .groups = "drop"),
        by = "periodo"
      ) %>%
      mutate(
        apellidos_top10_pct = ifelse(!is.na(total_n) & total_n > 0, 
                                     round(100 * top10_n / total_n, 1), 0)
      )
    
    cat("\nDiversidad por período:\n")
    print(diversidad)
  }
}

# Visualizaciones
cat("\n🎨 Generando visualizaciones...\n")

dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs/tables", showWarnings = FALSE, recursive = TRUE)

# Gráfico 1: Top apellidos por período (heatmap)
if (nrow(cargos_df) > 0 && nrow(apellidos_por_periodo) > 0) {
  # Seleccionar top 20 apellidos en total
  top_20_apellidos <- apellidos_por_periodo %>%
    group_by(surname_1) %>%
    summarise(total = sum(n)) %>%
    arrange(desc(total)) %>%
    slice_head(n = 20) %>%
    pull(surname_1)
  
  heatmap_data <- apellidos_por_periodo %>%
    filter(surname_1 %in% top_20_apellidos) %>%
    complete(periodo, surname_1, fill = list(n = 0, rank = 999))
  
  p1 <- ggplot(heatmap_data, aes(x = periodo, y = reorder(surname_1, -rank), fill = n)) +
    geom_tile(color = "white", linewidth = 0.5) +
    scale_fill_gradient(low = "white", high = "#0033A0", name = "N° cargos") +
    labs(
      title = "Cambio de Élites en Chile: Apellidos Dominantes por Período",
      subtitle = "Top 20 apellidos con más cargos políticos (Pre-1973 vs Post-1973)",
      x = "Período",
      y = "Apellido",
      caption = "Intensidad = número de cargos políticos"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      plot.subtitle = element_text(hjust = 0.5, size = 12),
      axis.text.y = element_text(size = 8),
      legend.position = "right"
    )
  
  ggsave("outputs/figures/cambio_elites_apellidos_heatmap.png", p1, 
         width = 14, height = 10, dpi = 300)
  cat("✅ Guardado: outputs/figures/cambio_elites_apellidos_heatmap.png\n")
}

# Gráfico 2: Evolución temporal de apellidos top
if (nrow(cargos_df) > 0) {
  # Agrupar por década
  cargos_df_decade <- cargos_df %>%
    filter(!is.na(year)) %>%
    mutate(decade = floor(year / 10) * 10) %>%
    filter(decade >= 1900 & decade <= 2020)
  
  if (nrow(cargos_df_decade) > 0) {
    top_10_apellidos <- cargos_df_decade %>%
      filter(!is.na(surname_1) & surname_1 != "") %>%
      count(surname_1, sort = TRUE) %>%
      slice_head(n = 10) %>%
      pull(surname_1)
    
    trend_data <- cargos_df_decade %>%
      filter(surname_1 %in% top_10_apellidos) %>%
      count(decade, surname_1) %>%
      complete(decade, surname_1, fill = list(n = 0))
    
    p2 <- ggplot(trend_data, aes(x = decade, y = n, color = surname_1)) +
      geom_line(linewidth = 1.2, alpha = 0.8) +
      geom_point(size = 2) +
      geom_vline(xintercept = 1973, linetype = "dashed", color = "red", linewidth = 1) +
      geom_vline(xintercept = 1990, linetype = "dashed", color = "blue", linewidth = 1) +
      annotate("text", x = 1973, y = max(trend_data$n, na.rm = TRUE) * 0.9, 
               label = "Golpe 1973", angle = 90, vjust = -0.5, color = "red", fontface = "bold") +
      annotate("text", x = 1990, y = max(trend_data$n, na.rm = TRUE) * 0.9, 
               label = "Retorno democracia", angle = 90, vjust = -0.5, color = "blue", fontface = "bold") +
      labs(
        title = "Evolución Temporal de Apellidos en Cargos Políticos",
        subtitle = "Top 10 apellidos con más cargos por década",
        x = "Década",
        y = "Número de cargos",
        color = "Apellido",
        caption = "Líneas verticales: Golpe 1973 (rojo) y Retorno democracia 1990 (azul)"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        legend.position = "right"
      )
    
    ggsave("outputs/figures/cambio_elites_evolucion_temporal.png", p2, 
           width = 16, height = 9, dpi = 300)
    cat("✅ Guardado: outputs/figures/cambio_elites_evolucion_temporal.png\n")
  }
}

# Gráfico 3: Comparación Pre vs Post 1973 (barras)
if (nrow(cargos_df) > 0 && nrow(apellidos_por_periodo) > 0) {
  # Top 15 apellidos en cada período
  top_pre <- apellidos_por_periodo %>%
    filter(periodo == "Pre-1973") %>%
    slice_head(n = 15)
  
  top_post <- apellidos_por_periodo %>%
    filter(periodo %in% c("Dictadura (1973-1990)", "Post-dictadura (1990+)")) %>%
    group_by(surname_1) %>%
    summarise(n = sum(n), .groups = "drop") %>%
    arrange(desc(n)) %>%
    slice_head(n = 15) %>%
    mutate(periodo = "Post-1973")
  
  # Obtener todos los apellidos únicos para categorización
  all_surnames_pre <- unique(top_pre$surname_1)
  all_surnames_post <- unique(top_post$surname_1)
  
  comparison_data <- bind_rows(
    top_pre %>% select(surname_1, n, periodo),
    top_post %>% select(surname_1, n, periodo)
  ) %>%
    mutate(
      aparece_pre = surname_1 %in% all_surnames_pre,
      aparece_post = surname_1 %in% all_surnames_post,
      categoria = case_when(
        aparece_pre & aparece_post ~ "Ambos períodos",
        aparece_pre ~ "Solo Pre-1973",
        aparece_post ~ "Solo Post-1973",
        TRUE ~ "Otro"
      )
    )
  
  p3 <- ggplot(comparison_data, aes(x = reorder(surname_1, n), y = n, fill = categoria)) +
    geom_bar(stat = "identity") +
    facet_wrap(~ periodo, scales = "free_y", ncol = 1) +
    coord_flip() +
    scale_fill_manual(
      values = c("Ambos períodos" = "#0033A0", "Solo Pre-1973" = "#8B0000", "Solo Post-1973" = "#006400"),
      name = "Categoría"
    ) +
    labs(
      title = "Comparación de Apellidos Dominantes: Pre vs Post 1973",
      subtitle = "Top 15 apellidos por período",
      x = "Apellido",
      y = "Número de cargos políticos",
      caption = "Rojo = desaparece post-1973 | Verde = nuevo post-1973 | Azul = presente en ambos"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      plot.subtitle = element_text(hjust = 0.5, size = 12),
      strip.text = element_text(face = "bold", size = 12)
    )
  
  ggsave("outputs/figures/cambio_elites_comparacion_pre_post.png", p3, 
         width = 14, height = 12, dpi = 300)
  cat("✅ Guardado: outputs/figures/cambio_elites_comparacion_pre_post.png\n")
}

# Guardar tablas
if (nrow(cargos_df) > 0) {
  if (nrow(apellidos_por_periodo) > 0) {
    write_csv(apellidos_por_periodo, "outputs/tables/apellidos_por_periodo.csv")
  }
  if (nrow(apellidos_unicos) > 0) {
    write_csv(apellidos_unicos, "outputs/tables/apellidos_unicos_por_periodo.csv")
  }
  if (nrow(apellidos_nuevos) > 0) {
    write_csv(apellidos_nuevos, "outputs/tables/apellidos_nuevos_post_1973.csv")
  }
  if (nrow(apellidos_desaparecidos) > 0) {
    write_csv(apellidos_desaparecidos, "outputs/tables/apellidos_desaparecidos_post_1973.csv")
  }
  if (exists("tipos_por_periodo") && nrow(tipos_por_periodo) > 0) {
    write_csv(tipos_por_periodo, "outputs/tables/tipos_cargos_por_periodo.csv")
  }
  if (nrow(diversidad) > 0) {
    write_csv(diversidad, "outputs/tables/diversidad_apellidos_por_periodo.csv")
  }
  
  cat("\n✅ Tablas guardadas en outputs/tables/\n")
}

cat("\n", strrep("=", 80), "\n")
cat("📊 RESUMEN FINAL\n")
cat(strrep("=", 80), "\n")
if (nrow(cargos_df) > 0) {
  cat("   Total de registros de cargos:", nrow(cargos_df), "\n")
  cat("   Apellidos únicos:", n_distinct(cargos_df$surname_1, na.rm = TRUE), "\n")
  if (exists("apellidos_nuevos")) {
    cat("   Apellidos NUEVOS post-1973:", nrow(apellidos_nuevos), "\n")
  }
  if (exists("apellidos_desaparecidos")) {
    cat("   Apellidos que DESAPARECEN post-1973:", nrow(apellidos_desaparecidos), "\n")
  }
}
cat(strrep("=", 80), "\n")

