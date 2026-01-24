"""
Configuración para el scraper de Wikipedia
"""

# URLs iniciales por país
INITIAL_URLS = {
    "chile": [
        "https://es.wikipedia.org/wiki/Salvador_Allende",
        "https://es.wikipedia.org/wiki/Michelle_Bachelet",
        "https://es.wikipedia.org/wiki/Sebastián_Piñera",
        "https://es.wikipedia.org/wiki/José_Miguel_Carrera",
    ],
    "argentina": [
        "https://es.wikipedia.org/wiki/Juan_Domingo_Perón",
        "https://es.wikipedia.org/wiki/Evita",
    ],
    "mexico": [
        "https://es.wikipedia.org/wiki/Benito_Juárez",
    ]
}

# Configuración de scraping
MAX_DEPTH = 1  # Profundidad máxima de búsqueda de familiares
DELAY_SECONDS = 1  # Delay entre requests para respetar rate limits

# Rutas de salida
OUTPUT_DIR = "data/raw"
MANUAL_INPUT_FILE = "data/manual/familia_link_manual2.xlsx"

# Headers para requests
HEADERS = {
    'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36'
}

# Palabras clave para identificar tipos de relación
RELATIONSHIP_KEYWORDS = {
    "padres": "padre/madre",
    "cónyuge": "cónyuge",
    "hijos": "hijo/a",
    "familia": "familia",
    "hermanos": "hermano/a"
}

# Prefijos de URLs de Wikipedia a excluir
EXCLUDED_URL_PREFIXES = [
    "/wiki/Ayuda:",
    "/wiki/Archivo:",
    "/wiki/Especial:",
    "/wiki/Plantilla:",
    "/wiki/Portal:",
    "/wiki/Categoría:",
    "/wiki/Familia_"  # Excluir páginas genéricas de familias
]
