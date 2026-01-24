"""
Funciones auxiliares para el scraper de Wikipedia
"""
import requests
from bs4 import BeautifulSoup
import time
from config import HEADERS, EXCLUDED_URL_PREFIXES, RELATIONSHIP_KEYWORDS

BASE_URL = "https://es.wikipedia.org"


def get_soup(url, retries=3):
    """
    Obtiene el objeto BeautifulSoup de una URL con reintentos
    
    Args:
        url: URL a scrapear
        retries: Número de reintentos en caso de fallo
        
    Returns:
        BeautifulSoup object o None si falla
    """
    for i in range(retries):
        try:
            resp = requests.get(url, headers=HEADERS, timeout=10)
            resp.raise_for_status()
            return BeautifulSoup(resp.text, 'html.parser')
        except Exception as e:
            print(f"Error cargando {url}: {e}. Reintento {i+1}/{retries}")
            time.sleep(2)
    return None


def extract_person_data(soup, url):
    """
    Extrae información de una persona desde su página de Wikipedia
    
    Args:
        soup: BeautifulSoup object de la página
        url: URL de la página
        
    Returns:
        tuple: (person_info dict, relationships list)
    """
    infobox = soup.find("table", class_=lambda x: x and 'infobox' in x)
    if not infobox:
        return None, []
    
    # Extraer nombre
    name = ""
    h1 = soup.find("h1", id="firstHeading")
    if h1:
        name = h1.text.strip()
    
    person_info = {
        "Nombre": name,
        "URL": url,
        "Fecha de nacimiento": "",
        "Lugar de nacimiento": "",
        "Fecha de fallecimiento": "",
        "Ocupación": "",
        "Partido político": ""
    }
    
    relationships = []
    
    # Procesar filas del infobox
    rows = infobox.find_all("tr")
    for row in rows:
        th = row.find("th")
        td = row.find("td")
        if not th or not td:
            continue
        
        header = th.text.strip()
        
        # Extraer información básica
        if "Nacimiento" in header:
            person_info["Fecha de nacimiento"] = td.get_text(" ", strip=True)
        elif "Fallecimiento" in header:
            person_info["Fecha de fallecimiento"] = td.get_text(" ", strip=True)
        elif "Ocupación" in header or "Cargo" in header:
            person_info["Ocupación"] = td.get_text("; ", strip=True)
        elif "Partido político" in header:
            person_info["Partido político"] = td.get_text("; ", strip=True)
            
        # Extraer relaciones familiares
        rel_type = None
        header_lower = header.lower()
        
        for keyword, rel_name in RELATIONSHIP_KEYWORDS.items():
            if keyword in header_lower:
                rel_type = rel_name
                break
                
        if rel_type:
            for a in td.find_all("a", href=True):
                href = a['href']
                # Filtrar links no deseados
                if href.startswith("/wiki/") and not any(href.startswith(excl) for excl in EXCLUDED_URL_PREFIXES):
                    target_url = BASE_URL + href
                    target_name = a.text.strip()
                    if target_name:
                        relationships.append({
                            "source_name": name,
                            "source_url": url,
                            "target_name": target_name,
                            "target_url": target_url,
                            "type": rel_type
                        })
    
    return person_info, relationships


def is_valid_person_url(url):
    """
    Verifica si una URL es válida para scrapear
    
    Args:
        url: URL a verificar
        
    Returns:
        bool: True si es válida
    """
    if not url.startswith("https://es.wikipedia.org/wiki/"):
        return False
    
    for prefix in EXCLUDED_URL_PREFIXES:
        if prefix in url:
            return False
    
    return True
