# wikipedia_scraper_ext.py
import requests
from bs4 import BeautifulSoup
import csv
import time
import pandas as pd

BASE_URL = "https://es.wikipedia.org"
CSV_FILENAME = "datos_wikipedia.csv"

initial_urls = [
    "https://es.wikipedia.org/wiki/Michelle_Bachelet",
    "https://es.wikipedia.org/wiki/Axel_Kaiser",
    "https://es.wikipedia.org/wiki/Sebasti%C3%A1n_Pi%C3%B1era",
    "https://es.wikipedia.org/wiki/Fernando_Castillo_Velasco",
    "https://es.wikipedia.org/wiki/Fidelma_Allende",
    "https://es.wikipedia.org/wiki/Pedro_Humberto_Allende",
    "https://es.wikipedia.org/wiki/Beatriz_Allende",
    "https://es.wikipedia.org/wiki/Isabel_Allende",
    "https://es.wikipedia.org/wiki/Maya_Fernández",
    "https://es.wikipedia.org/wiki/Paula_Frías_Allende",
    "https://es.wikipedia.org/wiki/Isabel_Allende_Bussi",
    "https://es.wikipedia.org/wiki/Laura_Allende_Gossens",
    "https://es.wikipedia.org/wiki/Andrés_Pascal_Allende",
    "https://es.wikipedia.org/wiki/Denise_Pascal",
    "https://es.wikipedia.org/wiki/Ramón_Allende",
    "https://es.wikipedia.org/wiki/Salvador_Allende_Castro",
    "https://es.wikipedia.org/wiki/Marcia_Tambutti",
    "https://es.wikipedia.org/wiki/José_Manuel_Balmaceda",
    "https://es.wikipedia.org/wiki/Manuel_de_Balmaceda_Ballesteros",
    "https://es.wikipedia.org/wiki/Aníbal_Pinto",
    "https://es.wikipedia.org/wiki/Luisa_Garmendia",
    "https://es.wikipedia.org/wiki/Francisco_Antonio_Pinto",
    "https://es.wikipedia.org/wiki/Delfina_de_la_Cruz",
    "https://es.wikipedia.org/wiki/Francisco_Rozas_Mendiburú",
    "https://es.wikipedia.org/wiki/Ricardo_Ariztía_Urmeneta",
    "https://es.wikipedia.org/wiki/Francisco_Antonio_Pinto_Cruz",
    "https://es.wikipedia.org/wiki/Ignacio_Carrera_Pinto",
    "https://es.wikipedia.org/wiki/Fidelma_Allende",
    "https://es.wikipedia.org/wiki/Ricardo_Ariztía_Urmeneta",
    "https://es.wikipedia.org/wiki/Manuel_Montt_Balmaceda",
    "https://es.wikipedia.org/wiki/Marta_Montt_Balmaceda",
    "https://es.wikipedia.org/wiki/Lorenzo_Montt",
    "https://es.wikipedia.org/wiki/Arturo_Matte_Alessandri",
    "https://es.wikipedia.org/wiki/José_Esteban_de_Montt_Cabrera",
    "https://es.wikipedia.org/wiki/José_Santiago_Montt_Irarrázaval",
    "https://es.wikipedia.org/wiki/José_Anacleto_Montt_Goyenechea",
    "https://es.wikipedia.org/wiki/Rosario_Montt",
    "https://es.wikipedia.org/wiki/Enrique_Montt_Montt",
    "https://es.wikipedia.org/wiki/Ambrosio_Montt_Luco",
    "https://es.wikipedia.org/wiki/Ismael_Pérez_Montt"
    "https://es.wikipedia.org/wiki/José_Esteban_de_Montt_Cabrera",
    "https://es.wikipedia.org/wiki/José_Santiago_Montt_Irarrázaval",
    "https://es.wikipedia.org/wiki/José_Anacleto_Montt_Goyenechea",
    "https://es.wikipedia.org/wiki/Rosario_Montt",
    "https://es.wikipedia.org/wiki/Enrique_Montt_Montt",
    "https://es.wikipedia.org/wiki/Ambrosio_Montt_Luco",
    "https://es.wikipedia.org/wiki/Lorenzo_Montt_y_Pérez_de_Valenzuela",
    "https://es.wikipedia.org/wiki/Ismael_Pérez_Montt",
    "https://es.wikipedia.org/wiki/Juan_José_Pérez_Vergara",
    "https://es.wikipedia.org/wiki/Luis_Montt_Montt",
    "https://es.wikipedia.org/wiki/Julio_Montt_Salamanca",
    "https://es.wikipedia.org/wiki/Lorenzo_Montt",
    "https://es.wikipedia.org/wiki/Eugenio_Guzmán_Montt",
    "https://es.wikipedia.org/wiki/José_Eugenio_Guzmán_Irarrázaval",
    "https://es.wikipedia.org/wiki/José_Manuel_Guzmán_Echeverría",
    "https://es.wikipedia.org/wiki/Roberto_Guzmán_Montt",
    "https://es.wikipedia.org/wiki/Jorge_Guzmán_Montt",
    "https://es.wikipedia.org/wiki/Alberto_Cruz_Montt",
    "https://es.wikipedia.org/wiki/Cristina_Montt",
    "https://es.wikipedia.org/wiki/Teresa_Wilms_Montt",
    "https://es.wikipedia.org/wiki/Manuel_Montt_Lehuedé",
    "https://es.wikipedia.org/wiki/Julio_Montt",
    "https://es.wikipedia.org/wiki/Francisco_Vidal_Salinas",
    "https://es.wikipedia.org/wiki/Julio_Montt_Vidal",
    "https://es.wikipedia.org/wiki/Manuel_Montt_Balmaceda",
    "https://es.wikipedia.org/wiki/Mario_Garrido_Montt",
    "https://es.wikipedia.org/wiki/Marta_Montt_Balmaceda",
    "https://es.wikipedia.org/wiki/Luis_Montt_Dubournais",
    "https://es.wikipedia.org/wiki/Pedro_Montt_Leiva",
    "https://es.wikipedia.org/wiki/Raúl_Celis_Montt",
    "https://es.wikipedia.org/wiki/Víctor_R._Celis_Maturana",
    "https://es.wikipedia.org/wiki/Andrés_Wood",
    "https://es.wikipedia.org/wiki/Andrés_Celis",
    "https://es.wikipedia.org/wiki/Cristo_Montt",
    "https://es.wikipedia.org/wiki/Pablo_Larraín",
    "https://es.wikipedia.org/wiki/Hernán_Larraín_Fernández",
    "https://es.wikipedia.org/wiki/Magdalena_Matte",
    "https://es.wikipedia.org/wiki/Hernán_Larraín_Matte",
    "https://es.wikipedia.org/wiki/María_Eugenia_Larraín",
    "https://es.wikipedia.org/wiki/Marcelo_Ríos",
    "https://es.wikipedia.org/wiki/Fernando_Larraín",
    "https://es.wikipedia.org/wiki/Nicolás_Larraín",
    "https://es.wikipedia.org/wiki/Juan_Cristóbal_Guarello",
    "https://es.wikipedia.org/wiki/Ángel_Guarello",
    "https://es.wikipedia.org/wiki/Jorge_Guarello_Fitz-Henry",
    "https://es.wikipedia.org/wiki/Fernando_Guarello_Fitz-Henry",
    "https://es.wikipedia.org/wiki/Ángel_Guarello",
    "https://es.wikipedia.org/wiki/Alejandro_Guarello",
    "https://es.wikipedia.org/wiki/Nicolás_Larraín",
    "https://es.wikipedia.org/wiki/Mago_Larraín",
    "https://es.wikipedia.org/wiki/Antonia_Orellana"

]

visited_urls = set()

# Función para extraer información de una persona o entidad
def extract_info(url, depth=0, max_depth=2):
    if url in visited_urls or depth > max_depth:
        return None
    visited_urls.add(url)

    response = requests.get(url)
    if response.status_code != 200:
        return None

    soup = BeautifulSoup(response.text, 'html.parser')
    infobox = soup.find("table", class_="infobox biography vcard")
    if not infobox:
        return None

    info = {
        "Nombre": "",
        "Fecha de nacimiento": "",
        "Residencia": "",
        "Nacionalidad": "",
        "Cargos": "",
        "Educación": "",
        "Trabajos previos": "",
        "Área": "",
        "Partido político": "",
        "Familia": "",
        "URL": url
    }

    title = soup.find("h1", class_="firstHeading")
    if title:
        info["Nombre"] = title.text.strip()

    for row in infobox.find_all("tr"):
        header = row.find("th")
        value = row.find("td")
        if header and value:
            header_text = header.text.strip()
            value_text = value.text.strip()

            if "Nacimiento" in header_text:
                info["Fecha de nacimiento"] = value_text
            elif "Residencia" in header_text:
                info["Residencia"] = value_text
            elif "Nacionalidad" in header_text:
                info["Nacionalidad"] = value_text
            elif any(k in header_text for k in ["Ocupación", "Cargos", "Cargo"]):
                for item in value.stripped_strings:
                    info["Cargos"] += item + "; "
            elif any(k in header_text for k in ["Educación", "Educado en", "Educada en"]):
                education_links = [link.text for link in value.find_all("a") if link.get("href", "").startswith("/wiki/")]
                info["Educación"] = "; ".join(education_links)
            elif "Área" in header_text:
                info["Área"] = value_text
            elif "Partido político" in header_text:
                parties = [link.text for link in value.find_all("a")]
                info["Partido político"] = "; ".join(parties)
            elif any(k in header_text for k in ["Padres", "Cónyuge", "Hijos", "Familia"]):
                family_links = []
                for link in value.find_all("a"):
                    href = link.get("href", "")
                    if (href.startswith("/wiki/") and
                        not any(href.startswith(excl) for excl in [
                            "/wiki/Ayuda:", "/wiki/Archivo:", "/wiki/Especial:", "/wiki/Plantilla:", "/wiki/Portal:", "/wiki/Categor%C3%ADa:", "/wiki/Familia_"])):
                        family_links.append(f"{link.text} ({BASE_URL}{href})")
                    else:
                        family_links.append(link.text)
                info["Familia"] += "; ".join(family_links) + "; "

    # Extraer trabajos previos desde bloques válidos únicamente
    rows = infobox.find_all("tr")
    for i in range(len(rows) - 1):
        th = rows[i].find("th")
        td = rows[i + 1].find("td")
        if th and td and th.get("colspan") == "3" and 'background-color:#E6E6FA' in th.get("style", ""):
            if th.find("a"):  # asegurarse que tiene enlace, para evitar otros bloques como altura, religión, etc.
                cargos = [a.text for a in th.find_all("a") if a.get("href", "").startswith("/wiki/")]
                fechas = td.get_text(strip=True)
                cargo_str = "; ".join(cargos)
                info["Trabajos previos"] += f"{cargo_str} – {fechas}; "

    save_to_csv(info)
    print(f"Guardado: {info['Nombre']}")

    for family_member in info["Familia"].split("; "):
        if "(" in family_member and ")" in family_member:
            family_url = family_member.split("(")[1].split(")")[0]
            if ("wikipedia" in family_url and not "Familia_" in family_url):
                extract_info(family_url, depth + 1, max_depth)
                time.sleep(2)

# Guardar datos en CSV
def save_to_csv(data):
    with open(CSV_FILENAME, "a", newline="", encoding="utf-8") as file:
        writer = csv.DictWriter(file, fieldnames=data.keys())
        if file.tell() == 0:
            writer.writeheader()
        writer.writerow(data)

# Limpiar archivo CSV al inicio
open(CSV_FILENAME, "w", encoding="utf-8").close()

# Iniciar scraping
for url in initial_urls:
    extract_info(url)
    time.sleep(2)

# Eliminar duplicados
df = pd.read_csv(CSV_FILENAME)
df.drop_duplicates(subset=["Nombre"], keep="first", inplace=True)
df.to_csv(CSV_FILENAME, index=False)
print("Proceso completado. Datos guardados en", CSV_FILENAME)
