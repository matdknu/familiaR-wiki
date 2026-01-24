import requests
from bs4 import BeautifulSoup
import time
import csv

BASE_URL = "https://es.wikipedia.org"
processed_urls = set()


def obtener_sopa(url, intentos=3):
    """Realiza una solicitud HTTP y devuelve una BeautifulSoup, con reintentos."""
    for intento in range(intentos):
        try:
            response = requests.get(url, timeout=10)
            response.raise_for_status()
            return BeautifulSoup(response.text, 'html.parser')
        except requests.exceptions.Timeout:
            print(f"Tiempo de espera agotado para {url}. Reintentando ({intento + 1}/{intentos})...")
            time.sleep(5)
        except requests.RequestException as e:
            print(f"Error al acceder a {url}: {e}")
            return None
    return None


def extraer_familias(sopa):
    """Extrae las familias desde la página principal de categoría."""
    familias = []
    for div in sopa.find_all('div', class_='mw-category-group'):
        for li in div.find_all('li'):
            a_tag = li.find('a')
            if a_tag and 'Familia' in a_tag.text and 'Fundadoras' not in a_tag.text:
                familias.append((a_tag.text.strip(), BASE_URL + a_tag['href']))
    return familias


def extraer_miembros_familia(sopa):
    """
    Extrae recursivamente los miembros de una categoría:
      1) Recorre subcategorías y llama recursivo.
      2) Luego extrae las páginas de 'mw-pages'.
    """
    miembros = set()
    subcat = sopa.find('div', id='mw-subcategories')
    if subcat:
        for div in subcat.find_all('div', class_='mw-category-group'):
            for li in div.find_all('li'):
                a = li.find('a', href=True)
                if a and '/wiki/Categor%C3%ADa:' in a['href']:
                    sub_sopa = obtener_sopa(BASE_URL + a['href'])
                    if sub_sopa:
                        miembros |= extraer_miembros_familia(sub_sopa)
    pages = sopa.find('div', id='mw-pages')
    if pages:
        for div in pages.find_all('div', class_='mw-category-group'):
            for li in div.find_all('li'):
                a = li.find('a', href=True)
                if a:
                    miembros.add((a.text.strip(), BASE_URL + a['href']))
    return miembros


def save_to_csv(info, filename="familias_mexicanas.csv"):
    """Guarda un diccionario en el CSV si su URL no se ha procesado aún."""
    if info["URL"] in processed_urls:
        return
    processed_urls.add(info["URL"])

    with open(filename, mode='a', newline='', encoding='utf-8') as f:
        writer = csv.writer(f, delimiter=';')
        row = [
            info.get("Nombre", ""),
            info.get("Fecha de nacimiento", ""),
            info.get("Residencia", ""),
            info.get("Nacionalidad", ""),
            info.get("Cargos", ""),
            info.get("Educación", ""),
            info.get("Trabajos previos", ""),
            info.get("Área", ""),
            info.get("Partido político", ""),
            info.get("Familia", ""),
            info.get("Categorías", ""),
            info.get("URL", "")
        ]
        writer.writerow(row)


def dedup_csv(filename="familias_mexicanas.csv"):
    """Elimina filas duplicadas en el CSV, basándose en todas las columnas."""
    seen = set()
    unique = []
    with open(filename, newline='', encoding='utf-8') as f:
        reader = csv.reader(f, delimiter=';')
        header = next(reader)
        for row in reader:
            row_tup = tuple(row)
            if row_tup not in seen:
                seen.add(row_tup)
                unique.append(row)
    with open(filename, mode='w', newline='', encoding='utf-8') as f:
        writer = csv.writer(f, delimiter=';')
        writer.writerow(header)
        writer.writerows(unique)


def extract_info(url, depth=0, max_depth=1):
    sopa = obtener_sopa(url)
    if not sopa:
        return

    infobox = sopa.find("table", class_="infobox biography vcard")
    if not infobox:
        return

    info = {
        "Nombre": "", "Fecha de nacimiento": "", "Residencia": "",
        "Nacionalidad": "", "Cargos": "", "Educación": "",
        "Trabajos previos": "", "Área": "", "Partido político": "",
        "Familia": "", "Categorías": "", "URL": url
    }

    h1 = sopa.find("h1", class_="firstHeading")
    if h1:
        info["Nombre"] = h1.text.strip()

    cat_div = sopa.find("div", id="mw-normal-catlinks")
    if cat_div:
        info["Categorías"] = "; ".join(a.text for a in cat_div.find_all("a"))

    rows = infobox.find_all("tr")
    for row in rows:
        th = row.find("th")
        td = row.find("td")
        if not th or not td:
            continue
        h = th.text.strip()
        txt = td.get_text(" ", strip=True)

        if "Nacimiento" in h:
            info["Fecha de nacimiento"] = txt
        elif "Residencia" in h:
            info["Residencia"] = txt
        elif "Nacionalidad" in h:
            info["Nacionalidad"] = txt
        elif any(k in h for k in ["Ocupación", "Cargo", "Cargos"]):
            info["Cargos"] = "; ".join(td.stripped_strings)
        elif any(k in h for k in ["Educación", "Educado en", "Educada en"]):
            info["Educación"] = "; ".join(
                a.text for a in td.find_all("a", href=True) if a["href"].startswith("/wiki/")
            )
        elif "Área" in h:
            info["Área"] = txt
        elif "Partido político" in h:
            info["Partido político"] = "; ".join(a.text for a in td.find_all("a", href=True))
        elif any(k in h for k in ["Padres", "Cónyuge", "Hijos", "Familia"]):
            familia = []
            for a in td.find_all("a", href=True):
                href = a["href"]
                if href.startswith("/wiki/") and not any(href.startswith(excl) for excl in [
                        "/wiki/Ayuda:", "/wiki/Archivo:", "/wiki/Especial:",
                        "/wiki/Plantilla:", "/wiki/Portal:", "/wiki/Categor%C3%ADa:", "/wiki/Familia_"
                ]):
                    familia.append(f"{a.text} ({BASE_URL}{href})")
            info["Familia"] = "; ".join(familia)

    for i in range(len(rows)-1):
        th = rows[i].find("th")
        td = rows[i+1].find("td")
        if th and td and th.get("colspan") == "3" and "background-color:#E6E6FA" in th.get("style", ""):
            cargos = "; ".join(a.text for a in th.find_all("a", href=True))
            fechas = td.get_text(" ", strip=True)
            info["Trabajos previos"] += f"{cargos} – {fechas}; "

    save_to_csv(info)
    print(f"Guardado: {info['Nombre']}")

    if depth < max_depth and info["Familia"]:
        for miembro in info["Familia"].split("; "):
            if "(" in miembro and miembro.endswith(")"):
                member_url = miembro.split("(")[1][:-1]
                extract_info(member_url, depth+1, max_depth)
                time.sleep(2)


def main():
    url_categorias = f'{BASE_URL}/wiki/Categor%C3%ADa:Familias_de_M%C3%A9xico'

    # Crear CSV con encabezado
    header = [
        "Nombre", "Fecha de nacimiento", "Residencia", "Nacionalidad",
        "Cargos", "Educación", "Trabajos previos", "Área",
        "Partido político", "Familia", "Categorías", "URL"
    ]
    with open("familias_chilenas.csv", mode='w', newline='', encoding='utf-8') as f:
        writer = csv.writer(f, delimiter=';')
        writer.writerow(header)

    sopa_principal = obtener_sopa(url_categorias)
    if not sopa_principal:
        print("No se pudo cargar la categoría principal.")
        return

    familias = extraer_familias(sopa_principal)
    for fam_nombre, fam_url in familias:
        print(f"Procesando miembros de {fam_nombre}...")
        sopa_fam = obtener_sopa(fam_url)
        if not sopa_fam:
            continue
        miembros = extraer_miembros_familia(sopa_fam)
        for _, miembro_url in miembros:
            extract_info(miembro_url, depth=0, max_depth=1)
            time.sleep(1)

    # Eliminar duplicados finales
    dedup_csv("familias_mexicanas.csv")
    print("Extracción completa. CSV limpio y sin duplicados listo.")

if __name__ == '__main__':
    main()
