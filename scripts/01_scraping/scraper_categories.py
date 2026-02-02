"""
Script para scrapear familias desde categorías de Wikipedia Chile

Este script:
1. Toma una URL de categoría de familia (ej: Categoría:Familia_Alessandri)
2. Extrae todos los miembros de esa familia listados en la categoría
3. Para cada miembro, extrae toda la información del infobox estructurada
4. Guarda los datos en CSV con información completa

Uso:
    python scraper_categories.py --category "Familia_Alessandri"
    python scraper_categories.py --url "https://es.wikipedia.org/wiki/Categoría:Familia_Alessandri"
"""

import requests
from bs4 import BeautifulSoup
import pandas as pd
import time
import argparse
import json
import os
from urllib.parse import urljoin, unquote, quote
from datetime import datetime
from pathlib import Path

BASE_URL = "https://es.wikipedia.org"
API_URL = "https://es.wikipedia.org/w/api.php"
DEFAULT_USER_AGENT = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36'


def get_headers():
    """Arma headers para requests normales (sin token)."""
    user_agent = os.environ.get('WIKI_USER_AGENT', DEFAULT_USER_AGENT)
    headers = {'User-Agent': user_agent}
    return headers


def get_api_headers():
    """Arma headers con token para llamadas a la API."""
    headers = get_headers()
    access_token = os.environ.get('WIKI_ACCESS_TOKEN')
    if access_token:
        headers['Authorization'] = f"Bearer {access_token}"
    return headers
DELAY_SECONDS = 10  # Aumentado para evitar bloqueos 403


def get_project_root():
    """Obtiene la ruta raíz del proyecto."""
    return Path(__file__).resolve().parents[2]


def get_output_path(output_name, country="chile"):
    """Construye la ruta de salida para una familia."""
    safe_name = output_name.replace(' ', '_').replace(':', '').lower()
    country_safe = country.lower()
    output_dir = get_project_root() / "data" / "raw" / country_safe / "familias"
    output_dir.mkdir(parents=True, exist_ok=True)
    return output_dir / f"{safe_name}_completo.csv"


def load_existing_data(output_path):
    """Carga datos previos si existe un CSV (para reanudar)."""
    if not output_path.exists():
        return pd.DataFrame()
    try:
        return pd.read_csv(output_path, sep=';', encoding='utf-8')
    except Exception as e:
        print(f"⚠️  No se pudo leer archivo existente {output_path}: {e}")
        return pd.DataFrame()

def get_soup(url, retries=3, base_delay=2, rate_limit_delay=60):
    """Obtiene BeautifulSoup de una URL con reintentos y backoff."""
    for i in range(retries):
        try:
            resp = requests.get(url, headers=get_headers(), timeout=15)
            if resp.status_code in (403, 429):
                retry_after = resp.headers.get('Retry-After')
                wait = (
                    int(retry_after)
                    if retry_after and retry_after.isdigit()
                    else rate_limit_delay * (i + 1)
                )
                print(f"⚠️  Rate limit en {url}. Esperando {wait}s (intento {i+1}/{retries})")
                time.sleep(wait)
                continue
            resp.raise_for_status()
            return BeautifulSoup(resp.text, 'html.parser')
        except Exception as e:
            print(f"❌ Error cargando {url}: {e}. Reintento {i+1}/{retries}")
            time.sleep(base_delay * (i + 1))
    return None


def get_category_title_from_url(category_url):
    """Convierte URL de categoría a título para la API."""
    if '/wiki/' in category_url:
        title = category_url.split('/wiki/')[-1]
    else:
        title = category_url
    title = unquote(title).replace('_', ' ')
    if not title.startswith('Categoría:'):
        title = f"Categoría:{title}"
    return title


def extract_category_members_api(category_title):
    """Extrae miembros de una categoría usando la API de Wikipedia."""
    members = []
    params = {
        'action': 'query',
        'list': 'categorymembers',
        'cmtitle': category_title,
        'cmtype': 'page',
        'cmlimit': 'max',
        'format': 'json'
    }

    while True:
        resp = requests.get(API_URL, headers=get_api_headers(), params=params, timeout=15)
        if resp.status_code in (403, 429):
            retry_after = resp.headers.get('Retry-After')
            wait = int(retry_after) if retry_after and retry_after.isdigit() else 60
            print(f"⚠️  Rate limit en API. Esperando {wait}s")
            time.sleep(wait)
            continue
        resp.raise_for_status()
        data = resp.json()
        for item in data.get('query', {}).get('categorymembers', []):
            title = item.get('title', '').strip()
            if not title:
                continue
            url = f"{BASE_URL}/wiki/{quote(title.replace(' ', '_'))}"
            members.append({'nombre': title, 'url': url})
            print(f"  ✓ Encontrado: {title}")

        if 'continue' in data:
            params.update(data['continue'])
        else:
            break

    return members


def extract_category_members(category_url):
    """
    Extrae todos los miembros (páginas) de una categoría de Wikipedia
    
    Args:
        category_url: URL de la categoría (ej: https://es.wikipedia.org/wiki/Categoría:Familia_Alessandri)
        
    Returns:
        list: Lista de diccionarios con {nombre, url}
    """
    category_title = get_category_title_from_url(category_url)
    return extract_category_members_api(category_title)


def extract_infobox_data(soup, url):
    """
    Extrae TODA la información del infobox de forma estructurada
    
    Args:
        soup: BeautifulSoup object
        url: URL de la página
        
    Returns:
        dict: Diccionario con toda la información extraída
    """
    # Buscar el infobox (tabla con clase que contiene 'infobox')
    infobox = soup.find('table', class_=lambda x: x and 'infobox' in x.lower())
    
    if not infobox:
        print(f"  ⚠️  No se encontró infobox en {url}")
        return None
    
    # Extraer nombre del título (h1)
    nombre = ""
    h1 = soup.find('h1', id='firstHeading')
    if h1:
        nombre = h1.get_text(strip=True)

    # Extraer biografía inicial (primer párrafo válido)
    biografia_inicial = ""
    content = soup.find("div", class_="mw-parser-output")
    if content:
        for p in content.find_all("p", recursive=False):
            text = p.get_text(" ", strip=True)
            if text:
                biografia_inicial = text
                break
    
    # Inicializar datos base
    data = {
        'nombre': nombre,
        'url': url,
        'biografia_inicial': biografia_inicial,
        'biografia': '',
        'fecha_nacimiento': '',
        'lugar_nacimiento': '',
        'fecha_fallecimiento': '',
        'lugar_fallecimiento': '',
        'residencia': '',
        'nacionalidad': '',
        'religion': '',
        'ocupacion': '',
        'partido_politico': '',
        'educacion': '',
        'alma_mater': '',
        'padres': '',
        'conyuge': '',
        'pareja': '',
        'hijos': '',
        'hermanos': '',
        'familia': '',
        'cargos_politicos': '',
        'periodo': '',
        'predecesor': '',
        'sucesor': '',
        'distinciones': '',
        'premios': '',
        'sitio_web': '',
        'infobox_completa': '',
        'infobox_json': '',
        'perfiles_relacionados': '',
        'perfiles_relacionados_padres': '',
        'perfiles_relacionados_conyuge': '',
        'perfiles_relacionados_pareja': '',
        'perfiles_relacionados_hijos': '',
        'perfiles_relacionados_hermanos': '',
        'perfiles_relacionados_familia': '',
        'profundidad_scraping': 0,
        'timestamp': datetime.now().isoformat()
    }

    # Extraer biografía completa (párrafos iniciales antes del primer H2)
    if content:
        bio_paragraphs = []
        for child in content.find_all(['p', 'h2'], recursive=False):
            if child.name == 'h2':
                break
            if child.name == 'p':
                text = child.get_text(" ", strip=True)
                if text:
                    bio_paragraphs.append(text)
        if bio_paragraphs:
            data['biografia'] = "\n\n".join(bio_paragraphs)
    
    # Procesar todas las filas del infobox
    rows = infobox.find_all('tr')
    infobox_entries = []
    infobox_struct = []
    
    for row in rows:
        th = row.find('th')
        td = row.find('td')
        
        if not th and not td:
            continue
        
        # Obtener el nombre del campo (header)
        header = th.get_text(strip=True) if th else ""
        header_lower = header.lower()
        
        # Obtener el valor (con y sin enlaces)
        value_text = td.get_text(' ', strip=True) if td else ""
        value_with_links = extract_value_with_links(td) if td else ""

        # Guardar todas las filas en formato libre
        if header or value_text:
            entry_label = header if header else "detalle"
            entry_value = value_with_links if value_with_links else value_text
            infobox_entries.append(f"{entry_label}: {entry_value}".strip())
            infobox_struct.append({
                "label": entry_label,
                "value_text": value_text,
                "value_with_links": value_with_links
            })
        
        # Mapear campos del infobox a nuestras columnas
        if 'nacimiento' in header_lower:
            data['fecha_nacimiento'] = value_text
            # Intentar extraer lugar de nacimiento si viene en la misma línea
            if '\n' in value_text:
                parts = value_text.split('\n')
                data['fecha_nacimiento'] = parts[0].strip()
                if len(parts) > 1:
                    data['lugar_nacimiento'] = parts[1].strip()
        
        elif 'fallecimiento' in header_lower or 'muerte' in header_lower:
            data['fecha_fallecimiento'] = value_text
            if '\n' in value_text:
                parts = value_text.split('\n')
                data['fecha_fallecimiento'] = parts[0].strip()
                if len(parts) > 1:
                    data['lugar_fallecimiento'] = parts[1].strip()
        
        elif 'residencia' in header_lower:
            data['residencia'] = value_text
        
        elif 'nacionalidad' in header_lower:
            data['nacionalidad'] = value_text
        
        elif 'religión' in header_lower or 'religion' in header_lower:
            data['religion'] = value_text
        
        elif 'ocupación' in header_lower or 'ocupacion' in header_lower:
            data['ocupacion'] = value_text
        
        elif 'partido' in header_lower and 'político' in header_lower:
            data['partido_politico'] = value_text
        
        elif 'educación' in header_lower or 'educado' in header_lower or 'educada' in header_lower:
            data['educacion'] = value_text
        
        elif 'alma' in header_lower and 'mater' in header_lower:
            data['alma_mater'] = value_text
        
        elif 'padres' in header_lower or 'padre' in header_lower or 'madre' in header_lower:
            data['padres'] = value_with_links
        
        elif 'cónyuge' in header_lower or 'conyuge' in header_lower or 'esposa' in header_lower or 'esposo' in header_lower:
            data['conyuge'] = value_with_links
        
        elif 'pareja' in header_lower or 'conviviente' in header_lower:
            data['pareja'] = value_with_links
        
        elif 'hijos' in header_lower or 'hijo' in header_lower or 'hija' in header_lower:
            data['hijos'] = value_with_links
        
        elif 'hermanos' in header_lower or 'hermano' in header_lower or 'hermana' in header_lower:
            data['hermanos'] = value_with_links
        
        elif 'familia' in header_lower:
            data['familia'] = value_with_links
        
        elif 'cargo' in header_lower or 'cargos' in header_lower:
            data['cargos_politicos'] = value_text
        
        elif 'período' in header_lower or 'periodo' in header_lower:
            data['periodo'] = value_text
        
        elif 'predecesor' in header_lower:
            data['predecesor'] = value_text
        
        elif 'sucesor' in header_lower:
            data['sucesor'] = value_text
        
        elif 'distinción' in header_lower or 'distinciones' in header_lower or 'premio' in header_lower or 'premios' in header_lower:
            if data['distinciones']:
                data['distinciones'] += '; ' + value_text
            else:
                data['distinciones'] = value_text
        
        elif 'sitio' in header_lower and 'web' in header_lower:
            data['sitio_web'] = value_text

    if infobox_entries:
        data['infobox_completa'] = " | ".join(infobox_entries)
        data['infobox_json'] = json.dumps(infobox_struct, ensure_ascii=False)
    
    return data


def extract_value_with_links(td):
    """
    Extrae el valor de una celda incluyendo los nombres de enlaces
    
    Args:
        td: BeautifulSoup tag de la celda
        
    Returns:
        str: Texto con formato "Nombre1 (URL1); Nombre2 (URL2); ..."
    """
    result = []
    
    # Buscar todos los enlaces dentro de la celda
    links = td.find_all('a', href=True)
    
    for link in links:
        href = link.get('href', '')
        text = link.get_text(strip=True)
        
        # Solo incluir enlaces a artículos (no a archivos, categorías, etc.)
        if href.startswith('/wiki/') and text and not any(x in href for x in [':', 'Archivo:', 'Especial:', 'Categoría:']):
            full_url = BASE_URL + href
            result.append(f"{text} ({full_url})")
    
    # Si no hay enlaces, devolver el texto plano
    if not result:
        return td.get_text(' ', strip=True)
    
    return '; '.join(result)


def extract_family_urls_from_data(person_data):
    """
    Extrae todas las URLs de familiares desde los datos de una persona
    
    Args:
        person_data: Diccionario con datos de la persona
        
    Returns:
        list: Lista de URLs de familiares
    """
    import re
    urls = []
    
    # Campos que contienen información familiar con URLs
    family_fields = ['padres', 'conyuge', 'pareja', 'hijos', 'hermanos', 'familia']
    
    for field in family_fields:
        value = person_data.get(field, '')
        if value and 'wikipedia.org' in value:
            # Extraer URLs del formato "Nombre (URL); Nombre2 (URL2)"
            matches = re.findall(r'([^;]+?)\s*\((https://es\.wikipedia\.org/wiki/[^\)]+)\)', value)
            if matches:
                for name, url in matches:
                    urls.append({
                        'url': url,
                        'field': field,
                        'name': name.strip()
                    })
            else:
                found_urls = re.findall(r'https://es\.wikipedia\.org/wiki/[^\)]+', value)
                for url in found_urls:
                    urls.append({
                        'url': url,
                        'field': field,
                        'name': ''
                    })
    
    # Eliminar duplicados por URL + campo
    unique = {}
    for item in urls:
        key = (item['url'], item['field'])
        unique[key] = item
    return list(unique.values())


def scrape_family_from_category(category_url, output_name=None, scrape_relatives=True, max_depth=2, resume=False, country="chile"):
    """
    Scrapea toda una familia desde su categoría de Wikipedia
    CON scraping recursivo de familiares enlazados
    
    Args:
        category_url: URL de la categoría
        output_name: Nombre para los archivos de salida
        scrape_relatives: Si True, scrapea también familiares enlazados (recursivo)
        max_depth: Profundidad máxima de recursión (2 = persona + padres + abuelos)
        
    Returns:
        DataFrame con todos los datos
    """
    print(f"\n🚀 Iniciando scraping de categoría: {category_url}")
    print(f"⚙️  Configuración: Scraping recursivo={'SÍ' if scrape_relatives else 'NO'}, Profundidad máxima={max_depth}, Delay={DELAY_SECONDS}s")
    print("=" * 80)
    
    # Extraer nombre de la categoría si no se proporciona output_name
    if not output_name:
        # Extraer de la URL (ej: "Familia_Alessandri" de la URL)
        output_name = category_url.split(':')[-1].replace('_', ' ')

    output_path = get_output_path(output_name, country=country)
    
    print(f"\n📋 Extrayendo miembros de la categoría...")
    members = extract_category_members(category_url)
    
    if not members:
        print("❌ No se encontraron miembros en la categoría")
        return None, output_name
    
    print(f"\n✓ Encontrados {len(members)} miembros en la categoría")
    print("=" * 80)
    
    # Cargar datos existentes si se reanuda
    all_data = []
    visited_urls = set()
    discovered_by = {}
    if resume:
        existing_df = load_existing_data(output_path)
        if not existing_df.empty and 'url' in existing_df.columns:
            visited_urls = set(existing_df['url'].dropna().unique())
            all_data = existing_df.to_dict(orient='records')
            print(f"↩️  Reanudando: {len(visited_urls)} URLs ya scrapearon")

    # Scrapear cada miembro + sus familiares (recursivo)
    urls_to_scrape = [
        (m['url'], m['nombre'], 0) for m in members if m['url'] not in visited_urls
    ]  # (url, nombre, depth)
    
    contador = 0
    while urls_to_scrape:
        current_url, current_name, depth = urls_to_scrape.pop(0)
        
        # Saltar si ya fue visitada
        if current_url in visited_urls:
            continue
        
        # Saltar si excede profundidad
        if depth > max_depth:
            continue
        
        visited_urls.add(current_url)
        contador += 1
        
        # Determinar si es miembro original de la categoría o familiar descubierto
        source = "📁 Categoría" if depth == 0 else f"🔗 Familiar nivel {depth}"
        
        print(f"\n[{contador}] {source}: {current_name}")
        print(f"  URL: {current_url}")
        print(f"  Pendientes: {len(urls_to_scrape)}")
        
        soup = get_soup(current_url)
        if not soup:
            print(f"  ❌ No se pudo cargar la página")
            continue
        
        person_data = extract_infobox_data(soup, current_url)
        
        if person_data:
            # Agregar metadata
            person_data['categoria_origen'] = category_url
            person_data['familia'] = output_name
            person_data['pais_origen'] = country
            person_data['profundidad_scraping'] = depth
            if current_url in discovered_by:
                flat_sources = []
                for field, items in discovered_by[current_url].items():
                    unique_items = sorted(set(items))
                    flat_sources.extend(unique_items)
                    person_data[f"perfiles_relacionados_{field}"] = "; ".join(unique_items)
                person_data['perfiles_relacionados'] = "; ".join(sorted(set(flat_sources)))
            all_data.append(person_data)
            print(f"  ✓ Datos extraídos exitosamente")
            
            # Si scraping recursivo está activado, extraer URLs de familiares
            if scrape_relatives and depth < max_depth:
                family_urls = extract_family_urls_from_data(person_data)
                
                if family_urls:
                    nuevos = [item for item in family_urls if item['url'] not in visited_urls]
                    print(f"  🔍 Encontrados {len(family_urls)} familiares enlazados ({len(nuevos)} nuevos)")
                    
                    # Agregar a la cola (si no fueron visitados)
                    for fam in nuevos:
                        # Extraer nombre del URL
                        fam_url = fam['url']
                        fam_name = fam_url.split('/wiki/')[-1].replace('_', ' ').replace('%C3%A9', 'é').replace('%C3%A1', 'á').replace('%C3%B3', 'ó')
                        source_field = fam.get('field', 'relacion')
                        source_label = f"{current_name} ({current_url})"
                        discovered_by.setdefault(fam_url, {}).setdefault(source_field, []).append(source_label)
                        urls_to_scrape.append((fam_url, fam_name, depth + 1))
        else:
            print(f"  ⚠️  No se pudo extraer infobox")
        
        # Delay AUMENTADO para evitar bloqueos de Wikipedia
        time.sleep(DELAY_SECONDS)
    
    # Crear DataFrame
    df = pd.DataFrame(all_data)
    if not df.empty and 'url' in df.columns:
        df.drop_duplicates(subset=['url'], keep='first', inplace=True)
    
    print(f"\n" + "=" * 80)
    print(f"📊 Resumen del scraping:")
    print(f"   Total de personas: {len(df)}")
    if scrape_relatives:
        print(f"   Miembros originales: {len([d for d in all_data if d.get('profundidad_scraping') == 0])}")
        print(f"   Familiares descubiertos: {len([d for d in all_data if d.get('profundidad_scraping', 0) > 0])}")
    print("=" * 80)
    
    return df, output_name


def save_data(df, output_name, country="chile"):
    """Guarda los datos en CSV"""
    if df is None or df.empty:
        print("\n❌ No hay datos para guardar")
        return
    
    # Guardar
    filename = get_output_path(output_name, country=country)
    df.to_csv(str(filename), index=False, sep=';', encoding='utf-8')
    
    print(f"\n" + "=" * 80)
    print(f"✅ SCRAPING COMPLETADO EXITOSAMENTE!")
    print(f"=" * 80)
    print(f"   Total de personas: {len(df)}")
    
    # Mostrar distribución por profundidad si existe esa columna
    if 'profundidad_scraping' in df.columns:
        print(f"\n   Distribución por profundidad:")
        depth_counts = df['profundidad_scraping'].value_counts().sort_index()
        for depth, count in depth_counts.items():
            label = "Categoría" if depth == 0 else f"Familiares nivel {depth}"
            print(f"     - {label}: {count} personas")
    
    print(f"\n   📁 Archivo guardado en:")
    print(f"      {filename}")
    print(f"=" * 80)


def main():
    parser = argparse.ArgumentParser(
        description='Scrapear familia completa desde categoría de Wikipedia con scraping recursivo'
    )
    parser.add_argument(
        '--category',
        type=str,
        help='Nombre de la categoría (ej: "Familia_Alessandri")'
    )
    parser.add_argument(
        '--url',
        type=str,
        help='URL completa de la categoría'
    )
    parser.add_argument(
        '--output',
        type=str,
        help='Nombre para los archivos de salida (opcional)'
    )
    parser.add_argument(
        '--no-relatives',
        action='store_true',
        help='Desactivar scraping recursivo de familiares (más rápido pero menos completo)'
    )
    parser.add_argument(
        '--depth',
        type=int,
        default=2,
        help='Profundidad máxima de scraping recursivo (default: 2)'
    )
    parser.add_argument(
        '--resume',
        action='store_true',
        help='Reanudar desde archivo existente si está disponible'
    )
    parser.add_argument(
        '--country',
        type=str,
        default='chile',
        help='País para la ruta de salida (default: chile)'
    )
    
    args = parser.parse_args()
    
    # Construir URL
    if args.url:
        category_url = args.url
    elif args.category:
        category_url = f"{BASE_URL}/wiki/Categoría:{args.category}"
    else:
        print("❌ Error: Debes especificar --category o --url")
        parser.print_help()
        return
    
    # Scrapear
    scrape_relatives = not args.no_relatives
    df, family_name = scrape_family_from_category(
        category_url,
        args.output,
        scrape_relatives=scrape_relatives,
        max_depth=args.depth,
        resume=args.resume,
        country=args.country
    )
    
    # Guardar
    save_data(df, family_name, country=args.country)


if __name__ == "__main__":
    main()
