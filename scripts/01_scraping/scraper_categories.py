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
import os
from urllib.parse import urljoin, unquote
from datetime import datetime
from pathlib import Path

BASE_URL = "https://es.wikipedia.org"
HEADERS = {
    'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36'
}
DELAY_SECONDS = 10  # Aumentado para evitar bloqueos 403


def get_project_root():
    """Obtiene la ruta raíz del proyecto."""
    return Path(__file__).resolve().parents[2]

def get_soup(url, retries=3):
    """Obtiene BeautifulSoup de una URL con reintentos"""
    for i in range(retries):
        try:
            resp = requests.get(url, headers=HEADERS, timeout=15)
            resp.raise_for_status()
            return BeautifulSoup(resp.text, 'html.parser')
        except Exception as e:
            print(f"❌ Error cargando {url}: {e}. Reintento {i+1}/{retries}")
            time.sleep(2)
    return None


def extract_category_members(category_url):
    """
    Extrae todos los miembros (páginas) de una categoría de Wikipedia
    
    Args:
        category_url: URL de la categoría (ej: https://es.wikipedia.org/wiki/Categoría:Familia_Alessandri)
        
    Returns:
        list: Lista de diccionarios con {nombre, url}
    """
    soup = get_soup(category_url)
    if not soup:
        return []
    
    members = []
    
    # Buscar la sección "Páginas en la categoría"
    pages_section = soup.find('div', {'id': 'mw-pages'})
    if not pages_section:
        print("⚠️  No se encontró la sección de páginas en la categoría")
        return []
    
    # Extraer todos los enlaces a páginas de personas
    links = pages_section.find_all('a')
    
    for link in links:
        href = link.get('href', '')
        text = link.get_text(strip=True)
        
        # Filtrar solo enlaces a artículos (que empiezan con /wiki/ y no son especiales)
        if href.startswith('/wiki/') and ':' not in href:
            full_url = BASE_URL + href
            members.append({
                'nombre': text,
                'url': full_url
            })
            print(f"  ✓ Encontrado: {text}")
    
    return members


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
        'profundidad_scraping': 0,
        'timestamp': datetime.now().isoformat()
    }
    
    # Procesar todas las filas del infobox
    rows = infobox.find_all('tr')
    
    for row in rows:
        th = row.find('th')
        td = row.find('td')
        
        if not th or not td:
            continue
        
        # Obtener el nombre del campo (header)
        header = th.get_text(strip=True).lower()
        
        # Obtener el valor (con y sin enlaces)
        value_text = td.get_text(' ', strip=True)
        value_with_links = extract_value_with_links(td)
        
        # Mapear campos del infobox a nuestras columnas
        if 'nacimiento' in header:
            data['fecha_nacimiento'] = value_text
            # Intentar extraer lugar de nacimiento si viene en la misma línea
            if '\n' in value_text:
                parts = value_text.split('\n')
                data['fecha_nacimiento'] = parts[0].strip()
                if len(parts) > 1:
                    data['lugar_nacimiento'] = parts[1].strip()
        
        elif 'fallecimiento' in header or 'muerte' in header:
            data['fecha_fallecimiento'] = value_text
            if '\n' in value_text:
                parts = value_text.split('\n')
                data['fecha_fallecimiento'] = parts[0].strip()
                if len(parts) > 1:
                    data['lugar_fallecimiento'] = parts[1].strip()
        
        elif 'residencia' in header:
            data['residencia'] = value_text
        
        elif 'nacionalidad' in header:
            data['nacionalidad'] = value_text
        
        elif 'religión' in header or 'religion' in header:
            data['religion'] = value_text
        
        elif 'ocupación' in header or 'ocupacion' in header:
            data['ocupacion'] = value_text
        
        elif 'partido' in header and 'político' in header:
            data['partido_politico'] = value_text
        
        elif 'educación' in header or 'educado' in header or 'educada' in header:
            data['educacion'] = value_text
        
        elif 'alma' in header and 'mater' in header:
            data['alma_mater'] = value_text
        
        elif 'padres' in header or 'padre' in header or 'madre' in header:
            data['padres'] = value_with_links
        
        elif 'cónyuge' in header or 'conyuge' in header or 'esposa' in header or 'esposo' in header:
            data['conyuge'] = value_with_links
        
        elif 'pareja' in header or 'conviviente' in header:
            data['pareja'] = value_with_links
        
        elif 'hijos' in header or 'hijo' in header or 'hija' in header:
            data['hijos'] = value_with_links
        
        elif 'hermanos' in header or 'hermano' in header or 'hermana' in header:
            data['hermanos'] = value_with_links
        
        elif 'familia' in header:
            data['familia'] = value_with_links
        
        elif 'cargo' in header or 'cargos' in header:
            data['cargos_politicos'] = value_text
        
        elif 'período' in header or 'periodo' in header:
            data['periodo'] = value_text
        
        elif 'predecesor' in header:
            data['predecesor'] = value_text
        
        elif 'sucesor' in header:
            data['sucesor'] = value_text
        
        elif 'distinción' in header or 'distinciones' in header or 'premio' in header or 'premios' in header:
            if data['distinciones']:
                data['distinciones'] += '; ' + value_text
            else:
                data['distinciones'] = value_text
        
        elif 'sitio' in header and 'web' in header:
            data['sitio_web'] = value_text
    
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
            found_urls = re.findall(r'https://es\.wikipedia\.org/wiki/[^\)]+', value)
            urls.extend(found_urls)
    
    return list(set(urls))  # Eliminar duplicados


def scrape_family_from_category(category_url, output_name=None, scrape_relatives=True, max_depth=2):
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
    
    print(f"\n📋 Extrayendo miembros de la categoría...")
    members = extract_category_members(category_url)
    
    if not members:
        print("❌ No se encontraron miembros en la categoría")
        return None, output_name
    
    print(f"\n✓ Encontrados {len(members)} miembros en la categoría")
    print("=" * 80)
    
    # Scrapear cada miembro + sus familiares (recursivo)
    all_data = []
    visited_urls = set()
    urls_to_scrape = [(m['url'], m['nombre'], 0) for m in members]  # (url, nombre, depth)
    
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
            person_data['profundidad_scraping'] = depth
            all_data.append(person_data)
            print(f"  ✓ Datos extraídos exitosamente")
            
            # Si scraping recursivo está activado, extraer URLs de familiares
            if scrape_relatives and depth < max_depth:
                family_urls = extract_family_urls_from_data(person_data)
                
                if family_urls:
                    nuevos = [url for url in family_urls if url not in visited_urls]
                    print(f"  🔍 Encontrados {len(family_urls)} familiares enlazados ({len(nuevos)} nuevos)")
                    
                    # Agregar a la cola (si no fueron visitados)
                    for fam_url in nuevos:
                        # Extraer nombre del URL
                        fam_name = fam_url.split('/wiki/')[-1].replace('_', ' ').replace('%C3%A9', 'é').replace('%C3%A1', 'á').replace('%C3%B3', 'ó')
                        urls_to_scrape.append((fam_url, fam_name, depth + 1))
        else:
            print(f"  ⚠️  No se pudo extraer infobox")
        
        # Delay AUMENTADO para evitar bloqueos de Wikipedia
        time.sleep(DELAY_SECONDS)
    
    # Crear DataFrame
    df = pd.DataFrame(all_data)
    
    print(f"\n" + "=" * 80)
    print(f"📊 Resumen del scraping:")
    print(f"   Total de personas: {len(df)}")
    if scrape_relatives:
        print(f"   Miembros originales: {len([d for d in all_data if d.get('profundidad_scraping') == 0])}")
        print(f"   Familiares descubiertos: {len([d for d in all_data if d.get('profundidad_scraping', 0) > 0])}")
    print("=" * 80)
    
    return df, output_name


def save_data(df, output_name):
    """Guarda los datos en CSV"""
    if df is None or df.empty:
        print("\n❌ No hay datos para guardar")
        return
    
    # Limpiar nombre para archivo
    safe_name = output_name.replace(' ', '_').replace(':', '').lower()
    
    # Crear directorio de salida
    output_dir = get_project_root() / "data" / "raw" / "chile" / "familias"
    output_dir.mkdir(parents=True, exist_ok=True)
    
    # Guardar
    filename = output_dir / f"{safe_name}_completo.csv"
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
        max_depth=args.depth
    )
    
    # Guardar
    save_data(df, family_name)


if __name__ == "__main__":
    main()
