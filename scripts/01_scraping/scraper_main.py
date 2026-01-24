"""
Script principal para scrapear relaciones familiares desde Wikipedia

Uso:
    python scraper_main.py --country chile --depth 1
    python scraper_main.py --manual data/manual/familia_link_manual2.xlsx --output chile_completo
"""
import argparse
import os
import pandas as pd
import time
from scraper_utils import get_soup, extract_person_data, is_valid_person_url
from config import INITIAL_URLS, MAX_DEPTH, DELAY_SECONDS, OUTPUT_DIR, MANUAL_INPUT_FILE


def scrape_family_tree(start_urls, max_depth=1):
    """
    Realiza scraping recursivo de árbol familiar
    
    Args:
        start_urls: Lista de URLs iniciales
        max_depth: Profundidad máxima de búsqueda
        
    Returns:
        tuple: (personas, relaciones)
    """
    visited = set()
    queue = [(url, 0) for url in start_urls]
    
    all_persons = []
    all_relationships = []
    
    while queue:
        current_url, depth = queue.pop(0)
        
        if current_url in visited or depth > max_depth:
            continue
            
        print(f"[Profundidad {depth}] Scrapeando: {current_url}")
        
        soup = get_soup(current_url)
        if not soup:
            visited.add(current_url)
            continue
            
        person_data, relationships = extract_person_data(soup, current_url)
        visited.add(current_url)
        
        if person_data:
            all_persons.append(person_data)
            all_relationships.extend(relationships)
            
            # Agregar familiares a la cola si no hemos alcanzado max_depth
            if depth < max_depth:
                for rel in relationships:
                    target_url = rel["target_url"]
                    if target_url not in visited and is_valid_person_url(target_url):
                        queue.append((target_url, depth + 1))
        
        time.sleep(DELAY_SECONDS)
        
    return all_persons, all_relationships


def save_data(persons, relationships, output_name):
    """
    Guarda los datos en archivos CSV
    
    Args:
        persons: Lista de diccionarios con info de personas
        relationships: Lista de diccionarios con relaciones
        output_name: Nombre base para los archivos de salida
    """
    # Crear directorios si no existen
    personas_dir = os.path.join(OUTPUT_DIR, output_name.split('_')[0], "personas")
    relaciones_dir = os.path.join(OUTPUT_DIR, output_name.split('_')[0], "relaciones")
    
    os.makedirs(personas_dir, exist_ok=True)
    os.makedirs(relaciones_dir, exist_ok=True)
    
    # Guardar personas
    df_persons = pd.DataFrame(persons)
    if not df_persons.empty:
        df_persons.drop_duplicates(subset=["URL"], inplace=True)
        personas_file = os.path.join(personas_dir, f"{output_name}_personas.csv")
        df_persons.to_csv(personas_file, index=False, sep=";", encoding="utf-8")
        print(f"✓ Guardadas {len(df_persons)} personas en: {personas_file}")
    
    # Guardar relaciones
    df_rels = pd.DataFrame(relationships)
    if not df_rels.empty:
        df_rels.drop_duplicates(inplace=True)
        relaciones_file = os.path.join(relaciones_dir, f"{output_name}_relaciones.csv")
        df_rels.to_csv(relaciones_file, index=False, sep=";", encoding="utf-8")
        print(f"✓ Guardadas {len(df_rels)} relaciones en: {relaciones_file}")


def load_manual_urls(filepath):
    """
    Carga URLs desde archivo Excel manual
    
    Args:
        filepath: Ruta al archivo Excel
        
    Returns:
        list: Lista de URLs
    """
    if not os.path.exists(filepath):
        print(f"⚠ Archivo manual no encontrado: {filepath}")
        return []
    
    df = pd.read_excel(filepath)
    urls = df['URL'].dropna().str.strip().str.replace(r'\.\.\.$', '', regex=True).unique().tolist()
    print(f"✓ Cargadas {len(urls)} URLs desde archivo manual")
    return urls


def main():
    parser = argparse.ArgumentParser(description='Scrapear relaciones familiares de Wikipedia')
    parser.add_argument('--country', type=str, choices=['chile', 'argentina', 'mexico'], 
                        help='País a scrapear (usa URLs predefinidas)')
    parser.add_argument('--manual', type=str, 
                        help='Ruta a archivo Excel con URLs manuales')
    parser.add_argument('--depth', type=int, default=MAX_DEPTH, 
                        help=f'Profundidad máxima de búsqueda (default: {MAX_DEPTH})')
    parser.add_argument('--output', type=str, 
                        help='Nombre base para archivos de salida (default: nombre del país)')
    
    args = parser.parse_args()
    
    # Determinar URLs iniciales
    initial_urls = []
    output_name = args.output
    
    if args.manual:
        initial_urls = load_manual_urls(args.manual)
        if not output_name:
            output_name = os.path.splitext(os.path.basename(args.manual))[0]
    elif args.country:
        initial_urls = INITIAL_URLS.get(args.country, [])
        if not output_name:
            output_name = args.country
        print(f"✓ Usando {len(initial_urls)} URLs predefinidas para {args.country}")
    else:
        print("❌ Error: Debes especificar --country o --manual")
        parser.print_help()
        return
    
    if not initial_urls:
        print("❌ No se encontraron URLs para scrapear")
        return
    
    # Ejecutar scraping
    print(f"\n🚀 Iniciando scraping con profundidad máxima: {args.depth}")
    print("=" * 70)
    
    persons, rels = scrape_family_tree(initial_urls, max_depth=args.depth)
    
    # Guardar resultados
    print("\n" + "=" * 70)
    print("💾 Guardando resultados...")
    save_data(persons, rels, output_name)
    
    print(f"\n✅ Scraping completado exitosamente")
    print(f"   Personas encontradas: {len(persons)}")
    print(f"   Relaciones encontradas: {len(rels)}")


if __name__ == "__main__":
    main()
