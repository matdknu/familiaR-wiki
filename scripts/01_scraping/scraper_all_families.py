"""
Script para scrapear TODAS las familias desde la categoría principal

Este script:
1. Lee la página de Categoría:Familias_de_Chile
2. Extrae todas las subcategorías de familias
3. Scrapea cada familia usando scraper_categories.py
4. Guarda un archivo consolidado con todas las familias

Uso:
    python scraper_all_families.py
    python scraper_all_families.py --limit 5  # Scrapear solo primeras 5 familias (para testing)
"""

import requests
from bs4 import BeautifulSoup
import pandas as pd
import time
import argparse
import os
from scraper_categories import scrape_family_from_category, HEADERS, BASE_URL

def get_soup(url, retries=3):
    """Obtiene BeautifulSoup de una URL con reintentos"""
    for i in range(retries):
        try:
            resp = requests.get(url, headers=HEADERS, timeout=15)
            resp.raise_for_status()
            return BeautifulSoup(resp.text, 'html.parser')
        except Exception as e:
            print(f"❌ Error: {e}. Reintento {i+1}/{retries}")
            time.sleep(2)
    return None


def extract_family_categories():
    """
    Extrae todas las categorías de familias desde Categoría:Familias_de_Chile
    
    Returns:
        list: Lista de diccionarios con {nombre, url}
    """
    url = f"{BASE_URL}/wiki/Categoría:Familias_de_Chile"
    
    print(f"\n🔍 Buscando categorías de familias en: {url}")
    print("=" * 80)
    
    soup = get_soup(url)
    if not soup:
        return []
    
    families = []
    
    # Buscar la sección de subcategorías
    subcats_section = soup.find('div', {'id': 'mw-subcategories'})
    
    if not subcats_section:
        print("⚠️  No se encontró la sección de subcategorías")
        return []
    
    # Extraer todos los enlaces a categorías de familias
    links = subcats_section.find_all('a')
    
    for link in links:
        href = link.get('href', '')
        text = link.get_text(strip=True)
        
        # Filtrar solo categorías de familias específicas
        if href.startswith('/wiki/Categoría:Familia_') or href.startswith('/wiki/Categoría:Famila_'):
            full_url = BASE_URL + href
            families.append({
                'nombre': text.replace('Categoría:', ''),
                'url': full_url
            })
            print(f"  ✓ {text}")
    
    print(f"\n✅ Total de familias encontradas: {len(families)}")
    return families


def scrape_all_families(limit=None):
    """
    Scrapea todas las familias chilenas
    
    Args:
        limit: Número máximo de familias a scrapear (None = todas)
    """
    print("\n" + "=" * 80)
    print("🚀 SCRAPING MASIVO DE FAMILIAS CHILENAS")
    print("=" * 80)
    
    # Obtener lista de familias
    families = extract_family_categories()
    
    if not families:
        print("❌ No se encontraron familias para scrapear")
        return
    
    # Aplicar límite si se especificó
    if limit:
        families = families[:limit]
        print(f"\n⚠️  Limitando a las primeras {limit} familias")
    
    # Crear directorio de salida
    output_dir = "data/raw/chile/familias"
    os.makedirs(output_dir, exist_ok=True)
    
    # Scrapear cada familia
    all_data = []
    successful = 0
    failed = 0
    
    for i, family in enumerate(families, 1):
        print(f"\n{'=' * 80}")
        print(f"[{i}/{len(families)}] Procesando: {family['nombre']}")
        print(f"{'=' * 80}")
        
        try:
            df, family_name = scrape_family_from_category(family['url'], family['nombre'])
            
            if df is not None and not df.empty:
                # Guardar archivo individual por familia
                safe_name = family_name.replace(' ', '_').replace(':', '').lower()
                filename = f"{output_dir}/{safe_name}_completo.csv"
                df.to_csv(filename, index=False, sep=';', encoding='utf-8')
                
                # Agregar al dataset consolidado
                all_data.append(df)
                successful += 1
                
                print(f"✅ {family['nombre']}: {len(df)} personas extraídas")
            else:
                failed += 1
                print(f"⚠️  {family['nombre']}: Sin datos")
        
        except Exception as e:
            failed += 1
            print(f"❌ Error en {family['nombre']}: {e}")
        
        # Delay entre familias
        time.sleep(2)
    
    # Consolidar todos los datos
    if all_data:
        print("\n" + "=" * 80)
        print("📊 Consolidando datos...")
        
        df_consolidated = pd.concat(all_data, ignore_index=True)
        
        # Eliminar duplicados (personas que aparecen en múltiples familias)
        df_consolidated.drop_duplicates(subset=['url'], keep='first', inplace=True)
        
        # Guardar archivo consolidado
        consolidated_file = f"{output_dir}/_CONSOLIDADO_todas_familias.csv"
        df_consolidated.to_csv(consolidated_file, index=False, sep=';', encoding='utf-8')
        
        print(f"\n" + "=" * 80)
        print("✅ SCRAPING COMPLETADO")
        print("=" * 80)
        print(f"   Familias exitosas: {successful}")
        print(f"   Familias fallidas: {failed}")
        print(f"   Total de personas: {len(df_consolidated)}")
        print(f"   Archivo consolidado: {consolidated_file}")
        print("=" * 80)
    else:
        print("\n❌ No se pudo extraer ningún dato")


def main():
    parser = argparse.ArgumentParser(
        description='Scrapear todas las familias chilenas desde Wikipedia'
    )
    parser.add_argument(
        '--limit',
        type=int,
        default=None,
        help='Número máximo de familias a scrapear (útil para testing)'
    )
    
    args = parser.parse_args()
    
    scrape_all_families(limit=args.limit)


if __name__ == "__main__":
    main()
