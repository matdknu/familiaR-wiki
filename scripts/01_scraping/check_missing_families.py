"""
Script para verificar qué familias faltan scrapear

Uso:
    python check_missing_families.py
"""

import requests
from pathlib import Path
from urllib.parse import quote
import os
import time

BASE_URL = "https://es.wikipedia.org"
API_URL = f"{BASE_URL}/w/api.php"
DEFAULT_USER_AGENT = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36'

def get_project_root():
    """Obtiene la ruta raíz del proyecto."""
    return Path(__file__).resolve().parents[2]

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

def get_all_families_from_wikipedia(retries=3, base_delay=2, rate_limit_delay=60):
    """Obtiene todas las categorías de familias desde Wikipedia con reintentos."""
    category_title = "Categoría:Familias_de_Chile"
    families = []
    params = {
        'action': 'query',
        'list': 'categorymembers',
        'cmtitle': category_title,
        'cmtype': 'subcat',
        'cmlimit': 'max',
        'format': 'json'
    }

    for attempt in range(retries):
        try:
            while True:
                resp = requests.get(API_URL, headers=get_api_headers(), params=params, timeout=15)
                if resp.status_code in (403, 429):
                    retry_after = resp.headers.get('Retry-After')
                    wait = (
                        int(retry_after)
                        if retry_after and retry_after.isdigit()
                        else rate_limit_delay * (attempt + 1)
                    )
                    print(f"⚠️  Rate limit en API. Esperando {wait}s (intento {attempt+1}/{retries})")
                    time.sleep(wait)
                    continue
                resp.raise_for_status()
                data = resp.json()

                for item in data.get('query', {}).get('categorymembers', []):
                    title = item.get('title', '').strip()
                    if not title:
                        continue
                    if 'familia' not in title.lower():
                        continue
                    families.append(title.replace('Categoría:', ''))

                if 'continue' in data:
                    params.update(data['continue'])
                else:
                    break
            
            return sorted(families)
        except Exception as e:
            if attempt < retries - 1:
                wait = base_delay * (attempt + 1)
                print(f"⚠️  Error en intento {attempt+1}/{retries}: {e}")
                print(f"   Reintentando en {wait}s...")
                time.sleep(wait)
            else:
                raise
    
    return sorted(families)

def get_scraped_families():
    """Obtiene las familias ya scrapeadas."""
    output_dir = get_project_root() / "data" / "raw" / "chile" / "familias"
    files = list(output_dir.glob("*_completo.csv"))
    scraped = []
    for f in files:
        family_name = f.stem.replace('_completo', '')
        scraped.append(family_name)
    return sorted(scraped)

def normalize_family_name(name):
    """Normaliza nombre de familia para comparación."""
    return name.lower().replace(' ', '_').replace(':', '').replace('categoría', '').strip()

def main():
    print("=" * 80)
    print("🔍 VERIFICANDO FAMILIAS FALTANTES")
    print("=" * 80)
    
    # Primero obtener familias scrapeadas (siempre funciona)
    print("\n📂 Obteniendo familias ya scrapeadas...")
    scraped_families = get_scraped_families()
    print(f"✅ Total familias scrapeadas: {len(scraped_families)}")
    
    if scraped_families:
        print("\n📋 Primeras 10 familias scrapeadas:")
        for i, family in enumerate(scraped_families[:10], 1):
            print(f"  {i:2d}. {family}")
        if len(scraped_families) > 10:
            print(f"  ... y {len(scraped_families) - 10} más")
    
    # Intentar obtener familias de Wikipedia
    print("\n📊 Obteniendo lista de familias desde Wikipedia...")
    all_families = None
    try:
        all_families = get_all_families_from_wikipedia()
        print(f"✅ Total familias en Wikipedia: {len(all_families)}")
    except Exception as e:
        print(f"❌ Error consultando Wikipedia: {e}")
        print("\n⚠️  No se pudo conectar a Wikipedia para verificar familias faltantes.")
        print("   Esto puede deberse a:")
        print("   - Problemas de conexión a internet")
        print("   - Rate limiting de Wikipedia")
        print("   - Problemas de DNS")
        print("\n💡 Para verificar familias faltantes, ejecuta este script cuando tengas conexión.")
        print("   O ejecuta directamente:")
        print("   cd scripts/01_scraping")
        print("   python3 scraper_all_families.py --resume")
        print("\n   El flag --resume saltará las familias ya scrapeadas automáticamente.")
        return
    
    # Comparar y encontrar faltantes
    if all_families:
        # Normalizar nombres para comparación
        all_normalized = {normalize_family_name(f): f for f in all_families}
        scraped_normalized = {normalize_family_name(f) for f in scraped_families}
        
        # Encontrar faltantes
        missing_normalized = set(all_normalized.keys()) - scraped_normalized
        missing_families = [all_normalized[f] for f in sorted(missing_normalized)]
        
        print("\n" + "=" * 80)
        print(f"📋 FAMILIAS FALTANTES: {len(missing_families)}")
        print("=" * 80)
        
        if missing_families:
            print(f"\nSe encontraron {len(missing_families)} familias que faltan scrapear:\n")
            for i, family in enumerate(missing_families, 1):
                print(f"  {i:3d}. {family}")
        else:
            print("  ✅ ¡Todas las familias ya están scrapeadas!")
        
        print("\n" + "=" * 80)
        print("💡 PARA SCRAPEAR LAS FALTANTES:")
        print("=" * 80)
        print("  cd scripts/01_scraping")
        print("  python3 scraper_all_families.py --resume")
        print("\n  El flag --resume saltará las familias ya scrapeadas automáticamente.")
        print("=" * 80)

if __name__ == "__main__":
    main()
