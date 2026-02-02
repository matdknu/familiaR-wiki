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
from concurrent.futures import ProcessPoolExecutor, as_completed
from scraper_categories import (
    scrape_family_from_category,
    save_data,
    BASE_URL,
    get_output_path,
    get_project_root,
    get_headers,
    get_api_headers,
)
from urllib.parse import quote

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
            print(f"❌ Error: {e}. Reintento {i+1}/{retries}")
            time.sleep(base_delay * (i + 1))
    return None


COUNTRY_CATEGORY = {
    "chile": "Familias_de_Chile",
    "argentina": "Familias_de_Argentina",
    "mexico": "Familias_de_México",
    "peru": "Familias_de_Perú",
    "colombia": "Familias_de_Colombia",
    "venezuela": "Familias_de_Venezuela",
    "uruguay": "Familias_de_Uruguay",
    "bolivia": "Familias_de_Bolivia",
    "ecuador": "Familias_de_Ecuador",
    "paraguay": "Familias_de_Paraguay",
}


def extract_family_categories(country="chile", custom_category=None):
    """
    Extrae todas las categorías de familias desde una categoría
    
    Args:
        country: País (chile, argentina, mexico)
        custom_category: Categoría personalizada (ej: "Familias_políticas_de_Argentina")
    
    Returns:
        list: Lista de diccionarios con {nombre, url}
    """
    api_url = f"{BASE_URL}/w/api.php"
    
    if custom_category:
        category_key = custom_category
    else:
        category_key = COUNTRY_CATEGORY.get(country.lower(), None)
        if not category_key:
            raise ValueError(f"País no soportado: {country}")
    
    category_title = f"Categoría:{category_key}"

    print(f"\n🔍 Buscando categorías de familias en: {BASE_URL}/wiki/{category_title}")
    print("=" * 80)

    families = []
    params = {
        'action': 'query',
        'list': 'categorymembers',
        'cmtitle': category_title,
        'cmtype': 'subcat',
        'cmlimit': 'max',
        'format': 'json'
    }

    while True:
        resp = requests.get(api_url, headers=get_api_headers(), params=params, timeout=15)
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
            if 'familia' not in title.lower():
                continue
            url_title = quote(title.replace(' ', '_'))
            full_url = f"{BASE_URL}/wiki/{url_title}"
            families.append({
                'nombre': title.replace('Categoría:', ''),
                'url': full_url
            })
            print(f"  ✓ {title.replace('Categoría:', '')}")

        if 'continue' in data:
            params.update(data['continue'])
        else:
            break
    
    print(f"\n✅ Total de familias encontradas: {len(families)}")
    return families


def scrape_and_save_family(family, resume=False, country="chile"):
    """Scrapea una familia y guarda el CSV individual."""
    try:
        df, family_name = scrape_family_from_category(
            family['url'],
            family['nombre'],
            resume=resume,
            country=country
        )
        if df is not None and not df.empty:
            save_data(df, family_name, country=country)
            return {"status": "success", "family": family['nombre']}
        return {"status": "empty", "family": family['nombre']}
    except Exception as e:
        return {"status": "error", "family": family['nombre'], "error": str(e)}


def scrape_all_families(limit=None, workers=1, resume=False, country="chile", custom_category=None):
    """
    Scrapea todas las familias
    
    Args:
        limit: Número máximo de familias a scrapear (None = todas)
        country: País (chile, argentina, mexico)
        custom_category: Categoría personalizada (ej: "Familias_políticas_de_Argentina")
    """
    print("\n" + "=" * 80)
    category_name = custom_category if custom_category else COUNTRY_CATEGORY.get(country.lower(), country)
    print(f"🚀 SCRAPING MASIVO DE FAMILIAS ({country.upper()})")
    if custom_category:
        print(f"📂 Categoría: {custom_category}")
    print("=" * 80)
    
    # Obtener lista de familias
    families = extract_family_categories(country=country, custom_category=custom_category)
    
    if not families:
        print("❌ No se encontraron familias para scrapear")
        return
    
    # Aplicar límite si se especificó
    if limit:
        families = families[:limit]
        print(f"\n⚠️  Limitando a las primeras {limit} familias")

    families_all = list(families)
    if resume:
        remaining = []
        skipped = 0
        for family in families:
            output_path = get_output_path(family['nombre'])
            if output_path.exists():
                skipped += 1
            else:
                remaining.append(family)
        families = remaining
        print(f"\n↩️  Reanudar activo: {skipped} familias ya existían")

    # Scrapear cada familia
    successful = 0
    failed = 0

    if workers and workers > 1:
        print(f"\n⚙️  Modo paralelo activado: {workers} workers")
        with ProcessPoolExecutor(max_workers=workers) as executor:
            futures = {
                executor.submit(scrape_and_save_family, family, resume, country): family
                for family in families
            }
            for future in as_completed(futures):
                result = future.result()
                if result["status"] == "success":
                    successful += 1
                    print(f"✅ {result['family']}: OK")
                elif result["status"] == "empty":
                    failed += 1
                    print(f"⚠️  {result['family']}: Sin datos")
                else:
                    failed += 1
                    print(f"❌ Error en {result['family']}: {result.get('error')}")
    else:
        for i, family in enumerate(families, 1):
            print(f"\n{'=' * 80}")
            print(f"[{i}/{len(families)}] Procesando: {family['nombre']}")
            print(f"{'=' * 80}")

            result = scrape_and_save_family(family, resume=resume, country=country)
            if result["status"] == "success":
                successful += 1
                print(f"✅ {family['nombre']}: OK")
            elif result["status"] == "empty":
                failed += 1
                print(f"⚠️  {family['nombre']}: Sin datos")
            else:
                failed += 1
                print(f"❌ Error en {family['nombre']}: {result.get('error')}")

            # Delay entre familias
            time.sleep(2)

    # Consolidar todos los datos
    output_dir = get_project_root() / "data" / "raw" / country.lower() / "familias"
    output_files = []
    for family in families_all:
        output_path = get_output_path(family['nombre'], country=country)
        if output_path.exists():
            output_files.append(output_path)

    if output_files:
        print("\n" + "=" * 80)
        print("📊 Consolidando datos...")

        all_data = []
        for path in output_files:
            try:
                df = pd.read_csv(path, sep=';', encoding='utf-8')
                if not df.empty:
                    all_data.append(df)
            except Exception as e:
                print(f"⚠️  No se pudo leer {path}: {e}")

        if all_data:
            df_consolidated = pd.concat(all_data, ignore_index=True)

            # Eliminar duplicados (personas que aparecen en múltiples familias)
            if 'url' in df_consolidated.columns:
                df_consolidated.drop_duplicates(subset=['url'], keep='first', inplace=True)

            # Guardar archivo consolidado
            consolidated_file = output_dir / "_CONSOLIDADO_todas_familias.csv"
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
    else:
        print("\n❌ No hay archivos disponibles para consolidar")


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
    parser.add_argument(
        '--workers',
        type=int,
        default=1,
        help='Número de procesos en paralelo (default: 1)'
    )
    parser.add_argument(
        '--resume',
        action='store_true',
        help='Reanudar saltando familias ya descargadas'
    )
    parser.add_argument(
        '--country',
        type=str,
        default='chile',
        help='País a scrapear (chile, argentina, mexico)'
    )
    parser.add_argument(
        '--category-custom',
        type=str,
        default=None,
        help='Categoría personalizada (ej: "Familias_políticas_de_Argentina")'
    )
    
    args = parser.parse_args()
    
    scrape_all_families(
        limit=args.limit, 
        workers=args.workers, 
        resume=args.resume, 
        country=args.country,
        custom_category=args.category_custom
    )


if __name__ == "__main__":
    main()
