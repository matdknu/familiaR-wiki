#!/bin/bash
# Script para scrapear países faltantes: Colombia, Paraguay, Bolivia, Uruguay, Ecuador
# Uso: ./scrape_paises_faltantes.sh

export WIKI_USER_AGENT='familiares/1.0'
cd "$(dirname "$0")/../.."

echo "🌎 Scrapeando Países Faltantes"
echo "=============================================="
echo ""
echo "Este script scrapeará:"
echo "  - Colombia (43 familias faltantes de 161)"
echo "  - Paraguay (3 familias)"
echo "  - Bolivia (6 familias)"
echo "  - Uruguay (13 familias)"
echo "  - Ecuador (13 familias)"
echo ""
echo "Presiona Enter para continuar o Ctrl+C para cancelar..."
read

PAISES=("colombia" "paraguay" "bolivia" "uruguay" "ecuador")

for pais in "${PAISES[@]}"; do
    echo ""
    echo "=============================================================="
    echo "🚀 Iniciando scraping de: $pais"
    echo "=============================================================="
    python3 scripts/01_scraping/scraper_all_families.py --country "$pais" --resume --workers 1
    
    if [ $? -eq 0 ]; then
        echo ""
        echo "✅ Completado: $pais"
    else
        echo ""
        echo "⚠️  Error en: $pais (puede continuar con el siguiente)"
    fi
    
    echo "Esperando 10 segundos antes del siguiente país..."
    sleep 10
done

echo ""
echo "=============================================================="
echo "✅ SCRAPING COMPLETADO"
echo "=============================================================="
echo ""
echo "Verificando consolidados..."

# Consolidar cada país
for pais in "${PAISES[@]}"; do
    if [ -d "data/raw/$pais/familias" ]; then
        python3 << PYEOF
import pandas as pd
from pathlib import Path

output_dir = Path("data/raw/$pais/familias")
csv_files = [f for f in output_dir.glob("*_completo.csv") if not f.name.startswith("_CONSOLIDADO")]

if csv_files:
    all_data = []
    for csv_file in csv_files:
        try:
            df = pd.read_csv(csv_file, sep=';', encoding='utf-8')
            if not df.empty:
                all_data.append(df)
        except:
            pass
    
    if all_data:
        df_consolidated = pd.concat(all_data, ignore_index=True)
        if 'url' in df_consolidated.columns:
            df_consolidated.drop_duplicates(subset=['url'], keep='first', inplace=True)
        
        output_file = output_dir / "_CONSOLIDADO_todas_familias.csv"
        df_consolidated.to_csv(output_file, index=False, sep=';', encoding='utf-8')
        print(f"✅ $pais: {len(df_consolidated)} personas consolidadas")
PYEOF
    fi
done

echo ""
echo "✅ Proceso completado!"
