#!/bin/bash
# Script maestro para scrapear familias de todos los países latinoamericanos
# Uso: ./scrape_todos_paises.sh

export WIKI_USER_AGENT='familiares/1.0'
cd "$(dirname "$0")/../.."

echo "🌎 Scrapeando Familias de Todos los Países Latinoamericanos"
echo "=============================================================="
echo ""
echo "Este script scrapeará familias de:"
echo "  - México (116 familias)"
echo "  - Perú (200 familias)"
echo "  - Colombia (161 familias)"
echo "  - Venezuela (25 familias)"
echo "  - Uruguay (13 familias)"
echo "  - Bolivia (6 familias)"
echo "  - Ecuador (13 familias)"
echo "  - Paraguay (3 familias)"
echo ""
echo "⚠️  ADVERTENCIA: Esto puede tomar MUCHAS horas (8-12 horas estimadas)"
echo ""
echo "Presiona Enter para continuar o Ctrl+C para cancelar..."
read

PAISES=("mexico" "peru" "colombia" "venezuela" "uruguay" "bolivia" "ecuador" "paraguay")

for pais in "${PAISES[@]}"; do
    echo ""
    echo "=============================================================="
    echo "🚀 Iniciando scraping de: $pais"
    echo "=============================================================="
    python3 scripts/01_scraping/scraper_all_families.py --country "$pais" --resume --workers 1
    echo ""
    echo "✅ Completado: $pais"
    echo "Esperando 30 segundos antes del siguiente país..."
    sleep 30
done

echo ""
echo "=============================================================="
echo "✅ SCRAPING COMPLETADO PARA TODOS LOS PAÍSES"
echo "=============================================================="
