#!/bin/bash
# Script para scrapear familias políticas de Argentina
# Uso: ./scrape_familias_politicas_argentina.sh

export WIKI_USER_AGENT='familiares/1.0'

echo "🚀 Scrapeando Familias Políticas de Argentina"
echo "=============================================="
echo ""
echo "Este script scrapeará todas las familias de la categoría:"
echo "https://es.wikipedia.org/wiki/Categoría:Familias_políticas_de_Argentina"
echo ""
echo "Presiona Enter para continuar o Ctrl+C para cancelar..."
read

cd "$(dirname "$0")/../.."

python3 scripts/01_scraping/scraper_all_families.py \
  --country argentina \
  --category-custom "Familias_políticas_de_Argentina" \
  --resume \
  --workers 1

echo ""
echo "✅ Scraping completado!"
echo "Los archivos se guardaron en: data/raw/argentina/familias/"
