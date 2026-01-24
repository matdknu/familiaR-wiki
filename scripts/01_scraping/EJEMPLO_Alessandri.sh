#!/bin/bash

# =============================================================================
# EJEMPLO: Scrapear la Familia Alessandri desde Wikipedia
# =============================================================================
# 
# Este script demuestra cómo usar scraper_categories.py para extraer
# toda la información de la Familia Alessandri desde su categoría de Wikipedia
#
# Uso: bash EJEMPLO_Alessandri.sh
# =============================================================================

echo "=============================================================================="
echo "🚀 EJEMPLO: Scraping de Familia Alessandri"
echo "=============================================================================="
echo ""
echo "Este script va a:"
echo "  1. Acceder a https://es.wikipedia.org/wiki/Categoría:Familia_Alessandri"
echo "  2. Extraer todos los miembros listados (aprox. 20 personas)"
echo "  3. Para cada persona, extraer su infobox completo"
echo "  4. Guardar en: data/raw/chile/familias/familia_alessandri_completo.csv"
echo ""
echo "Presiona Enter para continuar o Ctrl+C para cancelar..."
read

# Ejecutar el scraper
python scraper_categories.py --category "Familia_Alessandri"

echo ""
echo "=============================================================================="
echo "✅ Scraping completado!"
echo "=============================================================================="
echo ""
echo "Para ver los datos extraídos:"
echo "  - Abrir: data/raw/chile/familias/familia_alessandri_completo.csv"
echo ""
echo "Otros ejemplos de familias para scrapear:"
echo "  - python scraper_categories.py --category 'Familia_Piñera'"
echo "  - python scraper_categories.py --category 'Familia_Bachelet'"
echo "  - python scraper_categories.py --category 'Familia_Montt'"
echo "  - python scraper_categories.py --category 'Familia_Allende'"
echo ""
echo "Para scrapear TODAS las familias chilenas:"
echo "  - python scraper_all_families.py"
echo ""
