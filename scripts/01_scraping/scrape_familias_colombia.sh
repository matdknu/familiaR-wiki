#!/bin/bash
# Script para scrapear familias de Colombia
export WIKI_USER_AGENT='familiares/1.0'
cd "$(dirname "$0")/../.."
python3 scripts/01_scraping/scraper_all_families.py --country colombia --resume --workers 1
