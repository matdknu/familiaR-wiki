#!/bin/bash
# Script para scrapear familias de Ecuador
export WIKI_USER_AGENT='familiares/1.0'
cd "$(dirname "$0")/../.."
python3 scripts/01_scraping/scraper_all_families.py --country ecuador --resume --workers 1
