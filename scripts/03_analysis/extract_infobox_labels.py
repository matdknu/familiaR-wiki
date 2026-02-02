"""
Extrae las etiquetas ("label") presentes en el campo `infobox_json` de todos
los archivos `_CONSOLIDADO_todas_familias.csv` y genera tablas con los
conteos globales y por país.

Salidas:
- outputs/tables/infobox_labels_global.csv        (label, count)
- outputs/tables/infobox_labels_por_pais.csv     (pais, label, count)
- outputs/tables/infobox_labels_top20_por_pais.csv (pais, label, count) solo top 20 por país

Uso:
    python scripts/03_analysis/extract_infobox_labels.py
"""

import csv
import json
import glob
import os
import re
from collections import Counter, defaultdict


def clean_text(text: str) -> str:
    if text is None:
        return ""
    # Remover caracteres de control
    text = re.sub(r"[\x00-\x1F\x7F]", " ", text)
    return text.strip()


def parse_infobox(text: str):
    t = clean_text(text)
    if not t:
        return None
    try:
        return json.loads(t)
    except Exception:
        return None


def main():
    base = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", ".."))
    pattern_list = [
        os.path.join(base, "data", "raw", "*", "familias", "_CONSOLIDADO_todas_familias.csv"),
        os.path.join(base, "data", "raw", "*", "*", "familias", "_CONSOLIDADO_todas_familias.csv"),
    ]

    files = []
    for pattern in pattern_list:
        files.extend(glob.glob(pattern))

    all_labels = Counter()
    labels_by_country = defaultdict(Counter)

    for path in files:
        country = path.split(os.sep)[-3]
        with open(path, newline="", encoding="utf-8") as f:
            reader = csv.DictReader(f, delimiter=";")
            if "infobox_json" not in reader.fieldnames:
                continue
            for row in reader:
                data = parse_infobox(row.get("infobox_json"))
                if isinstance(data, list):
                    for item in data:
                        if isinstance(item, dict):
                            label = clean_text(item.get("label", ""))
                            if label:
                                all_labels[label] += 1
                                labels_by_country[country][label] += 1

    out_dir = os.path.join(base, "outputs", "tables")
    os.makedirs(out_dir, exist_ok=True)

    # Global
    with open(os.path.join(out_dir, "infobox_labels_global.csv"), "w", newline="", encoding="utf-8") as f:
        writer = csv.writer(f)
        writer.writerow(["label", "count"])
        for label, count in all_labels.most_common():
            writer.writerow([label, count])

    # Por país
    with open(os.path.join(out_dir, "infobox_labels_por_pais.csv"), "w", newline="", encoding="utf-8") as f:
        writer = csv.writer(f)
        writer.writerow(["pais", "label", "count"])
        for country, ctr in labels_by_country.items():
            for label, count in ctr.most_common():
                writer.writerow([country, label, count])

    # Top 20 por país (para lectura rápida)
    with open(os.path.join(out_dir, "infobox_labels_top20_por_pais.csv"), "w", newline="", encoding="utf-8") as f:
        writer = csv.writer(f)
        writer.writerow(["pais", "label", "count"])
        for country, ctr in labels_by_country.items():
            for label, count in ctr.most_common(20):
                writer.writerow([country, label, count])

    print(f"Procesados {len(files)} archivos.")
    print(f"Labels distintos (global): {len(all_labels)}")
    if files:
        print(f"Salidas en: {out_dir}")


if __name__ == "__main__":
    main()
