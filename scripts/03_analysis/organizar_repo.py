"""
Organiza copias limpias de los consolidado de familias:
- Copia cada `_CONSOLIDADO_todas_familias.csv` de data/raw/<pais>/familias/
  a `data/processed/familias/<pais>/consolidado.csv`
- Genera un consolidado latinoamericano en
  `data/processed/familias/_CONSOLIDADO_familias_latam.csv`
- Genera un resumen de filas por país en
  `outputs/tables/familias/resumen_conteo_filas.csv`

No borra ni mueve archivos originales; solo crea copias ordenadas.

Uso:
    python scripts/03_analysis/organizar_repo.py
"""

import os
import glob
import pandas as pd


def main():
    base = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", ".."))
    raw_pattern = os.path.join(base, "data", "raw", "*", "familias", "_CONSOLIDADO_todas_familias.csv")
    raw_pattern_nested = os.path.join(base, "data", "raw", "*", "*", "familias", "_CONSOLIDADO_todas_familias.csv")

    files = glob.glob(raw_pattern) + glob.glob(raw_pattern_nested)
    if not files:
        print("❌ No se encontraron consolidado en data/raw/*/familias/")
        return

    processed_dir = os.path.join(base, "data", "processed", "familias")
    tables_dir = os.path.join(base, "outputs", "tables", "familias")
    os.makedirs(processed_dir, exist_ok=True)
    os.makedirs(tables_dir, exist_ok=True)

    combined = []
    resumen = []

    for path in files:
        # país = carpeta anterior a "familias"
        parts = path.split(os.sep)
        try:
            idx = parts.index("familias")
            country = parts[idx - 1]
        except ValueError:
            country = "desconocido"

        print(f"📂 Procesando {country}: {path}")
        df = pd.read_csv(path, sep=";", dtype=str)

        # Guardar copia ordenada por país
        out_dir_country = os.path.join(processed_dir, country)
        os.makedirs(out_dir_country, exist_ok=True)
        out_country_path = os.path.join(out_dir_country, "consolidado.csv")
        df.to_csv(out_country_path, sep=";", index=False)

        resumen.append({"pais": country, "filas": len(df)})

        # Agregar columna país para el combinado
        df_latam = df.copy()
        df_latam["pais"] = country
        combined.append(df_latam)

    if combined:
        latam = pd.concat(combined, ignore_index=True)
        latam_path = os.path.join(processed_dir, "_CONSOLIDADO_familias_latam.csv")
        latam.to_csv(latam_path, sep=";", index=False)
        print(f"✅ Consolidado LATAM: {latam_path} (filas={len(latam)})")

    resumen_df = pd.DataFrame(resumen)
    resumen_path = os.path.join(tables_dir, "resumen_conteo_filas.csv")
    resumen_df.to_csv(resumen_path, index=False)
    print(f"✅ Resumen por país: {resumen_path}")


if __name__ == "__main__":
    main()
