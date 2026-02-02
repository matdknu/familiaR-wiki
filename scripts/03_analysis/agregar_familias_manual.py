"""
Agrega familias manuales (extras) por país a los consolidado procesados
`data/processed/familias/<pais>/consolidado.csv` y vuelve a generar el LATAM.

Entrada esperada: CSVs en `data/manual/familias_extra_<pais>.csv`
con el mismo header que los consolidado originales (se asume ';' como separador).

Uso:
    python scripts/03_analysis/agregar_familias_manual.py
"""

import os
import glob
import pandas as pd


def load_manual():
    base = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", ".."))
    pattern = os.path.join(base, "data", "manual", "familias_extra_*.csv")
    files = glob.glob(pattern)
    manual = {}
    for path in files:
        country = os.path.basename(path).replace("familias_extra_", "").replace(".csv", "")
        manual[country] = pd.read_csv(path, sep=";", dtype=str)
    return manual


def main():
    base = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", ".."))
    processed_dir = os.path.join(base, "data", "processed", "familias")

    manual = load_manual()
    if not manual:
        print("❌ No se encontraron archivos en data/manual/familias_extra_*.csv")
        return

    combined = []
    for country, df_extra in manual.items():
        path_processed = os.path.join(processed_dir, country, "consolidado.csv")
        if os.path.exists(path_processed):
            df_base = pd.read_csv(path_processed, sep=";", dtype=str)
        else:
            # fallback: intentar desde raw
            path_raw = os.path.join(base, "data", "raw", country, "familias", "_CONSOLIDADO_todas_familias.csv")
            if os.path.exists(path_raw):
                df_base = pd.read_csv(path_raw, sep=";", dtype=str)
            else:
                print(f"⚠️ No se encontró consolidado para {country}, se usará solo manual.")
                df_base = pd.DataFrame(columns=df_extra.columns)

        # agregar extra y quitar duplicados por URL si existe la columna
        df_out = pd.concat([df_base, df_extra], ignore_index=True)
        if "url" in df_out.columns:
            df_out = df_out.drop_duplicates(subset=["url"], keep="first")

        os.makedirs(os.path.dirname(path_processed), exist_ok=True)
        df_out.to_csv(path_processed, sep=";", index=False)
        print(f"✅ Actualizado {path_processed} (filas={len(df_out)})")

        # para LATAM
        df_latam = df_out.copy()
        df_latam["pais"] = country
        combined.append(df_latam)

    # recomponer LATAM usando TODOS los consolidado procesados
    latam_files = glob.glob(os.path.join(processed_dir, "*", "consolidado.csv"))
    latam_frames = []
    for fpath in latam_files:
        country = fpath.split(os.sep)[-2]
        df_c = pd.read_csv(fpath, sep=";", dtype=str)
        df_c["pais"] = country
        latam_frames.append(df_c)
    if latam_frames:
        latam = pd.concat(latam_frames, ignore_index=True)
        latam_path = os.path.join(processed_dir, "_CONSOLIDADO_familias_latam.csv")
        latam.to_csv(latam_path, sep=";", index=False)
        print(f"✅ LATAM actualizado: {latam_path} (filas={len(latam)})")


if __name__ == "__main__":
    main()
