#!/usr/bin/env python3
"""
Enriquecer df_consolidado con la API de OpenAI (GPT).

Extrae y normaliza:
  - cargo_1, cargo_2, cargo_3 (cargos limpios)
  - partido_limpio
  - nacimiento_ano, fallecimiento_ano
  - hijos_lista
  - conexion_otros_paises (texto breve)
  - redes_dentro_pais, redes_entre_paises (texto breve)

Uso:
  export OPENAI_API_KEY=sk-...
  python scripts/02_processing/enriquecer_con_gpt.py --muestra 10   # prueba
  python scripts/02_processing/enriquecer_con_gpt.py               # todo

Coste estimado: ver COSTOS_API_GPT.md (~1.50–3 USD con GPT-5 mini para ~6700 filas).
"""

import os
import re
import json
import argparse
import time
from pathlib import Path

import pandas as pd

try:
    from openai import OpenAI
except ImportError:
    OpenAI = None

# Rutas (desde raíz del proyecto)
BASE = Path(__file__).resolve().parent.parent.parent
DF_IN = BASE / "outputs" / "df_consolidado.csv"
DF_OUT = BASE / "outputs" / "df_consolidado_enriquecido_gpt.csv"
PROGRESS_FILE = BASE / "outputs" / ".enriquecer_gpt_progress.json"

SYSTEM_PROMPT = """Eres un asistente que extrae datos estructurados de biografías de personajes latinoamericanos.
Responde ÚNICAMENTE con un JSON válido, sin markdown ni texto extra.
Usa null para campos desconocidos. Listas como arrays. Años como números (ej: 1770)."""

USER_PROMPT_TEMPLATE = """De la siguiente ficha, extrae y devuelve un JSON con exactamente estas claves:
- cargo_1, cargo_2, cargo_3: los tres cargos más relevantes (políticos, militares, institucionales), normalizados y breves (ej: "Presidente de Chile", "Senador").
- partido_limpio: partido político si se menciona, sino null.
- nacimiento_ano: año de nacimiento (número) o null.
- fallecimiento_ano: año de fallecimiento (número) o null.
- hijos_lista: array de nombres de hijos si se mencionan, o [].
- conexion_otros_paises: breve texto si tiene nacionalidad múltiple, residencia en otro país o vínculos transfronterizos; sino null.
- redes_dentro_pais: breve texto sobre vínculos con otras élites/familias del mismo país (matrimonios, cargos compartidos); sino null.
- redes_entre_paises: breve texto sobre vínculos con personas/élites de otros países; sino null.

Ficha:
Nombre: {nombre}
País (registro): {pais}
Nacionalidad: {nacionalidad}
Biografía (inicio): {biografia}
Cargos (crudo): {cargos}
Padres: {padres}
Cónyuge: {conyuge}
Hijos: {hijos}
Partido: {partido}
Nacimiento: {fecha_nac}
Fallecimiento: {fecha_fall}
"""


def get_client():
    if OpenAI is None:
        raise SystemExit("Instala: pip install openai")
    key = os.environ.get("OPENAI_API_KEY") or os.environ.get("OPENAI_KEY")
    if not key:
        raise SystemExit("Define OPENAI_API_KEY en el entorno (o OPENAI_KEY)")
    return OpenAI(api_key=key)


def safe_str(x):
    if pd.isna(x) or x == "":
        return ""
    return str(x).strip()[:2000]


def parse_gpt_json(text: str) -> dict:
    """Extrae un único objeto JSON del texto (puede venir envuelto en ```json ... ```)."""
    text = (text or "").strip()
    # Quitar bloque markdown
    m = re.search(r"```(?:json)?\s*([\s\S]*?)```", text)
    if m:
        text = m.group(1).strip()
    # Buscar primer { ... }
    start = text.find("{")
    if start == -1:
        return {}
    depth = 0
    for i in range(start, len(text)):
        if text[i] == "{":
            depth += 1
        elif text[i] == "}":
            depth -= 1
            if depth == 0:
                try:
                    return json.loads(text[start : i + 1])
                except json.JSONDecodeError:
                    pass
                break
    return {}


def enrich_one(client, row, model: str = "gpt-4o-mini") -> dict:
    """Llama a la API para una fila y devuelve el diccionario enriquecido."""
    prompt = USER_PROMPT_TEMPLATE.format(
        nombre=safe_str(row.get("nombre")),
        pais=safe_str(row.get("pais")),
        nacionalidad=safe_str(row.get("nacionalidad")),
        biografia=safe_str(row.get("biografia_inicial")),
        cargos=safe_str(row.get("cargos")),
        padres=safe_str(row.get("padres")),
        conyuge=safe_str(row.get("conyuge")),
        hijos=safe_str(row.get("hijos")),
        partido=safe_str(row.get("partido_politico")),
        fecha_nac=safe_str(row.get("fecha_nacimiento")),
        fecha_fall=safe_str(row.get("fecha_fallecimiento")),
    )
    try:
        r = client.chat.completions.create(
            model=model,
            messages=[
                {"role": "system", "content": SYSTEM_PROMPT},
                {"role": "user", "content": prompt},
            ],
            temperature=0.1,
            max_tokens=500,
        )
        content = (r.choices[0].message.content or "").strip()
        return parse_gpt_json(content)
    except Exception as e:
        return {"_error": str(e)}


def main():
    parser = argparse.ArgumentParser(description="Enriquecer df_consolidado con API GPT")
    parser.add_argument("--muestra", type=int, default=0, help="Solo procesar N filas (0 = todas)")
    parser.add_argument("--model", type=str, default="gpt-4o-mini", help="Modelo OpenAI (ej: gpt-4o-mini, gpt-4o)")
    parser.add_argument("--delay", type=float, default=0.5, help="Segundos entre llamadas para no saturar")
    parser.add_argument("--resumir", action="store_true", help="Reanudar desde última fila guardada")
    args = parser.parse_args()

    if not DF_IN.exists():
        raise SystemExit(f"No se encontró {DF_IN}. Ejecuta antes leer_consolidado.R para generar el CSV.")

    df = pd.read_csv(DF_IN, nrows=None)
    total = len(df)
    if args.muestra and args.muestra < total:
        df = df.head(args.muestra)
        total = len(df)

    start_idx = 0
    if args.resumir and PROGRESS_FILE.exists():
        try:
            with open(PROGRESS_FILE) as f:
                start_idx = json.load(f).get("last_index", 0)
            print(f"Reanudando desde fila {start_idx}")
        except Exception:
            pass

    # Columnas que añadirá GPT
    extra_cols = [
        "cargo_1", "cargo_2", "cargo_3",
        "partido_limpio",
        "nacimiento_ano", "fallecimiento_ano",
        "hijos_lista",
        "conexion_otros_paises",
        "redes_dentro_pais",
        "redes_entre_paises",
    ]
    for c in extra_cols:
        if c not in df.columns:
            df[c] = None

    client = get_client()
    for i in range(start_idx, total):
        row = df.iloc[i]
        out = enrich_one(client, row, model=args.model)
        if "_error" in out:
            print(f"Fila {i}: error -> {out['_error']}")
            continue
        for k in extra_cols:
            if k in out and out[k] is not None:
                v = out[k]
                if k == "hijos_lista" and isinstance(v, list):
                    df.at[df.index[i], k] = " | ".join(str(x) for x in v) if v else ""
                elif k in ("nacimiento_ano", "fallecimiento_ano") and isinstance(v, (int, float)):
                    df.at[df.index[i], k] = int(v)
                else:
                    df.at[df.index[i], k] = v if not isinstance(v, list) else json.dumps(v)
        if (i + 1) % 50 == 0:
            df.to_csv(DF_OUT, index=False)
            with open(PROGRESS_FILE, "w") as f:
                json.dump({"last_index": i + 1}, f)
            print(f"Procesadas {i + 1}/{total} filas. Guardado en {DF_OUT}")
        time.sleep(args.delay)

    df.to_csv(DF_OUT, index=False)
    if PROGRESS_FILE.exists():
        PROGRESS_FILE.unlink()
    print(f"Listo. Output: {DF_OUT} ({total} filas)")


if __name__ == "__main__":
    main()
