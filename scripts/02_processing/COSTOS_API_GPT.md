# Estimación de costos: API GPT para enriquecer el consolidado

Enriquecer el `df_consolidado` (~6.700 personas) con la API de OpenAI para obtener campos **limpios y estructurados**:

- **Cargos:** cargo_1, cargo_2, cargo_3, … (normalizados)
- **Partido político**
- **Nacimiento / fallecimiento** (año o fecha normalizada)
- **Hijos** (lista de nombres o cantidad)
- **Conexión con otros países** (sí/no + detalle)
- **Redes dentro del país** (vínculos con otras élites/familias del mismo país)
- **Redes entre países** (vínculos transnacionales)

---

## Tokens aproximados

| Concepto | Por persona | Total (~6.700) |
|---------|-------------|----------------|
| **Input** (prompt sistema + nombre, biografía, cargos, familia, país) | ~500–700 tokens | ~3,5–4,7 M tokens |
| **Output** (JSON con los campos anteriores) | ~100–180 tokens | ~0,7–1,2 M tokens |

---

## Precios OpenAI (referencia 2025)

[OpenAI Pricing](https://openai.com/api-pricing/)

| Modelo | Input (por 1M tokens) | Output (por 1M tokens) | Uso típico |
|--------|------------------------|------------------------|------------|
| **GPT-5 mini** | 0,25 USD | 2,00 USD | Barato, tareas bien definidas |
| **GPT-4.1 mini** | ~0,80 USD (fine-tuning) | ~3,20 USD | Balance costo/calidad |
| **GPT-4o / gpt-4o** | ~2,50 USD | ~10 USD | Máxima calidad |
| **Batch API** | −50 % sobre lo anterior | −50 % | Procesamiento asíncrono (24 h) |

---

## Costo estimado para ~6.700 personas

Supuesto: **4 M tokens input** + **1 M tokens output**.

| Modelo | Input | Output | **Total aprox.** |
|--------|--------|--------|-------------------|
| **GPT-5 mini** | 4 × 0,25 = 1,00 USD | 1 × 2,00 = 2,00 USD | **~3 USD** |
| **GPT-5 mini (Batch −50 %)** | 0,50 USD | 1,00 USD | **~1,50 USD** |
| **GPT-4o** | 4 × 2,50 = 10 USD | 1 × 10 = 10 USD | **~20 USD** |
| **GPT-4o (Batch −50 %)** | 5 USD | 5 USD | **~10 USD** |

---

## Recomendación

- **Probar con una muestra** (p. ej. 50–100 personas) con **GPT-5 mini** para validar el prompt y la calidad.
- **Proceso completo** con **GPT-5 mini** en modo normal: **~3 USD**; con **Batch API**: **~1,50 USD**.
- Si necesitas mejor calidad en redes/conexiones, usar **GPT-4o** (o similar) en Batch: **~10 USD**.

---

## Cómo usar el script de enriquecimiento

1. Definir `OPENAI_API_KEY` en el entorno (o en `.env`).
2. Ejecutar con muestra para probar:
   ```bash
   python scripts/02_processing/enriquecer_con_gpt.py --muestra 50
   ```
3. Ejecutar sobre todo el consolidado (o por lotes):
   ```bash
   python scripts/02_processing/enriquecer_con_gpt.py
   ```
4. Opción Batch API (cuando esté implementado) para ahorrar ~50 %.

Los resultados se guardan en `outputs/df_consolidado_enriquecido_gpt.csv` con columnas adicionales: `cargo_1`, `cargo_2`, `partido_limpio`, `nacimiento_ano`, `fallecimiento_ano`, `hijos_lista`, `conexion_otros_paises`, `redes_dentro_pais`, `redes_entre_paises`.
