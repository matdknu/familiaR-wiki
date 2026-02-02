#!/usr/bin/env python3
"""
Script para agregar la familia Tovar y María Corina Machado a los datos de Venezuela.
También actualiza el archivo consolidado de LATAM.
"""

import pandas as pd
import os
from datetime import datetime

# Rutas
BASE_PATH = "/Users/matdknu/Dropbox/social-data-science/wiki-chile_project"
MANUAL_FILE = f"{BASE_PATH}/data/manual/familia_tovar_venezuela_manual.csv"
VENEZUELA_CONSOLIDATED = f"{BASE_PATH}/data/processed/familias/venezuela/consolidado.csv"
LATAM_CONSOLIDATED = f"{BASE_PATH}/data/processed/familias/_CONSOLIDADO_familias_latam.csv"

def main():
    print("=" * 60)
    print("Agregando familia Tovar y María Corina Machado a Venezuela")
    print("=" * 60)
    
    # Leer archivo manual
    print("\n📖 Leyendo archivo manual...")
    df_manual = pd.read_csv(MANUAL_FILE, sep=';', encoding='utf-8')
    print(f"   - Personas a agregar: {len(df_manual)}")
    for _, row in df_manual.iterrows():
        print(f"     • {row['nombre']}")
    
    # Leer consolidado de Venezuela
    print("\n📖 Leyendo consolidado de Venezuela...")
    df_venezuela = pd.read_csv(VENEZUELA_CONSOLIDATED, sep=';', encoding='utf-8')
    print(f"   - Personas actuales: {len(df_venezuela)}")
    
    # Verificar duplicados
    nombres_existentes = set(df_venezuela['nombre'].str.lower().str.strip())
    nombres_nuevos = set(df_manual['nombre'].str.lower().str.strip())
    duplicados = nombres_existentes.intersection(nombres_nuevos)
    
    if duplicados:
        print(f"\n⚠️  Personas ya existentes (no se agregarán):")
        for d in duplicados:
            print(f"     • {d}")
        # Filtrar solo los nuevos
        df_manual = df_manual[~df_manual['nombre'].str.lower().str.strip().isin(duplicados)]
        print(f"   - Personas nuevas a agregar: {len(df_manual)}")
    
    if len(df_manual) == 0:
        print("\n✅ No hay nuevas personas para agregar a Venezuela")
    else:
        # Agregar a Venezuela
        print("\n📝 Agregando nuevas personas a Venezuela...")
        df_venezuela_updated = pd.concat([df_venezuela, df_manual], ignore_index=True)
        df_venezuela_updated.to_csv(VENEZUELA_CONSOLIDATED, sep=';', index=False, encoding='utf-8')
        print(f"   - Total personas en Venezuela: {len(df_venezuela_updated)}")
    
    # Actualizar LATAM consolidado
    print("\n📖 Leyendo consolidado LATAM...")
    df_latam = pd.read_csv(LATAM_CONSOLIDATED, sep=';', encoding='utf-8')
    print(f"   - Personas actuales en LATAM: {len(df_latam)}")
    
    # Verificar duplicados en LATAM
    nombres_latam = set(df_latam['nombre'].str.lower().str.strip())
    nuevos_latam = df_manual[~df_manual['nombre'].str.lower().str.strip().isin(nombres_latam)]
    
    if len(nuevos_latam) > 0:
        print(f"\n📝 Agregando {len(nuevos_latam)} personas nuevas a LATAM...")
        df_latam_updated = pd.concat([df_latam, nuevos_latam], ignore_index=True)
        df_latam_updated.to_csv(LATAM_CONSOLIDATED, sep=';', index=False, encoding='utf-8')
        print(f"   - Total personas en LATAM: {len(df_latam_updated)}")
    else:
        print("\n✅ No hay nuevas personas para agregar a LATAM")
    
    # Resumen final
    print("\n" + "=" * 60)
    print("RESUMEN")
    print("=" * 60)
    
    # Re-leer para confirmar
    df_final_ve = pd.read_csv(VENEZUELA_CONSOLIDATED, sep=';', encoding='utf-8')
    df_final_latam = pd.read_csv(LATAM_CONSOLIDATED, sep=';', encoding='utf-8')
    
    print(f"\n📊 Venezuela consolidado: {len(df_final_ve)} personas")
    print(f"📊 LATAM consolidado: {len(df_final_latam)} personas")
    
    # Verificar que las personas se agregaron
    print("\n🔍 Verificando adiciones (familia Tovar):")
    nombres_buscar = ['María Corina Machado', 'Martín Tovar Ponte', 'Manuel Felipe de Tovar', 
                      'Martín Tovar y Tovar', 'Simón Planas', 'Francisco Rodríguez del Toro',
                      'Fernando Rodríguez del Toro', 'Simón Planas Suárez']
    
    for nombre in nombres_buscar:
        if nombre.lower() in df_final_ve['nombre'].str.lower().values:
            print(f"   ✅ {nombre}")
        else:
            print(f"   ❌ {nombre} (no encontrado)")
    
    print("\n✅ Proceso completado!")

if __name__ == "__main__":
    main()
