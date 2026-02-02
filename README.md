# Redes y reproducción de élites latinoamericanas

> *¿Te sorprendería saber que existe una relación clara entre el Mio Cid Campeador y Vicente Huidobro? ¿O entre Atahualpa y Piñera?*

Este proyecto analiza las **redes familiares y estrategias de reproducción de las élites latinoamericanas** utilizando Wikipedia como fuente de datos. Aplicamos técnicas de análisis de redes sociales y web scraping para mapear las relaciones familiares documentadas en artículos de Wikipedia.

Estamos desarrollando el paquete **familiaRes**, que reúne datos de **múltiples países latinoamericanos** sobre familias de élite y sus conexiones.

---

## 🌎 Visualización Principal

![Redes Familiares de Élites Latinoamericanas](outputs/figures/red_familias_latam.png)

**La red muestra:**
- **122 personas** de familias destacadas de Chile, Argentina y Colombia
- **228 conexiones familiares** (padres, cónyuges, hijos, hermanos)
- **Vínculos transnacionales** como el matrimonio de Cornelio Saavedra (prócer chileno) con María Saturnina de Otálora (Argentina, 1801)

### Familias incluidas

| País | Familias |
|------|----------|
| 🇨🇱 Chile | Aylwin, García-Huidobro, Bello, Balmaceda, Saavedra |
| 🇦🇷 Argentina | Otálora, Saavedra |
| 🇨🇴 Colombia | López, Lleras, Ospina |

---

## 📁 Estructura del Proyecto

```
wiki-chile_project/
├── data/                          # Datos del proyecto
│   ├── raw/                       # Datos crudos de scraping por país
│   │   ├── chile/
│   │   ├── argentina/
│   │   ├── colombia/
│   │   └── otros_paises/
│   ├── processed/                 # Datos procesados y consolidados
│   │   └── familias/
│   │       ├── chile/consolidado.csv
│   │       ├── argentina/consolidado.csv
│   │       ├── colombia/consolidado.csv
│   │       └── _CONSOLIDADO_familias_latam.csv
│   └── manual/                    # Datos ingresados manualmente
│
├── scripts/                       # Scripts de análisis
│   ├── 02_processing/            # Limpieza y normalización
│   │   ├── 01_parse_and_normalize.R
│   │   ├── 02_descriptive_analysis.R
│   │   └── 03_visualizations.R
│   └── 03_analysis/              # Análisis de redes
│       ├── network_analysis.R
│       ├── red_familias_multipais_v2.R
│       └── analisis_endogamia_politica_multipais.R
│
├── notebooks/                     # Notebooks exploratorios
│   ├── 01_exploracion/
│   └── 02_scraping_paises/       # Notebooks de scraping por país
│
├── outputs/                       # Resultados finales
│   ├── figures/                   # Gráficos y visualizaciones
│   └── tables/                    # Tablas procesadas
│
├── bibliography/                  # Referencias bibliográficas
└── README.md                      # Este archivo
```

---

## 🚀 Inicio Rápido

### Requisitos previos

**Python 3.8+** y **R 4.0+**

### Instalación

1. **Clonar el repositorio**
```bash
git clone https://github.com/matdknu/familiaR-wiki.git
cd wiki-chile_project
```

2. **Instalar dependencias Python**
```bash
pip install -r requirements.txt
```

3. **Instalar dependencias R**
```R
install.packages(c("readr", "tidyverse", "janitor", "ggraph", "tidygraph", "viridis", "ggrepel"))
```

---

## 📊 Análisis de Redes

### Generar visualización principal

```R
Rscript scripts/03_analysis/red_familias_multipais_v2.R
```

Esto genera la red multi-país con:
- Clusters separados por país
- Conexiones transnacionales destacadas
- Métricas de centralidad

### Análisis de endogamia

```R
Rscript scripts/03_analysis/analisis_endogamia_politica_multipais.R
```

---

## 🌎 Países Disponibles

| País | Familias | Personas | Estado |
|------|----------|----------|--------|
| 🇨🇱 Chile | 97 | 1,398 | ✅ Completo |
| 🇦🇷 Argentina | 165 | 1,190 | ✅ Completo |
| 🇨🇴 Colombia | 149 | 1,411 | ✅ Completo |
| 🇲🇽 México | 50+ | 500+ | 🔄 En progreso |
| 🇵🇪 Perú | 30+ | 300+ | 🔄 En progreso |

---

## 📝 Datos Procesados

Los datos consolidados incluyen para cada persona:
- **Identificación**: nombre, URL de Wikipedia
- **Biografía**: fecha/lugar de nacimiento y fallecimiento
- **Relaciones**: padres, cónyuge, hijos, hermanos (con URLs)
- **Carrera**: ocupación, cargos políticos, partido
- **Educación**: alma mater, títulos
- **Infobox JSON**: datos estructurados completos

---

## 📚 Metodología

### Fuentes de Datos
- Wikipedia (español): Infoboxes de biografías
- Categorías de familias por país

### Tipos de Relaciones
- Padre/Madre
- Cónyuge/Pareja
- Hijo/Hija
- Hermano/Hermana

### Análisis de Redes
- Layout Fruchterman-Reingold por país
- Centralidad de grado y betweenness
- Detección de conexiones transnacionales

---

## 📖 Referencias

- Padgett, J. F., & Ansell, C. K. (1993). Robust Action and the Rise of the Medici, 1400-1434. *American Journal of Sociology*, 98(6), 1259-1319.

---

## 🤝 Contribuciones

Para contribuir:
1. Crear una rama: `git checkout -b feature/nueva-funcionalidad`
2. Hacer commit: `git commit -m "Descripción"`
3. Push: `git push origin feature/nueva-funcionalidad`
4. Crear Pull Request

---

## 📧 Contacto

Para preguntas o sugerencias, abrir un issue en el repositorio.

---

## 📄 Licencia

MIT License - Ver archivo LICENSE para más detalles.
