# Imputación de país y señales útiles

## Objetivo

Construir una variable `pais_imputado` que no dependa solo del `pais` original del scraping, sino de evidencia biográfica, relacional e institucional que permita:

- corregir asignaciones dudosas
- justificar la adscripción nacional
- detectar actores que unen países
- dejar trazabilidad sobre las razones detrás de cada imputación

## Variables más útiles para imputar país

### Señales fuertes

- `nacionalidad`
- `lugar_nacimiento`
- `lugar_fallecimiento`
- `residencia`
- `cargo_principal`
- `cargo_1`, `cargo_2`, `cargo_3`
- `bloque_cargo_llm`
- `es_cargo_diplomatico`
- `tipo_diplomatico`
- `partido_politico`
- `partido_limpio`

Estas variables sirven cuando el país está explícito o muy sugerido por el cargo: por ejemplo, presidente, senador, canciller o embajador de un país concreto.

### Señales familiares y relacionales

- `familia`
- `categoria_origen`
- `padres`
- `conyuge`
- `pareja`
- `hijos`
- `hermanos`
- `perfiles_relacionados`
- `perfiles_relacionados_padres`
- `perfiles_relacionados_conyuge`
- `perfiles_relacionados_pareja`
- `perfiles_relacionados_hijos`
- `perfiles_relacionados_hermanos`
- `perfiles_relacionados_familia`

Estas variables permiten imputar país por linaje, circulación familiar y matrimonios transnacionales.

### Señales textuales y de contexto

- `biografia_inicial`
- `biografia`
- `ocupacion`
- `educacion`
- `alma_mater`
- `periodo`
- `predecesor`
- `sucesor`
- `distinciones`
- `premios`
- `infobox_json`
- `infobox_completa`

Estas variables ayudan a justificar la imputación cuando el país no está limpio en un solo campo, pero aparece repetidamente en trayectorias, cargos, lugares o instituciones.

## Variables proyectadas para dejar trazabilidad

- `pais_nacimiento`
- `pais_muerte`
- `pais_residencia`
- `pais_nacionalidad`
- `pais_cargo`
- `pais_pareja`
- `pais_padres`
- `pais_hijos`
- `pais_hermanos`
- `pais_infobox`
- `pais_biografia`
- `pais_imputado`
- `confianza_imputacion_pais`
- `razon_imputacion_pais`
- `evidencia_imputacion_pais`
- `fuente_imputacion_pais`

## Reglas sugeridas

### Regla 1: priorizar lo explícito

Si `nacionalidad`, `lugar_nacimiento`, `cargo_principal` o `partido_limpio` apuntan claramente a un mismo país, ese país debe dominar la imputación.

### Regla 2: separar anclaje y circulación

Una persona puede estar anclada a un país, pero unir varios. Por eso conviene guardar:

- `pais_imputado`: país principal
- `score_union_paises`: capacidad de conectar países
- `tipo_conexion_dominante`: si une países por pareja, cargo, diplomacia, linaje o residencia

### Regla 3: no confundir muerte con pertenencia

`lugar_fallecimiento` sirve como señal, pero normalmente debería pesar menos que:

- nacionalidad
- nacimiento
- cargo político
- inserción familiar

Puede ser clave en exilios, migraciones y trayectorias transnacionales, pero no debería gobernar por sí solo.

### Regla 4: pareja y conyuge son claves para puentes entre países

Cuando `pareja` o `conyuge` conectan con otro país, conviene no sobreescribir el país principal, sino marcar:

- `vinculo_transnacional_pareja_existe`
- `vinculo_transnacional_pareja_pais_relacionado`
- `tipo_conexion_dominante = pareja`

### Regla 5: el cargo diplomático merece tratamiento especial

Un actor diplomático puede tener:

- país de origen
- país de representación
- país de residencia temporal

Por eso conviene separar:

- `pais_imputado`
- `es_cargo_diplomatico`
- `tipo_diplomatico`
- `pais_cargo`

## Cómo evidenciar biografías

Para que la imputación sea auditable, no basta con guardar el país final. Conviene conservar:

- la razón principal
- una o más evidencias textuales breves
- los campos fuente usados
- una confianza final

Eso hace posible revisar casos complejos sin volver a procesar toda la biografía.

## Cómo unir países analíticamente

Las variables más útiles para mostrar unión entre países son:

- `conexion_otros_paises`
- `redes_entre_paises`
- `vinculo_transnacional_pareja_pais_relacionado`
- `es_cargo_diplomatico`
- `tipo_diplomatico`
- `pais_pareja`
- `pais_cargo`
- `pais_nacimiento` versus `pais_imputado`
- `pais_muerte` versus `pais_imputado`
- `n_conexiones_inter_paises`
- `score_union_paises`

## Resultado ideal

La salida futura debería ser una tabla persona-nivel con al menos:

- un país principal imputado
- una confianza
- una razón principal
- evidencia breve
- señales secundarias
- indicadores de si la persona conecta elites dentro de un país o entre países

Eso permitiría pasar de una comparación descriptiva entre países a una sociología relacional de circulación de élites.
