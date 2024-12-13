# Proyecto de Análisis de la ENIGH

Este proyecto utiliza las Encuestas Nacionales de Ingresos y Gastos de los Hogares (ENIGH) para realizar un análisis de datos socioeconómicos y demográficos. A continuación se describen las carpetas y archivos que componen el flujo de trabajo.

## Estructura de Carpetas

- **db/**: Contiene los archivos de la ENIGH utilizados en el análisis. Estos datos se procesan y limpian para crear una base de datos homogénea y manejable.
- **imagenes/**: Almacena las gráficas y resultados visuales generados a partir del análisis en R.

## Descripción de Archivos

1. **limpieza.R**: Este script procesa los archivos de la ENIGH ubicados en la carpeta `db/`. A partir de este procesamiento, se crea el archivo `base_q.csv`, el cual contiene los datos limpios y estructurados para su análisis posterior.

2. **dominanciaestocastica.R**: A partir de los datos en `base_q.csv`, este archivo realiza cálculos relacionados con la dominancia estocástica. Los resultados de estos cálculos pueden ser visualizados en las imágenes generadas en la carpeta `imagenes/`.

3. **estadisticas_descriptivas.R**: Este archivo calcula estadísticas descriptivas básicas a partir de `base_q.csv`. Los resultados y gráficas descriptivas se guardan en la carpeta `imagenes/`.

4. **pruebasdehipotesis.R**: Este archivo realiza pruebas de hipótesis para analizar las diferencias de medias y varianzas entre diferentes tipos de ingresos. Los resultados de estas pruebas ayudan a identificar disparidades significativas en la distribución de ingresos y están documentados mediante tablas y gráficas almacenadas en la carpeta `imagenes/`.

## Flujo de Trabajo

1. **Procesamiento de Datos**: Los archivos de la ENIGH en `db/` son limpiados y procesados en `limpieza.R` para crear la base `base_q.csv`.
2. **Análisis de Dominancia Estocástica**: En `dominanciaestocastica.R`, se realizan cálculos avanzados de dominancia estocástica usando la base de datos generada.
3. **Pruebas de Hipótesis**: En `pruebasdehipotesis.R`, se realizan pruebas estadísticas (diferencias de medias y varianzas) para identificar patrones relevantes en los ingresos.
4. **Estadísticas Descriptivas**: Se generan estadísticas descriptivas en `estadisticas_descriptivas.R`, produciendo tanto datos como visualizaciones almacenadas en `imagenes/`.

## Requisitos

- R y las librerías necesarias para procesar y analizar los datos.

## Notas

Los datos y resultados en `imagenes/` son generados directamente de los cálculos en los archivos `.R`, permitiendo reproducir el análisis y visualizar los resultados de manera efectiva.
