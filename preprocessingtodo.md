# Lista de Tareas para Completar el Proyecto de Modelización Estadística

## Variable Objetivo (Target)
La variable objetivo principal para el proyecto será **INDFMPIR** (ratio de ingresos familiares a la línea de pobreza).  
**Por qué esta variable**:  
- Es numérica continua (rango 0-5+, con valores >5 topcodeados), ideal para GLM (regresión lineal/generalizada) como respuesta principal.  
- Mide desigualdad económica y pobreza, un tema relevante en demografía y salud pública (ej. correlacionado con acceso a servicios médicos).  
- Alta variabilidad (media ~2-3), lo que permite modelos robustos y análisis significativos.  
- Faltantes moderados (~10-15%), pero imputables (ej. con mediana por grupos demográficos).  
- Facilita interpretaciones sociales: predecir pobreza basada en edad, educación, raza o género, alineándose con los objetivos del proyecto (análisis descriptivo, GLM, PCA, clustering).  
- Diferencia de otras opciones como edad (RIDAGEYR), ya que enfoca en aspectos económicos en lugar de biológicos.

## Por qué el Dataset Actual No Vale para Series Temporales
El dataset `demographic.csv` (NHANES 2013-2014) no es adecuado para análisis de series temporales porque:
- La variable `SDDSRVYR` es constante (valor 8 para todos los registros), representando solo el ciclo 2013-2014, sin variación temporal dentro del dataset.
- No hay variables de fechas (como fechas de nacimiento, eventos o mediciones repetidas) que permitan analizar tendencias a lo largo del tiempo.
- Para cumplir con el requisito del proyecto, necesitas un dataset adicional relacionado (ej. datos de NHANES de múltiples años sobre salud/nutrición) para construir una serie temporal significativa (ej. evolución de obesidad por edad/raza a lo largo de años).

## Tareas de Preprocesamiento y Proyecto (TODO List)

### 1. Exploración Inicial de Datos
- [ ] Leer el archivo `demographic.csv` en R (usar `read.csv` o `readr::read_csv`).
- [ ] Verificar estructura básica: número de filas/columnas, tipos de datos (`str()`, `summary()`).
- [ ] Identificar variables clave según requisitos:
  - Numéricas: `RIDAGEYR`, `DMDHHSIZ`, `DMDFMSIZ`, `INDFMPIR`, etc.
  - Binarias: `RIAGENDR`, `DMQMILIZ`, `DMDCITZN`.
  - Categóricas: `RIDRETH1`, `DMDEDUC3`, `DMDMARTL`, etc.
  - Fechas: Ninguna directa; planear dataset adicional para series temporales.
- [ ] Calcular estadísticas básicas: medias, medianas, rangos, % de faltantes por variable y total.

### 2. Preprocesamiento de Datos
- [ ] **Manejo de Datos Faltantes**:
  - Identificar patrones de faltantes (usar `mice` o `VIM` para visualización).
  - Decidir estrategia: imputar (media/mediana para numéricas, moda para categóricas) o eliminar filas si <5% faltantes.
  - Documentar decisiones y justificar (ej. imputar edad con mediana por género).
- [ ] **Detección y Manejo de Outliers**:
  - Usar boxplots, z-scores o IQR para variables numéricas (ej. `RIDAGEYR`, `INDFMPIR`).
  - Decidir: remover, imputar o transformar (ej. winsorizar).
  - Justificar decisiones.
- [ ] **Detección de Errores**:
  - Verificar rangos plausibles (ej. edad entre 0-80, ingresos positivos).
  - Corregir inconsistencias (ej. género binario debe ser 1 o 2).
- [ ] **Agrupación de Variables Categóricas**:
  - Reducir categorías si >5 (ej. agrupar niveles educativos en "Bajo", "Medio", "Alto").
  - Usar `forcats` para recodificar.
- [ ] **Transformaciones de Datos**:
  - Normalizar/escalar variables numéricas si necesario (ej. logaritmo para ingresos sesgados).
  - Crear variables derivadas (ej. categorías de edad: niño, adulto, anciano).
- [ ] Guardar dataset preprocesado como `demographic_preprocessed.csv`.

### 3. Análisis Descriptivo Inicial
- [ ] Estadísticas univariadas: histogramas, boxplots, tablas de frecuencias.
- [ ] Estadísticas bivariadas: correlaciones, scatterplots, tablas cruzadas.
- [ ] Comparar datos crudos vs. preprocesados (distribuciones).
- [ ] Documentar hallazgos en un párrafo (ej. distribución de edad por género).

### 4. Modelos GLM (Generalized Linear Models)
- [ ] Identificar respuesta numérica (ej. `RIDAGEYR` o `INDFMPIR`) y ajustar GLM (usar `glm`).
- [ ] Identificar respuesta binaria (ej. `DMQMILIZ`) y ajustar modelo logístico.
- [ ] Validar modelos: residuos, AUC, etc.
- [ ] Justificar calidad del modelo.

### 5. Análisis de Series Temporales
- [ ] Obtener dataset adicional (ej. NHANES años 2000-2020 sobre BMI o diabetes).
- [ ] Construir serie temporal (ej. promedio de BMI por año).
- [ ] Ajustar modelo TS (usar `forecast` o `tsibble`).
- [ ] Validar y justificar.

### 6. Análisis PCA (Principal Component Analysis)
- [ ] Seleccionar variables numéricas.
- [ ] Realizar PCA (usar `prcomp`).
- [ ] Interpretar screeplot, componentes principales y mapas factoriales.

### 7. Clustering
- [ ] **Clustering Jerárquico**: Seleccionar variables, calcular distancias, dendrograma, elegir número de clusters.
- [ ] **Clustering Particional (K-means)**: Aplicar K-means, visualizar, elegir K óptimo.
- [ ] Describir tamaños de clusters.

### 8. Profiling de Clusters
- [ ] Usar variable clase (ej. `RIDRETH1`) para analizar distribuciones condicionales.
- [ ] Identificar diferencias entre clusters (pruebas estadísticas, gráficos).
- [ ] Crear perfiles descriptivos y sintetizar en "templates" por cluster.

### 9. Documentación y Entregables
- [ ] Crear documento D2 (data document): 2 páginas con descripción, estructura, faltantes.
- [ ] Crear D3 (work plan): Gantt, asignación de tareas, plan de riesgos.
- [ ] Preparar D4 (pre-project): Reporte PDF, presentación, zip con datos crudos/preprocesados y scripts R.
- [ ] Preparar D5 (final project): Todo lo anterior + PCA, clustering, profiling, conclusiones.
- [ ] Asegurar equipo de 4-5 personas, comunicar progreso.

### Notas Generales
- Usar R para todo (instalar paquetes: `dplyr`, `ggplot2`, `caret`, etc.).
- Validar cada paso: ejecutar código y verificar resultados.
- Si encuentras problemas, iterar (ej. ajustar imputación si modelos fallan).
- Fecha límite: D4 (29/10/2025), D5 (16/12/2025).</content>
<parameter name="filePath">/Users/nicolasrosales/Documents/GitHub/me_projecte/preprocessingtodo.md