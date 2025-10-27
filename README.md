# 🧍‍♂️ Análisis demográfico de los suicidios en España (2013–2020)

Proyecto grupal centrado en el estudio demográfico de los suicidios en España entre **2013 y 2020**, con el objetivo de analizar la relación entre **edad**, **sexo** y **método empleado**.  
Este repositorio recoge **mi contribución individual**, que incluye la **limpieza de datos**, el **primer gráfico (pirámide poblacional comparativa)** y el **tercer gráfico (método de suicidio por grupo de edad)**, junto con sus interpretaciones.

---

## 🧠 Objetivo
Explorar si existen relaciones significativas entre la **edad del individuo** y el **método utilizado** para suicidarse, así como visualizar la distribución demográfica de los suicidios en comparación con la población general española.

---

## 🔧 Limpieza y preparación de los datos (mi parte)
- Integración de las bases de datos del **INE (2013–2020)** sobre suicidios por **método**, **sexo** y **edad**.  
- Estandarización de variables (`Sexo`, `Medio.empleado`, `Edad`) y creación de una columna de **año** para identificar el periodo de cada observación.  
- Agrupación de los métodos individuales en **8 categorías** mediante `mutate()` y `case_when()` en R:  
  `Envenenamiento`, `Asfixia`, `Disparo`, `Exposición al calor`, `Corte`, `Precipitación`, `Vehículos`, `Otros`.  
- Clasificación de las edades en tres grandes grupos:  
  - **Jóvenes (0–29)**  
  - **Adultos (30–64)**  
  - **Ancianos (+65)**  

---

## 📊 Gráfico 1 — Pirámide poblacional comparativa
- Representación de los suicidios en 2020 por **sexo y grupo de edad**.  
- Comparación visual con la **pirámide poblacional española** del mismo año.  
- Normalización de valores para obtener comparaciones proporcionales entre hombres y mujeres.  
- Combinación de ambas pirámides con `grid.arrange()` para una visualización conjunta.

**Conclusión:**  
Ambas distribuciones presentan una forma regresiva típica de países desarrollados, concentrando la mayor parte de los casos en edades adultas (40–59 años) y una incidencia muy baja entre menores de 15 años.

---

## 📈 Gráfico 3 — Método de suicidio por grupo de edad
- Cálculo de **probabilidades condicionadas**:  
  - `P(Edad | Método)` → proporción de edad dentro de cada método.  
  - `P(Método | Edad)` → probabilidad de usar un método dentro de cada grupo de edad.  
- Visualización mediante:
  - **Gráfico de barras facetado** (edad según método).  
  - **Gráficos de sectores** (método según edad) para jóvenes, adultos y ancianos.  
- Unión final con `facet_grid()` y `layout_matrix` para una presentación equilibrada.

**Conclusión:**  
- La **asfixia** es el método predominante en todos los grupos (~50%).  
- Los **jóvenes** muestran una proporción notablemente mayor en el método *vehículos*.  
- Los **adultos y ancianos** comparten patrones similares, predominando *asfixia* y *precipitación*.

---

## 🧰 Herramientas utilizadas
R · ggplot2 · dplyr · scales · gridExtra · Excel

---

## 📚 Fuentes de datos
- Instituto Nacional de Estadística (INE): suicidios por método, sexo y edad (2013–2020).  
- INE: estructura poblacional española por edad y sexo (2020).  
- Ministerio de Sanidad: informes anuales sobre causas de muerte.

---

## 📈 Principales conclusiones
- La edad influye significativamente en la frecuencia y el método empleado.  
- Los grupos **adultos y mayores** concentran la mayoría de casos.  
- El incremento observado en **2020** afectó especialmente a los grupos **jóvenes y ancianos**, coincidiendo con el periodo de confinamiento por la COVID-19.  

---

## 🔗 Enlaces
- [📘 Ver resumen y contexto del proyecto en Notion](https://www.notion.so/Portfolio-de-Alejandro-6e1093c6146a4f648263a4f243777c60?source=copy_link) 

---

📌 *Proyecto grupal (4 integrantes).  
Responsable de la limpieza de datos, el análisis exploratorio y la creación de los gráficos 1 y 3 con sus conclusiones.*

