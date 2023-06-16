# PFG_definitivo
Este repositorio contiene todos los archivos empleados por el estudiante de 5º de ADE + Ingeniería en Tecnologáis Industriales Iñigo 
García de Eulate Pascual para el desarrollo del Proyecto de Fin de Grado "Desarrollo de modelos de caracterización a corto plazo de la 
evolución térmico-energética de edificios", realizado en colaboración con el tutor Roberto Garay Martínez.

El trabajo ha sido desarrollado en el entorno de programación R Studio.

El PFG realizado desarrolla modelos para la predicción a corto plazo de la temperatura interior de una de las 5 habitaciones del edificio 4 de la School of Design & Environment de la National University of Singapore (NUS) sobre las que se aporta información en un conjunto de datos público.

## Temas a destacar

### Scripts relevantes

Entre los scripts más relevantes y que pueden ser de uso general para cualquier proyecto que requiera realizar las funciones para las que han sido concebidos, destacan los siguientes:
  - #### Script 4
Permite el cálculo de la hora solar de cualquier ubicación del planeta a partir de la introducción de la longitud de la ubicación y de la longitud del meridiano estándar en el que se referencia la hora estándar de la ubicación.
  - #### Script 6
Permite el cálculo de las coordenadas solares en cualquier momento del año para cualquier ubicación del planeta introduciendo únicamente la latitud de dicha ubicación (es necesaria la información del script 4).
  - ### Script 7
Permite la transformación de una señal de radiación solar global horizontal a una señal de radiación solar global sobre fachada introduciendo el acimut del plano de la fachada (es necesaria la información de los scripts 4 y 6).
  - ### Script 8
Permite la preparación de un conjunto de datos para la realización de regresiones lineales ajustando el tiempo de muestreo y el número de pasos anteriores de las variables exógenas y de la variable predicha que se vayan a emplear.

### Gráficas relevantes

A lo largo de los scripts del proyecto se encuentran diferentes gráficas realizadas todas ellas con la función básica de representaciones gráficas de R “plot”. 

Algunas características interesantes de las representaciones gráficas que se emplean:
- Representación gráfica de más de una señal sobre una misma variable dependiente
- Representación gráfica de dos variables y representación de una tercera variable a través del color de los puntos y presentación de una leyenda asociada.
- Modificación de los parámetors de la función "plot" para que las gráficas resulten claramente legibles e interpretables.


### Conclusiones del trabajo en términos de ajuste del modelo

Finalmente, y tras confirmar la posibilidad de modelado, se han conseguido desarrollar modelos que predicen la temperatura interior, con tiempos de muestreo y predicción de una hora y cumpliendo con los criterios de validación marcados, a través de ocho mecanismos diferentes. El modelo más garantista predice la temperatura interior a con un R2 superior a 0,99, MAE cercano a 0,137°C y con un 95% de confianza, el valor de predicción se sitúa, como mucho, 0,404°C grados por encima o 0,402°C por debajo de la medición. El número de variables usadas en este modelo es de 14.

## Estructura del repositorio

En el propio repositorio se encuentra:
  - Archivo del proyecto: PFG_definitivo.Rproj
  - Scripts: 
    - script1_preprocesado.R. En este script se abre el archivo de datos, se cambian los nombres de las cabeceras y se cambia el tipo de datos de la marca de tiempo.
    - script2_graficación.R. En este script se realizan diferentes representaciones gráficas a partir de los datos.
    - script3_mejora_senal_ocupacion_conteo.R. En este script se calculan las medias móviles para la mejora de la robustez de la señal de ocupación seleccionando la media móvil centrada de 3 elementos.
    - script4_hora_solar.R. En este script se calcula la hora solar a partir de la hora estándar.
    - script5_correlaciones.R. En este script se calculan las correlaciones entre diferentes variables
    - script6_posicion_solar.R. En este script se calcula la geometría solar que posteriormente sirve para la corrección de la radiación solar
    - script7_correccion_radiación_solar.R. En este script se corrige la radiación solar a partir del
    - script8_preparación_datasets.R. En este script se preparan los data sets para el modelado posterior, cambiando el tiempo de muestreo a 60 minutos y obteniendo los valores pasados de cada variable que permiten conferir el carácter autorregresivo al modelo.
    - script9_ARX_iniciales.R. En este script se modelizan varios ARX iniciales que confirman la posibilidad de modelado y dan cierta información.
    - script10_modelos_ARX.R. En este script se desarrollan los 8 modelos ARX diseñados.
    - script11_graficas_submodelos.R. En este script se realizan las representaciones gráficas referentes a cada submodelo elegido para cada modelo

En el repositorio se encuentran diferentes carpetas:
  - correlations. En esta carpeta se contienen todos los archivos referentes al cálculo de correlaciones.
  - data. En esta carpeta están contenidos los conjuntos de datos originales propuestos por los autores.
  - datasets. En esta carpeta se almacenan los archivos CSV que se van generando a lo largo de los scripts.
  - output. En esta carpeta sí va almacenando la información proveniente de los 8 modelos ARX.
  - plots. Esta carpeta está subdividida en diferentes carpetas que almacenan las representaciones gráficas que se van realizando a lo largo del proyecto.
  - rds_files. En esta carpeta se guardan los archivos de tipo RDS que se van generando a lo largo del proyecto. Se trata de un tipo de archivo nativo a R y que guarda la información tal y como está, eliminando la necesidad de realizar conversiones cuando se vuelve a abrir.
