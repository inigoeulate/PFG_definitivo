#===============================================================================
#===============================================================================
# Décimo script del PFG "Desarrollo de modelos de caracterización a corto plazo
# de la evolución térmico-energética en edificios" elaborado por el estudiante
# Iñigo García de Eulate Pascual (5ºADE+ITI, Universidad de Deusto) en
# colaboración con el tutor del PFG Roberto Garay Martínez.
#===============================================================================
#===============================================================================

#===============================================================================
#===============================================================================
# En este décimo script se llevarán a cabo los 8 modelos ARX diseñados.
#===============================================================================
# Tras el análisis de los 3 ARX iniciales se llega a la conclusión de que se van
# a emplear 8 modelos (2x2x2)
  # - Intersección en 0 o libre
  # - Extender el modelo desde el instante inicial hasta el pasado o modelo
  #   completo e ir reduciendo
  # - Extender/reducir por pasos de tiempo o por identificación de variable
  #   más/menos relevante (que haya los mismos pasos de tiempo de todas las 
  #   variables o que haya diferentes pasos de tiempo para cada variable)
#===============================================================================
#===============================================================================

#===============================================================================
# Carga de librerías
#-------------------------------------------------------------------------------

# Librería "Metrics" que permite calcular el MAE automáticamente.

library(Metrics)

#===============================================================================

#===============================================================================
# Obtención del dataframe
# Nos centramos en el modelo con sampleo de 60 minutos
#-------------------------------------------------------------------------------

{
  wd<-getwd()
  
  sampleo<-60
  
  horas_anteriores<-6
  
  obs_anteriores<-horas_anteriores*60/sampleo
  
  rm(horas_anteriores)
  
  dataframe_trabajo<-readRDS(paste(wd, 
                                   "/rds_files/dataframe_", 
                                   sampleo, 
                                   ".rds", 
                                   sep=""))
}

#===============================================================================

#===============================================================================
# Variables comunes a todos los ARX
#-------------------------------------------------------------------------------

{
  variables_regresion_input<-c("ocupantes_conteo_robus3",
                               "temperatura_exterior",
                               "energia_agua_refrigerada",
                               "radiacion_global_fachada")
  
  regresion_output<-"temperatura_interior"
  
  grados_margen<-0.25
  
  # Creación de la ecuación de regresión del modelo introduciendo todas las
  # variables y todas las observaciones anteriores de las variables para obtener
  # el número y nombre de las filas
  
  formula<-paste(regresion_output, " ~ 0 +", sep="")
  
  for (i in 1:length(variables_regresion_input)) {
    formula<-paste(formula, " ", variables_regresion_input[i], sep="") 
    if (i<length(variables_regresion_input))
      formula<-paste(formula, " +", sep="") 
  }
  
  for (i in 1:obs_anteriores) {
    nombre_var<-paste(regresion_output, "_", i, sep="")
    formula<-paste(formula, " + ", nombre_var, sep="") 
    
    nombre_var<-paste(variables_regresion_input, "_", i, sep="")
    
    for (j in 1:length(nombre_var)) {
      formula<-paste(formula, " + ",nombre_var[j], sep="")
    }
  }
  
  attach (dataframe_trabajo)
  arx<-lm(formula)
  detach(dataframe_trabajo)
  a<-summary(arx)
  
  numero_filas<-length(names(arx$coefficients))+2
  
  nombres_filas<-c("(Intercept)",names(arx$coefficients), "R2", "MAE",
                   "Media residuales", "Desviación típica residuales",
                   "Número de variables")
  
  prob<-0.05
  
  z<-qnorm(prob/2,0,1,lower.tail=FALSE)
}

#-------------------------------------------------------------------------------

# Limpieza de variables

rm(a, arx, dataframe_60, formula, i, j, nombre_dataframe, nombre_var)

#===============================================================================

#===============================================================================
# Modelo 1:
  # Intersección en 0
  # Extensión del modelo desde el instante inicial hasta el pasado
  # Extensión por pasos de tiempo
#-------------------------------------------------------------------------------

# Creación del dataframe de resultados

{
  dataframe_resultados<-data.frame(matrix(nrow=numero_filas+5))
  rownames(dataframe_resultados)<-c(nombres_filas, 
                                    "p-valor del parámetro menos significativo")
  colnames(dataframe_resultados)<-"Iteración 1"
}

#-------------------------------------------------------------------------------

# ARX con los diferentes pasos de tiempo

{
  for (j in 0:obs_anteriores){
    formula<-paste(regresion_output, " ~ 0 +", sep="")
    
    for (i in 1:length(variables_regresion_input)){
      formula<-paste(formula, " ", variables_regresion_input[i], sep="") 
      if (i<length(variables_regresion_input)){
        formula<-paste(formula, " +", sep="")
      }
    }
    
    if (j>0){
      for (i in 1:j) {
        nombre_var<-paste(regresion_output, "_", i, sep="")
        formula<-paste(formula, " + ", nombre_var, sep="") 
        
        nombre_var<-paste(variables_regresion_input, "_", i, sep="")
        
        for (k in 1:length(nombre_var)) {
          formula<-paste(formula, " + ",nombre_var[k], sep="")
        }
      }
    }
    
    attach(dataframe_trabajo)
    arx<-lm(formula)
    detach(dataframe_trabajo)
    a<-summary(arx)
    
    x<-j+1
    
    temperatura_interior_pred<-predict(arx)
    temperatura_interior_med<-
      dataframe_trabajo$temperatura_interior[x:nrow(dataframe_trabajo)]
    
    significancia_min<-0
    
    for (i in 1:length(arx$coefficients)){
      if (a[["coefficients"]][i,4] > significancia_min){
        significancia_min<-a[["coefficients"]][i,4]
      }
    }
    
    media<-mean(arx$residuals)
    desv_tipica<-sd(arx$residuals)
    
    if (j==0){
      for (i in 1:nrow(dataframe_resultados)){
        for (k in 1:length(names(arx$coefficients))){
          if (rownames(dataframe_resultados[i,0])==names(arx$coefficients)[k]){
            dataframe_resultados[i,1]<-arx$coefficients[k]
          }
        }
        if (rownames(dataframe_resultados)[i] == "Número de variables"){
          dataframe_resultados[i,1]<-length(arx$coefficients)
        }
        if (rownames(dataframe_resultados)[i] == "(Intercept)"){
          dataframe_resultados[i,1]<-0
        }
        if (rownames(dataframe_resultados)[i] == "R2"){
          dataframe_resultados[i,1]<-summary(arx)$r.squared
        }
        if (rownames(dataframe_resultados)[i] == "MAE"){
          dataframe_resultados[i,1]<-mae(temperatura_interior_med,
                                                       temperatura_interior_pred)
        }
        if (rownames(dataframe_resultados)[i] == "p-valor del parámetro menos significativo"){
          dataframe_resultados[i,1]<-significancia_min
        }
        if (rownames(dataframe_resultados)[i] == "Media residuales"){
          dataframe_resultados[i,1]<-media
        }
        if (rownames(dataframe_resultados)[i] == "Desviación típica residuales"){
          dataframe_resultados[i,1]<-desv_tipica
        }
      }
    } else {
      nombre_columna<-paste("Iteración ", j+1, sep="")
      dataframe_resultados$mas<-rep(NA,nrow(dataframe_resultados))
      colnames(dataframe_resultados)[j+1]<-nombre_columna
      for (i in 1:nrow(dataframe_resultados)){
        for (k in 1:length(names(arx$coefficients))){
          if (rownames(dataframe_resultados[i,0])==names(arx$coefficients)[k]){
            dataframe_resultados[i,j+1]<-arx$coefficients[k]
          }
        }
        if (rownames(dataframe_resultados)[i] == "Número de variables"){
          dataframe_resultados[i,j+1]<-length(arx$coefficients)
        }
        if (rownames(dataframe_resultados)[i] == "(Intercept)"){
          dataframe_resultados[i,j+1]<-0
        }
        if (rownames(dataframe_resultados)[i] == "R2"){
          dataframe_resultados[i,j+1]<-summary(arx)$r.squared
        }
        if (rownames(dataframe_resultados)[i] == "MAE"){
          dataframe_resultados[i,j+1]<-mae(temperatura_interior_med,
                                           temperatura_interior_pred)
        }
        if (rownames(dataframe_resultados)[i] == "p-valor del parámetro menos significativo"){
          dataframe_resultados[i,j+1]<-significancia_min
        }
        if (rownames(dataframe_resultados)[i] == "Media residuales"){
          dataframe_resultados[i,j+1]<-media
        }
        if (rownames(dataframe_resultados)[i] == "Desviación típica residuales"){
          dataframe_resultados[i,j+1]<-desv_tipica
        }
      }
    }
  } 
}

#-------------------------------------------------------------------------------

# Exportar el dataframe de resultados a archivo csv

write.csv2(dataframe_resultados, 
           paste(wd, "/output/dataframe_resultados_1.csv", sep=""))

#-------------------------------------------------------------------------------

# Limpieza de variables

rm(a, arx, dataframe_resultados, formula, i, j, k, nombre_columna, nombre_var,
   temperatura_interior_med, temperatura_interior_pred, x, significancia_min,
   media, desv_tipica)

#===============================================================================

#===============================================================================
# Modelo 2:
  # Intersección en 0
  # Extensión del modelo desde el instante inicial hasta el pasado
  # Extensión por identificación de variable más relevante
#-------------------------------------------------------------------------------

# Creación de la ecuación de regresión del modelo introduciendo todas las
# variables y la observación actual

{
  formula<-paste(regresion_output, " ~ 0 +", sep="")
  
  for (i in 1:length(variables_regresion_input)) {
    formula<-paste(formula, " ", variables_regresion_input[i], sep="") 
    if (i<length(variables_regresion_input))
      formula<-paste(formula, " +", sep="") 
  }
}

#-------------------------------------------------------------------------------

# ARX con el instante actual

{
  attach(dataframe_trabajo)
  arx<-lm(formula)
  detach(dataframe_trabajo)
  a<-summary(arx)
}

#-------------------------------------------------------------------------------

# Creación del dataframe de resultados

{
  dataframe_resultados<-data.frame(matrix(nrow=numero_filas+4))
  rownames(dataframe_resultados)<-nombres_filas
  colnames(dataframe_resultados)<-"Iteración 1" 
}

#-------------------------------------------------------------------------------

# Introducción del primer ARX en el dataframe de resultados

{
  
  temperatura_interior_pred<-predict(arx)
  temperatura_interior_med<-dataframe_trabajo$temperatura_interior
  
  media<-mean(arx$residuals)
  desv_tipica<-sd(arx$residuals)
  
  for (i in 1:nrow(dataframe_resultados)){
    for (j in 1:length(arx$coefficients)){
      if (rownames(dataframe_resultados)[i] == names(arx$coefficients)[j]){
        dataframe_resultados[i,1]<-arx$coefficients[j]
      }
    }
    if (rownames(dataframe_resultados)[i] == "Número de variables"){
      dataframe_resultados[i,1]<-length(arx$coefficients)
    }
    if (rownames(dataframe_resultados)[i] == "(Intercept)"){
      dataframe_resultados[i,1]<-0
    }
    if (rownames(dataframe_resultados)[i] == "R2"){
      dataframe_resultados[i,1]<-summary(arx)$r.squared
    }
    if (rownames(dataframe_resultados)[i] == "MAE"){
      dataframe_resultados[i,1]<-mae(temperatura_interior_med,
                                                   temperatura_interior_pred)
    }
    if (rownames(dataframe_resultados)[i] == "Media residuales"){
      dataframe_resultados[i,1]<-media
    }
    if (rownames(dataframe_resultados)[i] == "Desviación típica residuales"){
      dataframe_resultados[i,1]<-desv_tipica
    }
  }
}

#-------------------------------------------------------------------------------

# ARX exteniendo por identificación de variable más relevante

  # Quitar las variables no significativas del primer ARX

{
  dejadas<-c()
  
  for (i in 1:length(arx$coefficients)){
    if (a[["coefficients"]][i,4] > 0.05){
      quitar<-names(arx$coefficients[i])
      formula<-gsub(quitar, "", formula)
    }else{
      dejadas<-c(dejadas, names(arx$coefficients)[i])
    }
  }
}

  # Bucle que introduce cada vez un nuevo paso de tiempo y quita las variables
  # no significativas. Cuando una variable no es signficativa par aun paso de
  # tiempo, se considera no significativa para los siguientes y no se introduce
  # en la regresión

for (i in 1:obs_anteriores){
  
  if (i == 1){
    nombre_var<-paste(regresion_output, "_", i, sep="")
    for (j in 1:length(nombre_var)) {
      formula<-paste(formula, " + ",nombre_var[j], sep="")
    }
  }else{
    nombre_var<-paste(regresion_output, "_", i, sep="")
    final<-nchar(nombre_var)
    for (j in 1:length(dejadas)){
      if (dejadas[j]==substring(nombre_var, 1, final-2)){
        formula<-paste(formula, " + ",nombre_var[j], sep="")
      }else{
        quitadas<-substring(nombre_var, 1, final-2)
      }
    }
  }
  
  nombre_var<-paste(variables_regresion_input, "_", i, sep="")
  
  for (j in 1:length(nombre_var)){
    final<-nchar(nombre_var[j])
    for (k in 1:length(dejadas)){
      if (dejadas[k] == substring(nombre_var[j], 1, final-2)){
        formula<-paste(formula, " + ", nombre_var[j], sep="")
      }
    }
  }
  
  attach(dataframe_trabajo)
  arx<-lm(formula)
  detach(dataframe_trabajo)
  a<-summary(arx)
  
  x<-i+1
  
  temperatura_interior_pred<-predict(arx)
  temperatura_interior_med<-
    dataframe_trabajo$temperatura_interior[x:nrow(dataframe_trabajo)]
  
  media<-mean(arx$residuals)
  desv_tipica<-sd(arx$residuals)
  
  nombre_columna<-paste("Iteración ", i+1, sep="")
  dataframe_resultados$mas<-rep(NA,nrow(dataframe_resultados))
  colnames(dataframe_resultados)[i+1]<-nombre_columna
  
  for (j in 1:nrow(dataframe_resultados)){
    for (k in 1:length(names(arx$coefficients))){
      if (rownames(dataframe_resultados[j,0])==names(arx$coefficients)[k]){
        dataframe_resultados[j,i+1]<-arx$coefficients[k]
      }
    }
    if (rownames(dataframe_resultados)[j] == "Número de variables"){
      dataframe_resultados[j,i+1]<-length(arx$coefficients)
    }
    if (rownames(dataframe_resultados)[j] == "(Intercept)"){
      dataframe_resultados[j,i+1]<-0
    }
    if (rownames(dataframe_resultados)[j] == "R2"){
      dataframe_resultados[j,i+1]<-summary(arx)$r.squared
    }
    if (rownames(dataframe_resultados)[j] == "MAE"){
      dataframe_resultados[j,i+1]<-mae(temperatura_interior_med,
                                       temperatura_interior_pred)
    }
    if (rownames(dataframe_resultados)[j] == "Media residuales"){
      dataframe_resultados[j,i+1]<-media
    }
    if (rownames(dataframe_resultados)[j] == "Desviación típica residuales"){
      dataframe_resultados[j,i+1]<-desv_tipica
    }
  }
  
  dejadas<-c()
  
  for (j in 1:length(arx$coefficients)){
    final<-nchar(names(arx$coefficients[j]))
    if (a[["coefficients"]][j,4] > 0.05 &
        substring(names(arx$coefficients[j]), final-1, final-1) == "_" & 
        substring(names(arx$coefficients[j]), final, final) == i){
      quitar<-names(arx$coefficients[j])
      formula<-gsub(quitar, "", formula)
    }else if (a[["coefficients"]][j,4] <= 0.05 &
              substring(names(arx$coefficients[j]), final-1, final-1) == "_" & 
              substring(names(arx$coefficients[j]), final, final) == i){
      dejadas<-c(dejadas, substring(names(arx$coefficients)[j], 1, final-2))
    }
  }
  final<-nchar(formula)
  while (substring(formula, final, final) == " " || 
         substring(formula, final, final) == "+"){
    formula<-substring(formula, 1, final-1)
    final<-final-1
  }
}

#-------------------------------------------------------------------------------

# Exportar el dataframe de resultados a archivo csv

write.csv2(dataframe_resultados, 
           paste(wd, "/output/dataframe_resultados_2.csv", sep=""))

#-------------------------------------------------------------------------------

# Limpieza de variables

rm(a, arx, dataframe_resultados, dejadas, final, formula, i, j, k,
   nombre_columna, nombre_var, quitadas, quitar, temperatura_interior_med,
   temperatura_interior_pred, x, media, desv_tipica)

#===============================================================================

#===============================================================================
# Modelo 3:
  # Intersección en 0
  # Modelo completo e ir reduciendo
  # Reducción por pasos de tiempo
#-------------------------------------------------------------------------------

# Creación de la ecuación de regresión del modelo introduciendo todas las
# variables y todas las observaciones anteriores de las variables

{
  formula<-paste(regresion_output, " ~ 0 +", sep="")
  
  for (i in 1:length(variables_regresion_input)) {
    formula<-paste(formula, " ", variables_regresion_input[i], sep="") 
    if (i<length(variables_regresion_input))
      formula<-paste(formula, " +", sep="") 
  }
  
  for (i in 1:obs_anteriores) {
    nombre_var<-paste(regresion_output, "_", i, sep="")
    formula<-paste(formula, " + ", nombre_var, sep="") 
    
    nombre_var<-paste(variables_regresion_input, "_", i, sep="")
    
    for (j in 1:length(nombre_var)) {
      formula<-paste(formula, " + ",nombre_var[j], sep="")
    }
  }
}

#-------------------------------------------------------------------------------

# ARX completo

{
  attach(dataframe_trabajo)
  arx<-lm(formula)
  detach(dataframe_trabajo)
  a<-summary(arx)
}

#-------------------------------------------------------------------------------

# Creación del dataframe de resultados

{
  dataframe_resultados<-data.frame(matrix(nrow=numero_filas+5))
  rownames(dataframe_resultados)<-c(nombres_filas, 
                                    "p-valor del parámetro menos significativo")
  colnames(dataframe_resultados)<-"Iteración 1" 
}

#-------------------------------------------------------------------------------

# Introducción del primer ARX en el dataframe de resultados

{
  x<-obs_anteriores+1
  
  temperatura_interior_pred<-predict(arx)
  temperatura_interior_med<-
    dataframe_trabajo$temperatura_interior[x:nrow(dataframe_trabajo)]
  
  media<-mean(arx$residuals)
  desv_tipica<-sd(arx$residuals)
  
  significancia_min<-0
  
  for (i in 1:length(arx$coefficients)){
    if (a[["coefficients"]][i,4] > significancia_min){
      significancia_min<-a[["coefficients"]][i,4]
    }
  }
  
  for (i in 1:nrow(dataframe_resultados)){
    for (j in 1:length(arx$coefficients)){
      if (rownames(dataframe_resultados)[i] == names(arx$coefficients)[j]){
        dataframe_resultados[i,1]<-arx$coefficients[j]
      }
    }
    if (rownames(dataframe_resultados)[i] == "Número de variables"){
      dataframe_resultados[i,1]<-length(arx$coefficients)
    }
    if (rownames(dataframe_resultados)[i] == "(Intercept)"){
      dataframe_resultados[i,1]<-0
    }
    if (rownames(dataframe_resultados)[i] == "R2"){
      dataframe_resultados[i,1]<-summary(arx)$r.squared
    }
    if (rownames(dataframe_resultados)[i] == "MAE"){
      dataframe_resultados[i,1]<-mae(temperatura_interior_med,
                                                   temperatura_interior_pred)
    }
    if (rownames(dataframe_resultados)[i] == "p-valor del parámetro menos significativo"){
      dataframe_resultados[i,1]<-significancia_min
    }
    if (rownames(dataframe_resultados)[i] == "Media residuales"){
      dataframe_resultados[i,1]<-media
    }
    if (rownames(dataframe_resultados)[i] == "Desviación típica residuales"){
      dataframe_resultados[i,1]<-desv_tipica
    }
  }
}

#-------------------------------------------------------------------------------

# ARX cada vez con un paso de tiempo menos

{
  for (j in obs_anteriores:1){
    for (i in 1:length(arx$coefficients)){
      final<-nchar(names(arx$coefficients[i]))
      if (substring(names(arx$coefficients[i]), final, final) == j &
          substring(names(arx$coefficients[i]), final-1, final-1) == "_"){
        quitar<-names(arx$coefficients[i])
        formula<-gsub(quitar, " ", formula)
      }
      final<-nchar(formula)
      while (substring(formula, final, final) == " " || 
             substring(formula, final, final) == "+"){
        formula<-substring(formula, 1, final-1)
        final<-final-1
      }
    }
    
    attach(dataframe_trabajo)
    arx<-lm(formula)
    detach(dataframe_trabajo)
    a<-summary(arx)
    
    x<-j
    
    temperatura_interior_pred<-predict(arx)
    temperatura_interior_med<-
      dataframe_trabajo$temperatura_interior[x:nrow(dataframe_trabajo)]
    
    media<-mean(arx$residuals)
    desv_tipica<-sd(arx$residuals)
    
    dataframe_resultados$mas<-NA
    
    for (i in 1:length(arx$coefficients)){
      if (a[["coefficients"]][i,4] > significancia_min){
        significancia_min<-a[["coefficients"]][i,4]
      }
    }
    
    for (i in 1:nrow(dataframe_resultados)){
      for (k in 1:length(arx$coefficients)){
        if (rownames(dataframe_resultados)[i] == names(arx$coefficients)[k]){
          dataframe_resultados$mas[i]<-arx$coefficients[k]
        }
      }
      if (rownames(dataframe_resultados)[i] == "Número de variables"){
        dataframe_resultados$mas[i]<-length(arx$coefficients)
      }
      if (rownames(dataframe_resultados)[i] == "(Intercept)"){
        dataframe_resultados$mas[i]<-0
      }
      if (rownames(dataframe_resultados)[i] == "R2"){
        dataframe_resultados$mas[i]<-summary(arx)$r.squared
      }
      if (rownames(dataframe_resultados)[i] == "MAE"){
        dataframe_resultados$mas[i]<-mae(temperatura_interior_med,
                                         temperatura_interior_pred)
      }
      if (rownames(dataframe_resultados)[i] == "p-valor del parámetro menos significativo"){
        dataframe_resultados$mas[i]<-significancia_min
      }
      if (rownames(dataframe_resultados)[i] == "Media residuales"){
        dataframe_resultados$mas[i]<-media
      }
      if (rownames(dataframe_resultados)[i] == "Desviación típica residuales"){
        dataframe_resultados$mas[i]<-desv_tipica
      }
    }
    l<-which(names(dataframe_resultados) == "mas")
    nombre_columna<-paste("Iteración ", l, sep="")
    colnames(dataframe_resultados)[l]<-nombre_columna
  }
}

#-------------------------------------------------------------------------------

# Exportar el dataframe de resultados a archivo csv

write.csv2(dataframe_resultados, 
           paste(wd, "/output/dataframe_resultados_3.csv", sep=""))

#-------------------------------------------------------------------------------

# Limpieza de variables

rm(a, arx, dataframe_resultados, final, formula, i, j, k, l, nombre_columna,
   nombre_var, quitar, temperatura_interior_med, temperatura_interior_pred, x,
   significancia_min, media, desv_tipica)

#===============================================================================

#===============================================================================
# Modelo 4:
  # Intersección en 0
  # Modelo completo e ir reduciendo
  # Reducción por identificación de variable menos relevante
#-------------------------------------------------------------------------------

# Fórmula completa

{
  dejadas<-c()
  
  formula<-paste(regresion_output, " ~ 0 + ", sep="")
  
  for (i in 1:length(variables_regresion_input)) {
    formula<-paste(formula, " ", variables_regresion_input[i], sep="") 
    if (i<length(variables_regresion_input))
      formula<-paste(formula, " +", sep="") 
  }
  
  for (i in 1:obs_anteriores) {
    nombre_var<-paste(regresion_output, "_", i, sep="")
    formula<-paste(formula, " + ", nombre_var, sep="") 
    
    nombre_var<-paste(variables_regresion_input, "_", i, sep="")
    
    for (j in 1:length(nombre_var)) {
      formula<-paste(formula, " + ",nombre_var[j], sep="")
    }
  }
}

#-------------------------------------------------------------------------------

# ARX con todos los pasos de tiempo anteriores y todas las variables

{
  attach(dataframe_trabajo)
  arx<-lm(formula)
  detach(dataframe_trabajo)
  a<-summary(arx)
}

#-------------------------------------------------------------------------------

# Creación del dataframe de resultados e introducción del primer ARX

{
  dataframe_resultados<-data.frame(matrix(nrow=numero_filas+4))
  rownames(dataframe_resultados)<-nombres_filas
  colnames(dataframe_resultados)<-"Iteración 1"
  
  x<-obs_anteriores+1
  
  temperatura_interior_pred<-predict(arx)
  temperatura_interior_med<-
    dataframe_trabajo$temperatura_interior[x:nrow(dataframe_trabajo)]
  
  media<-mean(arx$residuals)
  desv_tipica<-sd(arx$residuals)
  
  for (i in 1:nrow(dataframe_resultados)){
    for (j in 1:length(arx$coefficients)){
      if (rownames(dataframe_resultados)[i] == names(arx$coefficients)[j]){
        dataframe_resultados[i,1]<-arx$coefficients[j]
      }
    }
    if (rownames(dataframe_resultados)[i] == "Número de variables"){
      dataframe_resultados[i,1]<-length(arx$coefficients)
    }
    if (rownames(dataframe_resultados)[i] == "(Intercept)"){
      dataframe_resultados[i,1]<-0
    }
    if (rownames(dataframe_resultados)[i] == "R2"){
      dataframe_resultados[i,1]<-summary(arx)$r.squared
    }
    if (rownames(dataframe_resultados)[i] == "MAE"){
      dataframe_resultados[i,1]<-mae(temperatura_interior_med,
                                                   temperatura_interior_pred)
    }
    if (rownames(dataframe_resultados)[i] == "Media residuales"){
      dataframe_resultados[i,1]<-media
    }
    if (rownames(dataframe_resultados)[i] == "Desviación típica residuales"){
      dataframe_resultados[i,1]<-desv_tipica
    }
  }
}

#-------------------------------------------------------------------------------

# Reducción del ARX según la significatividad de las variables (cuando una
# variable es significativa para un paso de tiempo, se considera significativa
# para los pasos de tiempo restantes hasta el actual)

{
  for (j in obs_anteriores:0){
    exit=0
    if (j==obs_anteriores){
      for (i in 1:length(names(arx$coefficients))){
        final<-nchar(names(arx$coefficients)[i])
        if (substring(names(arx$coefficients[i]),final,final)==obs_anteriores){
          if (a[["coefficients"]][i,4]>0.05){
            quitar<-names(arx$coefficients[i])
            formula<-gsub(quitar, "", formula)
          }else{
            dejadas<-c(dejadas, substring(names(arx$coefficients)[i],1,final-2))
          }
        }
      }
      
    }else if (j<obs_anteriores & j>0){
      for (i in 1:length(names(arx$coefficients))){
        exit<-0
        final<-nchar(names(arx$coefficients)[i])
        if (substring(names(arx$coefficients[i]),final-1,final) == paste("_", 
                                                                         j, 
                                                                         sep="")){
          if (a[["coefficients"]][i,4]>0.05){
            for (k in 1:length(dejadas)){
              if (substring(names(arx$coefficients[i]),1,final-2) == dejadas[k]){
                exit<-1
              }
            }
            if (exit!=1){
              quitar<-names(arx$coefficients[i])
              formula<-gsub(quitar, "", formula)
            }
          }else{
            dejadas<-c(dejadas, substring(names(arx$coefficients)[i],1,final-2))
          }
        }
      }
      
    }else if (j==0){
      for (i in 1:length(names(arx$coefficients))){
        final<-nchar(names(arx$coefficients)[i])
        if (substring(names(arx$coefficients[i]),final-1,final-1) != "_"){
          exit<-0
          if (a[["coefficients"]][i,4]>0.05){
            for (k in 1:length(dejadas)){
              if (names(arx$coefficients[i])==dejadas[k]){
                exit<-1
              }
            }
            if (exit!=1){
              quitar<-names(arx$coefficients[i])
              formula<-gsub(quitar, " ", formula)
            }
          }
        }
      }
    }
    
    final<-nchar(formula)
    while (substring(formula, final, final) == " " || 
           substring(formula, final, final) == "+"){
      formula<-substring(formula, 1, final-1)
      final<-final-1
    }
    
    attach(dataframe_trabajo)
    arx<-lm(formula)
    detach(dataframe_trabajo)
    a<-summary(arx)
    
    x<-obs_anteriores+1
    
    temperatura_interior_pred<-predict(arx)
    temperatura_interior_med<-
      dataframe_trabajo$temperatura_interior[x:nrow(dataframe_trabajo)]
    
    media<-mean(arx$residuals)
    desv_tipica<-sd(arx$residuals)
    
    dataframe_resultados$mas<-NA
    
    for (i in 1:nrow(dataframe_resultados)){
      for (k in 1:length(arx$coefficients)){
        if (rownames(dataframe_resultados)[i] == names(arx$coefficients)[k]){
          dataframe_resultados$mas[i]<-arx$coefficients[k]
        }
      }
      if (rownames(dataframe_resultados)[i] == "Número de variables"){
        dataframe_resultados$mas[i]<-length(arx$coefficients)
      }
      if (rownames(dataframe_resultados)[i] == "(Intercept)"){
        dataframe_resultados$mas[i]<-0
      }
      if (rownames(dataframe_resultados)[i] == "R2"){
        dataframe_resultados$mas[i]<-summary(arx)$r.squared
      }
      if (rownames(dataframe_resultados)[i] == "MAE"){
        dataframe_resultados$mas[i]<-mae(temperatura_interior_med,
                                         temperatura_interior_pred)
      }
      if (rownames(dataframe_resultados)[i] == "Media residuales"){
        dataframe_resultados$mas[i]<-media
      }
      if (rownames(dataframe_resultados)[i] == "Desviación típica residuales"){
        dataframe_resultados$mas[i]<-desv_tipica
      }
    }
    j<-which(names(dataframe_resultados) == "mas")
    nombre_columna<-paste("Iteración ", j, sep="")
    colnames(dataframe_resultados)[j]<-nombre_columna
  } 
}

#-------------------------------------------------------------------------------

# Exportar el dataframe de resultados a archivo csv

write.csv2(dataframe_resultados, 
           paste(wd, "/output/dataframe_resultados_4.csv", sep=""))

#-------------------------------------------------------------------------------

# Limpieza de variables

rm(a, arx, dataframe_resultados, dejadas, exit, final, formula, i, j, k,
   nombre_columna, nombre_var, quitar, temperatura_interior_med,
   temperatura_interior_pred, x, media, desv_tipica)

#===============================================================================

#===============================================================================
# Modelo 5:
  # Intersección libre
  # Extensión del modelo desde el instante inicial hasta el pasado
  # Extensión por pasos de tiempo
#-------------------------------------------------------------------------------

# Creación del dataframe de resultados

{
  dataframe_resultados<-data.frame(matrix(nrow=numero_filas+5))
  rownames(dataframe_resultados)<-c(nombres_filas,
                                    "p-valor del parámetro menos significativo")
  colnames(dataframe_resultados)<-"Iteración 1"
}

#-------------------------------------------------------------------------------

# ARX con los diferentes pasos de tiempo

{
  for (j in 0:obs_anteriores){
    formula<-paste(regresion_output, " ~ ", sep="")
    
    for (i in 1:length(variables_regresion_input)){
      formula<-paste(formula, " ", variables_regresion_input[i], sep="") 
      if (i<length(variables_regresion_input)){
        formula<-paste(formula, " +", sep="")
      }
    }
    
    if (j>0){
      for (i in 1:j) {
        nombre_var<-paste(regresion_output, "_", i, sep="")
        formula<-paste(formula, " + ", nombre_var, sep="") 
        
        nombre_var<-paste(variables_regresion_input, "_", i, sep="")
        
        for (k in 1:length(nombre_var)) {
          formula<-paste(formula, " + ",nombre_var[k], sep="")
        }
      }
    }
    
    attach(dataframe_trabajo)
    arx<-lm(formula)
    detach(dataframe_trabajo)
    a<-summary(arx)
    
    x<-j+1
    
    temperatura_interior_pred<-predict(arx)
    temperatura_interior_med<-
      dataframe_trabajo$temperatura_interior[x:nrow(dataframe_trabajo)]
    
    media<-mean(arx$residuals)
    desv_tipica<-sd(arx$residuals)
    
    significancia_min<-0
    
    for (i in 1:length(arx$coefficients)){
      if (a[["coefficients"]][i,4] > significancia_min){
        significancia_min<-a[["coefficients"]][i,4]
      }
    }
    
    if (j==0){
      for (i in 1:nrow(dataframe_resultados)){
        for (k in 1:length(names(arx$coefficients))){
          if (rownames(dataframe_resultados[i,0])==names(arx$coefficients)[k]){
            dataframe_resultados[i,1]<-arx$coefficients[k]
          }
        }
        if (rownames(dataframe_resultados)[i] == "Número de variables"){
          dataframe_resultados[i,1]<-length(arx$coefficients)
        }
        if (rownames(dataframe_resultados)[i] == "R2"){
          dataframe_resultados[i,1]<-summary(arx)$r.squared
        }
        if (rownames(dataframe_resultados)[i] == "MAE"){
          dataframe_resultados[i,1]<-mae(temperatura_interior_med,
                                                       temperatura_interior_pred)
        }
        if (rownames(dataframe_resultados)[i] == "p-valor del parámetro menos significativo"){
          dataframe_resultados[i,1]<-significancia_min
        }
        if (rownames(dataframe_resultados)[i] == "Media residuales"){
          dataframe_resultados[i,1]<-media
        }
        if (rownames(dataframe_resultados)[i] == "Desviación típica residuales"){
          dataframe_resultados[i,1]<-desv_tipica
        }
      }
    } else {
      nombre_columna<-paste("Iteración ", j+1, sep="")
      dataframe_resultados$mas<-rep(NA,nrow(dataframe_resultados))
      colnames(dataframe_resultados)[j+1]<-nombre_columna
      for (i in 1:nrow(dataframe_resultados)){
        for (k in 1:length(names(arx$coefficients))){
          if (rownames(dataframe_resultados[i,0])==names(arx$coefficients)[k]){
            dataframe_resultados[i,j+1]<-arx$coefficients[k]
          }
        }
        if (rownames(dataframe_resultados)[i] == "Número de variables"){
          dataframe_resultados[i,j+1]<-length(arx$coefficients)
        }
        if (rownames(dataframe_resultados)[i] == "R2"){
          dataframe_resultados[i,j+1]<-summary(arx)$r.squared
        }
        if (rownames(dataframe_resultados)[i] == "MAE"){
          dataframe_resultados[i,j+1]<-mae(temperatura_interior_med,
                                           temperatura_interior_pred)
        }
        if (rownames(dataframe_resultados)[i] == "p-valor del parámetro menos significativo"){
          dataframe_resultados[i,j+1]<-significancia_min
        }
        if (rownames(dataframe_resultados)[i] == "Media residuales"){
          dataframe_resultados[i,j+1]<-media
        }
        if (rownames(dataframe_resultados)[i] == "Desviación típica residuales"){
          dataframe_resultados[i,j+1]<-desv_tipica
        }
      }
    }
  } 
}

#-------------------------------------------------------------------------------

# Exportar el dataframe de resultados a archivo csv

write.csv2(dataframe_resultados, 
           paste(wd, "/output/dataframe_resultados_5.csv", sep=""))

#-------------------------------------------------------------------------------

# Limpieza de variables

rm(a, arx, dataframe_resultados, formula, i, j, k, nombre_columna, nombre_var,
   temperatura_interior_med, temperatura_interior_pred, x, significancia_min,
   media, desv_tipica)

#===============================================================================

#===============================================================================
# Modelo 6:
  # Intersección libre
  # Extensión del modelo desde el instante inicial hasta el pasado
  # Extensión por identificación de variable más relevante
#-------------------------------------------------------------------------------

# Creación de la ecuación de regresión del modelo introduciendo todas las
# variables y la observación actual de dichas variables

{
  formula<-paste(regresion_output, " ~ ", sep="")
  
  for (i in 1:length(variables_regresion_input)) {
    formula<-paste(formula, " ", variables_regresion_input[i], sep="") 
    if (i<length(variables_regresion_input))
      formula<-paste(formula, " +", sep="") 
  }
}

#-------------------------------------------------------------------------------

# ARX con el instante actual

{
  attach(dataframe_trabajo)
  arx<-lm(formula)
  detach(dataframe_trabajo)
  a<-summary(arx)
}

#-------------------------------------------------------------------------------

# Creación del dataframe de resultados

{
  dataframe_resultados<-data.frame(matrix(nrow=numero_filas+4))
  rownames(dataframe_resultados)<-c(nombres_filas)
  colnames(dataframe_resultados)<-"Iteración 1" 
}

#-------------------------------------------------------------------------------

# Introducción del primer ARX en el dataframe de resultados

{
  
  temperatura_interior_pred<-predict(arx)
  temperatura_interior_med<-dataframe_trabajo$temperatura_interior
  
  media<-mean(arx$residuals)
  desv_tipica<-sd(arx$residuals)
  
  for (i in 1:nrow(dataframe_resultados)){
    for (j in 1:length(arx$coefficients)){
      if (rownames(dataframe_resultados)[i] == names(arx$coefficients)[j]){
        dataframe_resultados[i,1]<-arx$coefficients[j]
      }
    }
    if (rownames(dataframe_resultados)[i] == "R2"){
      dataframe_resultados[i,1]<-summary(arx)$r.squared
    }
    if (rownames(dataframe_resultados)[i] == "MAE"){
      dataframe_resultados[i,1]<-mae(temperatura_interior_med,
                                                   temperatura_interior_pred)
    }
    if (rownames(dataframe_resultados)[i] == "Media residuales"){
      dataframe_resultados[i,1]<-media
    }
    if (rownames(dataframe_resultados)[i] == "Desviación típica residuales"){
      dataframe_resultados[i,1]<-desv_tipica
    }
    if (rownames(dataframe_resultados)[i] == "Número de variables"){
      dataframe_resultados[i,1]<-length(arx$coefficients)
    }
  }
}

#-------------------------------------------------------------------------------

# ARX

  # Quitar las variables no significativas del primer ARX

{
  dejadas<-c()
  
  for (i in 1:length(arx$coefficients)){
    if (a[["coefficients"]][i,4] > 0.05){
      quitar<-names(arx$coefficients[i])
      formula<-gsub(quitar, "", formula)
    }else{
      dejadas<-c(dejadas, names(arx$coefficients)[i])
    }
  }
}

  # Bucle que introduce cada vez un nuevo paso de tiempo y quita las variables
  # no significativas. Cuando una variable no es signficativa par aun paso de
  # tiempo, se considera no significativa para los siguientes y no se introduce
  # en la regresión

for (i in 1:obs_anteriores){
  
  if (length(dejadas) != 0){
    if (i == 1){
      nombre_var<-paste(regresion_output, "_", i, sep="")
      for (j in 1:length(nombre_var)) {
        formula<-paste(formula, " + ",nombre_var[j], sep="")
      }
    }else{
      nombre_var<-paste(regresion_output, "_", i, sep="")
      final<-nchar(nombre_var)
      for (j in 1:length(dejadas)){
        if (dejadas[j]==substring(nombre_var, 1, final-2)){
          formula<-paste(formula, " + ",nombre_var[j], sep="")
        }else{
          quitadas<-substring(nombre_var, 1, final-2)
        }
      }
    }
    
    nombre_var<-paste(variables_regresion_input, "_", i, sep="")
    
    for (j in 1:length(nombre_var)){
      final<-nchar(nombre_var[j])
      for (k in 1:length(dejadas)){
        if (dejadas[k] == substring(nombre_var[j], 1, final-2)){
          formula<-paste(formula, " + ", nombre_var[j], sep="")
        }
      }
    }
    
    attach(dataframe_trabajo)
    arx<-lm(formula)
    detach(dataframe_trabajo)
    a<-summary(arx)
    
    x<-i+1
    
    temperatura_interior_pred<-predict(arx)
    temperatura_interior_med<-
      dataframe_trabajo$temperatura_interior[x:nrow(dataframe_trabajo)]
    
    media<-mean(arx$residuals)
    desv_tipica<-sd(arx$residuals)
    
    nombre_columna<-paste("Iteración ", i+1, sep="")
    dataframe_resultados$mas<-rep(NA,nrow(dataframe_resultados))
    colnames(dataframe_resultados)[i+1]<-nombre_columna
    
    for (j in 1:nrow(dataframe_resultados)){
      for (k in 1:length(names(arx$coefficients))){
        if (rownames(dataframe_resultados[j,0])==names(arx$coefficients)[k]){
          dataframe_resultados[j,i+1]<-arx$coefficients[k]
        }
      }
      if (rownames(dataframe_resultados)[j] == "R2"){
        dataframe_resultados[j,i+1]<-summary(arx)$r.squared
      }
      if (rownames(dataframe_resultados)[j] == "MAE"){
        dataframe_resultados[j,i+1]<-mae(temperatura_interior_med,
                                         temperatura_interior_pred)
      }
      if (rownames(dataframe_resultados)[j] == "Media residuales"){
        dataframe_resultados[j,i+1]<-media
      }
      if (rownames(dataframe_resultados)[j] == "Desviación típica residuales"){
        dataframe_resultados[j,i+1]<-desv_tipica
      }
      if (rownames(dataframe_resultados)[j] == "Número de variables"){
        dataframe_resultados[j,i+1]<-length(arx$coefficients)
      }
    }
    
    dejadas<-c()
    
    for (j in 2:length(arx$coefficients)){
      final<-nchar(names(arx$coefficients[j]))
      if (a[["coefficients"]][j,4] > 0.05 &
          substring(names(arx$coefficients[j]), final-1, final-1) == "_" & 
          substring(names(arx$coefficients[j]), final, final) == i){
        quitar<-names(arx$coefficients[j])
        formula<-gsub(quitar, "", formula)
      }else if (a[["coefficients"]][j,4] <= 0.05 &
                substring(names(arx$coefficients[j]), final-1, final-1) == "_" & 
                substring(names(arx$coefficients[j]), final, final) == i){
        dejadas<-c(dejadas, substring(names(arx$coefficients)[j], 1, final-2))
      }
    }
    final<-nchar(formula)
    while (substring(formula, final, final) == " " || 
           substring(formula, final, final) == "+"){
      formula<-substring(formula, 1, final-1)
      final<-final-1
    }
  }
}

#-------------------------------------------------------------------------------

# Exportar el dataframe de resultados a archivo csv

write.csv2(dataframe_resultados, 
           paste(wd, "/output/dataframe_resultados_6.csv", sep=""))

#-------------------------------------------------------------------------------

# Limpieza de variables

rm(a, arx, dataframe_resultados, dejadas, final, formula, i, j, k,
   nombre_columna, nombre_var, quitadas, quitar, temperatura_interior_med,
   temperatura_interior_pred, x, media, desv_tipica)

#===============================================================================

#===============================================================================
# Modelo 7:
  # Intersección libre
  # Modelo completo e ir reduciendo
  # Reducción por pasos de tiempo
#-------------------------------------------------------------------------------

# Creación de la ecuación de regresión del modelo introduciendo todas las
# variables y todas las observaciones anteriores de las variables

{
  formula<-paste(regresion_output, " ~ ", sep="")
  
  for (i in 1:length(variables_regresion_input)) {
    formula<-paste(formula, " ", variables_regresion_input[i], sep="") 
    if (i<length(variables_regresion_input))
      formula<-paste(formula, " +", sep="") 
  }
  
  for (i in 1:obs_anteriores) {
    nombre_var<-paste(regresion_output, "_", i, sep="")
    formula<-paste(formula, " + ", nombre_var, sep="") 
    
    nombre_var<-paste(variables_regresion_input, "_", i, sep="")
    
    for (j in 1:length(nombre_var)) {
      formula<-paste(formula, " + ",nombre_var[j], sep="")
    }
  }
}

#-------------------------------------------------------------------------------

# ARX completo

{
  attach(dataframe_trabajo)
  arx<-lm(formula)
  detach(dataframe_trabajo)
  a<-summary(arx)
}

#-------------------------------------------------------------------------------

# Creación del dataframe de resultados

{
  dataframe_resultados<-data.frame(matrix(nrow=numero_filas+5))
  rownames(dataframe_resultados)<-c(nombres_filas,
                                    "p-valor del parámetro menos significativo")
  colnames(dataframe_resultados)<-"Iteración 1" 
}

#-------------------------------------------------------------------------------

# Introducción del primer ARX en el dataframe de resultados

{
  x<-obs_anteriores+1
  
  temperatura_interior_pred<-predict(arx)
  temperatura_interior_med<-
    dataframe_trabajo$temperatura_interior[x:nrow(dataframe_trabajo)]
  
  media<-mean(arx$residuals)
  desv_tipica<-sd(arx$residuals)
  
  significancia_min<-0
  
  for (i in 1:length(arx$coefficients)){
    if (a[["coefficients"]][i,4] > significancia_min){
      significancia_min<-a[["coefficients"]][i,4]
    }
  }
  
  for (i in 1:nrow(dataframe_resultados)){
    for (j in 1:length(arx$coefficients)){
      if (rownames(dataframe_resultados)[i] == names(arx$coefficients)[j]){
        dataframe_resultados[i,1]<-arx$coefficients[j]
      }
    }
    if (rownames(dataframe_resultados)[i] == "R2"){
      dataframe_resultados[i,1]<-summary(arx)$r.squared
    }
    if (rownames(dataframe_resultados)[i] == "MAE"){
      dataframe_resultados[i,1]<-mae(temperatura_interior_med,
                                                   temperatura_interior_pred)
    }
    if (rownames(dataframe_resultados)[i] == "p-valor del parámetro menos significativo"){
      dataframe_resultados[i,1]<-significancia_min
    }
    if (rownames(dataframe_resultados)[i] == "Media residuales"){
      dataframe_resultados[i,1]<-media
    }
    if (rownames(dataframe_resultados)[i] == "Desviación típica residuales"){
      dataframe_resultados[i,1]<-desv_tipica
    }
    if (rownames(dataframe_resultados)[i] == "Número de variables"){
      dataframe_resultados[i,1]<-length(arx$coefficients)
    }
  }
}

#-------------------------------------------------------------------------------

# ARX cada vez con un paso de tiempo menos

{
  for (j in obs_anteriores:1){
    for (i in 1:length(arx$coefficients)){
      final<-nchar(names(arx$coefficients[i]))
      if (substring(names(arx$coefficients[i]), final, final) == j &
          substring(names(arx$coefficients[i]), final-1, final-1) == "_"){
        quitar<-names(arx$coefficients[i])
        formula<-gsub(quitar, " ", formula)
      }
      final<-nchar(formula)
      while (substring(formula, final, final) == " " || 
             substring(formula, final, final) == "+"){
        formula<-substring(formula, 1, final-1)
        final<-final-1
      }
    }
    
    attach(dataframe_trabajo)
    arx<-lm(formula)
    detach(dataframe_trabajo)
    a<-summary(arx)
    
    x<-j
    
    temperatura_interior_pred<-predict(arx)
    temperatura_interior_med<-
      dataframe_trabajo$temperatura_interior[x:nrow(dataframe_trabajo)]
    
    media<-mean(arx$residuals)
    desv_tipica<-sd(arx$residuals)
    
    significancia_min<-0
    
    for (i in 1:length(arx$coefficients)){
      if (a[["coefficients"]][i,4] > significancia_min){
        significancia_min<-a[["coefficients"]][i,4]
      }
    }
    
    dataframe_resultados$mas<-NA
    
    for (i in 1:nrow(dataframe_resultados)){
      for (k in 1:length(arx$coefficients)){
        if (rownames(dataframe_resultados)[i] == names(arx$coefficients)[k]){
          dataframe_resultados$mas[i]<-arx$coefficients[k]
        }
      }
      if (rownames(dataframe_resultados)[i] == "R2"){
        dataframe_resultados$mas[i]<-summary(arx)$r.squared
      }
      if (rownames(dataframe_resultados)[i] == "MAE"){
        dataframe_resultados$mas[i]<-mae(temperatura_interior_med,
                                         temperatura_interior_pred)
      }
      if (rownames(dataframe_resultados)[i] == "p-valor del parámetro menos significativo"){
        dataframe_resultados$mas[i]<-significancia_min
      }
      if (rownames(dataframe_resultados)[i] == "Media residuales"){
        dataframe_resultados$mas[i]<-media
      }
      if (rownames(dataframe_resultados)[i] == "Desviación típica residuales"){
        dataframe_resultados$mas[i]<-desv_tipica
      }
      if (rownames(dataframe_resultados)[i] == "Número de variables"){
        dataframe_resultados$mas[i]<-length(arx$coefficients)
      }
    }
    l<-which(names(dataframe_resultados) == "mas")
    nombre_columna<-paste("Iteración ", l, sep="")
    colnames(dataframe_resultados)[l]<-nombre_columna
  }
}

#-------------------------------------------------------------------------------

# Exportar el dataframe de resultados a archivo csv

write.csv2(dataframe_resultados, 
           paste(wd, "/output/dataframe_resultados_7.csv", sep=""))

#-------------------------------------------------------------------------------

# Limpieza de variables

rm(a, arx, dataframe_resultados, final, formula, i, j, k, l, nombre_columna,
   nombre_var, quitar, temperatura_interior_med, temperatura_interior_pred, x,
   significancia_min, media, desv_tipica)

#===============================================================================

#===============================================================================
# Modelo 8:
  # Intersección libre
  # Modelo completo e ir reduciendo
  # Reducción por identificación de variable menos relevante
#-------------------------------------------------------------------------------

# Fórmula completa

{
  dejadas<-c()
  
  formula<-paste(regresion_output, " ~ ", sep="")
  
  for (i in 1:length(variables_regresion_input)) {
    formula<-paste(formula, " ", variables_regresion_input[i], sep="") 
    if (i<length(variables_regresion_input))
      formula<-paste(formula, " +", sep="") 
  }
  
  for (i in 1:obs_anteriores) {
    nombre_var<-paste(regresion_output, "_", i, sep="")
    formula<-paste(formula, " + ", nombre_var, sep="") 
    
    nombre_var<-paste(variables_regresion_input, "_", i, sep="")
    
    for (j in 1:length(nombre_var)) {
      formula<-paste(formula, " + ",nombre_var[j], sep="")
    }
  }
}

#-------------------------------------------------------------------------------

# ARX con todos los pasos de tiempo anteriores y todas las variables

{
  attach(dataframe_trabajo)
  arx<-lm(formula)
  detach(dataframe_trabajo)
  a<-summary(arx)
}

#-------------------------------------------------------------------------------

# Creación del dataframe de resultados e introducción del primer ARX

{
  dataframe_resultados<-data.frame(matrix(nrow=numero_filas+4))
  rownames(dataframe_resultados)<-c(nombres_filas)
  colnames(dataframe_resultados)<-"Interación 1"
  
  x<-obs_anteriores+1
  
  temperatura_interior_pred<-predict(arx)
  temperatura_interior_med<-
    dataframe_trabajo$temperatura_interior[x:nrow(dataframe_trabajo)]
  
  media<-mean(arx$residuals)
  desv_tipica<-sd(arx$residuals)
  
  for (i in 1:nrow(dataframe_resultados)){
    for (j in 1:length(arx$coefficients)){
      if (rownames(dataframe_resultados)[i] == names(arx$coefficients)[j]){
        dataframe_resultados[i,1]<-arx$coefficients[j]
      }
    }
    if (rownames(dataframe_resultados)[i] == "R2"){
      dataframe_resultados[i,1]<-summary(arx)$r.squared
    }
    if (rownames(dataframe_resultados)[i] == "MAE"){
      dataframe_resultados[i,1]<-mae(temperatura_interior_med,
                                                   temperatura_interior_pred)
    }
    if (rownames(dataframe_resultados)[i] == "Media residuales"){
      dataframe_resultados[i,1]<-media
    }
    if (rownames(dataframe_resultados)[i] == "Desviación típica residuales"){
      dataframe_resultados[i,1]<-desv_tipica
    }
    if (rownames(dataframe_resultados)[i] == "Número de variables"){
      dataframe_resultados[i,1]<-length(arx$coefficients)
    }
  }
}

#-------------------------------------------------------------------------------

# Reducción del ARX según la significatividad de las variables (cuando una
# variable es significativa para un paso de tiempo, se considera significativa
# para los pasos de tiempo restantes hasta el actual)

{
  for (j in obs_anteriores:0){
    exit=0
    if (j==obs_anteriores){
      for (i in 1:length(names(arx$coefficients))){
        final<-nchar(names(arx$coefficients)[i])
        if (substring(names(arx$coefficients[i]),final,final)==obs_anteriores){
          if (a[["coefficients"]][i,4]>0.05){
            quitar<-names(arx$coefficients[i])
            formula<-gsub(quitar, "", formula)
          }else{
            dejadas<-c(dejadas, substring(names(arx$coefficients)[i],1,final-2))
          }
        }
      }
      
    }else if (j<obs_anteriores & j>0){
      for (i in 1:length(names(arx$coefficients))){
        exit<-0
        final<-nchar(names(arx$coefficients)[i])
        if (substring(names(arx$coefficients[i]),final-1,final) == paste("_", 
                                                                         j, 
                                                                         sep="")){
          if (a[["coefficients"]][i,4]>0.05){
            for (k in 1:length(dejadas)){
              if (substring(names(arx$coefficients[i]),1,final-2) == dejadas[k]){
                exit<-1
              }
            }
            if (exit!=1){
              quitar<-names(arx$coefficients[i])
              formula<-gsub(quitar, "", formula)
            }
          }else{
            dejadas<-c(dejadas, substring(names(arx$coefficients)[i],1,final-2))
          }
        }
      }
      
    }else if (j==0){
      for (i in 1:length(names(arx$coefficients))){
        final<-nchar(names(arx$coefficients)[i])
        if (substring(names(arx$coefficients[i]),final-1,final-1) != "_"){
          exit<-0
          if (a[["coefficients"]][i,4]>0.05){
            for (k in 1:length(dejadas)){
              if (names(arx$coefficients[i])==dejadas[k]){
                exit<-1
              }
            }
            if (exit!=1){
              quitar<-names(arx$coefficients[i])
              formula<-gsub(quitar, " ", formula)
            }
          }
        }
      }
    }
    
    final<-nchar(formula)
    while (substring(formula, final, final) == " " || 
           substring(formula, final, final) == "+"){
      formula<-substring(formula, 1, final-1)
      final<-final-1
    }
    
    attach(dataframe_trabajo)
    arx<-lm(formula)
    detach(dataframe_trabajo)
    a<-summary(arx)
    
    x<-obs_anteriores+1
    
    temperatura_interior_pred<-predict(arx)
    temperatura_interior_med<-
      dataframe_trabajo$temperatura_interior[x:nrow(dataframe_trabajo)]
    
    media<-mean(arx$residuals)
    desv_tipica<-sd(arx$residuals)
    
    dataframe_resultados$mas<-NA
    
    for (i in 1:nrow(dataframe_resultados)){
      for (k in 1:length(arx$coefficients)){
        if (rownames(dataframe_resultados)[i] == names(arx$coefficients)[k]){
          dataframe_resultados$mas[i]<-arx$coefficients[k]
        }
      }
      if (rownames(dataframe_resultados)[i] == "R2"){
        dataframe_resultados$mas[i]<-summary(arx)$r.squared
      }
      if (rownames(dataframe_resultados)[i] == "MAE"){
        dataframe_resultados$mas[i]<-mae(temperatura_interior_med,
                                         temperatura_interior_pred)
      }
      if (rownames(dataframe_resultados)[i] == "Media residuales"){
        dataframe_resultados$mas[i]<-media
      }
      if (rownames(dataframe_resultados)[i] == "Desviación típica residuales"){
        dataframe_resultados$mas[i]<-desv_tipica
      }
      if (rownames(dataframe_resultados)[i] == "Número de variables"){
        dataframe_resultados$mas[i]<-length(arx$coefficients)
      }
    }
    j<-which(names(dataframe_resultados) == "mas")
    nombre_columna<-paste("Iteración ", j, sep="")
    colnames(dataframe_resultados)[j]<-nombre_columna
  } 
}

#-------------------------------------------------------------------------------

# Exportar el dataframe de resultados a archivo csv

write.csv2(dataframe_resultados, 
           paste(wd, "/output/dataframe_resultados_8.csv", sep=""))

#-------------------------------------------------------------------------------

# Limpieza de variables

rm(a, arx, dataframe_resultados, formula, dejadas, exit, final, i, j, k,
   nombre_columna, nombre_var, quitar, temperatura_interior_med,
   temperatura_interior_pred, x, media, desv_tipica)

#===============================================================================

#===============================================================================
# Limpieza de variables
#-------------------------------------------------------------------------------

rm(grados_margen, nombres_filas, numero_filas, obs_anteriores, regresion_output,
   sampleo, variables_regresion_input, prob, z)

#===============================================================================