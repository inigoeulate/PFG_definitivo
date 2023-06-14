#===============================================================================
#===============================================================================
# Noveno script del PFG "Desarrollo de modelos de caracterización a corto plazo
# de la evolución térmico-energética en edificios" elaborado por el estudiante
# Iñigo García de Eulate Pascual (5ºADE+ITI, Universidad de Deusto) en
# colaboración con el tutor del PFG Roberto Garay Martínez.
#===============================================================================
#===============================================================================

#===============================================================================
#===============================================================================
# En este noveno script se llevarán a cabo los ARX iniciales.
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
# Variables que se emplearán para los modelos
#-------------------------------------------------------------------------------

variables_regresion_input<-c("ocupantes_conteo_robus3",
                             "temperatura_exterior",
                             "energia_agua_refrigerada",
                             "radiacion_global_fachada")

regresion_output<-"temperatura_interior"

#===============================================================================

#===============================================================================
# Primer modelo ARX: se ponen todas las variables para todos los pasos de tiempo
# anteriores y se van quitando las no signficiativas, hasta quedar solo las 
# significativas
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

# Modelo ARX con todas las variables y todas sus observaciones anteriores

{
  attach (dataframe_trabajo)
  arx<-lm(formula)
  detach(dataframe_trabajo)
  a<-summary(arx)
}

#-------------------------------------------------------------------------------

# Creación de las variables temperatura interior medida y predicha

{
  x<-obs_anteriores+1
  
  temperatura_interior_pred<-predict(arx)
  temperatura_interior_med<-
    dataframe_trabajo$temperatura_interior[x:nrow(dataframe_trabajo)]
}

# Graficación de este ARX 0

{
  grados_margen<-0.25
  
  plot (temperatura_interior_med, temperatura_interior_pred, 
        xlab="Temperatura interior medida [ºC]", 
        ylab="Temperatura interior predicha [ºC]",
        xlim=c(25,29),
        ylim=c(25,29))
  abline(a=0, b=1, col="red", lwd=4)
  abline(a=-1, b=1, col="green", lwd=4)
  abline(a=1, b=1, col="green", lwd=4)
  
  png(paste(getwd(),"/plots/4_ARX_ini/temp_interior_med_vs_pred_0.png",sep=""), width=800, 
      height=800)
  par(mar=c(5,5,3,5))
  plot (temperatura_interior_med, temperatura_interior_pred, 
        xlab="Temperatura interior medida [ºC]", 
        ylab="Temperatura interior predicha [ºC]",
        xlim=c(25,29),
        ylim=c(25,29),
        cex.lab=1.5, cex.axis=1.5)
  abline(a=0, b=1, col="red", lwd=4)
  abline(a=-1, b=1, col="green", lwd=4)
  abline(a=1, b=1, col="green", lwd=4)
  dev.off()
  
  dataframe_trabajo$hora_solar<-
    as.POSIXct(strftime(dataframe_trabajo$hora_solar, format = "%Y-%m-%d %H:%M:%OS"))
  
  plot(dataframe_trabajo$hora_solar[x:nrow(dataframe_trabajo)],
       temperatura_interior_med,
       xlab="Marca de tiempo", ylab= "Temperatura [ºC]", col="red", cex=1.5,
       ylim=c(min(min(temperatura_interior_med-grados_margen),
                  min(temperatura_interior_pred-grados_margen)),
              max(max(temperatura_interior_med+grados_margen),
                  max(temperatura_interior_pred+grados_margen))))
  points(dataframe_trabajo$hora_solar[x:nrow(dataframe_trabajo)],
         temperatura_interior_pred, col="blue", cex=1.5)
  legend(x="topleft", legend=c("Temperatura interior medida [ºC]",
                               "Temperatura interior predicha [ºC]"),
         col=c("red","blue"),
         lty=c(1,1),
         cex=0.5)
  
  png(paste(getwd(),"/plots/4_ARX_ini/temp_interior_med_vs_pred_con_marca_tiempo_0.png",
            sep=""), width=800, height=800)
  par(mar=c(5,5,3,5))
  plot(dataframe_trabajo$hora_solar[x:nrow(dataframe_trabajo)],
       temperatura_interior_med,
       xlab="Marca de tiempo", ylab= "Temperatura [ºC]", col="red", cex=1.5,
       ylim=c(min(min(temperatura_interior_med-grados_margen),
                  min(temperatura_interior_pred-grados_margen)),
              max(max(temperatura_interior_med+grados_margen),
                  max(temperatura_interior_pred+grados_margen))),
       cex.lab=1.5, cex.axis=1.5)
  points(dataframe_trabajo$hora_solar[x:nrow(dataframe_trabajo)],
         temperatura_interior_pred, col="blue", cex=1.5)
  legend(x="topleft", legend=c("Temperatura interior medida [ºC]",
                               "Temperatura interior predicha [ºC]"),
         col=c("red","blue"),
         lty=c(1,1),
         cex=2)
  dev.off() 
}

#-------------------------------------------------------------------------------

# Creación de la matriz de resultados e introducción de los resultados del
# primer modelo ARX

{
  numero_filas<-length(names(arx$coefficients))+2
  
  nombres_filas<-c(names(arx$coefficients), "R2", "MAE")
  
  matriz_resultados<-matrix(nrow=numero_filas,
                            ncol=length(names(arx$coefficients)))
  rownames(matriz_resultados)<-nombres_filas
  matriz_resultados[,1]<-c(a[["coefficients"]][,4],
                           summary(arx)$r.squared,
                           mae(temperatura_interior_med,
                               temperatura_interior_pred))
}

#-------------------------------------------------------------------------------

# Bucle para eliminar con cada iteración la variable menos significativa (la que
# tiene "Pr(>|t|)" mayor)

# La última columna de la matriz_resultados es el modelo ARX en el que todas las
# variables son significativas ((Pr(>|t|))<0.05)

{
  j<-2
  y<-1
  
  while (y==1) {
    minimo<-0
    for (i in 1:length(names(arx$coefficients))){
      if (a[["coefficients"]][i,4]>minimo & a[["coefficients"]][i,4]>0.05){
        minimo<-a[["coefficients"]][i,4]
        quitar<-paste(names(arx$coefficients)[i], " ", sep="")
      }
    }
    if (minimo==0){
      quitar=""
      y=0
    }
    
    formula<-gsub(quitar, "", formula)
    
    attach (dataframe_trabajo)
    arx<-lm(formula)
    detach(dataframe_trabajo)
    a<-summary(arx)
    
    x<-obs_anteriores+1
    
    temperatura_interior_pred<-predict(arx)
    temperatura_interior_med<-
      dataframe_trabajo$temperatura_interior[x:nrow(dataframe_trabajo)]
    
    for (i in 1:nrow(matriz_resultados)){
      for (k in 1:length(arx$coefficients)){
        if (rownames(matriz_resultados)[i] == names(arx$coefficients)[k]){
          matriz_resultados[i,j]<-arx$coefficients[k]
        }
      }
      if (rownames(matriz_resultados)[i] == "R2"){
        matriz_resultados[i,j]<-summary(arx)$r.squared
      }
      if (rownames(matriz_resultados)[i] == "MAE"){
        matriz_resultados[i,j]<-mae(temperatura_interior_med,
                                    temperatura_interior_pred)
      }
    }
    j<-j+1
  }
}

#-------------------------------------------------------------------------------

# Graficación del primer ARX

{
  
  plot (temperatura_interior_med, temperatura_interior_pred, 
        xlab="Temperatura interior medida [ºC]", 
        ylab="Temperatura interior predicha [ºC]",
        xlim=c(25,29),
        ylim=c(25,29))
  abline(a=0, b=1, col="red", lwd=4)
  abline(a=-1, b=1, col="green", lwd=4)
  abline(a=1, b=1, col="green", lwd=4)
  
  png(paste(getwd(),"/plots/4_ARX_ini/temp_interior_med_vs_pred_1.png",sep=""), width=800, 
      height=800)
  par(mar=c(5,5,3,5))
  plot (temperatura_interior_med, temperatura_interior_pred, 
        xlab="Temperatura interior medida [ºC]", 
        ylab="Temperatura interior predicha [ºC]",
        xlim=c(25,29),
        ylim=c(25,29),
        cex.lab=1.5, cex.axis=1.5)
  abline(a=0, b=1, col="red", lwd=4)
  abline(a=-1, b=1, col="green", lwd=4)
  abline(a=1, b=1, col="green", lwd=4)
  dev.off()
  
  dataframe_trabajo$hora_solar<-
    as.POSIXct(strftime(dataframe_trabajo$hora_solar, format = "%Y-%m-%d %H:%M:%OS"))
  
  plot(dataframe_trabajo$hora_solar[x:nrow(dataframe_trabajo)],
       temperatura_interior_med,
       xlab="Marca de tiempo", ylab= "Temperatura [ºC]", col="red", cex=1.5,
       ylim=c(min(min(temperatura_interior_med-grados_margen),
                  min(temperatura_interior_pred-grados_margen)),
              max(max(temperatura_interior_med+grados_margen),
                  max(temperatura_interior_pred+grados_margen))))
  points(dataframe_trabajo$hora_solar[x:nrow(dataframe_trabajo)],
         temperatura_interior_pred, col="blue", cex=1.5)
  legend(x="topleft", legend=c("Temperatura interior medida [ºC]",
                               "Temperatura interior predicha [ºC]"),
         col=c("red","blue"),
         lty=c(1,1),
         cex=0.5)
  
  png(paste(getwd(),"/plots/4_ARX_ini/temp_interior_med_vs_pred_con_marca_tiempo_1.png",
            sep=""), width=800, height=800)
  par(mar=c(5,5,3,5))
  plot(dataframe_trabajo$hora_solar[x:nrow(dataframe_trabajo)],
       temperatura_interior_med,
       xlab="Marca de tiempo", ylab= "Temperatura [ºC]", col="red", cex=1.5,
       ylim=c(min(min(temperatura_interior_med-grados_margen),
                  min(temperatura_interior_pred-grados_margen)),
              max(max(temperatura_interior_med+grados_margen),
                  max(temperatura_interior_pred+grados_margen))),
       cex.axis=1.5, cex.lab=1.5)
  points(dataframe_trabajo$hora_solar[x:nrow(dataframe_trabajo)],
         temperatura_interior_pred, col="blue", cex=1.5)
  legend(x="topleft", legend=c("Temperatura interior medida [ºC]",
                               "Temperatura interior predicha [ºC]"),
         col=c("red","blue"),
         lty=c(1,1),
         cex=2)
  dev.off() 
}

#-------------------------------------------------------------------------------

# Limpieza de variables

rm(a, arx, matriz_resultados, formula, i, j, k, minimo, nombre_var, quitar, 
   sampleo, temperatura_interior_med, temperatura_interior_pred, x, y)

#===============================================================================

#===============================================================================
# Segundo(s) ARX: instante de tiempo actual y se va añadiendo cada vez un
# instante de tiempo pasado forzando al intercept a ser 0
#-------------------------------------------------------------------------------

# Creación del dataframe que contendrá los resultados

{
  dataframe_resultados<-data.frame(matrix(nrow=numero_filas))
  rownames(dataframe_resultados)<-nombres_filas
  colnames(dataframe_resultados)<-"instante_actual" 
}

#-------------------------------------------------------------------------------

# ARX con todos los pasos de tiempo anteriores

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
    
    if (j==0){
      for (i in 1:nrow(dataframe_resultados)){
        for (k in 1:length(names(arx$coefficients))){
          if (rownames(dataframe_resultados[i,0])==names(arx$coefficients)[k]){
            dataframe_resultados$instante_actual[i]<-arx$coefficients[k]
          }
        }
        if (rownames(dataframe_resultados)[i] == "R2"){
          dataframe_resultados$instante_actual[i]<-summary(arx)$r.squared
        }
        if (rownames(dataframe_resultados)[i] == "MAE"){
          dataframe_resultados$instante_actual[i]<-mae(temperatura_interior_med,
                                                       temperatura_interior_pred)
        }
      }
    } else {
      nombre_columna<-paste("instante_pasado", j, sep="_")
      dataframe_resultados$mas<-rep(NA,nrow(dataframe_resultados))
      colnames(dataframe_resultados)[j+1]<-nombre_columna
      for (i in 1:nrow(dataframe_resultados)){
        for (k in 1:length(names(arx$coefficients))){
          if (rownames(dataframe_resultados[i,0])==names(arx$coefficients)[k]){
            dataframe_resultados[i,j+1]<-arx$coefficients[k]
          }
        }
        if (rownames(dataframe_resultados)[i] == "R2"){
          dataframe_resultados[i,j+1]<-summary(arx)$r.squared
        }
        if (rownames(dataframe_resultados)[i] == "MAE"){
          dataframe_resultados[i,j+1]<-mae(temperatura_interior_med,
                                           temperatura_interior_pred)
        }
      }
    }
  } 
}

#-------------------------------------------------------------------------------

# A partir del ARX con el instante de tiempo actual (se elige porque tiene un 
# R2>0,98), se analiza la significatividad de las variables

{
  pasos_tiempo_pasados<-0
  dejadas<-c()
  
  formula<-paste(regresion_output, " ~ 0 +", sep="")
  
  for (i in 1:length(variables_regresion_input)) {
    formula<-paste(formula, " ", variables_regresion_input[i], sep="") 
    if (i<length(variables_regresion_input))
      formula<-paste(formula, " +", sep="") 
  }
  
  if (pasos_tiempo_pasados>0){
    for (i in 1:pasos_tiempo_pasados) {
      nombre_var<-paste(regresion_output, "_", i, sep="")
      formula<-paste(formula, " + ", nombre_var, sep="") 
      
      nombre_var<-paste(variables_regresion_input, "_", i, sep="")
      
      for (j in 1:length(nombre_var)) {
        formula<-paste(formula, " + ",nombre_var[j], sep="")
      }
    } 
  }
  
  attach(dataframe_trabajo)
  arx<-lm(formula)
  detach(dataframe_trabajo)
  a<-summary(arx)
  
  for (j in pasos_tiempo_pasados:0){
    exit=0
    if (j == pasos_tiempo_pasados){
      for (i in 1:length(names(arx$coefficients))){
        final<-nchar(names(arx$coefficients)[i])
        if (substring(names(arx$coefficients[i]),final,final) == 
            pasos_tiempo_pasados){
          if (a[["coefficients"]][i,4] > 0.05){
            quitar<-names(arx$coefficients[i])
            formula<-gsub(quitar, "", formula)
          }else{
            dejadas<-c(dejadas, substring(names(arx$coefficients)[i],1,final-2))
          }
        }
      }
      
    }else if (j<pasos_tiempo_pasados & j>0){
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
    
    x<-j+1
    
    temperatura_interior_pred<-predict(arx)
    temperatura_interior_med<-
      dataframe_trabajo$temperatura_interior[x:nrow(dataframe_trabajo)]
    
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
    }
    j<-which(names(dataframe_resultados) == "mas")
    nombre_columna<-paste("iteracion", "_", j, sep="")
    colnames(dataframe_resultados)[j]<-nombre_columna
  } 
}

#-------------------------------------------------------------------------------

# Graficación del segundo ARX

{
  plot (temperatura_interior_med, temperatura_interior_pred, 
        xlab="Temperatura interior medida [ºC]", 
        ylab="Temperatura interior predicha [ºC]",
        xlim=c(25,29),
        ylim=c(25,29))
  abline(a=0, b=1, col="red", lwd=4)
  abline(a=-1, b=1, col="green", lwd=4)
  abline(a=1, b=1, col="green", lwd=4)
  
  png(paste(getwd(),"/plots/4_ARX_ini/temp_interior_med_vs_pred_2.png",sep=""), width=800, 
      height=800)
  par(mar=c(5,5,3,5))
  plot (temperatura_interior_med, temperatura_interior_pred, 
        xlab="Temperatura interior medida [ºC]", 
        ylab="Temperatura interior predicha [ºC]",
        xlim=c(25,29),
        ylim=c(25,29),
        cex.lab=1.5, cex.axis=1.5)
  abline(a=0, b=1, col="red", lwd=4)
  abline(a=-1, b=1, col="green", lwd=4)
  abline(a=1, b=1, col="green", lwd=4)
  dev.off()

  plot(dataframe_trabajo$hora_solar[pasos_tiempo_pasados+1:nrow(dataframe_trabajo)],
       temperatura_interior_med,
       xlab="Marca de tiempo", ylab= "Temperatura [ºC]", col="red", cex=1.5,
       ylim=c(min(min(temperatura_interior_med-grados_margen),
                  min(temperatura_interior_pred-grados_margen)),
              max(max(temperatura_interior_med+grados_margen),
                  max(temperatura_interior_pred+grados_margen))))
  points(dataframe_trabajo$hora_solar[pasos_tiempo_pasados+1:nrow(dataframe_trabajo)],
         temperatura_interior_pred, col="blue", cex=1.5)
  legend(x="topleft", legend=c("Temperatura interior medida [ºC]",
                               "Temperatura interior predicha [ºC]"),
         col=c("red","blue"),
         lty=c(1,1),
         cex=0.5)
  
  png(paste(getwd(),"/plots/4_ARX_ini/temp_interior_med_vs_pred_con_marca_tiempo_2.png",
            sep=""), width=800, height=800)
  par(mar=c(5,5,3,5))
  plot(dataframe_trabajo$hora_solar[pasos_tiempo_pasados+1:nrow(dataframe_trabajo)],
       temperatura_interior_med,
       xlab="Marca de tiempo", ylab= "Temperatura [ºC]", col="red", cex=1.5,
       ylim=c(min(min(temperatura_interior_med-grados_margen),
                  min(temperatura_interior_pred-grados_margen)),
              max(max(temperatura_interior_med+grados_margen),
                  max(temperatura_interior_pred+grados_margen))),
       cex.lab=1.5, cex.axis=1.5)
  points(dataframe_trabajo$hora_solar[pasos_tiempo_pasados+1:nrow(dataframe_trabajo)],
         temperatura_interior_pred, col="blue", cex=1.5)
  legend(x="topleft", legend=c("Temperatura interior medida [ºC]",
                               "Temperatura interior predicha [ºC]"),
         col=c("red","blue"),
         lty=c(1,1),
         cex=2)
  dev.off() 
}

#-------------------------------------------------------------------------------

# Limpieza de variables

rm(a, arx, dataframe_resultados, dejadas, exit, final, formula, i, j, k, x,
   nombre_columna, nombre_var, temperatura_interior_med,
   temperatura_interior_pred, pasos_tiempo_pasados)

#===============================================================================

#===============================================================================
# Tercer(os) ARX: instante de tiempo actual y se va añadiendo cada vez un
# instante de tiempo pasado sin forzar al intercept a ser 0
#-------------------------------------------------------------------------------

{
  dataframe_resultados<-data.frame(matrix(nrow=numero_filas+1))
  rownames(dataframe_resultados)<-c("(Intercept)",nombres_filas)
  colnames(dataframe_resultados)<-"instante_actual"
  
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
    
    if (j==0){
      for (i in 1:nrow(dataframe_resultados)){
        for (k in 1:length(names(arx$coefficients))){
          if (rownames(dataframe_resultados[i,0])==names(arx$coefficients)[k]){
            dataframe_resultados$instante_actual[i]<-arx$coefficients[k]
          }
        }
        if (rownames(dataframe_resultados)[i] == "R2"){
          dataframe_resultados$instante_actual[i]<-summary(arx)$r.squared
        }
        if (rownames(dataframe_resultados)[i] == "MAE"){
          dataframe_resultados$instante_actual[i]<-mae(temperatura_interior_med,
                                                       temperatura_interior_pred)
        }
      }
    } else {
      nombre_columna<-paste("instante_pasado", j, sep="_")
      dataframe_resultados$mas<-rep(NA,nrow(dataframe_resultados))
      colnames(dataframe_resultados)[j+1]<-nombre_columna
      for (i in 1:nrow(dataframe_resultados)){
        for (k in 1:length(names(arx$coefficients))){
          if (rownames(dataframe_resultados[i,0])==names(arx$coefficients)[k]){
            dataframe_resultados[i,j+1]<-arx$coefficients[k]
          }
        }
        if (rownames(dataframe_resultados)[i] == "R2"){
          dataframe_resultados[i,j+1]<-summary(arx)$r.squared
        }
        if (rownames(dataframe_resultados)[i] == "MAE"){
          dataframe_resultados[i,j+1]<-mae(temperatura_interior_med,
                                           temperatura_interior_pred)
        }
      }
    }
  } 
}

#-------------------------------------------------------------------------------

# A partir del ARX con el instante de tiempo actual y los 6 instantes anteriores
# (se elige porque tiene el mayor R2 (R2=0,83)), se analiza la significatividad
# de las variables desde las mas antiguas
# Ejemplo: si el quinto instante anterior de la radiación solar es
# significativo, se consideran como significativos lo instantes del quinto al
# actual para la radiacion solar

{
  pasos_tiempo_pasados<-6
  dejadas<-c()
  
  formula<-paste(regresion_output, " ~ ", sep="")
  
  for (i in 1:length(variables_regresion_input)) {
    formula<-paste(formula, " ", variables_regresion_input[i], sep="") 
    if (i<length(variables_regresion_input))
      formula<-paste(formula, " +", sep="") 
  }
  
  for (i in 1:pasos_tiempo_pasados) {
    nombre_var<-paste(regresion_output, "_", i, sep="")
    formula<-paste(formula, " + ", nombre_var, sep="") 
    
    nombre_var<-paste(variables_regresion_input, "_", i, sep="")
    
    for (j in 1:length(nombre_var)) {
      formula<-paste(formula, " + ",nombre_var[j], sep="")
    }
  }
  
  attach(dataframe_trabajo)
  arx<-lm(formula)
  detach(dataframe_trabajo)
  a<-summary(arx)
  
  for (j in pasos_tiempo_pasados:0){
    exit=0
    if (j == pasos_tiempo_pasados){
      for (i in 1:length(names(arx$coefficients))){
        final<-nchar(names(arx$coefficients)[i])
        if (substring(names(arx$coefficients[i]),final,final) == 
            pasos_tiempo_pasados){
          if (a[["coefficients"]][i,4] > 0.05){
            quitar<-names(arx$coefficients[i])
            formula<-gsub(quitar, "", formula)
          }else{
            dejadas<-c(dejadas, substring(names(arx$coefficients)[i],1,final-2))
          }
        }
      }
      
    }else if (j<pasos_tiempo_pasados & j>0){
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
    }
    j<-which(names(dataframe_resultados) == "mas")
    nombre_columna<-paste("iteracion", "_", j, sep="")
    colnames(dataframe_resultados)[j]<-nombre_columna
  } 
}

#-------------------------------------------------------------------------------

# Graficación del tercer ARX

{
  plot (temperatura_interior_med, temperatura_interior_pred, 
        xlab="Temperatura interior medida [ºC]", 
        ylab="Temperatura interior predicha [ºC]",
        xlim=c(25,29),
        ylim=c(25,29))
  abline(a=0, b=1, col="red", lwd=4)
  abline(a=-1, b=1, col="green", lwd=4)
  abline(a=1, b=1, col="green", lwd=4)
  
  png(paste(getwd(),"/plots/4_ARX_ini/temp_interior_med_vs_pred_3.png",sep=""), width=800, 
      height=800)
  par(mar=c(5,5,3,5))
  plot (temperatura_interior_med, temperatura_interior_pred, 
        xlab="Temperatura interior medida [ºC]", 
        ylab="Temperatura interior predicha [ºC]",
        xlim=c(25,29),
        ylim=c(25,29),
        cex.lab=1.5, cex.axis=1.5)
  abline(a=0, b=1, col="red", lwd=4)
  abline(a=-1, b=1, col="green", lwd=4)
  abline(a=1, b=1, col="green", lwd=4)
  dev.off()
  
  dataframe_trabajo$hora_solar<-as.POSIXct(strftime(dataframe_trabajo$hora_solar, 
                                                    format = "%Y-%m-%d %H:%M:%OS"))
  
  plot(dataframe_trabajo$hora_solar[x:nrow(dataframe_trabajo)],
       temperatura_interior_med,
       xlab="Marca de tiempo", ylab= "Temperatura [ºC]", col="red", cex=1.5,
       ylim=c(min(temperatura_interior_med-grados_margen),
              max(temperatura_interior_med+grados_margen)))
  points(dataframe_trabajo$hora_solar[x:nrow(dataframe_trabajo)],
         temperatura_interior_pred, col="blue", cex=1.5)
  legend(x="topleft", legend=c("Temperatura interior medida [ºC]",
                               "Temperatura interior predicha [ºC]"),
         col=c("red","blue"),
         lty=c(1,1),
         cex=0.5)
  
  png(paste(getwd(),"/plots/4_ARX_ini/temp_interior_med_vs_pred_con_marca_tiempo_3.png",
            sep=""), width=800, height=800)
  par(mar=c(5,5,3,5))
  plot(dataframe_trabajo$hora_solar[x:nrow(dataframe_trabajo)],
       temperatura_interior_med,
       xlab="Marca de tiempo", ylab= "Temperatura [ºC]", col="red", cex=1.5,
       ylim=c(min(temperatura_interior_med-grados_margen),
              max(temperatura_interior_med+grados_margen)),
       cex.lab=1.5, cex.axis=1.5)
  points(dataframe_trabajo$hora_solar[x:nrow(dataframe_trabajo)],
         temperatura_interior_pred, col="blue", cex=1.5)
  legend(x="topleft", legend=c("Temperatura interior medida [ºC]",
                               "Temperatura interior predicha [ºC]"),
         col=c("red","blue"),
         lty=c(1,1),
         cex=1.5)
  dev.off() 
}

#-------------------------------------------------------------------------------

# Limpiado de variables

rm(a, arx, dataframe_resultados, dataframe_trabajo, dejadas, exit, final,
   formula, grados_margen, i, j, k, nombre_columna, nombre_var, nombres_filas,
   numero_filas, obs_anteriores, pasos_tiempo_pasados, quitar, regresion_output,
   temperatura_interior_med, temperatura_interior_pred,
   variables_regresion_input, x)

#===============================================================================