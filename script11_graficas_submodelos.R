#===============================================================================
#===============================================================================
# Undécimo script del PFG "Desarrollo de modelos de caracterización a corto plazo
# de la evolución térmico-energética en edificios" elaborado por el estudiante
# Iñigo García de Eulate Pascual (5ºADE+ITI, Universidad de Deusto) en
# colaboración con el tutor del PFG Roberto Garay Martínez.
#===============================================================================
#===============================================================================

#===============================================================================
#===============================================================================
# En este undécimo script se grafica el submodelo elegido de cada modelo
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
  
  prob<-0.05
  
  z<-qnorm(prob/2,0,1,lower.tail=FALSE)
}

#-------------------------------------------------------------------------------

# Limpieza de variables

rm(a, arx, dataframe_60, formula, i, j, nombre_dataframe, nombre_var)

#===============================================================================

#===============================================================================
# Modelo 1
#-------------------------------------------------------------------------------

{
  formula<-paste(regresion_output, " ~ 0 +", sep="")
  
  for (i in 1:length(variables_regresion_input)) {
    formula<-paste(formula, " ", variables_regresion_input[i], sep="") 
    if (i<length(variables_regresion_input))
      formula<-paste(formula, " +", sep="") 
  }
  
  for (i in 1:2) {
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
  
  temperatura_interior_pred<-predict(arx)
  temperatura_interior_med<-
    dataframe_trabajo$temperatura_interior[3:nrow(dataframe_trabajo)]
  
  media<-mean(arx$residuals)
  desv_tipica<-sd(arx$residuals)
  
  intervalo_sup<-temperatura_interior_pred+media+desv_tipica*z
  intervalo_inf<-temperatura_interior_pred+media-desv_tipica*z
  
  plot (temperatura_interior_med, temperatura_interior_pred, 
        xlab="Temperatura interior medida [ºC]", 
        ylab="Temperatura interior predicha [ºC]",
        xlim=c(25,29), ylim=c(25,29))
  abline(a=0, b=1, col="red", lwd=4)
  abline(a=-1, b=1, col="green", lwd=4)
  abline(a=1, b=1, col="green", lwd=4)
  
  png(paste(getwd(),"/plots/temp_interior_med_vs_pred_s1.png",sep=""), width=800, 
      height=800)
  par(mar=c(5,5,3,5))
  plot (temperatura_interior_med, temperatura_interior_pred, 
        xlab="Temperatura interior medida [ºC]", 
        ylab="Temperatura interior predicha [ºC]",
        xlim=c(25,29), ylim=c(25,29),
        cex.lab=1.5, cex.axis=1.5)
  abline(a=0, b=1, col="red", lwd=4)
  abline(a=-1, b=1, col="green", lwd=4)
  abline(a=1, b=1, col="green", lwd=4)
  dev.off()
}

#===============================================================================

#===============================================================================
# Modelo 2
#-------------------------------------------------------------------------------

{
  formula<-paste(regresion_output, " ~ 0 +", sep="")
  
  variables_regresion_input_m<-variables_regresion_input[variables_regresion_input
                                                         !="energia_agua_refrigerada"]
  
  for (i in 1:length(variables_regresion_input_m)) {
    formula<-paste(formula, " ", variables_regresion_input_m[i], sep="") 
    if (i<length(variables_regresion_input_m))
      formula<-paste(formula, " +", sep="") 
  }
  
  variables_regresion_input_m<-variables_regresion_input_m[variables_regresion_input_m
                                                           !="temperatura_exterior"]
  
  variables_regresion_input_m<-variables_regresion_input_m[variables_regresion_input_m
                                                           !="radiacion_global_fachada"]
  
  for (i in 1:3) {
    nombre_var<-paste(regresion_output, "_", i, sep="")
    formula<-paste(formula, " + ", nombre_var, sep="") 
    
    nombre_var<-paste(variables_regresion_input_m, "_", i, sep="")
    
    for (j in 1:length(nombre_var)) {
      formula<-paste(formula, " + ",nombre_var[j], sep="")
    }
  }
  
  attach (dataframe_trabajo)
  arx<-lm(formula)
  detach(dataframe_trabajo)
  a<-summary(arx)
  
  temperatura_interior_pred<-predict(arx)
  temperatura_interior_med<-
    dataframe_trabajo$temperatura_interior[4:nrow(dataframe_trabajo)]
  
  media<-mean(arx$residuals)
  desv_tipica<-sd(arx$residuals)
  
  intervalo_sup<-temperatura_interior_pred+media+desv_tipica*z
  intervalo_inf<-temperatura_interior_pred+media-desv_tipica*z
  
  plot (temperatura_interior_med, temperatura_interior_pred, 
        xlab="Temperatura interior medida [ºC]", 
        ylab="Temperatura interior predicha [ºC]",
        xlim=c(25,29), ylim=c(25,29))
  abline(a=0, b=1, col="red", lwd=4)
  abline(a=-1, b=1, col="green", lwd=4)
  abline(a=1, b=1, col="green", lwd=4)
  
  png(paste(getwd(),"/plots/temp_interior_med_vs_pred_s2.png",sep=""), width=800, 
      height=800)
  par(mar=c(5,5,3,5))
  plot (temperatura_interior_med, temperatura_interior_pred, 
        xlab="Temperatura interior medida [ºC]", 
        ylab="Temperatura interior predicha [ºC]",
        xlim=c(25,29), ylim=c(25,29),
        cex.lab=1.5, cex.axis=1.5)
  abline(a=0, b=1, col="red", lwd=4)
  abline(a=-1, b=1, col="green", lwd=4)
  abline(a=1, b=1, col="green", lwd=4)
  dev.off() 
}

#===============================================================================

#===============================================================================
# Modelo 3
#-------------------------------------------------------------------------------

{
  formula<-paste(regresion_output, " ~ 0 +", sep="")
  
  for (i in 1:length(variables_regresion_input)) {
    formula<-paste(formula, " ", variables_regresion_input[i], sep="") 
    if (i<length(variables_regresion_input))
      formula<-paste(formula, " +", sep="") 
  }
  
  for (i in 1:2) {
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
  
  temperatura_interior_pred<-predict(arx)
  temperatura_interior_med<-
    dataframe_trabajo$temperatura_interior[3:nrow(dataframe_trabajo)]
  
  media<-mean(arx$residuals)
  desv_tipica<-sd(arx$residuals)
  
  intervalo_sup<-temperatura_interior_pred+media+desv_tipica*z
  intervalo_inf<-temperatura_interior_pred+media-desv_tipica*z
  
  plot (temperatura_interior_med, temperatura_interior_pred, 
        xlab="Temperatura interior medida [ºC]", 
        ylab="Temperatura interior predicha [ºC]",
        xlim=c(25,29), ylim=c(25,29))
  abline(a=0, b=1, col="red", lwd=4)
  abline(a=-1, b=1, col="green", lwd=4)
  abline(a=1, b=1, col="green", lwd=4)
  
  png(paste(getwd(),"/plots/temp_interior_med_vs_pred_s3.png",sep=""), width=800, 
      height=800)
  par(mar=c(5,5,3,5))
  plot (temperatura_interior_med, temperatura_interior_pred, 
        xlab="Temperatura interior medida [ºC]", 
        ylab="Temperatura interior predicha [ºC]",
        xlim=c(25,29), ylim=c(25,29),
        cex.lab=1.5, cex.axis=1.5)
  abline(a=0, b=1, col="red", lwd=4)
  abline(a=-1, b=1, col="green", lwd=4)
  abline(a=1, b=1, col="green", lwd=4)
  dev.off() 
}

#===============================================================================

#===============================================================================
# Modelo 4
#-------------------------------------------------------------------------------

{
  formula<-paste(regresion_output, " ~ 0 +", sep="")
  
  variables_regresion_input_m<-variables_regresion_input[variables_regresion_input
                                                         !="temperatura_exterior"]
  
  for (i in 1:length(variables_regresion_input_m)) {
    formula<-paste(formula, " ", variables_regresion_input_m[i], sep="") 
    if (i<length(variables_regresion_input_m))
      formula<-paste(formula, " +", sep="") 
  }
  
  for (i in 1:4) {
    nombre_var<-paste(regresion_output, "_", i, sep="")
    formula<-paste(formula, " + ", nombre_var, sep="") 
    
    nombre_var<-paste(variables_regresion_input_m, "_", i, sep="")
    
    for (j in 1:length(nombre_var)) {
      formula<-paste(formula, " + ",nombre_var[j], sep="")
    }
  }
  
  variables_regresion_input_m<-variables_regresion_input_m[variables_regresion_input_m
                                                           !="radiacion_global_fachada"]
  
  for (i in 5:6) {
    nombre_var<-paste(regresion_output, "_", i, sep="")
    formula<-paste(formula, " + ", nombre_var, sep="") 
    
    nombre_var<-paste(variables_regresion_input_m, "_", i, sep="")
    
    for (j in 1:length(nombre_var)) {
      formula<-paste(formula, " + ",nombre_var[j], sep="")
    }
  }
  
  attach (dataframe_trabajo)
  arx<-lm(formula)
  detach(dataframe_trabajo)
  a<-summary(arx)
  
  temperatura_interior_pred<-predict(arx)
  temperatura_interior_med<-
    dataframe_trabajo$temperatura_interior[7:nrow(dataframe_trabajo)]
  
  media<-mean(arx$residuals)
  desv_tipica<-sd(arx$residuals)
  
  intervalo_sup<-temperatura_interior_pred+media+desv_tipica*z
  intervalo_inf<-temperatura_interior_pred+media-desv_tipica*z
  
  plot (temperatura_interior_med, temperatura_interior_pred, 
        xlab="Temperatura interior medida [ºC]", 
        ylab="Temperatura interior predicha [ºC]",
        xlim=c(25,29), ylim=c(25,29))
  abline(a=0, b=1, col="red", lwd=4)
  abline(a=-1, b=1, col="green", lwd=4)
  abline(a=1, b=1, col="green", lwd=4)
  
  png(paste(getwd(),"/plots/temp_interior_med_vs_pred_s4.png",sep=""), width=800, 
      height=800)
  par(mar=c(5,5,3,5))
  plot (temperatura_interior_med, temperatura_interior_pred, 
        xlab="Temperatura interior medida [ºC]", 
        ylab="Temperatura interior predicha [ºC]",
        xlim=c(25,29), ylim=c(25,29),
        cex.lab=1.5, cex.axis=1.5)
  abline(a=0, b=1, col="red", lwd=4)
  abline(a=-1, b=1, col="green", lwd=4)
  abline(a=1, b=1, col="green", lwd=4)
  dev.off()
}

#===============================================================================

#===============================================================================
# Modelo 5
#-------------------------------------------------------------------------------

{
  formula<-paste(regresion_output, " ~ ", sep="")
  
  for (i in 1:length(variables_regresion_input)) {
    formula<-paste(formula, " ", variables_regresion_input[i], sep="") 
    if (i<length(variables_regresion_input))
      formula<-paste(formula, " +", sep="") 
  }
  
  for (i in 1:3) {
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
  
  temperatura_interior_pred<-predict(arx)
  temperatura_interior_med<-
    dataframe_trabajo$temperatura_interior[4:nrow(dataframe_trabajo)]
  
  media<-mean(arx$residuals)
  desv_tipica<-sd(arx$residuals)
  
  intervalo_sup<-temperatura_interior_pred+media+desv_tipica*z
  intervalo_inf<-temperatura_interior_pred+media-desv_tipica*z
  
  plot (temperatura_interior_med, temperatura_interior_pred, 
        xlab="Temperatura interior medida [ºC]", 
        ylab="Temperatura interior predicha [ºC]",
        xlim=c(25,29), ylim=c(25,29))
  abline(a=0, b=1, col="red", lwd=4)
  abline(a=-1, b=1, col="green", lwd=4)
  abline(a=1, b=1, col="green", lwd=4)
  
  png(paste(getwd(),"/plots/temp_interior_med_vs_pred_s5.png",sep=""), width=800, 
      height=800)
  par(mar=c(5,5,3,5))
  plot (temperatura_interior_med, temperatura_interior_pred, 
        xlab="Temperatura interior medida [ºC]", 
        ylab="Temperatura interior predicha [ºC]",
        xlim=c(25,29), ylim=c(25,29),
        cex.axis=1.5, cex.lab=1.5)
  abline(a=0, b=1, col="red", lwd=4)
  abline(a=-1, b=1, col="green", lwd=4)
  abline(a=1, b=1, col="green", lwd=4)
  dev.off()
}

#===============================================================================

#===============================================================================
# Modelo 6
#-------------------------------------------------------------------------------

{
  formula<-paste(regresion_output, " ~ radiacion_global_fachada + ",
                 "temperatura_interior_1 +radiacion_global_fachada_1 + ",
                 "temperatura_interior_2 + temperatura_interior_3 + ", 
                 "temperatura_interior_4", sep="")
  
  attach (dataframe_trabajo)
  arx<-lm(formula)
  detach(dataframe_trabajo)
  a<-summary(arx)
  
  temperatura_interior_pred<-predict(arx)
  temperatura_interior_med<-
    dataframe_trabajo$temperatura_interior[5:nrow(dataframe_trabajo)]
  
  media<-mean(arx$residuals)
  desv_tipica<-sd(arx$residuals)
  
  intervalo_sup<-temperatura_interior_pred+media+desv_tipica*z
  intervalo_inf<-temperatura_interior_pred+media-desv_tipica*z
  
  plot (temperatura_interior_med, temperatura_interior_pred, 
        xlab="Temperatura interior medida [ºC]", 
        ylab="Temperatura interior predicha [ºC]",
        xlim=c(25,29), ylim=c(25,29))
  abline(a=0, b=1, col="red", lwd=4)
  abline(a=-1, b=1, col="green", lwd=4)
  abline(a=1, b=1, col="green", lwd=4)
  
  png(paste(getwd(),"/plots/temp_interior_med_vs_pred_s6.png",sep=""), width=800, 
      height=800)
  par(mar=c(5,5,3,5))
  plot (temperatura_interior_med, temperatura_interior_pred, 
        xlab="Temperatura interior medida [ºC]", 
        ylab="Temperatura interior predicha [ºC]",
        xlim=c(25,29), ylim=c(25,29),
        cex.lab=1.5, cex.axis=1.5)
  abline(a=0, b=1, col="red", lwd=4)
  abline(a=-1, b=1, col="green", lwd=4)
  abline(a=1, b=1, col="green", lwd=4)
  dev.off()
}

#===============================================================================

#===============================================================================
# Modelo 7
#-------------------------------------------------------------------------------

{
  formula<-paste(regresion_output, " ~ ", sep="")
  
  for (i in 1:length(variables_regresion_input)) {
    formula<-paste(formula, " ", variables_regresion_input[i], sep="") 
    if (i<length(variables_regresion_input))
      formula<-paste(formula, " +", sep="") 
  }
  
  for (i in 1:2) {
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
  
  temperatura_interior_pred<-predict(arx)
  temperatura_interior_med<-
    dataframe_trabajo$temperatura_interior[3:nrow(dataframe_trabajo)]
  
  media<-mean(arx$residuals)
  desv_tipica<-sd(arx$residuals)
  
  intervalo_sup<-temperatura_interior_pred+media+desv_tipica*z
  intervalo_inf<-temperatura_interior_pred+media-desv_tipica*z
  
  plot (temperatura_interior_med, temperatura_interior_pred, 
        xlab="Temperatura interior medida [ºC]", 
        ylab="Temperatura interior predicha [ºC]",
        xlim=c(25,29), ylim=c(25,29))
  abline(a=0, b=1, col="red", lwd=4)
  abline(a=-1, b=1, col="green", lwd=4)
  abline(a=1, b=1, col="green", lwd=4)
  
  png(paste(getwd(),"/plots/temp_interior_med_vs_pred_s7.png",sep=""), width=800, 
      height=800)
  par(mar=c(5,5,3,5))
  plot (temperatura_interior_med, temperatura_interior_pred, 
        xlab="Temperatura interior medida [ºC]", 
        ylab="Temperatura interior predicha [ºC]",
        xlim=c(25,29), ylim=c(25,29),
        cex.lab=1.5, cex.axis=1.5)
  abline(a=0, b=1, col="red", lwd=4)
  abline(a=-1, b=1, col="green", lwd=4)
  abline(a=1, b=1, col="green", lwd=4)
  dev.off() 
}

#===============================================================================

#===============================================================================
# Modelo 8
#-------------------------------------------------------------------------------

{
  formula<-paste(regresion_output, " ~ ", sep="")
  
  variables_regresion_input_m<-variables_regresion_input[variables_regresion_input
                                                         !="temperatura_exterior"]
  
  for (i in 1:length(variables_regresion_input_m)) {
    formula<-paste(formula, " ", variables_regresion_input_m[i], sep="") 
    if (i<length(variables_regresion_input_m))
      formula<-paste(formula, " +", sep="") 
  }
  
  for (i in 1:4) {
    nombre_var<-paste(regresion_output, "_", i, sep="")
    formula<-paste(formula, " + ", nombre_var, sep="") 
    
    nombre_var<-paste(variables_regresion_input_m, "_", i, sep="")
    
    for (j in 1:length(nombre_var)) {
      formula<-paste(formula, " + ",nombre_var[j], sep="")
    }
  }
  
  variables_regresion_input_m<-variables_regresion_input_m[variables_regresion_input_m
                                                           !="radiacion_global_fachada"]
  
  for (i in 5:6) {
    nombre_var<-paste(regresion_output, "_", i, sep="")
    formula<-paste(formula, " + ", nombre_var, sep="") 
    
    nombre_var<-paste(variables_regresion_input_m, "_", i, sep="")
    
    for (j in 1:length(nombre_var)) {
      formula<-paste(formula, " + ",nombre_var[j], sep="")
    }
  }
  
  attach (dataframe_trabajo)
  arx<-lm(formula)
  detach(dataframe_trabajo)
  a<-summary(arx)
  
  temperatura_interior_pred<-predict(arx)
  temperatura_interior_med<-
    dataframe_trabajo$temperatura_interior[7:nrow(dataframe_trabajo)]
  
  media<-mean(arx$residuals)
  desv_tipica<-sd(arx$residuals)
  
  intervalo_sup<-temperatura_interior_pred+media+desv_tipica*z
  intervalo_inf<-temperatura_interior_pred+media-desv_tipica*z
  
  plot (temperatura_interior_med, temperatura_interior_pred, 
        xlab="Temperatura interior medida [ºC]", 
        ylab="Temperatura interior predicha [ºC]",
        xlim=c(25,29), ylim=c(25,29))
  abline(a=0, b=1, col="red", lwd=4)
  abline(a=-1, b=1, col="green", lwd=4)
  abline(a=1, b=1, col="green", lwd=4)
  
  png(paste(getwd(),"/plots/temp_interior_med_vs_pred_s8.png",sep=""), width=800, 
      height=800)
  par(mar=c(5,5,3,5))
  plot (temperatura_interior_med, temperatura_interior_pred, 
        xlab="Temperatura interior medida [ºC]", 
        ylab="Temperatura interior predicha [ºC]",
        xlim=c(25,29), ylim=c(25,29),
        cex.lab=1.5, cex.axis=1.5)
  abline(a=0, b=1, col="red", lwd=4)
  abline(a=-1, b=1, col="green", lwd=4)
  abline(a=1, b=1, col="green", lwd=4)
  dev.off()
}

#===============================================================================
