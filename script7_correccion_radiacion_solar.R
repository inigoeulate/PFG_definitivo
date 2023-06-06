#===============================================================================
#===============================================================================
# Séptimo script del PFG "Desarrollo de modelos de caracterización a corto plazo
# de la evolución térmico-energética en edificios" elaborado por el estudiante
# Iñigo García de Eulate Pascual (5ºADE+ITI, Universidad de Deusto) en
# colaboración con el tutor del PFG Roberto Garay Martínez.
#===============================================================================
#===============================================================================

#===============================================================================
#===============================================================================
# En este séptimo script se llevará a cabo la corrección de la radiación solar.
#===============================================================================
#===============================================================================

#===============================================================================
# Obtención del dataset
#-------------------------------------------------------------------------------

dataset<-readRDS(paste(wd, "/rds_files/dataset.rds", sep=""))

#===============================================================================

#===============================================================================
# Corrección de la radiación solar
#-------------------------------------------------------------------------------

# Diferenciación entre radiación disufa y directa
# Se eligen 100 W*m^2 como límite de la radiación difusa

{
  radiacion_difusa<-0
  for (i in 1:nrow(dataset)){
    if (dataset$radiacion_solar_global_horizontal[i]<100){
      radiacion_difusa[i]<-dataset$radiacion_solar_global_horizontal[i]
    }else{
      radiacion_difusa[i]<-100
    }
  }
  
  radiacion_directa_horizontal<-dataset$radiacion_solar_global_horizontal-
    radiacion_difusa
}

#-------------------------------------------------------------------------------

# Conversión de radiación solar sobre plano horizontal a sobre plano
# perpendicular a la incidencia solar

radiacion_directa_perpendicular<-radiacion_directa_horizontal*
  sin(dataset$altura*pi/180)

#-------------------------------------------------------------------------------

# Cálculo del acimut aparente sobre una superficie vertical arbitraria

{
  acimut_plano<-20 # obtenido de la vista satélite
  
  acimut_aparente<-dataset$acimut-acimut_plano
}

#-------------------------------------------------------------------------------

# Conversión de la radiación solar sobre plano perpendicular a la incidencia
# solar a radiación solar incidente sobre fachada

radiacion_directa_fachada<-radiacion_directa_perpendicular*
  cos(dataset$altura*pi/180)*cos(acimut_aparente*pi/180)

# Verificación:
  # si el sol ve la fachada (azimuth aparente = [-90º,90º]) -> radiación directa
  # sino, radiación directa = 0

for (i in 1:nrow(dataset)){
  if (acimut_aparente[i]>90 | acimut_aparente[i]< -90) {
    radiacion_directa_fachada[i]<-0
  }
}

#-------------------------------------------------------------------------------

# Generación de una nueva varaible de radiación global sobre fachada de estudio

radiacion_global_fachada<-radiacion_directa_fachada+radiacion_difusa

#-------------------------------------------------------------------------------

# Se añaden las variables de radiación solar al dataset

dataset<-cbind(dataset, radiacion_global_fachada, radiacion_difusa, 
               radiacion_directa_fachada)

#-------------------------------------------------------------------------------

# Generación de un  dataset con todas las variables solares

{
  marca_tiempo<-dataset$marca_tiempo
  hora_solar<-dataset$hora_solar
  acimut<-dataset$acimut
  altura<-dataset$altura
  cenit<-dataset$cenit
  radiacion_solar_global_horizontal<-dataset$radiacion_solar_global_horizontal
  
  dataset_solar<-data.frame(marca_tiempo, hora_solar,
                            acimut, altura, cenit,
                            radiacion_solar_global_horizontal,
                            radiacion_difusa, radiacion_directa_horizontal,
                            radiacion_directa_perpendicular,
                            radiacion_directa_fachada, radiacion_global_fachada)
  
  dataset_solar$marca_tiempo<-as.character(dataset_solar$marca_tiempo)
  dataset_solar$hora_solar<-as.character(dataset_solar$hora_solar)
  
  write.csv2(dataset_solar, paste(wd,"/datasets/room4_var_solar.csv",sep=""), 
             row.names=FALSE) 
}

#-------------------------------------------------------------------------------

# Exportar dataset

{
  saveRDS(dataset, paste(wd, "/rds_files/dataset.rds", sep=""))
  
  write.csv2(dataset, paste(wd, "/datasets/room4_5.csv", sep=""), 
             row.names=FALSE) 
}

#-------------------------------------------------------------------------------

# Correlaciones tras el ajuste de la radiación solar

{
  nombres<-names(dataset)
  nombres
  
  # Coeficientes de correlación de Pearson
  matrizcorrel<-(cor(dataset[,nombres %in% nombres[c(5, 29, 43, 41)]], 
                     method="pearson"))
  write.csv2(matrizcorrel, 
             paste(wd,"/correlations/matriz_correlaciones_solar.csv", sep=""))
  
  # Graficación de las señales, unas con respecto a otras
  plot(dataset[,nombres %in% nombres[c(5, 29, 43, 41)]])
  
  png(filename=paste(wd,"/correlations/correlaciones_solar.png", sep=""), 
      width=800, height=800)
  plot(dataset[,nombres %in% nombres[c(5, 29, 43, 41)]])
  dev.off()
}

#-------------------------------------------------------------------------------

# Graficación de las variables de radiación solar para dos días:
  # Uno con alta radiación solar
  # Uno con baja radiación solar

dataset$hora_solar<-as.POSIXct(strftime(dataset$hora_solar, 
                                        format = "%Y-%m-%d %H:%M:%OS"))

# plot(dataset$radiacion_solar_global_horizontal, xlim=c(0,2000))
# Día con mucha radiación solar: 2021/09/10
# Dia con poca radiación solar: 2021/09/13

# Bucles para delimitar los días y graficación
  # Día con mucha radiación solar: 2021/09/10

{
  exit<-0
  i<-1
  j<-1
}

    # Bucle para obtener el número de fila en el que empieza el día 2021/09/10
{
  while (exit!=1) {
    if (as.numeric(format(dataset[i,"hora_solar"],"%d"))==10 & 
        as.numeric(format(dataset[i,"hora_solar"],"%m"))==9 & 
        as.numeric(format(dataset[i,"hora_solar"],"%H"))==00) {
      exit<-1
    } else {
      i<-i+1
    }
  }
  
  comienzo<-i
  
  exit<-0 
}

    # Bucle para obtener el número de fila en el que acaba el día 2021/09/10
{
  while (exit!=1) {
    if (as.numeric(format(dataset[i,"hora_solar"],"%d"))!=10) {
      exit<-1
    } else {
      i<-i+1
    }
  }
  
  final<-i-1
  
  dia10<-dataset[comienzo:final,]
}

    # Generación del archivo PNG con las gráficas
{
  png(paste(wd,"/plots/dia_alta_radiacion.png",sep=""), width=800, height=800)
  plot(dia10$hora_solar, dia10$radiacion_solar_global_horizontal,
       xlab="Hora solar", ylab="Radiación [W/m2]",col="blue",cex=1.5,
       ylim=c(min(dia10$radiacion_solar_global_horizontal),
              max(dia10$radiacion_solar_global_horizontal)))
  points(dia10$hora_solar, dia10$radiacion_difusa, col="yellow", cex=1.5)
  points(dia10$hora_solar, dia10$radiacion_directa_fachada, col="orange", 
         cex=1.5)
  points(dia10$hora_solar, dia10$radiacion_global_fachada, col="red", 
         cex=1.5)
  legend(x="topleft", legend=c("Radiación solar global horizontal", 
                               "Radiación solar difusa",
                               "Radiación solar directa sobre fachada",
                               "Radiación solar global sobre fachada"),
         col=c("blue", "yellow", "orange", "red"),
         lty=c(1, 1, 1, 1),
         cex=1.4)
  dev.off()
}

  # Día con poca radiación solar: 2021/09/13
{
  exit<-0
  i<-1
  j<-1
}

    # Bucle para obtener el número de fila en el que empieza el día 2021/09/13
{
  while (exit!=1) {
    if (as.numeric(format(dataset[i,"hora_solar"],"%d"))==13 & 
        as.numeric(format(dataset[i,"hora_solar"],"%m"))==9 & 
        as.numeric(format(dataset[i,"hora_solar"],"%H"))==00) {
      exit<-1
    } else {
      i<-i+1
    }
  }
  
  comienzo<-i
  
  exit<-0
}

    # Bucle para obtener el número de fila en el que acaba el día 2021/09/13
{
  while (exit!=1) {
    if (as.numeric(format(dataset[i,"hora_solar"],"%d"))!=13) {
      exit<-1
    } else {
      i<-i+1
    }
  }
  
  final<-i-1
}

dia13<-dataset[comienzo:final,]

    # Generación del archivo PNG con las gráficas

{
  png(paste(getwd(),"/plots/dia_baja_radiacion.png",sep=""), width=800, 
      height=800)
  plot(dia13$hora_solar, dia13$radiacion_solar_global_horizontal, 
       xlab="Hora solar", ylab="Radiación [W/m2]",col="blue",cex=1.5,
       ylim=c(min(dia10$radiacion_solar_global_horizontal),
              max(dia10$radiacion_solar_global_horizontal)))
  points(dia13$hora_solar, dia13$radiacion_difusa, col="yellow", cex=1.5)
  points(dia13$hora_solar, dia13$radiacion_directa_fachada, col="orange", 
         cex=1.5)
  points(dia13$hora_solar, dia13$radiacion_global_fachada, col="red", 
         cex=1.5)
  legend(x="topleft", legend=c("Radiación solar global horizontal", 
                               "Radiación solar difusa",
                               "Radiación solar directa sobre fachada",
                               "Radiación solar global sobre fachada"),
         col=c("blue", "yellow", "orange", "red"),
         lty=c(1, 1, 1, 1),
         cex=1.4)
  dev.off()
}

#-------------------------------------------------------------------------------

# Limpieza de variables

rm(acimut, acimut_aparente, acimut_plano, altura, cenit, comienzo, final, exit,
   i, j, hora_solar, marca_tiempo, radiacion_difusa, radiacion_directa_fachada,
   radiacion_directa_horizontal, radiacion_directa_perpendicular, 
   radiacion_global_fachada, radiacion_solar_global_horizontal, dia10, dia13,
   dataset_solar, nombres, matrizcorrel)

#===============================================================================