#===============================================================================
#===============================================================================
# Tercer script del PFG "Desarrollo de modelos de caracterización a corto plazo
# de la evolución térmico-energética en edificios" elaborado por el estudiante
# Iñigo García de Eulate Pascual (5ºADE+ITI, Universidad de Deusto) en
# colaboración con el tutor del PFG Roberto Garay Martínez.
#===============================================================================
#===============================================================================

#===============================================================================
#===============================================================================
# En este tercer script se llevará a cabo la mejora de la señal de ocupación por
# conteo.
#===============================================================================
#===============================================================================

#===============================================================================
# Obtención del dataset
#-------------------------------------------------------------------------------

{
  wd<-getwd()
  dataset<-readRDS(paste(wd, "/rds_files/dataset1.rds", sep=""))
}

#===============================================================================

#===============================================================================
# Carga de librerías
#-------------------------------------------------------------------------------

# Librería "zoo" que permite emplear la función rollemean para el cálculo de
# medias móviles.

library(zoo)

#===============================================================================


#===============================================================================
# Mejora de la robustez de la señal de ocupación por conteo
#-------------------------------------------------------------------------------

# Se hacen dos medias móviles centradas:

# 3 elementos
ocupantes_conteo_robus3<-rollmean(dataset$ocupantes_conteo,
                                  k=3,fill=NA,align="center")
# 5 elementos
ocupantes_conteo_robus5<-rollmean(dataset$ocupantes_conteo,
                                  k=5,fill=NA,align="center")

#-------------------------------------------------------------------------------

# Generación de un subset para la marca de tiempo y las medias móviles de
# ocupación

{
  marca_tiempo<-dataset$marca_tiempo
  ocupantes_conteo<-dataset$ocupantes_conteo
  
  subset<-data.frame(marca_tiempo, ocupantes_conteo,
                     ocupantes_conteo_robus3, ocupantes_conteo_robus5)
}

#-------------------------------------------------------------------------------

# Eliminación de los NAs del subset

subset<-na.omit(subset)

#-------------------------------------------------------------------------------

# Cambio del formato de la marca de tiempo para que tenga formato fecha

subset$marca_tiempo<-as.POSIXct(strftime(subset$marca_tiempo, 
                                         format = "%Y-%m-%d %H:%M:%OS"))

#------------------------------------------------------------------------------

# Se grafican para el primer día del que se tiene datos y se decide cual será la
# empleada.

  # Bucles para delimitar en qué filas del dataset comienza y termina el primer
  # día del que se tiene datos.

# Variables auxiliares para los bucles
{
  exit<-0
  i<-1
}

  # Bucle para obtener el número de fila en el que empieza el primer día

{
  while (exit!=1) {
    if (as.numeric(format(subset[i,"marca_tiempo"],"%H"))==00 & 
        as.numeric(format(subset[i,"marca_tiempo"],"%M"))==00) {
      exit<-1
    } else {
      i<-i+1
    }
  }
  comienzo<-i
}

  # Bucle para obtener el número de fila en el que acaba el primer día

{
  exit<-0
  
  while (exit!=1) {
    if (as.numeric(format(subset[i,"marca_tiempo"],"%H"))==23 &
        as.numeric(format(subset[i,"marca_tiempo"],"%M"))==55) {
      exit<-1
    } else {
      i<-i+1
    }
  }
  
  final<-i
}

  # Graficación de ambas medias móviles y de la señal sin media móvil

{
  plot(subset$marca_tiempo[comienzo:final],
       subset$ocupantes_conteo[comienzo:final],
       xlab="Hora", ylab="Número de ocupantes",
       ylim=c(min(dataset$ocupantes_conteo),
              max(dataset$ocupantes_conteo)))
  
  png(paste(wd,"/plots/2_occupation/ocupantes_conteo_primer_dia_postmm.png",sep=""), 
      width=800, height=800)
  par(mar=c(5,5,3,5))
  plot(subset$marca_tiempo[comienzo:final],
       subset$ocupantes_conteo[comienzo:final],
       xlab="Hora", ylab="Número de ocupantes",
       ylim=c(min(dataset$ocupantes_conteo),
              max(dataset$ocupantes_conteo)), cex.axis=1.5, cex.lab=1.5)
  dev.off()
  
  plot(subset$marca_tiempo[comienzo:final],
       subset$ocupantes_conteo_robus3[comienzo:final],
       xlab="Hora", ylab="Número de ocupantes",
       ylim=c(min(dataset$ocupantes_conteo),
              max(dataset$ocupantes_conteo)))
  
  png(paste(wd,"/plots/2_occupation/ocupantes_conteo_robus3_primer_dia.png",sep=""), 
      width=800, height=800)
  par(mar=c(5,5,3,5))
  plot(subset$marca_tiempo[comienzo:final],
       subset$ocupantes_conteo_robus3[comienzo:final],
       xlab="Hora", ylab="Número de ocupantes",
       ylim=c(min(dataset$ocupantes_conteo),
              max(dataset$ocupantes_conteo)), cex.axis=1.5, cex.lab=1.5)
  dev.off()
  
  plot(subset$marca_tiempo[comienzo:final],
       subset$ocupantes_conteo_robus5[comienzo:final],
       xlab="Hora", ylab="Número de ocupantes",
       ylim=c(min(dataset$ocupantes_conteo),
              max(dataset$ocupantes_conteo)))
  
  png(paste(wd,"/plots/2_occupation/ocupantes_conteo_robus5_primer_dia.png",sep=""),
      width=800, height=800)
  par(mar=c(5,5,3,5))
  plot(subset$marca_tiempo[comienzo:final],
       subset$ocupantes_conteo_robus5[comienzo:final],
       xlab="Hora", ylab="Número de ocupantes",
       ylim=c(min(dataset$ocupantes_conteo),
              max(dataset$ocupantes_conteo)), cex.axis=1.5, cex.lab=1.5)
  dev.off() 
}

#-------------------------------------------------------------------------------

# Se elige la media móvil centrada de 3 elementos y se añade al dataset original

dataset<-cbind(dataset, ocupantes_conteo_robus3)

#-------------------------------------------------------------------------------

# Eliminación de los NAs del dataset

dataset<-na.omit(dataset)

#-------------------------------------------------------------------------------

# Exportar dataset

{
  saveRDS(dataset, paste(wd, "/rds_files/dataset2.rds", sep=""))
  
  write.csv2(dataset, paste(wd, "/datasets/room4_2.csv", sep=""), 
             row.names=FALSE) 
}

#-------------------------------------------------------------------------------

# Limpieza de variables

rm(i, exit, subset, marca_tiempo, ocupantes_conteo, ocupantes_conteo_robus3,
   comienzo, final, ocupantes_conteo_robus5)

#===============================================================================