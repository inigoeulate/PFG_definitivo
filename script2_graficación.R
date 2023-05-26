#===============================================================================
#===============================================================================
# Segundo script del PFG "Desarrollo de modelos de caracterización a corto plazo
# de la evolución térmico-energética en edificios" elaborado por el estudiante
# Iñigo García de Eulate Pascual (5ºADE+ITI, Universidad de Deusto) en
# colaboración con el tutor del PFG Roberto Garay Martínez.
#===============================================================================
#===============================================================================

#===============================================================================
#===============================================================================
# En este segundo script se llevará a cabo la graficación de variables
# relevantes.
#===============================================================================
#===============================================================================

#===============================================================================
# Obtención del dataset
#-------------------------------------------------------------------------------

dataset<-readRDS(paste(wd, "/rds_files/dataset.rds", sep=""))

#===============================================================================

#===============================================================================
# Graficación de las señales de ocupación
#-------------------------------------------------------------------------------

# Bucle para obtener el número de fila en el que acaba el primer día y creación
# del datafrmae con los datos de ese día

{
  exit<-0
  i<-1
  while (exit!=1) {
    if (as.numeric(format(dataset[i,"marca_tiempo"],"%H"))==23 &
        as.numeric(format(dataset[i,"marca_tiempo"],"%M"))==55) {
      exit<-1
    } else {
      i<-i+1
    }
  }
  
  subset<-dataset[1:i,]
}

#-------------------------------------------------------------------------------

# Graficación de las señales de ocupación

{
  plot(subset$marca_tiempo, subset$ocupantes_presencia, 
       xlab="Hora", ylab="Presencia de ocupantes")
  
  png(paste(wd,"/plots/ocupantes_presencia_primer_dia.png",sep=""), 
      width=800, height=800)
  plot(subset$marca_tiempo, subset$ocupantes_presencia, 
       xlab="Hora", ylab="Presencia de ocupantes")
  dev.off()
  
  plot(subset$marca_tiempo, subset$ocupantes_conteo, 
       xlab="Hora", ylab="Presencia de ocupantes",
       ylim=c(min(dataset$ocupantes_conteo),
              max(dataset$ocupantes_conteo)))
  
  png(paste(wd,"/plots/ocupantes_conteo_primer_dia.png",sep=""), 
      width=800, height=800)
  plot(subset$marca_tiempo, subset$ocupantes_conteo, 
       xlab="Hora", ylab="Presencia de ocupantes",
       ylim=c(min(dataset$ocupantes_conteo),
              max(dataset$ocupantes_conteo)))
  dev.off()
}

#-------------------------------------------------------------------------------

# Limpieza de variables

rm(i,exit, subset)

#===============================================================================

#===============================================================================
# Graficación de las siguientes señales para los dos primeros días de los que se
# tienen datos completos:
# - Temperatura interior
# - Temperatura exterior
# - Energía de agua refrigerada
# - Radiación_solar_global_horizontal
#-------------------------------------------------------------------------------

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
    if (as.numeric(format(dataset[i,"marca_tiempo"],"%H"))==00 & 
        as.numeric(format(dataset[i,"marca_tiempo"],"%M"))==00) {
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
    if (as.numeric(format(dataset[i,"marca_tiempo"],"%H"))==23 &
        as.numeric(format(dataset[i,"marca_tiempo"],"%M"))==55) {
      exit<-1
    } else {
      i<-i+1
    }
  }
  final<-i
}

#-------------------------------------------------------------------------------

# Graficación del primer día

{
  plot(dataset$marca_tiempo[comienzo:final], 
       dataset$temperatura_interior[comienzo:final],
       xlab="Hora", ylab="Temperatura interior [ºC]",
       ylim=c(min(dataset$temperatura_interior),
              max(dataset$temperatura_interior)))
  
  png(paste(wd,"/plots/graf_temperatura_interior1.png",sep=""), 
      width=800, height=800)
  plot(dataset$marca_tiempo[comienzo:final], 
       dataset$temperatura_interior[comienzo:final],
       xlab="Hora", ylab="Temperatura interior [ºC]",
       ylim=c(min(dataset$temperatura_interior),
              max(dataset$temperatura_interior)))
  dev.off()
  
  plot(dataset$marca_tiempo[comienzo:final], 
       dataset$temperatura_exterior[comienzo:final],
       xlab="Hora", ylab="Temperatura exterior [ºC]",
       ylim=c(min(dataset$temperatura_exterior),
              max(dataset$temperatura_exterior)))
  
  png(paste(wd,"/plots/graf_temperatura_exterior1.png",sep=""), 
      width=800, height=800)
  plot(dataset$marca_tiempo[comienzo:final], 
       dataset$temperatura_exterior[comienzo:final],
       xlab="Hora", ylab="Temperatura exterior [ºC]",
       ylim=c(min(dataset$temperatura_exterior),
              max(dataset$temperatura_exterior)))
  dev.off()
  
  plot(dataset$marca_tiempo[comienzo:final], 
       dataset$energia_agua_refrigerada[comienzo:final],
       xlab="Hora", ylab="Energía agua refrigerada [kWh]",
       ylim=c(min(dataset$energia_agua_refrigerada),
              max(dataset$energia_agua_refrigerada)))
  
  png(paste(wd,"/plots/graf_energia_agua_refrigerada1.png",sep=""), 
      width=800, height=800)
  plot(dataset$marca_tiempo[comienzo:final], 
       dataset$energia_agua_refrigerada[comienzo:final],
       xlab="Hora", ylab="Energía agua refrigerada [kWh]",
       ylim=c(min(dataset$energia_agua_refrigerada),
              max(dataset$energia_agua_refrigerada)))
  dev.off()
  
  plot(dataset$marca_tiempo[comienzo:final], 
       dataset$radiacion_solar_global_horizontal[comienzo:final],
       xlab="Hora", ylab="Radiación solar global horizontal [W/m2]",
       ylim=c(min(dataset$radiacion_solar_global_horizontal),
              max(dataset$radiacion_solar_global_horizontal)))
  
  png(paste(wd,"/plots/graf_radiacion_solar_global_horizontal1.png",sep=""), 
      width=800, height=800)
  plot(dataset$marca_tiempo[comienzo:final], 
       dataset$radiacion_solar_global_horizontal[comienzo:final],
       xlab="Hora", ylab="Radiación solar global horizontal [W/m2]",
       ylim=c(min(dataset$radiacion_solar_global_horizontal),
              max(dataset$radiacion_solar_global_horizontal)))
  dev.off() 
}

#-------------------------------------------------------------------------------

# Graficación del segundo día

{
  diferencia<-final-comienzo
  comienzo<-comienzo+diferencia+1
  final<-final+diferencia+1
  
  plot(dataset$marca_tiempo[comienzo:final], 
       dataset$temperatura_interior[comienzo:final],
       xlab="Hora", ylab="Temperatura interior [ºC]",
       ylim=c(min(dataset$temperatura_interior),
              max(dataset$temperatura_interior)))
  
  png(paste(wd,"/plots/graf_temperatura_interior2.png",sep=""), 
      width=800, height=800)
  plot(dataset$marca_tiempo[comienzo:final], 
       dataset$temperatura_interior[comienzo:final],
       xlab="Hora", ylab="Temperatura interior [ºC]",
       ylim=c(min(dataset$temperatura_interior),
              max(dataset$temperatura_interior)))
  dev.off()
  
  plot(dataset$marca_tiempo[comienzo:final], 
       dataset$temperatura_exterior[comienzo:final],
       xlab="Hora", ylab="Temperatura exterior [ºC]",
       ylim=c(min(dataset$temperatura_exterior),
              max(dataset$temperatura_exterior)))
  
  png(paste(wd,"/plots/graf_temperatura_exterior2.png",sep=""), 
      width=800, height=800)
  plot(dataset$marca_tiempo[comienzo:final], 
       dataset$temperatura_exterior[comienzo:final],
       xlab="Hora", ylab="Temperatura exterior [ºC]",
       ylim=c(min(dataset$temperatura_exterior),
              max(dataset$temperatura_exterior)))
  dev.off()
  
  plot(dataset$marca_tiempo[comienzo:final], 
       dataset$energia_agua_refrigerada[comienzo:final],
       xlab="Hora", ylab="Energía agua refrigerada [kWh]",
       ylim=c(min(dataset$energia_agua_refrigerada),
              max(dataset$energia_agua_refrigerada)))
  
  png(paste(wd,"/plots/graf_energia_agua_refrigerada2.png",sep=""), 
      width=800, height=800)
  plot(dataset$marca_tiempo[comienzo:final], 
       dataset$energia_agua_refrigerada[comienzo:final],
       xlab="Hora", ylab="Energía agua refrigerada [kWh]",
       ylim=c(min(dataset$energia_agua_refrigerada),
              max(dataset$energia_agua_refrigerada)))
  dev.off()
  
  plot(dataset$marca_tiempo[comienzo:final], 
       dataset$radiacion_solar_global_horizontal[comienzo:final],
       xlab="Hora", ylab="Radiación solar global horizontal [W/m2]",
       ylim=c(min(dataset$radiacion_solar_global_horizontal),
              max(dataset$radiacion_solar_global_horizontal)))
  
  png(paste(wd,"/plots/graf_radiacion_solar_global_horizontal2.png",sep=""), 
      width=800, height=800)
  plot(dataset$marca_tiempo[comienzo:final], 
       dataset$radiacion_solar_global_horizontal[comienzo:final],
       xlab="Hora", ylab="Radiación solar global horizontal [W/m2]",
       ylim=c(min(dataset$radiacion_solar_global_horizontal),
              max(dataset$radiacion_solar_global_horizontal)))
  dev.off() 
}

#-------------------------------------------------------------------------------

# Limpieza de variables

rm(comienzo, final, diferencia, exit, i)

#===============================================================================