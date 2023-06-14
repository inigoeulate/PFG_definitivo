#===============================================================================
#===============================================================================
# Cuarto script del PFG "Desarrollo de modelos de caracterización a corto plazo
# de la evolución térmico-energética en edificios" elaborado por el estudiante
# Iñigo García de Eulate Pascual (5ºADE+ITI, Universidad de Deusto) en
# colaboración con el tutor del PFG Roberto Garay Martínez.
#===============================================================================
#===============================================================================

#===============================================================================
#===============================================================================
# En este cuarto script se llevará a cabo el cálculo de la hora solar.
#===============================================================================
#===============================================================================

#===============================================================================
# Obtención del dataset
#-------------------------------------------------------------------------------

{
  wd<-getwd()
  dataset<-readRDS(paste(wd, "/rds_files/dataset2.rds", sep=""))
}

#===============================================================================

#===============================================================================
# Carga de librerías
#-------------------------------------------------------------------------------

# Librería "lubridate" que introduce funciones para facilitar el manejo de
# fechas.

library(lubridate)

#===============================================================================

#===============================================================================
# Cálculo de la hora solar
#-------------------------------------------------------------------------------

# A partir de las ecuaciones propuestas en:
# Duffie, J. A., & Beckman, W. A. (2013). Solar Engineering of Thermal Processes
# (4th ed.). Wiley

# La diferencia entre la hora solar y la estándar se calcula como:
# hora solar-hora estándar = 4·(L_st - L_loc) + E

# En esta expresión:
  # L_st es el meridiano estándar para la zona horaria local.
  # L_loc es la longitud de la localización en cuestión.
  # E es la ecuación del tiempo en minutos.

# La ecuación del tiempo en minutos se define como:
  # E=229.2·(0.000075+0.001868·cos⁡B-0.032077·sin⁡B-0.014615·cos⁡2B-0.04089·sin⁡2B )
    # B=(n-1)·360/365
      # 1≤n≤365

#En estas expresiones:
  # n es el día del año.
  # Las unidades de B son grados angulares.

# Los husos horarios están centrados en meridianos de longitudes múltiplos de 15°
# El huso horario conocido como UTC+0 (Coordinated Universal Time) está centrado
# en el meridiano de Greenwich, cuya longitud es 0.
# El uso horario GMT+1 está centrado en el meridiano de longitud 15°,
# y así sucesivamente. 

#El edificio que estamos estudiando se encuentra en Singapur. 
  # Uso horario: UTC+8.
  # Singapur no cambia de hora con motivo de las estaciones.
  # Coordenadas del edificio: 1.28967°N, 103.85007°E.

#-------------------------------------------------------------------------------

# Se entiende que hay una errata en las ecuaciones propuestas por los autores,
# dado que entendiendo L_loc y L_st como se recogen en Solar Engineering of
# Thermal Processes (4th ed.), la diferencia queda de signo contrario al que
# debería.

# Por lo tanto, se considera:
  # L_st: longitud de la localización en cuestión.
  # L_loc: longitud meridiano estándar para la zona horaria local.

{
  L_loc<-8*15
  L_st<-103.850077
  
  B<-(as.numeric(strftime(dataset$marca_tiempo,format="%j"))-1)*360/365
  
  E<-229.2*(0.000075+
              0.001868*cos(B*pi/180)-
              0.032077*sin(B*pi/180)-
              0.014615*cos(2*B*pi/180)-
              0.04089*sin(2*B*pi/180))
  
  hora_solar<-dataset$marca_tiempo + seconds_to_period(60*(4*(L_st-L_loc)+E))
}

#-------------------------------------------------------------------------------

# Se añade la variable creada para la hora solar al dataset

dataset<-cbind(dataset, hora_solar)

#-------------------------------------------------------------------------------

# Exportar dataset

{
  saveRDS(dataset, paste(wd, "/rds_files/dataset3.rds", sep=""))
  
  write.csv2(dataset, paste(wd, "/datasets/room4_3.csv", sep=""), 
             row.names=FALSE) 
}

#-------------------------------------------------------------------------------

# Limpieza de variables

rm(B, E, hora_solar, L_loc, L_st)

#===============================================================================