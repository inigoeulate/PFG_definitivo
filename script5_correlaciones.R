#===============================================================================
#===============================================================================
# Quinto script del PFG "Desarrollo de modelos de caracterización a corto plazo
# de la evolución térmico-energética en edificios" elaborado por el estudiante
# Iñigo García de Eulate Pascual (5ºADE+ITI, Universidad de Deusto) en
# colaboración con el tutor del PFG Roberto Garay Martínez.
#===============================================================================
#===============================================================================

#===============================================================================
#===============================================================================
# En este quinta script se llevará a cabo el cálculo de las correlaciones entre
# diferentes variables que se consideran relevantes.
#===============================================================================
#===============================================================================

#===============================================================================
# Carga de librerías
#-------------------------------------------------------------------------------

# Librería "lubridate" que introduce funciones para facilitar el manejo de
# fechas.

library(lubridate)

#===============================================================================

#===============================================================================
# Obtención del dataset
#-------------------------------------------------------------------------------

dataset<-readRDS(paste(wd, "/rds_files/dataset.rds", sep=""))

#===============================================================================

#===============================================================================
# Correlaciones entre variables relevantes
#-------------------------------------------------------------------------------

# Correlaciones entre las variables:
  # - Temperatura interior
  # - Temperatura exterior
  # - Radiación solar global horizontal
  # - Ocupación por conteo
  # - Energía agua refrigerada

{
  nombres<-names(dataset)
  nombres
  
  # Coeficientes de correlación de Pearson
  matrizcorrel<-(cor(dataset[,nombres %in% nombres[c(5, 28, 29, 36, 13)]], 
                     method="pearson"))
  write.csv2(matrizcorrel, 
             paste(wd,"/correlations/matriz_correlaciones.csv", sep=""))
  
  # Graficación de las señales, unas con respecto a otras
  plot(dataset[,nombres %in% nombres[c(5, 28, 29, 36, 13)]])
  
  png(filename=paste(wd,"/correlations/correlaciones.png", sep=""), 
      width=800, height=800)
  plot(dataset[,nombres %in% nombres[c(5, 28, 29, 36, 13)]])
  dev.off()
}

#-------------------------------------------------------------------------------

# Limpiado de variables

rm(matrizcorrel, nombres)

#===============================================================================