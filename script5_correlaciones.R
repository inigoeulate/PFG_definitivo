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
# Obtención del dataset
#-------------------------------------------------------------------------------

dataset<-readRDS(paste(wd, "/rds_files/dataset.rds", sep=""))

#===============================================================================

#===============================================================================
# Correlaciones entre variables relevantes
#-------------------------------------------------------------------------------

# Correlaciones entre las variables:
  # a. Temperatura ambiental exterior, temperatura ambiental interior y número
  #    de personas
  # b. Temperatura ambiental exterior, radiación solar, número de personas y
  #    consumo de climatización
  # c. Temperatura ambiental exterior, radiación solar, hora solar y consumo de
  #    climatización

{
  hora<-hour(dataset$hora_solar)
  dataset<-cbind(dataset, hora)
  
  nombres<-names(dataset)
  nombres
  
  # a. Temperatura ambiental exterior, temperatura ambiental interior y número de
  # personas
  
  # Coeficientes de correlación de Pearson
  matrizcorrel1<-(cor(dataset[,nombres %in% nombres[c(28,5,36)]]))
  write.csv2(matrizcorrel1, 
             paste(wd,"/correlations/matriz_correlaciones1.csv", sep=""))
  
  # Graficación de las señales, unas con respecto a otras
  plot(dataset[,nombres %in% nombres[c(28,5,36)]])
  png(filename=paste(wd,"/correlations/correlaciones1.png", sep=""), 
      width=800, height=800)
  plot(dataset[,nombres %in% nombres[c(28,5,36)]])
  dev.off()
  
  # b. Temperatura ambiental exterior, radiación solar, número de personas y
  # consumo de climatización
  
  # Coeficientes de correlación de Pearson
  matrizcorrel2<-(cor(dataset[,nombres %in% nombres[c(28,29,36,13)]]))
  write.csv2(matrizcorrel2, 
             paste(wd,"/correlations/matriz_correlaciones2.csv", sep=""))
  
  # Graficación de las señales, unas con respecto a otras
  plot(dataset[,nombres %in% nombres[c(28,29,36,13)]])
  png(filename=paste(wd,"/correlations/correlaciones2.png", sep=""), 
      width=800, height=800)
  plot(dataset[,nombres %in% nombres[c(28,29,36,13)]])
  dev.off()
  
  # c. Temperatura ambiental exterior, radiación solar, hora solar y consumo de
  # climatización
  
  # Coeficientes de correlación de Pearson
  matrizcorrel3<-(cor(dataset[,nombres %in% nombres[c(28,29,38,13)]]))
  write.csv2(matrizcorrel3, 
             paste(wd,"/correlations/matriz_correlaciones3.csv", sep=""))
  
  # Graficación de las señales, unas con respecto a otras
  plot(dataset[,nombres %in% nombres[c(28,29,38,13)]])
  png(filename=paste(wd,"/correlations/correlaciones3.png", sep=""), 
      width=800, height=800)
  plot(dataset[,nombres %in% nombres[c(28,29,38,13)]])
  dev.off()
}

#-------------------------------------------------------------------------------

# Eliminar del dataset la variable "hora" creada para el cálculo de las
# correlaciones

dataset<-subset(dataset, select=-hora)

#-------------------------------------------------------------------------------

# Limpiado de variables

rm(matrizcorrel1, matrizcorrel2, matrizcorrel3, hora, nombres)

#===============================================================================