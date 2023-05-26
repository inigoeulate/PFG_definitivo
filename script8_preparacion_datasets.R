#===============================================================================
#===============================================================================
# Octavo script del PFG "Desarrollo de modelos de caracterización a corto plazo
# de la evolución térmico-energética en edificios" elaborado por el estudiante
# Iñigo García de Eulate Pascual (5ºADE+ITI, Universidad de Deusto) en
# colaboración con el tutor del PFG Roberto Garay Martínez.
#===============================================================================
#===============================================================================

#===============================================================================
#===============================================================================
# En este octavo script se llevará a cabo la preparación de los datasets para 
# los modelos ARX que predecirán la temperatura interior.
#===============================================================================
#===============================================================================

#===============================================================================
# Obtención del dataset
#-------------------------------------------------------------------------------

dataset<-readRDS(paste(wd, "/rds_files/dataset.rds", sep=""))

#===============================================================================

#===============================================================================
# Resampleo del dataset para tener datos cada 15, 30 y 60 minutos
#-------------------------------------------------------------------------------

# Creación de un dataframe que contenga únicamente las variables que se van a
# emplear para las regresiones (o por lo menos para la preparación del dataset
# previo a las regresiones)

{
  variables<-c("marca_tiempo",
               "hora_solar",
               "ocupantes_conteo_robus3",
               "temperatura_exterior",
               "temperatura_interior",
               "energia_agua_refrigerada",
               "radiacion_global_fachada")
  
  dataframe<-data.frame(matrix(ncol=length(variables), nrow=nrow(dataset)))
  names(dataframe)<-variables
  
  for(i in 1:length(dataframe)){
    for(j in 1:length(dataset)){
      if (names(dataset[j])==names(dataframe[i])){
        dataframe[,i]<-dataset[,j]
      }
    }
  }
}

#-------------------------------------------------------------------------------

# Creación de los diferentes dataframes en función del tiempo de sampleo deseado
# Se calculan medias móviles

{
  sampleo<-c(15,30,60) # Introducir en este vector los tiempos de sampleo deseados
  
  for (i in 1:length(sampleo)){
    
    n_elementos<-sampleo[i]/5
    
    dataframe_trabajo<-dataframe
    
    for (j in 1:length(dataframe_trabajo)){
      if (names(dataframe[j])!="marca_tiempo" & 
          names(dataframe[j])!="hora_solar"){
        dataframe_trabajo[, j]<-rollmean(dataframe_trabajo[, j], k=n_elementos, 
                                         fill=NA, align="center") 
      }
    }
    
    dataframe_trabajo<-na.omit(dataframe_trabajo) 
    # Quitar los NA que salen al hacer la media móvil
    
    dataframe_trabajo<-subset(dataframe_trabajo,
                              as.numeric(format(dataframe_trabajo$marca_tiempo,
                                                "%M")) %% sampleo[i] ==0)
    
    nombre_dataframe<-paste("dataframe", sampleo[i], sep="_")
    assign(nombre_dataframe, dataframe_trabajo)
  }
}

#-------------------------------------------------------------------------------

# Limpieza de variables

rm(dataframe_trabajo, dataset, i, j, n_elementos, nombre_dataframe, variables)

#===============================================================================

#===============================================================================
# Preparación de los dataframes para los modelos ARX añadiendo variables que
# recojan las observaciones anteriores (hasta 6 horas) para cada variable
#-------------------------------------------------------------------------------

for (i in 1:length(sampleo)){
  nombre_dataframe<-paste("dataframe", sampleo[i], sep="_")
  dataframe_trabajo<-get(nombre_dataframe)
  
  horas_anteriores<-6
  
  obs_anteriores<-horas_anteriores*60/sampleo[i]
  
  for (k in 1:length(dataframe_trabajo)){
    for (j in 1:obs_anteriores){
      dataframe_trabajo[[paste(names(dataframe_trabajo[k]),j,sep="_")]]<-
        filter(dataframe_trabajo[,k], c(rep(0,j),1), sides=1)
    }
  }
  
  nombre_dataframe<-paste("dataframe", sampleo[i], sep="_")
  assign(nombre_dataframe, dataframe_trabajo) 
}

#-------------------------------------------------------------------------------

# Exportar datasets

for (i in 1:length(sampleo)){
  nombre_dataframe<-paste("dataframe", sampleo[i], sep="_")
  dataframe_trabajo<-get(nombre_dataframe)
  
  saveRDS(dataframe_trabajo, paste(wd, "/rds_files/", nombre_dataframe, ".rds", 
                                   sep=""))
  
  write.csv2(dataframe_trabajo, 
             paste(wd, "/datasets/", nombre_dataframe, ".csv", sep=""), 
             row.names=FALSE) 
}

#-------------------------------------------------------------------------------

# Limpieza de variables

rm(i, j, k, nombre_dataframe, obs_anteriores, dataframe_trabajo, dataframe_15,
   dataframe_30, dataframe_60, sampleo, dataframe, horas_anteriores)

#===============================================================================