#===============================================================================
#===============================================================================
# Primer script del PFG "Desarrollo de modelos de caracterización a corto plazo
# de la evolución térmico-energética en edificios" elaborado por el estudiante
# Iñigo García de Eulate Pascual (5ºADE+ITI, Universidad de Deusto) en
# colaboración con el tutor del PFG Roberto Garay Martínez.
#===============================================================================
#===============================================================================

#===============================================================================
#===============================================================================
# En este primer script se llevará a cabo el preprocesado del dataset.
#===============================================================================
#===============================================================================

#===============================================================================
#===============================================================================
# Antes de comenzar con la programación como tal, hay que dejar claro que el
# presente código se ha realizado para la habitación 4 del dataset.
# Para extender el estudio al resto de habitaciones del dataset bastaría con
# cambiar el número de la habitación en la variable "file" a continuación.
#===============================================================================
#===============================================================================

#===============================================================================
# Obtención del archivo .csv con los datos
#-------------------------------------------------------------------------------

{
  file<-"/data/combined_Room4_modif.csv"
  wd<-getwd()
  ruta<-paste(wd,file,sep="")
  dataset<-read.csv2(ruta)
  #View(dataset)
  
  rm(ruta, file)
}

#===============================================================================

#===============================================================================
# Preprocesado del dataset
#-------------------------------------------------------------------------------

# Eliminación de NAs.

dataset<-na.omit(dataset)

#-------------------------------------------------------------------------------

# Cambio en los nombres de las variables para que tengan nombres más
# autoexplicativos y fáciles de emplear.

{
  names(dataset)[1]<-"marca_tiempo"
  names(dataset)[2]<-"voc"
  names(dataset)[3]<-"presion sonora"
  names(dataset)[4]<-"humedad_relativa_interior"
  names(dataset)[5]<-"temperatura_interior"
  names(dataset)[6]<-"iluminancia_interior"
  names(dataset)[7]<-"pm2.5"
  names(dataset)[8]<-"co2_interior"
  names(dataset)[9]<-"dispositivos_contectados_wifi"
  names(dataset)[10]<-"energia_ventilador_techo"
  names(dataset)[11]<-"energia_iluminacion"
  names(dataset)[12]<-"energia_enchufes"
  names(dataset)[13]<-"energia_agua_refrigerada"
  names(dataset)[14]<-"energia_ventilador_ahu"
  names(dataset)[15]<-"suministro_aire"
  names(dataset)[16]<-"posicion_damper"
  names(dataset)[17]<-"setpoint_temperatura"
  names(dataset)[18]<-"posicion_valvula_bobina_enfriamiento"
  names(dataset)[19]<-"comando_valvula_bobina_enfriamiento"
  names(dataset)[20]<-"velocidad_ventilador_ahu"
  names(dataset)[21]<-"temperatura_ambiental_offcoil"
  names(dataset)[22]<-"setpoint_temperatura_offcoil"
  names(dataset)[23]<-"presion_filtro"
  names(dataset)[24]<-"humedad_suministro_aire"
  names(dataset)[25]<-"presion_suministro_aire"
  names(dataset)[26]<-"temperatura_suministro_aire"
  names(dataset)[27]<-"presion_barametrica"
  names(dataset)[28]<-"temperatura_exterior"
  names(dataset)[29]<-"radiacion_solar_global_horizontal"
  names(dataset)[30]<-"direccion_viento"
  names(dataset)[31]<-"velocidad_viento"
  names(dataset)[32]<-"co2_exterior"
  names(dataset)[33]<-"humedad_relativa_exterior"
  names(dataset)[34]<-"ocupantes_presencia"
  names(dataset)[35]<-"ocupantes_conteo"
}

#-------------------------------------------------------------------------------

# Cambio del formato de la marca de tiempo para que tenga formato fecha

dataset$marca_tiempo<-as.POSIXct(strftime(dataset$marca_tiempo, 
                                          format = "%Y-%m-%d %H:%M:%OS"))

#-------------------------------------------------------------------------------

# Exportar dataset

{
  write.csv2(dataset, paste(wd, "/datasets/room4_1.csv", sep=""), 
             row.names=FALSE) 
  
  saveRDS(dataset, paste(wd, "/rds_files/dataset.rds", sep=""))
}

#===============================================================================