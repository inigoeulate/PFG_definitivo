#===============================================================================
#===============================================================================
# En este script se programan todas operaciones necesarias para la obtención de
# los resulados del PFG "INSERTAR TÍTULO DEL PFG" elaborado por el estudiante
# Iñigo García de Eulate Pascual (5ºADE+ITI Universidad de Deusto) en
# colaboración con el tutor del PFG Roberto Garay Martínez.
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

file<-"/data/combined_Room4_modif.csv"
wd<-getwd()
ruta<-paste(wd,file,sep="")
dataset<-read.csv2(ruta)
#View(dataset)

rm(ruta, file)

#===============================================================================

#===============================================================================
# Preprocesado del dataset
#-------------------------------------------------------------------------------

# Eliminación de NAs.

dataset<-na.omit(dataset)

#-------------------------------------------------------------------------------

# Cambio en los nombres de las variables para que tengan nombres más
# autoexplicativos y fáciles de emplear.

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

#-------------------------------------------------------------------------------

# Cambio del formato de la marca de tiempo para que tenga formato fecha

dataset$marca_tiempo<-as.POSIXct(strftime(dataset$marca_tiempo, 
                                          format = "%Y-%m-%d %H:%M:%OS"))

#===============================================================================

#===============================================================================
# Mejora de la robustez de la señal de ocupación por conteo
#-------------------------------------------------------------------------------

# Librería "zoo" que permite emplear la función rollemean para el cálculo de
# medias móviles.

library(zoo)

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

marca_tiempo<-dataset$marca_tiempo

subset<-data.frame(marca_tiempo, 
                   ocupantes_conteo_robus3, ocupantes_conteo_robus5)

#-------------------------------------------------------------------------------

# Eliminación de los NAs del subset

subset<-na.omit(subset)

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------

# Cambio del formato de la marca de tiempo para que tenga formato fecha

subset$marca_tiempo<-as.POSIXct(strftime(subset$marca_tiempo, 
                                          format = "%Y-%m-%d %H:%M:%OS"))

#------------------------------------------------------------------------------

# Se grafican para el primer día del que se tiene datos y se decide cual será la
# empleada.

  # Bucles para delimitar en que filas del dataset comienza y termina el primer
  # día del que se tiene datos.

    # Variables auxiliares para los bucles

exit<-0
i<-1
j<-1

    # Bucle para obtener el número de fila en el que empieza el primer día

while (exit!=1) {
  if (as.numeric(format(subset[i,"marca_tiempo"],"%H"))==00 & 
      as.numeric(format(subset[i,"marca_tiempo"],"%M"))==00) {
    exit<-1
  } else {
    i<-i+1
  }
}
comienzo<-i

    # Bucle para obtener el número de fila en el que acaba el primer día

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

  # Graficación de ambas medias móviles

png(paste(wd,"/plots/ocupantes_conteo_robus3_primer_dia.png",sep=""), width=800, height=800)
plot(subset$marca_tiempo[comienzo:final], subset$ocupantes_conteo_robus3[comienzo:final],
     xlab="hora", ylab="número de ocupantes")
dev.off()

png(paste(wd,"/plots/ocupantes_conteo_robus5_primer_dia.png",sep=""), width=800, height=800)
plot(subset$marca_tiempo[comienzo:final], subset$ocupantes_conteo_robus5[comienzo:final],
     xlab="hora", ylab="número de ocupantes")
dev.off()

#-------------------------------------------------------------------------------

# Se elige la media móvil centrada de 3 elementos y se añade al dataset original

dataset<-cbind(dataset, ocupantes_conteo_robus3)

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------

# Eliminación de los NAs del dataset

dataset<-na.omit(dataset)

#-------------------------------------------------------------------------------

# Limpiado de variables

rm(i,j,exit,comienzo,final,subset,marca_tiempo,
   ocupantes_conteo_robus3,ocupantes_conteo_robus5)

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

# Los husos horarios están centrados en meridianos de longitudes múltiplos de 15°.
# El huso horario conocido como UTC+0 (Coordinated Universal Time) está centrado
# en el meridiano de Greenwich, cuya longitud es 0.
# El uso horario GMT+1 está centrado en el meridiano de longitud 15°,
# y así sucesivamente. 

#El edificio que estamos estudiando se encuentra en Singapur. 
# Uso horario: UTC+8.
# Singapur no cambia de hora con motivo de las estaciones.

# Coordenadas del edificio: 1.28967°N, 103.85007°E.

#-------------------------------------------------------------------------------

# Librería "lubridat" que introduce funciones para facilitar el manejo de fechas.

library(lubridate)

#-------------------------------------------------------------------------------

# Se entiende que hay una errata en las ecuaciones propuestas por los autores,
# dado que entendiendo L_loc y L_st como lo recogen en el Solar Engineering
# of Thermal Processes (4th ed.), la diferencia queda de signo contrario al que
# debería.

# Por lo tanto, se considera:
  # L_st: longitud de la localización en cuestión.
  # L_loc: longitud meridiano estándar para la zona horaria local.

L_loc<-8*15
L_st<-103.850077

B<-(as.numeric(strftime(dataset$marca_tiempo,format="%j"))-1)*360/365

E<-229.2*(0.000075+
            0.001868*cos(B*pi/180)-
            0.032077*sin(B*pi/180)-
            0.014615*cos(2*B*pi/180)-
            0.04089*sin(2*B*pi/180))

hora_solar<-dataset$marca_tiempo + seconds_to_period(60*(4*(L_st-L_loc)+E))

#-------------------------------------------------------------------------------

# Se añade la variable creada para la hora solar al dataset

dataset<-cbind(dataset,hora_solar)

#-------------------------------------------------------------------------------

# Limpiado de variables

rm(B,E,hora_solar,L_loc,L_st)

#===============================================================================

#===============================================================================
# Cálculo de la posición solar
#-------------------------------------------------------------------------------

# A partir de la información de Duffie, J. A., & Beckman, W. A. (2013). Solar
# Engineering of Thermal Processes (4th ed.). Wiley

# Ángulos que describen la posición del sol relativa a un plano
  # φ: Latitude, the angular location north or south of the equator, north
      # positive; −90◦ ≤ φ ≤ 90◦.
  # δ: Declination, the angular position of the sun at solar noon (i.e., when
      #the sun is on the local meridian) with respect to the plane of the
      #equator, north positive; −23.45◦ ≤ δ ≤ 23.45◦.
  # β: Slope, the angle between the plane of the surface in question and the
      #horizontal; 0◦ ≤ β ≤ 180◦. (β > 90◦ means that the surface has a
      #downward-facing component.)
  # γ: Surface azimuth angle, the deviation of the projection on a horizontal
      #plane of the normal to the surface from the local meridian, with zero due
      #south, east negative, and west positive; −180◦ ≤ γ ≤ 180◦.
  # ω: Hour angle, the angular displacement of the sun east or west of the local
      #meridian due to rotation of the earth on its axis at 15◦ per hour; 
      #morning negative, afternoon positive.
  # θ: Angle of incidence, the angle between the beam radiation on a surface and
      #the normal to that surface.

# Ángulos adicionales que describen la posición del sol en el cielo
  
  # θ_z: Zenith angle, the angle between the vertical and the line to the sun,
        #that is, the angle of incidence of beam radiation on a horizontal
        #surface.
    # θ_z = arccos(cosφ*cosδ*cosω + sinφ*sinδ)
    # θ_z = 90-α_s
      # δ = (180/π)(0.006918 − 0.399912 cos B + 0.070257 sin B − 0.006758 cos 2B
            # + 0.000907 sin 2B − 0.002697 cos 3B + 0.00148 sin 3B)
      # φ: Latitude, the angular location north or south of the equator, north
          #positive; −90◦ ≤ φ ≤ 90◦.
      # ω = 15*(hora-12h00)
  
  # α_s: Solar altitude angle, the angle between the horizontal and the line to
        #the sun, that is, the complement of the zenith angle.
    # Usaremos la fórmula propuesta en http://www.sc.ehu.es/sbweb/fisica3/celeste/sol/sol.html
      # α_s = arcsin(cosφ*cosω*cosδ + sinφ*sinδ)

  # γ_s: Solar azimuth angle, the angular displacement from south of the 
        #projection of beam radiation on the horizontal plane, shown in Figure 
        #1.6.1. Displacements east of south are negative and west of south are
        #positive.
    # Usaremos la fórmula propuesta en http://www.sc.ehu.es/sbweb/fisica3/celeste/sol/sol.html
      # y_c = arccos((cosφ*sinδ-cosω*sinφ*cosδ)/cosα_s)
        # Si ω<0 --> y_s = y_c
        # Si ω>0 --> y_s = 360 - y_c
      # Se corregirá este sistema de referencia de la siguiente forma:
        # Sistema de referencia de las ecuaciones: 0º - 360º (de N a E, S, O)
        # Sistema de referencia a emplear: -180º - 180º 
            # (S=0º, hacia el E negativo, hacia el O positivo)

#-------------------------------------------------------------------------------

B<-(as.numeric(strftime(dataset$hora_solar,format="%j"))-1)*360/365

declinacion<-(180/pi)*(0.006918 - 0.399912*cos(B*pi/180)
                       + 0.070257*sin(B*pi/180) 
                       - 0.006758*cos(2*B*pi/180) + 0.000907*sin(2*B*pi/180)
                       - 0.002697*cos(3*B*pi/180) + 0.00148*sin(3*B*pi/180))

latitud<-1.28967

diferencia<-(as.numeric(strftime(dataset$hora_solar,format="%H")))+
  (as.numeric(strftime(dataset$hora_solar,format="%M")))/60-12

angulo_horario<-15*(diferencia)

cenit<-180/pi*acos(cos(latitud*pi/180)*cos(declinacion*pi/180)*cos(angulo_horario*pi/180)
                   +sin(latitud*pi/180)*sin(declinacion*pi/180))

altura<-180/pi*asin(cos(latitud*pi/180)*cos(declinacion*pi/180)*cos(angulo_horario*pi/180)
                    +sin(latitud*pi/180)*sin(declinacion*pi/180))

#zenith_comprobacion<-90-altura

cociente<-((cos(latitud*pi/180)*sin(declinacion*pi/180)
            -cos(angulo_horario*pi/180)*sin(latitud*pi/180)*cos(declinacion*pi/180))/
             cos(altura*pi/180))

# El cociente puede ser >1 o <-1 por las aproximaciones que se van introduciendo.
# Se limita el valor del cociente al rango [-1,1].
cociente<-pmin(1,cociente)
cociente<-pmax(-1,cociente)

acimut<-180/pi*acos(cociente)

for(i in 1:nrow(dataset)){
  if (angulo_horario[i]>0){
    acimut[i]<-360-acimut[i]
  }
}

# Corregir el sistema de referencia del acimuth:
acimut<-acimut-180

#-------------------------------------------------------------------------------

# Truncar:
  # Valores negativos de la altura solar a 0º.
  # Valores mayores de 90º del cenit a 90º.
# Son aquellos momentos en los que el sol está por debajo del horizonte.

for(i in 1:nrow(dataset)){
  if (altura[i]<0){
    altura[i]<-0
  }
  if (cenit[i]>90){
    cenit[i]<-90
  }
}

#-------------------------------------------------------------------------------

# Se añaden las variables creadas para la altura, cenit y acimut al dataset

dataset<-cbind(dataset,altura,cenit,acimut)

#-------------------------------------------------------------------------------

# Limpiado de variables

rm(acimut,altura, angulo_horario,B,cenit,cociente,declinacion,diferencia,i,
   latitud)

#===============================================================================

#===============================================================================
# Corrección de la radiación solar
#-------------------------------------------------------------------------------

# Diferenciación entre radiación disufa y directa
  # Se eligen 100 W*m^2 como límite de la radiación difusa

radiacion_difusa<-0
for (i in 1:nrow(dataset)){
  if (dataset$radiacion_solar_global_horizontal[i]<100){
    radiacion_difusa[i]<-dataset$radiacion_solar_global_horizontal[i]
  }else{
    radiacion_difusa[i]<-100
  }
}

radiacion_directa_horizontal<-dataset$radiacion_solar_global_horizontal-radiacion_difusa

#-------------------------------------------------------------------------------

# Conversión de radiación solar sobre plano horizontal a sobre plano
# perpendicular a la incidencia solar

radiacion_directa_perpendicular<-radiacion_directa_horizontal*
  sin(dataset$altura*pi/180)

#-------------------------------------------------------------------------------

# Cálculo del acimut aparente sobre una superficie vertical arbitraria

acimut_plano<-20 # obtenido de la vista satélite

acimut_aparente<-dataset$acimut-acimut_plano

#-------------------------------------------------------------------------------

# Conversión de la radiación solar sobre plano perpendicular a la incidencia
# solar a radiación solar incidente sobre fachada

radiacion_directa_fachada<-radiacion_directa_perpendicular*cos(dataset$altura*pi/180)*cos(acimut_aparente*pi/180)

# Verificación:
  # si el sol ve la fachada (azimuth aparente entre -90º y 90º) -> radiación directa
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

marca_tiempo<-dataset$marca_tiempo
hora_solar<-dataset$hora_solar
acimut<-dataset$acimut
altura<-dataset$altura
cenit<-dataset$cenit
radiacion_solar_global_horizontal<-dataset$radiacion_solar_global_horizontal

dataset_solar<-cbind(marca_tiempo, hora_solar,
               acimut, altura, cenit,
               radiacion_solar_global_horizontal,
               radiacion_difusa, radiacion_directa_horizontal,
               radiacion_directa_perpendicular,
               radiacion_directa_fachada, radiacion_global_fachada)

write.csv2(dataset_solar, paste(wd,"/datasets/room4_var_solar.csv",sep=""), 
           row.names=FALSE)

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

exit<-0
i<-1
j<-1

    # Bucle para obtener el número de fila en el que empieza el día 2021/09/10
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

    # Bucle para obtener el número de fila en el que acaba el día 2021/09/10
while (exit!=1) {
  if (as.numeric(format(dataset[i,"hora_solar"],"%d"))!=10) {
    exit<-1
  } else {
    i<-i+1
  }
}

final<-i-1

dia10<-dataset[comienzo:final,]

    # Generación del archivo PNG con las gráficas

png(paste(wd,"/plots/dia_alta_radiacion.png",sep=""), width=800, height=800)
plot(dia10$hora_solar, dia10$radiacion_solar_global_horizontal,
     xlab="hora", ylab="radiación [W/m2]",col="red",cex=1.5,
     ylim=c(min(dia10$radiacion_solar_global_horizontal),
            max(dia10$radiacion_solar_global_horizontal)))
points(dia10$hora_solar, dia10$radiacion_difusa, col="blue", cex=1.5)
points(dia10$hora_solar, dia10$radiacion_directa_fachada, col="yellow", cex=1.5)
legend(x="topleft", legend=c("Radiación solar global horizontal", 
                             "Radiación difusa",
                             "Radiación directa s/ fachada"),
       col=c("red", "blue", "yellow"),
       lty=c(1, 1, 1, 1),
       cex=1.4)
dev.off()

  # Día con poca radiación solar: 2021/09/13
exit<-0
i<-1
j<-1

    # Bucle para obtener el número de fila en el que empieza el día 2021/09/13
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

    # Bucle para obtener el número de fila en el que acaba el día 2021/09/13
while (exit!=1) {
  if (as.numeric(format(dataset[i,"hora_solar"],"%d"))!=13) {
    exit<-1
  } else {
    i<-i+1
  }
}

final<-i-1

dia13<-dataset[comienzo:final,]

# Generación del archivo PNG con las gráficas

png(paste(getwd(),"/plots/dia_baja_radiacion.png",sep=""), width=800, height=800)
plot(dia13$hora_solar, dia13$radiacion_solar_global_horizontal, xlab="hora",
     ylab="radiación [W/m2]",col="red",cex=1.5,
     ylim=c(min(dia10$radiacion_solar_global_horizontal),
            max(dia10$radiacion_solar_global_horizontal)))
points(dia13$hora_solar, dia13$radiacion_difusa, col="blue", cex=1.5)
points(dia13$hora_solar, dia13$radiacion_directa_fachada, col="yellow", cex=1.5)
legend(x="topleft", legend=c("Radiación solar global horizontal", 
                             "Radiación difusa",
                             "Radiación directa s/ fachada"),
       col=c("red", "blue", "yellow"),
       lty=c(1, 1, 1, 1),
       cex=1.4)
dev.off()

#-------------------------------------------------------------------------------

# Limpiado de variables

rm(acimut,acimut_aparente,acimut_plano,altura,cenit,
   comienzo,final,exit,i,j,
   hora_solar,marca_tiempo,
   radiacion_difusa,
   radiacion_directa_fachada,
   radiacion_directa_horizontal,
   radiacion_directa_perpendicular,
   radiacion_global_fachada,
   radiacion_solar_global_horizontal,
   dia10, dia13, dataset_solar)

#===============================================================================

#===============================================================================
# Siguiente pasooo
#-------------------------------------------------------------------------------
