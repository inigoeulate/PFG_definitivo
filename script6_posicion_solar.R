#===============================================================================
#===============================================================================
# Sexto script del PFG "Desarrollo de modelos de caracterización a corto plazo
# de la evolución térmico-energética en edificios" elaborado por el estudiante
# Iñigo García de Eulate Pascual (5ºADE+ITI, Universidad de Deusto) en
# colaboración con el tutor del PFG Roberto Garay Martínez.
#===============================================================================
#===============================================================================

#===============================================================================
#===============================================================================
# En este sexto script se llevará a cabo el cálculo de las posición solar.
#===============================================================================
#===============================================================================

#===============================================================================
# Obtención del dataset
#-------------------------------------------------------------------------------

dataset<-readRDS(paste(wd, "/rds_files/dataset.rds", sep=""))

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

{
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
}

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

# Exportar dataset

{
  saveRDS(dataset, paste(wd, "/rds_files/dataset.rds", sep=""))
  
  write.csv2(dataset, paste(wd, "/datasets/room4_4.csv", sep=""), 
             row.names=FALSE) 
}

#-------------------------------------------------------------------------------

# Limpieza de variables

rm(acimut, altura, angulo_horario, B, cenit, cociente, declinacion,d iferencia,
   i, latitud)

#===============================================================================