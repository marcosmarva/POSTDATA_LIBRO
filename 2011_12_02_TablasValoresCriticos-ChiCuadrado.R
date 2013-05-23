##############################################################
#
# Estadística, Grado en Biología UAH
# Curso 2011/2012.
# Fichero de instrucciones R para la generar tablas de valores críticos
# para la distribucion Chi cuadrado (tablaChi)
#
# Estos valores son útiles para construir intervalos de confianza
# y realizar contrastes de hipótesis.
##############################################################


################################################################
# Estos son los valores de probabilidad para los que se calculan 
# los correspondientes valores críticos
################################################################
p<-rev(c(0.010,0.025,0.050,0.075,0.100,0.90,0.925,0.95,0.975,0.99))
p

######################################
# TABLA DE VALORES DE Chi cuadrado
######################################

#Introducimos los grados de libertad
gradosLibertad<-10

#Creamos la tabla (inicialmente vacía)
tablaChi<-array(dim=c(1,length(p),0))

# Se generan los chi_alfa y se añaden a la tabla.
# Estos valores tienen la propiedad de que si
# Y es una chi cuadrado (con los grados de libertad
# que hemos establecido)
#
#    P(Y>chi_alfa)=alfa
#
# la cola derecha vale alfa
# o de forma equivalente
#
# P(Y<=chi_alfa)=1-alfa
#
alfa<-p
tablaChi<-rbind(tablaChi,alfa)
chi_alfa<-qchisq(df=gradosLibertad,1-alfa)
tablaChi<-rbind(tablaChi,chi_alfa)

# Para los intervalos de confianza y los contrastes bilaterales 
# usamos los valores chi_alfamedios.
# Ahora se generan los chi_alfamedios y se añaden a la tabla.
# Estos valores tienen la propiedad de que
#
#    P(Y>chi_alfamedios)+P(Y < (-chi_alfamedios) )=alfa
#
# es decir, las dos colas suman alfa.
#
alfamedios<-p/2
tablaChi<-rbind(tablaChi,alfamedios)
chi_alfamedios<-qchisq(df=gradosLibertad,1-alfamedios)
tablaChi<-rbind(tablaChi,chi_alfamedios)

# Para los contrastes unilaterales 
# usamos los valores chi_unomenosalfa.
# Ahora se generan los chi_unomenosalfa y se añaden a la tabla.
# Estos valores tienen la propiedad de que
#
#    P(z<unomenosalfa)=alfa
#
# es decir, la cola izquierda vale alfa.
#
unomenosalfa<-1-p
tablaChi<-rbind(tablaChi,unomenosalfa)
chi_unomenosalfa<-qchisq(df=gradosLibertad,1-unomenosalfa)
tablaChi<-rbind(tablaChi,chi_unomenosalfa)

#######################################
# TABLA DE VALORES DE Chi cuadrado
#######################################

tablaChi


