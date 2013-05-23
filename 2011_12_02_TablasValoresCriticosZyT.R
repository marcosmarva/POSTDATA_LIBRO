##############################################################
#
# Estadística, Grado en Biología UAH
# Curso 2011/2012.
# Fichero de instrucciones R para la generar tablas de valores críticos
# para la distribucion normal (tablaZ)y la t de Student (tablaT)
#
# Estos valores son útiles para construir intervalos de confianza
# y realizar contrastes de hipótesis.
##############################################################


################################################################
# Estos son los valores de probabilidad para los que se calculan 
# los correspondientes valores críticos
################################################################
p<-c(0.010,0.025,0.050,0.075,0.100,0.90,0.925,0.95,0.975,0.99)

#Creamos la tabla (inicialmente vacía)
TablaZ<-array(dim=c(1,length(p),0))

# Se generan los z_alfa y se añaden a la tabla.
# Estos valores tienen la propiedad de que
# si Z es la normal N(0,1) entonces
#
#    P(Z>z_alfa)=alfa
#
# la cola derecha vale alfa
# o de forma equivalente
#
# P(Z<=z_alfa)=1-alfa
#
alfa<-p
TablaZ<-rbind(TablaZ,alfa)
z_alfa<-qnorm(1-alfa)
TablaZ<-rbind(TablaZ,z_alfa)

# Para los intervalos de confianza y los contrastes bilaterales 
# usamos los valores z_alfamedios.
# Ahora se generan los z_alfamedios y se añaden a la tabla.
# Estos valores tienen la propiedad de que
# si Z es la normal N(0,1) entonces
#
#    P(Z>z_alfamedios)+P(Z < (-alfamedios) )=alfa
#
# es decir, las dos colas suman alfa.
#
alfamedios<-p/2
TablaZ<-rbind(TablaZ,alfamedios)
z_alfamedios<-qnorm(1-alfamedios)
TablaZ<-rbind(TablaZ,z_alfamedios)

# Para los contrastes unilaterales 
# usamos los valores z_unomenosalfa.
# Ahora se generan los z_unomenosalfa y se añaden a la tabla.
# Estos valores tienen la propiedad de que
# si Z es la normal N(0,1) entonces
#
#    P(z<unomenosalfa)=alfa
#
# es decir, la cola izquierda vale alfa.
#
unomenosalfa<-1-p
TablaZ<-rbind(TablaZ,unomenosalfa)
z_unomenosalfa<-qnorm(1-unomenosalfa)
TablaZ<-rbind(TablaZ,z_unomenosalfa)


####################################
# TABLA DE VALORES z DE LA NORMAL
####################################

TablaZ


#TablaZ[c(1,4),]

######################################
# TABLA DE VALORES DE LA t de Student 
######################################


gradosLibertad<-10

TablaT<-array(dim=c(1,length(p),0))


alfa<-p
TablaT<-rbind(TablaT,alfa)
t_alfa<-qt(df=gradosLibertad,1-alfa)
TablaT<-rbind(TablaT,t_alfa)

alfamedios<-p/2
TablaT<-rbind(TablaT,alfamedios)
t_alfamedios<-qt(df=gradosLibertad,1-alfamedios)
TablaT<-rbind(TablaT,t_alfamedios)


unomenosalfa<-1-p
TablaT<-rbind(TablaT,unomenosalfa)
t_unomenosalfa<-qt(df=gradosLibertad,1-unomenosalfa)
TablaT<-rbind(TablaT,t_unomenosalfa)

TablaT
