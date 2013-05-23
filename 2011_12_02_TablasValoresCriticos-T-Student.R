##############################################################
#
# Estadística, Grado en Biología UAH
# Curso 2011/2012.
# Fichero de instrucciones R para la generar tablas de valores críticos
# para la distribucion t de Student (tablaT)
#
# Estos valores son útiles para construir intervalos de confianza
# y realizar contrastes de hipótesis.
##############################################################


################################################################
# Estos son los valores de probabilidad para los que se calculan 
# los correspondientes valores críticos
################################################################
p<-c(0.010,0.025,0.050,0.075,0.100,0.90,0.925,0.95,0.975,0.99)

######################################
# TABLA DE VALORES DE LA t de Student 
######################################

#Introducimos los grados de libertad
gradosLibertad<-10

#Creamos la tabla (inicialmente vacía)
TablaT<-array(dim=c(1,length(p),0))

# Se generan los t_alfa y se añaden a la tabla.
# Estos valores tienen la propiedad de que
# 
#
#    P(T>t_alfa)=alfa
#
# la cola derecha vale alfa
# o de forma equivalente
#
# P(T<=t_alfa)=1-alfa
#
alfa<-p
TablaT<-rbind(TablaT,alfa)
t_alfa<-qt(df=gradosLibertad,1-alfa)
TablaT<-rbind(TablaT,t_alfa)

# Para los intervalos de confianza y los contrastes bilaterales 
# usamos los valores t_alfamedios.
# Ahora se generan los t_alfamedios y se añaden a la tabla.
# Estos valores tienen la propiedad de que
#
#    P(T>t_alfamedios)+P(T < (-alfamedios) )=alfa
#
# es decir, las dos colas suman alfa.
#
alfamedios<-p/2
TablaT<-rbind(TablaT,alfamedios)
t_alfamedios<-qt(df=gradosLibertad,1-alfamedios)
TablaT<-rbind(TablaT,t_alfamedios)

# Para los contrastes unilaterales 
# usamos los valores t_unomenosalfa.
# Ahora se generan los t_unomenosalfa y se añaden a la tabla.
# Estos valores tienen la propiedad de que
#
#    P(z<unomenosalfa)=alfa
#
# es decir, la cola izquierda vale alfa.
#
unomenosalfa<-1-p
TablaT<-rbind(TablaT,unomenosalfa)
t_unomenosalfa<-qt(df=gradosLibertad,1-unomenosalfa)
TablaT<-rbind(TablaT,t_unomenosalfa)

#######################################
# TABLA DE VALORES DE LA t DE STUDENT
#######################################

TablaT

#TablaZ[c(1,4),]
