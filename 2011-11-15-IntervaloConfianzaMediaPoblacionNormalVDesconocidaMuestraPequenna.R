##############################################################
#
# Estadística, Grado en Biología UAH
# Curso 2011/2012.
# Fichero de instrucciones R para calcular
# un intervalo de confianza (1-alfa) para la media de una
# poblacion normal N(mu,sigma), a partir de una
# muestra de tamaño n.
# La varianza de la poblacion se desconoce, 
# y la muestra es pequeña  n<30.
##############################################################


# Introducimos el valor de la media y desviación típica muestrales,
s<-4
xbar<-320

# el tamaño de la muestra,
n<- 50

# y el nivel de confianza deseado.
alfa<-0.99

# Calculamos el valor crítico:

t_kalfa2<- -qt((1-alfa)/2,df=(n-1))

# la semianchura del intervalo:
semianchura<-t_kalfa2*s/sqrt(n)
semianchura

# Y el intervalo de confianza para es este:
#extremo inferior
xbar-semianchura 

#extremo superior
xbar+semianchura 



