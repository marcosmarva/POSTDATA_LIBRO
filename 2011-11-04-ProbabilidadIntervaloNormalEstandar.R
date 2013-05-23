##############################################################
#
# Estadística, Grado en Biología UAH
# Curso 2011/2012.
# Fichero de instrucciones R para calcular
# la probabilidad P(a<=X<=b) de una distribucion normal N(mu,sigma)
#
##############################################################


# Introducimos los valores de a y b

a<-(-4/3)
b<-(4/3)

# El comando R para hacer esto directamente es

pnorm(b)-pnorm(a)
