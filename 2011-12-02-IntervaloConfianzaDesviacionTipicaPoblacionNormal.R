##############################################################
#
# Estadística, Grado en Biología UAH
# Curso 2011/2012.
# Fichero de instrucciones R para calcular
# un intervalo de confianza al nivel (1-alfa) para la 
# desviacion típica de una  poblacion normal N(mu,sigma), 
# a partir de una muestra de tamaño n.
##############################################################


# Introducimos el valor de la desviación típica muestral,
s<-sqrt(0.15)


# el tamaño de la muestra,
n<- 10

# y el nivel de confianza deseado.
nc<-0.90
alfa<-1-nc

# Calculamos los valor críticos necesarios:

chi_alfamedios<- qchisq(1-alfa/2,df=(n-1))
chi_alfamedios
chi_unomenosalfamedios<- qchisq(alfa/2,df=(n-1))
chi_unomenosalfamedios

#Para la varianza, el intervalo de confianza sería
#((n-1)*s^2/chi_alfamedios)
#((n-1)*s^2/chi_unomenosalfamedios)


# Y para la desviación típica el intervalo de confianza es este:
#extremo inferior a
sqrt((n-1)*s^2/chi_alfamedios)

#extremo superior b
sqrt((n-1)*s^2/chi_unomenosalfamedios)






