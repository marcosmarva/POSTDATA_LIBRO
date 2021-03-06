##############################################################
#
# Estad�stica, Grado en Biolog�a UAH
# Curso 2011/2012.
# Fichero de instrucciones R para calcular
# un intervalo de confianza al nivel (1-alfa) para la 
# desviacion t�pica de una  poblacion normal N(mu,sigma), 
# a partir de una muestra de tama�o n.
##############################################################


# Introducimos el valor de la desviaci�n t�pica muestral,
s<-sqrt(0.15)


# el tama�o de la muestra,
n<- 10

# y el nivel de confianza deseado.
nc<-0.90
alfa<-1-nc

# Calculamos los valor cr�ticos necesarios:

chi_alfamedios<- qchisq(1-alfa/2,df=(n-1))
chi_alfamedios
chi_unomenosalfamedios<- qchisq(alfa/2,df=(n-1))
chi_unomenosalfamedios

#Para la varianza, el intervalo de confianza ser�a
#((n-1)*s^2/chi_alfamedios)
#((n-1)*s^2/chi_unomenosalfamedios)


# Y para la desviaci�n t�pica el intervalo de confianza es este:
#extremo inferior a
sqrt((n-1)*s^2/chi_alfamedios)

#extremo superior b
sqrt((n-1)*s^2/chi_unomenosalfamedios)






