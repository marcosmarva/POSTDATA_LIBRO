##############################################################
#
# Estad�stica, Grado en Biolog�a UAH
# Fichero de instrucciones R para calcular
# un intervalo de confianza (1-alfa) para la media de una
# poblacion normal N(mu,sigma), a partir de una
# muestra de tama�o n.
# La varianza de la poblacion se desconoce, pero n>30.
##############################################################


# Introducimos el valor de la media y desviaci�n t�pica muestrales,
s=4
xbar=320

# el tama�o de la muestra,
n= 50

# y el nivel de confianza deseado.
alfa=0.99

# Calculamos el valor cr�tico:

z_alfa2= -qnorm((1-alfa)/2)

#y la semianchura del intervalo
semianchura=z_alfa2*s/sqrt(n)
semianchura

# Y el intervalo de confianza para es este:
#extremo inferior
xbar-semianchura 

#extremo superior
xbar+semianchura 

#Advertencia para muestras peque�as.
if(n<30){"CUIDADO: LA MUESTRA ES DEMASIADO PEQUE�A"} else {"EL TAMA�O DE LA MUESTRA ES>30"} 


