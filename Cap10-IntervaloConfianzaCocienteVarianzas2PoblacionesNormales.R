##############################################################
#
# Estadística, Grado en Biología y Bio. Sanitaria UAH
# Fichero de instrucciones R para calcular
# un intervalo de confianza al nivel (1-alfa) para el 
# cociente de varianzas en dos poblaciones normales
##############################################################


# Introducimos los valores de las desviaciones típicas muestrales,
s1 = sqrt(57.7)
s2 = sqrt(9.8)


# los tamaños de las muestras,
n1 =  5
n2 =  5

# y el nivel de confianza deseado.
nc = 0.90
alfa = 1-nc

# Calculamos los valor críticos necesarios:

f_alfamedios =  qf(1-alfa/2,df1=(n1-1),df2=(n2-1))
f_alfamedios
f_unomenosalfamedios =  qf(alfa/2,df1=(n1-1),df2=(n2-1))
f_unomenosalfamedios


# El intervalo de confianza para el cociente de varianzas es este:
#extremo inferior a
f_unomenosalfamedios*s1^2/s2^2

#extremo superior b
f_alfamedios*s1^2/s2^2






