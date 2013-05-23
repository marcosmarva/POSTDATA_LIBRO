############################################################## 
# 
# Estadística, Grado en Biología y Bio. Sanitaria UAH 
# Fichero de instrucciones R para calcular 
# un intervalo de confianza (1-alfa) para la 
# diferencia de proporciones, en dos 
# poblaciones normales, a partir de sendas
# muestras de tamaños n1 y n2 grandes (>30). 
############################################################## 
 
 
# Introducimos el valor de las dos proporciones muestrales, 
p1_gorro=0.48
p2_gorro=0.27

# y los tamaños de la muestras, 
n1=429
n2=154
 
# y el nivel de confianza deseado. 
alfa=0.95

################################################
#NO CAMBIES NADA DE AQUï PARA ABAJO
################################################
 
# Calculamos el valor crítico: 
 
z_alfa2= -qnorm((1-alfa)/2) 

# el valor de los q_gorro
 
q1_gorro=1-p1_gorro
q2_gorro=1-p2_gorro


#La semianchura del intervalo es

semianchura=z_alfa2*sqrt( (p1_gorro*q1_gorro)/n1 + (p2_gorro*q2_gorro)/n2 )
semianchura 
 
# Y el intervalo de confianza para es este: 
#extremo inferior 
(p1_gorro-p2_gorro)-semianchura  
 
#extremo superior 
(p1_gorro-p2_gorro)+semianchura  
 
 





