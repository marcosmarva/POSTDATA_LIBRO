############################################################## 
# 
# Estadística, Grado en Biología UAH 
# Curso 2011/2012. 
# Fichero de instrucciones R para calcular 
# un intervalo de confianza (1-alfa) para la media de una 
# poblacion normal N(mu,sigma), a partir de una 
# muestra de tamaño n. 
############################################################## 
 
 
# Introducimos el valor de sigma y de la media muestral, 
sigma=3.2
xbar=-16.1
sigma
xbar

# el tamaño de la muestra, 
n<- 97
 
# y el nivel de confianza deseado. 
alfa<-0.99 
 
# Calculamos el valor crítico: 
 
z_alfa2<- -qnorm((1-alfa)/2) 
 
#y la semianchura del intervalo 
semianchura<-z_alfa2*sigma/sqrt(n) 
semianchura 
 
# Y el intervalo de confianza para es este: 
#extremo inferior 
xbar-semianchura  
 
#extremo superior 
xbar+semianchura  
 
 

