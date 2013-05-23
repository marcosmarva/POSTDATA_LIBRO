##############################################################
#
# Estadística, Grados en Biología y Biología Sanitaria, UAH
# Fichero de instrucciones R para calcular
# un intervalo de confianza al nivel (1-alfa) para el
# cociente de varianzas en dos poblaciones normales
##############################################################

rm(list=ls())


##Introducimos los vectores de datos
x=c(1.2,1.8,3.1,4.9,5.7,7.1,8.6,9.8)
y=c(4.5,5.9,7.0,7.8,7.2,6.8,4.5,2.7)

length(x)
length(y)


## Si los datos están en un fichero csv, con dos columnas
## la x en la primera columna, la y en la segunda
## y sin línea de cabecera (los datos empiezan en la
## primera fila del fichero), debes utilizar las siguientes
## lineas, eliminando el símbolo de comentario al comienzo.
## Recuerda que debes:
## 1. fijar el directorio de trabajo
## 2. cambiar el nombre del fichero de datos en la primera
##    línea después de estas instrucciones,

#setwd("PON AQUI TU DIRECTORIO DE TRABAJO!!")
#datos=read.table("datosRegresion.csv",header=T)
#attach(datos)
#datos
#(x=datos[,1])
#(y=datos[,2])

################################################
#NO CAMBIES NADA DE AQUÍ PARA ABAJO
################################################
##Calculamos las medias de x e y.
(bar_x = mean(x))
(bar_y = mean(y))
##Y sus varianzas
n=length(x)
(Vx=((n-1)/n)*var(x))
(Vy=((n-1)/n)*var(y))

##Calculamos la covarianza.
(covarianza=((n-1)/n)*cov(x,y))

##La pendiente de la recta de regresión lineal
(b=covarianza/Vx)
## y su ordenada en el origen.
(a=bar_y-b*bar_x)

## Finalmente calculamos el coeficiente de regresión lineal de Pearson.
(r2=(covarianza/sqrt(Vx*Vy))^2)
(r=sign(b)*sqrt(r2))

################################################
# Contraste beta=0
################################################

(EstadContraste=abs(covarianza/sqrt((Vx*Vy-covarianza^2)/(n-2))))
(pValor=2*(1-pt(EstadContraste,df=n-2)))
# Para comparar con el estadístico F de más abajo.
EstadContraste^2

################################################
# ANÁLISIS MEDIANTE LA FUNCIÓN lm DE R
################################################

## Definimos el modelo
lm.xy=lm(y~x)

## Resumen de resultados
summary(lm.xy)

##Representaciones gráficas. Descomentar la que se desee

# Recta y nube de puntos. Valores predichos y residuos
plot(x,y,col="red")
abline(lm.xy)
fitted(lm.xy)
resid(lm.xy)
segments(x,fitted(lm.xy),x,y)


# Descomentar la siguiente línea para comprobar la hipótesis de la normalidad mediante un gráfico qqnorm.
#qqnorm(resid(lm.xy))

###########################################################
# INTERVALO DE CONFIANZA PARA LOS COEFICIENTES DE LA RECTA
###########################################################

nc=0.95
alfa=1-nc

#Para la pendiente (beta)
b+qt(c(alfa/2,1-alfa/2),df=n-2)*sqrt((Vy-b*covarianza)/(Vx*(n-2)))

#Para la ordenada en el origen (alfa, no confundir con el alfa del nivel de confianza)
a+qt(c(alfa/2,1-alfa/2),df=n-2)*sqrt((Vy-b*covarianza)/(Vx*(n-2))*(Vx+bar_x^2))

# O directamente, con la función confint de R
confint(lm.xy,level=nc)

