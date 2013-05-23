##############################################################
#
# Estadística, Grados en Biología y Biología Sanitaria, UAH
# Fichero de instrucciones R para modelos de regresión no lineales
#
##############################################################

#Antes de empezar, limpiamos los objetos en memoria.
#Comenta esta linea si no es eso lo que quieres.
rm(list=ls())

##Introducimos los vectores de datos
#x=c(0.9,1.4,1.8,1.9,2.3,2.5,2.6,2.9)
#y=c(0.63,4.38,5.86,6.43,7.81,10.05,9.02,11.23)
#x=c(0.72,1.31,1.95,2.58,3.14)
#y=c(2.16,1.61,1.16,0.85,0.5)
#x=(seq(11,17,length.out=20))
#(y=2+3*exp(x)+rnorm(20,sd=0.01))
x = seq(-10,0,length.out=20)#-(1:100)/10
y = 100 + 10 * exp(x / 2) + rnorm(x)/10


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
##    líneal después de estas instrucciones,

#setwd("PON AQUI TU DIRECTORIO DE TRABAJO!!")
#setwd("C:/Users/Fernando/Desktop")
#datos=read.table("Anscombe.csv",header=T)
#attach(datos)
#(x=datos[,2])
#(y=datos[,3])

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

# Y descomentar la siguiente línea para un análisis gráfico del modelo.
#plot(lm.xy)


##############################################################
# REGRESIÓN NO LINEAL
##############################################################

#Modelo polinomial
# Descomentar para usar
gradoPolinomio=2
lm.Poly_xy=lm(y~poly(x,gradoPolinomio,raw=T))
summary(lm.Poly_xy)
xx = seq(min(x),max(x), length.out=250)
lines(xx, predict(lm.Poly_xy, data.frame(x=xx)), col='blue',lwd=3)


# Modelo exponencial
nls1.xy=nls(y~a1+a2*exp(a3*x))
summary(nls1.xy)
yy1=predict(nls1.xy,data.frame(x=xx))
lines(xx,yy1, col='red',lwd=3)

# Modelo reciproco
#nls2.xy=nls(y~a1+a2/x)
#summary(nls2.xy)
#yy2=predict(nls2.xy,data.frame(x=xx))
#lines(xx,yy2, col=2,lwd=6)

#Modelo logaritmico
#nls3.xy=nls(y~a1+a2*log(x+a3))
#summary(nls3.xy)
#yy3=predict(nls3.xy,data.frame(x=xx))
#lines(xx,yy3, col=3,lwd=6)







