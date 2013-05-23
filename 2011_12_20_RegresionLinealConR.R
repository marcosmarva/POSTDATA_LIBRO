##############################################################
#
# Estadística, Grado en Biología UAH
# Curso 2011/2012.
# Fichero de instrucciones R para calcular
# un intervalo de confianza al nivel (1-alfa) para el 
# cociente de varianzas en dos poblaciones normales
##############################################################

##Introducimos los vectores de datos
x<-c(0.9,1.4,1.8,1.9,2.3,2.5,2.6,2.9)
y<-c(0.63,4.38,5.86,6.43,7.81,10.05,9.02,11.23)

## Si los datos están en un fichero csv, con dos columnas
## la x en la primera columna, la y en la segunda
## y sin línea de cabecera (los datos empiezan en la 
## primera fila del fichero), debes utilizar las siguientes 
## lineas, eliminando el símbolo de comentario al comienzo.
## Recuerda que debes:
## 1. fijar el directorio de trabajo
## 2. cambiar el nombre del fichero de datos en la primera 
##    línea después de estas instrucciones,

#datos<-read.table("datosRegresion.csv",header=false)
#attach(datos)
#datos

################################################
#NO CAMBIES NADA DE AQUÍ PARA ABAJO
################################################
##Calculamos las medias de x e y.
bar_x=mean(x);bar_x
bar_y=mean(y);bar_y
##Y sus varianzas 
n=length(x)
Vx=((n-1)/n)*var(x) ; Vx
Vy=((n-1)/n)*var(y);  Vy

##Calculamos la covarianza.
covarianza=((n-1)/n)*cov(x,y) ; covarianza

##La pendiente de la recta de regresión lineal
b=covarianza/Vx ; b
## y su ordenada en el origen.
a=bar_y-b*bar_x ; a

## Finalmente calculamos el coeficiente de regresión lineal de Pearson.
r2=(covarianza/sqrt(Vx*Vy))^2; r=sqrt(r2); r;



