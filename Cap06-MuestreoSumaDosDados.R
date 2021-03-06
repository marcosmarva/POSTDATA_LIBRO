##############################################################
#
# Estad�stica, Grado en Biolog�a UAH
#
# Fichero de instrucciones R para analizar
# la distribuci�n muestral de la variable suma
# en el lanzamiento de dos dados. Se van a obtener
# todas (hay 46656) las muestras de tama�o 3, y se calcula a partir
# de ellas la media y desviaci�n t�pica muestral.
##############################################################

# Vamos a crear una matriz Muestras que contenga
# en cada fila una de las muestras,
# representada por tres n�meros del 1 al 36.
# El n�mero 1 corresponde a la tirada (1,1), el 
# n�mero 7 a la tirada (2,1) y as� hasta el 36 que corresponde a 
# la tirada (6,6).



Muestras=array(dim=c(1,3,0))
Muestras
for(i1 in 1:36){
  for(i2 in 1:36){
    for(i3 in 1:36){
      Muestras=rbind(Muestras,c(i1,i2,i3))
    }
  }
}
dim(Muestras)

# Necesitamos una forma de recuperar el valor de la suma para la
# tirada de dados original a partir de cada uno de los n�meros
# x del 1 al 36. La funci�n sumafc se encarga de esto. Para ello
# utiliza dos funciones auxiliares, ffila y fcolumna que calculan
# respectivamente, el n�mero de fila y columna a partir del n�mero x.

ffila=function(x)((x-1)%/%6)+1
fcolumna=function(x)(x-6*(ffila(x)-1))
sumafc=function(x)ffila(x)+fcolumna(x)


# Empezamos por calcular la distribucion de la variable suma en la
# poblacion original. El espacio muestral lo forman los n�meros del 1 al 36,
# y los valores de la suma se obtienen aplicando la funci�n sumafc a cada uno de
# esos 36 n�meros. Se guardan en el vector poblacion.

espacioMuestral=1:36
poblacion=sumafc(espacioMuestral)
poblacion

# Y ahora calculamos la media y la varianza poblacionales de la variable suma.
mu=mean(poblacion)
mu
desvestPob=sqrt(sum((poblacion-mu)^2)/length(poblacion))
desvestPob

# A continuaci�n comenzamos a estudiar la distribuci�n poblacional de la suma.
# Para obtenerla, primero aplicamos la funci�n sumafc a cada una de las tres componentes de cada muestra.
# Es decir, hay 46656 muestras, cada una con tres n�meros del 1 al 36, y calculamos sumafc para
# cada uno de esos tres. Y una vez calculada la suma, hacemos la media de las tres sumas que corresponden a cada muestra.
# Al final tenemos un vector mediasMuestrales, con 46656 medias muestrales, una por muestra.
mediasMuestrales=c()
for(j in 1:46656){
mediasMuestrales=c(mediasMuestrales,mean(sumafc(Muestras[j,])))
}
mediasMuestrales

# Ahora ya estamos en condiciones de estudiar la distribuci�n poblacional.
# Primero su media, que coincide con la de la poblaci�n.
muMM=mean(mediasMuestrales)
muMM

# Y ahora su desviaci�n t�pica, que �no coincide con la de la poblaci�n original!
# Recordamos la de la poblaci�n original para comparar.
desvestMM=sqrt(sum((mediasMuestrales-muMM)^2)/length(mediasMuestrales))
desvestMM
desvestPob

# �Y vemos que la desviaci�n t�pica de las medias muestrales es bastante m�s peque�a!

# Si miramos el cociente de ambas desviaciones t�picas,
desvestPob/desvestMM

# veremos que coincide con
sqrt(3)

# y la raz�n, naturalmente, es que el tama�o de las muestras es 3.

#Descomentar estas filas para obtener los diagramas de barras poblacional y muestral.
#tablaPoblacion=as.matrix(table(poblacion))
#tablaPoblacion
#frecuPoblacion=tablaPoblacion[1:length(tablaPoblacion)]
#frecuPoblacion
#barplot(frecuPoblacion)
#
#tablaMuestral=as.matrix(table(mediasMuestrales))
#tablaMuestral
#frecuMuestral=tablaMuestral[1:length(tablaMuestral)]
#frecuMuestral
#barplot(frecuMuestral	)
