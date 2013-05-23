##############################################################
#
# Estadística, Grado en Biología UAH
# Curso 2011/2012. Sesión de teoría 6. 
#
##############################################################

# Introducimos un vector con los valores distintos de la variable

valores<-c(8,13,16,19,23,28)

# y otro con las frecuencias respectivas de esos valores

frecuencias<-c(14,13,11,7,13,3)

# en caso de que tengamos los valores sin agrupar por frecuencias. comentamos la anterior
# y descomentamos la siguiente, que asigna frecuencia uno a todos los valores

#frecuencias<-rep(1,length(valores))

# Creamos un vector con los datos desagrupados (repitiéndolos tantas veces como indica su frecuencia)

datos<-rep(valores,frecuencias)

# La función var de R calcula la varianza muestral (con n-1 en el denominador)

var(datos)

# Así que vamos a calcular "a mano" la varianza poblacional (con n en el denominador)
# Primero la media aritmética (calculamos y mostramos)

media<-mean(datos)
media

# Y ahora la varianza (calculamos y mostramos)

varp<-sum((datos-media)^2)/length(datos)
varp

#Finalmente la desviación típica (poblacional) es

sqrt(varp)

# Además podemos pedir un resumen de indicadores estadísticos para esta variable
summary(datos)