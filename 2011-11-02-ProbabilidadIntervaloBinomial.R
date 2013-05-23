##############################################################
#
# Estadística, Grado en Biología UAH
# Curso 2011/2012.
# Fichero de instrucciones R para calcular
# la probabilidad P(a<=X<=b) de una distribucion binomial B(n,p)
#
##############################################################


# Introducimos los parametros de la binomial X de tipo  B(n,p)

n<-1000
p<-1/3

# Calculamos la media y desviacion tipica

mu<-n*p
sigma<-sqrt(n*p*(1-p))


# Introducir el valor k para el que se desea calcular P(X=k)

a<-300
b<-600

# El valor de P(X=k) se obtiene con dbinom(k,size=n,prob=p)

# Creamos una lista con los valores P(X=k) necesarios

lista<-c()
for (k in 300:600){
 lista<-c(lista,dbinom(k,size=n,prob=p)) #añadimos el valor a la lista
}

#Descomenta las siguientes líneas si quieres ver en detalle la lista (puede ser muy larga)
#y su rango

#lista
#range(lista)


# Y ahora sumamos la lsita para obtener P(a<=X<=b)

sum(lista)


# El comando R para hacer esto directamente es

pbinom(b,size=n,prob=p)-pbinom(,size=n,prob=p)
