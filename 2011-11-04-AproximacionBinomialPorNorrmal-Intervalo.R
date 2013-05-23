##############################################################
#
# Estadística, Grado en Biología UAH
# Curso 2011/2012.
# Fichero de instrucciones R para calcular
# los valores de probabilidad de una distribucion biomial
# y su aproximaxicion por una distibucion normal
#
##############################################################


# Introducimos los parametros de la binomial X de tipo  B(n,p)
n<-100
p<-1/3

# Calculamos la media y desviacion tipica
mu<-n*p
sigma<-sqrt(n*p*(1-p))


# Introducir los valores a y b para los que se desea calcular P(a<=X<=b)
a<-30
b<-35

# El valor de P(X=k) se obtiene con
p1<-0
for (j in a:b){
p1 <- p1 + dbinom(j,size=n,prob=p)
}


# Y usando la aproximacion por la normal se obtiene
dnorm(b+0.5,mean=mu,sd=sigma)-dnorm(a-0.5,mean=mu,sd=sigma)
