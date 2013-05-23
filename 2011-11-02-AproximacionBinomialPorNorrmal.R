##############################################################
#
# Estadística, Grado en Biología UAH
#
# Fichero de instrucciones R para calcular
# los valores de probabilidad de una distribucion biomial
# y su aproximaxicion por una distibucion normal
#
##############################################################


# Introducimos los parametros de la binomial X de tipo  B(n,p)
n=100
p=1/3

# Calculamos la media y desviacion tipica
mu=n*p
sigma=sqrt(n*p*(1-p))


# Introducir el valor k para el que se desea calcular P(X=k)
k=30

# El valor de P(X=k) se obtiene con
dbinom(k,size=n,prob=p)

# o tambien, usando la definicion
choose(n,k)*p^k*(1-p)^(n-k)

# Y usando la aproximacion por la normal se obtiene
dnorm(k,mean=mu,sd=sigma)
