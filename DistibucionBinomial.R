##############################################################
#
# Estadística, Grado en Biología UAH
# Curso 2011/2012.
# Fichero de instrucciones R para calcular
# los valores de probabilidad de una distribucion biomial,
# directamente, y mediante su aproximaxicion por una distibucion normal.
# Se usa correcion de continuidad (y damos los valores sin ella, para comparar).
# Se incluyen tres casos:
# 1) P(X=k)
# 2) P(X<=k)
# 3) P(k1<=X<=k2)
##############################################################


# Introducimos los parametros de la binomial X de tipo  B(n,p)
n = 25
p = 0.5

# Calculamos la media y desviacion tipica
mu = n*p
sigma = sqrt(n*p*(1-p))
mu
sigma

##############################################################
# CASO 1 P(X=k)
##############################################################
# Introducir k
k = 12

# El valor "exacto" de P(X=k) se obtiene con
dbinom(k,size=n,prob=p)
# Y usando la aproximacion por la normal se obtiene
pnorm(k+0.5,mean=mu,sd=sigma)-pnorm(k-0.5,mean=mu,sd=sigma)
# En este caso la aproximacion sin correccion de continuidad sería
dnorm(k,,mean=mu,sd=sigma)

##############################################################
# CASO 2 P(X<=k)
##############################################################
# Introducir k
k = 8
# El valor "exacto" de P(X<=k) se obtiene con
pbinom(k,size=n,prob=p)
# Y usando la aproximacion por la normal se obtiene
pnorm(k+0.5,mean=mu,sd=sigma)
# Sin correccion de continuidad
pnorm(k,mean=mu,sd=sigma)

# CASO 2a P(X>k)
#exacto
1-pbinom(k,size=n,prob=p)
#normal
1-pnorm(k+0.5,mean=mu,sd=sigma)


##############################################################
# CASO 3 P(k1<=X<=k2)
##############################################################
# Introducir los valores k1 y k2 para los que se desea calcular P(k1<=X<=k2)
k1 = 0
k2 = 15
# El valor "exacto" de P(k1<=X<=k2),que llamamos p1, se obtiene con
p1 = 0
vectorj = c()
vectorp = c()
for (j in k1:k2){
vectorj = c(vectorj,j)
vectorp = c(vectorp,dbinom(j,size=n,prob=p))
p1  =  p1 + dbinom(j,size=n,prob=p)
}
p1
vectorj
vectorp
# Y usando la aproximacion por la normal se obtiene
pnorm(k2+0.5,mean=mu,sd=sigma)-pnorm(k1-0.5,mean=mu,sd=sigma)
# Sin correccion de continuidad
pnorm(k2,mean=mu,sd=sigma)-pnorm(k1,mean=mu,sd=sigma)

