##############################################################
#
# Estadística, Grados en Biología y Biología Sanitaria UAH
# Curso 2012/2013.
# Fichero de instrucciones R para la distibucion de Poisson.
# En particular, para una variable X con distribucion 
# Poi(lambda) 
# vamos a aprender a calcular
# (0) P(X=k)
# (1) P(X<=k)         (1a) P(X<k)
# (2) P(X>=k)         (2a) P(X>k) 
# (3) P(k1<=X<=k2)    (3a) P(k1<X<k2)
# (4) dada una probabilidad u, hallar el mínimo k con P(X<=k)>=u
#
# Además, al final del fichero se incluye el comando para generar 
# valores aleatorios de esta distribución.
##############################################################

# Introducimos el parametro lambda (y lo mostramos)
lambda=2
lambda


#### CASO (0) P(X=k)#########
#
# Introducir k
k=3

# El valor de P(X=k) se obtiene con
dpois(k,lambda)


#### CASO (1) P(X<=k) #########
# Introducir k
k=3
# El valor de P(X<=k) se obtiene con
ppois(k,lambda)

#### CASO (1a) P(X<k) #########
# Introducir k
k=3
# El valor de P(X<k) se obtiene con
ppois(k-1,lambda)


#### CASO (2) P(X>=k) #########
# Introducir k
k=3
# El valor de P(X<=k) se obtiene con
1-ppois(k-1,lambda)


#### CASO (2a) P(X>k) #########
# Introducir k
k=3
# El valor de P(X>k) se obtiene con
1-ppois(k,lambda)


#### CASO (3) P(k1<=X<=k2) #########

# Introducir los valores k1 y k2 para los que se desea calcular P(k1<=X<=k2)
k1=2
k2=5
# El valor exacto de P(k1<=X<=k2),que llamamos p1, se obtiene con
ppois(k2)-ppois(k1-1)

#### CASO (3a) P(k1<X<k2) #########

# Introducir los valores k1 y k2 para los que se desea calcular P(k1<X<k2)
k1= 12
k2=22
# El valor de P(k1<X<k2) se obtiene con
ppois(k2-1,lambda)-ppois(k1,lambda)


#### CASO (4), dada una probabilidad u, hallar el mínimo k con P(X<=k)>=u #########
# Introducir u
u=0.85
# El valor de k se obtiene con
qpois(u,lambda)


#### Generación de nümeros aleatorios

#Cuántos números aleatorios queremos
m=50
#Se generan mediante
rpois(m,lambda)





