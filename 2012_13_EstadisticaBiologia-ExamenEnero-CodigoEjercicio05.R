rm(list=ls())

# Datos iniciales
X1=c(182,232,191,200,148,249,276,213,241,380,262)
X2=c(198,210,194,220,138,220,219,161,210,313,226)

# Diferencias
(Y=X1-X2)
muestra=Y

# Caracteristicas de la muestra
(xbar=mean(muestra))
(s=sd(muestra))
(n=length(muestra))
(k=n-1)

mu0=0 #Introducir el valor

(Estadistico=(xbar-mu0)/(s/sqrt(n)))

TipoContraste=1 # 1 para Hip. alternativa mu > mu0, 2 para mu < mu0, 3 para mu = mu0

(pValor=1-pt(Estadistico,df=k))
