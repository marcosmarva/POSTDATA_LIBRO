##############################################################
#
# Estadística, Grado en Biología UAH
# Curso 2011/2012.
# Fichero de instrucciones R para dibujar
# un diagrama tipo histograma de los valores de probabilidad 
# de una distribucion biomial
#
##############################################################

# Introducimos los parametros de la binomial X de tipo  B(n,p)
n<-21
p<-1/3

# Se dibuja el diagrama

x<-dbinom(0:n,size=n,prob=p)
names(x)<-c(0:n)
barplot(x,n,legend.text=NULL,col="orange",space=0)