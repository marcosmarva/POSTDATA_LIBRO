##############################################################
#
# Estadística, Grados en Biología y Biología Sanitaria, UAH
#
# Fichero de instrucciones R para calcular
# la tabla ANOVA para un conjunto de datos.
################################################	
#
# Se supone que:
#  -los datos estan en un fichero csv
#  -en cada fila los elementos están separados por espacios (no tabuladores)
#  -los títulos de las columnas aparecen en la primera fila (los titulos no incluyen espacios).
#  -no es necesario que todas las columnas tengan la misma longitud. Si faltan elementos, se
#   sustituyen por "NA" (comillas incluidas)
##############################################################

#############################################################################
# NO OLVIDES CAMBIAR EL DIRECTORIO DE TRABAJO, AL QUE CONTIENE EL FICHERO csv
# Y CAMBIAR EL NOMBRE DEL FICHERO EN LA LINEA DEL read.table
#############################################################################

#########################################
# LECTURA Y PREPARACIÓN DE DATOS
#########################################

#Limpiamos la memoria del sistema. Comentalo si no deseas hacer esto!!
rm(list=ls()) 

setwd("C:/Users/Fernando/Desktop")

#Leemos los datos del fichero. 

#Para ver alguno de los ejemplos de clase, descomentar esa línea.
#NombreFichero="Datos-ANOVA-01.txt"
#NombreFichero="Datos-ANOVA-02.txt"
#NombreFichero="Datos-ANOVA-03.txt"
#NombreFichero="Datos-ANOVA-04.txt"
#NombreFichero="Datos-ANOVA-05.txt"
#NombreFichero="Datos-ANOVA-06.csv"
#NombreFichero="Datos-ANOVA-07.csv"
#NombreFichero="InsectSprays.csv"  #Basado en un fichero de ejemplo incluido en R
#NombreFichero="medley2.csv"  ##Ejemplo Logan, Biostatistical design, pag265. Descomentar también más abajo.
NombreFichero="experimento-01.csv"  #Fichero del blog

##########################
#Recuerda que es ESENCIAL seleccionar  el valor correcto del separador de campos (comas, espacios)
##########################
separador=" "

# Este valor se utiliza a continuación para detectar el tipo de colocación de los datos en el fichero.
pruebaLectura=read.table(NombreFichero,sep=separador,header=T,na.strings="NA")

if(dim(pruebaLectura)[2]>2){
	(2+2)
	#(datos1=read.csv(NombreFichero,sep=separador,header=T,na.strings="NA"))
	numGrupos=dim(pruebaLectura)[2]
	Respuesta=c()
	Tratamiento=c()
	for(k in 1:numGrupos){
		Respuesta=c(Respuesta,pruebaLectura[,k])
		Tratamiento=c(Tratamiento, rep(colnames(pruebaLectura)[k],length(pruebaLectura[,k])) )	
	}
	datos=data.frame(Respuesta,Tratamiento)
} else {
	(datos=read.csv(NombreFichero,sep=separador,header=T,na.strings="NA"))
}
datos

if(class(datos[,2])=="factor"){datos=data.frame(datos[,2],datos[,1])}
colnames(datos)=c("Tratamiento","Respuesta")

# Si deseas hacer algún ajuste manual a los datos (por ejemplo, reordenar los niveles del factor), 
# este es posiblemente el mejor momento. Se incluye de uestra una modificacion adhoc para un 
# ejemplo de un libro.

##Ejemplo Logan, Biostatistical design, pag265. Descomentar también más abajo.
#datos$Tratamiento= factor(datos$Tratamiento, levels = c("HIGH", "MED","LOW", "BACK"), ordered = F)

#########################################
# ANÁLISIS DE LOS DATOS
#########################################

### Pedimos a R que calcule el ANOVA del modelo
(datos.aov=aov(Respuesta~Tratamiento,datos))


### Gráficos para el análisis mediante residuos de las hipótesis del modelo.

# Boxplots paralelos de los datos
boxplot(Respuesta~Tratamiento,datos,col=terrain.colors(5))

# También se debe comprobar que no hay una relación evidente entre la media y la varianza. Descomentar para usar.
#plot(tapply(datos$Respuesta,datos$Tratamiento, mean),tapply(datos$Respuesta, datos$Tratamiento, var),col="red",lwd=8,xlab="medias",ylab="varianzas")

# Descomentar la siguiente para un análisis gráfico de los residuos.
#plot(datos.aov)

# Y finalmente hacemos la tabla ANOVA
anova(lm(Respuesta~Tratamiento,datos))
# de otra manera (descomentar para usar)
# summary(datos.aov)

# Si se desea obtener un constraste no paramétrico de igualdad de medias, descomentar esto.
# oneway.test(Respuesta~Tratamiento,datos)

###############################################################
## Si el resultado es significativo
###############################################################

# En caso de un resultado significativo, podemos comparar los grupos dos a dos
summary(lm(Respuesta~Tratamiento,datos))

# Los p-valores de las comparaciones se obtienen con este comando:
pairwise.t.test(datos$Respuesta,datos$Tratamiento, p.adj="bonferroni")

# O también podemos usar un contraste no paramétrico de Tukey (descomentar)
#library(multcomp)
#summary(glht(datos.aov, linfct = mcp(Tratamiento = "Tukey")))


##########################################
## REPRESENTACIONES GRAFICAS DE LOS GRUPOS
##########################################

#Para que funciones el grafico 2 se debe descomentar el 1. 
#Pero si se quiere el 1, dejar el 2 comentado.

## Grafico 1
## Se muestran las nubes de puntos, sus medias e intervalos de confianza

#IntervaloConfMedia=function(x,nivelConfianza){
#  xbarra=mean(x)
#  s=sd(x)
#  n=length(x)
#  alfa=1-nivelConfianza
#  if(n>30){
#    valCritico=qnorm(1-alfa/2)
#  } else{
#    valCritico=qt(1-alfa/2,df=n-1)
#  }
#  c(-1,1)*valCritico*s/sqrt(n)+xbarra
#}
#
#
#(k=length(levels(datos$Tratamiento)))
#intervalos=c()
#for(i in 1:k){
#intervalos=rbind(intervalos,IntervaloConfMedia(datos[datos[,1]==levels(datos$Tratamiento)[i],2],0.95))
#}
#intervalos
#
#mediasMuestrales=tapply(datos$Respuesta,datos$Tratamiento, mean)
#cuasiDesvTipMuestrales=tapply(datos$Respuesta,datos$Tratamiento,sd)
#longitudesMuestrales=tapply(datos$Respuesta,datos$Tratamiento,length)
#sem=cuasiDesvTipMuestrales/sqrt(longitudesMuestrales)
#stripchart(datos$Respuesta~datos$Tratamiento,method="jitter",jitter=0.05, pch=16, vert=T)
#arrows(1:k,intervalos[,1],1:k,intervalos[,2],angle=90,code=3,length=.1,lwd=4,col="blue")
#lines(1:k,mediasMuestrales,pch=10,type="b",cex=2)


## Grafico 2

## Bargraphs "caseros"
#b=barplot(mediasMuestrales, ylim=c(min(intervalos)*0.8, max(intervalos)*1.2), xpd=F,col=heat.colors(5))
#arrows(b, intervalos[,2], b, intervalos[,1],angle=90, code=3,lwd=3)
#box(bty="c")

## Grafico 3

## Otra versión de bargraphs, seguramente más adecuada para un uso profesional, pero menos configurable.
# del libro Biostatistical Design and Analysis Using R, de M.Logan
# ver http://users.monash.edu.au/~murray/BDAR/index.html
#library(biology) 
#Mbargraph(datos$Respuesta, datos$Tratamiento, symbols = levels(Tratamiento), ylab = "Respuesta", xlab = "Tratamientos")


