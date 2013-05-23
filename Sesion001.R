setwd("C:/Users/Fernando/Documents/Dropbox/Clases/EstadisticaGradoBiologia/Esquemas/")
datos <-read.table("GonickSmith-p009-GeneroPesoEdad.csv",header=TRUE, sep=",", na.strings="NA", dec=".")
attach(datos)
datos
table(Peso)
table(Genero)
table(Edad)
hist(Peso,breaks=10)
lista = seq(87.5, 222.5, by=15)
lista
hist(Peso,breaks=lista,col=heat.colors(5))
