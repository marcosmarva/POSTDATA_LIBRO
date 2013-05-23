rm(list=ls())

# Fijamos el directorio de tarbajo, que contiene los datos
setwd("C:/Dropbox/Clases/UAH/EstadisticaGradoBiologia/2012-2013-Teoria")

# Cargamos el fichero que contiene los valores de rendimiento como una única columna

robles=read.table(file="roblesR.csv",header=TRUE)

class(robles$Manganeso)

class(robles$Zona)
(robles$Zona=factor(robles$Zona))
class(robles$Zona)

class(robles$Tratamiento)
(robles$Tratamiento=factor(robles$Tratamiento))
class(robles$Tratamiento)

attach(robles)

# Definimos el modelo, con todas las interacciones posibles.

modelo.aov=lm( Manganeso ~ Zona * Tratamiento, data=robles)
summary(modelo.aov)

# Para asegurarnos, comprobamos que se trata de un diseño equilibrado

replications(modelo.aov,data=robles)
(isBalanced=!is.list(replications(modelo.aov, data=robles)))


# Y obtenemos su tabla ANOVA

anova(modelo.aov)

# Gráfica de los residuos, para comprobar las hipótesis del modelo

plot(modelo.aov, which = 1:2)

# Estos son los gráficos de interacción para cada par de factores

interaction.plot(Tratamiento,Zona,Manganeso,col=1:4,lwd=4)


# Alternativamente, los gráficos que proporciona el paquete effects 

library(effects)
modelo.effects=allEffects(modelo.aov,confidence.level=0.90)
plot(modelo.effects,rug=FALSE,x.var="Tratamiento")

# Y puesto que el resultado es significativo, podemos hacer comparaciones
# entre tratamientos, usando Tukey, LSD y Bonferroni del paquete agricolae

interact <- with(robles, interaction(Tratamiento, Zona))
amod <- aov(Manganeso ~ interact, data=robles)
library(agricolae)
(HSD=HSD.test(amod, "interact", group=TRUE))

HSD$groups[,3]

(LSD1=LSD.test(amod, "interact", group=TRUE,p.adj="none"))
LSD1$groups[,3]

(LSD2=LSD.test(amod, "interact", group=TRUE,p.adj="bonferroni"))
LSD2$groups[,3]

# También se pueden obtener detalles de los 
# tratamientos con tapply y summary

tapply(Manganeso,Tratamiento:Zona,summary)

