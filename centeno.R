rm(list=ls())

# Cargamos el fichero que contiene los valores de rendimiento como una única columna
fichero="http://www.fernandosansegundo.es/Estadistica/centeno.csv"
(rendimiento=scan(file=fichero))
class(rendimiento)

# Puesto que hemos leído la tabla como un vector, vamos a fabricar los vectores correspondientes a los factores
# usando la función gl para ello.

repli=4

nf_var=3
nf_fert=2
nf_ag=2

(n=repli*nf_ag*nf_fert*nf_var)

(agua=gl(2,repli*nf_var*nf_fert,length=n,labels=c("BAJO","ALTO")))

(variedad=gl(3,2,length=n,labels=c("I","II","III")))

(fertilizante=gl(2,1,length=n,labels=c("A","B")))

# Con esto podemos crear la estructura de datos que vamos a usar.

datos=data.frame(rendimiento,agua,variedad,fertilizante)
View(datos)

# Definimos el modelo, con todas las interacciones posibles.

modelo.aov=lm( rendimiento ~ agua * variedad * fertilizante, data=datos)

# Para asegurarnos, comprobamos que se trata de un diseño equilibrado

replications(modelo.aov,data=datos)
(isBalanced=!is.list(replications(modelo.aov, data=datos)))


# Y obtenemos su tabla ANOVA

anova(modelo.aov)

# Gráfica de los residuos, para comprobar las hipótesis del modelo

layout(matrix(c(1,2),1,2))
plot(modelo.aov,which=1:2)

# Estos son los gráficos de interacción para cada par de factores

layout(matrix(1:3,1,3)) #para mostrar los tres gráficos juntos
interaction.plot(datos$variedad,datos$fertilizante,datos$rendimiento,col=1:2,lwd=3)
interaction.plot(datos$variedad,datos$agua,datos$rendimiento,col=1:2,lwd=3)
interaction.plot(datos$fertilizante,datos$agua,datos$rendimiento,col=1:2,lwd=3)
layout(matrix(1,1))


#TukeyHSD(aov(modelo.aov))


# Alternativamente, los gráficos que proporciona el paquete effects 

library(effects)
modelo.effects=allEffects(modelo.aov,confidence.level=0.90)
plot(modelo.effects,rug=FALSE,x.var="variedad")

# Y puesto que el resultado es significativo, podemos hacer comparaciones
# entre tratamientos, usando Tukey del paquete agricolae

interact <- with(datos, interaction(agua, fertilizante,variedad))
amod <- aov(rendimiento ~ interact, data=datos)
library(agricolae)
HSD.test(amod, "interact", group=TRUE)

# También se pueden obtener detalles de los 
# tratamientos con tapply y summary

tapply(datos$rendimiento,datos$agua:datos$fertilizante:datos$variedad,summary)

# Y otra manera gráfica de hacer las comparaciones entre tratamientos, 
# mediante el paquete multcomp.

library(multcomp)
tuk <- glht(amod, linfct = mcp(interact = "Tukey"))
tuk.cld <- cld(tuk)   # Tarda un poco
opar <- par(mai=c(1.5,3,3,0.5),xpd=NA) #los márgenes del gráfico
layout(matrix(1:1,1,1))
plot(tuk.cld,col="lightseagreen")
par(opar)
