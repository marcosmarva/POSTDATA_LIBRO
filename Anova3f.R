(valle=gl(2,4*7,length=56,labels=c("ABIERTO","CERRADO")))
(orientacion=gl(2,2*7,length=56,labels=c("NORTE","SUR")))
(altura=gl(2,1*7,length=56,labels=c("ARRIBA","ABAJO")))
(liquen=runif(min=0,max=1,56))
datos=data.frame(liquen,valle,orientacion,altura)
View(datos)
modelo=lm( liquen ~ valle * orientacion * altura, data=datos)

replications(modelo,data=datos)
(isBalanced=!is.list(replications(modelo, data=datos)))


modelo.aov=aov( liquen ~ valle * orientacion * altura, data=datos)
summary(modelo.aov)

plot(modelo.aov, which = 1)

modeloVA.aov=aov( liquen ~ valle *  altura, data=datos)
summary(modeloVA.aov)
interaction.plot(datos$altura,datos$valle,datos$liquen)

modeloVO.aov=aov( liquen ~ valle *  orientacion, data=datos)
summary(modeloVO.aov)
interaction.plot(datos$orientacion,datos$valle,datos$liquen)


boxplot(datos$liquen ~ datos$valle)
boxplot(datos$liquen ~ datos$orientacion)
boxplot(datos$liquen ~ datos$altura)




?interaction.plot