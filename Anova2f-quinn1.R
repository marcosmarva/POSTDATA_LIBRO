fichero="http://www.fernandosansegundo.es/Estadistica/quinn1.csv"
(quinn1=read.table(fichero,header=T,sep=","))
class(quinn1$SEASON)

class(quinn1$DENSITY)

quinn1$DENSITY = factor(quinn1$DENSITY)
class(quinn1$DENSITY)


boxplot(EGGS ~ DENSITY, quinn1)
boxplot(EGGS ~ SEASON, quinn1)
boxplot(EGGS ~SEASON * DENSITY, quinn1)


modelQuinn1=lm(EGGS ~ DENSITY * SEASON,data=quinn1)

replications(modelQuinn1,data=quinn1)

(isBalanced=!is.list(replications(modelQuinn1, data=quinn1)))

quinn1.aov = aov(EGGS ~ SEASON * DENSITY, data = quinn1)
summary(quinn1.aov)

plot(quinn1.aov, which = 1)

interaction.plot(quinn1$DENSITY, quinn1$SEASON, quinn1$EGGS)

library(effects)
modelQuinn1.effect=effect("DENSITY*SEASON",modelQuinn1,confidence.level=0.90)
plot(modelQuinn1.effect)

