fichero="http://www.fernandosansegundo.es/Estadistica/quinn.csv"
(quinn=read.table(fichero,header=T,sep=","))
class(quinn$SEASON)

class(quinn$DENSITY)

quinn$DENSITY = factor(quinn$DENSITY)
class(quinn$DENSITY)

replications(EGGS ~ SEASON * DENSITY, quinn)
(isBalanced=!is.list(replications(EGGS ~ DENSITY * SEASON, quinn)))
quinn.aov <- aov(EGGS ~ SEASON * DENSITY, data = quinn)
plot(quinn.aov, which = 1)
summary(quinn.aov)
interaction.plot(quinn$DENSITY, quinn$SEASON, quinn$EGGS)
interaction.plot(quinn$SEASON, quinn$DENSITY,  quinn$EGGS)
library(effects)
modelQuinn=lm(EGGS ~ DENSITY * SEASON,data=quinn)
modelQuinn.effect=effect("DENSITY:SEASON",modelQuinn,confidence.level=0.95)
plot(modelQuinn.effect)

TukeyHSD(quinn.aov, which = "SEASON")

TukeyHSD(quinn.aov, which = "DENSITY")

library(multcomp)
summary(glht(quinn.aov, linfct = mcp(DENSITY = "Tukey")))


centrosSpring=1:4
centrosSummer=5:8
bp=boxplot(EGGS ~ DENSITY * SEASON, quinn,boxwex=0.4,col=rep(c("burlywood4","paleturquoise3"),c(4,4)))
lines(centrosSpring,bp$stats[3,1:4],lwd=2,col="red")
lines(centrosSummer,bp$stats[3,5:8],lwd=2,col="blue")

?boxplot
#################################################################################
fichero="http://www.fernandosansegundo.es/Estadistica/quinn.csv"
(quinn=read.table(fichero,header=T,sep=","))
class()



data.ex5                                      #show the data
aov.ex5 = aov(Recall~(Task*Valence*Gender*Dosage)+Error(Subject/(Task*Valence))+(Gender*Dosage),data.ex5   )
summary(aov.ex5)
print(model.tables(aov.ex5,"means"),digits=3)       #report the means and the number of subjects/cell
boxplot(Recall~Task*Valence*Gender*Dosage,data=data.ex5) #graphical summary of means of the 36 cells
boxplot(Recall~Task*Valence*Dosage,data=data.ex5) #graphical summary of means of  18 cells





A=rnorm(1000)
B=sample(1:3,1000,replace=TRUE)
Bf=factor(B)
C=sample(1:2,1000,replace=TRUE)
Cf=factor(C)
datos=data.frame(A,Bf,Cf)
head(datos)


(bp=boxplot(A~Bf*Cf,at=centros,boxwex=0.2,col=rep(c("burlywood4","paleturquoise3"),c(3,3))))





?is.balanced