tiradas=c()
for(i in 1:3000){
	jugada=sample(1:36,24,replace=T)
	jugada
	cuantos36=length(jugada[jugada==36])
	tiradas=c(tiradas,cuantos36)
	
}
tiradas
tiradas= (tiradas>=1)
#tiradas
table(tiradas)
table(tiradas)/length(tiradas)
