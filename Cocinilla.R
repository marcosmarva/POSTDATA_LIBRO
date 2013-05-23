rm(list=ls())
mu=sample(-10:10,1)
sigma=sample(0:10,1)
n=64
mu=13.6
sigma=4.3

iter=0
repeat{
iter=iter+1
germ=rnorm(n-1)
germ=c(germ,-sum(germ))
(germ=sort(germ))
germ[1]
germ[n]
(mean(germ))
(s=sd(germ))
length(germ)
##
muestra=germ*s+mu
mean(muestra)
(s=sd(muestra))

(discr=(muestra[n]-muestra[1])^2 + 2*(n-1)*(sigma^2-s^2))
if((discr>0)||(iter>100)){break}
}

( h=(1/2)*(muestra[n]-muestra[1]+sqrt(discr)) )
muestra[1]=muestra[1]+h
muestra[n]=muestra[n]-h
mu;mean(muestra)
sigma;sd(muestra)

library(TeachingDemos)
z.test(muestra,mu=12, alternative = c("two.sided"), sd = 4.3, conf.level = 0.95)

##A mano
(Est=(mu-12)/(sigma/sqrt(n)))
(pValor=2*(1-pnorm(Est)))






