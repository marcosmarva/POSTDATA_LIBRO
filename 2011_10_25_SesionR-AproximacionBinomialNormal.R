#setwd("C:/Users/Fernando/Documents/Dropbox/Clases/EstadisticaGradoBiologia/Recursos/AnimacionesR")
library("animation")

#oopt = ani.options(interval = 1, nmax = 100,outdir = getwd())
## use a loop to create images one by one
for (n in seq(1,ani.options("nmax"),2)) {
n<-150
p<-0.2
mu<-n*p
sigma<-sqrt(n*p*(1-p))

#x<-dbinom(0:n,size=n,prob=p)/n

x<-dbinom(0:n,size=n,prob=p)
names(x)<-c(1:n)
barplot(x,n,names.arg=NULL,legend.text=NULL,col="orange",space=0)
lim_y<-range(c(dnorm(0,mu,sigma),range(x)))
barplot(x,n,names.arg=NULL,legend.text=NULL,col="orange",plot=F,ylim<-lim_y)
par(new=TRUE)
u<-seq(0,n,0.01)
plot(u,dnorm(u,mu,sigma),lwd=4,col="blue",type="l",ylab="",yaxt="n")

#ani.pause() ## pause for a while (’interval’)
}

