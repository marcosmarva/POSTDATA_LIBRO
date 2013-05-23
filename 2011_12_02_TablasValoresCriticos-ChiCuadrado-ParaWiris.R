##############################################################
#
# Estadística, Grado en Biología UAH
# Curso 2011/2012.
# Fichero de instrucciones R para la generar tablas de valores críticos
# para la distribucion Chi cuadrado (tablaChi)
#
# Estos valores son útiles para construir intervalos de confianza
# y realizar contrastes de hipótesis.
##############################################################


################################################################
# Estos son los valores de probabilidad para los que se calculan 
# los correspondientes valores críticos
################################################################
nivelConfianza<-c(0.90,0.95,0.99)
tablaChi<-array(dim=c(1,length(nivelConfianza),0))
tablaChi<-rbind(tablaChi,nivelConfianza)
alfa<-1-nc
tablaChi<-rbind(tablaChi,alfa)
unomenosalfamedios<-1-alfa/2
#alfamedios<-alfa/2
tablaChi<-rbind(tablaChi,unomenosalfamedios)
#tablaChi<-rbind(tablaChi,alfamedios)
for(gradosLibertad in 5:15){
chi_unomenosalfamedios<-qchisq(df=gradosLibertad,1-unomenosalfamedios)
#chi_alfamedios<-qchisq(df=gradosLibertad,1-alfamedios)
tablaChi<-rbind(tablaChi,chi_unomenosalfamedios)
#tablaChi<-rbind(tablaChi,chi_alfamedios)
}
tablaChi




