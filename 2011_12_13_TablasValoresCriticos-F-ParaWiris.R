##############################################################
#
# Estadística, Grado en Biología UAH
# Curso 2011/2012.
# Fichero de instrucciones R para la generar tablas de valores críticos
# para la distribucion F de Fisher-Snedecor (tablaF)
#
# Estos valores son útiles para construir intervalos de confianza
# y realizar contrastes de hipótesis.
##############################################################


################################################################
# Estos son los valores de probabilidad para los que se calculan 
# los correspondientes valores críticos
################################################################
tablaF<-array(dim=c(1,length(11),0))
#tablaChi<-rbind(tablaChi,nivelConfianza)
alfa<-0.005
#tablaChi<-rbind(tablaChi,alfa)
#unomenosalfamedios<-1-alfa/2
#alfamedios<-alfa/2
#tablaChi<-rbind(tablaChi,unomenosalfamedios)
#tablaChi<-rbind(tablaChi,alfamedios)
for(grLib1 in 5:15){
  F_alfa<-c()
  for(grLib2 in 5:15){
    F_alfa<-c(F_alfa,qf(1-alfa,df=grLib1,df2=grLib2))
  }
  tablaF<-rbind(tablaF,F_alfa)
}
alfa
tablaF

1-pf(tablaF[7,3],df1=11,df2=7)
