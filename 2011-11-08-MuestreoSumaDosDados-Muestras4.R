##############################################################
#
# Estadística, Grado en Biología UAH
# Curso 2011/2012.
# Fichero de instrucciones R para analizar
# la distribución muestral de la variable suma 
# en el lanzamiento de dos dados. Se van a obtener
# todas las muestras de tamaño n.
##############################################################

# Vamos a crear una matriz Muestras que
#


Muestras<-array(1,4)
Muestras
for(i1 in 1:36){
  for(i2 in 1:36){
    for(i3 in 1:36){
      for(i4 in 1:36){
        Muestras<-rbind(Muestras,c(i1,i2,i3,i4))
      }
    }
  }
}
Muestras<-Muestras[c(-1),]
dim(Muestras)

ffila<-function(x)((x-1)%/%6)+1
fcolumna<-function(x)(x-6*(ffila(x)-1))
sumafc<-function(x)ffila(x)+fcolumna(x)

resultados<-1:36

poblacion<-sumafc(resultados)
poblacion

mu<-mean(poblacion)
desvestPob<-sqrt(sum((poblacion-mu)^2)/length(poblacion))
desvestPob
desvestPob


mediasMuestrales<-c()
for(j in 1:1679616){
mediasMuestrales<-c(mediasMuestrales,mean(sumafc(Muestras[j,])))
}

mediasMuestrales

muMM<-mean(mediasMuestrales)
muMM

desvestMM<-sqrt(sum((mediasMuestrales-muMM)^2)/length(mediasMuestrales))
desvestMM

desvestPob

desvestPob/desvestMM
