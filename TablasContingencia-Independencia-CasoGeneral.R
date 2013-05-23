####################################################
#
# Tablas de contingencia generales, contraste ji cuadrado
# para la independencia de dos variables categóricas.
#
####################################################
# Los datos marginales de la tabla son:

factorA=c(244,91,40,138,125,63,43,124,91,140,44)
factorB=c(272,780,91)

# Podemos comprobar que son coherentes con el total

(total=sum(factorA))
(total=sum(factorB))

# A partir de ellos calculamos la tabla

(tablaEsperada=factorA %*% t(factorB) / sum(factorA))


# Y para hacerla más legible le añadimos etiquetas:

rownames(tablaEsperada)=c("Talamanca", "Ribatejada","Meco","Daganzo","CamarmaDaganzo","Camarma","Cobeña","Campo Real","Pinto","Torrejon","Estremera")
colnames(tablaEsperada)=c("MachosAdultos","Hembras","MachosJovenes")

# Y los valores marginales:

(addmargins(tablaEsperada))

# Ahora hacemos algo parecido con la tabla observada
# Pero esta vez partiendo de los datos "interiores"
# de la tabla.

(tablaObservada=cbind(c(53,16,10,18,34,17,4,38,28,37,17),c(177,68,30,108,79,41,27,74,57,95,24),c(14,7,0,12,12,5,12,12,6,8,3)))

# Y como antes le añadimos la decoración

rownames(tablaEsperada)=c("Talamanca", "Ribatejada","Meco","Daganzo","CamarmaDaganzo","Camarma","Cobeña","Campo Real","Pinto","Torrejon","Estremera")
colnames(tablaEsperada)=c("MachosAdultos","Hembras","MachosJovenes")
(addmargins(tablaEsperada))

# Ya estamos listos para aplicar el test a la tabla observada

(chisq.Observada=chisq.test(tablaObservada,correct=FALSE))


# Representación gráfica.
#
#
# Una posible representación de este contraste es en forma de gráfico de
# columnas apiladas (la altura indica el porcentaje), con una columna 
# por género. Para obtener esa representación empezamos por fabricar una 
# tabla de proporciones (porcentajes), en la que los porcentajes son 
# sobre el total de la muestra. Añadimos las sumas marginales para que
# sea más fácil entender esta tabla.

tablaProporciones1=prop.table(tablaObservada)
addmargins(tablaProporciones1)

# Para que cada columna de la tabla nos de las alturas de las columnas
# del gráfico, tenemos que dividir la tabla por las sumas por columnas,
# calculadas con colSums.
#
# Vamos a redondearla para mejorar la presentación.

(tablaProporciones2=tablaObservada/rowSums(tablaObservada))

t(tablaProporciones2)

# Ya podemos usar barplot para dibujar el grafico, con colores y etiquetas en los ejes. 

(bp_t2=barplot(t(tablaProporciones2),col=c("lightseagreen","darkgoldenrod1","red"),ylab="Edad",xlab="Zona",main="Avutardas en Madrid",font=2))

# La interpretación de un gráfico como este es mucho más fácil, si se rotulan las columnas
# con el porcentaje de una de las categoría en cada columna. Primero fabricamos los 
# rótulos, y luego los colocamos en sus posiciones con text.


# Por último, si tenemos dudas sobre las hipótesis de normalidad subyacentes al
# test chi cuadrado, podemos hacer un test no paramétrico para confirmar lo anterior. 

fisher.test(tablaObservada)









