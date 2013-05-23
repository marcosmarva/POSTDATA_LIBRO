####################################################
#
# Tablas de contingencia 2x2, contraste ji cuadrado
#
####################################################
# Los datos marginales de la tabla son:

creencia=c(1864,588)
genero=c(1205,1247)

# Podemos comprobar que son coherentes con el total

(total=sum(creencia))
(total=sum(genero))

# A partir de ellos calculamos la tabla

(tablaEsperada=creencia %*% t(genero) / sum(genero))

# Y para hacerla más legible le añadimos etiquetas:

colnames(tablaEsperada)=c("H","M")
rownames(tablaEsperada)=c("CREY","NO_CREY")

# Y los valores marginales:

(addmargins(tablaEsperada))

# Ahora hacemos algo parecido con la tabla observada
# Pero esta vez partiendo de los datos "interiores"
# de la tabla.

(tablaObservada=rbind(c(849,1015),c(356,232)))

# Y como antes le añadimos la decoración

colnames(tablaObservada)=c("H","M")
rownames(tablaObservada)=c("CREY","NO_CREY")
(addmargins(tablaObservada))

# Ya estamos listos para aplicar el test a la tabla observada

(chisq.Observada=chisq.test(tablaObservada,correct=FALSE))

# Ahora vamos a repetir, a mano, algunas de las operaciones
# que han conducido a ese resultado. Al aplicar el test, R devuelve, 
# dentro de la variable chisq.Observada, una lista con todo lo 
# necesario. Accedemos a las componentes de esa lista con $. Por 
# ejemplo, la tabla de valores esperados se obtiene así:

(E=chisq.Observada$expected)

# La de valores observados así:

(O=chisq.Observada$observed)

# Con estas dos tablas los residuos se definen así:

(residuos=(O-E)/sqrt(E))

# Pero también se pueden obtener directamente haciendo esto:

chisq.Observada$res

# Para calcular el Estadístico de este contraste debemos 
# elevar al cuadrado los residuos,

(tablaChi2=residuos^2)


#  y Sumar todos los valores de la tabla.

addmargins(tablaChi2)

# Entonces el valor del estadístico es la suma total de la tabla, que 
# aparece en la esquina inferior derecha.

(estadistico=addmargins(tablaChi2)[3,3])

# Y como sólo hay un grado de libertad, el p-valor es

(pValor=1-pchisq(estadistico,df=1))

# Se debe comparar estos valores con los que obtuvimos usando chisq.test,
# y también se debe ejecutar chisq.test con correct=TRUE.

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

(tablaProporciones2=round(tablaObservada/colSums(tablaObservada),digits=1))

# Ya podemos usar barplot para dibujar el grafico, con colores y etiquetas en los ejes. 

(bp_t2=barplot(tablaProporciones2,col=c("lightseagreen","darkgoldenrod1"),xlab="Género",ylab="Creyente/No creyente",main="Creencias religiosas por género",font=2))

# La interpretación de un gráfico como este es mucho más fácil, si se rotulan las columnas
# con el porcentaje de una de las categoría en cada columna. Primero fabricamos los 
# rótulos, y luego los colocamos en sus posiciones con text.

rotulos=paste(format(100*signif(tablaProporciones2[1,],digits=2)),"% creyentes",sep="")
text(bp_t2, tablaProporciones2[1,]+0.05,rotulos , xpd = TRUE, font=2)


# Un problema como este, con una tabla de contingencia 2x2, también se puede abordar con
# un contraste de igualdad de proporciones para dos muestras. Vamos a hacerlo aquí 
# (con correct=FALSE), para que se vea que R devuelve el mismo valor del estadístico, 
# y el mismo p-valor.

(creyentesObs=tablaObservada[1,])
prop.test(creyentesObs,genero,correct=F)

# Por último, si tenemos dudas sobre las hipótesis de normalidad subyacentes al
# test chi cuadrado, podemos hacer un test no paramétrico para confirmar lo anterior. 

fisher.test(tablaObservada)









