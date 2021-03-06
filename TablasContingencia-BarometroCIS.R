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

# Y para hacerla m�s legible le a�adimos etiquetas:

colnames(tablaEsperada)=c("H","M")
rownames(tablaEsperada)=c("CREY","NO_CREY")

# Y los valores marginales:

(addmargins(tablaEsperada))

# Ahora hacemos algo parecido con la tabla observada
# Pero esta vez partiendo de los datos "interiores"
# de la tabla.

(tablaObservada=rbind(c(849,1015),c(356,232)))

# Y como antes le a�adimos la decoraci�n

colnames(tablaObservada)=c("H","M")
rownames(tablaObservada)=c("CREY","NO_CREY")
(addmargins(tablaObservada))

# Ya estamos listos para aplicar el test a la tabla observada

(chisq.Observada=chisq.test(tablaObservada,correct=FALSE))

# Ahora vamos a repetir, a mano, algunas de las operaciones
# que han conducido a ese resultado. Al aplicar el test, R devuelve, 
# dentro de la variable chisq.Observada, una lista con todo lo 
# necesario. Accedemos a las componentes de esa lista con $. Por 
# ejemplo, la tabla de valores esperados se obtiene as�:

(E=chisq.Observada$expected)

# La de valores observados as�:

(O=chisq.Observada$observed)

# Con estas dos tablas los residuos se definen as�:

(residuos=(O-E)/sqrt(E))

# Pero tambi�n se pueden obtener directamente haciendo esto:

chisq.Observada$res

# Para calcular el Estad�stico de este contraste debemos 
# elevar al cuadrado los residuos,

(tablaChi2=residuos^2)


#  y Sumar todos los valores de la tabla.

addmargins(tablaChi2)

# Entonces el valor del estad�stico es la suma total de la tabla, que 
# aparece en la esquina inferior derecha.

(estadistico=addmargins(tablaChi2)[3,3])

# Y como s�lo hay un grado de libertad, el p-valor es

(pValor=1-pchisq(estadistico,df=1))

# Se debe comparar estos valores con los que obtuvimos usando chisq.test,
# y tambi�n se debe ejecutar chisq.test con correct=TRUE.

# Representaci�n gr�fica.
#
#
# Una posible representaci�n de este contraste es en forma de gr�fico de
# columnas apiladas (la altura indica el porcentaje), con una columna 
# por g�nero. Para obtener esa representaci�n empezamos por fabricar una 
# tabla de proporciones (porcentajes), en la que los porcentajes son 
# sobre el total de la muestra. A�adimos las sumas marginales para que
# sea m�s f�cil entender esta tabla.

tablaProporciones1=prop.table(tablaObservada)
addmargins(tablaProporciones1)

# Para que cada columna de la tabla nos de las alturas de las columnas
# del gr�fico, tenemos que dividir la tabla por las sumas por columnas,
# calculadas con colSums.
#
# Vamos a redondearla para mejorar la presentaci�n.

(tablaProporciones2=round(tablaObservada/colSums(tablaObservada),digits=1))

# Ya podemos usar barplot para dibujar el grafico, con colores y etiquetas en los ejes. 

(bp_t2=barplot(tablaProporciones2,col=c("lightseagreen","darkgoldenrod1"),xlab="G�nero",ylab="Creyente/No creyente",main="Creencias religiosas por g�nero",font=2))

# La interpretaci�n de un gr�fico como este es mucho m�s f�cil, si se rotulan las columnas
# con el porcentaje de una de las categor�a en cada columna. Primero fabricamos los 
# r�tulos, y luego los colocamos en sus posiciones con text.

rotulos=paste(format(100*signif(tablaProporciones2[1,],digits=2)),"% creyentes",sep="")
text(bp_t2, tablaProporciones2[1,]+0.05,rotulos , xpd = TRUE, font=2)


# Un problema como este, con una tabla de contingencia 2x2, tambi�n se puede abordar con
# un contraste de igualdad de proporciones para dos muestras. Vamos a hacerlo aqu� 
# (con correct=FALSE), para que se vea que R devuelve el mismo valor del estad�stico, 
# y el mismo p-valor.

(creyentesObs=tablaObservada[1,])
prop.test(creyentesObs,genero,correct=F)

# Por �ltimo, si tenemos dudas sobre las hip�tesis de normalidad subyacentes al
# test chi cuadrado, podemos hacer un test no param�trico para confirmar lo anterior. 

fisher.test(tablaObservada)









