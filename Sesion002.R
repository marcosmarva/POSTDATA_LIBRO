## ANTES DE EMPEZAR A TRABAJAR, RECUERDA QUE DEBES 
## FIJAR EL DIRECTORIO DE TRABAJO (DONDE ESTAN LOS DATOS)

# Leemos el fichero de datos, y lo cargamos en la variable datos.

datos <-read.table("2011-09-27-DatosParaBoxPlot.csv",header=TRUE)

# Y con esto asociamos el nombre del encabezamiento a una variable de R (llamada Valor)

attach(datos)

# Esta es la tabla de frecuencias de Valor

table(Valor)

#Aquí calculamos su mediana

median(Valor)

# Aquí el primer y tercer cuartil

quantile(Valor,0.25)
quantile(Valor,0.75)

# Si no indicamos valor, se muestran todos los cuartiles y el rango completo (de min a max). 

quantile(Valor)

# También se pueden calcular percentiles con el valor correspondiente

quantile(Valor,0.10)

#Y usando secuencias

quantile(Valor,seq(0,1,0.1))

#Finalmente un diagrama de caja y bigotes.

boxplot(Valor)
