## ANTES DE EMPEZAR A TRABAJAR, RECUERDA QUE DEBES 
## FIJAR EL DIRECTORIO DE TRABAJO (DONDE ESTAN LOS DATOS)

# Leemos el fichero de datos, y lo cargamos en la variable datos.

datos <-read.table("2011-09-27-titanic2.csv",header=TRUE, sep=" ")

# Y ahora asociamos los nombre de las variables del encabezamiento a variables de R 
# (llamadas Age, Class, Index, Sex, Survived)

attach(datos)

# Ahora usamos la función summary con cada una de las variables, para obtener información básica.

summary(Age)
summary(Class)
summary(Sex)
summary(Survived)
