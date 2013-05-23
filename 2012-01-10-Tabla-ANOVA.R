##############################################################
#
# Estadística, Grado en Biología UAH
# Curso 2011/2012.
# Fichero de instrucciones R para calcular
# la tabla ANOVA para un conjunto de datos.
# Se supone que:
#  -los datos estan en un fichero csv
#  -cada grupo de datos está en una columna
#  -en cada fila los elementos están separados por espacios (no tabuladores)
#  -los títulos de las columnas son G1, G2, G3, etcétera.
#  -todas las columnas tienen la misma longitud. Si faltan elementos, se 
#   sustituyen por "NA" (comillas incluidas)
##############################################################

#############################################################################
# NO OLVIDES CAMBIAR EL DIRECTORIO DE TRABAJO, AL QUE CONTIENE EL FICHERO csv
# Y CAMBIAR EL NOMBRE DEL FICHERO EN LA LINEA DEL read.table
#############################################################################
#Leemos los datos del fichero
datos<-read.table("2012-01-10-Datos-ANOVA.csv",sep=" ",header=T,na.strings="NA")
datos
attach(datos)

#Ahora generamos un data.frame, llamado tablaDatos, a partir de esos datos, y con la estructura adecuada.
Grupos<-c(G1,G2,G3,G4)
Nombres<-c(rep("G1",length(G1)),rep("G2",length(G2)),rep("G3",length(G3)),rep("G4",length(G4)))
tablaDatos<-data.frame(Grupos,Nombres)

#Mostramos el data.frame (se puede omitir. comentándolo)

# Y finalmente hacemos la tabla ANOVA
anova(lm(Grupos~Nombres,tablaDatos))
