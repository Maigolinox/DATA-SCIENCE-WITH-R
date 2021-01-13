#POSTWORK SESION 1-VICTOR MIGUEL TERRON MACIAS
#1.Importa los datos de soccer de la temporada 2019/2020 de la primera división de
# la liga española a R, los datos los puedes encontrar en el siguiente enlace: 
# https://www.football-data.co.uk/spainm.php
datos<-read.csv("https://www.football-data.co.uk/mmz4281/1920/SP1.csv")
View(datos)

#2.Del data frame que resulta de importar los datos a R, extrae las columnas que 
# contienen los números de goles anotados por los equipos que jugaron en casa 
# (FTHG) y los goles anotados por los equipos que jugaron como visitante (FTAG)
colnames(datos)
#CON ESTO EXTRAIGO LAS COLUMNAS
CASA<-datos$FTHG
EXTE<-datos$FTAG
#MI PLUS DE COMPLEMENTO
EQUIPOCASA<-datos$HomeTeam
EQUIPOEXTERNO<-datos$AwayTeam
extraccion<-data.frame(EQUIPODECASA=EQUIPOCASA,GOLESCASA=CASA,EQUIPOINVITADO=EQUIPOEXTERNO,GOLESEXTERNOS=EXTE)
extraccion
#3.Consulta cómo funciona la función table en R al ejecutar en la consola ?table
#Posteriormente elabora tablas de frecuencias relativas para estimar las
#siguientes probabilidades:
#1.La probabilidad (marginal) de que el equipo que
#juega en casa anote x goles (x = 0, 1, 2, ...) 
#2.La probabilidad (marginal) 
#de que el equipo que juega como visitante anote y goles (y = 0, 1, 2, ...)
#3.La probabilidad (conjunta) de que el equipo que juega en casa anote x goles
#y el equipo que juega como visitante anote y goles 
#(x = 0, 1, 2, ..., y = 0, 1, 2, ...)
##CALCULANDO DE QUE EL EQUIPO DE CASA ANOTE X GOLES
fastercasa=(table(datos$FTHG)/dim(datos)[1])*100
fastercasa
View(fastercasa)
##CALCULANDO LA PROBABILIDAD DE QUE EL EQUIPO EXTERNO ANOTE X GOLES
fasterexterno=(table(datos$FTAG)/dim(datos)[1])*100
fasterexterno
View(fasterexterno)
#PROBABILIDADES CONJUNTAS
probconj=(table(datos$FTHG, datos$FTAG)/dim(datos)[1])*100
probconj
View(probconj)
