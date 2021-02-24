#RETO 3
data<-read.csv("cbe.csv")
View(data)
class(data)
start(AP); end(AP); frequency(AP)
View(data)
datats <-ts(data[,3],start = 1958,frequency = 12)
#SE COLOCA EL ORIGEN DE DATOS Y EN LA TERCERA COLUMNA ES EL AÑO
#POR LO QUE SE SELECCIONA LA TERCER COLUMNA INICIANDO POR EL AÑO
#1958 Y COMO CADA AO TIENE 12 MESES SE COLOCA 12 EN FRECUENCIA
class(datats)
View(datats)

#REALIZANDO LA DESCOMPOSICIÓN MULTIPLICATIVA DE LA SERIE DE TIEMPO
datadecommulti <- decompose(datats,type = "mult" )
#GRAFICANDO LA DESCOMPOSICIÓN MULTIPLICATIVA
plot(datadecommulti,
     xlab="Tiempo",
     sub="Descomposición de los datos de producción de electricidad")
#GRAFICANDO COMPONENTES SEPARADOS
#GRAFICANDO COMPONENTE DE ESTACIONALIDAD
plot(datadecommulti$seasonal,col="blue",lwd=0.5,main="Componente de temporada",
     ylab="TENDENCIA",
     xlab="AÑO")
#GRAFICANDO COMPONENTE DE TENDENCIA
plot(datadecommulti$trend,col="black",lwd=1,
     main="Componente de tendencia",
     ylab="TENDENCIA",
     xlab="X")
#GRAFICANDO LA COMPONENTE ALEATORIA
plot(datadecommulti$random,col="black",lwd=1,
     main="Componente aleatoria",
     xlab="X",
     ylab="Y")
#GRAFICANDO COMPONENTE DE TENDENCIA POR ESTACIONALIDAD
plot(datadecommulti$trend*datadecommulti$seasonal,lwd=1,
     main="TENDENCIA*ESTACIONALIDAD",xlab="TENDENCIA",
     ylab="NUMEROS")
#SUPERPUESTOS COMPONENTES SUPERPUESTAS
Tendencia=datadecommulti$trend
Seasonal=datadecommulti$seasonal
Random=datadecommulti$random

#GRAFICANDO
ts.plot(cbind(Tendencia,Tendencia*Seasonal),
        main="Datos de producción de electricidad",
        xlab="Tiempo",
        ylab="Cantidad",       
        lty=1:2,
        sub="Tendencia con efectos estacionales superpuestos")
#RECUERDA QUE lty INDICA EL TIPO DE LINEA QUE SE COLOCA
#COMPROBANDO TENEMOS LO SIGUIENTE
Tendencia[7]*Seasonal[7]*Random[7]
datats[7]

Tendencia[100]*Seasonal[100]*Random[100]
datats[100]

