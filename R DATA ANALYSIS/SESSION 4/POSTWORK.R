#POSTWORK SESION 4

#Lo primero que haremos es cargar los paquetes que usaremos más adelante.
# Usamos las 
#funciones suppressWarnings y supperssMessages 
# para que no se impriman mensajes ni 
#advertencias al cargar el paquete.

suppressWarnings(suppressMessages(library(dplyr)))
suppressWarnings(suppressMessages(library(reshape2)))
suppressWarnings(suppressMessages(library(ggplot2)))

#IMPORTAR DATOS CSV A R

dataset1 <- "https://www.football-data.co.uk/mmz4281/1718/SP1.csv"
dataset2 <- "https://www.football-data.co.uk/mmz4281/1819/SP1.csv"
dataset3 <- "https://www.football-data.co.uk/mmz4281/1920/SP1.csv"
#LECTURA DE LOS DATASETS
lecdataset1 <- read.csv(file = dataset1) # Importación de los datos a R
lecdataset2 <- read.csv(file = dataset2)
lecdataset3 <- read.csv(file = dataset3)

#Obtenemos una mejor idea de los datos que se encuentran en los data frames
# con las funciones str, head, View y summary
#Obtenemos informacion sobre cada dataset
str(lecdataset1)
str(lecdataset2)
str(lecdataset3)
# head(lecdataset1)
# head(lecdataset2)
# head(lecdataset3)
#View(lecdataset1)
# View(lecdataset2)
# View(lecdataset3)
summary(lecdataset1)
summary(lecdataset2) 
summary(lecdataset3)

#seleccionaremos  las columnas Date, HomeTeam, AwayTeam,
# FTHG, FTAG y FTR en cada uno de los data frames. Primero guardaremos los 
#data frames en una lista con nombre lista y después con ayuda de las funciones 
#lapply y select (del paquete dplyr), seleccionaremos las columnas deseadas. 
#Los nuevos data frames quedarán guardados en nlista.
lista <- list(lecdataset1, lecdataset2, lecdataset3)
nlista <- lapply(lista, select, Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)

#Con las funciones lapply y str observaremos la estrucura de nuestros nuevos data frames
lapply(nlista, str)

#Arreglamos las columnas Date para que R reconozca los elementos como fechas,
# esto lo hacemos con las funciones mutate (paquete dplyr) y as.Date.
nlista[[1]] <- mutate(nlista[[1]], Date = as.Date(Date, "%d/%m/%y"))
nlista[[2]] <- mutate(nlista[[2]], Date = as.Date(Date, "%d/%m/%Y"))
nlista[[3]] <- mutate(nlista[[3]], Date = as.Date(Date, "%d/%m/%Y"))

#Verificamos que nuestros cambios se hayan llevado a cabo
lapply(nlista, str)

#Finalmente, con ayuda de las funciones rbind y do.call 
#combinamos los data frames contenidos en nlista como un único data frame
data <- do.call(rbind, nlista)
dim(data)
str(data)
tail(data)
View(data)
summary(data)

#Con ayuda de la función table obtenemos las estimaciones
# de probabilidades solicitadas
(pcasa <- round(table(data$FTHG)/dim(data)[1], 3)) # Probabilidades marginales 
#estimadas para los equipos que juegan en casa

(pvisita <- round(table(data$FTAG)/dim(data)[1], 3)) # Probabilidades marginales 
#estimadas para los equipos que juegan como visitante

(pcta <- round(table(data$FTHG, data$FTAG)/dim(data)[1], 3)) # Probabilidades
# conjuntas estimadas para los partidos

#Con la función apply  dividimos cada elemento de las columnas de la 
#matriz de probabilidades conjuntas, por las probabilidades marginales 
# asociadas 
#y que corresponden al equipo de casa. 
# Note como hemos definido una función anómima
# dentro de apply. 
# Luego dividimos cada elemento de las filas de la matriz que resulta,
# por las probabilidades marginales asociadas 
# y que corresponden al equivo visitante. 
#Finalmente obtenemos la transpuesta de la matriz que resulta. 
# Esta última matriz,
# es la matriz de cociente buscada, es decir, 
# hemos dividio cada probabilidad conjunta,
# por el producto de probabilidades marginales correspondientes.
(coci <- apply(pcta, 2, function(col) col/pcasa))

(coci <- apply(coci, 1, function(fila) fila/pvisita))

(coci <- t(coci))

# #Lo anterior igual lo pudimos lograr de la siguiente manera:
# 
# pcta/outer(pcasa, pvisita, "*")

#Primero extraemos de manera aleatoria algunas filas de nuestro data
# frame data, esto lo hacemos con ayuda de la función sample.
set.seed(123)
indices <- sample(dim(data)[1], size = 380, replace = TRUE)
newdata <- data[indices, ]

#Con ayuda de la función table obtenemos las estimaciones de probabilidades

(pcasa <- round(table(newdata$FTHG)/dim(newdata)[1], 3)) # Probabilidades marginales
# estimadas para los equipos que juegan en casa

(pvisita <- round(table(newdata$FTAG)/dim(newdata)[1], 3)) # Probabilidades 
#marginales estimadas para los equipos que juegan como visitante

(pcta <- round(table(newdata$FTHG, newdata$FTAG)/dim(newdata)[1], 3)) # Probabilidades
#conjuntas estimadas para los partidos

#Obtenemos nuevamente los coci de probabilidades conjuntas entre probabilidades marginales

(coci <- pcta/outer(pcasa, pvisita, "*"))

#Repita el remuestreo anterior varias veces (unas 1000 veces)
# y obtenga una idea de las distribuciones de los coci. 
#Finalmente mencione en cuales casos le parece razonable la suposición de que 
#el cociente es igual a 1.

