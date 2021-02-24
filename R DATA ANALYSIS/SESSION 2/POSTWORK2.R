#POST-WORK SESION 2
#CARGAMOS LIBRERIA DPLYR
suppressWarnings(suppressMessages(library(dplyr)))
#IMPORTANDO LOS DATOS
U1 <- "https://www.football-data.co.uk/mmz4281/1718/SP1.csv"
U2 <- "https://www.football-data.co.uk/mmz4281/1819/SP1.csv"
U3 <- "https://www.football-data.co.uk/mmz4281/1920/SP1.csv"
U4 <- "https://www.football-data.co.uk/mmz4281/1718/SP2.csv"
U5 <- "https://www.football-data.co.uk/mmz4281/1819/SP2.csv"
U6 <- "https://www.football-data.co.uk/mmz4281/1920/SP2.csv"
#IMPORTANDO LOS DATASETS
dataset1 <- read.csv(file = U1)
dataset2 <- read.csv(file = U2)
dataset3 <- read.csv(file = U3)
dataset4 <- read.csv(file = U4)
dataset5 <- read.csv(file = U5)
dataset6 <- read.csv(file = U6)
# VERIFICAMOS EL TIPO DE DATOS QUE HAY EN CADA DATASET USANDO STR, HEAD, VIEW, SUMMARY
str(dataset1)
str(dataset2)
str(dataset3)
str(dataset4)
str(dataset5)
str(dataset6)

head(dataset1)
head(dataset2)
head(dataset3)
head(dataset4)
head(dataset5)
head(dataset6)

View(dataset1)
View(dataset2)
View(dataset3)
View(dataset4)
View(dataset5)
View(dataset6)

summary(dataset1)
summary(dataset2)
summary(dataset3)
summary(dataset4)
summary(dataset5)
summary(dataset6)

#PUNTO 3 SELECT DATE,HOMETEAM,AWAYTEAM,FTGH, FTAG Y FTR PARA CADA DATAFRAME

lista <-list(dataset1,dataset2,dataset3,dataset4,dataset5,dataset6)
camposelect <- lapply(lista, select,Date,HomeTeam,AwayTeam,FTHG,FTAG,FTR)
View(camposelect)
#COMPROBAMOS LOS CAMBIOS QUE HIZO A LAS VARIABLES, DATE ES UN CHAR Y NO DATE
str(camposelect)
#HACEMOS LA CONVERSION DE DATE DE CHAR A DATE
camposelect[[1]]<-mutate(camposelect[[1]],Date=as.Date(Date,"%d/%m/%y"))
camposelect[[2]]<-mutate(camposelect[[2]],Date=as.Date(Date,"%d/%m/%y"))
camposelect[[3]]<-mutate(camposelect[[3]],Date=as.Date(Date,"%d/%m/%y"))
camposelect[[4]]<-mutate(camposelect[[4]],Date=as.Date(Date,"%d/%m/%y"))
camposelect[[5]]<-mutate(camposelect[[5]],Date=as.Date(Date,"%d/%m/%y"))
camposelect[[6]]<-mutate(camposelect[[6]],Date=as.Date(Date,"%d/%m/%y"))
#VERIFICAMOS EL CAMBIO DEL CAMPO DATE
str(camposelect)
#UNIMOS LOS DATAFRAMES EN UNO SOLO CON RBIND Y DO.CALL
unidos<-do.call(rbind,camposelect)
View(unidos)
#PODEMOS OBTENER MÁS INFORMACIÓN DEL DATAFRAME UNIDO POR EJEMPLO LOS PRIMEROS 6
head(unidos)
#LOS ULTIMOS 6
tail(unidos)
#RESUMEN
summary(unidos)
#DIMENSIONES
dim(unidos)
#VISUALIZARLO
View(unidos)
