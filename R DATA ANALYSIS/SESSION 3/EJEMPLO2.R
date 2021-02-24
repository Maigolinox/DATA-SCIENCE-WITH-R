# EJMEPLO 2
#IMPORTAMOS LA LIBRERÍA
library(dplyr)
#REALIZAMOS LECTURA DEL CSV
data2 <- read.csv("C:/Users/Victor Miguel Terron/Documents/PHASE2/DATA-SCIENCE-2PHASE/SESSION 3/boxp.csv")
#VEMOS LOS PRIMEROS 6 DATOS DEL DATASET
head(data2)
#VEMOS LOS NOMBRES DE LAS COLUMNAS DEL DATASET
names(data2)
#GENERAMOS UN NUEVO DATASET DONDE MULTIPLICAMOS UNA CALUMNA
data <- mutate(data2, Mediciones = Mediciones*1.23)
#VEMOS EL CAMBIO
head(data)
#GRAFICAMOS EN FORMA DE HISTOGRAMA UTILIZANDO HIST()
hist(data$Mediciones, breaks = (seq(0,360, 20)), 
     main = "Histograma de Mediciones",
     xlab = "Mediciones",
     ylab = "Frecuencia")
#GRAFICAMOS UTILIZANDO GGPLOT
data <- na.omit(data) 

data %>%
  ggplot() + 
  aes(Mediciones) +
  geom_histogram(binwidth = 10)
 #MODIFICANDO UN POCO EL ESTILO
data %>%
  ggplot() + 
  aes(Mediciones) +
  geom_histogram(binwidth = 10, col="black", fill = "gray") + 
  ggtitle("Histograma de Mediciones") +
  ylab("Frecuencia") +
  xlab("Mediciones") + 
  theme_light()
