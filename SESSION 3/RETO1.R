#RETO 1 SESSION 3
#Viendo el dataset tenemos la opción de que podemos limpiar los datos o especificando el caracter de separación
library(ggplot2)
var <- read.csv("C:/Users/Victor Miguel Terron/Documents/BD_Altura_Alunos.csv",sep = ";")
#VEMOS LAS COLUMNAS DEL DATAFRAME
names(var)
dim(var)
#UTILIZAMOS FUNCION HIST
hist(x = var$Altura,
     breaks = 20,
     main = "Histograma de alturas",
     xlab = "Frecuencia",
     ylab = "Altura",
     col="gray")
#UTILIZAMOS FUNCION GGPLOT, DENTRO DE AES VA EL NOMBRE DE LA COLUMNA DEL DATAFRAME
ggplot(var,aes(Altura))+
  geom_histogram(binwidth=4,col="black",fill="gray")+
  ggtitle("Histograma de mediciones")+
  ylab("Frecuencia")+
  xlab("Altura")+
  theme_dark()
