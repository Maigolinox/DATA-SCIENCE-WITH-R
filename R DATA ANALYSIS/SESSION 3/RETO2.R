#RETO 2
library(ggplot2)
datasetnba <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-con-R-Santander/master/Sesion-03/Reto-02/players_stats.csv")
names(datasetnba)
View(datasetnba)

#HISTOGRAMA DE LOS MINUTOS TOTALES UTILIZANDO GGPLOT PUNTO 1
mediap1<-mean(datasetnba$MIN)
ggplot(datasetnba,aes(MIN))+
  geom_histogram(binwidth = 34,col="black",fill="gray")+
  ggtitle("HISTOGRAMA DE MINUTOS POR JUGADOR",paste("Media= ",mediap1))+
  xlab("MINUTOS")+
  ylab("FRECUENCIA")+
  geom_vline(xintercept = mediap1,col="blue",lwd=1.5,lty=2)+
  theme_dark()
#HISTOGRAMA DE EDAD Y AGREGAR UNA LÍNEA CON LA MEDIA PUNTO 2
mean(datasetnba$Age)
mediap2<-mean(na.omit(datasetnba$Age))
ggplot(datasetnba,aes(Age))+
  geom_histogram(binwidth = 5,col="black",fill="gray")+
  ggtitle("EDADES DE JUGADORES",paste("Edad promedio: ",mediap2))+
  xlab("EDAD")+
  ylab("FRECUENCIA")+
  geom_vline(xintercept = mediap2,col="red",lwd=1,lty=1)+
  theme_dark()
#SCATTERPLOT DE Weigth y height y observar correlación entre variables PUNTO 3
co1 <- ggplot(datasetnba,aes(x=Weight,y = Height))+
  geom_point()+
  theme_dark()
co1
co2 <- coef(lm(Height~Weight,data=datasetnba ))
co2
co1+geom_abline(intercept = co2[1],slope = co2[2],col="blue",lwd=1,lty=2)
#ÉSTA ES OTRA FORMA DE REFERENCIAR EL ORIGEN DE LOS DATOS
# datasetnba%>% ggplot(aes(Weight,Height))+
#   geom_point()+
#   theme_dark()

#PUNTO 4 JUGADOR MÁS ALTO
taller<-which.max(datasetnba$Height)
#OBTENEMOS LA POSICIÓN POR LO QUE SE METE ENTRE CORCHETES 
#PARA QUE AUTOMÁTICAMENTE REGRESE EL DATO
paste("El jugador más alto es: ",datasetnba$Name[taller],"con una altura de: ",round(datasetnba$Height[taller]/100,2),"metros")

#PUNTO 5 JUGADOR MÁS BAJITO
smaller<-which.min(datasetnba$Height)
paste("El jugador más bajito es: ",datasetnba$Name[smaller],"Con una altura de",round(datasetnba$Height[smaller]/100,2)," metros")

#PUNTO 6 ALTURA PROMEDIO, COMO HAY NA ENTONCES UTILIZAMOS UNA DE LAS FUNCIONES PARA EVITARLO
altprom<-round(mean(na.omit(datasetnba$Height)/100),2)
altprom
paste("La altura promedio de los jugadores es: ",altprom,"metros")

#PUNTO 7 SCATTERPLOT DE ASISTENCIAS TOTALES VS PUNTOS CON FACCEWRAP
ggplot(datasetnba,aes(x=AST.TOV,y=PTS))+
  geom_point()+
  theme_dark()+
  facet_wrap("Pos")
#Un facewrap sirve para separar la gráfica y obtener información más detallada