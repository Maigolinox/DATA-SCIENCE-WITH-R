#RETO 2 SESION 4
# 1. Genere 1500 muestras de tamaño 67 de la distribución exponencial con
# parámetro 5
# 2. Obtenga las 1500 medias correspondientes a cada una de las muestras
# 3. Realice el histograma de frecuencias de las 1500 medias
# 4. Encuentre la media muestral y desviación estándar muestral de las 
# 1500 medias 
# 5. Compare la media muestral encontrada en el paso anterior con 
# la media real (1/5) de la población de la cual provienen las muestras 
# 6. Compare la desviación estándar muestral encontrada con la desviación
# estándar real (1/5) de la población de la cual provienen las muestras 
# pero dividida por 67 (el tamaño de las muestras)

library(ggplot2)
set.seed(123)#PARA QUE TODO EL TEAM GENERE LOS MISMOS VALORES
#PUNTO1
d1 <- sapply(X = rep(67, 1500), FUN = rexp, rate = 5)
class(d1)
#PUNTO 2
medd1<-apply(d1,2,mean)
#SE HACE LA CONVERSION A DATA FRAME
dfmedd1<-as.data.frame(medd1)
class(dfmedd1)
#PUNTO 3 HISTOGRAMA
graficop3<-ggplot(dfmedd1,aes(medd1))+
  geom_histogram(colour='blue',
                 fill='red',
                 alpha=0.5)+
  geom_vline(xintercept=mean(medd1),
             linetype="dashed",
             color="black")+
  ggtitle("Histograma 1500 medias")+
  labs(x="Media",y="Frecuencia")+
  theme(plot.title = element_text(hjust = 0.5, size = 16)) 

#PUNTO 4
mean(medd1)
sd(medd1)
#PUNTO 5
mean(medd1); 1/5 
#PUNTO 6
sd(medd1);(1/5)/sqrt(67)
