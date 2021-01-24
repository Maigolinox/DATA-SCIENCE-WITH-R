#RETO 1 SESSION 4


##distribucion binomial
# Consideremos un experimento binomial con $n = 35$ pruebas idénticas e 
# independientes, en donde la probabilidad de éxito en cada prueba es 
# $p = 0.51$. Encuentre lo siguiente:
#   
# 1. La probabilidad de observar exactamente 10 éxitos
# 2. La probabilidad de observar 10 o más exitos
# 3. El cuantil de orden 0.5
# 4. Genere una muestra aleatoria de tamaño 1000 de esta
# distribución, construya una tabla de frecuencias relativas con los 
# resultados y realice el gráfico de barras de los resultados que 
# muestre las frecuencias relativas.

library(ggplot2)
#PUNTO 1
dbinom(x=10,size = 35,prob = 0.51)
#PUNTO 2
pbinom(q=9,size = 35,prob = 0.51,
       lower.tail = FALSE)
#PUNTO 3
qbinom(p = 0.5,size = 35,prob = 0.51)
# PUNTO 4
set.seed(123)
ale1000<- rbinom(n = 1000,size = 35,prob = 0.51)
class(ale1000)
ale1000dataaframe<-as.data.frame(table(ale1000)/length(ale1000))
head(ale1000dataaframe)
tail(ale1000dataaframe)
ggplot(ale1000dataaframe,
       aes(x = ale1000,y = Freq))+geom_bar(stat = "identity")



##DISTRIBUCION NORMAL
# Considere una variable aleatoria normal con media 110
#y desviación estándar 7. Realice lo siguiente:
# 1. Grafique la función de densidad de probabilidad
# 2. Encuentre la probabilidad de que la v.a. sea mayor o igual a 140
# 3. Encuentre el cuantil de orden 0.95
# 4. Genere una muestra aleatoria de tamaño 1000 y realice el histograma 
# de frecuencias relativas para esta muestra

#PUNTO 1

#GENERANDO LOS VALORES ALEATORIOS
vala1<- seq(10,100,by = 0.1)
View(vala1)
vala2<- dnorm(x = vala1,mean = 110,sd = 7)
class(vala1)
class(vala2)
df<-data.frame(vala1,vala2)
class(df)
View(df)
grafico<-ggplot(df,aes(vala1,vala2))+geom_line()


#PUNTO 2 PROBABILIDAD DE QUE VARIABLE ALEATORIA SEA MAYOR O IGUAL A 140
#LOWR TAIL SI ES VERDADERO LA PROBABILIDAD SERÁ P(X<=x) SI ES FALSO ES P(X>x)
pnorm(q = 140,mean = 110,sd = 7,lower.tail = FALSE)
#PUNTO 3. CUARTIL DE ORDEN 0.95
qnorm(p = 0.95,mean = 110,sd = 7)

#PUNTO 4.MUESTRA ALEATORIA DE TAMAÑO 1000 Y GENERE HISTOGRAMA DE FRECUENCIAS
set.seed(123)
datara<-rnorm(n = 1000,mean = 110,sd = 7)
dataradf<-as.data.frame(x = datara)
class(datara)
class(dataradf)
ggplot(data = dataradf,aes(datara))+
  geom_histogram(colour="blue",
                 fill="red",
                 alpha=0.6,
                 binwidth = 1.2)+
geom_density(aes(y=1.2*..count..))+
  geom_vline(xintercept = mean(datara),
             linetype="dashed", color = "black")+
  ggtitle(label = 'Histograma para la muestra normal')+
  labs(x='Valores obtenidos',y='Frecuencia')+
  theme_dark()+
  theme(plot.title = element_text(hjust = 0.5, size = 16))




