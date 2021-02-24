# SESION 4 EJEMPLO 1
library(ggplot2) # Utilizaremos estos paquetes para algunas gráficas
library(reshape2)
#PARA ENCONTRAR LA PROBABILIDAD DE QUE SE ACIERTEN EXACTAMENTE 20
dbinom(x = 20, size = 30, prob = 0.2)
#PARA ENCONTRAR LA PROBABILIDAD DE QUE A LO MUCHO SE ACIERTEN 20
pbinom(x<=20,size = 30,prob = 0.2)
#dbinom te dice cual es la probabilidad de Pr(X=x)
#pbinom te dice cual es la probabilidad de Pr(X<=x)

#Para encontrar el valor más pequeño b tal que P(X <= b) >= 0.35, es decir, 
# el cuantil de orden 0.35, usamos
#CUANTILES
qbinom(p = 0.35, size = 30, prob = 0.2) # b = 5

pbinom(q = 4, size = 30, prob = 0.2) # P(X <= 4) = 0.2552 < 0.35
pbinom(q = 5, size = 30, prob = 0.2) # P(X <= 5) = 0.4275 >= 0.35
pbinom(q = 6, size = 30, prob = 0.2) # P(X <= 6) = 0.6070 >= 0.35

#MUESTRAS ALEATORIAS
# Para obtener una muestra aleatoria de tamaño n = 1000, 
# de la distribución binomial con parámetros como especificamos, hacemos

set.seed(4857) # Establecemos una semilla, para poder
# reproducir la muestra en el futuro
muestra <- rbinom(n = 1000, size = 30, prob = 0.2)
length(muestra); muestra[1:3]

# Podemos observar las frecuencias absolutas de los distintos valores obtenidos
as.data.frame(table(muestra))

#TAMBIEN PODEMOS OBSERVAR LAS FRECUENCIAS RELATIVAS
(df1 <- as.data.frame(table(muestra)/length(muestra)))

valg <- as.character(df1$muestra) # distintos valores generados por rbinom
(valg <- as.numeric(valg)) # Convertimos a números
#las frecuencias relativas son muy parecidas a las siguientes probabilidades
(v1 <- round(sapply(valg, dbinom, size = 30, p = 0.2), 3))

#COMBINAMOS df1 y v1 en un unico dataframe


(df2 <- cbind(df1, v1))
(names(df2) <- c("Exitos", "FR", "Prob"))

(df2 <- melt(df2)) # función del paquete reshape2

#LAS FRECUENCIAS RELATIVAS SON MUY PARECIDAS A LAS PROBABILIDADES
ggplot(df2, aes(x = Exitos, y = value, fill = variable)) + 
  geom_bar (stat="identity", position = "dodge") # Funciones del paquete ggplot2
#Se considera a y como value ya qwue si aplicamos el comando names
#al origen de los datos vemos que asi se llama la columna
names(df2)


#DISTRIBUCIÓN NORMAL

x <- seq(-4, 4, 0.01)*6 + 175 # Algunos posibles 
# valores que puede tomar la v.a. 
# X (mínimo: mu-4sigma, máximo: mu+4sigma)
y <- dnorm(x, mean = 175, sd = 6) # Valores 
# correspondientes de la función de
# densidad de probabilidad
plot(x, y, type = "l", xlab = "", ylab = "")#Te grafica 
# en forma de campana los valores de x y y 
title(main = "Densidad de Probabilidad Normal",
      sub = expression(paste(mu == 175, " y ", sigma == 6)))#Agrega leyendas
#a gráfico
abline(v = 175, lwd = 2, lty = 2) # La media es 175 grafica la línea

pnorm(q = 180, mean = 175, sd = 6)

par(mfrow = c(2, 2))#ESTABLECE EL LIENZO EN EL PLOT PARA GRAFICAR 
#VARIAS EN UN MISMO ESPACIO
plot(x, y, type = "l",
     xlab = "",
     ylab = "")
#AGREGA LEYENDAS AL GRÁFICO
title(main = "Densidad de Probabilidad Normal",
      sub = expression(paste(mu == 175,
                             " y ",
                             sigma == 6)))
polygon(c(min(x), 
          x[x<=180],
          180),
        c(0, y[x<=180], 0),
        col="blue")#GRAFICA DENTRO DE LA DISTRIBUCIÓN
#LA PROBABILIDAD DE QUE X TOME VALORES MENORES A 180

#para obtener la probabilidad de que x P(x<=165), es decir, la probabilidad de
#que X tome un valor menor o igual a 165, ejecutamos:

pnorm(q = 165, mean = 175, sd = 6)
plot(x,y,type="l",
     xlab = "",
     ylab = "")
title(main = "Densidad de probabilidad normail",
      sub = expression(paste(mu==175,"y",sigma==6)))
polygon(c(min(x),
          x[x<=165],165),
        c(0,
          y[x<=165],0),
        col = "black")


#PARA OBTENER P(165 <= X <= 180), es decir, la probabilidad de que X
#tome un valor mayor o igual a 165 y menor o igual a 180, debemos correr

pnorm(q = 180, mean = 175, sd = 6) - pnorm(q = 165, mean = 175, sd = 6)
plot(x,y,type="l",
     xlab = "",
     ylab = "")
title(main = "Densidad de probabilidad normail",
      sub = expression(paste(mu==175,"y",sigma==6)))
polygon(c(165,
          x[x>=165&x<=180],180),
        c(0,
          y[x>=165&x<=180],0),
        col = "yellow")

# Para obtener P(X >= 182), es decir, la probabilidad de que X tome un valor
# mayor o igual a 182, una alternativa es
plot(x,y,type="l",
     xlab = "",
     ylab = "")
title(main = "Densidad de probabilidad normail",
      sub = expression(paste(mu==175,"y",sigma==6)))
polygon(c(182,
          x[x>=182], max(x)),
        c(0,
          y[x>=182], 0),
        col="blue")


#Para encontrar el número b, tal que P(X <= b) = 0.75, es decir,
#el cuantil de orden 0.75, ejecutamos

(b <- qnorm(p = 0.75, mean = 175, sd = 6)) 


#COMPROBANDO CON FORMA COMUN
pnorm(b, 175, 6)
# El cuantil se encuentra en el eje de medición (eje horizontal)
plot(x, y,
     type = "l",
     xlab="",
     ylab="")
title(main = "Densidad de Probabilidad Normal",
      sub = expression(paste(mu == 175, " y ", sigma == 6)))
#COLOCA UNA LÍNEA DONDE SE ENCUENTRA EL CUANTIL
axis(side = 1, at = b, font = 2, padj = 1, lwd = 2)
# SIDE un entero que especifica donde se colocará la linea
#1 abajo 2 izquierda 3 arriva 4 derecha
#AT es donde se colcoará la linea
#padj ajusta a cada linea, 0 es arriba o derecha, 1 es izq o inferior

#muestras aleatorias
set.seed(7563) # Para poder reproducir la muestra en el futuro
muestra <- rnorm(n = 1000, mean = 175, sd = 6)
length(muestra); mdf <- as.data.frame(muestra)
tail(mdf)
# Observamos que el histograma de la muestra generada tiene 
# forma de campana similar a la densidad de una normal
ggplot(mdf, aes(muestra)) + 
  geom_histogram(colour = 'red', 
                 fill = 'blue',
                 alpha = 0.3, # Intensidad del color fill
                 binwidth = 3) + 
  geom_density(aes(y = 3*..count..))+
  geom_vline(xintercept = mean(mdf$muestra), linetype="dashed", color = "black") + 
  ggtitle('Histograma para la muestra normal') + 
  labs(x = 'Valores obtenidos', y = 'Frecuencia')+
  theme_dark() +
  theme(plot.title = element_text(hjust = 0.5, size = 16))  



mean <- 175; sd <- 6
x <- seq(mean-4*sd, mean+4*sd, 0.01)
y <- dnorm(x, mean, sd)
plot(x, y, type = "l",
     xlab="valores",
     ylab = "", 
     xaxt = "n",
     yaxt = "n")
title(main = "Densidad de Probabilidad Normal",
      sub = expression(paste("Regla Empírica con ",
                             mu == 175, 
                             " y ",
                             sigma == 6)))
abline(v=mean, lty = 2, lwd = 2)
for(k in c(-3, -2, -1, 1, 2, 3)) abline(v = mean+k*sd, lty = 2, col = abs(k))
ps <- c(mean - 3*sd, mean - 2*sd, 
        mean - sd, mean,
        mean + sd, 
        mean + 2*sd,
        mean + 3*sd)
axis(side = 1, at = ps)
x0 <- NULL
for(i in 1:length(ps)-1) x0 <- c(x0, (ps[i]+ps[i+1])/2)
y0 <- dnorm(x0, mean, sd)*1/3
text(x = x0, y = y0, labels = c("2.35%", "13.5%", "34%", "34%", "13.5%", "2.35%"))
x1 <- (x[1]+ps[1])/2; y1 <- dnorm(mean, mean, sd)*1/2
xf <- (x[length(x)]+ps[length(ps)])/2; yf <- dnorm(mean, mean, sd)*1/2
text(x = c(x1, xf), y = c(y1, yf), labels = c("0.15%", "0.15%"))
segments(x0 = x1, y0 = 0, x1 = x1, y1 = y1,               # Draw one line as in Example 1
         col = "cornflowerblue",                               # Color of line
         lwd = 5,                                              # Thickness of line
         lty = "dotted")     
segments(x0 = xf, y0 = 0, x1 = xf, y1 = yf,               
         col = "cornflowerblue",                               
         lwd = 5,                                              
         lty = "dotted")     

## DISTRIBUCION T DE STUDENT

x <- seq(-4, 4, 0.01) # Algunos valores que puede tomar la v.a. 
# T con 7 gl
y <- dt(x, df = 7) # Valores correspondientes de 
# la densidad t de Student con 7 gl
plot(x, y, type = "l", main = "Densidad t de Student, gl = 7", xlab="", ylab="")
abline(v = 0, lwd=2, lty=2)

#FUNCION DE DISTRIBUCIÓN
# Para encontrar P(T <= 1.5), ejecutamos la siguiente instrucción

plot(x, y, 
     type = "l",
     main = "Densidad t de Student, gl = 7",
     xlab="",
     ylab="")
polygon(c(min(x),x[x<=1.5], 1.5),
        c(0, y[x<=1.5], 0),
        col="purple")

plot(x, y, type = "l",
     main = "Densidad t de Student, gl = 7",
     xlab="", 
     ylab="")
polygon(c(2, x[x>=2], max(x)),
        c(0, y[x>=2], 0), 
        col="orange")


#CUANTILES
# Para encontrar el número d tal que P(T <= d) = 0.025, es decir, el cuantil de orden 0.025, corremos la siguiente instrucción

(d <- qt(p = 0.025, df = 7))
#comprobando
pt(q = d, df = 7)
# Mostramos el cuantil encontrado en el eje de medición (eje horizontal)

plot(x, y, 
     type = "l",
     main = "Densidad t de Student, gl = 7",
     xlab="",
     ylab="")
axis(side = 1,
     at = d,
     font = 2,
     padj = 1,
     lwd = 2)

# MUESTRAS ALEATORIAS 
set.seed(777) # Para poder reproducir la muestra en el futuro
muestra <- rt(n = 1000, df = 7)
length(muestra); mdf <- as.data.frame(muestra)
tail(mdf)
# Observamos que el histograma de la muestra generada tiene forma de campana similar a la densidad t de Student

ggplot(mdf, aes(muestra)) + 
  geom_histogram(colour = 'green', 
                 fill = 'orange',
                 alpha = 0.7, # Intensidad del color fill
                 binwidth = 0.5) + 
  geom_density(aes(y = 0.5*..count..))+
  geom_vline(xintercept = mean(mdf$muestra), 
             linetype="dashed",
             color = "black") + 
  ggtitle('Histograma para la muestra t de Student') + 
  labs(x = 'Valores obtenidos', y = 'Frecuencia')+
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, size = 16))  
