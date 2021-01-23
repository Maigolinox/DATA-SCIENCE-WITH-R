#RETO 1 SESSION 5
a<-read.csv("datoslineal.csv")
#ATTACH sirve para llamar las columnas de un df como si fueran variables
attach(a)
#SIN ATTACH es necesario colocar el origen$variable
plot(a$x,a$y,main="GRAFICO DE DISPERSIÓN")
#MODELO DE REGRESION LINEAL
modelo1<-lm(a$y~a$x)
summary(modelo1)
plot(a$x,a$y,main="GRAFICO DE DISPERSIÓN",
     abline(lsfit(a$x,a$y)))
#GRAFICAS DE DIAGNOSTICO Y DIGA SI ES RAZONABLE SUPERPONER ERRORES
anova(modelo1)
plot(modelo1)
#ACOMODA LOS GRAFICOS EN UNA SOLA GRAFICA
par(mfrow=c(2,2))
plot(modelo1)
#dev.off borra el grid que teníamos anteriormente dentro de la sección de plot
dev.off()
# Diga si es razonable suponer para los errores aleatoriedad,
# normalidad y varianza constante.
summary(modelo1)
#