#RETO 1 SESSION 6
a<-read.csv("datoslineal.csv")
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
dev.off()
# Diga si es razonable suponer para los errores aleatoriedad,
# normalidad y varianza constante.