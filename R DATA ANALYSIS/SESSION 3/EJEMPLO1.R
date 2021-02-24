## EJEMPLO 1
library(ggplot2)
## RECORDAMOS LAS VARIABLES QUE CONTIENE EL DATASET MTCARS
names(mtcars)
##Graficamos las variables cyl en el eje x y hp en y, observa el comando ➡ geom_point()
ggplot(mtcars, aes(x=cyl, y = hp, colour = mpg )) + geom_point()
# Tipo de geometría, intenta utilizar alguna otra, se encentran en a cheat sheet

#AGREGANDO CARACTERÍSTICAS DE TEMA Y FACEWRAP
names(mtcars)
ggplot(mtcars, aes(x=cyl, y = hp, colour = mpg )) + 
  geom_point() +   
  theme_dark() +   # Temas (inteta cambiarlo)
  facet_wrap("cyl")  # Lo divide por el núm de cilindros
# CAMBIANDO LOS NOMBRES A LOS EJES X Y Y
names(mtcars)
ggplot(mtcars, aes(x=cyl, y = hp, colour = mpg )) + 
  geom_point() +   
  theme_dark() +   # Temas (inteta cambiarlo)
  facet_wrap("cyl") +  # Lo divide por el núm de cilindros
  xlab('Núm Acilindros')+  # Nombre en los ejes
  ylab('Caballos de Fuerza')
