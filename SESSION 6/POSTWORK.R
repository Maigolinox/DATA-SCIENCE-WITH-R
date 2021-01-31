#POSTWORK SESION 6

library(dplyr)

datos <- read.csv("match.data.csv")


mutada <- datos %>% 
  mutate(date = as.Date(date, "%Y-%m-%d"),
         sumagoles = home.score + away.score) %>%
  mutate(Ames = format(date, "%Y-%m")) %>%
  group_by(Ames) %>%
  summarise(promgoles = mean(sumagoles))

(mutada <- as.data.frame(nd))


(mutada <- mutada %>% filter(Ames != "2013-06"))
(mutada <- mutada[1:95,])

(promgoles <- ts(mutada$promgoles, start = 1,
                 frequency = 10))


ts.plot(promgoles)
