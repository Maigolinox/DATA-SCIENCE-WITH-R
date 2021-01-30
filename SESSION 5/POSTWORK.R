# POSTWEORK SESION 5

suppressWarnings(suppressMessages(library(dplyr)))
suppressWarnings(suppressMessages(library(fbRanks)))


url1718 <- "https://www.football-data.co.uk/mmz4281/1718/SP1.csv"
url1819 <- "https://www.football-data.co.uk/mmz4281/1819/SP1.csv"
url1920 <- "https://www.football-data.co.uk/mmz4281/1920/SP1.csv"
d1718 <- read.csv(file = url1718) # Importación de los datos a R
d1819 <- read.csv(file = url1819)
d1920 <- read.csv(file = url1920)


str(d1718); str(d1819); str(d1920)
head(d1718); head(d1819); head(d1920)
View(d1718); View(d1819); View(d1920)
summary(d1718); summary(d1819); summary(d1920)


lista <- list(d1718, d1819, d1920)
nlista <- lapply(lista, select, Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)

lapply(nlista, str)


nlista[[1]] <- mutate(nlista[[1]], Date = as.Date(Date, "%d/%m/%y"))
nlista[[2]] <- mutate(nlista[[2]], Date = as.Date(Date, "%d/%m/%Y"))
nlista[[3]] <- mutate(nlista[[3]], Date = as.Date(Date, "%d/%m/%Y"))


lapply(nlista, str)

data <- do.call(rbind, nlista)
dim(data)
str(data)
tail(data)
View(data)
summary(data)

SmallData <- select(data, date = Date, home.team = HomeTeam, 
                    home.score = FTHG, away.team = AwayTeam, 
                    away.score = FTAG)
write.csv(x = SmallData, file = "soccer.csv", row.names = FALSE)

listasoccer <- create.fbRanks.dataframes(scores.file = "soccer.csv")
anotaciones <- listasoccer$scores
equipos <- listasoccer$teams

fecha <- unique(anotaciones$date)
n <- length(fecha)
ranking <- rank.teams(scores = anotaciones, teams = equipos,
                      max.date = fecha[n-1],
                      min.date = fecha[1])


pred <- predict(ranking, date = fecha[n])

