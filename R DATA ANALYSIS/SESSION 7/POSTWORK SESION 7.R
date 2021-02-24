#POSTWORK SESION 7
# install.packages("mongolite")
# install.packages("data.table")
library(data.table)
library(mongolite)
#ES NECESARIO ISNTALAR MONGODB SHELL
#EJECUTAR EN SHELL mongod --port 27017
setwd("C:/Users/Victor Miguel Terron/Documents/PHASE2/DATA-SCIENCE-2PHASE/SESSION 7/")   #dependerá de donde estén guardados tus datos
match=data.table::fread("data.csv")
names(match)

my_collection = mongo(collection = "match", db = "match_games") # create connection, database and collection
my_collection$insert(match)  #insertando el CVS a la BDD


my_collection$count()
my_collection$find()

my_collection$find('{"date":"2015-12-20", "home_team":"Real Madrid"}')

my_collection = mongo(collection = "mtcars", db = "match_games") # create connection, database and collection
my_collection$insert(mtcars)

rm(my_collection)
