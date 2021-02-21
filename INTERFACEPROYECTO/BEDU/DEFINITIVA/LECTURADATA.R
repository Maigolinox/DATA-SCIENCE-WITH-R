#LECTURA DE DATASET
#install.packages("forecast",dependencies = TRUE)
library(dplyr)
library(ggplot2)
library(forecast)
library(tseries)
options(scipen = 999)
#setwd("C:/Users/Victor Miguel Terron/Documents/PHASE2/DATA-SCIENCE-2PHASE/INTERFACEPROYECTO/BEDU/DEFINITIVA/")
datos<-read.csv("CONSOLIDADO.csv")
