#Script de la pimera part de la practica 
#Autors: José Antonio Cegarra Alonso 1461305
#Autors: Juan Manuel Vallecillos Calzado 1401596

#instalar el paquete
install.packages("sdcMicro")

#Carrega de les llibreries
library(sdcMicro)

#Carregar les dades 
dades <- read.csv(file = "/data/salaris.csv", header=TRUE, sep=",", colClasses = c("CP"="character"))

#Mostra de les dades
colnames(dades)
head(dades)
summary(dades)