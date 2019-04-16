#Script de la pimera part de la practica 
#Autors: José Antonio Cegarra Alonso 1461305
#Autors: Juan Manuel Vallecillos Calzado 1401596

#instal·lació de paquets
install.packages("sdcMicro")
install.packages("dplyr")

#Carrega de les llibreries
library(sdcMicro)
library(plyr)
library(dplyr)

#Carregar les dades (Compte amb el path, en windows es diferent)
dades <- read.csv(file = "~/UNIVERSIDAD/3r Curso/2Semestre/GIS/Practica/Practica2/GIS/data/salaris.csv", header=TRUE, sep=",", colClasses = c("CP"="character"))

#Mostra de les dades
colnames(dades)
head(dades)
summary(dades)

# Visualitzar els 10 primers valors
dades[1:10, "Edat"]

# Calcul mean i sd
print(paste("Atribut 'Edat': mean value =", mean(dades[, "Edat"]), "and SD =", sd(dades[, "Edat"]), sep=" "))

# Visualitzacio dels valors (ordenats)
plot(sort(dades[, "Edat"]), type="p", col="red", xlab="Registres", ylab="Valor", main="Edat")

# Creem un subset de dades eliminant els identificadors (DNI i nombre de la SS)
dades_subset = dades[,c(3,4,5)]

#Creem un subset amb la frequencia del CP
dades_CP_freq = plyr::count(dades_subset, 'CP')

#Creem un subset amb la frequencia de l'edat
dades_Edat_freq = plyr::count(dades_subset, 'Edat')

#Creem un subset amb la frequencia del salari
dades_Salari_freq = plyr::count(dades_subset, 'Salari')

#Creem un subset amb els valors únics del CP
dades_unique_CP = dplyr::filter(dades_CP_freq, freq == "1")

#Creem un subset amb els valors únics de l'Edat
dades_unique_Edat = dplyr::filter(dades_Edat_freq, freq == "1")

#Creem un subset amb els valors únics del Salari
dades_unique_Salari = dplyr::filter(dades_Salari_freq, freq == "1")
