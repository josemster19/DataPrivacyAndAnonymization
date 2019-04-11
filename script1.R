#Script de la pimera part de la practica 
#Autors: José Antonio Cegarra Alonso 1461305
#Autors: Juan Manuel Vallecillos Calzado 1401596

#instal·lar el paquet
install.packages("sdcMicro")

#Carrega de les llibreries
library(sdcMicro)

#Carregar les dades (Compte amb el path, en windows es diferent)
dades <- read.csv(file = "/data/salaris.csv", header=TRUE, sep=",", colClasses = c("CP"="character"))

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
d_subset = d[,c(1,3,5)]

