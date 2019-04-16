# Script de la pimera part de la practica 
# Autors: José Antonio Cegarra Alonso 1461305
# Autors: Juan Manuel Vallecillos Calzado 1401596

# Instal·lació de paquets
install.packages("sdcMicro")

# Carrega de les llibreries
library(sdcMicro)

# Carregar les dades (Compte amb el path, en windows es diferent)
dades <- read.csv(file = "~/UNIVERSIDAD/3r Curso/2Semestre/GIS/Practica/Practica2/GIS/data/salaris.csv", header=TRUE, sep=",", colClasses = c("CP"="character"))

# Mostra de les dades
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

# Trobem la freqüència per el CP per saber si hi ha algún individu que es pot identificar de manera única 
dades_CP_freq = table(dades_subset$CP)

# Trobem la freqüència per l'Edat per saber si hi ha algún individu que es pot identificar de manera única 
dades_Edat_freq = table(dades_subset$Edat)

# Trobem la freqüència per el Salari per saber si hi ha algún individu que es pot identificar de manera única 
dades_Salari_freq = table(dades_subset$Salari)

# Trobem la freqüència per a la combinació CP-Edat per saber si hi ha algún individu que es pot identificar de manera única 
dades_CP_Edat_freq = table(dades_subset$CP,dades_subset$Edat)

# Trobem la freqüència per a la combinació Edat-Salari per saber si hi ha algún individu que es pot identificar de manera única 
dades_Edat_Salari_freq = table(dades_subset$Edat,dades_subset$Salari)

# Trobem la freqüència per a la combinació CP-Salari per saber si hi ha algún individu que es pot identificar de manera única 
dades_CP_Salari_freq = table(dades_subset$CP,dades_subset$Salari)

# Trobem la freqüència per a la combinació CP-Edat-Salari per saber si hi ha algún individu que es pot identificar de manera única 
dades_CP_Edat_Salari_freq = table(dades_subset$CP,dades_subset$Edat,dades_subset$Salari)

# Comprovem que el valor 1 aparegui en les taules generades. Si apareix printem un missatge perr informar que es pot identificar de manera única 
if('1' %in% dades_CP_freq) {
  print("A la taula dades_CP_freq hi ha un individu que es pot identificar")
}
if('1' %in% dades_Edat_freq) {
  print("A la taula dades_Edat_freq hi ha un individu que es pot identificar")
}
if ('1' %in% dades_Salari_freq) {
  print("A la taula dades_Salari_freq hi ha un individu que es pot identificar")
}
if ('1' %in% dades_CP_Edat_freq) {
  print("A la taula dades_CP_Edat_freq hi ha un individu que es pot identificar")
}
if ('1' %in% dades_Edat_Salari_freq) {
  print("A la taula dades_Edat_Salari_freq hi ha un individu que es pot identificar")
}
if ('1' %in% dades_CP_Salari_freq) {
  print("A la taula dades_CP_Salari_freq hi ha un individu que es pot identificar")
}
if ('1' %in% dades_CP_Edat_Salari_freq) {
  print("A la taula dades_CP_Edat_Salari_freq hi ha un individu que es pot identificar")
}
