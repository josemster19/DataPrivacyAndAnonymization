# Script de la part opcional de la practica 
# Autors: José Antonio Cegarra Alonso 1461305
# Autors: Juan Manuel Vallecillos Calzado 1401596

# Instal·lació de paquets
install.packages("sdcMicro")

# Carrega de les llibreries
library(sdcMicro)

# Carregar les dades (Compte amb el path, en windows es diferent)
dades <- read.csv(file = "~/UNIVERSIDAD/3r Curso/2Semestre/GIS/Practica/Practica2/GIS/data/hipoteca.csv", header=TRUE, sep=",", colClasses = c("CP"="character"))

# Apartat b
# Primer eliminem els identificadors
dades_subset = dades[,c(2,4,5,6,7,8,9,10)]

# Generalitzem el CP

ProvinciaCP <- function(CP) {
  CP = substr(CP,1,2)
  padding = "000"
  CP = paste(CP,padding,sep="")
  return (CP)
}

# Creem un nou data frame i generalitzem el CP
CPGeneralitzat <- dades_subset
CPGeneralitzat$CP=ProvinciaCP(dades_subset$CP)


# Clasifiquem els treballs en branques
GeneralitzarOcupacio <- function(Ocupacio) {
  counter = 1
  value <- c()
  for(Oindv in Ocupacio){
    if(Oindv == levels(CPGeneralitzat$Ocupacio)[1]){
      value = append(value,"Informàtic")
    }
    if(Oindv == levels(CPGeneralitzat$Ocupacio)[2]){
      value = append(value,"Informàtic")
    }
    if(Oindv == levels(CPGeneralitzat$Ocupacio)[3]){
      value = append(value,"Tècnic")
    }
    if(Oindv == levels(CPGeneralitzat$Ocupacio)[4]){
      value = append(value,"Tècnic")
    }
    if(Oindv == levels(CPGeneralitzat$Ocupacio)[5]){
      value = append(value,"Medicina")
    }
    if(Oindv == levels(CPGeneralitzat$Ocupacio)[6]){
      value = append(value,"Medicina")
    }
    if(Oindv == levels(CPGeneralitzat$Ocupacio)[7]){
      value = append(value,"Informàtic")
    }
    if(Oindv == levels(CPGeneralitzat$Ocupacio)[8]){
      value = append(value,"Tècnic")
    }
    if(Oindv == levels(CPGeneralitzat$Ocupacio)[9]){
      value = append(value,"Mecànic")
    }
    if(Oindv == levels(CPGeneralitzat$Ocupacio)[10]){
      value = append(value,"Mecànic")
    }
    if(Oindv == levels(CPGeneralitzat$Ocupacio)[11]){
      value = append(value,"Mecànic")
    }
    if(Oindv == levels(CPGeneralitzat$Ocupacio)[12]){
      value = append(value,"Medicina")
    }
    if(Oindv == levels(CPGeneralitzat$Ocupacio)[13]){
      value = append(value,"Informàtic")
    }
    if(Oindv == levels(CPGeneralitzat$Ocupacio)[14]){
      value = append(value,"Tècnic")
    }
    counter= counter + 1
  }
  return (value)
}

CPGeneralitzat$Ocupacio = GeneralitzarOcupacio(CPGeneralitzat$Ocupacio)

# Afegim soroll en el Prob
prob_soroll <- addNoise(CPGeneralitzat,'Prob',15)
# ACtualitzem el CPGeneralitzat amb el soroll que hem afegit
# TODO Corregir q hay valores > 1
CPGeneralitzat$Prob <- prob_soroll$xm

# Fem rank swapping amb l'edat
CPGeneralitzat <- rankSwap(CPGeneralitzat,'Edat',P=10)
# Fem micro-agregacio amb edat i salari
microa <- microaggregation(CPGeneralitzat[,c('Edat','Salari')],aggr = 3, method = "onedims")
CPGeneralitzat$Edat = microa$mx$Edat
CPGeneralitzat$Salari = microa$mx$Salari
# Fem micro-agregacio amb nivell d'estudis i fills
microa2 <- microaggregation(CPGeneralitzat[,c('NumFills','NivellEstudis')],aggr = 3, method = "mdav")
CPGeneralitzat$NumFills = microa2$mx$NumFills
CPGeneralitzat$NivellEstudis = microa2$mx$NivellEstudis