# Script de la part opcional de la practica 
# Autors: José Antonio Cegarra Alonso 1461305
# Autors: Juan Manuel Vallecillos Calzado 1401596

# Instal·lació de paquets
install.packages("sdcMicro")

# Carrega de les llibreries
library(sdcMicro)

# Carregar les dades (Compte amb el path, en windows es diferent)
dades <- read.csv(file = "~/Universidad/3ro/2do Semestre/GIS/GIS/data/hipoteca.csv", header=TRUE, sep=",", colClasses = c("CP"="character"))

# Apartat b
# Primer eliminem els identificadors
dades_subset_nopertorb = dades[,c(2,4,5,6,7,8,9,10)]
dades_subset_pertorb=dades_subset_nopertorb
# Generalitzem el CP
ProvinciaCP <- function(CP) {
  CP = substr(CP,1,2)
  padding = "000"
  CP = paste(CP,padding,sep="")
  return (CP)
}

# Creem un nou data frame i generalitzem el CP
dades_subset_pertorb$CP=ProvinciaCP(dades_subset_nopertorb$CP)
dades_subset_pertorb$CP=as.numeric(dades_subset_pertorb$CP)


# Clasifiquem els treballs en branques
GeneralitzarOcupacio <- function(Ocupacio) {
  counter = 1
  value <- c()
  for(Oindv in Ocupacio){
    if(Oindv == levels(dades_subset_pertorb$Ocupacio)[1]){
      value = append(value,"Informàtic")
    }
    if(Oindv == levels(dades_subset_pertorb$Ocupacio)[2]){
      value = append(value,"Informàtic")
    }
    if(Oindv == levels(dades_subset_pertorb$Ocupacio)[3]){
      value = append(value,"Tècnic")
    }
    if(Oindv == levels(dades_subset_pertorb$Ocupacio)[4]){
      value = append(value,"Tècnic")
    }
    if(Oindv == levels(dades_subset_pertorb$Ocupacio)[5]){
      value = append(value,"Medicina")
    }
    if(Oindv == levels(dades_subset_pertorb$Ocupacio)[6]){
      value = append(value,"Medicina")
    }
    if(Oindv == levels(dades_subset_pertorb$Ocupacio)[7]){
      value = append(value,"Informàtic")
    }
    if(Oindv == levels(dades_subset_pertorb$Ocupacio)[8]){
      value = append(value,"Tècnic")
    }
    if(Oindv == levels(dades_subset_pertorb$Ocupacio)[9]){
      value = append(value,"Mecànic")
    }
    if(Oindv == levels(dades_subset_pertorb$Ocupacio)[10]){
      value = append(value,"Mecànic")
    }
    if(Oindv == levels(dades_subset_pertorb$Ocupacio)[11]){
      value = append(value,"Mecànic")
    }
    if(Oindv == levels(dades_subset_pertorb$Ocupacio)[12]){
      value = append(value,"Medicina")
    }
    if(Oindv == levels(dades_subset_pertorb$Ocupacio)[13]){
      value = append(value,"Informàtic")
    }
    if(Oindv == levels(dades_subset_pertorb$Ocupacio)[14]){
      value = append(value,"Tècnic")
    }
    counter= counter + 1
  }
  return (value)
}

dades_subset_pertorb$Ocupacio = GeneralitzarOcupacio(dades_subset_pertorb$Ocupacio)

#NO TOCAR LA PROB DE HIPOTECA, ES LO PRINCIPAL DE ESTE DATA SET E INTERESA QUE SEA UN DATO VERIDICO
microa_mv <- microaggregation(dades_subset_pertorb, variables = c("NumFills","NivellEstudis") , aggr = 5, method = "mdav")
dades_subset_pertorb$NumFills = round(microa_mv$mx$NumFills)
dades_subset_pertorb$NivellEstudis = round(microa_mv$mx$NivellEstudis)

# Fem rank swapping amb l'edat
dades_subset_pertorb <- rankSwap(dades_subset_pertorb,'Edat',P=10)
edat_soroll <- addNoise(dades_subset_pertorb,'Edat',10)
dades_subset_pertorb$Edat = round(edat_soroll$xm)
# Afegim soroll al salari
sou_soroll <- addNoise(dades_subset_pertorb,'Salari',15)
dades_subset_pertorb$Salari = round(sou_soroll$xm)

#Microa eedat salari
microa_mv <- microaggregation(dades_subset_pertorb, variables = c("Edat","Salari") , aggr = 5, method = "mdav")
dades_subset_pertorb$Edat = round(microa_mv$mx$Edat)
dades_subset_pertorb$Salari = round(microa_mv$mx$Salari)


# Calcul de la utilitat extraer valores no numericos
dades_subset_nopertorb_nonumeric <-dades_subset_nopertorb[,c(2,3,4,5,7,8)]
dades_subset_nopertorb_nonumeric$CP = as.numeric(dades_subset_nopertorb_nonumeric$CP)
dades_subset_pertorb_nonumeric <-dades_subset_pertorb[,c(2,3,4,5,7,8)]
dUtility(obj=dades_subset_nopertorb_nonumeric, xm=dades_subset_pertorb_nonumeric)
dRisk(obj=dades_subset_nopertorb_nonumeric, xm=dades_subset_pertorb_nonumeric)
