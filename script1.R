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
dades_subset_comparar = dades_subset
dades_subset_comparar$CP = as.numeric(dades_subset_comparar$CP)

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

# Afegim soroll en el camp Edat
edat_soroll <- addNoise(dades_subset_comparar,'Edat',20)
# Copiem els camps salari i CP al nou data frame dades.an
dades.an = dades_subset_comparar
# Afegim la columna Edat amb el soroll
dades.an$Edat <- round(edat_soroll$xm)

# Apartat 3b
plot(cbind(dades$Edat, dades.an$Edat),
      ylim=c(min(dades$Edat),max(dades$Edat)),
      xlim=c(min(dades$Edat),max(dades$Edat)),
      xlab="Original", ylab="Masked", main="Additive Noise - Edat (P=0.20)")
abline(a=0, b=1, col="red")

# Fem el rank swap en el camp Edat
dades.rs <- rankSwap(dades_subset_comparar,'Edat',P=10)

# Apartat 4b
plot(cbind(dades$Edat, dades.rs$Edat),
     ylim=c(min(dades$Edat),max(dades$Edat)),
     xlim=c(min(dades$Edat),max(dades$Edat)),
     xlab="Original", ylab="Masked", main="Rank Swapping - Edat (P=0.10)")
abline(a=0, b=1, col="red")

# Pèrdua de informació per soroll aditiu 
dUtility(obj=dades_subset_comparar, xm=dades.an)

# Pèrdua de informació per rank swapping
dUtility(obj = dades_subset_comparar, xm=dades.rs)

# Risc de privacitat per a additive noise
dRisk(obj = dades_subset_comparar, xm=dades.an)

# Risc de privacitat per a rank swapping
dRisk(obj = dades_subset_comparar, xm=dades.rs)

# Afegim la microagregacio univariant
microa <- microaggregation(obj=dades_subset_comparar, variables = c("Edat","Salari") , aggr = 3, method = "onedims")
dades.microa = dades_subset_comparar
dades.microa$Edat <- round(microa$mx$Edat)
dades.microa$Salari <- round(microa$mx$Salari)

par(mfrow=c(2,2)) 

# Creem els 4 histogrames
hist(microa$x$Edat, main="Histograma Edat original",xlab="Edat",ylab="Freqüència")
hist(microa$mx$Edat, main="Histograma Edat amb microagregació",xlab="Edat",ylab="Freqüència")
hist(microa$x$Salari, main="Histograma Salari original",xlab="Salari",ylab="Freqüència")
hist(microa$mx$Salari, main="Histograma Salari amb microagregació",xlab="Salari",ylab="Freqüència")

# Afegim la microagrecacio multivariant
microa_mv <- microaggregation(obj=dades_subset_comparar, variables = c('Edat','Salari'),aggr = 3, method = "mdav")
dades.microamv = dades_subset_comparar
dades.microamv$Edat <- round(microa_mv$mx$Edat)
dades.microamv$Salari <- round(microa_mv$mx$Salari)

# Comparació dels 4 histogrames univariants i multivariants
hist(microa$mx$Edat, main="Histograma Edat univariant",xlab="Edat",ylab="Freqüència")
hist(microa_mv$mx$Edat, main="Histograma Edat multivariant",xlab="Edat",ylab="Freqüència")
hist(microa$mx$Salari, main="Histograma Salari univariant",xlab="Salari",ylab="Freqüència")
hist(microa_mv$mx$Salari, main="Histograma Salari multivariant",xlab="Salari",ylab="Freqüència")

# Calcul de la utilitat per a l'edat univariant
dUtility(obj=dades_subset_comparar, xm=dades.microa)

# Multivariant
dUtility(obj=dades_subset_comparar, xm=dades.microamv)

# Calcul del risc de privacitat per a l'edat univariant
dRisk(obj = dades_subset_comparar, xm=dades.microa)

# Multivariant
dRisk(obj = dades_subset_comparar, xm=dades.microamv)


# Creacio d'una funcio per generalitzar el codi postal l'input a de ser una String
ProvinciaCP <- function(CP) {
  CP = substr(CP,1,2)
  padding = "000"
  CP = paste(CP,padding,sep="")
  return (CP)
}

# Creem un nou data frame i generalitzem el CP
dades.CPGeneralitzat = dades_subset_comparar
dades.CPGeneralitzat$CP=as.numeric(ProvinciaCP(dades_subset$CP))

#dutility
dUtility(obj=dades_subset_comparar, xm=dades.CPGeneralitzat)
#drisk
dRisk(obj = dades_subset_comparar, xm=dades.CPGeneralitzat)


# Creem la funció d'abans pero modificada per si hi ha un registre únic camuflarlo
ProvinciaCPValorsUnics <- function(CP) {
  counter = 1
  for(CPindv in CP){
    if('1' %in% dades_CP_freq[CPindv]){
      CP[counter] = ProvinciaCP(CPindv)
    }
    counter= counter + 1
  }
  return (CP)
}

# Creem un nou data frame i generalitzem el CP
dades.CPGeneralitzatValorsUnics = dades_subset
dades.CPGeneralitzatValorsUnics$CP=as.numeric(ProvinciaCPValorsUnics(dades_subset$CP))

#dutility
dUtility(obj=dades_subset_comparar, xm=dades.CPGeneralitzatValorsUnics)
#drisk
dRisk(obj = dades_subset_comparar, xm=dades.CPGeneralitzatValorsUnics)


#Exercici 9
names <- c("AN", "RS", "MA one", "MA mul")
colors <- c("grey", "red", "blue", "green")
lty <- 1:4
lwd <- 3

dif_an <- abs(dades[, 'Edat']-dades.an[, 'Edat'])
dif_rs <- abs(dades[, 'Edat']-dades.rs[, 'Edat'])
dif_ma_one <- abs(dades[, 'Edat']-microa$mx[, 'Edat'])
dif_ma_mul <- abs(dades[, 'Edat']-microa_mv$mx[, 'Edat'])

ymin <- min(dif_an, dif_rs, dif_ma_one, dif_ma_mul)
ymax <- max(dif_an, dif_rs, dif_ma_one, dif_ma_mul)

par(mfrow=c(1,1))
plot(sort(dif_an), type="l", col=colors[1], lty=lty[1], lwd=lwd, ylim=c(ymin,ymax), xlab="
Registers", ylab="Error", main="Edat")
lines(sort(dif_rs), col=colors[2], lty=lty[2], lwd=lwd)
lines(sort(dif_ma_one), col=colors[3], lty=lty[3], lwd=lwd)
lines(sort(dif_ma_mul), col=colors[4], lty=lty[4], lwd=lwd)
legend(x="topleft", legend=names, col=colors, lty=lty, lwd=lwd)

