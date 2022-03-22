Raman_X_calib <- function()
# Appel de la fonction:
# res <- Raman_X_calib()  
# Retourne dans "res", les coefficients de régression (degré 3) et un tableau des
# résultats de la recherche de pics.
# Nécessite la boîte à outil pracma 
# B. Panneton, février 2020.
{
  #Pics calibrés
  pics_calib <- c(794.8176, 800.6157,801.4786,810.3693,
                  811.5311,826.4522,840.821,842.4648,
                  852.1442,866.7944,912.2967,922.4499)
  #Charge la librairie pracma
  OK <- require(pracma)
  if (!OK){
    install.packages("pracma")
    library(pracma)
  }
  
  #Choisir un fichier de données de PolySpecteur et lire les données
  rawfile <- choose.files("*.txt",caption="Choisir le fichier de données")
  dats <- read.table(rawfile,header=F,sep="\t")
  lmda_old <- as.numeric(dats[1,2:1025])
  rawdats <- dats[-1,2:1025]
  #Si plus d'un spectre, faire la moyenne.
  if (nrow(rawdats)>1) rawdats<- colMeans(rawdats)
  rawdats <- as.numeric(rawdats)
  x <- 0:1023
  
  #Trouve les pics
  pics <- findpeaks(as.numeric(rawdats),npeaks = 12, minpeakheight = 0.02*max(rawdats))
  #Ajoute 2 colonnes pour stocker la position interpolée du pic et l'intensité  
  #correspondante.
  pics <- cbind(pics,rep(0,nrow(pics)),rep(0,nrow(pics)))
  
  #Boucle sur les 12 pics pour estimer la position par interpolation quadratique
  for (k in 1:nrow(pics)){
    #Trouve 3 points les plus près de la valeur maxi
    maxi <- pics[k,1]
    indi <- order(rawdats[pics[k,3]:pics[k,4]]-maxi,decreasing = T)[1:3]
    indi <- sort((pics[k,3]:pics[k,4])[indi])
    
    #Calcule la régression
    lm1 <- lm (rawdats[indi] ~ x[indi] + I(x[indi]^2))
    
    #Calcule la position du pic et l'intensité correspondante
    cfs <- coef(lm1)
    pics[k,5] <- cfs[2]/-2/cfs[3]
    pics[k,6] <- cfs[1] + cfs[2]*pics[k,5] +cfs[3]*pics[k,5]^2
    
    #Fait un graphique pour montrer le résultat
    ymax <- 1.2*max(rawdats[indi])
    ymin <- min(rawdats[indi])
    plot(x[indi],rawdats[indi], type = "p", pch=21, col="black", bg="gray",
         ylim = c(ymin,ymax))
    points(pics[k,5],pics[k,6],pch=21,bg="pink",col="red")
    xs <- seq(min(x[indi]),max(x[indi]),0.1)
    ys <- cfs[1] + cfs[2]*xs + cfs[3]*xs^2
    lines(xs,ys,lty=2,col="blue")
  }
  
  #Calcule la régression cubique pour la longueur d'onde en fonction du no de
  #pixel.
  lm3 <- lm(pics_calib ~ pics[,5] + I(pics[,5]^2) + I(pics[,5]^3))
  
  #retourne les résultats
  return(list(coefficients=coef(lm3),tableau=pics))
}