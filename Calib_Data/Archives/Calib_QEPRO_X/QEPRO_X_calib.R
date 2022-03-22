QEPRO_X_calib <- function()
# Appel de la fonction:
# res <- Raman_X_calib()  
# Retourne dans "res", les coefficients de régression (degré 3) et un tableau des
# résultats de la recherche de pics.
# Nécessite la boîte outil pracma 
# B. Panneton, février 2020.

# plot(lmda_old,rawdats,type="l")
# abline(v=seq(200,900,20), col="pink")
# abline(v=seq(200,900,100), col="red")
# abline(h=810,col="blue")
  
#Ajoute points sur pics détectés sur figure
#Clic sur points ? éliminer
#Output: inclure R², rmse...
  
{
  #Pics calibrés
  
  # pics_calib <- c(794.8176,801.4786,826.4522,840.821,842.4648,
  #                 852.1442,866.7944,912.2967,922.4499,
  #                 935.422,965.779,978.45,1005.21)
  
  pics_calib <- c(794.8176,801.4786,811.464,826.4522,840.821,842.4648,
                  852.1442,866.7944,912.2967,922.4499,
                  935.422,965.779,978.508,1005.21)
  
  #pics_calib <- c(253.652,313.155,334.148,365.0153,404.6563,435.8328,491.607,
  #                546.0735,696.5431,727.2936,738.398,750.3869,763.5106,
  #                811.5311,826.4522,842.4648)
                  
#Charge la librairie pracma
  OK <- require(pracma)
  if (!OK){
    install.packages("pracma")
    library(pracma)
  }
  OK <- require(plotrix)
  if (!OK){
    install.packages("plotrix")
    library(pracma)
  }
  
  #Choisir un fichier de données de PolySpecteur et lire les données
  rawfile <- choose.files("*.txt",caption="Choisir le fichier de données")
  dats <- read.table(rawfile,header=F,sep="\t")
  lmda_old <- as.numeric(dats[1,2:ncol(dats)])
  rawdats <- dats[-1,2:ncol(dats)]
  #Si plus d'un spectre, faire la moyenne.
  if (nrow(rawdats)>1) rawdats<- colMeans(rawdats,na.rm = T)
  rawdats <- as.numeric(rawdats)
  x <- 0:(ncol(dats)-2)
  
  #Trouve les pics
  pics <- findpeaks(as.numeric(rawdats),npeaks = 2*length(pics_calib), minpeakheight = 0.01*max(rawdats))
  #Ajoute 2 colonnes pour stocker la position interpolée du pic et l'intensité  
  #correspondante.
  pics <- cbind(pics,rep(0,nrow(pics)),rep(0,nrow(pics)))
  
  lmda_new <- lmda_old[pics[,2]]
  indi <- numeric()
  for (k in pics_calib){
    dum <- which(abs(k-lmda_new)<2)
    indi <- c(indi,ifelse(length(dum)>0,dum[1],NA))
  }
  pics_calib <- pics_calib[!is.na(indi)]
  lmda_new <- lmda_new[indi]
  lmda_new <- lmda_new[!is.na(lmda_new)]
  pics <- pics[indi,]
  pics <- pics[!is.na(pics[,1]),]
  
  windows()
  par(mfrow=c(1,1))
  plot(lmda_old,rawdats,type="l",
       xlab="Longueur d'onde (approx. valeurs dans le spectro) [nm]",
       ylab="Intensité [U.A.]",
       main="Pics appariés - Étiquette sur fond rose")
  abline(h=0.02*max(rawdats),col="blue")
  abline(v=pics_calib,col="darkgreen")
  for (k in pics_calib) text(k, max(rawdats)/2, 
                             paste(rep("\U2588",nchar(as.character(k))), collapse = ""),
                             srt=90, col="pink")
  for (k in pics_calib) text(k, max(rawdats)/2, as.character(k), srt=90)
  
#Boucle sur les 12 pics pour estimer la position par interpolation quadratique
  nplots <- nrow(pics)
  rplot <- ceiling(sqrt(nplots))
  cplot <- 1 + nplots %/% rplot
  
  windows()
  par(mfrow=c(rplot,cplot))
  
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
    ymax <- 1.02*max(rawdats[indi])
    ymin <- min(rawdats[indi])
    plot(x[indi],rawdats[indi], type = "p", pch=21, col="black", bg="gray",
         ylim = c(ymin,ymax), xlab="Pixel",ylab="I")
    points(pics[k,5],pics[k,6],pch=21,bg="pink",col="red")
    xs <- seq(min(x[indi]),max(x[indi]),0.1)
    ys <- cfs[1] + cfs[2]*xs + cfs[3]*xs^2
    lines(xs,ys,lty=2,col="blue")
  }
  
  #Calcule la régression cubique pour la longueur d'onde en fonction du no de
  #pixel.
  lm3 <- lm(pics_calib ~ pics[,5] + I(pics[,5]^2) + I(pics[,5]^3))
  rmse <- sqrt(mean(lm3$residuals^2))
  adjR2 <- summary(lm3)$adj.r.squared
  
  #retourne les résultats (R², rmse)
  return(list(coefficients=coef(lm3),adjR2=adjR2,rmse=rmse,tableau=pics))
}