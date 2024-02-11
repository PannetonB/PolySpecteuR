#************************************************************************************
#************************************************************************************
#                   INTERFACE R vers OmniDriver de Ocean Optics
#                              Bernard Panneton
#                               Février 2019
#
#
# OOInterface.R est une librairie qui comprend des fonctions pour communiquer avec les 
# spectros Ocean Optics à travers la librairie Java OmniDriver de Ocean Optics. Seules
# quelques fonctions ont été créées mais en se référant à ce qui est défini, il est assez
# facile de créer d'autres fonctions R qui peuvent appeler d'autres fonctions de OmniDriver.
#
# 
# Au coeur de cette librairie est la liste leSpectro créée avec la fonction 
# Define_Spectro. Cette liste comprend différents paramètres permettant de définir un
# spectro: 
#         number  : le numéro par lequel on y réfère; 
#         name    : le nom tel que retourne GetName() d'OmniDriver; 
#         xaxis   : les coordonnées des pixels telles que stokées dans le spectro; 
#         isQEPRO : un "boolean" pour identifier les QEPRO qui doivent gérés
#                   différemment à cause de leur "buffer" interne; 
#       wv ou wn  : les valeurs de longueur d'ondes (wv) ou de nombre d'onde (wn) auxquelles
#                   on interpole les spectres; 
#          xunit :  "wavelength" ou "wavenumber". 
#
# Pour les QEPROs, la liste contient aussi l'élément dat_buffer qui est un
# objet de OmniDriver pour gérer le "buffer" d'acquisition des QEPROs. Pour les spectros
# pouvant être refroidis, on peut aussi ajouter un objet TEC (Thermo Electric Cooling)
# à la liste avec la fonction GetTEC.
# 
# Une autre liste importante est la liste créée par Start_OO(). Cette liste a deux
# éléments: lesspectros-la liste des noms des spectromètres détectés; mywrap-l'objet
# wrapper de la librairie OmniDriver qui donne accès aux fonctions
# d'Ocean Optics.
# 
# Des explications sont données pour chaque fonction.
#************************************************************************************
# Auteur: Bernard Panneton, consultant
# Février 2019
# Droits d'auteur régis par la licence GPL-3 de R. Pour plus d'informations,
# exécuter la commande RShowDoc("GPL-3") dans R.
# GNU GENERAL PUBLIC LICENSE
# Version 3
#************************************************************************************
#

#*****************************************************************************
#On s'assure que rJava est chargée. La librairie OmniDriver est en Java!
#************************************************************************************
ok <- require(rJava)
if (!ok){
  install.packages("rJava")
  require(rJava)
}
#************************************************************************************
#


#*****************************************************************************
# OOobj <- Start_OO()
#
# Start_OO() crée un objet mywrap qui est un wrapper pour accéder aux fonctions
# de OmniDriver. Elle dresse aussi la liste des spectros détectés.
#************************************************************************************
# 
# ENTRÉES
#             Nil
#             
# SORTIES
#             Une liste comprenant 3 éléments: 
#                     1 - une liste des noms des spectros détectés;
#                     2 - l'objet mywrap
#                     3 - une liste des numéros de série dans l'ordre des spectros
#                         détectés.
#             NULL si aucun spectro n'est détecté.
#************************************************************************************
Start_OO <- function(){
  #Init. l'objet wrapper d'OO et dresse la liste des spectros disponibles
  #La fonction retourne la liste ou un message d'erreur.
  ooi_home <- Sys.getenv("OOI_HOME")
  mypath <- file.path(ooi_home,"OmniDriver.jar")
  .jinit()
  .jaddClassPath(mypath)
  mywrap <- .jnew("com.oceanoptics.omnidriver.api.wrapper.Wrapper") 
  Sys.sleep(0.3)
  nbspectro <- .jcall(mywrap,"I","openAllSpectrometers")
  #Construit une liste de spectros 
  spectrolst <- NULL
  seriallst <- NULL
  if (nbspectro>0){
    for (k in 1:nbspectro){
      spectrolst <- c(spectrolst,.jcall(mywrap,"S","getName",as.integer(k-1)))
      seriallst <- c(seriallst,.jcall(mywrap,"S",'getSerialNumber',as.integer(k-1)))
    }
    return(list(lesspectros=spectrolst,mywrap=mywrap,serial_no=seriallst))
  }
  else{
    cat("Pas de spectro rattaché\n")
    return(NULL)
  }
}
#************************************************************************************
#

#*****************************************************************************
# leSpectro <- Define_Spectro(OOobj,leno=NULL)
#
# Define_Spectro crée une liste permettant de définir un spectro en particulier
# et de définir quelques-uns de ses paramètres. Cette liste est modifiée par d'autres
# fonctions.
#************************************************************************************
# ENTRÉES
#             OOobj   : liste créée par Start_OO()
#             leno    : optionnel. Si absent, on demande d'identifier le numéro
#                       du spectro à la console. Si présent, doit être un
#                       numéro de spectro valide.
#             
# SORTIES
#             Une liste comprenant 5 ou 6 éléments: 
#                   1 - number est le numéro par lequel on va réferer au spectro; 
#                   2 - le nom du spectro; 
#                   3 - l'axe des x tel que stocké dans le spectro; 
#                   4 - le nombre maximal de 'counts' (range)
#                   5 - IsQEPRO, TRUE pour un QEPRO et FALSE autrement; 
#                   6 - dat_buffer un objet pour gérer le buffer d'acquisition d'un QEPRO.
#                       Retourné seulement si IsQEPRO=TRUE
#************************************************************************************
Define_Spectro <- function(OOobj,leno=NULL)
{
  #Pour définir un spectro à partir de la liste des spectros raccordés.
  leSpectro <- list()
  if (is.null(leno)){
    laliste <- NULL
    for (k in 1:length(OOobj$lesspectros)){
      laliste <- paste(laliste,"\n",as.character(k),": ",OOobj$lesspectros[k]," - S/N: ", OOobj$serial_no[k],sep="")
    }
    leSpectro$number <- as.integer(readline(paste("Choisir un spectro par son numéro:\n",laliste,"\n",sep="")))
  }else
    leSpectro$number <- leno
  
  leSpectro$name <- OOobj$lesspectros[leSpectro$number]
  leSpectro$number <- leSpectro$number-1
  leSpectro$xaxis <- OOobj$mywrap$getWavelengths(as.integer(leSpectro$number))
  leSpectro$range <- OOobj$mywrap$getMaximumIntensity(as.integer(leSpectro$number))
  
  if ('QE-PRO' %in% leSpectro$name){
    leSpectro$isQEPRO<-TRUE
  } else {
    leSpectro$isQEPRO<-FALSE
  }
  if (leSpectro$isQEPRO){    #Définir l'objet pour contrôler le buffer 
    leSpectro$dat_buffer<-OOobj$mywrap$getFeatureControllerDataBuffer(as.integer(leSpectro$number))
    Sys.sleep(0.2)
    leSpectro$dat_buffer$abortAcquisition()
    Sys.sleep(0.2)
  }
  return(leSpectro)
}
#************************************************************************************
#

#*****************************************************************************
# leSpectro <- Define_WavelengthRange(leSpectro,wv_min,wv_max,delta_wv)
#
# Define_WavelengthRange ajoute à une liste créée par Define_Spectro 2 éléments:
# wv-un vecteur de longueurs d'onde servant pour interpoler des spectres et 
# x-unit qui prend la valeur "wavelength" indiquant qu'on travaille en longueur
# d'onde. Si la liste comprend un membre du nom de "wn", ce membre est retiré
# parce qu'on peut pas à la fois être en longueur d'onde et en nombre d'onde.
# On enlève aussi le membre lambda0 qui n'est utile que lorsqu'on travaille
# en nombre d'onde.
# 
# ENTRÉES
#             leSpectro   : liste créée par Define_Spectro
#             wv_min      : longueur d'onde minimale
#             wv_max      : longueur d'onde maximale
#             delta_wv    : pas de longueur d'onde.
#             
# SORTIES
#             Liste leSpectro modifiée.
#************************************************************************************
Define_WavelengthRange <- function(leSpectro,wv_min,wv_max,delta_wv)
{
  #Pour définir la plage et l'incrément du vecteur de longueur d'onde pour l'interpolation des spectres
  leSpectro$wv <- seq(from=wv_min,to=wv_max,by=delta_wv)
  leSpectro$xunit <- "wavelength"
  compo<-names(leSpectro)
  if (length(grep("wn",compo))>0){
    leSpectro$wn <- NULL #peut pas être wavelength et wavenumber
    leSpectro$lambda0 <- NULL
  }
  if (length(grep("yint",compo))>0) leSpectro$yint <- NULL
  return(leSpectro)
}
#************************************************************************************
#

#*****************************************************************************
# leSpectro <- Define_WavenumberRange(leSpectro,wn_min,wn_max,delta_wn,lambda0)
#
# Define_WavenumberRange ajoute à une liste créée par Define_Spectro 2 éléments:
# wn-un vecteur de nombres d'onde servant pour interpoler des spectres et 
# x-unit qui prend la valeur "wavenumber" indiquant qu'on travaille en nombre
# d'onde. Si la liste comprend un membre du nom de "wv", ce membre est retiré
# parce qu'on peut pas à la fois être en longueur d'onde et en nombre d'onde.
# 
# ENTRÉES
#             leSpectro   : liste créée par Define_Spectro
#             wn_min      : longueur d'onde minimale
#             wn_max      : longueur d'onde maximale
#             delta_wn    : pas de longueur d'onde.
#             lambda0     : longueur d'onde de référence pour calculer les
#                           nombres d'onde (Pour Raman, longueur d'onde du laser)
#             
# SORTIES
#             Liste leSpectro modifiée incluant la valeur lambda0
#************************************************************************************
Define_WavenumberRange <- function(leSpectro,wn_min,wn_max,delta_wn,lambda0)
{
  #Pour définir la plage et l'incrément du vecteur de nombre d'onde pour l'interpolation des spectres
  leSpectro$wn <- seq(from=wn_min,to=wn_max,by=delta_wn)
  leSpectro$xunit <- "wavenumber"
  leSpectro$lambda0 <- lambda0
  compo<-names(leSpectro)
  if (length(grep("wv",compo))>0) leSpectro$wv <- NULL #peut pas être wavelength et wavenumber
  if (length(grep("yint",compo))>0) leSpectro$yint <- NULL
  return(leSpectro)
}
#************************************************************************************
#

#*****************************************************************************
# Close_Spectro(leSpectro)
#
# Close_Spectro détruit la liste définissant un spectro.
# 
# ENTRÉES
#             leSpectro   :   liste créée par Define_Spectro
#                     e   :   environnement où leSpectro est défini.
#                             Pas nésessaire si défini dans environnement courant.
#             
# SORTIES
#             nil
#************************************************************************************
Close_Spectro<-function(leSpectro,e=NULL)
{
  dum <- deparse(substitute(leSpectro))
  assign(dum,NULL)
  if (is.null(e)){
    eval(parse(text=paste("rm(",dum,")")))
  }else
  {
    eval(parse(text=paste("rm(",dum,")")),envir=e)
  }
  invisible(NULL)
}
#************************************************************************************
#

#*****************************************************************************
# Define_Acq_Param(OOobj,leSpectro,int_time=1000,boxcar=1,nscans=1)
#
# Define_Acq_Param définit les paramètres d'acquisition pour un spectro.
# 
# ENTRÉES
#             OOobj       :   liste créée par Start_OO()
#             leSpectro   :   liste créée par Define_Spectro
#             int_time    :   temps d'intégration en msec (1000 par défaut)
#             boxcar      :   Demi-longueur de l'opérateur de moyenne mobile
#                             (N-1)/2. Pour une moyenne mobile sur 5 valeurs
#                             boxcar=2. (1 par défaut)
#             nscans      :   Nombre de scans (1 par défaut)
#             
# SORTIES
#             nil
#************************************************************************************
Define_Acq_Param <- function(OOobj,leSpectro,int_time=1000,boxcar=1,nscans=1){
  
  OOobj$mywrap$setIntegrationTime(as.integer(leSpectro$number),as.integer(1000*int_time))
  Sys.sleep(0.2)
  OOobj$mywrap$setBoxcarWidth(as.integer(leSpectro$number),as.integer(boxcar))
  Sys.sleep(0.2)
  OOobj$mywrap$setScansToAverage(as.integer(leSpectro$number),as.integer(nscans))
  Sys.sleep(0.2)
  # No stability scan!
  # if (leSpectro$isQEPRO){
  #   leSpectro$dat_buffer$clearBuffer()
  #   Sys.sleep(0.2)
  #   leSpectro$dat_buffer$startAcquisition()
  #   Sys.sleep(0.2)
  #   sp=OOobj$mywrap$getSpectrum(as.integer(leSpectro$number))
  #   Sys.sleep(0.2)
  #   leSpectro$dat_buffer$abortAcquisition()
  #   Sys.sleep(0.2)
  # }else
  # {
  #   sp=OOobj$mywrap$getSpectrum(as.integer(leSpectro$number))
  #   Sys.sleep(0.2)
  # }
  invisible(NULL)
}
#************************************************************************************
#


#*****************************************************************************
# Define_Int_Time(OOobj,leSpectro,int_time=1000)
#
# Define_Int_time définit le temps d'intégration
# 
# ENTRÉES
#             OOobj       :   liste créée par Start_OO()
#             leSpectro   :   liste créée par Define_Spectro
#             int_time    :   temps d'intégration en msec (1000 par défaut)
#             
# SORTIES
#             nil
#************************************************************************************
Define_Int_Time <- function(OOobj,leSpectro,int_time=1000){
  
  OOobj$mywrap$setIntegrationTime(as.integer(leSpectro$number),as.integer(1000*int_time))
  Sys.sleep(0.2)
  invisible(NULL)
}
#************************************************************************************
#

#*****************************************************************************
# SetCorrections(OOobj,leSpectro,Lin=1,Dark=1)
#
# SetCorrections active la correction pour non-linéarité et celle pour le
# "electric dark".
# 
# ENTRÉES
#             OOobj       :   liste créée par Start_OO()
#             leSpectro   :   liste créée par Define_Spectro
#             Lin         :   1 pour activer la correction de linéarité
#                             0 pour désactiver la correction de linéarité
#             Dark        :   1 pour activer la correction "electric dark"
#                             0 pour désactiver la correction "electric dark"
# SORTIES
#             nil
#************************************************************************************
SetCorrections <- function(OOobj,leSpectro,Lin=1,Dark=1){
  OOobj$mywrap$setCorrectForDetectorNonlinearity(as.integer(leSpectro$number),as.integer(Lin))
  Sys.sleep(0.2)
  OOobj$mywrap$setCorrectForElectricalDark(as.integer(leSpectro$number),as.integer(Dark))
  Sys.sleep(0.2)
  invisible(NULL)
}
#************************************************************************************
#

#*****************************************************************************
# leSpectro <- Grab_Spectrum(leSpectro,OOobj)
#
# Grab_Spectrum fait l'acquisition d'un spectre. QEPRO est géré différemment
# des autres spectros.
#
# ENTRÉES
#             leSpectro   :   liste créée par Define_Spectro
#             OOobj       :   liste créée par Start_OO()
#             
# SORTIES
#             leSpectro modifiée comprenant:
#                  1 - un membre (sp) contenant un spectre brut
#                  2 - un membre (isSat) qui est TRUE si saturation et FALSE sinon.
#************************************************************************************
Grab_Spectrum <- function(leSpectro,OOobj)
{
  mywrap <- OOobj$mywrap
  if (leSpectro$isQEPRO){
    leSpectro$dat_buffer$clearBuffer()
    Sys.sleep(0.2)
    leSpectro$dat_buffer$startAcquisition()
    Sys.sleep(0.2)
  }
  leSpectro$sp <- mywrap$getSpectrum(as.integer(leSpectro$number))
  Sys.sleep(0.2)
  leSpectro$isSat <- mywrap$isSaturated(as.integer(leSpectro$number))
  if (leSpectro$isQEPRO){
    leSpectro$dat_buffer$abortAcquisition()
    Sys.sleep(0.2)
  }
  # Enlever un spectre interpolé si il y en a un.
  compo<-names(leSpectro)
  if (length(grep("int",compo))>0){
    leSpectro$int <- NULL
  }
  return(leSpectro)
}
#************************************************************************************
#

#*****************************************************************************
# leSpectro <- Interpolate_Spectrum(leSpectro)
#
# Interpolate_Spectrum fait l'interpolation (cubic spline) du spectre brut à partir 
# des données définies dans la liste créée par Define_Spectro.
#
# ENTRÉES
#             leSpectro   :   liste créée par Define_Spectro
#             
# SORTIES
#             leSpectro modifiée comprenant un membre (int) contenant un spectre
#               interpolé.
#************************************************************************************
Interpolate_Spectrum <- function(leSpectro)
{
  compo <- names(leSpectro)
  if (length(grep("sp",compo))==0) cat("Pas de spectre à interpoler. Saisir un spectre d'abord!")
  if (length(grep("xunit",compo))==0){
    cat("Définir la plage de longueur d'onde ou de nombre d'onde.")
  }else
  {
    if (length(grep("wv",compo))!=0) leSpectro$int <- spline(leSpectro$xaxis,leSpectro$sp,xout=leSpectro$wv)
    else{
      raw_wn <- 1e7*(1/leSpectro$lambda0-1/leSpectro$xaxis)
      leSpectro$int <- spline(raw_wn,leSpectro$sp,xout=leSpectro$wn)
    }
  }
  return(leSpectro)
}
#************************************************************************************
#

#*****************************************************************************
# Plot_Spectrum<-function(leSpectro,lequel=c("interpole","brut"),...)
#
# Plot_Spectrum met le spectre brut ou interpolé en graphique.
#
# ENTRÉES
#             leSpectro   :   liste créée par Define_Spectro
#             lequel      :   "interpole" ou "brut"
#             
# SORTIES
#             Rien. Affiche un message d'erreur dans la console si le spectre
#                   désiré n'existe pas.
#************************************************************************************
Plot_Spectrum <- function(leSpectro,lequel=c("interpole","brut"),...)
{
  lequel<-match.arg(lequel)
  compo<-names(leSpectro)
  if (length(grep("interpole",lequel))>0){
    if (length(grep("int",compo))>0){
      if (length(grep("wv",compo))>0) plot(leSpectro$int,type="l",col="blue",
                                           lwd=2,xlab="Longueur d'onde (nm)",
                                           ylab="Spectre interpolé - U.A.",...)
      else plot(leSpectro$int,type="l",col="blue",
                lwd=2,xlab=expression(paste("Nombre d'ondes (", cm^{-1}, ")",sep="")),
                ylab="Spectre interpolé - U.A.",...)
      
    }else
    {
      cat("Pas de données interpolées disponibles!")
      return(leSpectro)
    }
  }else
  {
    if (length(grep("sp",compo))>0){
       plot(leSpectro$xaxis,leSpectro$sp,type="l",col="blue",
                                           lwd=2,xlab="Longueur d'onde (nm)",
                                           ylab="Spectre interpolé - U.A.",...)
      
    }else
    {
      cat("Pas de données brutes disponibles!")
    }
    
  }
  invisible(NULL)
}
#************************************************************************************
#

#*****************************************************************************
# leSpectro <- GetTEC(leSpectro,OOobj)
#
# GetTEC définit un objet pour le contrôle de la température du détecteur.
# ATTENTION: ne gère pas les erreurs. L'usager doit s'assurer que le spectro
# permet le contrôle de température du détecteur par effet Peltier.
#
# ENTRÉES
#             leSpectro   :   liste créée par Define_Spectro
#             OOobj       :   listre créée par Start_OO()
#             
# SORTIES
#             leSpectro auquel le TEC est ajouté dans la liste
#************************************************************************************
GetTEC <- function(leSpectro,OOobj){
  leSpectro$TEC<-OOobj$mywrap$getFeatureControllerThermoElectric(as.integer(leSpectro$number))
  Sys.sleep(0.2)
  return(leSpectro)
}
#************************************************************************************
#

  

#*****************************************************************************
# T <- CoolSpectro(leSpectro, Temp, outputarea)
#
# CoolSpectro permet d'abaisser la température du détecteur et de mettre le
# ventilateur en route.
# ATTENTION: ne gère pas les erreurs. L'usager doit s'assurer que le spectro
# permet le contrôle de température du détecteur par effet Peltier.
#
# ENTRÉES
#             leSpectro   :   liste créée par Define_Spectro
#             Temp        :   température désirée en °C
#             outputarea  :   un widget de gwidgets2 pour afficher
#                             la température avec svalue(widget) <- T
#             
# SORTIES
#             La température atteinte en sortie de fonction
#************************************************************************************
CoolSpectro <- function(leSpectro,Temp,outputarea=NULL){
  
  TEC <- leSpectro$TEC
  TEC$setDetectorSetPointCelsius(as.double(Temp))
  Sys.sleep(0.3)
  TEC$setTECEnable(as.logical(TRUE))
  Sys.sleep(1.0)
  TEC$setFanEnable(as.logical(TRUE))
  Sys.sleep(0.3)
  ttest <- TEC$getDetectorTemperatureCelsius()
  Sys.sleep(0.3)
  ttest <- TEC$getDetectorTemperatureCelsius()
  ttest_old <- ttest
  ttest <- Temp+2  #Forcer au moins une boucle "while"
  while(ttest>Temp+1){
    if (is.null(outputarea)){
      cat(as.character(ttest), " °C\n")
    }else
    {
      svalue(outputarea) <- paste0(ttest," °C")
    }
    Sys.sleep(3)
    ttest <- TEC$getDetectorTemperatureCelsius()
    if ((ttest_old-ttest)<0.5){
      TEC$setDetectorSetPointCelsius(as.double(Temp))
      Sys.sleep(0.3)
      TEC$setTECEnable(as.logical(TRUE))
    }
    ttest_old <- ttest
  }
  return(TEC$getDetectorTemperatureCelsius())
}
#************************************************************************************
#

#*****************************************************************************
# WarmSpectro(leSpectro)
#
# Warm_Spectro ramène la température du détecteur proche de la température
# ambiante et ferme le ventilateur.
# ATTENTION: ne gère pas les erreurs. L'usager doit s'assurer que le spectro
# permet le contrôle de température du détecteur par effet Peltier.
#
# ENTRÉES
#             leSpectro   :   liste créée par Define_Spectro
#  [optionel] gTxt        :   fenêtre texte pour afficher les valeurs 
#             
# SORTIES
#             Rien.
#************************************************************************************
WarmSpectro <- function(leSpectro, gTxt=NULL){

  TEC <- leSpectro$TEC
  TEC$setDetectorSetPointCelsius(as.double(18))

  Sys.sleep(0.1)
  ttest <- TEC$getDetectorTemperatureCelsius()

  Sys.sleep(0.1)
  ttest <- TEC$getDetectorTemperatureCelsius()
  Sys.sleep(0.1)
  TEC$setTECEnable(as.logical(FALSE))
  Sys.sleep(1.0)
  ttest_old <- ttest

  while(ttest<15){
    if (is.null(gTxt)){
      cat(as.character(ttest)," °C\n")
    } else {insert(gTxt,paste("   ",as.character(ttest)," °C"))}
    
    Sys.sleep(3)
    ttest <- TEC$getDetectorTemperatureCelsius()
    if ((ttest-ttest_old)<0.5){
      TEC$setTECEnable(as.logical(FALSE))
    }
    ttest_old <- ttest  
  }

  Sys.sleep(0.3)
  TEC$setFanEnable(as.logical(FALSE))
  invisible(NULL)
}
#************************************************************************************
#

#*****************************************************************************
# Quit_OO(OOobj)
#
# Quit_OO détruit la liste créée par Start_OO()
# Appele closeAllSpectrometers().
#
# ENTRÉES
#             OOobj  : liste créée par Start_OO()
#             
# SORTIES
#             Rien.
#************************************************************************************
Quit_OO <- function(OOobj)
{
  
  OOobj$mywrap$closeAllSpectrometers()
  # dum=deparse(substitute(OOobj))
  # assign(dum,NULL)
  # eval(parse(text=paste("rm(",dum,")")))
  # if (exists("TEC")){
  #     TEC<<-NULL
  #     rm(TEC,envir = .GlobalEnv)
  # }
}
#************************************************************************************
#

#*****************************************************************************
# leno <- FindNo(OOobj,lenom,noserie)
#
# FindNo trouve le numero correspondant a un spectro identifie par son nom
# et numero de serie
#
# ENTRÉES
#             OOobj  : liste créée par Start_OO()
#             lenom  : le nom du spectro (e.g. 'USB4000')
#           noserie  : le numero de serie (e.g. USB4F06255)
#             
# SORTIES
#             le numero du spectro qu'on pourra utiliser dans Define_Spectro
#             ou 'NOT FOUND' s'il n'y a pas de correspondance.
#************************************************************************************
FindNo <- function(OOobj,lenom,noserie)
{
  out <- with(OOobj, (lesspectros==lenom & serial_no==noserie))
  if (length(out)>0){
    return(which(out))
  } else
    return('NOT FOUND')
  
}
#************************************************************************************
#
