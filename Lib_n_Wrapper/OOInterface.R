#-------------------------------------------------------------------------------------------
# OOInterface.R est une librairie qui comprend des fonctions pour communiquer avec les
# spectros Ocean Optics à travers la librairie Java OmniDriver de Ocean Optics. Seules
# quelques fonctions ont été créées mais en se référant à ce qui est défini, il est assez
# facile de créer d'autres fonctions R qui peuvent appeler d'autres fonctions de OmniDriver.
#
# Au coeur de cette librairie est la liste leSpectro créée avec la fonction
# Define_Spectro. Cette liste comprend différents paramètres permettant de définir un
# spectro: number-le numéro par lequel on y réfère; name-le nom
# tel que retourne GetName() d'OmniDriver; xaxis-les coordonnées des pixels telles que
# stokées dans le spectro; isQEPRO-un "boolean" pour identifier les QEPRO qui doivent gérés
# différemment à cause de leur "buffer" interne; wv ou wn-les valeurs de longueur d'ondes
# (wv) ou de nombre d'onde (wn) à laquelle on interpole les spectres; xunit-"wavelength"
# ou "wavenumber". Pour les QEPROs, la liste contient aussi l'élément dat_buffer qui est un
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
#-------------------------------------------------------------------------------------------
# Auteur: Bernard Panneton, Agriculture et Agroalimentaire Canada, St-Jean-sur-Richelieu
# Avril 2017
# Droits d'auteur régis par la licence GPL-3 de R. Pour plus d'informations,
# exécuter la commande RShowDoc("GPL-3") dans R.
# GNU GENERAL PUBLIC LICENSE
# Version 3
#-------------------------------------------------------------------------------------------

#*****************************************************************************
#On s'assure que rJava est chargée. La librairie OmniDriver est en Java!
ok = require(rJava)
if (!ok) {
  install.packages("rJava")
  require(rJava)
}


#*****************************************************************************
# Start_OO() crée un objet mywrap qui est un wrapper pour accéder aux fonctions
# de OmniDriver. Elle dresse aussi la liste des spectros détectés.
#
# ENTRÉES
#             Nil
#
# SORTIES
#             Une liste comprenant 2 éléments: 1-une liste des noms des spectros
#             détectés; 2-l'objet mywrap
#*****************************************************************************
Start_OO <- function() {
  #Init. l'objet wrapper d'OO et dresse la liste des spectros disponibles
  #La fonction retourne la liste ou un message d'erreur.
  ooi_home = Sys.getenv("OOI_HOME")
  mypath = file.path(ooi_home, "OmniDriver.jar")
  .jinit()
  while(!any(stringr::str_detect(.jclassPath(),"OmniDriver"))){
    .jaddClassPath(mypath)
    Sys.sleep(0.2)
  }
  mywrap <- .jnew("com.oceanoptics.omnidriver.api.wrapper.Wrapper")
  nbspectro = .jcall(mywrap, "I", "openAllSpectrometers")
  #Construit une liste de spectros pour l'outil "id" de type "gcombobox"
  spectrolst = NULL
  seriallst = NULL
  if (nbspectro > 0) {
    for (k in 1:nbspectro) {
      spectrolst = c(spectrolst, .jcall(mywrap, "S", "getName", as.integer(k -
                                                                             1)))
      seriallst = c(seriallst, mywrap$getSerialNumber(as.integer(k - 1)))
    }
    return(list(
      lesspectros = spectrolst,
      mywrap = mywrap,
      serial_no = seriallst
    ))
  }
  else{
    cat("Pas de spectro rattaché\n")
    return(NULL)
  }
}

#*****************************************************************************

#*****************************************************************************
# Define_Spectro crée une liste permettant de définir un spectro en particulier
# et de définir quelques-uns de ses paramètres. Cette liste est modifiée par d'autres
# fonctions.
#
# ENTRÉES
#             OOobj   : liste créée par Start_OO()
#
# SORTIES
#             Une liste comprenant 5 éléments: 1-number est le numéro par lequel on va
#                réferer au spectro; 2-le nom du spectro; 3-l'axe des x tel que stocké dans
#                le spectro; 4-IsQEPRO-TRUE pour un QEPRO et FALSE autrement; 5-dat_buffer
#                un objet pour gérer le buffer d'acquisition d'un QEPRO.
#*****************************************************************************
Define_Spectro <- function(OOobj, leno = NULL)
{
  #Pour définir un spectro à partir de la liste des spectros raccordés.
  leSpectro = list()
  if (is.null(leno)) {
    laliste = NULL
    for (k in 1:length(OOobj$lesspectros)) {
      laliste = paste(
        laliste,
        "\n",
        as.character(k),
        ": ",
        OOobj$lesspectros[k],
        " - S/N: ",
        OOobj$serial_no[k],
        sep = ""
      )
    }
    leSpectro$number <-
      as.integer(readline(
        paste("Choisir un spectro par son numéro:\n", laliste, "\n", sep = "")
      ))
  } else
    leSpectro$number = leno
  leSpectro$name = OOobj$lesspectros[leSpectro$number]
  leSpectro$number = leSpectro$number - 1
  leSpectro$xaxis = OOobj$mywrap$getWavelengths(as.integer(leSpectro$number))
  leSpectro$range = OOobj$mywrap$getMaximumIntensity(as.integer(leSpectro$number))
  if ('QE-PRO' %in% leSpectro$name) {
    leSpectro$isQEPRO <- TRUE
  } else {
    leSpectro$isQEPRO <- FALSE
  }
  if (leSpectro$isQEPRO) {
    #Définir l'objet pour contrôler le buffer
    leSpectro$dat_buffer <-
      OOobj$mywrap$getFeatureControllerDataBuffer(as.integer(leSpectro$number))
    Sys.sleep(0.2)
    leSpectro$dat_buffer$abortAcquisition()
    Sys.sleep(0.2)
  }
  return(leSpectro)
}
#*****************************************************************************

#*****************************************************************************
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
#*****************************************************************************
Define_WavelengthRange <- function(leSpectro, wv_min, wv_max, delta_wv)
{
  #Pour définir la plage et l'incrément du vecteur de longueur d'onde pour l'interpolation des spectres
  leSpectro$wv = seq(from = wv_min, to = wv_max, by = delta_wv)
  leSpectro$xunit = "wavelength"
  compo <- names(leSpectro)
  if (length(grep("wn", compo)) > 0) {
    leSpectro$wn = NULL #peut pas être wavelength et wavenumber
    leSpectro$lambda0 = NULL
  }
  if (length(grep("yint", compo)) > 0)
    leSpectro$yint = NULL
  return(leSpectro)
}

#*****************************************************************************

#*****************************************************************************
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
#*****************************************************************************
Define_WavenumberRange <-
  function(leSpectro,
           wn_min,
           wn_max,
           delta_wn,
           lambda0)
  {
    #Pour définir la plage et l'incrément du vecteur de nombre d'onde pour l'interpolation des spectres
    leSpectro$wn = seq(from = wn_min, to = wn_max, by = delta_wn)
    leSpectro$xunit = "wavenumber"
    leSpectro$lambda0 = lambda0
    compo <- names(leSpectro)
    if (length(grep("wv", compo)) > 0)
      leSpectro$wv = NULL #peut pas être wavelength et wavenumber
    if (length(grep("yint", compo)) > 0)
      leSpectro$yint = NULL
    return(leSpectro)
  }

#*****************************************************************************

#*****************************************************************************
# Close_Spectro détruit la liste définissant un spectro.
#
# ENTRÉES
#             leSpectro   :   liste créée par Define_Spectro
#
# SORTIES
#             nil
#*****************************************************************************
Close_Spectro <- function(leSpectro)
{
  dum = deparse(substitute(leSpectro))
  assign(dum, NULL)
  eval(parse(text = paste("rm(", dum, ")")))
}

#*****************************************************************************

#*****************************************************************************
# Define_Acq_Param définit les paramètres d'acquisition pour un spectro.
#
# ENTRÉES
#             OOobj       :   liste créée par Start_OO()
#             leSpectro   :   liste créée par Define_Spectro
#             int_time    :   temps d'intégration en msec
#             boxcar      :   Demi-longueur de l'opérateur de moyenne mobile
#                             (N-1)/2. Pour une moyenne mobile sur 5 valeurs
#                             boxcar=2.
#             nscans      :   Nombre de scans
#
# SORTIES
#             nil
#*****************************************************************************
Define_Acq_Param <-
  function(OOobj,
           leSpectro,
           int_time = 1000,
           boxcar = 1,
           nscans = 1) {
    OOobj$mywrap$setIntegrationTime(as.integer(leSpectro$number), as.integer(1000 *
                                                                               int_time))
    Sys.sleep(0.2)
    OOobj$mywrap$setBoxcarWidth(as.integer(leSpectro$number), as.integer(boxcar))
    Sys.sleep(0.2)
    OOobj$mywrap$setScansToAverage(as.integer(leSpectro$number), as.integer(nscans))
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
    return(leSpectro)
  }

#*****************************************************************************


#*****************************************************************************
# Define_Int_time définit le temps d'intégration
#
# ENTRÉES
#             OOobj       :   liste créée par Start_OO()
#             leSpectro   :   liste créée par Define_Spectro
#             int_time    :   temps d'intégration en msec
#
# SORTIES
#             nil
#*****************************************************************************
Define_Int_Time <- function(OOobj, leSpectro, int_time = 1000) {
  OOobj$mywrap$setIntegrationTime(as.integer(leSpectro$number), as.integer(1000 *
                                                                             int_time))
  Sys.sleep(0.2)
  return(leSpectro)
}

#*****************************************************************************

#*****************************************************************************
# SetCorrections active la correction pour non-linéarité et celle pour le
# "electric dark".
#
# ENTRÉES
#             OOobj       :   liste créée par Start_OO()
#             leSpectro   :   liste créée par Define_Spectro
# SORTIES
#             nil
#*****************************************************************************
SetCorrections <- function(OOobj,
                           leSpectro,
                           Lin = 1,
                           Dark = 1) {
  OOobj$mywrap$setCorrectForDetectorNonlinearity(as.integer(leSpectro$number), as.integer(Lin))
  Sys.sleep(0.2)
  OOobj$mywrap$setCorrectForElectricalDark(as.integer(leSpectro$number), as.integer(Dark))
  Sys.sleep(0.2)
}

#*****************************************************************************

#*****************************************************************************
# Grab_Spectrum fait l'acquisition d'un spectre. QEPRO est géré différemment
# des autres spectros.
#
# ENTRÉES
#             leSpectro   :   liste créée par Define_Spectro
#             mywrap      :   objet mywrap de la liste créée par Start_OO()
#
# SORTIES
#             leSpectro modifiée comprenant un membre (sp) contenant un spectre
#               brut.
#*****************************************************************************
Grab_Spectrum <- function(leSpectro, mywrap)
{
  if (leSpectro$isQEPRO) {
    leSpectro$dat_buffer$clearBuffer()
    Sys.sleep(0.2)
    leSpectro$dat_buffer$startAcquisition()
    Sys.sleep(0.2)
  }
  leSpectro$sp = mywrap$getSpectrum(as.integer(leSpectro$number))
  Sys.sleep(0.2)
  if (leSpectro$isQEPRO) {
    leSpectro$dat_buffer$abortAcquisition()
    Sys.sleep(0.2)
  }
  # Enlever un spectre interpolé si il y en a un.
  compo <- names(leSpectro)
  if (length(grep("int", compo)) > 0) {
    leSpectro$int = NULL
  }
  return(leSpectro)
}

#*****************************************************************************

#*****************************************************************************
# Interpolate_Spectrum fait l'interpolation (cubic spline) du spectre brut à partir
# des données définies dans la liste créée par Define_Spectro.
#
# ENTRÉES
#             leSpectro   :   liste créée par Define_Spectro
#
# SORTIES
#             leSpectro modifiée comprenant un membre (int) contenant un spectre
#               interpolé.
#*****************************************************************************
Interpolate_Spectrum <- function(leSpectro)
{
  compo = names(leSpectro)
  if (length(grep("sp", compo)) == 0)
    cat("Pas de spectre à interpoler. Saisir un spectre d'abord!")
  if (length(grep("xunit", compo)) == 0) {
    cat("Définir la plage de longueur d'onde ou de nombre d'onde.")
  } else
  {
    if (length(grep("wv", compo)) != 0)
      leSpectro$int = spline(leSpectro$xaxis, leSpectro$sp, xout = leSpectro$wv)
    else{
      raw_wn = 1e7 * (1 / leSpectro$lambda0 - 1 / leSpectro$xaxis)
      leSpectro$int = spline(raw_wn, leSpectro$sp, xout = leSpectro$wn)
    }
  }
  return(leSpectro)
}

#*****************************************************************************

#*****************************************************************************
# Plot_Spectrum met le spectre brut ou interpolé en graphique.
#
# ENTRÉES
#             leSpectro   :   liste créée par Define_Spectro
#             lequel      :   "interpole" ou "brut"
#             x0          :   minimum pour l'axe des x. (souvent pour éviter Rayleigh)
#
# SORTIES
#             Rien. Affiche un message d'erreur dans la console si le spectre
#                   désiré n'existe pas.
#*****************************************************************************
Plot_Spectrum <- function(leSpectro,
                          lequel = c("interpole", "brut"),
                          ...)
{
  lequel <- match.arg(lequel)
  compo <- names(leSpectro)
  
  
  if (length(grep("interpole", lequel)) > 0) {
    if (length(grep("int", compo)) > 0) {
      if (length(grep("wv", compo)) > 0)
        plot(
          leSpectro$int,
          type = "l",
          col = "blue",
          lwd = 2,
          xlab = "Longueur d'onde (nm)",
          ylab = "Spectre interpolé - U.A.",
          ...
        )
      else
        plot(
          leSpectro$int,
          type = "l",
          col = "blue",
          lwd = 2,
          xlab = expression(paste("Nombre d'ondes (", cm ^ {
            -1
          }, ")", sep = "")),
          ylab = "Spectre interpolé - U.A.",
          ...
        )
      
    } else
    {
      cat("Pas de données interpolées disponibles!")
      return(leSpectro)
    }
  } else
  {
    if (length(grep("sp", compo)) > 0) {
      plot(
        leSpectro$xaxis,
        leSpectro$sp,
        type = "l",
        col = "blue",
        lwd = 2,
        xlab = "Longueur d'onde (nm)",
        ylab = "Spectre interpolé - U.A.",
        ...
      )
      
    } else
    {
      cat("Pas de données brutes disponibles!")
    }
    
  }
}

#****************************************************************************

#*****************************************************************************
# GetTEC définit un objet pour le contrôle de la température du détecteur
#
# ENTRÉES
#             leSpectro   :   liste créée par Define_Spectro
#
# SORTIES
#             leSpectro auquel le TEC est ajouté dans la liste
#*****************************************************************************
GetTEC <- function(leSpectro, mywrap) {
  leSpectro$TEC <-
    mywrap$getFeatureControllerThermoElectric(as.integer(leSpectro$number))
  Sys.sleep(0.2)
  return(leSpectro)
}



#*****************************************************************************
# CoolSpectro permet d'abaisser la température du détecteur et de mettre le
# ventilateur en route.
#
# ENTRÉES
#             leSpectro   :   liste créée par Define_Spectro
#             mywrap      :   membre mywrap de la liste créée par Start_OO()
#             Temp        :   température désirée en °C
#
# SORTIES
#             La température atteinte en sortie de fonction
#*****************************************************************************
CoolSpectro <- function(leSpectro, mywrap, Temp) {
  TEC = leSpectro$TEC
  TEC$setDetectorSetPointCelsius(as.double(Temp))
  Sys.sleep(0.3)
  TEC$setTECEnable(as.logical(TRUE))
  Sys.sleep(1.0)
  TEC$setFanEnable(as.logical(TRUE))
  Sys.sleep(0.3)
  ttest = TEC$getDetectorTemperatureCelsius()
  Sys.sleep(0.3)
  ttest = TEC$getDetectorTemperatureCelsius()
  ttest_old = ttest
  ttest = Temp + 2  #Forcer au moins une boucle "while"
  while (ttest > Temp + 1) {
    cat(as.character(ttest), "°C\n")
    Sys.sleep(3)
    ttest = TEC$getDetectorTemperatureCelsius()
    if ((ttest_old - ttest) < 0.5) {
      TEC$setDetectorSetPointCelsius(as.double(Temp))
      Sys.sleep(0.3)
      TEC$setTECEnable(as.logical(TRUE))
    }
    ttest_old = ttest
  }
  return(TEC$getDetectorTemperatureCelsius())
}

#****************************************************************************

#*****************************************************************************
# Warm_Spectro ramène la température du détecteur proche de la température
# ambiante et ferme le ventilateur.
#
# ENTRÉES
#             leSpectro   :   liste créée par Define_Spectro
#             mywrap      :   membre mywrap de la liste créée par Start_OO()
#
# SORTIES
#             Rien.
#*****************************************************************************
WarmSpectro <- function(leSpectro, mywrap) {
  TEC = leSpectro$TEC
  TEC$setDetectorSetPointCelsius(as.double(18))
  Sys.sleep(0.3)
  ttest = TEC$getDetectorTemperatureCelsius()
  Sys.sleep(0.3)
  ttest = TEC$getDetectorTemperatureCelsius()
  Sys.sleep(0.3)
  TEC$setTECEnable(as.logical(FALSE))
  Sys.sleep(1.0)
  ttest_old = ttest
  while (ttest < 15) {
    cat(as.character(ttest), " °C\n")
    Sys.sleep(3)
    ttest = TEC$getDetectorTemperatureCelsius()
    if ((ttest - ttest_old) < 0.5) {
      TEC$setTECEnable(as.logical(FALSE))
    }
    ttest_old = ttest
  }
  Sys.sleep(0.3)
  TEC$setFanEnable(as.logical(FALSE))
}

#*****************************************************************************

#*****************************************************************************
# GetHighGain définit un objet pour le contrôle du gain 
#
# ENTRÉES
#             leSpectro   :   liste créée par Define_Spectro
#
# SORTIES
#             leSpectro auquel le HighGain est ajouté dans la liste. HighGain
#             est un objet pour accéder aux fonctions associées au contrôle du
#             gain élevé. Si cette option n'est pas disponible pour le spectro,
#             ça retourne NULL.
#*****************************************************************************
GetHighGain <- function(leSpectro, mywrap) {
  leSpectro$HighGain <-
    mywrap$getFeatureControllerHighGainMode(as.integer(leSpectro$number))
  Sys.sleep(0.2)
  return(leSpectro)
}

#*****************************************************************************

#*****************************************************************************
# SetHighGain 
#
# ENTRÉES
#             leSpectro   :   liste créée par Define_Spectro
#
# SORTIES
#            Aucune. Initie le gain élevé si disponible
#********************************************
SetHighGain <- function(leSpectro, mywrap, leGain) {
  HG <- leSpectro$HighGain
  if (!is.null(HG))
    HG$setHighGain(leGain)
  Sys.sleep(0.2)
  return(NULL)
}

#*****************************************************************************

#*****************************************************************************
# Quit_OO détruit la liste créée par Start_OO()
#
# ENTRÉES
#             OO  : liste créée par Start_OO()
#
# SORTIES
#             Rien.
#*****************************************************************************
Quit_OO <- function(OO)
{
  dum = deparse(substitute(OO))
  assign(dum, NULL)
  eval(parse(text = paste("rm(", dum, ")")))
  if (exists("TEC")) {
    TEC <<- NULL
    rm(TEC, envir = .GlobalEnv)
  }
}
#*****************************************************************************
