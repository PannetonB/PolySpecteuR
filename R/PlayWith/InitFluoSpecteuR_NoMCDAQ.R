InitFluoSpecteuR<-function(){
  #*****************************************************************************
  #*****************************************************************************
  # - Programme pour préparer l'acquisition de données de fluo avec le SpectrAAC.
  # - Identifier un fichier d'instrument.
  # - Initialiser le matériel.
  #*****************************************************************************
  # AUTEUR: Bernard Panneton, Agriculture et Agroalimentaire Canada
  # Février 2022
  #*****************************************************************************
  #*****************************************************************************
  # Charge des librairies additionnelles----
  
  lesPackages <- c("png","rChoiceDialogs","tcltk",
                   "tcltk2","utils","prospectr","here")
  lapply(lesPackages, function(pp){
    ok <- require(pp, character.only = TRUE)
    if (!(ok)){
      install.packages(pp,dependencies = T, character.only = TRUE)
      library(pp, character.only = TRUE)
    } 
  })
  #*****************************************************************************
  #Définit le répertoire du script en cours----
  PolySpecteuRPath=here::here()
  setwd( PolySpecteuRPath)  #Répertoire du projet PolySpecteur
  #*****************************************************************************
  
  
  #****************************************************************************
  # Crée un environnement unique pour l'instrument
  FluoEnv <- new.env(parent = .GlobalEnv)
  FluoEnv$type <- "Fluorescence"
  #*****************************************************************************
  
  #*****************************************************************************
  # Librairies pour MC et spectros----
  #*****************************************************************************
  source('Lib_n_Wrapper/MCDAQ.R')  
  source('Lib_n_Wrapper/OOInterface.R')
  
  
  #*****************************************************************************
  # I N I T I A L I S A T I O N S ------
  #*****************************************************************************
  # On a besoin du spectro, du module DAQ. On
  # va donc commencer par faire les intialisations
  # nécessaires.
  #____________________________________________________________________________
  #****************************************************************************
  # └ Charge la configuration de l'instrument----
  fichier_config <- rchoose.files(
    caption="FLUORESCENCE - Choisir le fichier de configuration de l'instrument",
    default = normalizePath("Fichiers_Instruments"),
    multi = FALSE,filters = "*.R")
  source(fichier_config,encoding = "UTF-8", local=FluoEnv)
  
  #*****************************************************************************
  # └ Fichiers paramètres instruments----
  laCaption <- paste0("FLUORESCENCE SUR ",FluoEnv$nomInstrument,
                      " - Choisir le fichier des paramètres")
  fichier_param <- rchoose.files(caption=laCaption,
                                 default = normalizePath("Fichiers_Parametres"),
                                 multi = FALSE,filters = "*.R")
  source(fichier_param, encoding = "UTF-8", local=FluoEnv)  
  FluoEnv$fichier_param <- fichier_param
 
  #*****************************************************************************
  # └ Démarre le wrapper Ocean Optics si nécessaire.----
  # Recherche "OOobj" dans l'environnement .GlobalEnv. Si existe, on assume
  # que le wrapper est actif à moins que OOobj==NULL.
  if (!exists("OOobj",where=.GlobalEnv) || is.null(OOobj)){ #construit wrapper
    # └ Active le "wrapper" donnant accès aux spectros.----
    OOobj <<- Start_OO()
    if (is.null(OOobj)){
      rm(OOobj, envir = .GlobalEnv )
      cat("Incapable de définir un wrapper OO. \n\n")
      return("ABANDON")
    }
  }
  #*****************************************************************************
  # └ └ Définir Spectros et plages d'interpolation----
  
  # Trouve le spectro de l'instrument.
  FluoEnv$lespectro=which((OOobj$serial_no==FluoEnv$les_spectro$serial) & 
                            (OOobj$lesspectros==FluoEnv$les_spectro$name))
  if (length(FluoEnv$lespectro)==0){
    cat("Ne trouve pas le spectro!\n Va quitter...\n")
    Quit_OO(OOObj)
    rm(OOobj, envir = .GlobalEnv)
    return("ABANDON")
  }
  
  
    #On définit le spectro pour l'environnement de l'instrument.
    with(FluoEnv,{ 
         lespectro <- Define_Spectro(OOobj,lespectro)
         SetCorrections(OOobj,lespectro,Lin = 1, Dark = 0)
         lespectro <- Define_WavelengthRange(
              lespectro,fluo_l_min,fluo_l_max,fluo_step
           )
    })
    
    #Lire la calibration en Y
    with(FluoEnv, gainY<-scan(Maya_Calib_File,sep="\t",quiet=TRUE))
  
  #*****************************************************************************
  # └ module MC DAQ----
  #*****************************************************************************
  # └└ Initialise les devices------
  # setwd("Lib_n_Wrapper")
  # Init_MCLib()
  # setwd("..")
  # lesDevices=GetDevices()
  # FluoEnv$lesDevices <- lesDevices
  # 
  # #Initialise chaque board
  # for (k in 1:lesDevices$nbdevices)
  #   dum=Init_DAQ(lesDevices$noms[k],lesDevices$serie[k],lesDevices$BoardNum[k])
  # lesnoms=lesDevices$noms[1:lesDevices$nbdevices]
  # 
  # Device <- which((lesDevices$serie==FluoEnv$les_mcusb$serial) & 
  #                   (lesDevices$noms==FluoEnv$les_mcusb$name))
  # if (length(Device)==0){
  #   cat("Ne trouve pas le module de MC!\n Va quitter...\n")
  #   setwd("Lib_n_Wrapper")
  #   Quitte_MCLIB() 
  #   setwd("..")
  #   with(FluoEnv, rm(lespectro))
  #   Quit_OO(OOobj)
  #   return("ABANDON")
  # } 
  # #Configure les boards. Seuls les ports DIO sont configurés.
  # Config_Board(lesDevices,FluoEnv$lesPorts)
  # #Configure les ports de sortie en courant  et ceux en voltage sur USB-3106
  # for (i in FluoEnv$lesports_amp){
  #   err=.C("BP_cbSetConfig",boardinfo=as.integer(BOARDINFO),bnum=as.integer(lesDevices$BoardNum[Device]),
  #          channelnum=as.integer(i),configitem=as.integer(BIDACRANGE),
  #          configval=as.integer(MA0TO20),out=as.integer(0))
  # }
  # for (i in FluoEnv$lesports_V){
  #   err=.C("BP_cbSetConfig",boardinfo=as.integer(BOARDINFO),bnum=as.integer(lesDevices$BoardNum[Device]),
  #          channelnum=as.integer(i),configitem=as.integer(BIDACRANGE),
  #          configval=as.integer(UNI10VOLTS),out=as.integer(0))
  # }
  # 
  # 
  
  #*****************************************************************************
  #État d'instrument Fluo + Normalisation----
  #************************************************************************************
  #
  #
  cat("\n***********************************\nVérification de(s) instrument(s)\n")
 
  FluoEnv$lesEXs <- get_DELs_dat("EX",mon_envir=FluoEnv)
  dum <- utils::winDialog(type="yesno",
                           message=" FLUORESCENCE\nFaire la mesure du facteur de normalisation?")
  
  dum1 <- get_DELs_dat("FEX",mon_envir = FluoEnv)
  dum1 <- apply(dum1,2,list)
  lesDels <- lapply(dum1,function(x) as.list(unlist(x)))
  FluoEnv$lesDels <- lesDels
  
  if (dum=="YES"){
    FluoEnv$NormY <- Mesures_Etalon_Fluo(FluoEnv, OOobj,logo,op)
    cat(paste("Facteur de correction: ",format(FluoEnv$NormY,
                                   digits=3,scientific = TRUE),"\n",sep=""))
   }
    
  if  (dum=="NO"){
    FluoEnv$NormY <- rep(1,length(lesDels))
  }
  
  readline("Appuyer sur Retour/Enter pour continuer. \n")
  
  dum <- get_DELs_dat("FEX",mon_envir = FluoEnv)
  dum <- apply(dum,2,list)
  lesDels <- lapply(dum,function(x) as.list(unlist(x)))
  FluoEnv$lesDels <- lesDels
  par(op)
  return(FluoEnv)
}  

#*****************************************************************************
#Mesures_Etalon_Fluo----
#*****************************************************************************
Mesures_Etalon_Fluo <- function(FluoEnv, OOobj,logo,op){
  
  dum <- get_DELs_dat("FEX",mon_envir = FluoEnv)
  dum <- apply(dum,2,list)
  lesDels <- lapply(dum,function(x) as.list(unlist(x)))
  
  FluoEnv$lespectro=Define_WavelengthRange(FluoEnv$lespectro,
                                           FluoEnv$fluo_l_min,
                                           FluoEnv$fluo_l_max,
                                           FluoEnv$fluo_step)
  Sys.sleep(0.2)
  
  N_ExFluo <- length(lesDels)
  NormY <- rep(0,N_ExFluo)
  
  #Récupère l'assignation à un standard pour chaque DEL
  std_4_DEL <- get_DELs_dat("STD_EX",mon_envir = FluoEnv)
  #Liste des standards
  les_stds <- unique(std_4_DEL)
  #Nombre de standards
  N_std <- length(les_stds)
  
  #Vérifie si le fichier Instrument_x_Verif.txt existe. Non, on le crée.
  #Oui, on lit les données.
  #path='Fichiers_Instruments'
  lefichier=FluoEnv$Instrument_Verif_File
  if (file.exists(lefichier)){
    histoire = read.table(lefichier,header=TRUE,sep="\t",stringsAsFactors = FALSE)
    histoire_length=nrow(histoire)
  }else
  {
    histoire=data.frame('Date'=as.character(),'Heure'=as.character(),stringsAsFactors = FALSE)
    for (kk in 1:N_ExFluo) histoire <- eval(parse(text=paste0("cbind(histoire,'DEL",kk,"'=as.numeric())")))
    
    histoire_length=nrow(histoire)
  }
  if (histoire_length>0){
    mean_DEL_I=colMeans(histoire[,-c(1,2)])
  }else
  {
    mean_DEL_I <- rep(0,N_ExFluo)
  }
  
  
  maxi=matrix(0,ncol=N_ExFluo,nrow=FluoEnv$Instr_reps)
  les_ii=matrix(0,ncol=N_ExFluo,nrow=FluoEnv$Instr_reps)
  for (ijk in (1:N_std)){
    utils::winDialog(type="ok",
                     message=paste0("FLUORESCENCE\nPlacer l'étalon ",
                                    les_stds[ijk],
                                    " dans l'instrument."))
    lesk <- which(std_4_DEL==les_stds[ijk])
    for (jj in 1:FluoEnv$Instr_reps){
      if (jj>1) utils::winDialog(type="ok","FLUORESCENCE\nDéplacer l'étalon.")
      for (k in lesk){
        FluoEnv$lespectro=Define_Acq_Param(OOobj,FluoEnv$lespectro,
                                   FluoEnv$Instr_T_exp_test[[std_4_DEL[k]]][k],
                                   FluoEnv$Instr_Boxcar_test[[std_4_DEL[k]]][k],
                                   FluoEnv$Instr_N_scans_test[[std_4_DEL[k]]][k])
        Sys.sleep(0.2)
        cat(paste("Acquisition d'un noir pour la DEL ",as.character(k),".\n",sep=""))
        FluoEnv$lespectro=Grab_Spectrum(FluoEnv$lespectro,OOobj$mywrap)
        #Met le noir en réserve
        noir=FluoEnv$lespectro$sp
        #Allume la DEL à EXi_DEL_I milliampères
        I_OUT(lesDels[[k]],FluoEnv$lesDevices,
              FluoEnv$FluoDel_I_test[[std_4_DEL[k]]][k])
        Sys.sleep(FluoEnv$T_DEL)       #Attend T_DEL secondes pour stabiliser la DEL
        cat(paste("Acquisition de fluorescence pour la DEL ",as.character(k),".\n",sep=""))
        FluoEnv$lespectro=Grab_Spectrum(FluoEnv$lespectro,OOobj$mywrap)
        #Coupe l'alimentation de la DEL (0 milliamps)
        I_OUT(lesDels[[k]],FluoEnv$lesDevices,0)
        #Soustraire le noir
        FluoEnv$lespectro$sp=FluoEnv$lespectro$sp-noir
        FluoEnv$lespectro$sp=FluoEnv$gainY*FluoEnv$lespectro$sp
        #Divise par le temps d'exposition sec, 1000 parce que le temps est en msec
        FluoEnv$lespectro$sp=FluoEnv$lespectro$sp/FluoEnv$Instr_T_exp_test[[std_4_DEL[k]]][k]*1000
        #On fait l'interpolation
        FluoEnv$lespectro=Interpolate_Spectrum(FluoEnv$lespectro)
        #Trouve max
        i1=floor(FluoEnv$Norm_Y_peak[[std_4_DEL[k]]]-FluoEnv$Norm_Y_BW[[std_4_DEL[k]]]/2)
        i1=which(FluoEnv$lespectro$int$x>=i1)[1]
        i2=ceiling(FluoEnv$Norm_Y_peak[[std_4_DEL[k]]]+FluoEnv$Norm_Y_BW[[std_4_DEL[k]]]/2)
        i2=which(FluoEnv$lespectro$int$x>i2)[1]-1
        ii=i1-1+which.max(FluoEnv$lespectro$int$y[i1:i2])
        les_ii[jj,k]=ii
        #Moyenne sur ±2 pixels
        #maxi[jj,k]=mean(lespectro$int$y[(ii-2):(ii+2)]) 
        #savgol puis maximum
        dum=savitzkyGolay(FluoEnv$lespectro$int$y[(ii-5):(ii+5)],0,3,5)
        maxi[jj,k]=dum[4]
      }
    }
  }
  
  #Remise à zéro de la fenêtre graphique
  windows(height = 10, width=10)
  ladev <- dev.cur()
  par(op)
  #10 graphiques - premier pour le logo et 8 suivants pour la fluo
  nf=layout(matrix(c(1,2,3,4,5,6,7,8,9,10),5,2,byrow=TRUE))
  #Affiche le logo
  par(mai=c(0,0,0,0))
  plot(as.raster(logo))
  par(mai=op$mai*0.35)
  
  for (k in 1:N_ExFluo){
    NormY=colMeans(maxi)
    iis=colMeans(les_ii)
    i2=histoire_length
    if (i2>5){
      i1=i2-4
    }else
    {
      i1=1
    }
    if (histoire_length==0){
      dat_4_plot=c(mean_DEL_I[k],NormY[k])
      names_4_plot=c("Moyenne","Courant")
      barplot(dat_4_plot,names.arg=names_4_plot,main=paste("Excitation à ",
              as.character(FluoEnv$lesEXs[k])," nm",sep=""), ylab="Absolu")
    }else
    {
      dat_4_plot=c(mean_DEL_I[k],histoire[(i1:i2),(k+2)],NormY[k])
      dat_4_plot=dat_4_plot/dat_4_plot[1]
      names_4_plot=c("Moyenne",histoire[(i1:i2),1],"Courant")
      barplot(dat_4_plot,names.arg=names_4_plot,main=paste("Excitation à ",
             as.character(FluoEnv$lesEXs[k])," nm",sep=""), ylab="Rel. à moyenne")
    }
  }
  
  mess <- paste0("\nOK pour effacer le graphique et continuer.")
  utils::winDialog("ok",mess)
  dev.set(ladev)
  par(op)
  dev.off()

  larow=nrow(histoire)+1
  histoire[larow,1] <-  format(Sys.time(),"%D")
  histoire[larow,2] <- format(Sys.time(),"%H:%M:%S")
  for (kk in 1:N_ExFluo){
    histoire[larow,kk+2] <- NormY[kk]
  }
  write.table(histoire,lefichier,sep="\t")
  return(NormY)
}

#*****************************************************************************
#Abort <- function(lesDevices,lesspectros,OOobj,op)----
#*****************************************************************************
Abort <- function(FluoEnv,OOobj,op){
  for (k in 1:FluoEnv$lesDevices$nbdevices) 
    ReleaseBoard(FluoEnv$lesDevices$BoardNum[k])
  (Quitte_MCLIB())
  with(FluoEnv, rm(lespectro))
  rm(OOobj)
  Quit_OO(OOobj)
  cat("\nPolySPecteur a terminé! A la prochaine!\n")
  par(op)
  return()
}

#*****************************************************************************
#get_DELs_dat-----------------------------------------
#*****************************************************************************
get_DELs_dat <- function(varname,mon_envir=.GlobalEnv)
  #Fonction pour récupérer des vecteurs/listes de paramètres
{
  dum <- ls(envir = mon_envir, pattern = paste0("^",varname,".$"))
  dum <- unname(sapply(dum,FUN=function(x) get(x,envir=mon_envir)))
  return(dum)
}
#END-----------------------------------------


