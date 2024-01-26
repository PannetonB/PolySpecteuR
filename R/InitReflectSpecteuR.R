InitReflectSpecteuR<-function(){
  #*****************************************************************************
  #*****************************************************************************
  # - Programme pour préparer l'acquisition de données de réflectance sur SAAC.
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
  # Crée un environnement unique pour l'instrument----
  ReflectEnv <- new.env(parent = .GlobalEnv)
  ReflectEnv$type <- "Reflectance"
  #*****************************************************************************
  
  #*****************************************************************************
  # Librairies pour MC et spectros----
  #*****************************************************************************
  source('Lib_n_Wrapper/OOInterface.R')
  source('Lib_n_Wrapper/MCDAQ.R')  #quelqes définitions utiles
  
  #*****************************************************************************
  # I N I T I A L I S A T I O N S ----
  #*****************************************************************************
  # On a besoin du spectro. On
  # va donc commencer par faire les intialisations
  # nécessaires.
  #____________________________________________________________________________
  #****************************************************************************
  ## Charge la configuration de l'instrument ----
  fichier_config <- rchoose.files(
    caption="RÉFLECTANCE - Choisir le fichier de configuration de l'instrument",
    default = normalizePath("Fichiers_Instruments"),
    multi = FALSE,filters = "*.R")
  source(fichier_config,encoding = "UTF-8", local=ReflectEnv)
  
  #*****************************************************************************
  ## Fichiers paramètres instrument s--------
  laCaption <- paste0("RÉFLECTANCE SUR ",ReflectEnv$nomInstrument,
                      " - Choisir le fichier des paramètres")
  fichier_param <- rchoose.files(caption=laCaption,
                                 default = normalizePath("Fichiers_Parametres"),
                                 multi = FALSE,filters = "*.R")
  source(fichier_param, encoding = "UTF-8", local=ReflectEnv)  
  ReflectEnv$fichier_param <- fichier_param
 
  #*****************************************************************************
  ## Démarre le wrapper Ocean Optics si nécessaire. ----
  # Recherche "OOobj" dans l'environnement .GlobalEnv. Si existe, on assume
  # que le wrapper est actif à moins que OOobj==NULL.
  if (!exists("OOobj",where=.GlobalEnv) || is.null(OOobj)){ #construit wrapper
    ### Active le "wrapper" donnant accès aux spectros. ----
    OOobj <<- Start_OO()
    if (is.null(OOobj)){
      rm(OOobj, envir = .GlobalEnv )
      cat("Incapable de définir un wrapper OO. \n\n")
      return("ABANDON")
    }
  }
  #*****************************************************************************
  ## Définir Spectros et plages d'interpolation ----
  
  # Trouve le spectro de l'instrument.
  ReflectEnv$lespectro=which((OOobj$serial_no==ReflectEnv$les_spectro$serial) & 
                            (OOobj$lesspectros==ReflectEnv$les_spectro$name))
  if (length(ReflectEnv$lespectro)==0){
    cat("Ne trouve pas le spectro!\n Va quitter...\n")
    Quit_OO(OOObj)
    rm(OOobj, envir = .GlobalEnv)
    return("ABANDON")
  }
  
  
    #On définit le spectro pour l'environnement de l'instrument.
    with(ReflectEnv,{ 
         lespectro <- Define_Spectro(OOobj,lespectro)
         SetCorrections(OOobj,lespectro,Lin = 1, Dark = 0)
         if (highGainMode){
           lespectro=GetHighGain(lespectro,OOobj$mywrap)
           SetHighGain(lespectro,OOobj$mywrap,highGainMode)
         }
         lespectro <- Define_WavelengthRange(
              lespectro,reflect_l_min,reflect_l_max,reflect_step
           )
    })
    
    #Lire la calibration en Y
    with(ReflectEnv, gainY<-scan(Maya_Calib_File,sep="\t",quiet=TRUE))
  
  
  #*****************************************************************************
  # État de la lampe blanche ----
  #************************************************************************************
  #
  #
  cat("\n***********************************\nVérification de la lampe\n")
 
  #***********************************************************************************
  # Mise sous tension de la lampe blanche et vérification du profil
  # On laisse la lampe ouverte mais on ferme le shutter.
  #***********************************************************************************

    mess <- paste0("\nS'assurer que la lampe blanche pour ",
                  ReflectEnv$nomInstrument,
                  " est en fonction. \n\n",
                  "Appuyer sur OK")
  utils::winDialog("ok",mess)
 
  ### Mesure sur le noir ----
  #Ferme le shutter
  mess <- paste0("\nFermer l'obturateur de la source blanche pour ",
                  ReflectEnv$nomInstrument,
                  "\n\n", "Appuyer sur OK.")
  utils::winDialog("ok",mess)
  
  with(ReflectEnv,{
       lespectro=Define_Acq_Param(OOobj,lespectro,
                                  T_Reflect,Box_Reflect,Scans_Reflect)
       Sys.sleep(0.2)
       #Mesure sur noir standard
       lespectro=Grab_Spectrum(lespectro,OOobj$mywrap)
       #Met le noir en réserve
       noir_std=lespectro$sp
       Plot_Spectrum(lespectro,"brut")
       title(main="Noir standard")
  })
  
  
  ### Mesure sur standard de réflectance ----
  mess <- paste0("\nPlacer le standard de réflectance  dans ",
                  ReflectEnv$nomInstrument, "\n\n",
                  "Ouvrir l'obturateur pour la source blanche.\n\n",
                  "Appuyer sur OK.")
  utils::winDialog("ok",mess)
  
  with(ReflectEnv,{
    lespectro=Grab_Spectrum(lespectro,OOobj$mywrap)
    blanc_std=lespectro$sp-noir_std
    lespectro$sp=blanc_std
    Plot_Spectrum(lespectro,"brut")
    title("Référence blanche")
  })
  
  ### Ferme le shutter----
  mess <- paste0("\nFermer l'obturateur de la source blanche pour ",
                  ReflectEnv$nomInstrument, "\n\n",
                  "Appuyer sur OK")
  utils::winDialog("ok",mess)
  
  
  ###Permet de quitter si c'est pas bon.----
  
  windows(height = 10, width = 8)
  ladev <- dev.cur()
  par(op)
  #3 graphiques - premier pour le logo et 2 suivants pour Noir et Référence Blanche
  nf=layout(matrix(c(1,2,3),3,1,byrow=TRUE))
  
  #Affiche le logo
  par(mai=c(0,0,0,0))
  plot(as.raster(logo))
  par(mai=op$mai*0.5)
  
  with(ReflectEnv,{
    Plot_Spectrum(lespectro,"brut")
    title("Référence blanche")
    lespectro$sp <- noir_std
    Plot_Spectrum(lespectro,"brut")
    title(main="Noir standard")
  })
  
  mess <- ("Satisfait du profil de la lampe?")
  dum <- utils::winDialog("yesno",mess)
  dev.set(ladev)
  par(op)
  dev.off()
  
  
  if (dum=="NO"){ #quitter le programme.
    #Clean and close
    
    for (sp in lesspectros) Close_Spectro(sp)
    rm(OOobj)
    Quit_OO(OOobj)
    par(op)
    graphics.off()
    return("ABANDON")
  }
  ReflectEnv$listDepth <- 1
  return(ReflectEnv)
}  

#END-----------------------------------------


