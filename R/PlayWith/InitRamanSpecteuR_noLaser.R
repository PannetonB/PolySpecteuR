InitRamanSpecteuR<-function(){
  #*****************************************************************************
  #*****************************************************************************
  # - Programme pour préparer l'acquisition de données Raman avec le SpectrAAC.
  # - Identifier un fichier d'instrument.
  # - Initialiser le matériel.
  #*****************************************************************************
  # AUTEUR: Bernard Panneton, Agriculture et Agroalimentaire Canada
  # Février 2022
  #*****************************************************************************
  #*****************************************************************************
  #___________________________________________________________________________
  # Charge des librairies additionnelles-----
  #___________________________________________________________________________
  library(rChoiceDialogs)
  library(rstudioapi)
  
  #****************************************************************************
  # Crée un environnement unique pour l'instrument
  RamanEnv <- new.env(parent = .GlobalEnv)
  
  #****************************************************************************
  # Charge la configuration de l'instrument----
  # RamanSpecteuRPath=utils::getSrcDirectory(function(x) {x})
  # setwd(RamanSpecteuRPath)
  # 
  fichier_config <- rchoose.files(
    caption="RAMAN - Choisir le fichier de configuration de l'instrument",
    default = normalizePath("Fichiers_Instruments"),
    multi = FALSE,filters = "*.R")
  source(fichier_config,encoding = "UTF-8", local=RamanEnv)
  
  #****************************************************************************
  # Charge les parametres.----
  laCaption <- paste0("RAMAN SUR ",RamanEnv$nomInstrument,
                      " - Choisir le fichier des paramètres")
  fichier_param <- rchoose.files(caption=laCaption,
          default = normalizePath("Fichiers_Parametres"),
          multi = FALSE,filters = "*.R")
  source(fichier_param, encoding = "UTF-8", local=RamanEnv)  
  RamanEnv$fichier_param <- fichier_param
  
  #****************************************************************************
  # Démarre le wrapper Ocean Optics si nécessaire.----
  # Recherche "OOobj" dans l'environnement .GlobalEnv. Si existe, on assume
  # que le wrapper est actif à moins que OOobj==NULL.
  if (!exists("OOobj",where=.GlobalEnv) || is.null(OOobj)){ #construit OO wrapper
    # └ Charge la librairie donnant accès aux spectros. ----
    source('Lib_n_Wrapper/OOInterface.R', encoding = "UTF-8")
    #**************************************************************************
    # └ Active le "wrapper" donnant accès aux spectros.----
    OOobj <<- Start_OO()
    if (is.null(OOobj)){
      rm(OOobj, envir = .GlobalEnv )
      cat("Incapable de définir un wrapper OO. \n\n")
      return("ABANDON")
    }
  }
  
  #****************************************************************************
  # Définit le spectro dans l'environnement de sortie----
  RamanEnv$lespectro=which((OOobj$serial_no==RamanEnv$les_spectro$serial) & (OOobj$lesspectros==RamanEnv$les_spectro$name))
  if (length(RamanEnv$lespectro)==0){
    cat("Ne trouve pas le spectro!\n Va quitter...\n")
    Quit_OO(OOObj)
    rm(OOobj, envir = .GlobalEnv)
    return("ABANDON")
  }
  #On définit le spectro dans l'environnement de l'instrument.
  with(RamanEnv,{ 
    lespectro=Define_Spectro(OOobj, lespectro)
    lespectro=Define_WavenumberRange(lespectro,ram_l_min,ram_l_max,ram_step,EX_Raman)
    SetCorrections(OOobj,lespectro,Lin = 1,Dark = 0)
    #UNCOMMENT
    # lespectro=GetTEC(lespectro,OOobj$mywrap)
    # T_QE=CoolSpectro(lespectro,OOobj$mywrap,QE_Temp)
    # cat("\n")
    # dum=readline(paste("Le detecteur du QE est à  ",as.character(T_QE),"°C. Continuer? (O/N):",sep=""))
    # if (toupper(dum)=="N"){
    #   WarmSpectro(lespectro,OOobj$mywrap)
    #   Close_Spectro(lespectro)
    #   Quit_OO(OOobj)
    #   return("ABANDON")
    # }
    #UNCOMMENT
  })
  
  #****************************************************************************
  # Prépare pour utiliser LS2----
  
  # if (RamanEnv$sourceLaser=="LS2"){
  #   source('Lib_n_Wrapper/Newport_LS_2.R', encoding = "UTF-8")
  #   test <- Connect_LS_2(1)
  #   if (test=="OK"){
  #     LaserOn(1,RamanEnv$P_Laser)
  #     cat("\n")
  #     dum=readline(paste("Connexion au LS-2 réussie? (O/N):",sep=""))
  #     if (toupper(dum)=="N"){
  #       Close_LS2()
  #       WarmSpectro(RamanEnv$lespectro,OOobj$mywrap)
  #       Close_Spectro(RamanEnv$lespectro)
  #       Quit_OO(OOobj)
  #       return("ABANDON")
  #     }
  #   }else
  #   {
  #     #UNCOMMENT
  #     return("ABANDON")
  #     #UNCOMMENT
  #   }
  # }
  # 
  # if (RamanEnv$sourceLaser=="IPS"){
  #   source('Lib_n_Wrapper/IPS_Laser.R', encoding = "UTF-8")
  #   test <- Connect_IPS()
  #   if (test=="OK"){
  #     LaserOn(1,RamanEnv$C_Laser)
  #     cat("\n")
  #     dum <- LaserID()
  #     dum=readline(paste0("Connexion au laser IPS ",dum$serialno," réussie? (O/N):",sep=""))
  #     if (toupper(dum)=="N"){
  #       Close_IPS()
  #       WarmSpectro(RamanEnv$lespectro,OOobj$mywrap)
  #       Close_Spectro(RamanEnv$lespectro)
  #       Quit_OO(OOobj)
  #       return("ABANDON")
  #     }
  #   }else
  #   {
  #     #UNCOMMENT
  #     return("ABANDON")
  #     #UNCOMMENT
  #   }
  # }
  #   
  
  
  #****************************************************************************
  # Lire les fichiers d'étalonnage----
  RamanEnv$gainQE=scan(RamanEnv$QEPro_Calib_File,sep="\t",quiet=TRUE)
  
  #****************************************************************************
  # Retourne un environnement en sortie----
  RamanEnv$type <- "Raman"
  return(RamanEnv)
  # END----
}
