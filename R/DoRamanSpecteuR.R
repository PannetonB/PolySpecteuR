DoRamanSpecteuR <- function(R_Inst,leplan,tuneParams=FALSE)
#*******************************************************************************
#*******************************************************************************
# Programme pour l'acquisition de données Raman avec SpectrAAC.
# 
# ENTRÉES
#     R_Inst    : environnement d'instrument créé par InitRamanSpecteuR.
#                 Cet environnement comprend la configuration de l'instrument,
#                 le lien avec le spectro pour OmniDriver. Le wrapper a été 
#                 créé dans l'environnement global de R pour être accessible
#                 par tous les instruments. L'environnement comprend aussi
#                 les paramètres d'acquisition.
#    leplan     : environnement créé par GetPlanExp et PickFromPlan contenant
#                 les descripteurs d'échantillon et l'échantillon courant
#                 dans le champ "selected".
#   tuneParams  : Permet la modification des paramètres à chaque fois.
#
# SORTIE
#     Une liste de matrices de spectres. Un spectre par ligne et autant de lignes
#     que le nombre de répétition sur les positions de l'échantillon (paramètre
#     posReps_Raman défini dans le fichier de paramètres).
#         - Si do_raman_baseline==FALSE: une seule matrice dans la liste avec les 
#           spectres interpolés.
#         - Si do_raman_baseline==TRUE: Trois matrices dans la liste: les 
#           spectres interpolés, les spectres de ligne de base, les spectres
#           sans la ligne de base.
#*******************************************************************************
# AUTEUR: Bernard Panneton, Agriculture et Agroalimentaire Canada
# Fevrier 2022
#*******************************************************************************
#*******************************************************************************
{
  #___________________________________________________________________________
  #Charge des librairies additionnelles-----
  ok <- require(utils)
  if (!ok){
    install.packages("utils",dependencies = T)
  }
  ok <- require(baseline)
  if (!ok){
    install.packages("baseline",dependencies = T)
  }
  
  #___________________________________________________________________________
  outlist <- list()   #liste vide pour les matrices de spectre en sortie 
 
  
  #___________________________________________________________________________  
  #Permettre la modification des paramètres----
  if (tuneParams){
    edit(file=R_Inst$fichier_param)  
    source(R_Inst$fichier_param)
    
    # Lire les fichiers d'etalonnage
    R_Inst$gainQE=scan(R_Inst$QEPro_Calib_File,sep="\t",quiet=TRUE)
  }
      
     
   
      
  #///////////////////////////////////////////////////////////////
  #Acquisition du spectre RAMAN - Corrections - Stockage----
  with(R_Inst, {
    lespectro=Define_WavenumberRange(lespectro,ram_l_min,ram_l_max,ram_step,EX_Raman)
    lespectro=Define_Acq_Param(OOobj,lespectro,8,Box_Raman,Scans_Raman)  #idle at 8 msec
  })
  
  #Mettre l'echantillon en place
  #utils::winDialog("ok",paste0("RAMAN\nIntroduire l'échantillon ",leplan$EchID,"."))
  
  cat("\n*********************\nAcquisition du Raman.\n")
  
  # └ Spectre au noir----
  cat(" - Spectre au noir\n")
  with(R_Inst, {Noir=Grab_f_QE(lespectro,OOobj,T_Raman*1000)})
  
  #Allume le shutter du laser
  #UNCOMMENT
  ShutterOn(1)
  # R_Inst$Noir <- R_Inst$Noir[1:1044]
  # R_Inst$lespectro$xaxis <- R_Inst$lespectro$xaxis[1:1044]
  # R_Inst$lespectro$wn <- R_Inst$lespectro$wn[1:1044]
  #UNCOMMENT
  Sys.sleep(R_Inst$T_Laser)       #Attend pour stabiliser laser
  
  # └ Boucle sur les répétitions de positions----
  for (k in 1:R_Inst$posReps_Raman){
    mess <- paste0("RAMAN\nPlacer l'échantillon ",leplan$EchID, "à la position ", k,".")
    utils::winDialog("ok",mess)
    
    #  └ └   Prendre un spectre brut ----
    cat("   - Signal brut\n")
    with(R_Inst, {Signal=Grab_f_QE(lespectro,OOobj,T_Raman*1000)})
    
    #Coupe l'alimentation du shutter
    #UNCOMMENT
    # R_Inst$Signal <-  R_Inst$Signal[1:1044]
    #UNCOMMENT
    Sys.sleep(0.3)
    
  
    # └ └  Traite les spectres ----
    cat("   - Traite les spectres\n")
    with(R_Inst, {
      #Soustraire le noir
      lespectro$sp=Signal-Noir
      #Dvise par le temps d'integration en secondes (temps pour Raman en sec dans le fichier de paramC*tres)
      cat("   - Corrige pour temps d'intégration\n")
      lespectro$sp=lespectro$sp/T_Raman
      #Applique la correction
      cat("   - Corrige pour le gain\n")
      lespectro$sp=gainQE*lespectro$sp
      #Interpole
      cat("   - Interpolation\n")
      lespectro=Interpolate_Spectrum(lespectro)
      #Enleve la ligne de base si demande
      if (do_raman_baseline){
        cat("   - Ligne de base\n")
        bc<-baseline::baseline(t(as.matrix(lespectro$int$y)), method='modpolyfit',
                     degree=baseline_deg,
                     rep=baseline_rep, tol = baseline_tol)
      }
    })
    
    
    # └ └  Prépare l'output -----
    if (k==1) {
      outlist[['Brut']] <- matrix(R_Inst$lespectro$xaxis,
                                  nrow=2,ncol=length(R_Inst$lespectro$xaxis),
                                  byrow=T)
      outlist[['Brut']][2,] <- R_Inst$lespectro$sp
      outlist[['Interpolé']] <- matrix(R_Inst$lespectro$int$x,
                                     nrow=2,ncol=length(R_Inst$lespectro$int$x),
                                     byrow=T)
      outlist[['Interpolé']][2,] <- R_Inst$lespectro$int$y
      
      if (R_Inst$do_raman_baseline){
        outlist[['Base']] <- matrix(R_Inst$lespectro$int$x,
                                       nrow=2,ncol=length(R_Inst$lespectro$int$x),
                                       byrow=T)
        outlist[['Base']][2,] <- baseline::getBaseline(R_Inst$bc)
        outlist[['Corrigé']] <- matrix(R_Inst$lespectro$int$x,
                                       nrow=2,ncol=length(R_Inst$lespectro$int$x),
                                       byrow=T)
        outlist[['Corrigé']][2,] <- baseline::getCorrected(R_Inst$bc)
      }
    }else
      outlist[['Brut']] <- rbind(outlist[['Brut']],
                                 R_Inst$lespectro$sp)
      outlist[['Interpolé']] <- rbind(outlist[['Interpolé']],
                                    R_Inst$lespectro$int$y)
      if (R_Inst$do_raman_baseline){
        outlist[['Base']] <- rbind(outlist[['Base']],
                                 baseline::getBaseline(R_Inst$bc))
        outlist[['Corrigé']] <- rbind(outlist[['Corrigé']],
                                    baseline::getCorrected(R_Inst$bc))
      }
  }
  
  #UNCOMMENT
  ShutterOff(1)
  #UNCOMMENT
  
   
  #///////////////////////////////////////////////////////////////
  #Demande de retirer l'echantillon
  utils::winDialog("ok",paste0("RAMAN\nRetirer l'échantillon ",leplan$EchID,"."))

  #return(outlist)
  R_Inst$Spectres <- outlist
}

#Grab_f_QE------------
#************************************************************
Grab_f_QE <- function(QE,OOobj,T){
  #Pour QE65000  
  QE=Define_Int_Time(OOobj,QE,T)
  QE=Grab_Spectrum(QE,OOobj$mywrap)
  QE=Define_Int_Time(OOobj,QE,8) 
  dum=QE$sp
  QE=Grab_Spectrum(QE,OOobj$mywrap)
  return(dum)
}
