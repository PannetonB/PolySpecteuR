DoReflectSpecteuR<-function(Refl_Inst,leplan,tuneParams=FALSE){
#*******************************************************************************
#*******************************************************************************
# Programme pour l'acquisition de données de fluorescence avec SpectrAAC.
# 
# ENTRÉES
#     Refl_Inst    : environnement d'instrument créé par InitReflectSpecteuR.
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
#*******************************************************************************
# AUTEUR: Bernard Panneton, Agriculture et Agroalimentaire Canada
# Fevrier 2022
#*******************************************************************************
#*******************************************************************************

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
  
  #___________________________________________________________________________
  #liste vide pour les matrices de spectre en sortie 
  outlist <- list()  
  
  #___________________________________________________________________________  
  #Permettre la modification des paramètres----
  if (tuneParams){
    edit(file=Refl_Inst$fichier_param)  
    source(Refl_Inst$fichier_param)
    
    # Lire les fichiers d'étalonnage au cas où changement!
    Refl_Inst$gainY=scan(Refl_Inst$Maya_Calib_File,sep="\t",quiet=TRUE)
  }
  
  
  
  #_______________________________________________________
  # Démarre la séquence d'acquistion -----------
  
  with(Refl_Inst,{
    lespectro=Define_WavelengthRange(lespectro,reflect_l_min,reflect_l_max,reflect_step)
    Sys.sleep(0.2)
    
    lespectro=Define_Acq_Param(OOobj,lespectro,T_Reflect,Box_Reflect,Scans_Reflect)
    Sys.sleep(0.2)
  })
  
  #Remise à zéro de la fenêtre graphique
  # graphics.off()
  # par(op)
  # #10 graphiques - 2 premiers pour le logo. 
  # #3 pour noir de référence, 4 pour le blanc de référence, 5 à 10 pour les
  # #reps (pas plus de 6!)
  # nf=layout(matrix(c(1,1,2,3,4,5,6,7,8,9),5,2,byrow=TRUE))
  # #Affiche le logo
  # par(mai=c(0,0,0,0))
  # plot(as.raster(logo))
  # par(mai=op$mai*0.35)
  
  
  
  
  ## Mesure sur noir standard----
  #Ouvre le shutter
  mess <- paste("RÉFLECTANCE\nS'assurer que l'obturateur de la source blanche pour l'instrument ",
                Refl_Inst$nomInstrument, " est fermé. \n\n",
                "Puis appuyer sur OK.",
                sep="")
  utils::winDialog(type="ok",mess)
  with(Refl_Inst,{
    lespectro=Grab_Spectrum(lespectro,OOobj$mywrap)
    #Plot_Spectrum(lespectro,"brut")
    #title(main="Noir standard")
    #Met le noir en réserve
    noir_std=lespectro$sp
  })
  
  ## Mesure sur standard de réflectance ----
  mess <- paste("RÉFLECTANCE\nPlacer le standard de réflectance  dans l'instrument ",
                Refl_Inst$nomInstrument, "\n\n",
                "Ouvrir l'obturateur de la source blanche de l'instrument ",
                Refl_Inst$nomInstrument, "\n\n",
                "Appuyer sur OK.",sep="")
  utils::winDialog(type="ok",mess)
  with(Refl_Inst,{
    lespectro=Grab_Spectrum(lespectro,OOobj$mywrap)
    blanc_std=lespectro$sp-noir_std
    # Correction pour le stray light
    if (stray_low>0){
      i1 <- which(lespectro$xaxis>=stray_low)[1]
      i2 <- which(lespectro$xaxis>=stray_high)[1]
      stray <- mean(blanc_std[i1:i2])
      blanc_std <- blanc_std-stray
    }
    #lespectro$sp=blanc_std
    #Plot_Spectrum(lespectro,"brut")
  })
  
  ## Mesures sur échantillon ----
  for (krep in 1:Refl_Inst$Nb_reps_Reflect){
    mess <- paste("RÉFLECTANCE\nPlacer l'échantillon ",leplan$EchID,
                  " à la position ",as.character(krep),
                  "dans l'instrument ", Refl_Inst$nomInstrument,
                  "\n\n","Appuyer sur OK",sep="")
    utils::winDialog(type="ok",mess)
    
    #Lire le spectre brut
    with(Refl_Inst,{
      lespectro=Grab_Spectrum(lespectro,OOobj$mywrap)
      data_ech <- lespectro$sp-noir_std
      # Correction pour le stray light
      if (stray_low>0){
        i1 <- which(lespectro$xaxis>=stray_low)[1]
        i2 <- which(lespectro$xaxis>=stray_high)[1]
        stray <- mean(data_ech[i1:i2])
        data_ech <- data_ech-stray
      }
      
      #Calcul de réflectance
      lespectro$sp=data_ech/blanc_std
      
      #On fait l'interpolation
      lespectro=Interpolate_Spectrum(lespectro)
    })
    
    if (krep==1) {
      outlist[['Brut']] <- matrix(Refl_Inst$lespectro$xaxis,
                                  nrow=2,ncol=length(Refl_Inst$lespectro$xaxis),
                                  byrow=T)
      outlist[['Brut']][2,] <- Refl_Inst$data_ech
      
      outlist[['Blanc']] <- matrix(Refl_Inst$lespectro$xaxis,
                                       nrow=2,ncol=length(Refl_Inst$lespectro$xaxis),
                                       byrow=T)
      outlist[['Blanc']][2,] <- Refl_Inst$blanc_std
      
      
      outlist[['Corrigé']] <- matrix(Refl_Inst$lespectro$int$x,
                                   nrow=2,ncol=length(Refl_Inst$lespectro$int$x),
                                   byrow=T)
      outlist[['Corrigé']][2,] <- Refl_Inst$lespectro$int$y
      
    }else
    {
      outlist[['Brut']] <- rbind(outlist[['Brut']],
                                 Refl_Inst$data_ech)
      
      outlist[['Blanc']] <- rbind(outlist[['Blanc']],
                                  Refl_Inst$blanc_std)
      
      outlist[['Corrigé']] <- rbind(outlist[['Corrigé']],
                                    Refl_Inst$lespectro$int$y)
    }
    
  }#boucle des reps
  
  #///////////////////////////////////////////////////////////////
  cat("\n")
  cat("\n")
  utils::winDialog(type="ok",
                   message = paste("RÉFLECTANCE\nRetirer l'échantillon ", leplan$EchID,
                                   " de l'instrument ", Refl_Inst$nomInstrument,
                                   sep="")) 
  
  par(op)
  Refl_Inst$Spectres <- outlist
}


