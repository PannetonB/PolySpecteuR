DoFluoSpecteuR<-function(F_Inst,leplan,tuneParams=FALSE){
#*******************************************************************************
#*******************************************************************************
# Programme pour l'acquisition de données de fluorescence avec SpectrAAC.
# 
# ENTRÉES
#     F_Inst    : environnement d'instrument créé par InitFluoSpecteuR.
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
#     posReps_Raman défini dans le fichier de paramètres). Il y a autant de
#     matrices que de longueurs d'onde d'excitation.
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
  #liste de listes vide pour les matrices de spectre en sortie 
  outerList <- list(Brut=list(),'Corrigé'=list())  
  
  #___________________________________________________________________________  
  #Permettre la modification des paramètres----
  if (tuneParams){
    edit(file=F_Inst$fichier_param)  
    source(R_Inst$fichier_param)
    
    # Lire les fichiers d'étalonnage au cas où changement!
    F_Inst$gainY=scan(F_Inst$Maya_Calib_File,sep="\t",quiet=TRUE)
  }
  
  
  
  #_______________________________________________________
  # Démarre la séquence d'acquistion -----------
  
  
    
  
   
  #///////////////////////////////////////////////////////////////
  
  #Pour limiter la lecture au noir à l'intervalle donné dans 
  leNoir <- matrix(0,nrow=1,ncol=length(F_Inst$lespectro$xaxis))
  t0Noir <- Sys.time()-(F_Inst$Delai_noir*60)
  
    
  # D'abord, regroupe des paramètres dans des listes/vecteurs
  Fluo_EX <- get_DELs_dat("DoEX", mon_envir = F_Inst)
  T_Fluo <- get_DELs_dat("T_EX", mon_envir = F_Inst)
  Box_FLuo <- get_DELs_dat("Box_EX", mon_envir = F_Inst)
  Scans_Fluo <- get_DELs_dat("Scans_EX", mon_envir = F_Inst)
  lesEXs <- get_DELs_dat("EX", mon_envir = F_Inst)
  FluoDel_I <- get_DELs_dat("LED_I_EX", mon_envir = F_Inst)
  
  
  F_Inst$lespectro=Define_WavelengthRange(F_Inst$lespectro,
                                          F_Inst$fluo_l_min,
                                          F_Inst$fluo_l_max,
                                          F_Inst$fluo_step)
  Sys.sleep(0.2)
 
  # └ Boucle sur les reps de position d'échantillon----
  for (krep in 1:F_Inst$posReps_Fluo){
    utils::winDialog(type="ok",
                     message=paste("FLUORESCENCE\nPlacer l'échantillon ", leplan$EchID,
                                   " à la position ",as.character(krep),
                                   " dans l'instrument ", F_Inst$nomInstrument,
                                   sep=""))
    
    #Remise à zéro de la fenêtre graphique
    
    
    # graphics.off()
    # par(op)
    # #10 graphiques - 2 premiers pour le logo et 7 suivants pour la fluo, la dernière pour réflectance
    # nf=layout(matrix(c(1,1,2,3,4,5,6,7,8,9),5,2,byrow=TRUE))
    # #Affiche le logo
    # par(mai=c(0,0,0,0))
    # plot(as.raster(logo))
    # 
    # par(mai=op$mai*0.35)
    
    #Définir un "flag" pour savoir s'il faut refaire les noirs
    tnow_noir=as.numeric(Sys.time())
    dt=(tnow_noir-as.numeric(t0Noir))/60
    flag_noir=FALSE
    if (dt>F_Inst$Delai_noir){
      cat("Délai pour noir: ",dt,"\n")
      flag_noir = TRUE
      t0Noir=as.numeric(Sys.time())
    }
    #└└  Boucle sur les excitations----
    for (k in F_Inst$RangFluo){
      if (Fluo_EX[k]){
        #Prep du spectro
        F_Inst$lespectro=Define_Acq_Param(OOobj,F_Inst$lespectro,
                                          T_Fluo[k],Box_FLuo[k],Scans_Fluo[k])
        Sys.sleep(0.2)
        
        #Prendre un spectre au noir
        if (flag_noir){
          cat("Acquisition d'un noir.\n")
          F_Inst$lespectro=Grab_Spectrum(F_Inst$lespectro,OOobj$mywrap)
          #Met le noir en réserve
          leNoir=F_Inst$lespectro$sp
        }
     
        #Allume la DEL à EXi_DEL_I milliampères
        I_OUT(F_Inst$lesDels[[k]],F_Inst$lesDevices,FluoDel_I[k])
        Sys.sleep(F_Inst$T_DEL)       #Attend T_DEL secondes pour stabiliser la DEL
        
        #Prendre un spectre brut de fluo
        
        cat(paste("Acquisition de fluorescence à "
                  ,as.character(F_Inst$lesEXs[k])," nm.\n",sep=""))
        
        F_Inst$lespectro=Grab_Spectrum(F_Inst$lespectro,OOobj$mywrap)
        
        #Coupe l'alimentation de la DEL (0 milliamps)
        I_OUT(F_Inst$lesDels[[k]],F_Inst$lesDevices,0)
        
        #Soustraire le noir
        F_Inst$lespectro$sp=F_Inst$lespectro$sp-leNoir
        
       
        #Applique la correction
        F_Inst$lespectro$sp=F_Inst$gainY*F_Inst$lespectro$sp
        #Normalise pour le pic d'étalonnage du début de la session
        F_Inst$lespectro$sp=F_Inst$lespectro$sp/F_Inst$NormY[k]
        
        
        #On fait l'interpolation
        F_Inst$lespectro=Interpolate_Spectrum(F_Inst$lespectro)
        #Normalise par le temps d'exposition en secondes
        F_Inst$lespectro$int$y=F_Inst$lespectro$int$y/T_Fluo[k]*1000  
        
        # On montre le spectre interpolé 
        x0=F_Inst$lesEXs[k]+25
        xend=range(F_Inst$lespectro$int$x)[2]
        i1=which(F_Inst$lespectro$int$x>=x0)[1]
        i2=length(F_Inst$lespectro$int$x)
        # Plot_Spectrum(F_Inst$lespectro,"interpole"
        #               ,xlim=c(x0,xend),
        #               ylim=range(F_Inst$lespectro$int$y[i1:i2]))
        # title(paste("Échantillon ",F_Inst$EchID," à ",
        #             as.character(F_Inst$lesEXs[k]),
        #             " - Rep = ",as.character(krep), sep=""))
        # grid()
       
       
      }else
      {
        # plot(c(0,1),c(0,1), type="n", axes=F, xlab="", ylab="")    #Pour passer au graphique suivant
        # text(0.5,0.5,paste("Pas de fluo. à ",
        #                    as.character(F_Inst$lesEXs[k]), sep=""))
      }
      
      # └└   Prépare la liste de sortie----
      if (krep==1) {
        nomEX <- paste0("EX",as.character(F_Inst$lesEXs[k]))
        outerList[['Brut']][[nomEX]] <- 
                    matrix(F_Inst$lespectro$xaxis,
                           nrow=2,ncol=length(F_Inst$lespectro$xaxis),
                           byrow=T)
        outerList[['Brut']][[nomEX]][2,] <- 
                    F_Inst$lespectro$sp
        
        outerList[['Corrigé']][[nomEX]] <- 
                    matrix(F_Inst$lespectro$int$x,
                           nrow=2,
                           ncol=length( F_Inst$lespectro$int$x),
                           byrow=T)
        outerList[['Corrigé']][[nomEX]][2,] <-  
                    F_Inst$lespectro$int$y
      }else
      {
        outerList[['Brut']][[nomEX]] <- 
                 rbind(outerList[['Brut']][[nomEX]],
                       F_Inst$lespectro$sp)
        outerList[['Corrigé']][[nomEX]] <- 
          rbind(outerList[['Corrigé']][[nomEX]],
                F_Inst$lespectro$int$y)
        
      }
    }  #Fin de boucle sur les EXs
   
    
  }#boucle des reps
  
  #///////////////////////////////////////////////////////////////
  cat("\n")
  cat("\n")
  utils::winDialog(type="ok",
                   message = paste("FLUORECENCE\nRetirer l'échantillon ", leplan$EchID,
                                   " de l'instrument ", F_Inst$nomInstrument,
                                   sep="")) 
  
  par(op)
  F_Inst$Spectres <- outerList
}



#Abort <- function(lesDevices,lesspectros,OOobj,op)----
Abort <- function(lesDevices,lesspectros,OOobj,op){
  for (k in 1:lesDevices$nbdevices) ReleaseBoard(lesDevices$BoardNum[k])
  (Quitte_MCLIB())
  for (sp in lesspectros) Close_Spectro(sp)
  rm(OOobj)
  Quit_OO(OOobj)
  cat("\nPolySPecteur a terminé! A la prochaine!\n")
  par(op)
  return()
}

#get_DELs_dat-----------------------------------------
get_DELs_dat <- function(varname,mon_envir=.GlobalEnv)
  #Fonction pour récupérer des vecteurs/listes de paramètres
{
  dum <- ls(envir = mon_envir, pattern = paste0("^",varname,".$"))
  dum <- unname(sapply(dum,FUN=function(x) get(x,envir=mon_envir)))
  return(dum)
}
#END-----------------------------------------


