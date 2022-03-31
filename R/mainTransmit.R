mainTransmit <- function()
  
#********************************************************
# INSTRUCTIONS ---- 
#********************************************************
# Script d'acquisition de données avec SpectrAAC-2 lorsqu'on ne fait que de la  
# fluorescence.
# Pour utiliser un autre instrument ou plusieurs instruments, il faut modifier
# les 2 lignes avec la mention "#options" à la fin. La première des deux peut
# être remplacée par une ou plusieurs lignes à raison d'une ligne par type
# de mesure désirée. Par exemple, pour faire des mesures de fluorescence
# suivie de mesures de Raman, la première ligne pourrait être remplacée par:
#       F_inst <- InitFluoSpecteuR()  
#       R_inst <- InitRamanSpecteuR()
# Chacune des lignes appelant une routine d'initialisation doit commencer
# par un nom unique. Ensuite, il faut mettre à jour les éléments de la 
# liste "lesInstruments" définie dans la deuxième ligne. Pour l'exemple
# ci-haut, cela donnerait:
#       lesInstruments <- list(F_inst,R_inst)
# Noter que l'ordre dans cette liste définit l'ordre d'acquisition des 
# données.
#
#********************************************************
# AUTEUR: Bernard Panneton, Agriculture et Agroalimentaire Canada
# Mars 2022
#********************************************************
{  
  #********************************************************
  #Initialisation ----
  #********************************************************
  ##Paramètres graphiques par défaut et logo comme variables globales----
  op <<- par(no.readonly = TRUE)   
  logo <<- png::readPNG("PolySpecteuR_Logo.png")
  #********************************************************

  #********************************************************
  ## Enlève OOobj si existant----
  if (exists("OOobj", envir = .GlobalEnv))  rm(OOobj, envir=.GlobalEnv)
  #********************************************************

  #********************************************************
  ## Charger des packages R ----
  #********************************************************
  ok <- require("rlang")
  if (!ok) install.packages('rlang')
  ok <- require("utils")
  if (!ok) install.packages('utils')
  ok <- require("here")
  if (!ok) install.packages('here')
  
  # Se placer dans le répertoire R du projet PolySpecteur.
  # Pour que ça marche, il faut que le présent script
  # soit dans le répertoire R du projet PolySpecteur.
  # Cela permet d'utiliser efficacement des chemins
  # relatifs pour se rendre dans les différents répertoires
  # utilisés.
  RPath=here::here()
  setwd(RPath)
  
  #********************************************************
  ## Charger les scripts du projet PolySpecteuR ----
  #********************************************************
  setwd("R")
  files.sources = list.files(pattern=glob2rx("*.R"),full.names = TRUE)
  dum <- sapply(files.sources, source, encoding="UTF-8")
  setwd("..")
  
  #********************************************************
  ##Définir les instruments nécessaires et les initialiser ----
  #********************************************************
  
  Tr_Inst <- InitTransmitSpecteuR()   #instrument pour Fluorescence #options
  if (is.character(Tr_Inst)) return( "ABANDON")                     #options 
  
  lesInstruments <- list(Tr_Inst)                                   #options

  lestypes <- lapply(lesInstruments, function(I) I$type)
  
  #********************************************************
  ## Définir le plan d'expérience et le chemin pour stocker les données ----
  #********************************************************
  Plan <- GetPlanExp()
  dataSetID <-utils::winDialogString(
    "Entrer un identifiant pour les noms de fichier de données",
    as.character(Sys.Date()))
  dataPath <- utils::choose.dir(default = "",
          caption = "Choisir un répertoire pour stocker les données.")
  
  #********************************************************
  ## Permettre la modif des paramètres d'acquisition à chaque échantillon ----
  #********************************************************
  tuneParams <- FALSE
  yesno <- utils::winDialog("yesno",
         "Permettre la modification des paramètres à chaque échantillon.")
  if (yesno=="YES") tuneParams <- TRUE
  
  #********************************************************
  #Phase d'acquisition de données ----
  #********************************************************
  goOn <- TRUE
  while(goOn){    ## Boucle sur les échantillons----
    letest <- PickFromPlan(Plan)
    if (letest == "OK"){
      k=0
      isValid <- TRUE
      ### Boucle sur les instruments----
      for (t in lestypes){   
        k <- k+1
        if (t=="Raman"){
          if (isValid) {
            DoRamanSpecteuR(lesInstruments[[k]], Plan, tuneParams)
            dum <- Plots_2_Shiny_MultiLevels(lesInstruments[[k]])
            isValid <- (dum=="OK") & isValid
          }
        }
        if (t=="Fluorescence"){
          if (isValid){
             DoFluoSpecteuR(lesInstruments[[k]], Plan, tuneParams)
             dum <- Plots_2_Shiny_MultiLevels(lesInstruments[[k]])
             isValid <- (dum=="OK") & isValid
          }
        }
        if (t=="Reflectance"){
          if (isValid){
            DoReflectSpecteuR(lesInstruments[[k]], Plan, tuneParams)
            dum <- Plots_2_Shiny_MultiLevels(lesInstruments[[k]])
            isValid <- (dum=="OK") & isValid
          }
        }
        if (t=="Transmittance"){
          if (isValid){
            DoTransmitSpecteuR(lesInstruments[[k]], Plan, tuneParams)
            dum <- Plots_2_Shiny_MultiLevels(lesInstruments[[k]])
            isValid <- (dum=="OK") & isValid
          }
        }
      }

      ### Écriture des données si valides----
      if (isValid){      
         writeYFile(Plan, dataPath,dataSetID)
         writeData(Plan,lesInstruments,dataPath,dataSetID) 
      }else
        #Enlève la dernière entrée si plan manuel.  
      {
        if (Plan$leType == "Manuel"){
          nRowPlan <- nrow(Plan$leplan)
          if (nRowPlan==1){
            Plan$leplan <- data.frame()
            Plan$selected <- 0
            Plan$EchID <- ""
          }else
          {
            Plan$leplan <- Plan$leplan[-nRowPlan,]
            Plan$selected <- nRowPlan-1
            Plan$EchID <- Plan$leplan[Plan$selected,1]
          }
        }
      }
    }  
    
    ### Option de continuer ou quitter ----
    sel <- select.list(c("Oui","Non"), preselect = "Oui", 
          title="CONTINUER?",graphics = T)
    goOn <- ifelse(sel=="Oui",TRUE,FALSE)
  }        #Fin de la boucle sur les échantillons
  
  #********************************************************
  #Nettoyage et arrêt du script ----
  #********************************************************
  setwd(RPath)
  Clean_n_Close(lesInstruments)
}