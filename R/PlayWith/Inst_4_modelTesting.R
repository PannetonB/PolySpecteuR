#Script pour créer des objets instrument avec membre Spectres pour tester
#l'application des modèles.

rm(list=ls())


get_DELs_dat <- function(varname,mon_envir=.GlobalEnv)
  #Fonction pour récupérer des vecteurs/listes de paramètres
{
  dum <- ls(envir = mon_envir, pattern = paste0("^",varname,".$"))
  dum <- unname(sapply(dum,FUN=function(x) get(x,envir=mon_envir)))
  return(dum)
}

library(here)
setwd(here())
op <<- par(no.readonly = TRUE)   
logo <<- png::readPNG("PolySpecteuR_Logo.png")
load(file.path(here(),"TestData","Rance_Huiles","InSpectoRData_4_models.RData"))

#Fluorescence 1
source(file.path(here::here(),"R/PlayWith/InitFluoSpecteuR_noMCDAQ.R"), encoding = 'UTF-8', echo=TRUE)
F_Inst <- InitFluoSpecteuR()
F_Inst$nomInstrument <- "Fluo1"
Fluo_EX <- get_DELs_dat("DoEX", mon_envir = F_Inst)


outerList <- list()

for (krep in 1:F_Inst$posReps_Fluo){
  for (k in F_Inst$RangFluo){
    if (Fluo_EX[k]){
      if (krep==1) {
        nomEX <- paste0("EX",as.character(F_Inst$lesEXs[k]))
        whichData <- which(stringr::str_detect(XDatalist,nomEX))
        x <- XData[[whichData]][1,-1]
        y <- XData[[whichData]][2,-1]
        outerList[['Brut']][[nomEX]] <- 
          matrix(x,
                 nrow=2,ncol=length(x),
                 byrow=T)
        outerList[['Brut']][[nomEX]][2,] <- y
        
        outerList[['Corrigé']][[nomEX]] <- 
          matrix(x,
                 nrow=2,
                 ncol=length(x),
                 byrow=T)
        outerList[['Corrigé']][[nomEX]][2,] <-y
      }else
      {
        whichData <- which(stringr::str_detect(XDatalist,nomEX))
        x <- XData[[whichData]][1,-1]
        y <- XData[[whichData]][2,-1]
        outerList[['Brut']][[nomEX]] <- 
          rbind(outerList[['Brut']][[nomEX]],x)
        outerList[['Corrigé']][[nomEX]] <- 
          rbind(outerList[['Corrigé']][[nomEX]],y)
        
      }
    }
  }
}
F_Inst$Spectres <- outerList

source('~/Consultant/AAC_2022/Progs/RProjets/PolySpecteuR/R/Plots_2_Shiny_MultiLevels.R', encoding = 'UTF-8', echo=TRUE)
Plots_2_Shiny_MultiLevels(F_Inst)


#Fluorescence 2
F_Inst2 <- rlang::env_clone(F_Inst, parent = parent.env(F_Inst))
F_Inst2$nomInstrument <- "Fluo2"

outerList <- list()

for (krep in 1:F_Inst$posReps_Fluo){
  for (k in F_Inst$RangFluo){
    if (Fluo_EX[k]){
      if (krep==1) {
        nomEX <- paste0("EX",as.character(F_Inst$lesEXs[k]))
        whichData <- which(stringr::str_detect(XDatalist,nomEX))
        x <- XData[[whichData]][1,-1]
        y <- XData[[whichData]][3,-1]
        outerList[['Brut']][[nomEX]] <- 
          matrix(x,
                 nrow=2,ncol=length(x),
                 byrow=T)
        outerList[['Brut']][[nomEX]][2,] <- y
        
        outerList[['Corrigé']][[nomEX]] <- 
          matrix(x,
                 nrow=2,
                 ncol=length(x),
                 byrow=T)
        outerList[['Corrigé']][[nomEX]][2,] <-y
      }else
      {
        whichData <- which(stringr::str_detect(XDatalist,nomEX))
        x <- XData[[whichData]][1,-1]
        y <- XData[[whichData]][3,-1]
        outerList[['Brut']][[nomEX]] <- 
          rbind(outerList[['Brut']][[nomEX]],x)
        outerList[['Corrigé']][[nomEX]] <- 
          rbind(outerList[['Corrigé']][[nomEX]],y)
        
      }
    }
  }
}
F_Inst2$Spectres <- outerList

Plots_2_Shiny_MultiLevels(F_Inst2)


#Raman 1
source(file.path(here::here(),"R/PlayWith/InitRamanSpecteuR_noLaser.R"), encoding = 'UTF-8', echo=TRUE)
R_Inst <- InitRamanSpecteuR()

R_Inst$nomInstrument <- "Raman1"

outlist <- list()   #liste vide pour les matrices de spectre en sortie 
R_Inst$posReps_Raman <-1

for (k in 1:R_Inst$posReps_Raman){
  if (k==1) {
    whichData <- which(stringr::str_detect(XDatalist,"Rama"))
    x <- XData[[whichData]][1,-1]
    y <- XData[[whichData]][2,-1]
    outlist[['Brut']] <- matrix(x,
                                nrow=2,ncol=length(x),
                                byrow=T)
    outlist[['Brut']][2,] <- y
    outlist[['Interpolé']] <- matrix(x,
                                     nrow=2,ncol=length(x),
                                     byrow=T)
    
    y <- XData[[whichData]][3,-1]
    outlist[['Interpolé']][2,] <- y
    
    if (R_Inst$do_raman_baseline){
      outlist[['Base']] <- matrix(x,
                                  nrow=2,ncol=length(x),
                                  byrow=T)
      
      y <- XData[[whichData]][4,-1]
      outlist[['Base']][2,] <- y
      outlist[['Corrigé']] <- matrix(x,
                                     nrow=2,ncol=length(x),
                                     byrow=T)
      outlist[['Corrigé']][2,] <- y
    }
  }else
  {
    whichData <- which(stringr::str_detect(XDatalist,"Rama"))
    x <- XData[[whichData]][1,-1]
    y <- XData[[whichData]][5,-1]
    outlist[['Brut']] <- rbind(outlist[['Brut']],y)
    y <- XData[[whichData]][6,-1]
    outlist[['Interpolé']] <- rbind(outlist[['Interpolé']],y)
    if (R_Inst$do_raman_baseline){
      whichData <- which(stringr::str_detect(XDatalist,"Rama"))
      x <- XData[[whichData]][1,-1]
      y <- XData[[whichData]][7,-1]
      outlist[['Base']] <- rbind(outlist[['Base']],y)
      y <- XData[[whichData]][8,-1]
      outlist[['Corrigé']] <- rbind(outlist[['Corrigé']],y)
    }
  }
  
  
}

R_Inst$Spectres <- outlist

Plots_2_Shiny_MultiLevels(R_Inst)

F_Inst$listDepth <- 2
F_Inst2$listDepth <- 2
R_Inst$listDepth <- 1

lesInstruments <- list(F_Inst, F_Inst2, R_Inst)                #options
#lesInstruments <- list(F_Inst) 

lestypes <- lapply(lesInstruments, function(I) I$type)
