writeData <- function(Plan,lesInstruments,dataPath,dataSetID)
  #****************************************************************************
  # Fonction qui écrit les données spectrales dans un format pour InSpectoR.
  # 
  # Les données spectrales brutes sont stockées dans un sous-répertoire
  # de "dataPath" nommé "Brutes". 
  # Les données spectrales interpolées et corrigées sont stockées dans
  # le répertoire "dataPath".
  # Pour un répertoire "dataPath", il faut s'assurer de n'utiliser
  # le "dataSetID" qu'une seule fois.
  #      
  # ENTRÉES:
  #     Plan       : environnement Plan créé par GetPlanExp.R et modifié 
  #                   par PickFromPlan.R  
  # lesInstruments : liste créé par une routine mainXXXX.R (e.g. mainFluo.R)
  # dataPath       : chemin vers le répertoire pour stocker les données
  # dataSetID      : identifiant pour former le nom du fichier. Même identifiant
  #                   doit être utilisé pour les fichiers des spectres. 
  #
  # SORTIE : aucune. Le fichier des Y est créé et/ou modifié.
  #****************************************************************************
  #
  #****************************************************************************
  # AUTEUR: Bernard Panneton, Agriculture et Agroalimentaire Canada
  # Mars 2022
  #****************************************************************************    
{ 
  # Fonctions utilitaires ----
  depth <- function(this,thisdepth=0){
    # Function to define depth of nested lists  
    if(!is.list(this)){
      return(thisdepth)
    }else{
      return(max(unlist(lapply(this,depth,thisdepth=thisdepth+1))))    
    }
  }
  
  # Mettre en place la structure de répertoire si nécessaire----
  if (!dir.exists(file.path(dataPath,"Brutes")))
      dir.create(file.path(dataPath,"Brutes"))
  
 
  #Inclure la profondeur de liste dans lesInstruments
  dum <- lapply(1:length(lesInstruments), function(i){
    lesInstruments[[i]]$listDepth <- depth(lesInstruments[[i]]$Spectres)
  })
  
 
  dum <- lapply(lesInstruments, function(Inst){
    if (Inst$listDepth==1){
      fname <- file.path(dataPath,"Brutes",
                         paste0(substr(Inst$type,1,5),
                                "w",
                                Inst$nomInstrument,
                                "_",
                                dataSetID,".txt")
      )
      
      DTypes <- names(Inst$Spectres)
      lesBrutes <- which(stringr::str_detect(toupper(DTypes),"RUT"))
      
      dataMat <- Inst$Spectres[[lesBrutes]]
      xdata <- dataMat[1,]
      #Faire la moyenne si plus d'une position d'échantillon
      if (nrow(dataMat)==2){
        sp <- dataMat[-1,]
      }else
      {
        sp <- colMeans(dataMat[-1,])
      }
      
      if (!file.exists(fname)){
        mycon=file(fname,"a")   #Axe des x puis le spectre
        cat("\t",file=mycon) #Saute le première colonne, EchID
        cat(xdata,file=mycon,sep="\t")   #axe des x
        cat("\n",file=mycon)
        cat(paste(Plan$EchID,"\t",sep=""),file=mycon)  #Id. d'échantillon
        cat(sp,file=mycon,sep="\t") #données
        cat("\n",file=mycon)
        close(mycon)
      }else
      {
        mycon=file(fname,"a")
        cat(paste(Plan$EchID,"\t",sep=""),file=mycon)
        cat(sp,file=mycon,sep="\t")
        cat("\n",file=mycon)
        close(mycon)
      }
    }
  })
  
  
  # if (listDepth==2){
  dum <- lapply(lesInstruments, function(Inst){
    if (Inst$listDepth==2){
      
      DTypes <- names(Inst$Spectres)
      lesBrutes <- which(stringr::str_detect(toupper(DTypes),"RUT"))
      
      dataMat <- Inst$Spectres[[lesBrutes]]
      lesNoms <- names(dataMat)
      for (k in 1:length(dataMat)){
        fname <- file.path(dataPath,"Brutes",
                           paste0(lesNoms[k],
                                  "w",
                                  Inst$nomInstrument,
                                  "_",
                                  dataSetID,".txt")
        )
        
        xdata <- dataMat[[k]][1,]
        #Faire la moyenne si plus d'une position d'échantillon
        if (nrow(dataMat[[k]]==2)){
          sp <- dataMat[[k]][-1,]
        }else
        {
          sp <- colMeans(dataMat[[k]][-1,])
        }
        
        if (!file.exists(fname)){
          mycon=file(fname,"a")   #Axe des x puis le spectre
          cat("\t",file=mycon) #Saute le première colonne, EchID
          cat(xdata,file=mycon,sep="\t")   #axe des x
          cat("\n",file=mycon)
          cat(paste(Plan$EchID,"\t",sep=""),file=mycon)  #Id. d'échantillon
          cat(sp,file=mycon,sep="\t") #données
          cat("\n",file=mycon)
          close(mycon)
        }else
        {
          mycon=file(fname,"a")
          cat(paste(Plan$EchID,"\t",sep=""),file=mycon)
          cat(sp,file=mycon,sep="\t")
          cat("\n",file=mycon)
          close(mycon)
        }
      }
    }
  })
  
  
  # Stocke les données interpolées ou corrigées----
 
  dum <- lapply(lesInstruments, function(Inst){
    if (Inst$listDepth==1){
      DTypes <- names(Inst$Spectres)
      if (any(stringr::str_detect(toupper(DTypes),"ORRIG"))){
        letype <- which(stringr::str_detect(toupper(DTypes),"ORRIG"))
      }else
      {
        letype <- which(stringr::str_detect(toupper(DTypes),"ERPOL"))
      }
      
      fname <- file.path(dataPath,
                         paste0(substr(Inst$type,1,5),
                                "w",
                                Inst$nomInstrument,
                                "_",
                                dataSetID,".txt")
      )
      
      dataMat <- Inst$Spectres[[letype]]
      xdata <- dataMat[1,]
      
      #Faire la moyenne si plus d'une position d'échantillon
      if (nrow(dataMat)==2){
        sp <- dataMat[-1,]
      }else
      {
        sp <- colMeans(dataMat[-1,])
      }
      
      if (!file.exists(fname)){
        mycon=file(fname,"a")   #Axe des x puis le spectre
        cat("\t",file=mycon) #Saute le première colonne, EchID
        cat(xdata,file=mycon,sep="\t")   #axe des x
        cat("\n",file=mycon)
        cat(paste(Plan$EchID,"\t",sep=""),file=mycon)  #Id. d'échantillon
        cat(sp,file=mycon,sep="\t") #données
        cat("\n",file=mycon)
        close(mycon)
      }else
      {
        mycon=file(fname,"a")
        cat(paste(Plan$EchID,"\t",sep=""),file=mycon)
        cat(sp,file=mycon,sep="\t")
        cat("\n",file=mycon)
        close(mycon)
      }
    }
  })
  
  # if (listDepth==2){
  dum <- lapply(lesInstruments, function(Inst){
    if (Inst$listDepth==2){
      DTypes <- names(Inst$Spectres)
      if (any(stringr::str_detect(toupper(DTypes),"ORRIG"))){
        letype <- which(stringr::str_detect(toupper(DTypes),"ORRIG"))
      }else
      {
        letype <- which(stringr::str_detect(toupper(DTypes),"ERPOL"))
      }
      
      dataMat <- Inst$Spectres[[letype]]
      lesNoms <- names(dataMat)
      for (k in 1:length(dataMat)){
        fname <- file.path(dataPath,
                           paste0(lesNoms[k],
                                  "w",
                                  Inst$nomInstrument,
                                  "_",
                                  dataSetID,".txt")
        )
        
        xdata <- dataMat[[k]][1,]
        
        #Faire la moyenne si plus d'une position d'échantillon
        if (nrow(dataMat[[k]]==1)){
          sp <- dataMat[[k]][-1,]
        }else
        {
          sp <- colMeans(dataMat[[k]][-1,])
        }
        
        if (!file.exists(fname)){
          mycon=file(fname,"a")   #Axe des x puis le spectre
          cat("\t",file=mycon) #Saute le première colonne, EchID
          cat(xdata,file=mycon,sep="\t")   #axe des x
          cat("\n",file=mycon)
          cat(paste(Plan$EchID,"\t",sep=""),file=mycon)  #Id. d'échantillon
          cat(sp,file=mycon,sep="\t") #données
          cat("\n",file=mycon)
          close(mycon)
        }else
        {
          mycon=file(fname,"a")
          cat(paste(Plan$EchID,"\t",sep=""),file=mycon)
          cat(sp,file=mycon,sep="\t")
          cat("\n",file=mycon)
          close(mycon)
        }
      }
    }
  })
}