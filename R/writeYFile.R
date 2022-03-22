writeYFile<-function(Plan, dataPath,dataSetID)
#****************************************************************************
# Fonction qui écrit les données du plan d'expérience dans un fichier Y 
# d'InSpectoR.
#      
# ENTRÉES:
#     Plan    : environnement Plan créé par GetPlanExp.R et modifié 
#               par PickFromPlan.R  
# dataPath    : chemin vers le répertoire pour stocker les données
# dataSetID   : identifiant pour former le nom du fichier. Le même identifiant
#               doit être utilisé pour les fichiers des spectres. 
#
# SORTIE : aucune. Le fichier des Y est créé et/ou modifié.
#****************************************************************************
#
#****************************************************************************
# AUTEUR: Bernard Panneton, Agriculture et Agroalimentaire Canada
# Février 2022
#****************************************************************************  
#Écrire les données ds un fichier en format txt avec "tab" comme séparateur
{ 
  #Nom complet du fichier----
  fichierDat=paste(dataPath,"\\Y","_",dataSetID,".txt",sep="")
  
  #Date et heure pour insérer dans le fichier----
  ladate=as.character(Sys.Date())
  letemps=format(Sys.time(),"%X")
  
  #Création d'une ligne pour écrire dans le fichier.----
  ligne=NULL
  ncol=dim(Plan$leplan)[2]
  for (i in 1:ncol){
    ligne=c(ligne,as.character(Plan$leplan[Plan$selected,i]))
  }
  ligne=c(ligne,ladate,letemps)
  
  #Écrire les données----
  #dans un fichier en format txt avec "tab" comme séparateur
  #On procède
  if (file.exists(fichierDat)==TRUE){  #└seulement les identifiants----
    mycon=file(fichierDat,"a")
    cat(ligne,file=mycon,sep="\t")
    cat("\n",file=mycon)
    close(mycon)
  }else
  {
    #└Entête puis les identifiants----
    entete=c(names(Plan$leplan),"Date","Heure")
    mycon=file(fichierDat,"a")   
    cat(entete,file=mycon,sep="\t")   #entête
    cat("\n",file=mycon)
    cat(ligne,file=mycon,sep="\t") #données
    cat("\n",file=mycon)
    close(mycon)
  }
  invisible(0)
}

