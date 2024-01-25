load('dataPaths_n_names.RData')
if (dataPath!=""){
  dumAnswer <- utils::winDialog(type="yesno",
                   message=paste("Voulez-vous stocker les données dans",
                                 dataPath,'\\..._',dataSetID))
 
}

if ((dataPath=="") | (dumAnswer=="NO")){
  dataSetID <-utils::winDialogString(
  "Entrer un identifiant pour les noms de fichier de données",
  as.character(Sys.Date()))
  dataPath <- utils::choose.dir(default = "",
                              caption = "Choisir un répertoire pour stocker les données.")
  save(dataPath,dataSetID, file="dataPaths_n_names.RData")
}

