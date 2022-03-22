GetPlanExp <- function()
#****************************************************************************
# Fonction qui offre 2 options:
#   1 : lit un fichier de plan d'expérience et retourne un environnement
#       contenant les informations au sujet de ce plan.
#   2 : Prépare pour que les identifiants des échantillons soient entrés
#       à la main par l'utilisateur à chaque fois que l'analyse donne des
#       résultats acceptables. Retourne un environnement contenant les 
#       informations pertinentes.  
#****************************************************************************
#
#****************************************************************************
# AUTEUR: Bernard Panneton, Agriculture et Agroalimentaire Canada
# Février 2022
#****************************************************************************
{
  #Make sure required packages are available----
  lesPackages <- c("editData","rChoiceDialogs")
  lapply(lesPackages, function(pp){
    ok <- require(pp, character.only = TRUE)
    if (!(ok)){
      install.packages(pp,dependencies = T, character.only = TRUE)
    } 
  })
  

  #Choix de l'utilisateur ----
  dum <- utils::winDialog("yesno","Utiliser un plan d'expérience?")
  
  if (dum=="YES"){
    
    # └ Plan d'expérience, liste des échantillons ----
    fichier_plan=rchoose.files(caption = "Choisir les fichier du plan d'expérience",
                               multi=FALSE,filters="*.txt")
    leplan=read.table(fichier_plan,header=TRUE,sep="\t")
    nrow=dim(leplan)[1]
    ncol=dim(leplan)[2]
    liste_ids=c()                          #Liste décrivant les échantillons
    for (k in 1:nrow){
      ligne=""
      for (i in 1:ncol){
        ligne=paste(ligne,as.character(leplan[k,i]))
      }
      liste_ids=rbind(liste_ids,ligne)
    }
    leType <- "Fichier"
  }else 
  {# └ Définir les descripteurs -----
    liste_ids <- as.matrix(Define_Descrip())
    leType <- "Manuel"
    leplan <- data.frame()
  }
  
  # Return values----
  plan <- new.env()
  #leType: "Manuel" ou "Fichier"
  plan$leType <- leType
  #Type=="Manuel": liste des champs, autrement liste des échantillons
  plan$liste_ids <- liste_ids 
  #Plan d'expérience sous forme de tableau. Vide si leType=="Manuel"
  plan$leplan <- leplan
  #selected: numéro de l'échantillon courant. 0 au début.
  plan$selected <- 0
  #EchID: identificateur unique de l'échantillon courant.
  plan$EchID <- ""
  return(plan)
# END ----  
}