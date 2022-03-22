plotSetUp <- function(instruments)
#*******************************************************************************
#*******************************************************************************
# Programme pour prépare le panneau Plots de RStudio pour les graphiques
# 
# ENTRÉES
#     instruments  :  liste des instruments définis par les fonctions
#                     d'initialisation.
#
# SORTIE : lestypes constitué du type de mesure.
#           Prépare le panneau Plots avec des graphiques vides et affiche
#           le type de graphique qui sera présenté.
#*******************************************************************************
# AUTEUR: Bernard Panneton, Agriculture et Agroalimentaire Canada
# Fevrier 2022
#*******************************************************************************
#*******************************************************************************
{
  #Récupère le logo et ses dimensions----
  require(png)
  logo=png::readPNG("PolySpecteuR_Logo.png")
  xp <- dim(logo)[2]/2
  yp <- dim(logo)[1]/8
  
  #Définir un nom pour les figures----
  #le nom vient du champ "type" de l'environnement des instruments.
  nomFigures <- character()
  n_instruments <- length(instruments)
  lestypes <- lapply(instruments, function(e) e$type)
  types <- unique(lestypes)
  for (k in types){
    ind <- which(lestypes==k)
    for (i in 1:length(ind)){
      nomFigures <- c(nomFigures, paste0(k," - ",i))
    }
  }
  
  #Création de graphiques vides, un par type de mesure.
  if (!is.null(dev.list())) dev.off()  #efface tous les graphiques
  for (k in 1:length(nomFigures)){
    plot(as.raster(logo))
    text(xp,yp,nomFigures[k],cex=2)  #identifie la fenêtre
  }
  return(lestypes)
}
