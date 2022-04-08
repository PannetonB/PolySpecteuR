InitModels<-function(lesInstruments){
  #*****************************************************************************
  #*****************************************************************************
  # - Programme pour charger les modèles qui seront appliqués aux données.
  # - Cette fonction doit être appelée une fois que tous les instruments
  #   ont été initialisés (e.g InitRamanSpecteuR.R) et que la liste des 
  #   environnements d'instruments a été constituée.
  # - Les modèles sont développés et stockés par InSpectoR.
  # - On utilise la fonction DoModels pour appliquer les modèles à chaque
  #   fois que de nouvelles données pour un échantillon sont obtenues.
  # On prend pour acquis que le modèle n'est pas appliqué aux données brutes mais
  # aux données interpolées/corrigées.
  #
  # ENTRÉE
  #    lesInstruments : la liste des environnements des différents instruments
  #                     créée dans un script mainXXXX.R (e.g. mainRaman.R). 
  #*****************************************************************************
  # AUTEUR: Bernard Panneton, Agriculture et Agroalimentaire Canada
  # Février 2022
  #*****************************************************************************
  #*****************************************************************************
  # Charge des librairies additionnelles----
  lesPackages <- c("shiny","miniUI","tools")
  dum <- lapply(lesPackages, function(pp){
    ok <- require(pp, character.only = TRUE)
    if (!(ok)){
      install.packages(pp,dependencies = T, character.only = TRUE)
    } 
  })
  
  #*****************************************************************************
  #Environnement vide avec les modèles----
  modelEnv <<- new.env(parent = .GlobalEnv)
  
  #*****************************************************************************
  # Shiny gadget pour créer un environnement contenant les modèles ----
  lesModsFiles <<- character()
  
  ui <- miniPage(
    gadgetTitleBar("Choix des modèles à appliquer",
                   right = miniTitleBarButton("done", "Terminer", primary = TRUE),
                   left = NULL),
    
    miniContentPanel(
      
      htmlOutput("titre"),
      fileInput("mods","Modèle 1",
                buttonLabel = "Choisir des fichiers de modèles",
                width = "600px"),
      hr(),
      h3("Liste des modèles sélectionnés"),
      htmlOutput("mod1")
    )
  )
  server = function(input,output,session){
    
    #permettre des fichiers de 60 MB et moins
    options(shiny.maxRequestSize=100*1024^2) 
    
    output$titre = renderText("<b>Nom des modèles")
    #A chaque fois que l'utilisateur appuie sur le bouton "Ajouter un champ",
    #  on crée un "textInput" pour entrer le nom du nouveau champ
    
    output$mod1 <- renderText({
      dum <- input$mods
      ifelse(is.null(dum),"Aucun modèle",{
        lesModsFiles <<- c(lesModsFiles,dum$name)
        modName <- tools::file_path_sans_ext(dum$name)
        createEnvStr <- paste0("modelEnv$",modName,"<-new.env()")
        eval(parse(text=createEnvStr))
        
        lePath <- stringr::str_replace_all(dum$datapath,"\\\\","/")
        print(lePath)
        loadStr <- paste0("load('",lePath,"',envir=modelEnv$",modName,")")
        eval(parse(text=loadStr))
        
      })
      HTML(paste(lesModsFiles, collaps = '<br/>'))
    })
    #Quand le bouton "Terminer" de la barre de titre est utilisé,
    #on quitte le shinyApp.
    observe(
      if(input$done){
        stopApp()
      })
  }
  runGadget(ui,server,viewer=paneViewer())
  rm(lesModsFiles, envir = .GlobalEnv)
  modelEnv <- as.list(modelEnv)
  
  #Vérifier compatibilité avec mesures----
  #*****************************************************************************
  # On vérifie à partir de la liste des environnements d'instrument si les 
  # données requises seront disponibles.
  # Pour les types de données:
  #   - Fluorescence: EXxxx ou xxx est la longueur d'onde d'excitation donnée
  #                   par F_Inst$lesEXs où F_Inst est un environnement créé par
  #                   InitFluoSpecteuR.
  #   - Autres : substr(Inst$type,1,5) où Inst est un environnement d'instrument 
  #              créé par InitRamanSpecteuR, InitTransmitSpecteuR ou 
  #              InitReflectSpecteuR.
  # On prend pour acquis que le modèle n'est pas appliqué aux données brutes mais
  # aux données interpolées/corrigées.
  # Si après ce bloc, la variable hasData==TRUE, c'est que toutes les données
  # nécessaires seront disponibles pour calculer les modèles pour chaque 
  # échantillon. 
  # Pour chaque élément de la liste des modèles, on ajoute une vecteur de 
  # caratères (character()) dont chaque élément
  #*****************************************************************************
  instDataType <- lapply(lesInstruments, function(inst){
    instType <- inst$type
    if (instType=="Fluorescence"){
      instDataType <- paste0("EX",inst$lesEXs)
      Fluo_EX <- get_DELs_dat("DoEX", mon_envir = inst)
      list(instType=instType,instDataType=instDataType[Fluo_EX])
    }else
    {
      list(instType=instType,instDataType=substr(instType,1,5))
    }
  })
  
  lapply(modelEnv, function(unModele)
  {
    #Note - dataSource est une matrice indiquant où trouver les données nécessaires
    #pour applique unModele. C'est une matrice où chaque ligne représente un des 
    #instruments et où chaque colonne est associée à un type de données nécessaire
    # au modèle. Il faut qu'il y ait au moins un TRUE dans chaque colonne sinon
    # le modèle ne peut pas s'appliquer. Si plusieurs lignes sont identiques, il
    # y a la possibilité d'applique unModele plusieurs fois. On doit créer une telle
    # matrice pour tous les modèles.
    requiredTypes_4_Model <- unModele$model_descript$datatype
    availableTypes_per_inst <- lapply(instDataType,function(idt) idt$instDataType)
    dum <-(lapply(availableTypes_per_inst, stringr::str_detect, pattern=requiredTypes_4_Model))
    dataSource <- matrix(unlist(dum),nrow=length(lesInstruments))
    lesnoms <- lapply(lesInstruments, function(I) I$nomInstrument)
    rownames(dataSource) <- unlist(lesnoms)
    colnames(dataSource) <- requiredTypes_4_Model
    canApplyModel <- all(colSums(dataSource)>0)
    nInstruments <- length(lesInstruments)
    #Calcul des indices pour combiner instruments. Une liste de longueur égale
    #au nombre d'instruments. Le premier élément est pour un instrument simple,
    #le 2ième pour toutes les combinaisons de 2 intruments et ...
    indCom <-  sapply(1:nInstruments,FUN=function(i) combn(1:nInstruments,i))
    #Utiliser indCom pour sélectionner des sous-ensembles de dataSource pour
    #déterminer les combinaisons d'instruments acceptables pour le modèle.
    #instCombi indique (TRUE/FALSE) les combinaisons dans indCom pouvant être
    #utilisée pour appliquer le modèle
    instCombi <- lapply(1:nInstruments, function(indi){
      dum <- as.matrix(indCom[[indi]], nrow=indi)
      nCombi <- ncol(dum)
      ddd <- logical(length=nCombi)
      for (k in 1:nCombi){
        dd <- colSums(matrix(dataSource[dum[,k],],nrow=indi,byrow=F))==1
        ddd[k] <- all(dd)
      }
      return(ddd)
    })
    
    #workingCombi donne une liste de listes d'instruments permettant d'appliquer
    #le modèle.
    nWorkingCombi <- sum(unlist(instCombi))
    workingCombi <- list()
    for (ii in 1:length(instCombi)){
      for (k in 1:length(instCombi[[ii]])){
        dum <- instCombi[[ii]][k]
        if (dum) workingCombi <- c(workingCombi,list(unlist(lesnoms)[indCom[[ii]][,k]]))
      }
    }
    return(workingCombi)
  })
  
  
  #*****************************************************************************
  #get_DELs_dat-----------------------------------------
  #*****************************************************************************
  get_DELs_dat <- function(varname,mon_envir=.GlobalEnv)
    #Fonction pour récupérer des vecteurs/listes de paramètres
  {
    dum <- ls(envir = mon_envir, pattern = paste0("^",varname,".$"))
    dum <- unname(sapply(dum,FUN=function(x) get(x,envir=mon_envir)))
    return(dum)
  }
  #END-----------------------------------------  
}