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
      list(instType,substr(instType,1,5))
    }
  })
  
  hasData <- TRUE
  for (unModele in modelEnv){
    unModele$instruments <- character()
    modDataType <- unModele$model_descript$datatype
    for (mod in modDataType){
      anyHasData <- FALSE
      for (inst in instDataType){
        anyHasData <- anyHasData | any(mod==inst$instDataType)
        if (hasData)
          unModele$instruments <- c(unModele$instruments,inst$instType)
      }
      hasData <- hasData & anyHasData
    }
  }
  
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