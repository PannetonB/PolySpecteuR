InitModels<-function(){
  #*****************************************************************************
  #*****************************************************************************
  # - Programme pour charger les modèles qui seront appliqués aux données.
  # - Les modèles sont développés et stockés par InSpectoR.
  # - On utilise la fonction DoModels pour appliquer les modèles à chaque
  #   fois que de nouvelles données pour un échantillon sont obtenues.
  #*****************************************************************************
  # AUTEUR: Bernard Panneton, Agriculture et Agroalimentaire Canada
  # Février 2022
  #*****************************************************************************
  #*****************************************************************************
  # Charge des librairies additionnelles----
  lesPackages <- c("shiny","miniUI","tools")
  lapply(lesPackages, function(pp){
    ok <- require(pp, character.only = TRUE)
    if (!(ok)){
      install.packages(pp,dependencies = T, character.only = TRUE)
    } 
  })
  
  #Environnement vide avec les modèles----
  modelEnv <<- new.env(parent = .GlobalEnv)
  
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
    #on prépare les données transférées en sortie de la fonction.
    observe(
      if(input$done){
        stopApp()
      })
  }
  
  
  
  runGadget(ui,server,viewer=paneViewer())
  rm(lesModsFiles, envir = .GlobalEnv)
}