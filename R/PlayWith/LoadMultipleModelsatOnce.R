testMultipleModelsSelection <- function()
{
  lesPackages <- c("shiny","miniUI","tools")
  dum <- lapply(lesPackages, function(pp){
    ok <- require(pp, character.only = TRUE)
    if (!(ok)){
      install.packages(pp,dependencies = T, character.only = TRUE)
    } 
  })
  
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
                multiple = T,
                buttonLabel = "Choisir des fichiers de modèles",
                width = "600px"),
      hr(),
      h3("Liste des modèles sélectionnés"),
      htmlOutput("mod1")
    )
  )
  server = function(input,output,session){
    
    #permettre des fichiers de 300 MB et moins
    options(shiny.maxRequestSize=300*1024^2) 
    
    output$titre = renderText("<b>Nom des modèles")
    #A chaque fois que l'utilisateur appuie sur le bouton "Ajouter un champ",
    #  on crée un "textInput" pour entrer le nom du nouveau champ
    
    output$mod1 <- renderText({
      dum <- input$mods
      ifelse(is.null(dum),"Aucun modèle",
                  {
                    for (k in 1:nrow(dum)){
                      lesModsFiles <<- c(lesModsFiles,dum$name[k])
                      modName <- tools::file_path_sans_ext(dum$name[k])
                      createEnvStr <- paste0("modelEnv$",modName,"<-new.env()")
                      eval(parse(text=createEnvStr))
                      
                      lePath <- stringr::str_replace_all(dum$datapath,"\\\\","/")
                      loadStr <- paste0("load('",lePath,"',envir=modelEnv$",modName,")")
                      eval(parse(text=loadStr))
                    }
                    HTML(paste(lesModsFiles, collaps = '<br/>'))
                  }
             )
     
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
}
