Define_Descrip <- function()
#****************************************************************************
# Gadget Shiny pour saisir les noms des descripteurs
#****************************************************************************
#
#****************************************************************************
# AUTEUR: Bernard Panneton, Agriculture et Agroalimentaire Canada
# Février 2022
#****************************************************************************
{
  library(shiny)
  library(miniUI)
  # Interface usager au début. Sera modifié du côté serveur----
  ui <- miniPage(
    
    
    gadgetTitleBar("Entrer les descripteurs d'échantillon",
                   right = miniTitleBarButton("done", "Terminer", primary = TRUE),
                   left = NULL),
    
    
    miniContentPanel(
      
      htmlOutput("titre"),
      actionButton("ajout", "Ajouter un champ"),
      textInput("champ1","EchID","EchID")
    )
    
  )
  
  # Serveur----
  server = function(input,output,session){
    output$titre = renderText("<b>DESCRIPTEURS DE CHAMP")
    
    #A chaque fois que l'utilisateur appuie sur le bouton "Ajouter un champ",
    #  on crée un "textInput" pour entrer le nom du nouveau champ
    observeEvent(input$ajout, 
      {
        insertUI( selector = paste0("#champ", input$ajout),
                  where = "afterEnd",
                  ui = textInput(paste0("champ", input$ajout+1),
                                 paste0("Champ ", input$ajout+1)))
       
      }
    )
    
    
  
    #Quand le bouton "Terminer" de la barre de titre est utilisé,
    #on prépare les données transférées en sortie de la fonction.
    observe(
      if(input$done){
        outlist <- reactiveValuesToList(input)
        outlist$done <- NULL
        outlist$cancel <- NULL
        outlist$ajout <- NULL
        df <- data.frame(Champ=character())
        outlist <- outlist[sort(names(outlist))]
        for (i in 1:length(outlist)) 
          df <- rbind(df,data.frame(Champ=outlist[[i]]))
        stopApp(df)
      })
  }
  
  
  
  runGadget(ui,server,viewer=paneViewer())
}