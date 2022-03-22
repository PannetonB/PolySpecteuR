Define_Descrip <- function()
{
  library(shiny)
  library(miniUI)
  
  ui <- miniPage(
    gadgetTitleBar("Entrer les descripteurs d'échantillon",
                   right = miniTitleBarButton("done", "Terminer", primary = TRUE)),
    
    miniContentPanel(
      
      htmlOutput("titre"),
      actionButton("ajout", "Ajouter un champ"),
      textInput("champ1","EchID","EchID")
    )
    
  )
  
  server = function(input,output,session){
    output$titre = renderText("<b>DESCRIPTEURS DE CHAMP")
    
    observeEvent(input$ajout, 
      {
        insertUI( selector = "#ajout",
                  where = "afterEnd",
                  ui = textInput(paste0("champ", input$ajout+1),
                                 paste0("Champ ", input$ajout+1))
        )
      }
    )
  
    
    observe(
      if(input$done){
        outlist <- reactiveValuesToList(input)
        outlist$done <- NULL
        outlist$cancel <- NULL
        outlist$ajout <- NULL
        outlist <- outlist[sort(names(outlist))]
        stopApp(outlist)
      })
  }
  
  
  
  runGadget(ui,server,viewer=paneViewer())
}