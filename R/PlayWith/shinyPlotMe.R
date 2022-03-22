shinyPlotMe <- function(x,y,titre, letype='l')
{
  library(shiny)
  library(miniUI)
  ui <- miniPage( 
    gadgetTitleBar(titre, 
                   left = NULL,
                   right = miniTitleBarButton("done", "Accepter", primary = TRUE)),
    # └ Affichage du logo et des graphiques----
    miniContentPanel(
      padding=0,
      plotOutput("leplot", height=500)
    ))
  
  
  #Serveur Shiny-----
  server = function(input, output, session)
    {
    output$leplot <- renderPlot({
      plot(x,y, type = letype)
    })
    # └ Message de sortie----
    observeEvent(input$done,{
      stopApp("OK")
    })
  }
  #Lancement du gadget----
  myviewer <- paneViewer()
  #myviewer <- dialogViewer("Graphiques",height = 900,width = 800)
  runGadget(ui,server,viewer=myviewer,stopOnCancel = F)
}