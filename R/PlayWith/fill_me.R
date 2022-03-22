#Fonction pour un formulaire d'entree de descripteur----
fill_me <- function(plan) {
  require(shiny)
  require(miniUI)
  require(DT)
  
  mylist <- plan$liste_ids
  leplan <- plan$leplan
  choix <- apply(leplan[,-1],2,unique)
  
  #  └ Interface shiny----
  ui = miniPage(
    gadgetTitleBar("Entrer les descripteurs d'échantillon",
                   right = miniTitleBarButton("done", "Terminer", primary = TRUE)),
    
    miniContentPanel(
      textInput(mylist[1], label = mylist[1], value = ""),
      if (length(choix)==0){
        lapply(2:length(mylist), 
               function(i) 
                 textInput(mylist[i], label = mylist[i]))
      }else
      {
        lapply(2:length(mylist), function(i) selectizeInput(mylist[i], label = mylist[i], 
                                                            choix[i], 
                                                            options = list(create = TRUE)))
      })
  )
  
  server = function(input, output, session) {
    output$result = renderText(number1 * as.numeric(input$number2))
    output$TitreTable = renderText( 
      HTML(paste0('<div style="background:white">',
                  "<br>DERNIÈRES 5 ENTRÉES","</div>")))
    
    
    
    output$mytable =  renderTable(tail(leplan,5), striped = T, bordered = T, hover = T)
    observeEvent(input$EchID, {
      curEchID <- input$EchID
      if (!(curEchID=="")){
        while (any(leplan$EchID==curEchID)){
          curEchID <- paste0(curEchID,"_bis")
        }
      }
      updateTextInput(session, mylist[1], value = curEchID)
    })
    observe({
      if(input$done){
        stopApp(reactiveValuesToList(input))}
    })
  }
  
  runGadget(ui,server,viewer=paneViewer())
}