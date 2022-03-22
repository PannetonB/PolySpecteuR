fill_me <-
  function(plan) {
    require(shiny)
    
    
    # require(rstudioapi)
    # if (rstudioapi::hasFun("viewer")) {
    #   options(shiny.launch.browser =  .rs.invokeShinyPaneViewer)
    # }
    
    
    mylist <- plan$liste_ids
    leplan <- plan$leplan
    choix <- apply(leplan[,-1],2,unique)

    runApp(list(
      ui = basicPage(
        textInput(mylist[1], label = mylist[1], value = ""),
        if (length(choix)==0){
          lapply(2:length(mylist), 
             function(i) 
               textInput(mylist[i], label = mylist[i]))
        }else
        {
          lapply(2:length(mylist), function(i) selectizeInput(mylist[i], label = mylist[i], 
                                                           choix[[i]], options = list(create = TRUE)))
        },
        
        actionButton('ok','OK'),
        tableOutput('table')
      ),
      
      server = function(input, output, session) {
        output$table = renderTable(leplan)
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
          if(input$ok){
            stopApp(reactiveValuesToList(input))}
        })
      }
    ))
  }