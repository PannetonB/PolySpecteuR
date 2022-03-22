PickFromPlan <- function(plan, monDelai = 1000)
  #****************************************************************************
  # Fonction qui offre 2 options:
  #   1 : si la liste provient d'un fichier de plan d'expérience, présente la 
  #       liste pour offrir un choix à l'utilisateur.
  #   2 : si la liste correspond à une entrée manuelle, l'opérateur doit entrer
  #       manuellement les valeurs des descripteurs à l'aide d'un menu.
  #****************************************************************************
  #ENTREES:
  #      plan   :   environnement construit par le script GetPlanExp.
  #                 Permet de passer par référence pour modifier les variables
  #                 de l'environnement dans ce script.
  #  monDelai   :   temps en msec avant de mettre à jour EchID avec l'extension
  #                 _bis quand l'EchID entré par l'utilisateur a déjà été 
  #                 utilisé.
  #SORTIE:
  #   Environnement plan modifié:
  #     selected : numéro de la ligne dans le plan si option 1 plus haut.
  #                Si is_empty(selected) est TRUE, pas de choix.
  #        EchID : l'identificateur unique d'échantillon.
  #    
  #****************************************************************************
  # AUTEUR: Bernard Panneton, Agriculture et Agroalimentaire Canada
  # Février 2022
  #****************************************************************************
{

  
  #Make sure required packages are available----
  lesPackages <- c("editData","rChoiceDialogs","rlang")
  lapply(lesPackages, function(pp){
    ok <- require(pp, character.only = TRUE)
    if (!(ok)){
      install.packages(pp,dependencies = T, character.only = TRUE)
    } 
  })
  
  #Fonction pour un formulaire d'entree de descripteur----
  fill_me <- function(plan, monDelai) {
      require(shiny)
      require(miniUI)
      require(DT)

      mylist <- plan$liste_ids
      leplan <- plan$leplan
      choix <- list()
      if (ncol(leplan)>0) for (k in 2:ncol(leplan)) choix[[k]] <- unique(leplan[[k]])
      

      #  └ Interface shiny----
      ui = miniPage(
        gadgetTitleBar("Entrer les descripteurs d'échantillon",
                       right = miniTitleBarButton("done", "Terminer", primary = TRUE)),

        miniContentPanel(
          #Boîte pour entre EchID
          textInput(mylist[1], label = mylist[1], value = ""),
          #Première boîte de texte car aucun descripteur déjà entré.
          if (length(choix)==0){
            lapply(2:length(mylist),
                   function(i)
                     textInput(mylist[i], label = mylist[i]))
          }else
          #Quand des descripteurs ont déjà été entrés, menus déroulants  
          #avec option pour permettre de créé une nouvelle entrée  
          {
            lapply(2:length(mylist), function(i) selectizeInput(mylist[i], label = mylist[i],
                                                                choix[[i]],
                                                                options = list(create = TRUE)))
          }
          
          #Affichage des 5 dernières entrées.
          , htmlOutput("TitreTable"),
          
          tableOutput("mytable")
          
          )
      )

      server = function(input, output, session) {
        # output$TitreTable = renderText(
        #   HTML(paste0('<div style="background:white;">',
        #               "<br>DERNIÈRES 5 ENTRÉES","</div>")))

        output$TitreTable = renderText("<b>DERNIÈRES 5 ENTRÉES")
          
        
        output$mytable =  renderTable(tail(leplan,5), striped = T, bordered = T, hover = T)
        
        curEchID <- reactive(input$EchID)
        curEchID_d <- debounce(curEchID,millis=monDelai)
        
        observeEvent(curEchID_d(), {
          # curEchID <- input$EchID
          if (any(leplan$EchID==curEchID_d()))
            updateTextInput(session, mylist[1], 
                            value = paste0(curEchID_d(),"_bis"))
        })
          
        
        
        observe({
          if(input$done){
            isolate(curEchID <- input$EchID)
            while(any(leplan$EchID==curEchID)) 
              curEchID <- paste0(curEchID,"_bis")
            outlist <- list(EchID = curEchID)
            for (elem in mylist[-1]) outlist[[elem]] <- isolate(input[[elem]])
            stopApp(outlist)}
          if(input$cancel){
            stopApp(NULL)
          }
        })
      }

      runGadget(ui,server,viewer=paneViewer())
  }
  
  if (plan$leType=="Manuel"){
  #Entrée manuelle----
    descripteurs <- fill_me(plan,monDelai)
    if (!is.null(descripteurs)){
      descripteurs <- descripteurs[names(descripteurs) != "cancel"]
      descripteurs <- descripteurs[names(descripteurs) != "done"]
      #print(descripteurs)
      plan$selected <- plan$selected+1
      plan$EchID <- descripteurs$EchID
      if (rlang::is_empty(plan$leplan)){
        names <- names(descripteurs)
        plan$leplan <- data.frame()
        for (k in names) plan$leplan[[k]] <- as.character()
      }
      plan$leplan <- rbind(plan$leplan,as.data.frame(descripteurs))
      return("OK")
    }else
    {
      return(NULL)
    }
    
    
  }else
  {
  #Choix dans une liste----  
    nrow <- nrow(plan$leplan)
    if (plan$selected<nrow){
      ydata=tcltk::tk_select.list(plan$liste_ids,
                                  title="Choisir le prochain échantillon",
                                  preselect = plan$liste_ids[plan$selected+1])
    }else
    {
      ydata=tcltk::tk_select.list(plan$liste_ids,
                                  title="Choisir le prochain échantillon",
                                  preselect =  plan$liste_ids[plan$selected])
    }
    if (!(ydata=="")){
      plan$selected=which(ydata==plan$liste_ids)
      plan$EchID=plan$leplan[plan$selected,1]
      return("OK")
    }else
    {
      return(NULL)
    }
  }
  #RETURN - END-----
  
}