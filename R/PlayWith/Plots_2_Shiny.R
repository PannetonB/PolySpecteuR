Plots_2_Shiny <- function(Inst)
#*******************************************************************************
#*******************************************************************************
# Programme pour l'affichage des spectres et la validation des données.
# 
# ENTRÉES
#     Inst    : environnement d'instrument créé par InitRamanSpecteuR.
#                 Cet environnement comprend la configuration de l'instrument,
#                 le lien avec le spectro pour OmniDriver. Le wrapper a été 
#                 créé dans l'environnement global de R pour être accessible
#                 par tous les instruments. L'environnement comprend aussi
#                 les paramètres d'acquisition. Il doit y avoir un élément
#                 nommé Spectres qui est une liste contenant les spectres
#                 à afficher. Spectres est créé par les scripts d'acquisition 
#                 comme DoRamanSpecteuR par exemple.
#
# SORTIE
#     "OK" si l'utilisateur a "Accepter" les données illustrées
#     "REJET" si l'utilisateur a "Rejeter" les données illustrées
#*******************************************************************************
# AUTEUR: Bernard Panneton, Agriculture et Agroalimentaire Canada
# Fevrier 2022
#*******************************************************************************
#*******************************************************************************  
{
    library(shiny)
    library(miniUI)
    
    #Stocke les paramètres graphiques par défaut.
    op <- par(no.readonly = TRUE)
  
    #Fabrique une liste des graphiques à réaliser----
    leType <- Inst$type
    Sps <- Inst$Spectres     #liste des graphiques à réaliser
    nSPectres <- nrow(Sps[[1]])-1 #plus d'un spectre si rep. de positions
                                  # d'échantillon
  
    #Extraire info pour faire la mise en page
    logo <- png::readPNG("PolySpecteuR_Logo.png")
    nPlots <- length(Sps)
    
    #Récupère ou crée les noms servant de titre aux graphiques
    lesNoms <- names(Sps)
    if (is.null(lesNoms)){
      lesNoms <- paste0("Spectre ",1:nPlots)
    }
    lesNomsID <- iconv(lesNoms, to='ASCII//TRANSLIT')  #Pour enlever les accents
    
    #Trois graphiques en largeur et autant de rangées que nécessaire
    plotRows <- ceiling(nPlots/3)
    plotColumns <- ifelse(nPlots==1,1,3)
    plotMat <- matrix(c(1:nPlots,rep(NA,(plotRows*plotColumns-nPlots))),
                      nrow=plotRows,ncol=plotColumns,byrow=T)
   
    #Utilise uiOutput et renderUI pour la mise en page.
    ui <- miniPage(
      
      gadgetTitleBar(paste0(leType, " - ",as.character(nSPectres)," répétition(s)."), 
                     left = miniTitleBarCancelButton("cancel","Rejeter"),
                     right = miniTitleBarButton("done", "Accepter", primary = TRUE)),
     
      
      miniContentPanel(
        plotOutput("out1")
      )
    )
    
    server = function(input, output, session)
    {
      
     
      output$out1 <- renderPlot({
        plot.new()
        par(op)
        #Plusieurs graphiques - 2 premiers pour le logo 
        nf=layout(matrix(
                         c(rep(1,plotColumns),2:(plotRows*plotColumns+1)),
                         plotRows+1,plotColumns,byrow=TRUE),
                  heights = c(0.1,rep((0.9/(plotRows+1))),plotRows))
        #Affiche le logo
        par(mai=c(0,0,0,0))
        plot(as.raster(logo))
        par(mai=op$mai*0.35)
        
        lapply(1:nPlots, function(p) {  #boucle sur types de spectres
          if (nSPectres==1){  #pas de répétitions sur la position d'échantillon
            plot(Sps[[p]][1,],Sps[[p]][2,],main=as.character(lesNoms[p]), 
                 type="l", col=1, lwd=2,
                 xlab="Longueur d'onde [nm]",
                 ylab="Raman [U.A.]")
          }else   #répétitions sur la position d'échantillon
          {
            rangeY <- lapply(Sps, function(p) range(p[-1,]))
            #affiche les répétitions en lignes pointillées de couleur
            plot(Sps[[p]][1,],Sps[[p]][2,],main=as.character(lesNoms[p]), 
                 type="l", col=2, lwd=1, lty=2,
                 xlab="Longueur d'onde [nm]",
                 ylab="Raman [U.A.]",
                 ylim=rangeY[[p]])
            for(i in 2:nSPectres){
              lines(Sps[[p]][1,],Sps[[p]][1+i,],type="l", 
                    col=(i+1), lwd=1, lty=2)
            }
            #Affiche le spectre moyen en noir gras, trait continu
            spmoyen <- colmeans(Sps[[p]][-1,])
            lines(Sps[[p]][1,],spmoyen, type="l", col=1, lwd=3,lty=1)
          }
        })
      })
      
      #Message de sortie
      observe({
        if(input$done){
          stopApp("OK")}
        if(input$cancel){
          stopApp("REJET")}
      })

    }
    
    
    runGadget(ui,server,viewer=paneViewer())
}  



