Plots_2_Shiny <- function(Inst)
  
{
    library(shiny)
    library(miniUI)

    op <- par(no.readonly = TRUE)
  
    #Fabrique une liste des graphiques à réaliser----
    leType <- Inst$type
    Sps <- Inst$Spectres     #liste des graphiques à réaliser
    nSPectres <- nrow(Sps[[1]])-1
  
    #Extraire info pour faire la mise en page
    logo <- png::readPNG("PolySpecteuR_Logo.png")
    nPlots <- length(Sps)
    
    lesNoms <- names(Sps)
    if (is.null(lesNoms)){
      lesNoms <- paste0("Spectre ",1:nPlots)
    }
    lesNomsID <- iconv(lesNoms, to='ASCII//TRANSLIT')  #Pour enlever les accents
    
    plotRows <- ceiling(nPlots/3)
    plotColumns <- ifelse(nPlots==1,1,3)
    plotMat <- matrix(c(1:nPlots,rep(NA,(plotRows*plotColumns-nPlots))),nrow=plotRows,ncol=plotColumns,byrow=T)
   
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
        
        lapply(1:nPlots, function(p) {
          if (nSPectres==1){
            plot(Sps[[p]][1,],Sps[[p]][2,],main=as.character(lesNoms[p]), 
                 type="l", col=1, lwd=2,
                 xlab="Longueur d'onde [nm]",
                 ylab="Raman [U.A.]")
          }else
          {
            rangeY <- lapply(Sps, function(p) range(p[-1,]))
            plot(Sps[[p]][1,],Sps[[p]][2,],main=as.character(lesNoms[p]), 
                 type="l", col=1, lwd=1, lty=2,
                 xlab="Longueur d'onde [nm]",
                 ylab="Raman [U.A.]",
                 ylim=rangeY[[p]])
            for(i in 2:nSPectres){
              lines(Sps[[p]][1,],Sps[[p]][1+i,],type="l", col=i, lwd=1, lty=2)
            }
          }
        })
      })
      
      observe({
        if(input$done){
          stopApp("OK")}
        if(input$cancel){
          stopApp("REJET")}
      })

    }
    
    
    runGadget(ui,server,viewer=paneViewer())
}  



