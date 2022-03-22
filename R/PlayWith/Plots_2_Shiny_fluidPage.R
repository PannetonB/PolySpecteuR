Plots_2_Shiny <- function(Inst)
  
{
    library(shiny)
    library(shinyjs)
  
    jscode <- "shinyjs.closeWindow = function() { window.close(); }"
  
  
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
    ui <- fluidPage(
      useShinyjs(),
      extendShinyjs(text = jscode, functions = c("closeWindow")),
      
      titlePanel(h3(paste0(leType, " - ",as.character(nSPectres)," répétition(s)."))),
      h5("Graphiques des spectres - Accepter ou rejeter les données"),
      
      fluidRow(
        
        
        column(6,
          actionButton("OK","Accepte les données"),
          actionButton("REJET","Rejette les données"),
          height="100px"
        ),
        column(3, offset=3, plotOutput("logo", height="100px", width="180px"), height="100px")
      ),
      uiOutput("out1")
    )
    
    server = function(input, output, session)
    {
      
     
      output$logo <- renderImage({
        # A temp file to save the output. It will be deleted after renderImage
        # sends it, because deleteFile=TRUE.
        outfile <- tempfile(fileext='.png')
        
        # Generate a png
        png(outfile, width=288, height=144)
        op <- par("mar")
        par(mar=rep(0.3,4))
        plot(as.raster(logo))
        par(mar=op)
        dev.off()
        
        # Return a list
        list(src = outfile,
             alt = "This is alternate text",
             width="180px", height="100px")
      }, deleteFile = TRUE)
      
      output$out1 <- renderUI({
        
        ui_parts <- c()
        
        lapply(1:nPlots, function(p) {
          output[[lesNomsID[p]]] <- renderPlot({
            opmgp <- par("mgp")
            opmar <- par("mar")
            par(mgp=c(2,0.5,0))
            par(mar=0.7*opmar)
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
            par(mgp=opmgp)
            par(opmar)
            }, height = 300)})
        
        for(i in 1:plotRows){
          if (is.na(plotMat[i,2])){
            ui_parts[[i]] <- fluidRow(
              column(4, 
                     plotOutput(lesNomsID[[(i-1)*3+1]]), 
                     style = "height:300px;"
                     )
            )
          }else
          {if (is.na(plotMat[i,3])){
            ui_parts[[i]] <- fluidRow(
              column(4, 
                     plotOutput(lesNomsID[[(i-1)*3+1]]), 
                     style = "height:300px;"
              ),
              column(4, 
                     plotOutput(lesNomsID[[(i-1)*3+2]]), 
                     style = "height:300px;")
            )   
          }else
          {
            ui_parts[[i]] <- fluidRow(
              column(4, 
                     plotOutput(lesNomsID[[(i-1)*3+1]]), 
                     style = "height:300px;"
                     ),
              column(4, 
                     plotOutput(lesNomsID[[(i-1)*3+2]]), 
                     style = "height:300px;"),
              column(4, 
                     plotOutput(lesNomsID[[(i-1)*3+3]]), 
                     style = "height:300px;")
            )
          }}
        }
      
      
      
         ui_parts
        
      })
      
      observe({
        if(input$OK){
          js$closeWindow()
          stopApp("OK")}
        if(input$REJET){
          js$closeWindow()
          stopApp("REJET")}
      })

    }
    
    myviewer <- dialogViewer("Accepter ou rejeter", width = 1100, height =850)
    runGadget(ui,server,viewer=myviewer)
}  



