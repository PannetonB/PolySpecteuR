#Utilise uiOutput et renderUI pour la mise en page.
lignebase <- 10*sin(1:100*2*pi/100)
e1t1 <- matrix(c(1:100,lignebase+rnorm(100), lignebase+2*rnorm(100)),nrow=3, byrow=T)
e1t2 <- matrix(c(1:100,smooth::sma(lignebase+rnorm(100))$fitted, smooth::sma(lignebase+2*rnorm(100))$fitted),nrow=3, byrow=T)
e2t1 <- matrix(c(1:100,lignebase+rchisq(100,3), lignebase+2*rchisq(100,3)),nrow=3, byrow=T)
e2t2 <- matrix(c(1:100,smooth::sma(lignebase+rchisq(100,3))$fitted, smooth::sma(lignebase+2*rchisq(100,3))$fitted),nrow=3, byrow=T)
e3t1 <- matrix(c(1:100,lignebase+rcauchy(100), lignebase+2*rcauchy(100)),nrow=3, byrow=T)
e3t2 <- matrix(c(1:100,smooth::sma(lignebase+rcauchy(100))$fitted, smooth::sma(lignebase+2*rcauchy(100))$fitted),nrow=3, byrow=T)
T1 <- list(EX1=e1t1,EX2=e2t1,EX3=e3t1)
T2 <- list(EX1=e1t2,EX2=e2t2,EX3=e3t2)
Sps <- list(Brut=T1,Lisse=T2)

Plots_2_Shiny_Multi <- function(Sps){
  library(shiny)
  library(miniUI)
  #Stocke les paramètres graphiques par défaut.
  op <- par(no.readonly = TRUE)
  logo <- png::readPNG("PolySpecteuR_Logo.png")
  
  topLevelNames <- names(Sps)
  secondLevelNames <- lapply(Sps,names)[[1]]
  #Pour enlever les accents
  topLevelNames <- iconv(topLevelNames, to='ASCII//TRANSLIT') 
  secondLevelNames <- iconv(secondLevelNames, to='ASCII//TRANSLIT') 
  
  topN <- length(topLevelNames)
  secN <- length(secondLevelNames)
  plotRows <- ceiling(secN/3)
  plotColumns <- ifelse(secN==1,1,3)
  repN <- nrow(Sps[[1]][[1]])-1
  plotYRanges <- lapply(Sps,function(s) lapply(s,function(s2) range(s2[-1,])))
  
  ui <- miniPage(
    
    gadgetTitleBar(paste0(leType, " - ",as.character(nSPectres)," répétition(s)."), 
                   left = miniTitleBarCancelButton("cancel","Rejeter"),
                   right = miniTitleBarButton("done", "Accepter", primary = TRUE)),
    
    miniContentPanel(
      padding=0,
      plotOutput("logo", height="150px"),
      plotOutput("leplot")
    ),
    
    miniButtonBlock(
      lapply(1:topN, function(b) actionButton(topLevelNames[b],topLevelNames[b]))
    )
    
  )
  
  server = function(input, output, session)
  {
    
    output$logo <- renderPlot({
      par(mai=c(0,0,0,0))
      plot(as.raster(logo))
      par(mai=op$mai*0.35)
    })
    
    output$leplot <- renderPlot({
      i=1
      par(mfrow=c(plotRows,plotColumns))
      for (j in 1:secN){
        if (repN==1){
          plot(Sps[[i]][[j]][1,],Sps[[i]][[j]][2,], 
               type="l",col=2, lwd=2,
               ylim = plotYRanges[[i]][[j]])
        }else
        {
          plot(Sps[[i]][[j]][1,],Sps[[i]][[j]][2,], 
               type="l",col=2,lty=2,
               ylim = plotYRanges[[i]][[j]])
          for (k in 2:repN)
            lines(Sps[[i]][[j]][1,],Sps[[i]][[j]][(k+1),], 
                  type="l",col=k+1, lty=2)
          lines(Sps[[i]][[j]][1,],colMeans(Sps[[i]][[j]][-1,]), 
                type="l",col=1, lty=1, lwd=2)
          title(main = paste(topLevelNames[i],"-",secondLevelNames[j]))
        }
      }
    })
    
    lapply(1:topN, function(i){
      observeEvent(input[[topLevelNames[i]]],{
        output$leplot <- renderPlot({
          par(mfrow=c(plotRows,plotColumns))
          for (j in 1:secN){
            if (repN==1){
              plot(Sps[[i]][[j]][1,],Sps[[i]][[j]][2,], 
                   type="l",col=2, lwd=2,
                   ylim = plotYRanges[[i]][[j]])
            }else
            {
              plot(Sps[[i]][[j]][1,],Sps[[i]][[j]][2,], 
                   type="l",col=2,lty=2,
                   ylim = plotYRanges[[i]][[j]])
              for (k in 2:repN)
                lines(Sps[[i]][[j]][1,],Sps[[i]][[j]][(k+1),], 
                      type="l",col=k+1, lty=2)
              lines(Sps[[i]][[j]][1,],colMeans(Sps[[i]][[j]][-1,]), 
                    type="l",col=1, lty=1, lwd=2)
              title(main = paste(topLevelNames[i],"-",secondLevelNames[j]))
            }
          }
        })
      })
    })
    
    #Message de sortie
    observeEvent(input$done,{
      stopApp("OK")
    })
    observeEvent(input$cancel,{
      stopApp("REJET")
    })
    
  }
  
  
  runGadget(ui,server,viewer=paneViewer())
}
