#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)

source(here::here("R","CalibShiny","OOInterface.R"))
# cat("Start_OO\n")
OOobj <<- Start_OO()
spectroIDs <<- paste0(OOobj$lesspectros,"-",OOobj$serial_no)
#Objet pour permettre d'accéder aux coefficients du spectro  
ccoef <<- .jnew("com.oceanoptics.omnidriver.spectrometer.Coefficients")



# Define server logic required to draw a histogram
function(input, output, session) {
 
  shinyjs::hide("loadNewCoeff")
  shinyjs::hide("calib")
  
  updateSelectInput(session,"lespectro",choices=spectroIDs,selected = spectroIDs[1])
  
  dumTable <- data.frame(Coefficients=c("Ordonnée à l'origine","Linéaire","Quadratique","Cubique"),
                         Spectro=c(NA,NA,NA,NA),
                         Étalonnage=c(NA,NA,NA,NA))
    
  values <- reactiveValues(spectroNo=1, 
                           sp=NULL, 
                           wlCoeffs=NULL,
                           coefTable=dumTable,
                           picPlots=NULL,
                           calibPlot=NULL,
                           regPlot=NULL)
  
  
  
  output$table <- DT::renderDataTable(values$coefTable,
                                  options = list(
                                    searching = F,
                                    paging= F
                                  ))
  
  
  # cat("Define default spectro\n")
  lespectro <<- Define_Spectro(OOobj,1L)
  values$spectroNo <- 1
  
  observeEvent(input$lespectro,{
    values$spectroNo <- which(spectroIDs == input$lespectro)
  })


  
  observe({
    values$spectroNo
    isolate({
      if (length(values$spectroNo==1)){
        # cat('Hum\n')
        shinyjs::hide("loadNewCoeff")
        shinyjs::hide("calib")
        lespectro <<- Define_Spectro(OOobj,as.integer(values$spectroNo))
        SetCorrections(OOobj,lespectro,Lin = 1, Dark = 0)
        
        #Pour permettre l'accès
        OOobj$mywrap$insertKey("Mat429sky")
        ccoef<<-OOobj$mywrap$getCalibrationCoefficientsFromEEProm(as.integer(lespectro$number))
        OOobj$mywrap$removeKey()
        values$coefTable[,2] <- format(ccoef$getWlCoefficients(),digits=8)
        Define_Acq_Param (OOobj,
                          lespectro,
                          int_time = input$tint,
                          boxcar = input$boxcar,
                          nscans = input$nscans)
        lespectro <<- Grab_Spectrum(leSpectro = lespectro,OOobj$mywrap)
        values$sp <- lespectro$sp
        
        
      }
    })
  })
  
  observeEvent(input$calibPics,{
    pics_calib <<- pics_calibration[[input$calibPics]]
  })
  
  observeEvent(input$unique,{
    # cat("Acquisition\n")
    
    shinyjs::hide("loadNewCoeff")
    shinyjs::show("calib")
    values$picPlots=NULL
    values$calibPlot=NULL
    values$regPlot=NULL
    Define_Acq_Param (OOobj,
               lespectro,
               int_time = input$tint,
               boxcar = input$boxcar,
               nscans = input$nscans)
    lespectro <<- Grab_Spectrum(leSpectro = lespectro,OOobj$mywrap)
    values$sp <- lespectro$sp
  })
  
  observeEvent(input$calib,{
    # pics_calib <- c(253.652,313.155,334.148,365.0153,404.6563,435.8328,491.607,
    #                 546.0735,696.5431,727.2936,738.398,750.3869,763.5106,
    #                 811.5311,826.4522,842.4648)
    if (!demoPicCalib){
      lmda_old <- lespectro$xaxis
      rawdats <- lespectro$sp
    }else
    {
      load(here::here("R","CalibShiny","DemoData.RData"))
      lmda_old <- as.numeric(lmda_old)
      rawdats <- as.numeric(rawdats)
    }
    
    
    isolate(fenetrePic <- input$fenetreEtal)
    isolate(tol <- input$tol)
    
    #Charge la librairie pracma
    OK <- require(pracma)
    if (!OK){
      install.packages("pracma")
      library(pracma)
    }
    
 
    
    #Trouve les pics
    pics <- findpeaks(as.numeric(rawdats),
                      npeaks = 2*length(pics_calib), 
                      minpeakheight = 0.02*max(rawdats),
                      minpeakdistance = 30)
    
    
    #On enlève les pics trop proche des bords
    offS <- ceiling(fenetrePic/2)
    lowEnd <- 1+offS
    highEnd <- length(lmda_old)-offS
    
    
    pics <- pics[(pics[,2] %in% lowEnd:highEnd),]
    
    #Ajoute 2 colonnes pour stocker la position interpolée du pic et l'intensité  
    #correspondante.
    pics <- cbind(pics,rep(0,nrow(pics)),rep(0,nrow(pics)))
    
    lmda_new <- lmda_old[pics[,2]]
    lmda_new_all <- lmda_new
    
    indi <- numeric()
    for (k in pics_calib){
      dum <- which(abs(k-lmda_new)<tol)
      indi <- c(indi,ifelse(length(dum)>0,dum[1],NA))
    }
    pics_calib <- pics_calib[!is.na(indi)]
    pics_calib <<- pics_calib
    lmda_new <- lmda_new[indi]
    lmda_new <<- lmda_new[!is.na(lmda_new)]
    pics <- pics[indi,]
    pics <- pics[!is.na(pics[,1]),]
    
    
    plot.new()
    par(mfrow=c(1,1))
    plot(lmda_old,rawdats,type="l",
         xlab="Longueur d'onde (approx. valeurs dans le spectro) [nm]",
         ylab="Intensité [U.A.]",
         main="Pics appariés - Étiquette sur fond rose",
         ylim=c(0,1.5*max(rawdats)))
    abline(h=0.02*max(rawdats),col="blue")
    abline(v=lmda_new_all,col="gray50",lty=2)
    abline(v=pics_calib,col="darkgreen")
    for (k in pics_calib) text(k, max(rawdats)*1.2, 
                               paste(rep("\U2588",nchar(as.character(k))), collapse = ""),
                               srt=90, col="pink",cex=0.7)
    for (k in pics_calib) text(k, max(rawdats)*1.2, as.character(k), srt=90, cex=0.7)
    values$sp <- recordPlot()
    dev.off()
    
    
    #Boucle sur les pics pour estimer la position par interpolation quadratique
    # cat("\nLocate peaks\n")
    nplots <- nrow(pics)
    rplot <- ceiling(sqrt(nplots))
    cplot <- 1 + nplots %/% rplot
    x <- 0:(length(rawdats)-1)
    for (k in 1:nrow(pics)){
      #Trouve points les plus près de la valeur maxi
      maxi <- pics[k,1]
      indi <- order(rawdats[pics[k,3]:pics[k,4]]-maxi,decreasing = T)[1:fenetrePic]
      indi <- sort((pics[k,3]:pics[k,4])[indi])

      #Calcule la régression
      lm1 <- lm (rawdats[indi] ~ x[indi] + I(x[indi]^2))
      #Calcule la position du pic et l'intensité correspondante
      cfs <- coef(lm1)
      pics[k,5] <- cfs[2]/-2/cfs[3]
      pics[k,6] <- cfs[1] + cfs[2]*pics[k,5] +cfs[3]*pics[k,5]^2
    }

    par(mfrow=c(rplot,cplot))
    x <- 0:(length(rawdats)-1)
    for (k in 1:nrow(pics)){
      #Fait un graphique pour montrer le résultat
      maxi <- pics[k,1]
      indi <- order(rawdats[pics[k,3]:pics[k,4]]-maxi,decreasing = T)[1:fenetrePic]
      indi <- sort((pics[k,3]:pics[k,4])[indi])
      lm1 <- lm (rawdats[indi] ~ x[indi] + I(x[indi]^2))
      #Calcule la position du pic et l'intensité correspondante
      cfs <- coef(lm1)
      ymax <- 1.02*max(rawdats[indi])
      ymin <- min(rawdats[indi])
      plot(x[indi],rawdats[indi], type = "p", pch=21, col="black", bg="gray",
           ylim = c(ymin,ymax), xlab="Pixel", cex=2, cex.lab=2,
           cex.axis=1.8,ylab="",yaxt="n")
      points(pics[k,5],pics[k,6],pch=21,bg="pink",col="red",cex=2)
      xs <- seq(min(x[indi]),max(x[indi]),0.1)
      ys <- cfs[1] + cfs[2]*xs + cfs[3]*xs^2
      lines(xs,ys,lty=2,col="blue")
    }
    values$picPlots <- recordPlot()
    dev.off()


    # #Calcule la régression cubique pour la longueur d'onde en fonction du no de
    # #pixel.

    x <- pics[,5]
    x2 <- pics[,5]^2
    x3 <- pics[,5]^3
    lm3 <<- lm(pics_calib ~ x + x2 + x3)
    rmse <- sqrt(mean(lm3$residuals^2))
    adjR2 <- summary(lm3)$adj.r.squared

    
    x <- pics[,5]
    x2 <- pics[,5]^2
    x3 <- pics[,5]^3
    lm3 <<- lm(pics_calib ~ x + x2 + x3)
    rmse <- sqrt(mean(lm3$residuals^2))
    adjR2 <- summary(lm3)$adj.r.squared
    par(mfrow=c(1,1))
    ys <- predict(lm3)
    plot(x=pics_calib, y= ys,
         xlab='Position réelle',
         ylab='Position prédite',
         main='Prédiction en fonction de la mesure', cex.lab=1.4)
    abline(a=0, b=1)
    dum <- format(adjR2, digits = 6)
    xt <- min(pics_calib)*1.1
    yt <- max(ys)-30
    text(xt,yt,bquote({R^2} (adj)  ==  .(dum)),cex=1.3, adj=0)
    text(xt,yt-50,paste0("RMSE = ",format(rmse,digits=4)),adj=0,cex=1.3)
    values$regPlot <- recordPlot()
    dev.off()
  

  

    nWl <- length(lmda_old)-1
    lmda_recall <- predict(lm3,newdata = data.frame(x=0:nWl, x2=(0:nWl)^2, x3=(0:nWl)^3))
    MSDiff <- mean((lmda_old-lmda_recall)^2)^0.5
    maxDiff <- max(abs(lmda_old-lmda_recall))
    old_mai <- par("mai")
    par(mai=old_mai+c(0,0.2,0,0))
    plot(0:nWl,(lmda_old-lmda_recall),type="p",pch=".",
         xlab = "Pixel",
         ylab = bquote( lambda[~appareil] - lambda[~étalonnage] ~ "[nm]"),
         cex.lab=1.5, cex.axis=1.3,
         ylim = c(-maxDiff,maxDiff),
         main=bquote("RMS de la différence" == .(format(MSDiff,4))))
    grid()

    par(mai=old_mai)
    isolate(values$coefTable[,3] <- format(coefficients(lm3),digits=8))
    values$calibPlot <- recordPlot()
    dev.off()
    shinyjs::show("loadNewCoeff")
    CalibOutput <<- (list(coefficients=coef(lm3),  #coefficients de la régression (étalonnage)
                          adjR2=adjR2,             #R2 ajusté pour la régression
                          rmseReg=rmse,            #rmse pour la régression
                          lmdas=list(old=lmda_old, new=lmda_recall),  #Longueurs d'onde avant/après
                          MSDiff = MSDiff,         #RMS de la différence des longueurs d'onde avant-après
                          tableau=pics))           #tableau pour les pics retenus.
  })
  
  
  observeEvent(input$tint,{
    if (!is.null(values$spectroNo)){
      shinyjs::hide("loadNewCoeff")
      shinyjs::hide("calib")
      values$picPlots=NULL
      values$calibPlot=NULL
      values$regPlot=NULL
      # cat("tint\n")
      Define_Acq_Param (OOobj,
                        lespectro,
                        int_time = input$tint,
                        boxcar = input$boxcar,
                        nscans = input$nscans)
      lespectro <<- Grab_Spectrum(leSpectro = lespectro,OOobj$mywrap)
      isolate(values$sp <- lespectro$sp)
    }
  })
  
  observeEvent(input$boxcar ,{
    if (!is.null(values$spectroNo)){
      shinyjs::hide("loadNewCoeff")
      shinyjs::hide("calib")
      values$picPlots=NULL
      values$calibPlot=NULL
      values$regPlot=NULL
      # cat("boxcar\n")
      Define_Acq_Param (OOobj,
                        lespectro,
                        int_time = input$tint,
                        boxcar = input$boxcar,
                        nscans = input$nscans)
      lespectro <<- Grab_Spectrum(leSpectro = lespectro,OOobj$mywrap)
      # cat("grab\n")
      isolate(values$sp <- lespectro$sp)
    }
  })
  
  observeEvent(input$nscans ,{
    if (!is.null(values$spectroNo)){
      shinyjs::hide("loadNewCoeff")
      shinyjs::hide("calib")
      values$picPlots=NULL
      values$calibPlot=NULL
      values$regPlot=NULL
      # cat("nscan\n")
      Define_Acq_Param (OOobj,
                        lespectro,
                        int_time = input$tint,
                        boxcar = input$boxcar,
                        nscans = input$nscans)
      lespectro <<- Grab_Spectrum(leSpectro = lespectro,OOobj$mywrap)
      isolate(values$sp <- lespectro$sp)
    }
  })
  
  observeEvent(input$loadNewCoeff,{
    shinyjs::hide("loadNewCoeff")
    shinyjs::hide("calib")
    # cat("loadNew\n")
    values$sp <- NULL
    values$picPlot <- NULL
    values$calib <- NULL
    values$regPlot <- NULL
    
    shinyjs::alert(paste0("Pas encore mis en oeuvre!\n",
                          "Les résultats se trouvent dans l'environnment\n",
                          "global de R dans la liste CalibOutput!")
    )
  })
  
   
  output$spPlot <- renderPlot({
    
      # cat("class(values$sp) : ",class(values$sp),"\n")
      if (toupper(class(values$sp)) != "RECORDEDPLOT"){
        if ("sp" %in% names(lespectro)){
         # cat("Plot spectrum\n") 
         Plot_Spectrum(lespectro,lequel = "brut")
        }
      }else
      {   #cat("plot spectrum with peaks ID\n")
          values$sp
      }
    
  })
  
  output$picPlot <- renderPlot({
    if (is.null(values$picPlots)){
      plot.new()
    }else 
      values$picPlots
  })
  
  output$regPlot <- renderPlot({
    values$regPlot
  })
  
  output$calib <- renderPlot({
    values$calibPlot
  })
  
  session$onSessionEnded(function() {
    OOobj$mywrap$closeAllSpectrometers()
    Quit_OO(OOobj)
    stopApp()
  })
  
 

}
