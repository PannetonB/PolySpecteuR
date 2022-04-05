Plots_2_Shiny_MultiLevels <- function(Inst)
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
#                 comme DoRamanSpecteuR par exemple. Spectres peut être une 
#                 liste de liste de matrices de spectres. Dans ce cas, la liste
#                 de premier niveau correspond à des types de spectres (e.g.
#                 brut, lissé, ...) et tous les spectres pour un élément de
#                 premier niveau sont affichés ensemble avec un graphique pour
#                 chaque élément du deuxième niveau de la liste. L'exemple
#                 plus bas illustre l'utilisation d'une liste à 2 niveaux.
#                 
#
# SORTIE
#     "OK" si l'utilisateur a "Accepter" les données illustrées
#     "REJET" si l'utilisateur a "Rejeter" les données illustrées
#
# EXEMPLE
# Pour générer un jeu de données pour tester:
# lignebase <- 10*sin(1:100*2*pi/100)
# e1t1 <- matrix(c(1:100,lignebase+rnorm(100), lignebase+2*rnorm(100)),nrow=3, byrow=T)
# e1t2 <- matrix(c(1:100,smooth::sma(lignebase+rnorm(100))$fitted, smooth::sma(lignebase+2*rnorm(100))$fitted),nrow=3, byrow=T)
# e2t1 <- matrix(c(1:100,lignebase+rchisq(100,3), lignebase+2*rchisq(100,3)),nrow=3, byrow=T)
# e2t2 <- matrix(c(1:100,smooth::sma(lignebase+rchisq(100,3))$fitted, smooth::sma(lignebase+2*rchisq(100,3))$fitted),nrow=3, byrow=T)
# e3t1 <- matrix(c(1:100,lignebase+rcauchy(100), lignebase+2*rcauchy(100)),nrow=3, byrow=T)
# e3t2 <- matrix(c(1:100,smooth::sma(lignebase+rcauchy(100))$fitted, smooth::sma(lignebase+2*rcauchy(100))$fitted),nrow=3, byrow=T)
# T1 <- list(EX1=e1t1,EX2=e2t1,EX3=e3t1)
# T2 <- list(EX1=e1t2,EX2=e2t2,EX3=e3t2)
# Sps <- list(Brut=T1,Lisse=T2)
# Inst <- new.env()
# Inst$type <- "Type de Spectre"
# Inst$Spectres <- Sps
# Plots_2_Shiny_MultiLevels(Inst)
#*******************************************************************************
# AUTEUR: Bernard Panneton, Agriculture et Agroalimentaire Canada
# Fevrier 2022
#*******************************************************************************
#*******************************************************************************  
{
    library(shiny)
    library(miniUI)
    library(keys)
    
    #Stocke les paramètres graphiques par défaut.
    op <- par(no.readonly = TRUE)
    #Pour afficher le logo
    logo <- png::readPNG("PolySpecteuR_Logo.png")
  
    #Récupère des infos de l'environnement Inst----
    leType <- Inst$type
    Sps <- Inst$Spectres  #liste des graphiques à réaliser
    
    #Détermine les niveaux et les noms correspondant----
    topLevelNames <- names(Sps)
    secondLevelNames <- lapply(Sps,names)[[1]]
    
    #Ajuster pour les niveaux de liste----
    if (is.null(secondLevelNames)){  #  └ un niveau----
      secondLevelNames <- topLevelNames
      #Pour enlever les accents
      secondLevelNames <- iconv(secondLevelNames, to='ASCII//TRANSLIT') 
      nbNiveau <- 1
    }else         
    {                                    #└plusieurs niveaux----
      #Pour enlever les accents
      topLevelNames <- iconv(topLevelNames, to='ASCII//TRANSLIT') 
      secondLevelNames <- iconv(secondLevelNames, to='ASCII//TRANSLIT') 
      nbNiveau <- 2
    }
    
    if (nbNiveau==1){   #on simule 2 niveaux pour garder le code plus simple
      Sps <- list(" "=Sps)
      topN <- 1
    }else
    {
      topN <- length(topLevelNames)
    }
    
    repN <- nrow(Sps[[1]][[1]])-1
    secN <- length(secondLevelNames)
    
    # Paramètres pour graphiques----
    plotXRanges <- lapply(Sps,function(s) lapply(s,function(s2) range(s2[1,])))
    plotYRanges <- lapply(Sps,function(s) lapply(s,function(s2) range(s2[-1,])))
    #Cas spécial pour la fluorescence pour enlever le Rayleigh
    if (leType=="Fluorescence"){
      for (secondNames in secondLevelNames){
        #Ajoute 25 nm au plus bas pour enlever Rayleigh
        lambdaEx <- as.numeric(substring(secondNames,3))
        for (k in 1:topN){
          plotXRanges[[k]][[secondNames]][1] <- lambdaEx+25
        }
      }
     
    }
    
    plotRows <- ceiling(secN/3)
    plotColumns <- ifelse(secN==1,1,3)
    
    #Interface Shiny----
    ui <- miniPage(
      useKeys(),
      keysInput("keys", "enter"),
      gadgetTitleBar(paste0(leType, " - ",as.character(repN)," répétition(s)."), 
                     left = miniTitleBarCancelButton("cancel","Rejeter"),
                     right = miniTitleBarButton("done", "Accepter", primary = TRUE)),
      
      # └ Affichage du logo et des graphiques----
      miniContentPanel(
        padding=0,
        plotOutput("logo", height="150px"),
        plotOutput("leplot", height=200*plotRows)
      ),
      
      # └ Bloc de boutons pour choisir dans le premier niveau----
      if (topN!=1){
        miniButtonBlock(
          lapply(1:topN, function(b) actionButton(topLevelNames[b],topLevelNames[b]))
        )
      }
      
    )
    
    #Serveur Shiny-----
    server = function(input, output, session)
    {
      
      
      # └ Affichage du logo----
      output$logo <- renderPlot({
        par(mai=c(0,0,0,0))
        plot(as.raster(logo))
        par(mai=op$mai*0.35)
      })
      
      # └ Affichage graphiques niveau 1 par défaut----
      output$leplot <- renderPlot({
        #indice de niveau 1 correspondant à Corrigé
        if (topN!=1){
          i=which(stringr::str_detect(topLevelNames,"orrig"))
        }else
        {
          i=1
        }
        
        par(mfrow=c(plotRows,plotColumns))
        
        for (j in 1:secN){                              #indice de niveau 2
          #Cas spécial pour la fluorescence pour enlever le Rayleigh
          if (leType=="Fluorescence"){
            
          }else
          {
            xlimits <- range(Sps[[i]][[j]][1,])
          }
          if (repN==1){
            plot(Sps[[i]][[j]][1,],Sps[[i]][[j]][2,], 
                 type="l",col=2, lwd=2,
                 xlim = plotXRanges[[i]][[j]],
                 ylim = plotYRanges[[i]][[j]],
                 ylab= Inst$type,
                 xlab= Inst$lespectro$xunit,
                 cex.lab=1.5, cex.axis=1.3)
          }else
          {
            plot(Sps[[i]][[j]][1,],Sps[[i]][[j]][2,], 
                 type="l",col=2,lty=2,
                 xlim = plotXRanges[[i]][[j]],
                 ylim = plotYRanges[[i]][[j]],
                 ylab= Inst$type,
                 xlab= Inst$lespectro$xunit,
                 cex.lab=1.5, cex.axis=1.3)
            for (k in 2:repN)                                 #indice des reps.
              lines(Sps[[i]][[j]][1,],Sps[[i]][[j]][(k+1),], 
                    type="l",col=k+1, lty=2)
            lines(Sps[[i]][[j]][1,],colMeans(Sps[[i]][[j]][-1,]), 
                  type="l",col=1, lty=1, lwd=2)
          }
          if (nbNiveau>1){
            title(main = paste(topLevelNames[i],"-",secondLevelNames[j]))
          }else
          {
            title(main = secondLevelNames[j])
          }
        }
      })
      
      
      # └ Création des handlers associés aux boutons----
      lapply(1:topN, function(i){
        observeEvent(input[[topLevelNames[i]]],{
          output$leplot <- renderPlot({
            par(mfrow=c(plotRows,plotColumns))
            for (j in 1:secN){
              if (repN==1){
                plot(Sps[[i]][[j]][1,],Sps[[i]][[j]][2,], 
                     type="l",col=2, lwd=2,
                     xlim = plotXRanges[[i]][[j]],
                     ylim = plotYRanges[[i]][[j]],
                     ylab= Inst$type,
                     xlab= Inst$lespectro$xunit,
                     cex.lab=1.5, cex.axis=1.3)
              }else
              {
                plot(Sps[[i]][[j]][1,],Sps[[i]][[j]][2,], 
                     type="l",col=2,lty=2,
                     xlim = plotXRanges[[i]][[j]],
                     ylim = plotYRanges[[i]][[j]],
                     ylab= Inst$type,
                     xlab= Inst$lespectro$xunit,
                     cex.lab=1.5, cex.axis=1.3)
                for (k in 2:repN)
                  lines(Sps[[i]][[j]][1,],Sps[[i]][[j]][(k+1),], 
                        type="l",col=k+1, lty=2)
                lines(Sps[[i]][[j]][1,],colMeans(Sps[[i]][[j]][-1,]), 
                      type="l",col=1, lty=1, lwd=2)
              }
              
              if (nbNiveau>1){
                title(main = paste(topLevelNames[i],"-",secondLevelNames[j]))
              }else
              {
                title(main = secondLevelNames[j])
              }
            }
          })
        })
      })
      
      
      # └ Message de sortie----
      observeEvent(input$keys,{
        stopApp("OK")
      })
      observeEvent(input$done,{
        stopApp("OK")
      })
      observeEvent(input$cancel,{
        stopApp("REJET")
      })
      
    }
    
    #Lancement du gadget----
    myviewer <- paneViewer()
    #myviewer <- dialogViewer("Graphiques",height = 900,width = 800)
    runGadget(ui,server,viewer=myviewer,stopOnCancel = F)
}  



