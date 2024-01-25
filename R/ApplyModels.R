ApplyModels <- function(Plan,lesInstruments,modelEnv,dataPath,dataSetID,
                        plotMe=T,debugPlot=F,
                        width=800, height=500)
{

  #Charge librairies et fonctions----
  
  lesPackages <- c("ggplot2","shiny","miniUI",
                   "pls","keys")
  dum <- lapply(lesPackages, function(pp){
    ok <- require(pp, character.only = TRUE)
    if (!(ok)){
      install.packages(pp,dependencies = T, character.only = TRUE)
      library(pp, character.only = TRUE)
    } 
  })
  
  source(file.path(here::here(),"R/Apply_PreTreatments.R"), encoding = 'UTF-8')
  source(file.path(here::here(),"R/playTwoSpectra.R"), encoding = 'UTF-8')
  
  
  #*************************************************************************
  #merge_png_2_pdf
  #*************************************************************************
  merge_png_2_pdf <- function(pdfFile, pngFiles, deletePngFiles=FALSE) {
    
    #### Package Install ####
    pngPackageExists <- require ("png")
    if ( !pngPackageExists ) {
      install.packages ("png")
      library ("png")
      
    }
    
    ok <- require("grid")
    if (!ok) {
      install.packages('grid')
      library("grid")
    }
    
    #########################
    
    pdf(pdfFile,paper= "USr",width=9.5,height = 7)
    
    n <- length(pngFiles)
    
    for( i in 1:n) {
      
      pngFile <- pngFiles[i]
      
      pngRaster <- readPNG(pngFile)
      
      grid.raster(pngRaster, width=unit(0.9, "npc"), 
                  height= unit(0.75, "npc"))
      
      if (i < n) plot.new()
      
    }
    
    dev.off()
    
    if (deletePngFiles) {
      
      unlink(pngFiles)
    }
    
  }
  
  
  #*************************************************************************
  #PlotAll
  #*************************************************************************
  PlotAll <- function(lesplots,echID="") {
    library(keys)
    nPlots <- length(lesplots)
    lesNoms <- names(lesplots)
    modelNames <- lapply(lesplots, function(p) p$alt)
    names(modelNames) <- NULL
    whichPlot <- 1
    ui <- miniPage(
      useKeys(),
      keysInput("keys", "enter"),
      gadgetTitleBar(paste("Résultats des modèles - Échantillon",echID), 
                     left = miniTitleBarButton("export", "Enregistrer"),
                     right = miniTitleBarButton("done", "Terminer", primary = TRUE)),
                     
      column(12, align="left",
             selectInput("modelselect","  Choisir un modèle", unlist(modelNames), width=0.8*width)),
      miniContentPanel(padding = 10,
                       imageOutput("plot", height = "100%")
      )
    )
    
    server <- function(input, output, session) {
      
      # Affichage premier graphique ----
      output$plot <- renderImage({
        whichPlot <- which(input$modelselect==unlist(modelNames))
        lesplots[[whichPlot]]
      }, deleteFile=FALSE)
      
      
      
      observeEvent(input$keys,{
        stopApp()
      })
      
      observeEvent(input$export,{
        # whichPlot <- which(input$modelselect==unlist(modelNames))
        unPath <- utils::choose.files(default = paste0("resMod_EchID_",echID),
                      caption = "Choisir un nom de fichier",
                      multi = F, filters = Filters[c("pdf"),])
        pngFiles <- lapply(lesplots, function(p) p$src)
        pngFiles <- unlist(pngFiles)
        merge_png_2_pdf(unPath, pngFiles, deletePngFiles=FALSE)
        #file.copy(from=lesplots[[whichPlot]]$src,
                  # to=unPath, overwrite=T)
      })
      
      observeEvent(input$done,{
        dum <- lapply(lesplots, function(p) file.remove(p$src))
        stopApp()
      })
    }
    
    runGadget(ui, server, viewer = dialogViewer("",width=width+30,
                                                height=height+150))
  }
  
  #*****************************************************************************
  #get_DELs_dat
  #*****************************************************************************
  get_DELs_dat <- function(varname,mon_envir=.GlobalEnv)
    #Fonction pour récupérer des vecteurs/listes de paramètres
  {
    dum <- ls(envir = mon_envir, pattern = paste0("^",varname,".$"))
    dum <- unname(sapply(dum,FUN=function(x) get(x,envir=mon_envir)))
    return(dum)
  }
  
  
  
  #******************************************************************************
  #Predict_plsda
  #*****************************************************************************
  Predict_plsda <- function(unModele,mydata=NULL,probs=TRUE)
    #Predicts classes or probabilities on mydata. If mydata is NULL, predicts on training
    #If probs is TRUE, will return a table of probabilities.
  {
    plsdaFit <- unModele$plsdaFit
    N_modeles<-length(plsdaFit)
    if (N_modeles>1){  #Need to aggregate results
      ind_apply<-as.list(1:N_modeles)
      Ps <- lapply(ind_apply, function(ii){
        if (is.null(mydata))
          predict(plsdaFit[[ii]]$finalModel, newdata=plsdaFit[[ii]]$trainingData[,-1], type="prob")
        else
          predict(plsdaFit[[ii]]$finalModel, newdata=mydata[[ii]], type="prob")
      })
      Ps<-lapply(Ps,abind::adrop,drop=c(F,F,T))  #remove useless third dimension.
      pooled <- Ps[[1]] * NA 
      n <- nrow(pooled) 
      classes <- colnames(pooled) 
      
      FUNC<- unModele$model_descript$aggregation
      for(i in 1:ncol(pooled)) 
      { 
        tmp <- lapply(Ps, function(y, col) y[,col], col = i) 
        tmp <- do.call("rbind", tmp) 
        pooled[,i] <- apply(tmp, 2, function(x) do.call(FUNC,as.list(x))) 
      } 
      pooled <- t(apply(pooled, 1, function(x) x/sum(x))) #the probs combined should be normalized
      classes <- colnames(pooled) 
      val_pred_cl <- pooled
      if (!probs)
        val_pred_cl <- factor(classes[apply(pooled, 1, which.max)], levels = classes) 
    }else  #only one model - no aggregation.
    {
      if (is.null(mydata)){
        val_pred_cl <- predict(plsdaFit[[1]]$finalModel,newdata=plsdaFit[[1]]$trainingData[,-1]
                               , type="prob")
      }else
      {
        val_pred_cl <- predict(plsdaFit[[1]]$finalModel,newdata=mydata[[1]], type="prob")
      }
      val_pred_cl<-abind::adrop(val_pred_cl,drop=c(F,F,T))  #remove useless third dimension.
      classes<-colnames(val_pred_cl)
      if (!probs)
        val_pred_cl <- factor(classes[apply(val_pred_cl, 1, which.max)], levels = classes) 
    }
    return(val_pred_cl)
  }
  
  
  #*****************************************************************************
  #Nomme les éléments de lesInstruments avec nomInstrument----
  nomsInstrument <- lapply(lesInstruments, function(inst) inst$nomInstrument)
  names(lesInstruments) <- nomsInstrument 
  
  #*****************************************************************************
  #Types de données associées aux instruments----
  instDataType <- lapply(lesInstruments, function(inst){
    instType <- inst$type
    if (instType=="Fluorescence"){
      instDataType <- paste0("EX",inst$lesEXs)
      Fluo_EX <- get_DELs_dat("DoEX", mon_envir = inst)
      list(instType=instType,instDataType=instDataType[Fluo_EX])
    }else
    {
      list(instType=instType,instDataType=substr(instType,1,5))
    }
  })
  names(instDataType) <- names(lesInstruments)
  
  echID <- Plan$EchID
  
  #BOUCLE SUR LES MODÈLES----
  lesplots <- list()
  nModeles <- length(modelEnv)
  for (kmod in 1:nModeles){
    unModele <- modelEnv[[kmod]]
    modelName <- names(modelEnv[kmod])
    
  
    ## Pour chaque combinaison d'instruments----
    
    for (instCombi in unModele$workingInstCombi){
      lesSp <- list()
      preTreatData <- list()
      # Y aller par type de données
      modDTypes <- unModele$model_descript$datatype
      
      if (debugPlot) par(mfrow=c(2,2))
      
      for (dtype in modDTypes){
        #Trouve l'instrument qui supporte le type demandé.
        #On utilise instDataType
        dum <- lapply(lapply(seq_along(instCombi),
                             function(i) instDataType[[instCombi[i]]]$instDataType),
                      function(x) x==dtype)
        whereDType <- lapply(dum,which)
        whichInst <- which(lengths(whereDType)==1)
        if (lesInstruments[[instCombi[[whichInst]]]]$listDepth==2){
          lesSp[[dtype]] <- lesInstruments[[instCombi[[whichInst]]]]$Spectres$'Corrigé'[[dtype]]
        }else
        {
          lesSp[[dtype]] <- lesInstruments[[instCombi[[whichInst]]]]$Spectres$'Corrigé'
        }
        ### Prétraitement----
        whichDType <- which(dtype==modDTypes)
        preTreatData[[dtype]] <- Apply_PreTreatments(unModele$prepro_params,whichDType,lesSp[[dtype]])
        #### Graphiques si debugPlot==TRUE----
        if (debugPlot) {
          plotSp(lesSp[[dtype]],preTreatData[[dtype]])
          title(paste(dtype,"sur",instCombi[whichInst]))
        }
      }
      
      if (debugPlot) par(mfrow=c(1,1))
      ### Prédiction----
      switch(unModele$model_descript$type,
             #### PLS----
             PLS = {
               if (unModele$model_descript$aggregation=="concatenate spectra"){
                 #ATTN : do not work with XData_p but with the selected items.
                 for (dtype in unModele$model_descript$datatype){
                   idtype <- which(dtype==unModele$model_descript$datatype)
                   spdf<-as.data.frame(t(preTreatData[[dtype]][2,]))
                   colnames(spdf)<-paste(dtype,as.character(preTreatData[[dtype]][1,]),sep="_")
                   if (idtype==1){
                     y <- spdf
                   }else
                   {
                     y <- cbind(y,spdf)
                   }
                 }
                 pls_set <- y
               }else  #autres modes d'aggrégation pas supporté par InSpectoR - Avril 2022
               {
                 
               }
               plspreds<-predict(unModele$plsFit[[1]],
                                 newdata = pls_set,
                                 ncomp=unModele$pls_ncomp)
               ##### Sortie fichier----
               leDir <- file.path(dataPath,"resModel")
               if (!dir.exists(leDir)) dir.create(leDir)
               fname <- file.path(leDir,paste0(modelName,"_",paste(instCombi,collapse='_'),".txt"))
               #Création d'une ligne pour écrire dans le fichier.
               ligne=NULL
               #Date et heure pour insérer dans le fichier
               ladate=as.character(Sys.Date())
               letemps=format(Sys.time(),"%X")
               
               ncol=dim(Plan$leplan)[2]
               for (i in 1:ncol){
                 ligne=c(ligne,as.character(Plan$leplan[Plan$selected,i]))
               }
               ligne=c(ligne,ladate,letemps,as.character(signif(plspreds,4)))
               if (file.exists(fname)) {  #append
                 mycon=file(fname,"a")
                 cat(ligne,file=mycon,sep="\t")
                 cat("\n",file=mycon)
                 close(mycon)
               }else
               {
                 entete=c(names(Plan$leplan),"Date","Heure","Prédiction")
                 mycon=file(fname,"a")   
                 cat(entete,file=mycon,sep="\t")   #entête
                 cat("\n",file=mycon)
                 cat(ligne,file=mycon,sep="\t") #données
                 cat("\n",file=mycon)
                 close(mycon)
               }
               
               ##### Graphiques----
               if (plotMe){
                 nggs <- length(lesplots)
                 fPattern <- paste0("Plot",sprintf("%02d", nggs+1))
                 outfile <- tempfile(pattern=fPattern,fileext = '.png')
                 png(outfile, width = width, height = height)
                 
                 nComp <- unModele$pls_ncomp
                 x=unModele$plsFit[[1]]$fitted.values[,,nComp] +  #valeurs du jeud
                   unModele$plsFit[[1]]$residuals[,,nComp]        #d'étalonnage
                 y=predict(unModele$plsFit[[1]],ncomp=nComp)
                 pred <- unlist(plspreds)  #prédiction pour l'échantillon en cours
                 
                 lm.out <- lm(y ~ x)   #Régression de Prédites vs Mesurées
                 
                 leTitre <- paste(modelName," sur ",
                                  paste(instCombi,collapse=' et '),
                                  "- Valeur prédite:",
                                  signif(pred,3),
                                  " - Échantillon",echID)
                 plot(x, y,
                      main=leTitre,
                      type="p",col="black", pch=21, bg="cyan",
                      xlab="Valeur mesurée",
                      ylab="Valeur prédite", 
                      cex=1.5, cex.lab=1.5, cex.main=1.6, cex.axis=1.25)
                 abline(lm.out, col="blue",lwd=2)
                 abline(a=0,b=1,lty=2,lwd=2,col="black")
                 grid()
                 abline(h=pred,col="darkgreen",lwd=3)  #valeur pour l'échantillon
                 legend("bottomright", inset=c(0.01,0.01),
                        legend=c("Données d'étalonnage", "Ligne 1:1",
                                 "Régression","Prédiction"),
                        lty=c(NA,2,1,1), lwd=c(0,2,2,3),
                        pch=c(21,rep(NA,3)), col=c("black","black","blue","darkgreen"),
                        pt.bg = "cyan", pt.cex = 1.5, cex=1.5)
                 R2 <- signif(pls::R2(unModele$plsFit[[1]],
                               ncomp=unModele$plsFit[[1]]$ncomp,
                               intercept=0,
                               estimate="CV")$val,3)
                 RMSECV <- signif(pls::RMSEP(unModele$plsFit[[1]],
                                      ncomp=unModele$plsFit[[1]]$ncomp,
                                      intercept=0,
                                      estimate="adjCV")$val,3)
                 legend("topleft", inset=c(0.02,0.02),
                        legend=c(paste0("R²(V.C) = ",R2),
                                 paste0("RMSECV (adj) = ",RMSECV)),
                        cex=1.5
                        )
                 dev.off()
                 
                 
                 lesplots[[paste0("PLS ",nggs+1)]] <- 
                   list(src = outfile,
                        contentType = 'image/png',
                        width = width,
                        height = height,
                        alt = paste0(modelName," sur ",
                                     paste(instCombi,collapse=' et '))
                   )
               }
               
               
             },
             #### PCA----
             PCA = {
               #Vérifier si le modèle vient de ShInSpectoR
               fromShInSpectoR <- "source" %in% names(unModele$model_descript)
               ###### Modèle de ShInSpectoR ----
               if (fromShInSpectoR){
                 for (dtype in unModele$model_descript$datatype){
                   idtype <- which(dtype==unModele$model_descript$datatype)
                   spdf<-as.data.frame(t(preTreatData[[dtype]][2,]))
                   colnames(spdf)<-paste(dtype,as.character(preTreatData[[dtype]][1,]),sep="_")
                   if (idtype==1){
                     y <- spdf
                   }else
                   {
                     y <- cbind(y,spdf)
                   }
                 }
                 pca_set <- y
                 
                 lesPreds <- predict(unModele$lePCA,pca_set)
                 ####### Graphiques----
                 if (plotMe){
                   nggs <- length(lesplots)
                   fPattern <- paste0("Plot",sprintf("%02d", nggs+1))
                   outfile <- tempfile(pattern=fPattern, fileext = '.png')
                   png(outfile, width = width, height = height)
                   
                   toColor <- unModele$colorby
                   if (is.data.frame(toColor)) toColor <- as.factor(rep("Données",nrow(toColor)))
                   
                   mescols=c("darkred","blue","green3","salmon","yellow3","black","red3","magenta","gray70","cyan") 
                   #To define corresponding lighter transparent colors for symbol fill
                   mescols_fill=col2rgb(mescols,alpha=TRUE)
                   mescols_fill[4,]=145
                   mescols_fill=rgb(mescols_fill[1,],mescols_fill[2,],mescols_fill[3,],alpha=mescols_fill[4,],maxColorValue = 255)
                   recycling=ceiling(length(unique(toColor))/10)  #Only 10 colors defined, so we recycle if necessary
                   mescols_fill=rep(mescols_fill,recycling)
                   
                   par(mfrow=c(2,2))
                   lesScores <- unModele$lePCA$x
                   
                   for (k in seq(1,4,2)){
                     xLimits <- extendrange(c(lesScores[,k],lesPreds[k]))
                     yLimits <- extendrange(c(lesScores[,(k+1)],lesPreds[k+1]))
                     plot(lesScores[,k],lesScores[,(k+1)], pch = 21, cex=2,
                          xlim = xLimits,
                          ylim = yLimits,
                          col = "black", bg=mescols_fill[toColor],
                          xlab=paste0("PC",k), ylab = paste0("PC",(k+1)),
                          cex.lab=1.5, cex.axis=1.5)
                     points(lesPreds[k],lesPreds[k+1]
                            ,col="white", bg="red", cex=3, pch=21)
                     abline(v=0,h=0,col="gray80",lty=3)
                   }
                   #TITRE et LÉGENDE
                   plot.new()
                   classes <- levels(toColor)
                   nCl <- length(classes)
                   legend("bottomright",legend=c(classes,"Prédiction","Limites"),
                          inset=c(0.1,0),
                          col=c(rep("black",nCl),"white","red"),
                          lty = c(rep(0,nCl),0,2),
                          lwd = c(rep(0,nCl),0,2),
                          pt.bg=c(mescols_fill[1:nCl],"red",NA), 
                          pch=c(rep(21,nCl),21,NA), 
                          pt.cex = c(rep(2,nCl),3), cex=1.2, bty="n")
                   
                   text(0,1,paste0(modelName,"\nsur\n",
                                   paste(instCombi,collapse=' et '),
                                   "\n", 
                                   paste(names(preTreatData), collapse = ' et '),
                                   "\n\nÉchantillon: ",echID),
                        adj=c(0,1), cex=1.2, font=2)
                   
                   #ODist vs SDist 
                   #Calcul OD et SD pour l'échantillon
                   #Voir manuel de PLS Toolbox de EigenVector dans Doc du projet
                   scs <- lesPreds[1,1:unModele$NCPs,drop=F]
                   eigVals <- unModele$lePCA$sdev[1:unModele$NCPs] 
                   midMat <- diag(1/eigVals^2)
                   SD <- sqrt(scs %*% midMat %*% t(scs))
                   
                   x <- as.matrix(pca_set-unModele$lePCA$center)
                   lds <- unModele$lePCA$rotation[,1:unModele$NCPs,drop=F]
                   midMat <- diag(nrow=dim(x)[2]) - lds %*% t(lds)
                   OD <- sqrt(x %*% midMat %*% t(x))
                   
                   xLimits <- extendrange(c(unModele$dds$SDist,SD))
                   yLimits <- extendrange(c(unModele$dds$ODist,OD))
                   plot(unModele$dds$SDist,unModele$dds$ODist, pch = 21, cex=1.5,
                        xlim = xLimits,
                        ylim = yLimits,
                        col = "black", bg=mescols_fill[toColor],
                        xlab=toupper("Distance dans le modèle"),
                        ylab = toupper("Distance résiduelle"),
                        cex.lab=1.5, cex.axis=1.5)
                   points(SD,OD,col="white", bg="red", cex=2.5, pch=21)
                   abline(h=unModele$dds$critOD, v=unModele$dds$critSD,
                          col="red",lwd=2,lty=2)
                   
                   dev.off()
                   
                   
                   nggs <- length(lesplots)
                   lesplots[[paste0("ACP ",nggs+1)]] <- 
                     list(src = outfile,
                          contentType = 'image/png',
                          width = width,
                          height = height,
                          alt = paste0(modelName," sur ",
                                       paste(instCombi,collapse=' et '),
                                       " - ",
                                       paste(names(preTreatData), collapse = ' et ')
                          )
                     )
                   
                   par(mfrow=c(1,1))
                   ##### Sortie fichier----
                   leDir <- file.path(dataPath,"resModel")
                   if (!dir.exists(leDir)) dir.create(leDir)
                   fname <- file.path(leDir,paste0(modelName,"_",paste(instCombi,collapse='_'),".txt"))
                   #Création d'une ligne pour écrire dans le fichier.
                   ligne=NULL
                   #Date et heure pour insérer dans le fichier
                   ladate=as.character(Sys.Date())
                   letemps=format(Sys.time(),"%X")
                   
                   ncol=dim(Plan$leplan)[2]
                   for (i in 1:ncol){
                     ligne=c(ligne,as.character(Plan$leplan[Plan$selected,i]))
                   }
                   ligne=c(ligne,ladate,letemps,as.character(signif(lesPreds[1:unModele$NCPs],4)))
                   ligne=c(ligne,signif(c(SD,OD),4))
                   if (file.exists(fname)) {  #append
                     mycon=file(fname,"a")
                     cat(ligne,file=mycon,sep="\t")
                     cat("\n",file=mycon)
                     close(mycon)
                   }else
                   {
                     entete=c(names(Plan$leplan),"Date","Heure",
                              paste0("PC",1:unModele$NCPs),"SD","OD")
                     mycon=file(fname,"a")   
                     cat(entete,file=mycon,sep="\t")   #entête
                     cat("\n",file=mycon)
                     cat(ligne,file=mycon,sep="\t") #données
                     cat("\n",file=mycon)
                     close(mycon)
                   }
                 }
               }else
                 
               #### Modèle de InSpectoR ----  
               {
                 iind=as.list(1:length(preTreatData))
                 acp_pred <- lapply(iind, function(i){
                   newdats <- as.data.frame(t(preTreatData[[i]][2,]))
                   colnames(newdats) <- dimnames(unModele$lesACPs[[i]]$rotation)[[1]]
                   lesPreds <- predict(unModele$lesACPs[[i]],newdats)
                  
                   
                   ####### Graphiques----
                   if (plotMe){
                     nggs <- length(lesplots)
                     fPattern <- paste0("Plot",sprintf("%02d", nggs+1))
                     outfile <- tempfile(pattern=fPattern, fileext = '.png')
                     png(outfile, width = width, height = height)
                     
                     toColor <- unModele$colorby
                     if (is.data.frame(toColor)) toColor <- as.factor(rep("Données",nrow(toColor)))
                     
                     mescols=c("darkred","blue","green3","salmon","yellow3","black","red3","magenta","gray70","cyan") 
                     #To define corresponding lighter transparent colors for symbol fill
                     mescols_fill=col2rgb(mescols,alpha=TRUE)
                     mescols_fill[4,]=145
                     mescols_fill=rgb(mescols_fill[1,],mescols_fill[2,],mescols_fill[3,],alpha=mescols_fill[4,],maxColorValue = 255)
                     recycling=ceiling(length(unique(toColor))/10)  #Only 10 colors defined, so we recycle if necessary
                     mescols_fill=rep(mescols_fill,recycling)
                     
                     par(mfrow=c(2,2))
                     lesScores <- unModele$lesACPs[[i]]$x
                     
                     for (k in seq(1,4,2)){
                       xLimits <- extendrange(c(lesScores[,k],lesPreds[k]))
                       yLimits <- extendrange(c(lesScores[,(k+1)],lesPreds[k+1]))
                       plot(lesScores[,k],lesScores[,(k+1)], pch = 21, cex=2,
                            xlim = xLimits,
                            ylim = yLimits,
                            col = "black", bg=mescols_fill[toColor],
                            xlab=paste0("PC",k), ylab = paste0("PC",(k+1)),
                            cex.lab=1.5, cex.axis=1.5)
                       points(lesPreds[k],lesPreds[k+1]
                              ,col="white", bg="red", cex=3, pch=21)
                       abline(v=0,h=0,col="gray80",lty=3)
                     }
                     #TITRE et LÉGENDE
                     plot.new()
                     classes <- levels(toColor)
                     nCl <- length(classes)
                     legend("bottomright",legend=c(classes,"Prédiction","Limites"),
                            inset=c(0.1,0),
                            col=c(rep("black",nCl),"white","red"),
                            lty = c(rep(0,nCl),0,2),
                            lwd = c(rep(0,nCl),0,2),
                            pt.bg=c(mescols_fill[1:nCl],"red",NA), 
                            pch=c(rep(21,nCl),21,NA), 
                            pt.cex = c(rep(2,nCl),3), cex=1.2, bty="n")
                     
                     text(0,1,paste0(modelName,"\nsur\n",
                                 paste(instCombi,collapse=' et '),
                                 "\n\n", names(preTreatData)[i],
                                 "\n\nÉchantillon: ",echID),
                           adj=c(0,1), cex=1.5, font=2)
                     
                     #ODist vs SDist 
                     #Calcul OD et SD pour l'échantillon
                     #Voir manuel de PLS Toolbox de EigenVector dans Doc du projet
                     scs <- lesPreds[1,1:unModele$lesNCPs[[i]],drop=F]
                     eigVals <- unModele$lesACPs[[i]]$sdev[1:unModele$lesNCPs[[i]]] 
                     midMat <- diag(1/eigVals^2)
                     SD <- sqrt(scs %*% midMat %*% t(scs))
                     
                     x <- as.matrix(newdats-unModele$lesACPs[[i]]$center)
                     lds <- unModele$lesACPs[[i]]$rotation[,1:unModele$lesNCPs[[i]],drop=F]
                     midMat <- diag(nrow=dim(x)[2]) - lds %*% t(lds)
                     OD <- sqrt(x %*% midMat %*% t(x))
                     
                     xLimits <- extendrange(c(unModele$dds[[i]]$SDist,SD))
                     yLimits <- extendrange(c(unModele$dds[[i]]$ODist,OD))
                     plot(unModele$dds[[i]]$SDist,unModele$dds[[i]]$ODist, pch = 21, cex=1.5,
                          xlim = xLimits,
                          ylim = yLimits,
                          col = "black", bg=mescols_fill[toColor],
                          xlab=toupper("Distance dans le modèle"),
                          ylab = toupper("Distance résiduelle"),
                          cex.lab=1.5, cex.axis=1.5)
                     points(SD,OD,col="white", bg="red", cex=2.5, pch=21)
                     abline(h=unModele$dds[[i]]$critOD, v=unModele$dds[[i]]$critSD,
                            col="red",lwd=2,lty=2)
                     
                     dev.off()
                     
                     
                     nggs <- length(lesplots)
                     lesplots[[paste0("ACP ",nggs+1)]] <<- #<<- because lapply!
                       list(src = outfile,
                            contentType = 'image/png',
                            width = width,
                            height = height,
                            alt = paste0(modelName," sur ",
                                         paste(instCombi,collapse=' et '),
                                         " - ",names(preTreatData)[i])
                       )
                     
                     par(mfrow=c(1,1))
                   }
                   ##### Sortie fichier----
                   leDir <- file.path(dataPath,"resModel")
                   if (!dir.exists(leDir)) dir.create(leDir)
                   fname <- file.path(leDir,paste0(modelName,"_",
                                                   unModele$model_descript$datatype[i],"_",
                                                   paste(instCombi,collapse='_'),".txt"))
                   #Création d'une ligne pour écrire dans le fichier.
                   ligne=NULL
                   #Date et heure pour insérer dans le fichier
                   ladate=as.character(Sys.Date())
                   letemps=format(Sys.time(),"%X")
                   
                   ncol=dim(Plan$leplan)[2]
                   for (i in 1:ncol){
                     ligne=c(ligne,as.character(Plan$leplan[Plan$selected,i]))
                   }
                   ligne=c(ligne,ladate,letemps,as.character(signif(lesPreds[1:unModele$lesNCPs[[i]]],4)))
                   ligne=c(ligne,signif(c(SD,OD),4))
                   if (file.exists(fname)) {  #append
                     mycon=file(fname,"a")
                     cat(ligne,file=mycon,sep="\t")
                     cat("\n",file=mycon)
                     close(mycon)
                   }else
                   {
                     entete=c(names(Plan$leplan),"Date","Heure",
                              paste0("PC",1:unModele$lesNCPs[[i]]),"SD","OD")
                     mycon=file(fname,"a")   
                     cat(entete,file=mycon,sep="\t")   #entête
                     cat("\n",file=mycon)
                     cat(ligne,file=mycon,sep="\t") #données
                     cat("\n",file=mycon)
                     close(mycon)
                   }
                 })
              }
             },
             #### PLSDA----
             PLSDA = {
               if (unModele$model_descript$aggregation=="concatenate"){
                 #ATTN : do not work with XData_p but with the selected items.
                 for (dtype in unModele$model_descript$datatype){
                   idtype <- which(dtype==unModele$model_descript$datatype)
                   spdf<-as.data.frame(t(preTreatData[[dtype]][2,]))
                   colnames(spdf)<-paste(dtype,as.character(preTreatData[[dtype]][1,]),sep="_")
                   if (idtype==1){
                     y=spdf
                   }else
                   {
                     y <- cbind(y,spdf)
                   }
                 }
                 plsda_set <- list(y)
               }else
               {
                 ind_lesX_list<-as.list(seq_len(length(unModele$model_descript$datatype)))
                 plsda_set <- list()
                 for (dtype in unModele$model_descript$datatype){
                   idtype <- which(dtype==unModele$model_descript$datatype)
                   spdf<-as.data.frame(t(preTreatData[[dtype]][2,]))
                   colnames(spdf)<-paste(dtype,as.character(preTreatData[[dtype]][1,]),sep="_")
                   plsda_set[[dtype]] <- spdf
                  }
               }
               
               plsda_probs <- Predict_plsda(unModele,mydata=plsda_set,probs=TRUE)
               plsda_cl <- Predict_plsda(unModele,mydata=plsda_set,probs=FALSE)
               
               ##### Sortie fichier----
               leDir <- file.path(dataPath,"resModel")
               if (!dir.exists(leDir)) dir.create(leDir)
               fname <- file.path(leDir,paste0(modelName,"_",paste(instCombi,collapse='_'),".txt"))
               #Création d'une ligne pour écrire dans le fichier.
               ligne=NULL
               #Date et heure pour insérer dans le fichier
               ladate=as.character(Sys.Date())
               letemps=format(Sys.time(),"%X")
               
               ncol=dim(Plan$leplan)[2]
               for (i in 1:ncol){
                 ligne=c(ligne,as.character(Plan$leplan[Plan$selected,i]))
               }
               ligne=c(ligne,ladate,letemps,
                       as.character(signif(plsda_probs,4)),
                       as.character(plsda_cl))
               if (file.exists(fname)) {  #append
                 mycon=file(fname,"a")
                 cat(ligne,file=mycon,sep="\t")
                 cat("\n",file=mycon)
                 close(mycon)
               }else
               {
                 entete=c(names(Plan$leplan),"Date","Heure",names(plsda_probs),"Prédiction")
                 mycon=file(fname,"a")   
                 cat(entete,file=mycon,sep="\t")   #entête
                 cat("\n",file=mycon)
                 cat(ligne,file=mycon,sep="\t") #données
                 cat("\n",file=mycon)
                 close(mycon)
               }
               ##### Graphique----
               
               nCl <- length(levels(plsda_cl))
               pred_prob <- Predict_plsda(unModele,probs=TRUE)
               dum1<-data.frame(cl=unModele$plsdaFit[[1]]$trainingData[,1],pred_prob)
               dum2<-tidyr::gather(dum1,Pred,Prob,-cl,factor_key = TRUE)
               levels(dum2$cl)=paste("True: ",levels(dum2$cl),sep="")
               if (plotMe){
                 nggs <- length(lesplots)
                 fPattern <- paste0("Plot",sprintf("%02d", nggs+1))
                 outfile <- tempfile(pattern=fPattern, fileext = '.png')
                 png(outfile, width = width, height = height)
                 
                 p<-ggplot2::ggplot(dum2,ggplot2::aes(Pred,Prob))
                 p<-p+ggplot2::geom_boxplot()
                 p<-p+geom_col(data=data.frame(cl=rep(paste0("PRÉDICTION - ",plsda_cl),nCl),
                                                 Pred=colnames(plsda_probs),
                                                 Prob=as.numeric(plsda_probs[1,])),
                                 ggplot2::aes(Pred,Prob, fill=Prob), colour=NA) +
                   scale_fill_gradient(low = "darkred", high = "green", na.value = NA)
                 p<-p +ggplot2::facet_wrap(~cl)
                                           
                 p<-p + ggplot2::theme(text = element_text(size=20))
                 p<-p + ggplot2::theme(axis.text.x = element_text(angle=60, hjust=1, vjust=1))
                 print(p)
                 dev.off()
                 nggs <- length(lesplots)
                 lesplots[[paste0("PLSDA ",nggs+1)]] <- 
                   list(src = outfile,
                        contentType = 'image/png',
                        width = width,
                        height = height,
                        alt = paste0(modelName," sur ",
                                     paste(instCombi,collapse=' et '))
                   )
               }
             }
      )
    }
  }
  PlotAll(lesplots,echID)
  invisible()
}