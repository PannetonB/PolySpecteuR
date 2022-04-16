#Essai pour appeler les prétraitements

unModele <- modelEnv$PCA_300_420_Raman; modelName <- names(modelEnv)[3]
unModele <- modelEnv$PLSDA_300_Raman_Huile2; modelName <- names(modelEnv)[1]
unModele <- modelEnv$PLS_300_420_4_Induction; modelName <- names(modelEnv)[2]
plotMe <- TRUE
debugPlot <- FALSE

#*****************************************************************************
#Nomme les éléments de lesInstruments avec nomInstrument----
nomsInstrument <- lapply(lesInstruments, function(inst) inst$nomInstrument)
names(lesInstruments) <- nomsInstrument 



library(ggplot2)
#*****************************************************************************
#get_DELs_dat-----------------------------------------
#*****************************************************************************
get_DELs_dat <- function(varname,mon_envir=.GlobalEnv)
  #Fonction pour récupérer des vecteurs/listes de paramètres
{
  dum <- ls(envir = mon_envir, pattern = paste0("^",varname,".$"))
  dum <- unname(sapply(dum,FUN=function(x) get(x,envir=mon_envir)))
  return(dum)
}

source(file.path(here::here(),"R/PlayWith/Apply_PreTreatments.R"), encoding = 'UTF-8')
source(file.path(here::here(),"R/PlayWith/playTwoSpectra.R"), encoding = 'UTF-8')


#******************************************************************************
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
        predict(plsdaFit[[ii]]$finalModel, newdata=mydata[[ii]][,-1], type="prob")
    })
    Ps<-lapply(Ps,drop)  #remove useless third dimension.
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
      val_pred_cl <- predict(plsdaFit[[1]],newdata=plsdaFit[[1]]$trainingData[,-1]
                             , type="prob")
    }else
    {
      val_pred_cl <- predict(plsdaFit[[1]],newdata=mydata[[1]], type="prob")
    }
    classes<-colnames(val_pred_cl)
    if (!probs)
      val_pred_cl <- factor(classes[apply(val_pred_cl, 1, which.max)], levels = classes) 
  }
  return(val_pred_cl)
}


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


#Pour chaque combinaison d'instruments:

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
    #Prétraitement
    whichDType <- which(dtype==modDTypes)
    preTreatData[[dtype]] <- Apply_PreTreatments(unModele$prepro_params,whichDType,lesSp[[dtype]])
    if (debugPlot) {
      plotSp(lesSp[[dtype]],preTreatData[[dtype]])
      title(paste(dtype,"sur",instCombi[whichInst]))
    }
  }
  if (debugPlot) par(mfrow=c(1,1))
  #Prédiction
  switch(unModele$model_descript$type,
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
           }else
           {
             
           }
           plspreds<-predict(unModele$plsFit[[1]],
                             newdata = pls_set,
                             ncomp=unModele$pls_ncomp)
           
           if (plotMe){
             nComp <- unModele$pls_ncomp
             x=unModele$plsFit[[1]]$fitted.values[,,nComp] + 
               unModele$plsFit[[1]]$residuals[,,nComp]
             y=predict(unModele$plsFit[[1]],ncomp=nComp)
             pred <- unlist(plspreds)
             
             lm.out <- lm(y ~ x)
             newx = seq(min(x),max(x),by = 0.05)
             conf_interval <- predict(lm.out, newdata=data.frame(x=newx), interval="confidence",
                                      level = 0.95)
             
             
             plot(x, y, xlab="x", ylab="y",
                  main=paste0("Model ... - Valeur prédite: ",signif(pred,3)),
                  type="p",col="lightblue", pch=20)
             abline(lm.out, col="blue",lwd=2)
             matlines(newx, conf_interval[,2:3], col = "red", lty=2, lwd=2)
             
             
             top <- conf_interval[,3]
             xtop <- approx(top,newx, xout=pred)
             
             bot <- conf_interval[,2]
             xbot <- approx(bot,newx, xout=pred)
             grid()
             
             
             plotrix::boxed.labels(xtop$y,pred,paste0("   ",signif(xtop$y,3)),srt=90,
                                   adj=c(0,0), font=2, ypad=4, border = F)
             plotrix::boxed.labels(xbot$y,pred,paste0("   ",signif(xbot$y,3)),
                                   srt=90, adj=c(0,0),font=2, ypad=4, border = F)
             lines(c(-1e20,xbot$y),c(pred,pred),col="green",lwd=3)
             lines(c(xtop$y,xtop$y),c(-1e20,pred),col="green",lwd=3)
             lines(c(xbot$y,xbot$y),c(-1e20,pred),col="green",lwd=3)
           }
           
           
         },
         
         PCA = {
           iind=as.list(1:length(preTreatData))
           acp_pred <- lapply(iind, function(i){
             newdats <- as.data.frame(t(preTreatData[[i]][2,]))
             colnames(newdats) <- dimnames(unModele$lesACPs[[i]]$rotation)[[1]]
             lesPreds <- predict(unModele$lesACPs[[i]],newdats)
            
             
             #Plots
             if (plotMe){
               mescols=c("darkred","blue","green3","salmon","yellow3","black","red3","magenta","gray70","cyan") 
               #To define corresponding lighter transparent colors for symbol fill
               mescols_fill=col2rgb(mescols,alpha=TRUE)
               mescols_fill[4,]=145
               mescols_fill=rgb(mescols_fill[1,],mescols_fill[2,],mescols_fill[3,],alpha=mescols_fill[4,],maxColorValue = 255)
               recycling=ceiling(length(unique(unModele$colorby))/10)  #Only 10 colors defined, so we recycle if necessary
               mescols_fill=rep(mescols_fill,recycling)
               
               par(mfrow=c(2,2))
               lesScores <- unModele$lesACPs[[i]]$x
               for (k in seq(1,4,2)){
                 xLimits <- extendrange(c(lesScores[,k],lesPreds[k]))
                 yLimits <- extendrange(c(lesScores[,(k+1)],lesPreds[k+1]))
                 plot(lesScores[,k],lesScores[,(k+1)], pch = 21, cex=1.5,
                      xlim = xLimits,
                      ylim = yLimits,
                      col = "black", bg=mescols_fill[unModele$colorby],
                      xlab=paste0("PC",k), ylab = paste0("PC",(k+1)))
                 points(lesPreds[k],lesPreds[k+1]
                        ,col="white", bg="red", cex=2.5, pch=21)
                 abline(v=0,h=0,col="gray80",lty=3)
               }
               #TITRE et LÉGENDE
               plot.new()
               classes <- levels(unModele$colorby)
               nCl <- length(classes)
               legend("topright",legend=c(classes,"Prédiction","Limites de confiance"),
                      col=c(rep("black",nCl),"white","red"),
                      lty = c(rep(0,nCl),0,2),
                      lwd = c(rep(0,nCl),0,2),
                      pt.bg=c(mescols_fill[1:nCl],"red",NA), 
                      pch=c(rep(21,nCl),21,NA), 
                      pt.cex = c(rep(1.5,nCl),2.5), cex=1.1)
               
               text(0,1,paste0(modelName,"\nsur\n",
                           paste(instCombi,collapse=' et '),
                           "\n\n", names(preTreatData)[i]),
                     adj=c(0,1), cex=1.25, font=2)
               
               #ODist vs SDist 
               #Calcul OD et SD pour l'échantillon
               scs <- lesPreds
               eigVals <- unModele$lesACPs[[i]]$sdev[1:unModele$lesNCPs[[i]]] 
               midMat <- diag(1/eigVals^2)
               SD <- sqrt(scs %*% midMat %*% t(scs))
               
               x <- as.matrix(newdats-unModele$lesACPs[[i]]$center)
               lds <- unModele$lesACPs[[i]]$rotation
               midMat <- diag(nrow=dim(x)[2]) - lds %*% t(lds)
               OD <- sqrt(x %*% midMat %*% t(x))
               
               xLimits <- extendrange(c(unModele$dds[[i]]$SDist,SD))
               yLimits <- extendrange(c(unModele$dds[[i]]$ODist,OD))
               plot(unModele$dds[[i]]$SDist,unModele$dds[[i]]$ODist, pch = 21, cex=1.5,
                    xlim = xLimits,
                    ylim = yLimits,
                    col = "black", bg=mescols_fill[unModele$colorby],
                    xlab="Distance dans le modèle",
                    ylab = "Distance résiduelle")
               points(SD,OD,col="white", bg="red", cex=2.5, pch=21)
               abline(h=unModele$dds[[i]]$critOD, v=unModele$dds[[i]]$critSD,
                      col="red",lwd=2,lty=2)
             }
             if (plotMe) par(mfrow=c(1,1))
             lesPreds
           })
           
         },
         
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
             ind_lesX_list<-as.list(seq_len(length(model_descript$datatype)))
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
           if (plotMe){
             p <- ggplot(data.frame(x=names(plsda_probs), y=as.numeric(plsda_probs[1,])),
                    aes(x=x, y=y, fill=y)) + 
               ggplot2::geom_bar(stat="identity") + 
               ggplot2::scale_fill_gradient(low = "red", high = "green") +
               theme(legend.position="none", text = element_text(size = 16)) +
               ggtitle(paste(modelName,"sur",
                             paste(instCombi,collapse=' et '))) +
               labs(y="Probabilité prédite", x="Classes")  
             print(p)
           }
         }
  )
}

