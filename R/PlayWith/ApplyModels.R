#Essai pour appeler les prétraitements

unModele <- modelEnv$PCA_300_420_Raman; modelName <- names(modelEnv)[3]
unModele <- modelEnv$PLSDA_300_Raman_Huile2; modelName <- names(modelEnv)[1]
unModele <- modelEnv$PLS_300_420_4_Induction; modelName <- names(modelEnv)[2]
plotMe <- TRUE



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
  
  if (plotMe) par(mfrow=c(2,2))
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
    if (plotMe) {
      plotSp(lesSp[[dtype]],preTreatData[[dtype]])
      title(paste(dtype,"sur",instCombi[whichInst]))
    }
  }
  if (plotMe) par(mfrow=c(1,1))
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
           plspreds<-predict(unModele$plsFit,
                             newdata = pls_set,
                             ncomp=unModele$pls_ncomp)
           if (plotMe){
            yRefs <- unlist(predict(unModele$plsFit))
            yLimits <- range(c(yRefs,unlist(plspreds))) 
            plot(yRefs, pch=21,
                 col="blue",bg="cyan",cex=1.4,
                 ylim=yLimits,
                 ylab = "Prédiction",
                 xlab = "Échantillon du jeu d'étalonnage")
            title(paste(modelName,"avec",instCombi),adj=0)
            abline(h=unlist(plspreds),col="red",lwd=2)
            par(xpd=TRUE)
            legend("topright", legend=c("Étalonnage","Nouvelle valeur"),
                   horiz=T, inset=c(0.01,-0.1), bty="n",
                   pch=c(21,NA), lty=c(NA,1), lwd = c(1,2),
                   col=c("blue","red"), pt.bg="cyan")
            par(xpd=F)
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
               par(mfrow=c(2,2))
               lesScores <- unModele$lesACPs[[i]]$x
               for (k in seq(1,8,2)){
                 xLimits <- range(c(lesScores[,k],lesPreds[k]))
                 yLimits <- range(c(lesScores[,(k+1)],lesPreds[k+1]))
                 plot(lesScores[,k],lesScores[,(k+1)], pch = 21, cex=1.4,
                      xlim = xLimits,
                      ylim = yLimits,
                      col = "blue", bg ="cyan",
                      xlab=paste0("PC",k), ylab = paste0("PC",(k+1)), cex.main=0.5)
                 points(lesPreds[k],lesPreds[k+1],col="red", bg="pink", cex=2, pch=21)
                 if (k==3){
                   par(xpd=T)
                   legend("topright",legend=c("Modèle","Prédiction"),
                                  pch=21, col=c("blue","red"), pt.bg=c("cyan","pink"),
                                  bty="n", horiz = T, inset=c(0.01,-0.15))
                   par(xpd=F)
                 }
                 if (k==1){ 
                   title(paste(modelName,"sur",
                               paste(instCombi,collapse=' et '),
                               "-", names(preTreatData)[i]),
                         adj=0, cex.main=0.85)
                 }
               }
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
               ggplot2::scale_fill_gradient(low = "red", high = "green", limits = c(0.1,0.9)) +
               theme(legend.position="none") +
               ggtitle(paste(modelName,"sur",
                             paste(instCombi,collapse=' et '))) +
               labs(y="Probabilité prédite", x="Classes")
             print(p)
           }
         }
  )
}

