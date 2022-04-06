modSimplify <- function(){
# Fonction pour simplifier les modèles développés dans InSpectoR afin de 
# réduire la taille de stockage. On y parvient en enlevant des environnements  

  lefile <- choose.files(caption="Choose an InSpectoR model file",
                         multi=FALSE,
                         filters= Filters["RData"] )
  load(lefile)
  modType <- model_descript$type
  
  
  
  if (modType == "PCA"){  # a list of models!
    #Rien à faire.
    ff <- tools::file_path_sans_ext(lefile)
    ff <- file.path(paste0(ff,"_stripped.RData"))
    save(model_descript,prepro_params,lesACPs,lesNCPs,colorby, file=ff)
  }
  if (modType == "PLSDA"){  # a list of models!
    plsdaFit <- lapply(plsdaFit ,function(mod){
      #mod <- mod$finalModel
      #Remove some environment which takes huge space
      attr(attr(mod$finalModel$model,which = "terms") , which = ".Environment") <- NULL
      attr(mod$finalModel$terms,which = ".Environment") <- NULL
      return(mod)
    })
    ff <- tools::file_path_sans_ext(lefile)
    ff <- file.path(paste0(ff,"_stripped.RData"))
    save(model_descript,prepro_params,plsdaFit,pls_ncomp, file=ff)
  }
  if (modType == "PLS"){  # a list of models!
    plsFit <- lapply(plsFit ,function(mod){
      #Remove some environment which takes huge space
      attr(attr(mod$model,which = "terms") , which = ".Environment") <- NULL
      attr(mod$terms,which = ".Environment") <- NULL
      return(mod)
    })
    ff <- tools::file_path_sans_ext(lefile)
    ff <- file.path(paste0(ff,"_stripped.RData"))
    save(model_descript,prepro_params,plsFit,pls_ncomp, file=ff)
  }
  cat("\n**********************\n")
  cat("Original file size: ",file.size(lefile),"\n")
  cat("Final file size: ",file.size(ff),"\n")
  cat("Size ratio: ",round(file.size(lefile)/file.size(ff),2),"\n")
  cat("**********************\n")
}