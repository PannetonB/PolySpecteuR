modSimplify <- function(){
# Fonction pour simplifier les modèles développés dans InSpectoR afin de 
# réduire la taille de stockage  

stripModel <- function(unModele){
  #Test if each element is needed. If not needed just trow out!
  #First build a list of required list members (indices)
  indi = as.list(seq_along(unModele))
  noms = names(unModele)
  indices <- numeric()
  
  #Test each member to see if required. If required, test will produce
  # error and index of required element is stored.
  lapply(indi, function(i){
    test = unModele
    test[[i]] <- NULL
    cat("\n\nTesting ",noms[i])
    tryCatch (predict(test), error = function(e) indices <<- c(indices,i))
  })
  
  #Strip to keep only indices
  dum_small <- unModele
  for (k in 1:length(unModele)){
    if (!any(k==indices)) dum_small[noms[k]] <- NULL
  } 
  #Reports size reduction.
  sizeIn <- object.size(unModele)
  sizeOut <- object.size(dum_small)
  cat("\n**********************\n")
  cat("Original model size: ",sizeIn,"\n")
  cat("Stripped model size: ",sizeOut,"\n")
  cat("**********************\n")
  
  return(dum_small)
  
  
}    
  
  lefile <- choose.files(caption="Choose an InSpectoR model file",
                         multi=FALSE,
                         filters= Filters["RData"] )
  load(lefile)
  modType <- model_descript$type
  
  if (modType == "PCA"){  # a list of models!
    lesACPs <- lapply(lesACPs,stripModel)
    ff <- tools::file_path_sans_ext(lefile)
    ff <- file.path(paste0(ff,"_stripped.RData"))
    save(model_descript,prepro_params,lesACPs,lesNCPs,colorby, file=ff)
  }
  if (modType == "PLSDA"){  # a list of models!
    plsdaFit <- lapply(plsdaFit ,function(mod){
      mod <- stripModel(mod$finalModel)
      #Remove some environment which takes huge space
      attr(attr(mod$model,which = "terms") , which = ".Environment") <- NULL
      attr(mod$terms,which = ".Environment") <- NULL
      return(mod)
    })
    ff <- tools::file_path_sans_ext(lefile)
    ff <- file.path(paste0(ff,"_stripped.RData"))
    save(model_descript,prepro_params,plsdaFit,pls_ncomp, file=ff)
  }
  if (modType == "PLS"){  # a list of models!
    plsFit <- lapply(plsFit ,function(mod){
      mod <- stripModel(mod)
      #Remove some environment which takes huge space
      attr(attr(mod$model,which = "terms") , which = ".Environment") <- NULL
      attr(mod$terms,which = ".Environment") <- NULL
      return(mod)
    })
    ff <- tools::file_path_sans_ext(lefile)
    ff <- file.path(paste0(ff,"_stripped.RData"))
    save(model_descript,prepro_params,plsdaFit,pls_ncomp, file=ff)
  }
  cat("\n**********************\n")
  cat("Original file size: ",file.size(lefile),"\n")
  cat("Final file size: ",file.size(ff),"\n")
  cat("Size ratio: ",round(file.size(lefile)/file.size(ff),2),"\n")
  cat("**********************\n")
}