modSimplify <- function(){

stripModel <- function(unModele){
  #Test if each element is needed. If not needed just trow out!
  #First build a list of required list members (indices)
  indi = as.list(seq_along(unModele))
  noms = names(unModele)
  indices <- numeric()
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
  sizeIn <- object.size(unModele)
  sizeOut <- object.size(dum_small)
  cat("\n**********************\n")
  cat("Original size: ",sizeIn,"\n")
  cat("Final size: ",sizeOut,"\n")
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
    plsdaFit <- lapply(plsdaFit,function(mod) stripModel(mod$finalModel))
    ff <- tools::file_path_sans_ext(lefile)
    ff <- file.path(paste0(ff,"_stripped.RData"))
    save(model_descript,prepro_params,plsdaFit,pls_ncomp, file=ff)
  }
  
}