#Essai pour appeler les prétraitements

unModele <- modelEnv$PCA_300_420_Raman
unModele <- modelEnv$PLSDA_300_Raman_Huile2


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
    preTreatData[[dtype]] <- Apply_PreTreatments(unModele$prepro_params,whichInst,lesSp[[dtype]])
  }
}
