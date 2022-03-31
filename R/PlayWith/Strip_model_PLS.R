#Run InSpectoR to build XData_p and create PLS model
#Here 2 spectra in model
c = cbind(XData_p[[1]][2:3,],XData_p[[2]][2:3,])

#Retrieve model
dumf<-plsFit[[1]]

#Test if each element is needed. If not needed just trow out!
#First build a list of required list members (indices)
#dumf=dum$finalModel
indi = as.list(seq_along(dumf))
noms = names(dumf)
indices <- numeric()
lapply(indi, function(i){
  test = dumf
  test[[i]] <- NULL
  cat("\n\nTesting ",noms[i])
  tryCatch (predict(test,newdata=c), error = function(e) indices <<- c(indices,i))
})

#Strip to keep only indices
dum_small = dumf
for (k in 1:length(dumf)){
  if (!any(k==indices)) dum_small[noms[k]] <- NULL
} 
predict(dum_small, newdata = c, ncomp = dum_small$ncomp)

object.size(dumf)
object.size(dum_small)
