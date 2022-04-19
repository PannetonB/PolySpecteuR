library(plsRglm)

data(Cornell)

#Lazraq-Cleroux PLS (Y,X) bootstrap
set.seed(250)
modpls <- plsR(Y~.,data=Cornell,3)
Cornell.bootYX <- bootpls(modpls, R=250, verbose=FALSE)
confints.bootpls(Cornell.bootYX,2:8)
confints.bootpls(Cornell.bootYX,2:8,typeBCa=FALSE)

boxplots.bootpls(Cornell.bootYX,indices=2:8)

plots.confints.bootpls(confints.bootpls(Cornell.bootYX,indices=2:8))


