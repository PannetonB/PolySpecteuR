testSrcPath <- function(){
RPath=utils::getSrcDirectory(function(x) {x})
setwd(RPath)
return(RPath)
}
