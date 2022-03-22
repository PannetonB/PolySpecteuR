dyn.load(  "C:/Users/pannetonb/Documents/My Documents/Projets/Spectro/SAIV_Version_Alain_2016/Progs/C/cbw64.dll")
library(inline)
src = ' int cbIgnoreInstaCal();
        int dum = cbIgnoreInstaCal();
        return Rcpp::wrap(dum);'

l = cfunction(body=src, Rcpp=TRUE,
              libargs = "-L. cbw64.dll")

cppFunction('int BP_cbIgnoreInstaCal(){
        int cbIgnoreInstaCal();
        int dum = cbIgnoreInstaCal();
        return(dum);
};')

cat(BP_cbIgnoreInstaCal())

dyn.unload(  "C:/Users/pannetonb/Documents/My Documents/Projets/Spectro/SAIV_Version_Alain_2016/Progs/C/cbw64.dll")
