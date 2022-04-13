plotSp <- function(sp1, sp2 ){
  plotrix::twoord.stackplot(sp1[1,],sp2[1,],sp1[2,],sp2[2,],
                       lcol="blue",rcol="red",
                       ltype="l", rtype="l", lwd=3,
                       xlab="Wavelength or Wavenumber",
                       rylab="PréTraité", lylab="Brute"
                      )
}
