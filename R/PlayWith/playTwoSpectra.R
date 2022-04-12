plotSp <- function(sp1, sp2 ){
  plotrix::twoord.plot(sp1[1,],sp1[2,],sp2[1,],sp2[2,],
                       lcol="blue",rcol="red",
                       type="l", lwd=3,
                       xlab="Wavelength or Wavenumber"
                      )
  grid()
}
