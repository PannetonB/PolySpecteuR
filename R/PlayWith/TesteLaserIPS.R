#Teste  le laser IPS
source("~/Programmes/RStudioProject_2022/PolySpecteuR/Lib_n_Wrapper/IPS_Laser.R", encoding = 'UTF-8')

#Teste les fonctions
Connect_IPS()

LaserID()

LaserOn(1,700)  #ajuste le courant

ShutterOn(1)

GetLaserCurrent()

GetLaserPower()

SetLaserCurrent(500)

GetLaserCurrent()

GetLaserPower()

ShutterOff()

LaserOff()

ShutterOn(1)

GetLaserCurrent()

Close_IPS()

#Teste la relation courant-puissance
Connect_IPS()
ShutterOn(1)
cset <- seq(300,800,50)
pset <- rep(0,length(cset))
for (i in 1:length(cset)){
  SetLaserCurrent(cset[i])
  Sys.sleep(5)
  pset[i] <- GetLaserPower()
}
ShutterOff()
LaserOff()
Close_IPS()

ggplot2::qplot(cset,pset,geom="line",main = "IPS laser s/n 30969", xlab = "Courant [mA]", ylab = "Puissance [mW]") + 
  geom_line(size=1.5, colour=I("blue"))


