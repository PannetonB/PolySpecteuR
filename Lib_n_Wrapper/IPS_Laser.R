#*****************************************************************************
#*****************************************************************************
# - Outils pour interagir avec le laser IPS Digital U-type module
# - Le nom des fonctions n'est pas toujours conforme aux fonctions disponibles
#   pour ce laser. Cela est nécessaire pour avoir les mêmes noms de fonctions
#   que pour la laser Newport LS2 afin de rendre les fonctions qui utilisent
#   un laser transparente par rapport au laser utilisé.
# - Voir chaque fonction pour les détails.
#*****************************************************************************
# AUTEUR: Bernard Panneton, Agriculture et Agroalimentaire Canada
# Février 2022
#*****************************************************************************
#*

# Charge les librairies utilisées ----
ok=require(serial)       #Pour accéder au RS232
if (!ok){
  install.packages("serial")
  require(serial)
} 
ok=require(stringr)  #POur manipulation de chaînes de caractères
if (!ok){
  install.packages("stringr")
  require(stringr)
} 

# Connecter au laser via RS232 -----
Connect_IPS <- function(){
  lesports <- listPorts()
  leport <- utils::select.list(choices=c("Aucun",lesports),
                               title = "Port pour IPS.",
                               graphics = T)
  con <<- serialConnection(name = "lecom",port = leport
                          ,mode = "115200,n,8,1"
                          ,newline = 1
                          ,translation = "crlf"
  )
  open(con)
  if (!isOpen(con)){
    utils::winDialog(type="ok",
            message="Ne peut pas ouvrir le port pour le laser IPS")
  return("ABANDON")
  }
  return("OK")
}

# Lit des paramètres du laser ----------
LaserID <- function(){
  write.serialConnection(con,"*IDN?")
  while(nBytesInQueue(con)[1]==0) Sys.sleep(0.1)
  lasID <- read.serialConnection(con)
  lasID <- unlist(strsplit(lasID,","))
  wavelength <- lasID[3]
  serialno <- lasID[3]
  firmwareVer <- lasID[5]
  return(list(wavelength=lasID[4],
              serialno=lasID[3],
              firmwareVer=lasID[5]))
}

#SetLaserCurrent-------
SetLaserCurrent <- function(current){
  write.serialConnection(con,
                         paste0("LASER:CUR  ",as.character(current)))
}

# GetLaserCurrent-------
GetLaserCurrent <- function(){
  write.serialConnection(con,
                         "LASER:CUR?")
  while(nBytesInQueue(con)[1]==0) Sys.sleep(0.1)
  lecur <- read.serialConnection(con)
  if (as.numeric(lecur)!=current){
    utils::winDialog(type="ok",
                     message="Échec en ajustant le courant du laser.")
  }
  return(as.numeric(lecur))
}

#LaserOn --------
LaserOn <- function(LasNum,laserCur){
#Paramètre LasNum pas utilisé. Pour compatibilité avec Newport_LS_2.R
  SetLaserCurrent(laserCur)
  dum <- GetLaserCurrent()
  if (dum!=laserCur){
    utils::winDialog(type="ok",
                     message="Échec en ajustant le courant du laser.")
  }
}

#LaserOff ------
LaserOff <- function(lasNum){
#Paramètre LasNum pas utilisé. Pour compatibilité avec Newport_LS_2.R
  SetLaserCurrent(0)
  dum <- GetLaserCurrent()
  if (dum!=laserCur){
    utils::winDialog(type="ok",
                     message="Échec en ajustant le courant du laser à 0.")
  }
}

#ShutterOn--------
ShutterOn<-function(LasNum){
#Paramètre LasNum pas utilisé. Pour compatibilité avec Newport_LS_2.R
  write.serialConnection(con,"LASER:EN ON")
}

#ShutterOff--------
ShutterOff<-function(LasNum){
  #Paramètre LasNum pas utilisé. Pour compatibilité avec Newport_LS_2.R
  write.serialConnection(con,"LASER:EN OFF")
}


# Fermer le laser et le déconnecter----
Close_IPS <- function(){
  ShutterOff()
  LaserOff()
  close(con)
}
  
  
  