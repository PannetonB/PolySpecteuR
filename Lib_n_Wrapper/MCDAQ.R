# *****************************************************************************
# FICHIER: MCDAQ.R
# Ce fichier comprend des fonctions pour faciliter l'utilisation de la bibliothèque
# cbw64.dll pour interagir avec les modules de Measurement Computing. Il y a des variables
# qui sont définies dans la première partie du fichier suivi d'un ensemble de fonctions.
# Chaque fonction comprend une description indiquant son utilisation et définissant les 
# paramètres.
# *****************************************************************************
# ATTENTION - CHEMIN POUR LE DLL SPÉCIFIÉ EN ABSOLU.
# ******************************************************************************
# Auteur: Bernard Panneton, Agriculture et Agroalimentaire Canada, St-Jean-sur-Richelieu
# Avril 2017
# Droits d'auteur régis par la licence GPL-3 de R. Pour plus d'informations,
# exécuter la commande RShowDoc("GPL-3") dans R.
# GNU GENERAL PUBLIC LICENSE
# Version 3
# Cette librairie fait appel à un "wrapper" écrit en C (MC_cbw64_CWrapper.dll)
# dont l'auteur est Bernard Panneton d'Agriculture et Agroalimentaire Canada. Ce
# wrapper n'est pas couvert par la licence GNU GPL. Aussi ce "wrapper" fait le 
# lien avec une librairie fournie par la compagnie Measurement Computing Inc
# (cbw64.dll).
# ******************************************************************************

library(stringr)

AUXPORT=1
FIRSTPORTA=10
FIRSTPORTB=11

HIGH=1
LOW=0

DIGITALOUT=1
DIGITALIN=2

#Range
BIP10VOLTS = 1              # -10 to 10 volts bipolar
UNI10VOLTS = 100            # 0 to 10 Volts
UNI5VOLTS  = 101            # 0 to 5 volts
MA0TO20 = 204               # 0 to 20 ma
MA4TO20 = 200               # 4 to 20 ma

#Input modes
DIFFERENTIAL=0
SINGLE_ENDED=1

#Parametres pour cbSetConfig
BOARDINFO=2
BIDACRANGE=114


#Type d'interfaces
USB_int=1
BlueTooth_int=2

#******************************************************************************
# Init_MCLIB charge le wrapper de la libraire cbw64.dll, définit comment les erreurs sont
# gérées et indique d'ignorer
# InstaCal ce qui permet de configurer à l'intérieur du programme.
# 
# PARAMÈTRES
#             nil
# SORTIE
#             O.K si tout s'est bien exécuté ou un message indiquant quelle fonction a
#			  échouée.
#******************************************************************************
Init_MCLib <- function(){
  dyn.load("MC_cbw64_CWrapper.dll")
  err=.C("BP_cbIgnoreInstaCal",out=as.integer(0))$out
  if (err==0)
    err=.C("BP_cbErrHandling", as.integer(3), as.integer(0), out=as.integer(0))$out
  else{
    err="BP_cbIgnoreInstaCal a échoué."
    return(err)
  }
  if (err!=0) return("BP_cbErrHandling a échoué")
  else return("O.K.")
}


#******************************************************************************
# Init_DAQ initialise un des modules et lui assigne un "Board Number" à l'aide duquel on
# réfère au module dans les différentes fonctions
# 
# PARAMÈTRES
#             NomModule : le nom du module (e.g. "USB-201")
#             Serie     : le numéro de série du module (e.g. "1B550AB"). Permet de
#						   distinguer 2 modules du même nom.
#             NumModule : le numéro que l'on va assigner au module.
# SORTIE
#             O.K si tout s'est bien exécuté ou un message indiquant quel la fonction a
#			   échouée.
#******************************************************************************
Init_DAQ <- function(NomModule,Serie,NumModule){
  err=.C("BP_InitDAQ",ProductName=NomModule, SerialID=Serie,
                  BoardNum=as.integer(NumModule), out=as.integer(0))$out
  if (err!=0) return("BP_InitDAQ a échoué. Vérifier le nom du module et qu'il est bien branché.")
  else return("O.K")
}


#******************************************************************************
# GetDevices retourne une liste des modules. Cette liste a 4 éléments qui définissent les
# noms, numéro de série le nombre de modules connectés et les numéros des module (Board
# number)
# 
# PARAMÈTRES
#             nil
# SORTIE
#             Liste de 4 éléments: noms est un vecteur des noms des modules; serie est un
#                vecteur des numéros de série
#                 des modules, nbdevices est le nombre de modules connectés et BoardNum
#                 est le "board number" des modules.
#******************************************************************************
GetDevices<-function(){
  pname=list(character(64),character(64),character(64),character(64),character(64),character(64))
  serialnum=pname
  dum=.C("BP_ListDevices", noms=as.character(pname),serie=as.character(serialnum),
											nbdevices=as.integer(3), out=as.integer(0))
  dum$out=NULL  
  dum=c(dum,BoardNum=list(1:dum$nbdevices))
}


#******************************************************************************
# Config_Board fait la configuration des modules. Pour l'instant, ne procède qu'à la
# configuration des ports DIO pour définir les ports d'entrée et de sortie.
# N.B. FONCTIONNE POUR LES BOARDS OÙ CHAQUE BIT EST CONFIGURÉ SÉPARÉMENT.
# 
# PARAMÈTRES
#             lesDevices : liste retournée pour GetDevices
#             lesPorts   : liste des ports. Cette liste est une liste de listes. Chacune
#                           des ces listes a 4 ou 5
#                          éléments: 1-nom du module; 2-numéro de série du module; 3-nom
#                          du port (e.g. DIO_1, VOUT_0...); 4-pour les DIO soit DIGITALIN
#                          ou DIGITALOUT, pour les autres le "range" (e.g. BIP10VOLTS);
#                          5 - pour les DIO, un type de port (AUXPORT ou FIRSTPORTA ou FIRSTPORTB)  
# SORTIE
#             nil
#******************************************************************************
Config_Board<-function(lesDevices,lesPorts){
  #Pour l'instant ne fait que programmer les ports DIO
  lesnoms=lesDevices$noms[1:lesDevices$nbdevices]
  lesseries=lesDevices$serie[1:lesDevices$nbdevices]
  lesboards=lesDevices$BoardNum[1:lesDevices$nbdevices]
  for (k in 1:length(lesPorts)){
    device=lesPorts[[k]][[1]]
    serial=lesPorts[[k]][[2]]
    leport=lesPorts[[k]][[3]]
    direction=lesPorts[[k]][[4]]
    if ((device %in% lesnoms) & (serial %in% lesseries)){
      dum=grep("DIO",leport)
      if (length(dum)>0){
        if (dum==1){
          dum=strsplit(leport,"_")[[1]]
          lebit=dum[[length(dum)]]
          ind=which(!is.na(match(lesseries,serial)+match(lesnoms,device)))
          dum=Config_IO_Bit(lesboards[ind],AUXPORT,lebit,direction)
          DIO_OFF(lesPorts[[k]],lesDevices)
        }
      }
    }
    
  }
}



#******************************************************************************
# Release_Board libère un "Board Number".
# 
# PARAMÈTRES
#             NumModule : le "Board Number"
# SORTIE
#             o.k. ou un message d'erreur
#******************************************************************************
ReleaseBoard<-function(NumModule){
  err=.C("BP_cbReleaseDaqDevice", BoardNum=as.integer(NumModule), out=as.integer(0))$out
  if (err!=0) return(paste("Module ",as.character(NumModule), " non libéré. 
                           Il n'existe peut-être pas.",sep=""))
  else return("O.K")
}


#******************************************************************************
# Quitte_MCLIB "unload" le wrapper.
# 
# PARAMÈTRES
#             nil
# SORTIE
#             o.k. ou un message d'erreur
#******************************************************************************
Quitte_MCLIB<-function(){
  dyn.unload("Lib_n_Wrapper/MC_cbw64_CWrapper.dll")
}



#******************************************************************************
# Config_IO_Bit fait la configuration d'un port DIO en entrée ou sortie. Pour les modules où
# chaque bit d'un port DIO peut être configuré indépendamment des autres.
# 
# PARAMÈTRES
#             bnum      : "Board number"
#             type      : type du port (souvent AUXPORT)
#             bit       : le numéro du bit à configurer (base 0)
#             direct    : DIGITALIN ou DIGITALOUT
# SORTIE
#             o.k. ou un message d'erreur
#******************************************************************************
Config_IO_Bit<-function(bnum,type,bit,direct){
  err=.C("BP_cbDConfigBit",BoardNum=as.integer(bnum), PortType=as.integer(type), 
         BitNum = as.integer(bit), Direction=as.integer(direct), out=as.integer(0))$out
  if (err!=0) return(paste("Échec de la configuration du bit ", as.character(bit),
                            " sur le module ", as.character(bnum),sep=""))
  else return("O.K")
}



#******************************************************************************
# Config_IO_Port fait la configuration d'un port DIO en entrée ou sortie. 
# 
# PARAMÈTRES
#             bnum      : "Board number"
#             type      : type du port (souvent AUXPORT)
#             direct    : DIGITALIN ou DIGITALOUT
# SORTIE
#             o.k. ou un message d'erreur
#******************************************************************************
Config_IO_Port<-function(bnum,type,direct){
  err=.C("BP_cbDConfigPort",BoardNum=as.integer(bnum), PortType=as.integer(type), 
         Direction=as.integer(direct), out=as.integer(0))$out
  if (err!=0) return(paste("Échec de la configuration du Port ", as.character(PortType),
                           " sur le module ", as.character(bnum),sep=""))
  else return("O.K")
}



#******************************************************************************
# AInputMode fait la configuration d'un port analogue. 
# 
# PARAMÈTRES
#             bnum      : "Board number"
#             type      : type du port (DIFFERENTIAL ou SINGLE_ENDED)
# SORTIE
#             o.k. ou un message d'erreur
#******************************************************************************
AInputMode<-function(bnum,type){
  err=.C("BP_cbAInputMode",BoardNum=as.integer(bnum), PortType=as.integer(type),
         out=as.integer(0))$out
  if (err!=0) return(paste("Échec de la configuration du Port ", as.character(PortType),
                           " sur le module ", as.character(bnum),sep=""))
  else return("O.K")
}



#******************************************************************************
# GetBitDepth retourne le nombre de bits pour les ports analogues (e.g.12 bits). Utile pour
# convertir les données brutes.
# PARAMÈTRES
#             device      : nom du module ("LS-201")
# SORTIE
#             nombre de bits.
#******************************************************************************
GetBitDepth<-function(device){
  switch (device,
          "USB-201"=12,
          "USB-3106"=16,
          "USB-3104"=16,
          "USB-1208LS"=12,
          "BTH-1208LS"=12)
}


#******************************************************************************
# BitOut définit la valeur d'un bit DIGITALOUT sur un port DIO (1 pour high, 0 pour low)
# 
# PARAMÈTRES
#             bnum      : "Board number"
#             type      : type du port (souvent AUXPORT)
#             bit       : le numéro du bit à configurer (base 0)
#             level     : 0 = low; 1 = high
# SORTIE
#             o.k. ou un message d'erreur
#******************************************************************************
BitOut<-function(bnum,type,bit,level){
  err=.C("BP_cbDBitOut",BoardNum=as.integer(bnum), PortType=as.integer(type),
          BitNum = as.integer(bit), BitValue=as.integer(level), out=as.integer(0))$out
  if (err!=0) return(paste("Échec d'assigner une valeur au bit ", as.character(bit),
                            " sur le module ", as.character(bnum),sep=""))
  else return("O.K")
}


#******************************************************************************
# BitIn lit la valeur d'un bit DIGITALIN sur un port DIO (0=low, 1=high)
# 
# PARAMÈTRES
#             bnum      : "Board number"
#             type      : type du port (souvent AUXPORT)
#             bit       : le numéro du bit à configurer (base 0)
# SORTIE
#             la valeur du bit (0 ou 1)
#******************************************************************************
BitIn<-function(bnum,type,bit){
  err=.C("BP_cbDBitIn",BoardNum=as.integer(bnum), PortType=as.integer(type), 
            BitNum = as.integer(bnum), BitValue=as.integer(0), out=as.integer(0))
  if (err$out!=0) return(paste("Échec de lecture du bit ", as.character(bit),
                               " sur le module ", as.character(bnum),sep=""))
  else return(err$BitValue)
}



#******************************************************************************
# AnalogIn lit la valeur d'un port analogue
#
# PARAMÈTRES
#             bnum      : "Board number"
#             channel   : le numéro du port
#             plage     : la plage du port définie par une des constantes en tête de ce
#                         fichier (e.g. BIP10VOLTS pour
#                         une plage de -10 à +10 volts)
# SORTIE
#             la valeur brute (e.g. pour un 12 bits entre 0 et 4095)
#******************************************************************************
AnalogIn<-function(bnum,channel,plage){
  
  err=.C("BP_cbAIn",BoardNum=as.integer(bnum),Channel=as.integer(channel),
         Range=as.integer(plage),DataValue=as.integer(0),out=as.integer(0))
  if (err$out!=0) return(paste("Échec en lecture du canal ", as.character(channel),
                               " sur le module ", as.character(bnum),sep=""))
  else return(err$DataValue) 
}


#******************************************************************************
# AnalogScanIn lit la valeur d'un port analogue. Prend des lectures à une fréquence donnée
#
# PARAMÈTRES
#             bnum        : "Board number"
#             plage       : la plage du port définie par une des constantes en tête de ce
#                           fichier (e.g. BIP10VOLTS pour
#                           une plage de -10 à +10 volts)
#             lowchannel  : canal de début
#             highchannel : canal de fin
#             rate        : taux d'échantillonnage (Hz)
#             count       : nombre de lectures
#         
# SORTIE
#             un vecteur de "count" valeurs brutes (e.g. pour un 12 bits entre 0 et 4095).
#             Si highchannel>lowchannel voir documentation de cbw64.dll pour l'ordre des
#             données dans le vecteur de sortie.
#******************************************************************************
AnalogScanIn<-function(bnum, plage, lowchan,highchan,rate,count){
  err=.C("BP_cbAInScan",BoardNum=as.integer(bnum), LowChan=as.integer(lowchan), 
        HighChan=as.integer(highchan), Count=as.integer(count), Rate=as.integer(rate),
		Range=as.integer(plage), DataValue=as.integer(rep(0,count)), out=as.integer(0) )
  return(err$DataValue)
}



#******************************************************************************
# AnalogOut définit la valeur d'un port de sortie analogique
#
# PARAMÈTRES
#             bnum        : "Board number"
#             chan        : le canal analogue
#             plage       : la plage du port définie par une des constantes en tête de ce
#                           fichier (e.g. BIP10VOLTS pour une plage de -10 à +10 volts)
#             value       : valeur brute (e.g. pour un 12 bits entre 0 et 4095)
#         
# SORTIE
#             o.k. ou un message d'erreur
#******************************************************************************
AnalogOut<-function(bnum,chan,plage,value){
  err=.C("BP_cbAOut",BoardNum=as.integer(bnum), Channel=as.integer(chan), 
                 Range=as.integer(plage), DataValue=as.integer(value), out=as.integer(0))
  if (err$out!=0) return(paste("Échec sur sortie analogue sur le canal ", 
                          as.character(chan), " sur le module ", as.character(bnum),sep=""))
  else return("O.K.")
}


#******************************************************************************
# VOut définit la valeur d'un port de sortie analogique en voltage
#
# PARAMÈTRES
#             bnum        : "Board number"
#             chan        : le canal analogue
#             plage       : la plage du port définie par une des constantes en tête de ce
#                           fichier (e.g. BIP10VOLTS pour
#                           une plage de -10 à +10 volts)
#             value       : valeur en volts
#         
# SORTIE
#             o.k. ou un message d'erreur
#******************************************************************************
VOut<-function(bnum,chan,plage,value){
  err=.C("BP_cbVOut",BoardNum=as.integer(bnum), Channel=as.integer(chan), 
                Range=as.integer(plage), DataValue=as.single(value), out=as.integer(0))
  if (err$out!=0) return(paste("Échec sur sortie de voltage analogue sur le canal ", 
                          as.character(chan), " sur le module ", as.character(bnum),sep=""))
  else return("O.K.")
}


#******************************************************************************
# DIO_ON met à high un bit d'un port DIO définit en DIGITALOUT
#
# PARAMÈTRES
#             leport      : voir Config_Board pouf la définition de cette liste
#             lesDevices  : retourner par GetDevices
#         
# SORTIE
#             nil
#******************************************************************************
DIO_ON<-function(leport,lesDevices){
  lesnoms=lesDevices$noms[1:lesDevices$nbdevices]
  lesseries=lesDevices$serie[1:lesDevices$nbdevices]
  lesboards=lesDevices$BoardNum[1:lesDevices$nbdevices]
  device=leport[[1]]
  serial=leport[[2]]
  port=leport[[3]]
  porttype=leport[[5]]
  direction=eval(parse(text=leport[[4]]))
  for (k in 1:length(lesnoms)){
    if ((device %in% lesnoms[k]) & (serial %in% lesseries[k])){
      dum=strsplit(port,"_")[[1]]
      lebit=dum[[length(dum)]]
      BitOut(lesboards[k],porttype,as.numeric(lebit),1)
    }
  }
 
}


#******************************************************************************
# DIO_OFF met à low un bit d'un port DIO définit en DIGITALOUT
#
# PARAMÈTRES
#             leport      : voir Config_Board pouf la définition de cette liste
#             lesDevices  : retourner par GetDevices
#         
# SORTIE
#             nil
#******************************************************************************
DIO_OFF<-function(leport,lesDevices){
  lesnoms=lesDevices$noms[1:lesDevices$nbdevices]
  lesseries=lesDevices$serie[1:lesDevices$nbdevices]
  lesboards=lesDevices$BoardNum[1:lesDevices$nbdevices]
  device=leport[[1]]
  serial=leport[[2]]
  port=leport[[3]]
  direction=leport[[4]]
  porttype=leport[[5]]
  for (k in 1:length(lesnoms)){
    if ((device %in% lesnoms[k]) & (serial %in% lesseries[k])){
      dum=strsplit(port,"_")[[1]]
      lebit=dum[[length(dum)]]
      BitOut(lesboards[k],porttype,as.numeric(lebit),0)
    }
  }
  
}


#******************************************************************************
# DIO_IN lit la valeur d'un bit d'un port DIO définit en DIGITALIN
#
# PARAMÈTRES
#             leport      : voir Config_Board pouf la définition de cette liste
#             lesDevices  : retourner par GetDevices
#         
# SORTIE
#             valeur du bit (0=low, 1=high)
#******************************************************************************
DIO_IN<-function(leport,lesDevices){
  lesnoms=lesDevices$noms[1:lesDevices$nbdevices]
  lesseries=lesDevices$serie[1:lesDevices$nbdevices]
  lesboards=lesDevices$BoardNum[1:lesDevices$nbdevices]
  device=leport[[1]]
  serial=leport[[2]]
  port=leport[[3]]
  direction=leport[[4]]
  porttype=leport[[5]]
  for (k in 1:length(lesnoms)){
    if ((device %in% lesnoms[k]) & (serial %in% lesseries[k])){
      dum=strsplit(port,"_")[[1]]
      lebit=dum[[length(dum)]]
      return(BitIn(lesboards[k],porttype,as.numeric(lebit)))
    }
  }
  
}


#******************************************************************************
# V_IN retourne la valeur en volts d'un port analogique d'entrée
#
# PARAMÈTRES
#             leport      : voir Config_Board pouf la définition de cette liste
#             lesDevices  : retourner par GetDevices
#         
# SORTIE
#             valeur en volts lut sur le port.
#******************************************************************************
V_IN<-function(leport,lesDevices){
  lesnoms=lesDevices$noms[1:lesDevices$nbdevices]
  lesseries=lesDevices$serie[1:lesDevices$nbdevices]
  lesboards=lesDevices$BoardNum[1:lesDevices$nbdevices]
  device=leport[[1]]
  serial=leport[[2]]
  port=leport[[3]]
  lerange=leport[[4]]
  m=5
  b=0
  if (lerange==BIP10VOLTS){
    m=20
    b=-10
  }else if (lerange==UNI10VOLTS){
    m=10
    b=0
  }else if (lerange==UNI5VOLTS){
    m=5
    b=0
  }
  for (k in 1:length(lesnoms)){
    if ((device %in% lesnoms[k]) & (serial %in% lesseries[k])){
      dum=strsplit(port,"_")[[1]]
      lecanal=dum[[length(dum)]]
      bits=GetBitDepth(device)
      dum=AnalogIn(lesboards[k],lecanal,lerange)
      return(dum/(2^bits-1)*m+b)
    }
  }
}


#******************************************************************************
# V_SACN_IN retourne un vecteur de "count" valeurs en volts d'un port analogique d'entrée
# lut à au taux de "rate" Hz.
#
# PARAMÈTRES
#             leport      : voir Config_Board pouf la définition de cette liste
#             lesDevices  : retourner par GetDevices
#             rate        : taux d'échantillonnage (Hz)
#             count       : nombre de lectures désirées.
#         
# SORTIE
#             valeur en volts lut sur le port.
#******************************************************************************
V_SCAN_IN<-function(leport,lesDevices,rate,count){
  lesnoms=lesDevices$noms[1:lesDevices$nbdevices]
  lesseries=lesDevices$serie[1:lesDevices$nbdevices]
  lesboards=lesDevices$BoardNum[1:lesDevices$nbdevices]
  device=leport[[1]]
  serial=leport[[2]]
  port=leport[[3]]
  lerange=leport[[4]]
  m=5
  b=0
  if (lerange==BIP10VOLTS){
    m=20
    b=-10
  }else if (lerange==UNI10VOLTS){
    m=10
    b=0
  }else if (lerange==UNI5VOLTS){
    m=5
    b=0
  }
  for (k in 1:length(lesnoms)){
    if ((device %in% lesnoms[k]) & (serial %in% lesseries[k])){
      dum=strsplit(port,"_")[[1]]
      lecanal=dum[[length(dum)]]
      bits=GetBitDepth(device)
      dum=AnalogScanIn(lesboards[k],lerange,lecanal,lecanal,rate,count)
      return(dum/(2^bits-1)*m+b)
    }
  }
}

#******************************************************************************
# V_OUT envoie la valeur en volts d'un port analogique de sortie
#
# PARAMÈTRES
#             leport      : voir Config_Board pouf la définition de cette liste
#             lesDevices  : retourner par GetDevices
#             voltage     : voltage à appliquer à la sortie
#         
# SORTIE
#             valeur en volts à transmettre sur le port.
#******************************************************************************
V_OUT<-function(leport,lesDevices,voltage){
  lesnoms=lesDevices$noms[1:lesDevices$nbdevices]
  lesseries=lesDevices$serie[1:lesDevices$nbdevices]
  lesboards=lesDevices$BoardNum[1:lesDevices$nbdevices]
  device=leport[[1]]
  serial=leport[[2]]
  port=leport[[3]]
  lerange=leport[[4]]
  for (k in 1:length(lesnoms)){
    if ((device %in% lesnoms[k]) & (serial %in% lesseries[k])){
      dum=strsplit(port,"_")[[1]]
      lecanal=dum[[length(dum)]]
      dum=VOut(lesboards[k],lecanal,lerange,voltage)
    }
  }
  invisible(0)
}

#******************************************************************************
# I_OUT envoie la valeur en milliampères d'un port analogique de sortie
#
# PARAMÈTRES
#             leport      : voir Config_Board pouf la définition de cette liste
#             lesDevices  : retourner par GetDevices
#             courant     : courant en milliamps à appliquer à la sortie.
#         
# SORTIE
#             valeur en milliampères à transmettre sur le port.
#******************************************************************************
I_OUT<-function(leport,lesDevices,courant){
  lesnoms=lesDevices$noms[1:lesDevices$nbdevices]
  lesseries=lesDevices$serie[1:lesDevices$nbdevices]
  lesboards=lesDevices$BoardNum[1:lesDevices$nbdevices]
  device=leport[[1]]
  serial=leport[[2]]
  port=leport[[3]]
  lerange=leport[[4]]
  mini=0
  span=20
  if (lerange==MA0TO20){
    mini=0
    span=20
  } else if (lerange==MA4TO20){
    mini=4
    span=20
  }
  for (k in 1:length(lesnoms)){
    if ((device %in% lesnoms[k]) & (serial %in% lesseries[k])){
      dum=strsplit(port,"_")[[1]]
      lecanal=dum[[length(dum)]]
      bits=GetBitDepth(device)
      outval=(courant-mini)/span*(2^bits-1)      
      dum=AnalogOut(lesboards[k],lecanal,lerange,outval)
    }
  }
  invisible(0)
}
