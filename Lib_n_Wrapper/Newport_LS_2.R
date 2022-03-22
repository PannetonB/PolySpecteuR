#Définition de variables globales
#Délai après ouverture ou fermeture DELs
L0_1<<-784.56   #Longueur du laser 1 - Newport LS-2 S/N: NLS2-1003
L0_2<<-785.40   #Longueur du laser 2 - Newport LS-2 S/N: NLS2-1003
debug_LS2=FALSE

ok=require(tcltk)       #Pour accéder au RS232
if (!ok){
  install.packages("tcltk")
  require(tcltk)
} 
ok=require(stringr)  #POur manipulation de chaînes de caractères
if (!ok){
  install.packages("stringr")
  require(stringr)
} 

ok=require(signal)  #POur manipulation de chaînes de caractères
if (!ok){
  install.packages("signal")
  require(signal)
} 

ok=require(rstudioapi)  #POur manipulation de chaînes de caractères
if (!ok){
  install.packages("rstudioapi")
  require(rstudioapi)
} 


#*****************************************************************************
Connect_LS_2<-function(LasNumber){
  #LasNumber permet de choisir le laser 1 ou le laser 2. La fonction retourne 
  #ce numéro.
  #Connecte au port RS232 pour les DELs
  #Obtenir une liste des ports et demander à l'usager de choisir
  dum=system("mode",intern=TRUE)
  dum=iconv(dum,"850","UTF-8")
  dd=str_match(dum,"COM([0-9]+)")
  ind=grep("COM",dum)
  if (ind[1]==0) return("PAS DE PORT COM DISPONIBLE. LS-2 NE SEMBLE PAS ÊTRE RACCORDÉ")
  listports=dd[ind,2]
  
  
  leport <- utils::select.list(choices=c("Aucun",listports),
                               title = "Port pour LS-2.",
                               graphics = T)
  
  #Open port com
  nom_port<<-"le_com"
  #Le truc c'est d'essayer et de voir si une erreur est retournée.
  test=data.class(result<-try(.Tcl(paste('set ', nom_port,
                                         ' [open "//./com', leport, '" r+]',sep="")),TRUE))=="try-error"
  if (test){
    rstudioapi::showDialog("ATTENTION!","<b>Pas de LS-2 d&eacute;tect&eacute;</b>")
    return("ABANDON")
  }else
  {
    #Configuration du port
    .Tcl('fconfigure $le_com -mode "19200,n,8,1" -buffering none -blocking 0 -handshake xonxoff')
    #Delay for port configuration to complete
    Sys.sleep(2)
    
    To_LS2('SLAS1')     #force laser 1 au mode APC
    To_LS2('SAPC')
    To_LS2('SLAS1')
    #force la valeur initiale de la puissance 
    if (LasNumber==1) To_LS2(paste("SPR",as.character(200),sep=""))
    else To_LS2(paste("SPR",as.character(0),sep=""))
    To_LS2('SLAS2')     #force laser 1 au mode APC
    To_LS2('SAPC')
    To_LS2('SLAS2')
    #force la valeur initiale de la puissance  
    if (LasNumber==2) To_LS2(paste("SPR",as.character(200),sep=""))
    else To_LS2(paste("SPR",as.character(0),sep=""))
  }
  return("OK")
}


#********************************************************
To_LS2<-function(commande)
  #Envoie une commande au LS2 et reçoit l'output
  #Les lignes débutant par "cat" aide au déboggage en faisant
  #un écho des commandes envoyées et de la réponse reçue.
  #Quand tout marche bien, le dernier caractère retourné par
  #l'instrument et ">" seul sur une ligne. Si c'est pas le cas,
  #il y a un problème de communication même si certaines fonctions
  #semblent s'exécuter.
  #On ne gère pas les erreurs. Si l'appareil ne reconnaît pas une
  #commande, il retourne "E0".
{
  if (debug_LS2) cat(paste("\nCommande au LS2: ",commande, "\n",sep=""))
  dum<-tclvalue(.Tcl('read $le_com'))  #vide le buffer
  Sys.sleep(0.05)
  .Tcl(paste("puts -nonewline $le_com ", commande, "\\r",sep=""))#Envoi 
  Sys.sleep(0.05)    #Délai de 50 msec
  dum<-tclvalue(.Tcl('read $le_com'))  #récupère la réponse du buffer
  if (debug_LS2) cat(paste("Réponse du LS2 ? ",commande, ": ",dum,"\n",sep=""))
  Sys.sleep(0.05)
  dum=unlist(strsplit(dum,"\n"))
  return(dum[2]) #le premier élément de dum est l'écho de la commande. 
  #On ne gère pas les erreurs signifiées par dum[2]=="E0"
}


#********************************************************
LaserOn<-function(LasNum,laserPwr){
  k=as.character(LasNum)
  To_LS2(paste("SLAS",k,sep="")) #Select laser
  To_LS2(paste("SLA",k,sep=""))  #Set laser as the active one
  To_LS2(paste("SLAS",k,sep="")) #Select laser
  To_LS2(paste('SPR',as.character(laserPwr),sep="")) #Set laser power
  To_LS2(paste("SLAS",k,sep="")) #Select laser
  To_LS2('SST600001')            #Set shutter time to INDEF
  To_LS2(paste("SLAS",k,sep="")) #Select laser
  To_LS2('SALO')                 #Turn laser on  
}


#********************************************************
LaserPower<-function(LasNum,laserPwr){
  To_LS2(paste("SLAS",as.character(LasNum),sep="")) #Select laser
  To_LS2(paste("SLA",as.character(LasNum),sep=""))  #Set laser as the active one
  To_LS2(paste("SLAS",as.character(LasNum),sep="")) #Select laser
  To_LS2(paste('SPR',as.character(laserPwr),sep="")) #Set laser power
}


#********************************************************
LaserOff<-function(LasNum){
  k=as.character(LasNum)
  To_LS2(paste("SLAS",k,sep="")) #Select laser
  To_LS2(paste("SLA",k,sep=""))  #Set laser as the active one
  To_LS2(paste("SLAS",k,sep="")) #Select laser
  To_LS2('SALS')                 #Stop laser
}


#********************************************************
ShutterOn<-function(LasNum){
  k=as.character(LasNum)
  To_LS2(paste("SLAS",k,sep="")) #Select laser
  To_LS2(paste("SLA",k,sep=""))  #Set laser as the active one
  To_LS2(paste("SLAS",k,sep="")) #Select laser
  To_LS2("SSHO")                 #ouverture du shutter
  #Sys.sleep(1)                  #délai de synchro
}


#********************************************************
ShutterOff<-function(LasNum){
  k=as.character(LasNum)
  #Ferme shutter
  To_LS2(paste("SLAS",k,sep=""))
  To_LS2("SSHC")
  #Sys.sleep(1)                   #délai de synchro
}

#********************************************************
Close_LS2<-function()
{
  ShutterOff(1)
  ShutterOff(2)
  LaserOff(1)
  LaserOff(2)
  dum<-.Tcl('close $le_com')  #Ferme port série du LS2
}
