Clean_n_Close <- function(lesInstr)
#****************************************************************************
# Fonction qui permet de finir une session d'acquisition proprement 
#****************************************************************************
#
#****************************************************************************
# AUTEUR: Bernard Panneton, Agriculture et Agroalimentaire Canada
# Février 2022
#****************************************************************************
{
 lestypes <- unlist(lapply(lesInstr, function(inst) inst$type))
 
 #Vérifier si Ramans pour éteindre le laser----
 lesRamans <- which(lestypes=="Raman")
 if (length(lesRamans)>0){
   for (rams in lesRamans){
     with(lesInstr[[rams]],
          {
              if (sourceLaser=="LS2"){
                LaserOff(1)
                ShutterOff(1)
                LaserPower(1,0)
                Close_LS2(1)
             }
             if (sourceLaser=="IPS"){
                ShutterOff(1)
                LaserOff(1)
                Close_IPS()
             }
          }
     )
   }
   
 }
 
 #Fermer la library pour MCDAQ au besoin----
 lesMCDAQ <- which(lestypes=="Fluorescence")
 if (length(lesMCDAQ)>0) 
   Quitte_MCLIB()
 

#Ferme les spectros----
OOobj$mywrap$closeAllSpectrometers() 
if (exists("OOobj", envir = .GlobalEnv))  rm(OOobj, envir=.GlobalEnv)
gc()
 
# END ----  
}