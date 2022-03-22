 #************************************************************************************
  #************************************************************************************
  nomInstrument="HS1"
  #
  #///////////////////////////////////////////////////////////////////////////////////
  # F I C H I E R S    D E    C O R R E C T I O N (Étalonnage en Y)
  Maya_Calib_File="Calib_Data/HS1_corrY.txt"
  
  #///////////////////////////////////////////////////////////////////////////////////    
  # S P E C T R O S   ET  M O D U L E   MC-USB
  les_spectro=list()
  les_mcusb=list()
  les_spectro$name="USB2000+"
  les_spectro$serial="FLMS12786"
  les_mcusb$name="USB-3104"
  les_mcusb$serial="111036"
  
  #___________________________________________________________________________
  #Définir les ports
  #___________________________________________________________________________________
  mc_mod=les_mcusb$name 
  USB_Ser=les_mcusb$serial
  FEX1=list(mc_mod,USB_Ser,"IOUT_0",MA0TO20)
  FEX2=list(mc_mod,USB_Ser,"IOUT_1",MA0TO20)
  FEX3=list(mc_mod,USB_Ser,"IOUT_2",MA0TO20)
  FEX4=list(mc_mod,USB_Ser,"IOUT_3",MA0TO20)
  FEX5=list(mc_mod,USB_Ser,"IOUT_4",MA0TO20)
  FEX6=list(mc_mod,USB_Ser,"IOUT_5",MA0TO20)
  FEX7=list(mc_mod,USB_Ser,"IOUT_6",MA0TO20)
  FEX8=list(mc_mod,USB_Ser,"IOUT_7",MA0TO20)

  
  
  #Faire une liste avec toutes les connections
  lesPorts=list(FEX1,FEX2,FEX3,FEX4,FEX5,FEX6,FEX7,FEX8)
  
  #Liste des ports de sortie en courant
  lesports_amp=c(0,1,2,3,4,5,6,7)   #0 20 mA
  
  #Liste des ports de sortie en voltage
  #lesports_V=c(11,13) #0-10 volts
  
  
  #___________________________________________________________________________
  #Définir les paramètres d'acquisition pour la vérification périodique
  #___________________________________________________________________________________
  Instr_reps = 2  # nombre de reps pour mesure du facteur de normalisation
  
  Instr_T_exp_test = list(          
    HRU = c(2,2,2,2,2,1,1,2),
    STA6 = c(200,200,100,150,150,75,75,100)
  ) #pour les DELs 1 à 8 respectivement, msec  
  
  Instr_N_scans_test = list(
    HRU = c(500,500,500,1000,500,1000,500,500),
    STA6 = c(10,10,20,15,15,15,15,20)
  )  #pour les DELs 1 à 8 respectivement
  
  Instr_Boxcar_test = list(
    HRU = c(1,1,1,1,1,1,1,1),
    STA6 = c(10,10,10,10,10,10,10,10)
  )   #pour les DELs 1 à 8 respectivement
  
  FluoDel_I_test = list(
    HRU = c(5,6,10,6,5,2,2,8),
    STA6 = c(20,20,20,20,20,20,20,20)
  ) #pour les DELs 1 à 8 respectivement, mamp
  
  Instrument_Verif_File="Fichiers_Instruments/SAAC2_HS1_Verif.txt"
  
  
  #_____________________________________________________________________________
  #Paramètres pour la longueur d'onde d'observation pour la normalisation de la fluorescence
  #_____________________________________________________________________________
  #Le programme va rechercher le pic entre Norm_Y_peak-(Norm_Y_BW/2) et Norm_Y_peak+(Norm_Y_BW/2)
  
  #longueur d'onde centrale
  Norm_Y_peak = list(
    HRU = 659,
    STA6 = 572
  )  
  
  #largeur de la bande en nm
  Norm_Y_BW = list(
    HRU = 5,
    STA6 = 5
  )


















