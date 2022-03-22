 #************************************************************************************
  #************************************************************************************
  #---------------------------------E N T ï¿½ T E----------------------------------------
  #
  #///////////////////////////////////////////////////////////////////////////////////
  # F I C H I E R S    D E    C O R R E C T I O N (ï¿½talonnage en Y)
  Maya_Calib_File="C:/Users/crdaspectral/Documents/Programmes/SAIV_Version_Alain_2016/Calib_Data/HS3_corrY.txt"
  
  #///////////////////////////////////////////////////////////////////////////////////    
  # S P E C T R O S   ET  M O D U L E   MC-USB
  les_spectro=list()
  les_mcusb=list()
  les_spectro$name="USB2000+"
  les_spectro$serial="FLMS04441"
  les_mcusb$name="USB-3106"
  les_mcusb$serial="110154"
  
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
  SB375=list(mc_mod,USB_Ser,"IOUT_8",MA0TO20)
  SB390=list(mc_mod,USB_Ser,"IOUT_10",MA0TO20)
  SB405=list(mc_mod,USB_Ser,"IOUT_12",MA0TO20)
  SB430=list(mc_mod,USB_Ser,"IOUT_14",MA0TO20)
  SBWhite=list(mc_mod,USB_Ser,"IOUT_9",MA0TO20)
  SBLP1=list(mc_mod,USB_Ser,"VOUT_11",UNI10VOLTS)
  SBLP2=list(mc_mod,USB_Ser,"VOUT_13",UNI10VOLTS)
  Shutter=list(mc_mod,USB_Ser,"DIO_0",DIGITALOUT,AUXPORT)
  
  #Faire une liste avec toutes les connections
  lesPorts=list(FEX1,FEX2,FEX3,FEX4,FEX5,FEX6,FEX7,FEX8,
                SB375,SB390,SB405,SB430,SBWhite,SBLP1,SBLP2,
                Shutter)
  
  lesports_amp=c(0,1,2,3,4,5,6,7)   #0 20 mA
  lesports_V=c(11,13) #0-10 volts
  
  
  #___________________________________________________________________________
  # Courants et voltage pour les composantes de la source blanche
  #___________________________________________________________________________
  I375=9.4    #amps
  I390=9.7
  I405=2.9
  I430=3.1
  IWhite=1.4
  VTungsten1=5  #volts
  
  T_LampeBL=2   # temps de stabilisation en secondes
  
  
  #Liste des éléments de la source blanche contrôlés par un courant et un voltage
  #Chaque élément de la liste contient la liste qui définit le port et la valeur de
  #voltage ou du courant selon le cas.
  SB_amps <- list(list(SB375,I375),list(SB390,I390),
                  list(SB405,I405),list(SB430,I430),
                  list(SBWhite,IWhite))
  SB_volts <- list(SBLP1,VTungsten1)

  #___________________________________________________________________________
  #Définir les paramètres d'acquisition pour la vérification périodique
  #___________________________________________________________________________________
  Instr_reps=2
  Instr_T_exp_test=c(c(2,2,2,2,2,1,1,3)) #pour les DELs 1 à 8 respectivement, msec
  Instr_N_scans_test= c(500,500,500,500,500,1000,1000,333) #pour les DELs 1 à 8 respectivement
  Instr_Boxcar_test = c(1,1,1,1,1,1,1,1) #pour les DELs 1 à 8 respectivement
  FluoDel_I_test= c(5,6,10,6,5,2,2,8)
  Instrument_Verif_File="~/Programmes/SAIV_Version_Alain_2016/Progs/R_Progs_Avril2017/Test_n_Demo_Code/Fichiers_Instruments/SAAC2_HS3_Verif.txt"
  
  
  #_____________________________________________________________________________
  #Paramètres pour la longueur d'onde d'observation pour la normalisation de la fluorescence
  #_____________________________________________________________________________
  #Le programme va rechercher le pic entre Norm_Y_peak-(Norm_Y_BW/2) et Norm_Y_peak+(Norm_Y_BW/2)
  Norm_Y_peak = 659 #longueur d'onde centrale
  Norm_Y_BW = 5    #largeur de la bande en nm


















