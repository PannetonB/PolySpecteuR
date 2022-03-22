  #///////////////////////////////////////////////////////////////////////////////////
  # F I C H I E R S    D E    C O R R E C T I O N (ï¿½talonnage en Y et ï¿½talonnage de la transmittance)
  Trans_Calib_File="../../../Calib_Data/Calib_Blanc_Transmit_ones.txt"
  Maya_Calib_File="../../../Calib_Data/Gain_Y_Maya_mars18.txt"
  QEPro_Calib_File="../../../Calib_Data/Calib_QE_pour_Raman_jan2018.txt"
  
  #///////////////////////////////////////////////////////////////////////////////////
  # S ï¿½ Q U E N C E    D' A C Q U I S I T I O N (TRUE ou FALSE)
  DoFluo=FALSE
  DoTransmit=FALSE
  DoRaman=TRUE
  DoTurbi=FALSE
  
  #///////////////////////////////////////////////////////////////////////////////////
  # P A R A M ï¿½ T R E S    F L U O R E S C E N C E
  #
  # Dï¿½finition par position (1 ï¿½ 6). Le numï¿½ro de la position augmente dans le sens des
  # aiguilles d'un montre quand on regarde la couronne de DELs de face. La position 1
  # est vers en haut ï¿½ gauche. Voir Position 1 pour la dï¿½finition des paramï¿½tres.
  # RangFluo donne l'ordre d'acquisition. Par exemple:
  # RangFluo=c(2,1,3,5,4,6) fait que on fait EX2 puis EX1 puis EX3 puis EX5 puis EX4 puis EX6
  # Il est important que le vecteur ait 6 valeurs, que ces valeurs soient entre 1 et 6 et 
  # que toutes les valeurs entre 1 et 6 soient prï¿½sentes.
  RangFluo=c(1,3,6,4,5,2)
  
  # Position 1
  DoEX1=TRUE    # TRUE - on prend la fluo ï¿½ cette longueur d'onde d'excitation
  EX1=418       # Longueur d'onde de la DEL
  T_EX1=100      # temps d'exposition en msec
  Box_EX1=1     # Boxcar pour l'acquisition
  Scans_EX1=5   # nombre de scans par acquisition
  EX1_LED_I=10  # Intensitï¿½ de la DEL (mamp)
  
  # Position 2
  DoEX2=TRUE    # TRUE - on prend la fluo ï¿½ cette longueur d'onde d'excitation
  EX2=300       # Longueur d'onde de la DEL
  T_EX2=1000      # temps d'exposition en msec
  Box_EX2=1     # Boxcar pour l'acquisition
  Scans_EX2=2   # nombre de scans par acquisition
  EX2_LED_I=20  # Intensitï¿½ de la DEL (mamp)
  
  # Position 3
  DoEX3=TRUE    # TRUE - on prend la fluo ï¿½ cette longueur d'onde d'excitation
  EX3=348       # Longueur d'onde de la DEL
  T_EX3=100      # temps d'exposition en msec
  Box_EX3=1     # Boxcar pour l'acquisition
  Scans_EX3=5   # nombre de scans par acquisition
  EX3_LED_I=10  # Intensitï¿½ de la DEL (mamp)
  
  # Position 4
  DoEX4=TRUE    # TRUE - on prend la fluo ï¿½ cette longueur d'onde d'excitation
  EX4=361      # Longueur d'onde de la DEL
  T_EX4=1000      # temps d'exposition en msec
  Box_EX4=1     # Boxcar pour l'acquisition
  Scans_EX4=2   # nombre de scans par acquisition
  EX4_LED_I=20  # Intensitï¿½ de la DEL (mamp)
  
  # Position 5
  DoEX5=TRUE    # TRUE - on prend la fluo ï¿½ cette longueur d'onde d'excitation
  EX5=320       # Longueur d'onde de la DEL
  T_EX5=1000      # temps d'exposition en msec
  Box_EX5=1     # Boxcar pour l'acquisition
  Scans_EX5=2   # nombre de scans par acquisition
  EX5_LED_I=20  # Intensitï¿½ de la DEL (mamp)
  
  # Position 6
  DoEX6=TRUE    # TRUE - on prend la fluo ï¿½ cette longueur d'onde d'excitation
  EX6=395       # Longueur d'onde de la DEL
  T_EX6=100      # temps d'exposition en msec
  Box_EX6=1     # Boxcar pour l'acquisition
  Scans_EX6=5   # nombre de scans par acquisition
  EX6_LED_I=20  # Intensitï¿½ de la DEL (mamp)
  
  # Plage et intervalle de longueur d'onde pour l'interpolation
  fluo_l_min=250  #longueur d'onde minimale
  fluo_l_max=800  #longueur d'onde maximale
  fluo_step=1     #pas de longueur d'onde.
  
  #///////////////////////////////////////////////////////////////////////////////////
  # P A R A M ï¿½ T R E S    R A M A N 
  EX_Raman=784.56  # Longueur d'onde du laser pour conversion en nombre d'onde (fichiers interpolï¿½s)
  T_Raman=2    # Temps d'intï¿½gration en secondes
  Box_Raman=1   # Boxcar pour le Raman
  Scans_Raman=1 # nombre de scans pour le Raman
  QE_Temp=-10   # tempï¿½rature pour le refroidissement du QEPro
  P_Laser=400   # Puissance du laser en mW
  
  # Plage et intervalle de nombre d'onde pour l'interpolation
  ram_l_min=250   #nombre d'onde minimale
  ram_l_max=1600  #nombre d'onde maximale
  ram_step=1      #pas de nombre d'onde.
  
  #Parametres pour la correction de la ligne de base
  do_raman_baseline=TRUE
  baseline_deg=7
  baseline_rep=50
  baseline_tol=0.005
  
  #///////////////////////////////////////////////////////////////////////////////////
  # P A R A M ï¿½ T R E S    T R A N S M I T T A N C E
  # Courants et voltage pour les composantes de la source blanche
  I370=9.4
  I380=9.7
  I395=3.2
  I405=2.9
  I420=6.1
  I435=3.1
  I490=6.5
  IWhite=1.4
  ITungsten1=5
  
  #Spectro
  T_Transm=30      # temps d'intï¿½gration en msec
  Box_Transm=4     # boxcar 
  Scans_Transm=35  # nombre de scans
  
  # Plage et intervalle de longueur d'onde pour l'interpolation
  transmit_l_min=360  #longueur d'onde minimale
  transmit_l_max=1100  #longueur d'onde maximale
  transmit_step=1     #pas de longueur d'onde.
  
  #///////////////////////////////////////////////////////////////////////////////////
  # P A R A M ï¿½ T R E S    T U R B I D I T ï¿½
  # Chaque photodiode est lue "Scans_Transm" fois ï¿½ un rythme de "Freq_Transm" hertz
  Scans_Turb=40 # nombre de lectures rï¿½pï¿½tï¿½es d'une photodiode
  Freq_Turb=100 # frï¿½quence en hertz pour la lecture des photodiodes
  
  #///////////////////////////////////////////////////////////////////////////////////
  # T E M P S   D E   S T A B I L I S A T I O N
  T_DEL=0.5   #secondes
  T_Laser=5
  T_LampeBL=2
  
  #///////////////////////////////////////////////////////////////////////////////////
  #___________________________________________________________________________
  #Définir les paramètres d'acquisition pour la vérification périodique
  #___________________________________________________________________________________
  Instr_reps=2
  Instr_T_exp_test=c(200,1000,800,1000,1000,250) #pour les DELs 1 à 6 respectivement, msec
  Instr_N_scans_test= c(8,3,4,3,3,7) #pour les DELs 1 à 6 respectivement
  Instr_Boxcar_test = c(5,5,5,5,5,5) #pour les DELs 1 à 6 respectivement
  FluoDel_I_test= c(20,20,20,20,20,20)
  Instrument_Verif_File="Fichiers_Instruments/SAAC1_Verif.txt"
  
  
  
  
 
























