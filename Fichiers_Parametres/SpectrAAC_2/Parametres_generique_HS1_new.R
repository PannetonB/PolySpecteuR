  #************************************************************************************
  #************************************************************************************
  #
  #///////////////////////////////////////////////////////////////////////////////////
  # S É Q U E N C E    D' A C Q U I S I T I O N (TRUE ou FALSE)
  DoFluo=FALSE
  Nb_reps_Fluo=1
  DoReflect=TRUE
  Nb_reps_Reflect=Nb_reps_Fluo  #ATTENTION - NE PAS MODIFIER - MÊME NOMBRE DE REPS
                                #POUR FLUO ET RÉFLECTANCE OBLIGATOIRE!
  
  #///////////////////////////////////////////////////////////////////////////////////    
  # P A R A M È T R E S    F L U O R E S C E N C E  
  #
  # Définition par position (1 à 8). Le numéro de la position augmente dans le sens des
  # aiguilles d'un montre quand on regarde la couronne de DELs de face. La position 1
  # est vers en haut à gauche. Voir Position 1 pour la définition des paramètres.
  # RangFluo donne l'ordre d'acquisition. Par exemple:
  # RangFluo=c(2,1,3,5,4,6,8,7) fait que on fait EX2 puis EX1 puis EX3 puis EX5 puis EX4 puis EX6 puis EX8 puis EX7
  # Il est important que le vecteur ait 8 valeurs, que ces valeurs soient entre 1 et 8 et 
  # que toutes les valeurs entre 1 et 8 soient présentes.
  RangFluo=c(7,6,5,4,3,2,1)
  Delai_noir=2  #Délai avant de reprendre un noir en minutes.
  
  # Position 1 (identifié zéro dans l'appareil)
  DoEX1=TRUE    # TRUE - on prend la fluo à cette longueur d'onde d'excitation
  EX1=279       # Longueur d'onde de la DEL
  T_EX1=30     # temps d'exposition en msec
  Box_EX1=1     # Boxcar pour l'acquisition
  Scans_EX1=40   # nombre de scans par acquisition
  LED_I_EX1=13.5  # Intensité de la DEL (mamp)
  
  # Position 2
  DoEX2=TRUE    # TRUE - on prend la fluo à cette longueur d'onde d'excitation
  EX2=322       # Longueur d'onde de la DEL
  T_EX2=30      # temps d'exposition en msec
  Box_EX2=1     # Boxcar pour l'acquisition
  Scans_EX2=40   # nombre de scans par acquisition
  LED_I_EX2=10.2  # Intensité de la DEL (mamp)
  
  # Position 3
  DoEX3=TRUE    # TRUE - on prend la fluo à cette longueur d'onde d'excitation
  EX3=341       # Longueur d'onde de la DEL
  T_EX3=15      # temps d'exposition en msec
  Box_EX3=1     # Boxcar pour l'acquisition
  Scans_EX3=100   # nombre de scans par acquisition
  LED_I_EX3=11.22  # Intensité de la DEL (mamp)
  
  # Position 4
  DoEX4=TRUE    # TRUE - on prend la fluo à cette longueur d'onde d'excitation
  EX4=361       # Longueur d'onde de la DEL
  T_EX4=15     # temps d'exposition en msec
  Box_EX4=1     # Boxcar pour l'acquisition
  Scans_EX4=100   # nombre de scans par acquisition
  LED_I_EX4=9.89  # Intensité de la DEL (mamp)
  
  # Position 5
  DoEX5=TRUE    # TRUE - on prend la fluo à cette longueur d'onde d'excitation
  EX5=395       # Longueur d'onde de la DEL
  T_EX5=5     # temps d'exposition en msec
  Box_EX5=1     # Boxcar pour l'acquisition
  Scans_EX5=200  # nombre de scans par acquisition
  LED_I_EX5=6.37  # Intensité de la DEL (mamp)
  
  # Position 6
  DoEX6=TRUE    # TRUE - on prend la fluo à cette longueur d'onde d'excitation
  EX6=422       # Longueur d'onde de la DEL
  T_EX6=5     # temps d'exposition en msec
  Box_EX6=1     # Boxcar pour l'acquisition
  Scans_EX6=200   # nombre de scans par acquisition
  LED_I_EX6=4.83  # Intensité de la DEL (mamp)
  
  # Position 7
  DoEX7=TRUE    # TRUE - on prend la fluo à cette longueur d'onde d'excitation
  EX7=443       # Longueur d'onde de la DEL
  T_EX7=5     # temps d'exposition en msec
  Box_EX7=1     # Boxcar pour l'acquisition
  Scans_EX7=200   # nombre de scans par acquisition
  LED_I_EX7=1.6  # Intensité de la DEL (mamp)
  
  # Plage et intervalle de longueur d'onde pour l'interpolation
  fluo_l_min=240  #longueur d'onde minimale
  fluo_l_max=800  #longueur d'onde maximale
  fluo_step=1     #pas de longueur d'onde.
  
  
  # T E M P S   D E   S T A B I L I S A T I O N
  T_DEL=0.8   #secondes
  
  #************************************************************************************
  #///////////////////////////////////////////////////////////////////////////////////    
  # P A R A M È T R E S    R É F L E C T A N C E 
  #************************************************************************************
  T_Reflect=15    # temps d'exposition en msec
  Box_Reflect=4     # Boxcar pour l'acquisition
  Scans_Reflect=80   # nombre de scans par acquisition

  # Plage et intervalle de longueur d'onde pour l'interpolation
  reflect_l_min=325  #longueur d'onde minimale
  reflect_l_max=880  #longueur d'onde maximale
  reflect_step=1     #pas de longueur d'onde.
  
  stray_low=200      #é?but de la plage de longueur d'onde pour évaluer stray light
  stray_high=230     #fin de la plage de longueur d'onde pour évaluer stray light






















































