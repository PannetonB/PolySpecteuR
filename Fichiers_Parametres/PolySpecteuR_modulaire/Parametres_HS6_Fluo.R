  #************************************************************************************
  # P A R A M È T R E S    F L U O
  #///////////////////////////////////////////////////////////////////////////////////
  # Nombre de répértitions en changeant la position de l'échantillon
  posReps_Fluo=1
  
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
  Delai_noir=20  #Délai avant de reprendre un noir en minutes.
  
  # Position 1 (identifié zéro dans l'appareil)
  DoEX1=TRUE    # TRUE - on prend la fluo à cette longueur d'onde d'excitation
  EX1=280       # Longueur d'onde de la DEL
  T_EX1=100     # temps d'exposition en msec
  Box_EX1=1     # Boxcar pour l'acquisition
  Scans_EX1=5   # nombre de scans par acquisition
  LED_I_EX1=20.0  # Intensité de la DEL (mamp)
  STD_EX1="STA6"  # standard de normalisation associé
  
  # Position 2
  DoEX2=TRUE    # TRUE - on prend la fluo à cette longueur d'onde d'excitation
  EX2=320       # Longueur d'onde de la DEL
  T_EX2=100      # temps d'exposition en msec
  Box_EX2=1     # Boxcar pour l'acquisition
  Scans_EX2=5   # nombre de scans par acquisition
  LED_I_EX2=20.0  # Intensité de la DEL (mamp)
  STD_EX2="STA6"  # standard de normalisation associé
  
  # Position 3
  DoEX3=TRUE    # TRUE - on prend la fluo à cette longueur d'onde d'excitation
  EX3=340       # Longueur d'onde de la DEL
  T_EX3=100      # temps d'exposition en msec
  Box_EX3=1     # Boxcar pour l'acquisition
  Scans_EX3=5   # nombre de scans par acquisition
  LED_I_EX3=20.0  # Intensité de la DEL (mamp)
  STD_EX3="STA6"  # standard de normalisation associé
  
  # Position 4
  DoEX4=TRUE    # TRUE - on prend la fluo à cette longueur d'onde d'excitation
  EX4=355       # Longueur d'onde de la DEL
  T_EX4=100     # temps d'exposition en msec
  Box_EX4=1     # Boxcar pour l'acquisition
  Scans_EX4=5   # nombre de scans par acquisition
  LED_I_EX4=20.0  # Intensité de la DEL (mamp)
  STD_EX4="STA6"  # standard de normalisation associé
  
  # Position 5
  DoEX5=TRUE    # TRUE - on prend la fluo à cette longueur d'onde d'excitation
  EX5=375       # Longueur d'onde de la DEL
  T_EX5=75     # temps d'exposition en msec
  Box_EX5=1     # Boxcar pour l'acquisition
  Scans_EX5=5  # nombre de scans par acquisition
  LED_I_EX5=20.0  # Intensité de la DEL (mamp)
  STD_EX5="STA6"  # standard de normalisation associé
  
  # Position 6
  DoEX6=TRUE  #TRUE - on prend la fluo à cette longueur d'onde d'excitation
  EX6=400       # Longueur d'onde de la DEL
  T_EX6=25      # temps d'exposition en msec
  Box_EX6=1     # Boxcar pour l'acquisition
  Scans_EX6=5   # nombre de scans par acquisition
  LED_I_EX6=20.0  # Intensité de la DEL (mamp)
  STD_EX6="STA6"  # standard de normalisation associé
  
  # Position 7
  DoEX7=TRUE    # TRUE - on prend la fluo à cette longueur d'onde d'excitation
  EX7=420       # Longueur d'onde de la DEL
  T_EX7=25     # temps d'exposition en msec
  Box_EX7=1     # Boxcar pour l'acquisition
  Scans_EX7=5   # nombre de scans par acquisition
  LED_I_EX7=20.0  # Intensité de la DEL (mamp)
  STD_EX7="STA6"  # standard de normalisation associé
  
  
  # Plage et intervalle de longueur d'onde pour l'interpolation
  fluo_l_min=240  #longueur d'onde minimale
  fluo_l_max=800  #longueur d'onde maximale
  fluo_step=1     #pas de longueur d'onde.
  
  
  # T E M P S   D E   S T A B I L I S A T I O N
  T_DEL=0.8   #secondes
  
  




















































