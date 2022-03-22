
  #*****************************************************************************
  # P A R A M È T R E S    R É F L E C T A N C E 
  #/////////////////////////////////////////////////////////////////////////////  
  T_Reflect=15    # temps d'exposition en msec
  Box_Reflect=4     # Boxcar pour l'acquisition
  Scans_Reflect=80   # nombre de scans par acquisition
  
  # Plage et intervalle de longueur d'onde pour l'interpolation
  reflect_l_min=325  #longueur d'onde minimale
  reflect_l_max=880  #longueur d'onde maximale
  reflect_step=1     #pas de longueur d'onde.
  
  stray_low=200      #Début de plage de longueur d'onde pour évaluer stray light
  stray_high=230     #fin de plage de longueur d'onde pour évaluer stray light

  # Nombre de répétitions sur un même échantillon en changeant la position
  posReps_Reflect=1