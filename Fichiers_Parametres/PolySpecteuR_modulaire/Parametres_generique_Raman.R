  #************************************************************************************  
  # P A R A M È T R E S    R A M A N 
  #///////////////////////////////////////////////////////////////////////////////////

  EX_Raman=784.56  # Longueur d'onde du laser pour conversion en nombre d'onde (fichiers interpolés)
  T_Raman=0.2     # Temps d'intégration en secondes
  Box_Raman=1   # Boxcar pour le Raman
  Scans_Raman=1 # nombre de scans pour le Raman
  QE_Temp=-10   # température pour le refroidissement du QEPro
  P_Laser=400   # Puissance du laser en mW (Newport LS2)
  C_Laser=250   # Courant pour le laser en mAmp (IPS U-Type)
  
  # Plage et intervalle de nombre d'onde pour l'interpolation
  ram_l_min=250   #nombre d'onde minimale
  ram_l_max=1600  #nombre d'onde maximale
  ram_step=1      #pas de nombre d'onde.
  
  # Nombre de répétitions sur un même échantillon en changeant la position
  posReps_Raman=1
  
  #Paramètres pour la correction de la ligne de base
  do_raman_baseline=TRUE
  baseline_deg=7
  baseline_rep=50
  baseline_tol=0.005
  
 
  #///////////////////////////////////////////////////////////////////////////////////
  # T E M P S   D E   S T A B I L I S A T I O N    D U    L A S E R
  T_Laser=5
  
  
  
  
  
  
 
























