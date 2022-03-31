  #***********************************************************************
  # P A R A M È T R E S    R A M A N 
  #///////////////////////////////////////////////////////////////////////

  
  #*********************************************************************
  # P A R A M Ê T R E S    P O U V A N T    Ê T R E    É D I T É S 
  # E N    C O U R S     D E     S E S S I O N     D' A C Q U I S I T I O N.
  #*********************************************************************
  EX_Raman=785.09  # Longueur d'onde du laser pour conversion en nombre d'onde (fichiers interpolés)
  T_Raman=5     # Temps d'intégration en secondes
  Box_Raman=1   # Boxcar pour le Raman
  Scans_Raman=1 # nombre de scans pour le Raman
  
  QE_Temp=-10   # température pour le refroidissement du QEPro
  
  P_Laser=400   # Puissance du laser en mW (Newport LS2)
  C_Laser=600   # Courant pour le laser en mAmp (IPS U-Type) 
  
  # Nombre de répétitions sur un même échantillon en changeant la position
  posReps_Raman=3
  
  #Paramètres pour la correction de la ligne de base
  do_raman_baseline=TRUE
  baseline_deg=7
  baseline_rep=50
  baseline_tol=0.005
  
  # T E M P S   D E   S T A B I L I S A T I O N    D U    L A S E R
  T_Laser=5
  
  
  
  #*********************************************************************
  # P A R A M Ê T R E S   N O N     M O D I F I A B L E S
  # E N    C O U R S     D E     S E S S I O N     D' A C Q U I S I T I O N.
  #*********************************************************************
  # Plage et intervalle de nombre d'onde pour l'interpolation
  ram_l_min=250   #nombre d'onde minimale
  ram_l_max=1600  #nombre d'onde maximale
  ram_step=1      #pas de nombre d'onde.
 
  
  
  
  
  
  
 
























