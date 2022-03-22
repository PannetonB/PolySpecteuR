focusToPlots <- function()
#*******************************************************************************
#*******************************************************************************
# Dans RStudio, script qui permet de sélectionner l'onglet Plots
#*******************************************************************************
# AUTEUR: Bernard Panneton, Agriculture et Agroalimentaire Canada
# Février 2022
#*******************************************************************************
#*******************************************************************************  
{
  ok <- require(KeyboardSimulator)
  if (!ok) install.packages("KeyboardSimulator", dependencies = T)
  KeyboardSimulator::keybd.press("Ctrl+6")
}