#Attention - Set Working Directory to source file location!
#setwd("~/Utilitaires_R_4_Robot/C")
system("R CMD SHLIB MC_cbw64_CWrapper.c -L. cbw64.dll")
