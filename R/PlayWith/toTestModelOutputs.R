

#Run sections in Inst_4_modelTesting.R

source("~/Consultant/AAC_2022/Progs/RProjets/PolySpecteuR/R/GetPlanExp.R", echo=TRUE)
source("~/Consultant/AAC_2022/Progs/RProjets/PolySpecteuR/R/Define_Descript.R", echo=TRUE)
source("~/Consultant/AAC_2022/Progs/RProjets/PolySpecteuR/R/PickFromPlan.R", echo=TRUE)
source("~/Consultant/AAC_2022/Progs/RProjets/PolySpecteuR/R/InitModels.R", echo=TRUE)
source("~/Consultant/AAC_2022/Progs/RProjets/PolySpecteuR/R/ApplyModels.R", echo=TRUE)


modelEnv <- InitModels(lesInstruments)
Plan <- GetPlanExp()
PickFromPlan(Plan)
ApplyModels(Plan,lesInstruments,modelEnv,"D:/Bernard/Desktop/Tmp","Dum")

save.image(file="D:/Bernard/Desktop/Tmp/ApplyModelReady.RData")

#Maintenant, on charge "D:/Bernard/Desktop/Tmp/ApplyModelReady.RData"
# et on peut utiliser ApplyModels directement!

ApplyModels(Plan,lesInstruments,modelEnv,"D:/Bernard/Desktop/Tmp","Dum")