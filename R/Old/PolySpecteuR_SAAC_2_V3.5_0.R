PolySpecteuR_SAAC_2<-function(){
  #************************************************************************************
  # Créé à partir de PolySpecteuR_Fluo_SpectrAAC_2 V4 pour inclure des mesures de réflectance.
  # Voir modifications de Avril 2018.
  #************************************************************************************
  # - Programme gérant l'acquisition de données avec le SpectrAAC 2
  # - Les acquisitions sont faites en séquence. 
  # - Les paramètres d'acquisition sont définies dans un fichier de configuration
  # - Le programme va répéter le cycle d'acquisition tant
  #   que l'opérateur n'indiquera pas que c'est terminé.
  # - Les fichiers de données sont stockés dans un répertoire au choix de l'utilisateur.
  #   Le nom des fichiers est normalisé: type de données + date
  #   Les fichierssont des fichiers en format texte et les colonnes séparées par un tab.
  #   Toutes les données sont stockées en brut (i.e. tel que récupérées des spectros)
  #   et après interpolation. Le nom des fichiers de données bruts se termine par un B
  #   et celui des données interpolées par un I.
  # - Les données du spectro Maya sont corrigées à l'aide de données
  #   stockées dans un fichier texte que l'opérateur doit spécifier dans le fichier de
  #   configuration.
  # 
  #************************************************************************************
  # AUTEUR: Bernard Panneton, Agriculture et Agroalimentaire Canada
  # 16 novembre 2017
  #************************************************************************************
  # MODIFICATIONS - 31 janvier 2018
  # - Utilise un fichier de configuration pour les instruments. Ce fichier nommé
  #   ????_Config.R, contient l'identification du spectro (nom et no de série), 
  #   l'identification du module de MC (nom et no de série) et la définition
  #   des ports pour les modules MC. Ainsi, l'utilisateur choisit un instrument
  #   dans une liste établie à partir du contenu du répertoire "Fichiers_Instruments"
  #   et le programme se charge de tout lancer du côté du hardware.
  #************************************************************************************
  # MODIFICATIONS - 7 février 2018
  # - Modifié pour pouvoir travailler sur 2 instruments dans une même session. Pour les 2 instruments,
  #   on utilise le même fichier de paramètres d'acquisition.
  # - Modifié pour pouvoir prendre des mesures répétées sur le même échantillon (pour la position).
  #   Le nombre de répétitions est spécifié par un paramètre dans le fichier des paramètres
  #   d'acquisition.
  #************************************************************************************
  # MODIFICATIONS - Mars 2018 - V4
  #   Inclure une vérification de l'instrument au démarrage.
  #   VOir la section Vérification de l'état d'instrument plus bas.
  #************************************************************************************
  #************************************************************************************
  # MODIFICATIONS - Avril 2018 - PolySpecteuR_Fluo_n_Reflect_SpectrAAC_2 V1
  #   Le programme PolySepcteur_Fluo_SpectrAAC_2 a été modifié suite à la modification 
  #   de l'instrument pour permettre des mesures de réflectance. On maintient la
  #   fonctionalité de pouvoir travailler avec 2 instruments.
  #   La stratégie retenue est de placer l'échantillon et de faire les mesures
  #   de fluorescence et de réflectance pour cet échantillon.
  #   Le fichier de configuration de l'appareil contient
  #   les définitions de ports et des valeurs de référence (ampérages, voltages...)
  #   pour chaque instrument. Le fichier de paramètres contient les paramètres de 
  #   mesure et il est partagé pour les mesures sur les 2 instruments.
  #   La séquence est la suivante:
  #     - au démarrage - configuration du matériel
  #     - Allumer la lampe blanche pour la réflectance et fermer l'obturateur.
  #     - Contrôle de qualité pour la fluorescence.
  #     - Mesures de fluorescence.
  #     - Prise d'un noir qui sera maintenu pour toutes les mesures de réflectance.
  #     - Ouvre le shutter de la lampe blanche - Mesure sur Spectralon et affiche profil
  #       de la lampe (contrôle de qualité)
  #     - Ferme shutter
  #     - Ouvre shutter - Mesure blanc - Mesure échantillon
  #     - Ferme shutter.
  #************************************************************************************
  # Modifications - 31 Janvier 2019
  # Les fichiers de configuration d'instruments comprennent 2 paramètres pour définir
  # une fenêtre de recherche pour trouver un maximum de fluorescence dans la sous-routine
  # Mesures_Etalon_Fluo. Ces 2 paramètres vont se retrouver dans les environnements d'instrument.
  #************************************************************************************
  #************************************************************************************
  # Modifications - Juin 2019
  # Modification pour supporter plusieurs instruments dans une meme session de travail.
  # On crée un environnement pour chaque instrument qui stocke la configuration et les
  # paramètres de même que le spectro correspondant. Les environnements d'instrument sont
  # dans une liste. L'ordre dans cette liste correspond à celui dans d'autres listes/vecteurs:
  # lesspectros, Device, lesinstruments.
  # Nettoyage du code.
  #************************************************************************************
  #************************************************************************************
  # Modifications - Mesures de réflectance - Aout 2019
  # Modification pour soustraire un estimé du "stray light" estimé par la moyenne 
  # d'intensité sur une plage de longueur d'onde donnée dans le fichier de configuration
  # dans la section sur les paramètres pour la réflectance: stray_low et stray_high 
  # finissent le bas et le haut de la plage utilisé. Si stray_low=0 pas de correction.
  #************************************************************************************
  #************************************************************************************
  # Modifications - Permettre l'utilisation de 8 DELs pour la fluorescence - Sept 2019
  # Permet de travailler avec toutes les DELs définies dans le fichier de paramètre. N.B.
  # le fichier de configuration doit comporter les définitions nécessaires pour associer
  # le "hardware" pour le contrôles des DELs.
  #************************************************************************************
  #************************************************************************************
  # Modifications - Permettre l'utilisation de standards de normalisation qui diffèrent
  # selon les DELs pour la fluorescence - Oct 2019
  # N.B. Cela demande des modifications aux fichiers de configuration d'instrument 
  # (temps d'exposition, scans.... et longueur d'onde d'observation) ET aux fichiers
  # de paramètres où pour chaque position, on a ajouté un paramètre (STD_EXi) identifiant
  # le standard à utiliser pour cette DEL. Dans PolySpecteur_SAAC2, seule la sous-routine
  # "Mesures_Etalon_Fluo" a été modifiée. N.B. Lors de la mise en oeuvre, ce serait 
  # probablement pertinent de créer un nouveau fichier "Verif".
  #************************************************************************************
  # Charge des librairies additionnelles----
  if (!require(rChoiceDialogs)){
    install.packages("rChoiceDialogs", dependencies = T)
    library(rChoiceDialogs)
  }
  if (!require(png)){
    install.packages("png", dependencies = T)
    library(png)
  }
  if (!require(tcltk)){
    install.packages("tcltk", dependencies = T)
    library(tcltk)
  }
  if (!require(tcltk2)){
    install.packages("tcltk2", dependencies = T)
    library(tcltk2)
  }
  if (!require(rpanel)){
    install.packages("rpanel", dependencies = T)
    library(rpanel)
  }
  if (!require(utils)){
    install.packages("utils", dependencies = T)
    library(utils)
  }
  
  if (!require(prospectr)){
    install.packages("prospectr", dependencies = T)
    library(prospectr)
  }
  
 
  #Définit le répertoire du script en cours----
  
  PolySpecteuRPath=getSrcDirectory(function(x) {x})
  setwd( PolySpecteuRPath)
  
  # Librairies pour MC et spectros----
  
  source('../Lib_n_Wrapper/MCDAQ.R')  
  source('../Lib_n_Wrapper/OOInterface.R')
   
  #* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  
  #___________________________________________________________________________ 
  # GESTION DE L'INTERFACE USAGER ---------------------------
  # └ Efface le contenu de la console ----
  cat("\f")
  #Affiche un message
  cat("Phase d'initialisation des composantes.\n")
  
  logo=readPNG("PolySpecteuR_Logo.png")
  graphics.off()
  op <- par(no.readonly = TRUE)
  #2 graphiques - Logo + spectre de lampe blanche
  nf=layout(matrix(c(1,2),2,1,byrow=TRUE))
  #Affiche le logo
  par(mai=c(0,0,0,0))
  plot(as.raster(logo)) 
  par(mai=op$mai)
  
  
  
  #************************************************************************************
  # I N I T I A L I S A T I O N S -----------------------------------
  #************************************************************************************
  # On a besoin du spectro, du module DAQ. On
  # va donc commencer par faire les intialisations
  # nécessaires.
  #____________________________________________________________________________
  # └ Recherche instruments disponibles et propose le choix-----
  
  lesinstruments=list.files(path='Fichiers_Instruments',pattern = 'Config\\.R$')
  dum=gregexpr(pattern ='_',lesinstruments)
  N_instruments=length(dum)
  instruments=rep("",N_instruments)
  laliste=list()
  for (k in 1:N_instruments){
    n_sep=length(dum[[k]])
    instruments[k]=substr(lesinstruments[k],start=1,stop=dum[[k]][n_sep]-1)
    laliste=c(laliste,instruments[k])
  }
  lesinstruments=select.list(as.character(laliste),multiple=T,title="Choisir les instruments",graphics=T)
  N_instruments=length(lesinstruments)
  #************************************************************************************
  # └ Fichiers paramètres instruments----
  
  fichier_param=character()
  lepath=file.path(getwd(),'Fichiers_Parametres')
  for (k in 1:N_instruments){
    fichier_param[k]=rchoose.files(default=lepath,
                                   caption=paste0("Choisir le fichier des paramètres pour ",
                                                  lesinstruments[k]),
                                   multi = FALSE,filters = "*.R")
    lepath=dirname(fichier_param[k])
  }
  #************************************************************************************
  # └Environnements pour chaque instrument-----
  
  lesenv <- list()
  for (k in 1:N_instruments){
    lesenv[[k]] <- new.env()
    source(paste("Fichiers_Instruments/",lesinstruments[k],"_Config.R",sep=""),local=lesenv[[k]])
    source(fichier_param[k],local=lesenv[[k]])
  }
  #S'assurer que DoReflect et DoFluo sont partout les mêmes
  partout=TRUE
  for (k in 1:N_instruments){
    tdat <- c(lesenv[[k]]$DoReflect,lesenv[[k]]$DoFluo)
    if (k==1){
      tdat_base <- tdat
    }else
    {
      if (!all(tdat==tdat_base)) partout=F
    }
  }
  if (!partout){
    cat("DoFluo et DoReflect diffèrent dans les fichiers de paramètre.\n")
  }
  
  sameEX <- TRUE
  for (k in 1:N_instruments){
    if (lesenv[[k]]$DoFluo){
      Fluo_EX <- get_DELs_dat("DoEX",mon_envir=lesenv[[k]])
      if (!exists("Fluo_EX_ref")){
        Fluo_EX_ref <- Fluo_EX
      }else
        if (length(Fluo_EX_ref)==length(Fluo_EX)){
          sameEX <- all(Fluo_EX_ref==Fluo_EX)
        }else sameEX <- FALSE
    }
    if (!sameEX) break
  }
  
  if (!sameEX){
    cat("Pas les mêmes DELs entre les instruments pour la fluorescence.\n")
    return("Terminé!")
  }
  
  #************************************************************************************
  # └ Active le "wrapper" spectros.-----
  
  OOobj=Start_OO()
  if (is.null(OOobj)){
    rm(OOobj)
    return("Terminé.")
  }
  
  #************************************************************************************
  # └ └ Définir Spectros et plages d'interpolation----
  
  # Trouve le spectro de l'instrument.
  lesspectros=list()  #liste des spectros correspondant aux environnements/instruments
  for (k in 1:N_instruments){
    lespectro=which((OOobj$serial_no==lesenv[[k]]$les_spectro$serial) & (OOobj$lesspectros==lesenv[[k]]$les_spectro$name))
    if (length(lespectro)==0){
      cat("Ne trouve pas le spectro!\n Va quitter...\n")
      for (sp in lesspectros) Close_Spectro(sp)
      Quit_OO(OOObj)
      rm(OOobj)
      return("Terminé!")
    }
    #On définit le spectro pour l'environnement de l'instrument.
    with(lesenv[[k]],{ 
         lespectro <- Define_Spectro(OOobj,lespectro)
         SetCorrections(OOobj,lespectro,Lin = 1, Dark = 0)
         lespectro <- Define_WavelengthRange(lespectro,fluo_l_min,fluo_l_max,fluo_step)
    })
  }
  
  #************************************************************************************
  # └ module MC DAQ----
  #************************************************************************************
  # └└ Initialise les devices------
  Init_MCLib()
  lesDevices=GetDevices()
  
  #Initialise chaque board
  for (k in 1:lesDevices$nbdevices)
    dum=Init_DAQ(lesDevices$noms[k],lesDevices$serie[k],lesDevices$BoardNum[k])
  lesnoms=lesDevices$noms[1:lesDevices$nbdevices]
  Device <- list()
  for (k in 1:N_instruments){
    Device[[k]]=which((lesDevices$serie==lesenv[[k]]$les_mcusb$serial) & (lesDevices$noms==lesenv[[k]]$les_mcusb$name))
    if (length(Device[[k]])==0){
      cat("Ne trouve pas le module de MC!\n Va quitter...\n")
      Quitte_MCLIB()
      for (sp in lesspectros) Close_Spectro(sp)
      Quit_OO(OOobj)
      return("Terminé.")
    } 
    #Configure les boards. Seuls les ports DIO sont configurés.
    Config_Board(lesDevices,lesenv[[k]]$lesPorts)
    #Configure les ports de sortie en courant  et ceux en voltage sur USB-3106
    for (i in lesenv[[k]]$lesports_amp){
      err=.C("BP_cbSetConfig",boardinfo=as.integer(BOARDINFO),bnum=as.integer(lesDevices$BoardNum[Device[[k]]]),
             channelnum=as.integer(i),configitem=as.integer(BIDACRANGE),
             configval=as.integer(MA0TO20),out=as.integer(0))
    }
    for (i in lesenv$lesports_V){
      err=.C("BP_cbSetConfig",boardinfo=as.integer(BOARDINFO),bnum=as.integer(lesDevices$BoardNum[Device[[k]]]),
             channelnum=as.integer(i),configitem=as.integer(BIDACRANGE),
             configval=as.integer(UNI10VOLTS),out=as.integer(0))
    }
    with(lesenv[[k]], gainY<-scan(Maya_Calib_File,sep="\t",quiet=TRUE))
  }
  
  
  
  
  #************************************************************************************
  #État d'instrument Fluo + Normalisation----
  #************************************************************************************
  #
  #
  cat("\n***************************************\nVérification de(s) instrument(s)")
  for (k in 1:N_instruments){
      with(lesenv[[k]],{
        lesEXs <- get_DELs_dat("EX",mon_envir=lesenv[[k]])
        if (DoFluo){
          dum <- readline(paste("\nENTER pour mesure le facteur de normalisation pour ", lesinstruments[k],
                         " ou Q-ENTER pour quitter.", sep=""))
          
          if (!(toupper(dum)=="Q")){
            NormY <- Mesures_Etalon_Fluo(lesenv[[k]],lespectro,
                                      OOobj,lesDevices,gainY,logo,op,lesEXs)
            cat(paste("Facteur de correction: ",format(lesenv[[k]]$NormY,
                                                    digits=3,scientific = TRUE),"\n",sep=""))
          }
        }
      })
    if (length(lesenv[[k]]$dum)>0){
      if  (toupper(lesenv[[k]]$dum)=="Q"){
        Abort(lesDevices,lesspectros,OOobj,op)
        graphics.off()
        return("Quitté prématurément!")
      }
    }
  }
  
  readline("Appuyer sur Retour/Enter pour continuer. \n")
  
 
  #***********************************************************************************
  # Mise sous tension de la lampe blanche et vérification du profil
  # On laisse la lampe ouverte mais on ferme le shutter.
  #***********************************************************************************
  #VÉrification lampe blanche pour réflectance ----
  dumtest <- lapply(lesenv, function(x) x$DoReflect)
  dumtest <- as.logical(dumtest)
  if (any(dumtest)){
    for (k in 1:N_instruments){
    graphics.off()
    par(op)
    #10 graphiques - 2 premiers pour le logo et 7 suivants pour la fluo, la dernière pour réflectance
    nf=layout(matrix(c(1,2,3),3,1,byrow=TRUE))
    #Affiche le logo
    par(mai=c(0,0,0,0))
    plot(as.raster(logo))
    
    par(mai=op$mai*0.35)
    
    
    with (lesenv[[k]],
          {
            if (DoReflect){
             readline(paste0("\nS'assurer que la lampe blanche pour ",
                                lesinstruments[[k]],
                                 " est en fonction \npuis appuyer sur Retour/Enter\n."))
              if (toupper(dum)=="Q"){
                Abort(lesDevices,lesspectros,OOobj,op)
                graphics.off()
                return("Quitté prématurément")
              }
              #Ouvre le shutter
              readline(paste0("\nFermer l'obturateur de la source blanche pour ",
                                lesinstruments[[k]],
                              "\n", "Appuyer sur Retour/Enter.\n"))
              
              lespectro=Define_Acq_Param(OOobj,lespectro,T_Reflect,Box_Reflect,Scans_Reflect)
              Sys.sleep(0.2)
              
              
              #Mesure sur noir standard
              lespectro=Grab_Spectrum(lespectro,OOobj$mywrap)
              #Met le noir en réserve
              noir_std=lespectro$sp
              Plot_Spectrum(lespectro,"brut")
              title(main="Noir standard")
              
              #Mesure sur standard de réflectance
              readline(paste0("\nPlacer le standard de réflectance  dans ",
                              lesinstruments[[k]], "\n",
                              "Ouvrir l'obturateur pour la source blanche puis appuyer sur Retour/Enter.\n"))
              lespectro=Grab_Spectrum(lespectro,OOobj$mywrap)
              blanc_std=lespectro$sp-noir_std
              lespectro$sp=blanc_std
              Plot_Spectrum(lespectro,"brut")
              title("Référence blanche")
              
              #Ferme le shutter
              readline(paste0("\nFermer l'obturateur de la source blanche pour ",
                              lesinstruments[[k]], "\npuis appuyer sur Retour/Enter.\n"))
              
              
              #Permet de quitter le programme si c'est pas bon.
              dum="a"
              while (sum(toupper(dum)==c("O","N"))==0)
                dum=readline("\nSatisfait du profil de la lampe (O/N): ")
              if (toupper(dum)=="N"){ #quitter le programme.
                #Clean and close
                for (k in 1:lesDevices$nbdevices) ReleaseBoard(lesDevices$BoardNum[k])
                (Quitte_MCLIB())
                for (sp in lesspectros) Close_Spectro(sp)
                rm(OOobj)
                Quit_OO(OOobj)
                par(op)
                graphics.off()
                return("PolySPecteur a terminé! A la prochaine!")
              }
            }
          })
    }
  }
  
  #_______________________________________________________
  # Démarre la séquence d'acquistion: -----------
  
  # └ Plan d'expérience, liste des échantillons----
  fichier_plan=rchoose.files(caption = "Choisir les fichier du plan d'expérience",
                             multi=FALSE,filters="*.txt")
  plan=read.table(fichier_plan,header=TRUE,sep="\t")
  nrow=dim(plan)[1]
  ncol=dim(plan)[2]
  liste_ech=c()
  for (k in 1:nrow){
    ligne=""
    for (i in 1:ncol){
      ligne=paste(ligne,as.character(plan[k,i]))
    }
    liste_ech=rbind(liste_ech,ligne)
  }
  
  
  
  # └ Répertoire pour données----
  
  fichierDat_path=rchoose.dir("C:\\Users\\crdaspectral\\Documents\\Programmes\\SAIV_Version_Alain_2016\\Data",
                        "Choisir un répertoire pour les données.")
  #Demande un identifiant pour créer le nom des fichiers de données.
  identifiant=winDialogString("Entrer un identifiant pour les noms de fichier de données",as.character(Sys.Date()))
  cat("\n")
  
  
  # └ GESTION DE L'INTERFACE USAGER ---------------------------  
  selected=0
  DONE=FALSE   #On boucle le cycle d'acquisition tant que l'usager inscrit un 
               #un nouveau code d'échantillon. Autrement, on arrête. 
  
  for (k in 1:N_instruments){
    with(lesenv[[k]], {
      lesNoirs <- matrix(0,nrow=8,ncol=length(lespectro$xaxis))
      t0Noir <- Sys.time()-(Delai_noir*60)
    })
  }
  # └ Boucle principale sur échantillons----------
  while (!DONE){   
    
    #*****************************************************************************
    # └└ Permet de changer de plan d'expérience------
    newPlan=readline("\nNouveau plan d'expérience (O/N - Défaut=N): ") 
    if (toupper(newPlan)=='O'){
      #On demande le fichier pour le plan d'expérience et on prépare la liste des échantillons
      fichier_plan=rchoose.files(caption = "Choisir les fichier du plan d'expérience",
                                 multi=FALSE,filters="*.txt")
      plan=read.table(fichier_plan,header=TRUE,sep="\t")
      nrow=dim(plan)[1]
      ncol=dim(plan)[2]
      liste_ech=c()
      for (k in 1:nrow){
        ligne=""
        for (i in 1:ncol){
          ligne=paste(ligne,as.character(plan[k,i]))
        }
        liste_ech=rbind(liste_ech,ligne)
      }
    }
    
    
    #*****************************************************************************
    # └└Choisir prochain dans plan d'expérience-----
    if (selected<nrow){
      ydata=tcltk::tk_select.list(liste_ech,title="Choisir le prochain échantillon",preselect = liste_ech[selected+1])
    }else
    {
      ydata=tcltk::tk_select.list(liste_ech,title="Choisir le prochain échantillon",preselect = liste_ech[selected])
    }
    if (ydata==""){
      DONE=TRUE
      dum=readline(prompt="\nRetirer le dernier échantillon puis appuyer sur Enter.")
      cat("\n")
    }else
    {
    
      #*****************************************************************************
      # └└Choix de l'instrument---------
      instrument_actif <- 1
      if (N_instruments>1){
        dum=0
        while (dum<1 | dum>N_instruments){
          dumt=select.list(as.character(lesinstruments),multiple=F,title="Choisir les instruments",graphics=T)
          dum <- which(lesinstruments==dumt)
          instrument_actif=dum
        }
      }
      with (lesenv[[instrument_actif]],{
        if (DoFluo){
          dum <- get_DELs_dat("FEX",mon_envir = lesenv[[instrument_actif]])
          dum <- apply(dum,2,list)
          lesDels <- lapply(dum,function(x) as.list(unlist(x)))
         }
      })
      
      #*****************************************************************************  
      #└└Permettre la modification des paramètres----
      newPlan='N'
      DoParam=readline("\nÉditer les (P)aramètres, (N)ouveau fihcier de paramètres ou (C)ontinuer (P/N/C - Défaut=C): ")
      if (toupper(DoParam)=='P'){
        edit(file=fichier_param[instrument_actif]) 
        source(fichier_param[instrument_actif],local=lesenv[[instrument_actif]])
      }else
      {if (toupper(DoParam)=='N')
        {
          fichier_param[instrument_actif]=rchoose.files(caption="Choisir le nouveau fichier des paramètres",multi = FALSE,filters = "*.R")
          source(fichier_param[instrument_actif],local=lesenv[[instrument_actif]])  #Charge les paramètres
        }
      }
    
      
   
    
      selected=which(ydata==liste_ech)
      EchID=plan[selected,1]
    
      
      
      with(lesenv[[instrument_actif]],{
        #///////////////////////////////////////////////////////////////
        #└└└Écrire le fichier des Y------
        if (DoFluo | DoReflect){
          fichierDat=paste(fichierDat_path,"\\Y","_",identifiant,".txt",sep="")
          for (k in (1:Nb_reps_Fluo))
            writeY(selected,plan,lesinstruments[instrument_actif],k,fichierDat)
        }
        
        #///////////////////////////////////////////////////////////////
        #└└└ DOFLUO------
        if (DoFluo){    #### BOUCLE POUR LA FLUORESCENCE
          
          # D'abord, regroupe des paramètres dans des listes/vecteurs
          Fluo_EX <- get_DELs_dat("DoEX", mon_envir = lesenv[[instrument_actif]])
          T_Fluo <- get_DELs_dat("T_EX", mon_envir = lesenv[[instrument_actif]])
          Box_FLuo <- get_DELs_dat("Box_EX", mon_envir = lesenv[[instrument_actif]])
          Scans_Fluo <- get_DELs_dat("Scans_EX", mon_envir = lesenv[[instrument_actif]])
          lesEXs <- get_DELs_dat("EX", mon_envir = lesenv[[instrument_actif]])
          FluoDel_I <- get_DELs_dat("LED_I_EX", mon_envir = lesenv[[instrument_actif]])
          
          
          lespectro=Define_WavelengthRange(lespectro,fluo_l_min,fluo_l_max,fluo_step)
          Sys.sleep(0.2)
         
          for (krep in 1:Nb_reps_Fluo){
            readline(paste("\nPlacer l'échantillon ",EchID,
                           " à la position ",as.character(krep),
                           " dans l'instrument ", lesinstruments[instrument_actif],
                           sep=""))
            #Remise à zéro de la fenêtre graphique
            graphics.off()
            par(op)
            #10 graphiques - 2 premiers pour le logo et 7 suivants pour la fluo, la dernière pour réflectance
            nf=layout(matrix(c(1,1,2,3,4,5,6,7,8,9),5,2,byrow=TRUE))
            #Affiche le logo
            par(mai=c(0,0,0,0))
            plot(as.raster(logo))
            
            par(mai=op$mai*0.35)
            
            #Définir un "flag" pour savoir s'il faut refaire les noirs
            tnow_noir=as.numeric(Sys.time())
            dt=(tnow_noir-as.numeric(t0Noir))/60
            flag_noir=FALSE
            if (dt>Delai_noir){
              cat("Délai pour noir: ",dt,"\n")
              flag_noir = TRUE
              t0Noir=as.numeric(Sys.time())
            }
            
            for (k in RangFluo){
              if (Fluo_EX[k]){
                #Prep du spectro
                lespectro=Define_Acq_Param(OOobj,lespectro,T_Fluo[k],Box_FLuo[k],Scans_Fluo[k])
                Sys.sleep(0.2)
                
                #Prendre un spectre au noir
                if (flag_noir){
                  cat("Acquisition d'un noir.\n")
                  lespectro=Grab_Spectrum(lespectro,OOobj$mywrap)
                  #Met le noir en réserve
                  lesNoirs[k,]=lespectro$sp
                }
             
                #Allume la DEL à EXi_DEL_I milliampères
                I_OUT(lesDels[[k]],lesDevices,FluoDel_I[k])
                Sys.sleep(T_DEL)       #Attend T_DEL secondes pour stabiliser la DEL
                
                #Prendre un spectre brut de fluo à 280 nm
                
                cat(paste("Acquisition de fluorescence à ",as.character(lesEXs[k])," nm.\n",sep=""))
                lespectro=Grab_Spectrum(lespectro,OOobj$mywrap)
                
                #Coupe l'alimentation de la DEL (0 milliamps)
                I_OUT(lesDels[[k]],lesDevices,0)
                
                #Soustraire le noir
                lespectro$sp=lespectro$sp-lesNoirs[k,]
                
                #On stocke les données brutes
                fichierDat=paste(fichierDat_path,"\\EX",as.character(lesEXs[k]),"_",identifiant,"_B.txt",sep="")
                writeDAT_Fluo(lespectro$xaxis,lespectro$sp,fichierDat,EchID)
                
                
                #Applique la correction
                lespectro$sp=gainY*lespectro$sp
                #Normalise pour le pic d'étalonnage du début de la session
                lespectro$sp=lespectro$sp/NormY[k]
                
                
                #On fait l'interpolation
                lespectro=Interpolate_Spectrum(lespectro)
                lespectro$int$y=lespectro$int$y/T_Fluo[k]*1000   #Normalise par le temps d'exposition en secondes
                
                # On montre le spectre interpolé 
                x0=lesEXs[k]+25
                xend=range(lespectro$int$x)[2]
                i1=which(lespectro$int$x>=x0)[1]
                i2=length(lespectro$int$x)
                Plot_Spectrum(lespectro,"interpole",xlim=c(x0,xend),ylim=range(lespectro$int$y[i1:i2]))
                title(paste("Échantillon ",EchID," à ", as.character(lesEXs[k])," - Rep = ",as.character(krep), sep=""))
                grid()
               
                #On stocke les donnees interpolees
                fichierDat=paste(fichierDat_path,"\\EX",as.character(lesEXs[k]),"_",identifiant,"_I.txt",sep="")
                writeDAT_Fluo(lespectro$int$x,lespectro$int$y,fichierDat,EchID)
              }else
              {
                plot(c(0,1),c(0,1), type="n", axes=F, xlab="", ylab="")    #Pour passer au graphique suivant
                text(0.5,0.5,paste("Pas de fluo. à ",as.character(lesEXs[k]), sep=""))
              }
            }
            
          }#boucle des reps
          ###Fin de la boucle pour la fluo
        }else
          if (!partout){
            for (krep in 1:Nb_reps_Fluo){
              for (k in RangFluo){
                if (Fluo_EX[k]){
                  #On stocke les données bidons en format de données brutes
                  fichierDat=paste(fichierDat_path,"\\EX",as.character(lesEXs[k]),"_",identifiant,"_B.txt",sep="")
                  #Trouve un spectro qui fait la fluorescence
                  lekk=0
                  for (kk in 1:N_instruments){
                    if (lesenv[[kk]]$DoFluo){
                      lekk=kk
                    }
                    if (lekk>0) break
                  }
                  
                  ydum=rep(NA,length(lesenv[[lekk]]$lespectro$xaxis))
                  writeDAT_Fluo(lesenv[[lekk]]$lespectro$xaxis,ydum,fichierDat,EchID)
                  
                  #On stocke les donnees bidons en format de données interpolées
                  fichierDat=paste(fichierDat_path,"\\EX",as.character(lesEXs[k]),"_",identifiant,"_I.txt",sep="")
                  xdum <- seq(lesenv[[lekk]]$fluo_l_min,lesenv[[lekk]]$fluo_l_max,lesenv[[lekk]]$fluo_step)
                  ydum=rep(NA,length(xdum))
                  writeDAT_Fluo(xdum,ydum,fichierDat,EchID)
                }
              }
            }
          }
        #Fin de la boucle pour la fluo
        
        
        
        #///////////////////////////////////////////////////////////////
        # └└└ DOREFLECT------------
        if (DoReflect){    #### BOUCLE POUR LA RÉFLECTANCE
          lespectro=Define_WavelengthRange(lespectro,reflect_l_min,reflect_l_max,reflect_step)
          Sys.sleep(0.2)
          
          lespectro=Define_Acq_Param(OOobj,lespectro,T_Reflect,Box_Reflect,Scans_Reflect)
          Sys.sleep(0.2)
          
          #Remise à zéro de la fenêtre graphique
          graphics.off()
          par(op)
          #10 graphiques - 2 premiers pour le logo. 
          #3 pour noir de référence, 4 pour le blanc de référence, 5 à 10 pour les
          #reps (pas plus de 6!)
          nf=layout(matrix(c(1,1,2,3,4,5,6,7,8,9),5,2,byrow=TRUE))
          #Affiche le logo
          par(mai=c(0,0,0,0))
          plot(as.raster(logo))
          
          par(mai=op$mai*0.35)
          
          
          #Ouvre le shutter
          readline(paste("\nS'assurer que l'obturateur de la source blanche pour l'instrument ",
                          lesinstruments[instrument_actif], " est fermé. \n",
                         "Puis appuyer sur Retour/Enter.",
                         sep=""))
          
          #Mesure sur noir standard
          lespectro=Grab_Spectrum(lespectro,OOobj$mywrap)
          Plot_Spectrum(lespectro,"brut")
          title(main="Noir standard")
          #Met le noir en réserve
          noir_std=lespectro$sp
          
          #Mesure sur standard de réflectance
          readline(paste("\nPlacer le standard de réflectance  dans l'instrument ",
                          lesinstruments[instrument_actif], "\n",
                         "Ouvrir l'obturateur de la source blanche de l'instrument ",
                          lesinstruments[instrument_actif], "\n",
                          "Appuyer sur Retour/Enter.",sep=""))
          lespectro=Grab_Spectrum(lespectro,OOobj$mywrap)
          blanc_std=lespectro$sp-noir_std
          # Correction pour le stray light
          if (stray_low>0){
            i1 <- which(lespectro$xaxis>=stray_low)[1]
            i2 <- which(lespectro$xaxis>=stray_high)[1]
            stray <- mean(blanc_std[i1:i2])
            blanc_std <- blanc_std-stray
          }
          lespectro$sp=blanc_std
          Plot_Spectrum(lespectro,"brut")
          
          
          for (krep in 1:Nb_reps_Reflect){
            readline(paste("\nPlacer l'échantillon ",EchID,
                           " à la position ",as.character(krep),
                           "dans l'instrument ", lesinstruments[instrument_actif],
                           sep=""))
           
            #Lire le spectre brut
            lespectro=Grab_Spectrum(lespectro,OOobj$mywrap)
            data_ech <- lespectro$sp-noir_std
            
            # Correction pour le stray light
            if (stray_low>0){
              i1 <- which(lespectro$xaxis>=stray_low)[1]
              i2 <- which(lespectro$xaxis>=stray_high)[1]
              stray <- mean(data_ech[i1:i2])
              data_ech <- data_ech-stray
            }
            
            #Calcul de réflectance
            lespectro$sp=data_ech/blanc_std
            
            #On fait l'interpolation
            lespectro=Interpolate_Spectrum(lespectro)
            
            # On montre le spectre interpolé 
            Plot_Spectrum(lespectro,"interpole",xlim=c(250,800),ylim=c(0,1.2))
            title(paste("Échantillon ",EchID," - Rep = ",as.character(krep), sep=""))
            grid()
            
            #On stocke les donnees interpolees
            fichierDat=paste(fichierDat_path,"\\Transmit","_",identifiant,"_I.txt",sep="")
            writeDAT_Transmit(lespectro$int$x,lespectro$int$y,fichierDat,EchID)
          }
          #Ouvre le shutter
          readline(paste("\nFemer l'obturateur de la source blanche pour l'instrument ",
                          lesinstruments[instrument_actif], "\n",
                         "Puis appuyer sur Retour/Enter.",
                         sep=""))
        }else
          if (!partout){
            for (krep in 1:Nb_reps_Reflect){
              #On stocke les donnees bidons en format de données interpolees
              fichierDat=paste(fichierDat_path,"\\Transmit","_",identifiant,"_I.txt",sep="")
              #Trouve un spectro qui fait la transmittance
              lekk=0
              for (kk in 1:N_instruments){
                if (lesenv[[kk]]$DoReflect){
                  lekk=kk
                }
                if (lekk>0) break
              }
              xdum <- seq(lesenv[[lekk]]$reflect_l_min,lesenv[[lekk]]$reflect_l_max,lesenv[[lekk]]$reflect_step)
              ydum <- rep(NA,length(xdum))
              writeDAT_Transmit(xdum,ydum,fichierDat,EchID)
            }
          }
        ### Fin de la boucle de réflectance
        
        #///////////////////////////////////////////////////////////////
        dum=readline(prompt=paste("\nRetirer l'échantillon ", EchID,
                                  " de l'instrument ", lesinstruments[instrument_actif],
                                   " puis appuyer sur Enter.",sep=""))
        cat("\n")
        cat("\n")
      })
    }
  }
  
  #///////////////////////////////////////////////////////////////
  #Clean and close-------
  for (k in 1:lesDevices$nbdevices) ReleaseBoard(lesDevices$BoardNum[k])
  (Quitte_MCLIB())
  for (sp in lesspectros) Close_Spectro(sp)
  rm(OOobj)
  Quit_OO(OOobj)
  cat("\nPolySPecteur a terminé! A la prochaine!\n")
  par(op)
  graphics.off()
}

#writeY----
#Fonction pour écrire le fichier des Y
writeY<-function(selected,plan,instrum,rep,monfichier)
  #Écrire les données ds un fichier en format txt avec "tab" comme séparateur
{ 
  ladate=as.character(Sys.Date())
  letemps=format(Sys.time(),"%X")
  ligne=NULL
  ncol=dim(plan)[2]
  for (i in 1:ncol){
    ligne=c(ligne,as.character(plan[selected,i]))
  }
  ligne=c(ligne,ladate,letemps,instrum,rep)
  #On procède
  if (file.exists(monfichier)==TRUE){  #seulement les identifiants
    mycon=file(monfichier,"a")
    cat(ligne,file=mycon,sep="\t")
    cat("\n",file=mycon)
    close(mycon)
  }
  else{
    entete=c(names(plan),"Date","Heure","Instrument","PosRep")
    mycon=file(monfichier,"a")   #Entête puis les identifiants
    cat(entete,file=mycon,sep="\t")   #entête
    cat("\n",file=mycon)
    cat(ligne,file=mycon,sep="\t") #données
    cat("\n",file=mycon)
    close(mycon)
  }
  invisible(0)
}



#writeDAT_Fluo----
#Fontion qui écrit les données de fluorescence dans un fichier.
writeDAT_Fluo<-function(wl,sp,monfichier,echID)
  #Écrire les données ds un fichier en format txt avec "tab" comme séparateur
{ 
  #On procède
    if (file.exists(monfichier)==TRUE){  #seulement le spectre
      mycon=file(monfichier,"a")
      cat(paste(echID,"\t",sep=""),file=mycon)
      cat(sp,file=mycon,sep="\t")
      cat("\n",file=mycon)
      close(mycon)
    }
    else{
      mycon=file(monfichier,"a")   #nombres d'onde puis le spectre
      cat("\t",file=mycon)
      cat(wl,file=mycon,sep="\t")   #entête
      cat("\n",file=mycon)
      cat(paste(echID,"\t",sep=""),file=mycon)  #Id. d'échantillon
      cat(sp,file=mycon,sep="\t") #données
      cat("\n",file=mycon)
      close(mycon)
    }
  invisible(0)
}


#writeDAT_Transmit-----
#Fontion qui écrit les données de tansmittance dans un fichier.
writeDAT_Transmit<-function(wl,sp,monfichier,echID)
  #Écrire les données ds un fichier en format txt avec "tab" comme séparateur
{ 
  #On procède
  if (file.exists(monfichier)==TRUE){  #seulement le spectre
    mycon=file(monfichier,"a")
    cat(paste(echID,"\t",sep=""),file=mycon)
    cat(sp,file=mycon,sep="\t")
    cat("\n",file=mycon)
    close(mycon)
  }
  else{
    mycon=file(monfichier,"a")   #nombres d'onde puis le spectre
    cat("\t",file=mycon)
    cat(wl,file=mycon,sep="\t")   #entête
    cat("\n",file=mycon)
    cat(paste(echID,"\t",sep=""),file=mycon)  #Id. d'échantillon
    cat(sp,file=mycon,sep="\t") #données
    cat("\n",file=mycon)
    close(mycon)
  }
  invisible(0)
}


#Mesures_Etalon_Fluo----
Mesures_Etalon_Fluo <- function(instrument.env,lespectro,OOobj,lesDevices,gainMaya,logo,op,lesEXs){
  
  dum <- get_DELs_dat("FEX",mon_envir = instrument.env)
  dum <- apply(dum,2,list)
  lesDels <- lapply(dum,function(x) as.list(unlist(x)))
  
  lespectro=Define_WavelengthRange(lespectro,instrument.env$fluo_l_min,
                              instrument.env$fluo_l_max,
                              instrument.env$fluo_step)
  Sys.sleep(0.2)
  
  N_ExFluo <- length(lesDels)
  NormY <- rep(0,N_ExFluo)
  
  #Récupère l'assignation à un standard pour chaque DEL
  std_4_DEL <- get_DELs_dat("STD_EX",mon_envir = instrument.env)
  #Liste des standards
  les_stds <- unique(std_4_DEL)
  #Nombre de standards
  N_std <- length(les_stds)
  
  #Vérifie si le fichier Instrument_x_Verif.txt existe. Non, on le crée.
  #Oui, on lit les données.
  #path='Fichiers_Instruments'
  lefichier=instrument.env$Instrument_Verif_File
  if (file.exists(lefichier)){
    histoire = read.table(lefichier,header=TRUE,sep="\t",stringsAsFactors = FALSE)
    histoire_length=nrow(histoire)
  }else
  {
    histoire=data.frame('Date'=as.character(),'Heure'=as.character(),stringsAsFactors = FALSE)
    for (kk in 1:N_ExFluo) histoire <- eval(parse(text=paste0("cbind(histoire,'DEL",kk,"'=as.numeric())")))
    
    histoire_length=nrow(histoire)
  }
  if (histoire_length>0){
    mean_DEL_I=colMeans(histoire[,-c(1,2)])
  }else
  {
    mean_DEL_I <- rep(0,N_ExFluo)
  }
  
  #Remise à zéro de la fenêtre graphique
  graphics.off()
  par(op)
  #10 graphiques - premier pour le logo et 8 suivants pour la fluo
  nf=layout(matrix(c(1,2,3,4,5,6,7,8,9,10),5,2,byrow=TRUE))
  #Affiche le logo
  par(mai=c(0,0,0,0))
  plot(as.raster(logo))
  par(mai=op$mai*0.35)
  
  maxi=matrix(0,ncol=N_ExFluo,nrow=instrument.env$Instr_reps)
  les_ii=matrix(0,ncol=N_ExFluo,nrow=instrument.env$Instr_reps)
  for (ijk in (1:N_std)){
    readline(paste0("\nPlacer l'étalon ",les_stds[ijk], " dans l'instrument."))
    lesk <- which(std_4_DEL==les_stds[ijk])
    for (jj in 1:instrument.env$Instr_reps){
      if (jj>1) readline("\nDéplacer l'étalon, puis ENTER.")
      for (k in lesk){
        lespectro=Define_Acq_Param(OOobj,lespectro,instrument.env$Instr_T_exp_test[[std_4_DEL[k]]][k],
                              instrument.env$Instr_Boxcar_test[[std_4_DEL[k]]][k],
                              instrument.env$Instr_N_scans_test[[std_4_DEL[k]]][k])
        Sys.sleep(0.2)
        cat(paste("Acquisition d'un noir pour la DEL ",as.character(k),".\n",sep=""))
        lespectro=Grab_Spectrum(lespectro,OOobj$mywrap)
        #Met le noir en réserve
        noir=lespectro$sp
        #Allume la DEL à EXi_DEL_I milliampères
        I_OUT(lesDels[[k]],lesDevices,instrument.env$FluoDel_I_test[[std_4_DEL[k]]][k])
        Sys.sleep(instrument.env$T_DEL)       #Attend T_DEL secondes pour stabiliser la DEL
        cat(paste("Acquisition de fluorescence pour la DEL ",as.character(k),".\n",sep=""))
        lespectro=Grab_Spectrum(lespectro,OOobj$mywrap)
        #Coupe l'alimentation de la DEL (0 milliamps)
        I_OUT(lesDels[[k]],lesDevices,0)
        #Soustraire le noir
        lespectro$sp=lespectro$sp-noir
        lespectro$sp=gainMaya*lespectro$sp
        #Divise par le temps d'exposition sec, 1000 parce que le temps est en msec
        lespectro$sp=lespectro$sp/instrument.env$Instr_T_exp_test[[std_4_DEL[k]]][k]*1000
        #On fait l'interpolation
        lespectro=Interpolate_Spectrum(lespectro)
        #Trouve max
        i1=floor(instrument.env$Norm_Y_peak[[std_4_DEL[k]]]-instrument.env$Norm_Y_BW[[std_4_DEL[k]]]/2)
        i1=which(lespectro$int$x>=i1)[1]
        i2=ceiling(instrument.env$Norm_Y_peak[[std_4_DEL[k]]]+instrument.env$Norm_Y_BW[[std_4_DEL[k]]]/2)
        i2=which(lespectro$int$x>i2)[1]-1
        ii=i1-1+which.max(lespectro$int$y[i1:i2])
        les_ii[jj,k]=ii
        #Moyenne sur ±2 pixels
        #maxi[jj,k]=mean(lespectro$int$y[(ii-2):(ii+2)]) 
        #savgol puis maximum
        dum=savitzkyGolay(lespectro$int$y[(ii-5):(ii+5)],0,3,5)
        maxi[jj,k]=dum[4]
      }
    }
  }
  for (k in 1:N_ExFluo){
    NormY=colMeans(maxi)
    iis=colMeans(les_ii)
    i2=histoire_length
    if (i2>5){
      i1=i2-4
    }else
    {
      i1=1
    }
    if (histoire_length==0){
      dat_4_plot=c(mean_DEL_I[k],NormY[k])
      names_4_plot=c("Moyenne","Courant")
      barplot(dat_4_plot,names.arg=names_4_plot,main=paste("Excitation à ",
              as.character(lesEXs[k])," nm",sep=""), ylab="Absolu")
    }else
    {
      dat_4_plot=c(mean_DEL_I[k],histoire[(i1:i2),(k+2)],NormY[k])
      dat_4_plot=dat_4_plot/dat_4_plot[1]
      names_4_plot=c("Moyenne",histoire[(i1:i2),1],"Courant")
      barplot(dat_4_plot,names.arg=names_4_plot,main=paste("Excitation à ",
             as.character(lesEXs[k])," nm",sep=""), ylab="Rel. à moyenne")
    }
  }

  larow=nrow(histoire)+1
  histoire[larow,1] <-  format(Sys.time(),"%D")
  histoire[larow,2] <- format(Sys.time(),"%H:%M:%S")
  for (kk in 1:N_ExFluo){
    histoire[larow,kk+2] <- NormY[kk]
  }
  write.table(histoire,lefichier,sep="\t")
  return(NormY)
}


#Abort <- function(lesDevices,lesspectros,OOobj,op)----
Abort <- function(lesDevices,lesspectros,OOobj,op){
  for (k in 1:lesDevices$nbdevices) ReleaseBoard(lesDevices$BoardNum[k])
  (Quitte_MCLIB())
  for (sp in lesspectros) Close_Spectro(sp)
  rm(OOobj)
  Quit_OO(OOobj)
  cat("\nPolySPecteur a terminé! A la prochaine!\n")
  par(op)
  return()
}

#get_DELs_dat-----------------------------------------
get_DELs_dat <- function(varname,mon_envir=.GlobalEnv)
  #Fonction pour récupérer des vecteurs/listes de paramètres
{
  dum <- ls(envir = mon_envir, pattern = paste0("^",varname,".$"))
  dum <- unname(sapply(dum,FUN=function(x) get(x,envir=mon_envir)))
  return(dum)
}
#END-----------------------------------------


