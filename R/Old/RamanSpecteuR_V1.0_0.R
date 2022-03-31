RamanSpecteuR<-function(){
  #************************************************************************************
  #************************************************************************************
  # - Programme gerant l'acquisition de donnees Raman avec le "kit Doric".
  # - Le programme va repeter le cycle d'acquisition tant
  #   que l'operateur n'indiquera pas que c'est termine.
  # - Les fichiers de donnees sont stockes dans un repertoire au choix de l'utilisateur.
  #   Le nom des fichiers est normalises: type de donnees + date.
  #   Les fichiers sont des fichiers en format texte et les colonnes separees par un tab.
  #   Toutes les donnees sont stockees en brut (i.e. tel que recuperees des spectros)
  #   et aprC(s interpolation et retrait de la ligne de base. 
  #   Le nom des fichiers de donnees bruts se termine par un B
  #   et celui des donnees interpolees par un I.
  # - Les donnees du spectra QEPro sont corrigees C  l'aide de donnees
  #   stockees dans un fichier texte que l'operateur doit specifier dans le fichier de paramC(tres.
  #************************************************************************************
  # AUTEUR: Bernard Panneton, Agriculture et Agroalimentaire Canada
  # Fevrier 2020
  #************************************************************************************
  #************************************************************************************
  #___________________________________________________________________________
  # Charge des librairies additionnelles-----
  require(rChoiceDialogs)
  require(png)
  require(tcltk)
  require(rpanel)
  require(utils)
  require(baseline)
  #___________________________________________________________________________
  
  #Charge les parametres.----
  RamanSpecteuRPath=getSrcDirectory(function(x) {x})
  setwd(RamanSpecteuRPath)
  
  fichier_param=rchoose.files(caption="Choisir le fichier des parametres",multi = FALSE,filters = "*.R")
  source(fichier_param)  
 
  #************************************************************************************
  #************************************************************************************
  #------------------ I N I T I A L I S A T I O N S -----------------------------------
  
  # On a besoin du spectro et d'un port RS232 pour le laser. On
  # va donc commencer par charger les librairies et faire les intialisations
  # necessaires.
  
  #___________________________________________________________________________ 
  # └ GESTION DE L'INTERFACE USAGER ---------------------------
  #Efface le contenu de la console
  cat("\f")
  #Affiche un message
  cat("Phase d'initialisation des composantes.\n")
  
  graphics.off()  #Efface tous les graphiques du panneau "Plots"
  
  logo=readPNG("PolySpecteuR_Logo.png")
  graphics.off()
  op <- par(no.readonly = TRUE)
  #2 graphiques - Logo + spectre de lampe blanche
  nf=layout(matrix(c(1,2),2,1,byrow=TRUE))
  #Affiche le logo
  par(mai=c(0,0,0,0))
  plot(as.raster(logo)) 
  par(mai=op$mai)
  
  
  # └ Charge la librairie donnant acces aux spectros. ----
  source('../Lib_n_Wrapper/OOInterface.R')
  #___________________________________________________________________________
  # └ Active le "wrapper" donnant accC(s aux spectros.====
  OOobj=Start_OO()
  #___________________________________________________________________________
  # └ Definir les spectros et plages pour l'interpolation----
  cat("Code pour le QE.\n")
  QE=Define_Spectro(OOobj)
  QE=Define_WavenumberRange(QE,ram_l_min,ram_l_max,ram_step,EX_Raman)
  SetCorrections(OOobj,QE,Lin = 1,Dark = 0)
  QE=GetTEC(QE,OOobj$mywrap)
  T_QE=CoolSpectro(QE,OOobj$mywrap,QE_Temp)
  cat("\n")
  dum=readline(paste("Le detecteur du QE est à  ",as.character(T_QE),"°C. Continuer? (O/N):",sep=""))
  if (toupper(dum)=="N"){
    WarmSpectro(QE,OOobj$mywrap)
    Close_Spectro(QE)
    Quit_OO(OOobj)
    return("ABANDON")
  }
  
  #___________________________________________________________________________
  # └ Prepare pour utiliser LS2----
  
  source('../Lib_n_Wrapper/Newport_LS_2.R')
  Connect_LS_2(1)
  LaserOn(1,P_Laser)
  cat("\n")
  dum=readline(paste("Connexion au LS-2 reussie? (O/N):",sep=""))
  if (toupper(dum)=="N"){
    Close_LS2()
    WarmSpectro(QE,OOobj$mywrap)
    Close_Spectro(QE)
    Quit_OO(OOobj)
    return("ABANDON")
  }
  #___________________________________________________________________________
  
  
  # └ Lire les fichiers d'etalonnage----
  gainQE=scan(QEPro_Calib_File,sep="\t",quiet=TRUE)
 
  #************************************************************************************
  #************************************************************************************
  #_______________________________________________________
  # On demarre la sequence d'acquistion----
  
 
  # └ Fichier plan d'experience et liste des echantillons----
  fichier_plan=rchoose.files(caption = "Choisir les fichier du plan d'experience",
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
  
  
  # └ Repertoire pour stocker les donnees.----
  fichierDat_path=rchoose.dir("C:\\Users\\crdaspectral\\Documents\\Programmes\\SAIV_Version_Alain_2016\\Data",
                        "Choisir un repertoire pour les donnees.")
  #Demande un identifiant pour creer le nom des fichiers de donnees.
  identifiant=winDialogString("Entrer un identifiant pour les noms de fichier de donnees",as.character(Sys.Date()))
  cat("\n")
  
 
  
  #└ GESTION DE L'INTERFACE USAGER ---------------------------  
  selected=0
  DONE=FALSE   #On boucle le cycle d'acquisition tant que l'usager inscrit un 
               #un nouveau code d'echantillon. Autrement, on arrC*te.
  
  
  # └└ Boucle principale sur les echantillons----
  while (!DONE){   
    
    # └└└ Permettre la modification des parametres----
    newPlan='N'
    DoParam=readline("editer les (P)arametres, (N)ouveau fichier de parametres ou (C) continuer - Defaut=(C): ")
    if (toupper(DoParam)=='P'){
      edit(file=fichier_param)  
      source(fichier_param)
      
      # Lire les fichiers d'etalonnage
      gainQE=scan(QEPro_Calib_File,sep="\t",quiet=TRUE)
      
      newPlan=readline("Nouveau plan d'experience (O/N): ")
      
    }else
    {if (toupper(DoParam)=='N'){
      fichier_param=rchoose.files(caption="Choisir le nouveau fichier des paramC(tres",multi = FALSE,filters = "*.R")
      source(fichier_param)  #Charge les parametres
      
      #Lire les fichiers d'etalonnage
      gainQE=scan(QEPro_Calib_File,sep="\t",quiet=TRUE)
      
      newPlan=readline("Nouveau plan d'experience (O/N): ") 
    }
    }
    # └└└ Permettre de changer de plan d'experience------
    if (toupper(newPlan)=='O'){
      #On demande le fichier pour le plan d'experience et on prepare la liste des echantillons
      fichier_plan=rchoose.files(caption = "Choisir les fichier du plan d'experience",
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
      
    
    #└└└Remise C  zero de la fenetre graphique---------
    graphics.off()
    par(op)
    #2 graphiques - 1 Raman brut, 1 Raman interpole et 1 interpole sans ligne de base.
    nf=layout(matrix(c(1,2,3),3,1,byrow=TRUE))
    #Affiche le logo
    par(mai=c(0,0,0,0))
    plot(as.raster(logo))
    
    par(mai=op$mai*0.35)
    
    #└└└ Choix d'echantillon----
    if (selected<nrow){
      ydata=tcltk:tk_select.list(liste_ech,title="Choisir le prochain echantillon",preselect = liste_ech[selected+1])
    }else
    {
      ydata=tcltk:tk_select.list(liste_ech,title="Choisir le prochain echantillon",preselect = liste_ech[selected])
    }
    if (ydata==""){
      DONE=TRUE
      dum=readline(prompt="Retirer le dernier echantillon puis appuyer sur Enter.")
      cat("\n")
    }else
    {
      selected=which(ydata==liste_ech)
      EchID=plan[selected,1]
    
      #///////////////////////////////////////////////////////////////
      #└└└└Ecrire le fichier des Y----
      fichierDat=paste(fichierDat_path,"\\Y","_",identifiant,".txt",sep="")
      writeY(selected,plan,fichierDat)
      
      #///////////////////////////////////////////////////////////////
      #└└└└Acquisition du spectre RAMAN - Corrections - Stockage----
        QE=Define_WavenumberRange(QE,ram_l_min,ram_l_max,ram_step,EX_Raman)
        QE=Define_Acq_Param(OOobj,QE,8,Box_Raman,Scans_Raman)  #idle at 8 msec
        
        #Mettre l'echantillon en place
        dum=readline(prompt=paste("\nPlacer ",EchID, " pour le Raman puis ENTER.",sep=""))
        cat("\n")
       
        cat("Acquisition du Raman.\n")
        
        #Spectre au noir
        Noir=Grab_f_QE(QE,OOobj,T_Raman*1000)
        
         #Allume le shutter du laser
        ShutterOn(1)
        Sys.sleep(T_Laser)       #Attend pour stabiliser laser
        
        #Prendre un spectre brut 
        Signal=Grab_f_QE(QE,OOobj,T_Raman*1000)
        
        #Coupe l'alimentation du shutter
        ShutterOff(1)
        Sys.sleep(0.3)
        
        #Soustraire le noir
        QE$sp=Signal-Noir
        #Dvise par le temps d'integration en secondes (temps pour Raman en sec dans le fichier de paramC*tres)
        QE$sp=QE$sp/T_Raman
        
        #On stocke les donnees brutes
        fichierDat=paste(fichierDat_path,"\\Raman","_",identifiant,"_B.txt",sep="")
        writeDAT_Raman(QE$xaxis,QE$sp,fichierDat,EchID)
        
        
        #Applique la correction
        QE$sp=gainQE*QE$sp
        
        #Affiche le graphique des donnees brutes aprC(s correction pour le gain
        Plot_Spectrum(QE,'brut')
        title(paste("Raman pour echantillon ", EchID, sep=""))
        grid()
        
        
        #Interpole
        QE=Interpolate_Spectrum(QE)
        
        #Enleve la ligne de base si demande
        if (do_raman_baseline){
          bc<-baseline(t(as.matrix(QE$int$y)), method='modpolyfit',
                         degree=baseline_deg,
                         rep=baseline_rep, tol = baseline_tol)
          
          #Affiche le graphique - donnees interpolees
          plot(QE$int$x,QE$int$y,type="l",xlab=toupper(QE$xunits),ylab="Raman",lwd=2,col="blue")
          title(paste("Raman pour echantillon ", EchID, sep=""))
          grid()
          
          
          #Affiche le graphique - corrige pour ligne de base
          plot(QE$int$x,bc@corrected,type="l",xlab=toupper(QE$xunits),ylab="Raman",lwd=2,col="blue")
          title(paste("Raman corrige pour ", EchID, sep=""))
          grid()
          
          #On stocke les donnees interpolees brutes
          fichierDat=paste(fichierDat_path,"\\Raman","_",identifiant,"_I.txt",sep="")
          writeDAT_Raman(QE$int$x,QE$int$y,fichierDat,EchID)
          
          #On stocke les donnees de la ligne de base
          fichierDat=paste(fichierDat_path,"\\Raman_base","_",identifiant,"_I.txt",sep="")
          writeDAT_Raman(QE$int$x,bc@baseline,fichierDat,EchID)
          
          #On stocke les donnees corrigees
          fichierDat=paste(fichierDat_path,"\\Raman_corr","_",identifiant,"_I.txt",sep="")
          writeDAT_Raman(QE$int$x,bc@corrected,fichierDat,EchID)
          
          
        }else
        {
          #Affiche le graphique
          plot(QE$int$x,QE$int$y,type="l",xlab=toupper(QE$xunits),ylab="Raman",lwd=2,col="blue")
          title(paste("Raman pour echantillon ", EchID, sep=""))
          grid()
          
         
          #On stocke les donnees interpolees
          fichierDat=paste(fichierDat_path,"\\Raman","_",identifiant,"_I.txt",sep="")
          writeDAT_Raman(QE$int$x,QE$int$y,fichierDat,EchID)
        }
        
      #///////////////////////////////////////////////////////////////
      #Demande de retirer l'echantillon
      dum=readline(prompt=paste("Retirer l'echantillon puis appuyer sur Enter.",sep=""))
      cat("\n")
    }
  }
  #GESTION DE L'INTERFACE USAGER -------------------------
  #On demande si on veut continuer. Si on veut pas DONE=FALSE.
  #Identifiant de l'echantillon. Si aucun nom->on a fini!
 
  
  #///////////////////////////////////////////////////////////////
  #Clean and close----
  if (exists("QE")){
    WarmSpectro(QE,OOobj$mywrap)
    Close_Spectro(QE)
  } 
  Quit_OO(OOobj)
  Close_LS2()

  cat("\nRamanSPecteur a termine! A la prochaine!\n")
  par(op)
  graphics.off()
}


#Fonction pour ecrire le fichier des Y----
#***************************************************
writeY<-function(selected,plan,monfichier){
  #?crire les donnees ds un fichier en format txt avec "tab" comme s?parateur
 
  ladate=as.character(Sys.Date())
  letemps=format(Sys.time(),"%X")
  
  ligne=NULL
  ncol=dim(plan)[2]
  for (i in 1:ncol){
    ligne=c(ligne,as.character(plan[selected,i]))
  }
  ligne=c(ligne,ladate,letemps)
  
  
  #On proc?de
  if (file.exists(monfichier)==TRUE){  #seulement les identifiants
    mycon=file(monfichier,"a")
    cat(ligne,file=mycon,sep="\t")
    cat("\n",file=mycon)
    close(mycon)
  }
  else{
    entete=c(names(plan),"Date","Heure")
    mycon=file(monfichier,"a")   #EntC*te puis les identifiants
    cat(entete,file=mycon,sep="\t")   #entC*te
    cat("\n",file=mycon)
    cat(ligne,file=mycon,sep="\t") #donnees
    cat("\n",file=mycon)
    close(mycon)
  }
  invisible(0)
}

#Fontion qui ecrit les donnees de Raman dans un fichier-----
#************************************************************
writeDAT_Raman<-function(wl,sp,monfichier,echID){
  #C	crire les donnees ds un fichier en format txt avec "tab" comme separateur
  #On proc?de
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
    cat(wl,file=mycon,sep="\t")   #ent?te
    cat("\n",file=mycon)
    cat(paste(echID,"\t",sep=""),file=mycon)  #Id. d'?chantillon
    cat(sp,file=mycon,sep="\t") #donn?es
    cat("\n",file=mycon)
    close(mycon)
  }
  invisible(0)
}


#Grab_f_QE------------
#************************************************************
Grab_f_QE <- function(QE,OOobj,T){
  #Pour QE65000  
  QE=Define_Int_Time(OOobj,QE,T)
  QE=Grab_Spectrum(QE,OOobj$mywrap)
  QE=Define_Int_Time(OOobj,QE,8) 
  dum=QE$sp
  QE=Grab_Spectrum(QE,OOobj$mywrap)
  return(dum)
}
#END--------------------
