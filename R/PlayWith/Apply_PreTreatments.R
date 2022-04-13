Apply_PreTreatments <- function(prepro_params, whichPrepro, unSpectre)
  #Apply all pretreatments as defined in prepro_par.
  
{
  # Truncation-----
  trunc_limits<-prepro_params$trunc_limits[whichPrepro,]
  wl<-unSpectre[1,]
  XData_p <- unSpectre[,((wl>=trunc_limits[1]) & (wl<=trunc_limits[2]))]
  
  # Then per spectra norm.----
  type <- prepro_params$byspectra_scaling_index[whichPrepro]
  
  ## Value-----
  letest <- type==2
  cntr_n_w <- prepro_params$cntr_n_w[whichPrepro,]
  if (letest){
    wl<-XData_p[1,] # wl vector
    X<-as.numeric(XData_p[2,]) # spectrum.
    i1<-which(wl >= (cntr_n_w[1]-cntr_n_w[2]))[1]
    i2<-which(wl >= (cntr_n_w[1]+cntr_n_w[2]))[1]
    X <-  X/mean(X[i1:i2])
    XData_p <-  rbind(wl,X) #rebuild spectrum with wl
  }
  
  ## Closure-----
  letest <- type==3
  if (letest){
    wl<-XData_p[1,] # wl vector
    X<-as.numeric(XData_p[2,]) # spectrum.
    L <- length(wl)
    X <- X*L/sum(X)
    XData_p <-  rbind(wl,X) #rebuild spectrum with wl
  } 
  
  #Savitzky-Golay-----
  
  dosavgol <- prepro_params$do_savgol[whichPrepro]
  m <- prepro_params$m[whichPrepro] 
  p <- prepro_params$p[whichPrepro]
  w <- prepro_params$w[whichPrepro]
 
  if (dosavgol){
    #retrieve paramaters
    wl<-XData_p[1,] # wl vector
    #Truncate wl to suit window size (w)
    #Need to adapt length of wl to result of Savitzky-Golay
    w_2=floor(w/2)
    wl <- wl[(w_2+1):(length(wl)-w_2)]
    
    X <- as.numeric(XData_p[2,]) # spectrum.
    X <- prospectr::savitzkyGolay(X,m,p,w)
    
    XData_p <-  rbind(wl,X) #rebuild spectrum with wl
    
  }
  #-----end----
  return(XData_p)
}