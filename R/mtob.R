#' Computing an implicability function from a mass function.
#'
#'\code{mtob} converts mass function to implicability function.
#' @param m : mass function (with 2^Omega elements, where Omega is the frame of discernment)
#' @return b : implicability function
#'@references P. Smets and R. Kennes. The Transferable Belief Model.
#'          Artificial Intelligence, 66:191–243, 1994.
#'@author S. Mutmainah, D. Mercier


mtob<-function(m){
  lm <- length(m);
  natoms <- round(log2(lm));
  if (2^natoms == lm){
    for (step in 1:natoms){
	    i124 <- 2^(step-1);
	    i842 <- 2^(natoms+1-step);
	    i421 <- 2^(natoms - step);
	    dim(m) <- c(i124,i842);
	    m[,(1:i421)*2] <- m[,(1:i421)*2] + m[,(1:i421)*2-1];
    }
    dim(m) <- c(1,lm);
    return(m);
  }else{
    print("ACCIDENT in mtob: length of input vector not OK: should be a power of 2");
  }
}

