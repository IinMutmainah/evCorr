#' Computing a mass function from an implicability function.
#'
#'\code{btom} converts implicability function to mass function.
#' @param b : implicability function
#' @return m : mass function
#'@references P. Smets and R. Kennes. The Transferable Belief Model.
#'          Artificial Intelligence, 66:191–243, 1994.
#'@author SS. Mutmainah, D. Mercier
#'

btom<-function(b){
  lm <- length(b);
  natoms <- round(log2(lm));
  if(2^natoms == lm){
    for (step in 1:natoms){
	    i124 <- 2^(step-1);
	    i842 <- 2^(natoms+1-step);
	    i421 <- 2^(natoms - step);
	    dim(b) <- c(i124,i842);
	    b[,(1:i421)*2] <- b[,(1:i421)*2] - b[,(1:i421)*2-1];
    }
    dim(b) <- c(1,lm);
    return(b);
  }else{
    print("ACCIDENT in mtoq: length of input vector not OK: should be a power of 2");
  }
}
