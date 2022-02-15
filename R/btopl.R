#' Computing a plausibility function from an implicability function.
#'
#'\code{btopl} converts implicability function to plausibility function.
#' @param b : implicability function
#' @return pl : plausibility function
#' @import pracma
#'@references P. Smets and R. Kennes. The Transferable Belief Model.
#'          Artificial Intelligence, 66:191–243, 1994.
#'@author S. Mutmainah, D. Mercier
#'

#require(pracma);

btopl <- function(b){
  lm <- length(b);
  natoms <- round(log2(lm));
  if (2^natoms == lm){
    dim(b) <- c(1,lm);
    b <- b[lm]-pracma::fliplr(b); # b[lm]=1 normally
    b[1] <- 0;
    return(b);
  }else{
    print("ACCIDENT in btopl: length of input vector not OK: should be a power of 2");
  }
}
