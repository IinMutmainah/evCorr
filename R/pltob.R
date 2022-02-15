#' Computing an implicability  function from a plausibility function.
#'
#'\code{pltob} converts plausibility function to implicability function.
#' @param pl : plausibility function
#' @return b : implicability function
#' @import pracma
#'@references P. Smets and R. Kennes. The Transferable Belief Model.
#'          Artificial Intelligence, 66:191–243, 1994.
#'@author S. Mutmainah, D. Mercier
#'



pltob <-function(pl){

  lm <- length(pl);
  natoms <- round(log2(lm));
  if (2^natoms == lm){
    dim(pl) <- c(1,lm);
    pl <- 1 - pracma::fliplr(pl);
    return(pl);
  }else{
    print("ACCIDENT in pltocf: length of input vector not OK: should be a power of 2");
  }
}
