#' Computing an implicability function from a belief function.
#'
#'\code{beltob} converts belief function to implicability function.
#' @param bel : belief function
#' @return b : implicability function
#'@references P. Smets and R. Kennes. The Transferable Belief Model.
#'          Artificial Intelligence, 66:191–243, 1994.
#' @import pracma
#'@author S. Mutmainah, D. Mercier

beltob <- function(bel){
  if(bel[1] > 0){
    print("ACCIDENT in beltob: bel on the emptyset cannot be greater than 0");
  }else{
    lm <- length(bel);
    natoms <- round(log2(lm));
    if(2^natoms == lm){
      m <- 1-bel[length(bel)]; # mass on the empty set
      bel <- bel+m;
      return(bel);
    }else{
      print("ACCIDENT in beltob: length of input vector not OK: should be a power of 2");
    }
  }
}
