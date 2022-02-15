#' Computing a belief function from an implicability function.
#'
#'\code{btobel} converts implicability function to belief function.
#' @param b : implicability function
#' @return bel : belief function
#'@references P. Smets and R. Kennes. The Transferable Belief Model.
#'          Artificial Intelligence, 66:191–243, 1994.
#'@author S. Mutmainah, D. Mercier
#'

btobel <- function(b){
  if(b[1] == 1){
    print("ACCIDENT in btobel: you try to normalize a categorical mass on the empty set");
  }else{
    lm <- length(b);
    natoms <- round(log2(lm));
    if(2^natoms == lm){
      b <- b-b[1];
      return(b);
    }else{
      print("ACCIDENT in btobel: length of input vector not OK: should be a power of 2");
    }
  }
}
