#' Computing a contour function from a plausibility function.
#'
#'\code{pltocf} converts plausibility function to contour function.
#' @param pl : plausibility function
#' @return cf : contour function
#' @import pracma
#'@references P. Smets and R. Kennes. The Transferable Belief Model.
#'          Artificial Intelligence, 66:191–243, 1994.
#'@author S. Mutmainah, D. Mercier
#'


# Computing contour function cf (pl on singletons) from pl
# in : pl
# out : cf

pltocf <- function(pl){
  lm <- length(pl);
  natoms <- round(log2(lm));
  if (2^natoms == lm){
    cf <- rep(0,natoms);
    for (i in 1:natoms){
      cf[i] <- pl[2^(i-1)+1];
    }
    return(cf);
  }else{
    print("ACCIDENT in pltocf: length of input vector not OK: should be a power of 2");
  }
}

