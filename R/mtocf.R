#' Computing contour function from a mass function.
#'
#'\code{mtocf} converts mass function to contour function.
#' @param m : mass function (with 2^Omega elements, where Omega is the frame of discernment)
#' @return cf : contour function
#'@references P. Smets and R. Kennes. The Transferable Belief Model.
#'          Artificial Intelligence, 66:191–243, 1994.
#'@author S. Mutmainah, D. Mercier


mtocf <- function(m){
  return(pltocf(mtopl(m)));
}

