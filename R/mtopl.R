#' Computing a plausibility function from a mass function.
#'
#'\code{mtopl} converts mass function to plausibility function.
#' @param m : mass function
#' @return pl : plausibility function
#'@references P. Smets and R. Kennes. The Transferable Belief Model.
#'          Artificial Intelligence, 66:191–243, 1994.
#'@author S. Mutmainah, D. Mercier
#'

mtopl <- function(m){
  return(btopl(mtob(m)));
}

