#' Computing a plausibility function from a belief function.
#'
#'\code{beltopl} converts belief function to plausibility function.
#' @param bel : belief function
#' @return m : plausibility function
#'@references P. Smets and R. Kennes. The Transferable Belief Model.
#'          Artificial Intelligence, 66:191–243, 1994.
#'@author S. Mutmainah, D. Mercier

beltopl <- function(bel){

  return(btopl(bel));
}
