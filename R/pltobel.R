#' Computing a belief function from a plausibility function.
#'
#'\code{pltobel} converts plausibility function to belief function.
#' @param pl : plausibility function
#' @return bel : belief function
#'@references P. Smets and R. Kennes. The Transferable Belief Model.
#'          Artificial Intelligence, 66:191–243, 1994.
#'@author S. Mutmainah, D. Mercier
#'

# Computing bel from pl using pltob
# in : pl
# out : bel

pltobel <- function(pl){
  return(btobel(pltob(pl)));
}
