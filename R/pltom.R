#' Computing a mass function from a plausibility function.
#'
#'\code{pltom} converts plausibility function to mass function.
#' @param pl : plausibility function
#' @return m : mass function
#' @import pracma
#'@references P. Smets and R. Kennes. The Transferable Belief Model.
#'          Artificial Intelligence, 66:191–243, 1994.
#'@author S. Mutmainah, D. Mercier
#'

pltom <-function(pl){
  return(btom(pltob(pl)));
}
