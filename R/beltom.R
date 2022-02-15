#' Computing a mass function from a belief function.
#'
#'\code{beltom} converts belief function to mass function.
#' @param bel : belief function
#' @return m : mass function
#'@references P. Smets and R. Kennes. The Transferable Belief Model.
#'          Artificial Intelligence, 66:191–243, 1994.
#' @import pracma
#'@author S. Mutmainah, D. Mercier


beltom <- function(bel){
  return(btom(beltob(bel)));
}
