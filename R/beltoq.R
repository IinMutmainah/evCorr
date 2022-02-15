#' Computing a commonality function from a belief function.
#'
#'\code{beltopl} converts belief function to commonality function.
#' @param bel : belief function
#' @return q : commonality function
#'@references P. Smets and R. Kennes. The Transferable Belief Model.
#'          Artificial Intelligence, 66:191–243, 1994.
#'@author SS. Mutmainah, D. Mercier
#'
beltoq <- function(bel){
  return(btoq(bel));
}
