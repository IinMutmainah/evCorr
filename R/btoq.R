#' Computing a commonality function from an implicability function.
#'
#'\code{btoq} converts implicability function to commonality function.
#' @param b : implicability function
#' @return q : commonality function
#'@references P. Smets and R. Kennes. The Transferable Belief Model.
#'          Artificial Intelligence, 66:191–243, 1994.
#'@author S. Mutmainah, D. Mercier
#'

btoq <- function(b){
  return(pltoq(btopl(b)));
}

