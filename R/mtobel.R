#' Computing a belief function from a mass function.
#'
#'\code{mtobel} converts mass function to belief function.
#' @param m : mass function (with 2^Omega elements, where Omega is the frame of discernment)
#' @return bel : belief function
#'@references P. Smets and R. Kennes. The Transferable Belief Model.
#'          Artificial Intelligence, 66:191–243, 1994.
#'@author S. Mutmainah, D. Mercier

# mtobel <- function(m){
#   if (any(m == 1)) m[m == 1] <- 0.9999999999999
#   return(btobel(mtob(m)));
# }

mtobel <- function(m){
  return(btobel(mtob(m)));
}

