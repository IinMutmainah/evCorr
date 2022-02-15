#' Computing a belief function from a commonality function.
#'
#'\code{qtobel} converts commonality function to belief function.
#' @param q : commonality function
#' @return bel : belief function
#' @import pracma
#'@references P. Smets and R. Kennes. The Transferable Belief Model.
#'          Artificial Intelligence, 66:191–243, 1994.
#'@author S. Mutmainah, D. Mercier
#'


# Compute FMT from q to bel
# in : q
# out : bel

qtobel <- function(q){
  return(btobel(qtob(q)));
}

