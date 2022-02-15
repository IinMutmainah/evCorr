#' Computing an implicability function from a commonality function.
#'
#'\code{qtob} converts commonality function to implicability function.
#' @param q : commonality function
#' @return b : implicability function
#' @import pracma
#'@references P. Smets and R. Kennes. The Transferable Belief Model.
#'          Artificial Intelligence, 66:191–243, 1994.
#'@author S. Mutmainah, D. Mercier
#'


# Compute FMT from q to b
# in : q
# out : b
# inspired from Smets qtob.m

qtob <- function(q){
  return(pltob(qtopl(q)));
}

