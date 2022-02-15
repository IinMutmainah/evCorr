#' Computing a commonality function from a plausibility function.
#'
#'\code{pltoq} converts plausibility function to commonality function.
#' @param pl : plausibility function
#' @return q : commonality function
#' @import pracma
#'@references P. Smets and R. Kennes. The Transferable Belief Model.
#'          Artificial Intelligence, 66:191–243, 1994.
#'@author S. Mutmainah, D. Mercier
#'


# Computing q from pl
# in : pl
# out : q
# inspired from Smets pltoq

pltoq <-function(pl){
  out <- abs(btom(pl));
  out[1] <- 1;
  return(out);
}
