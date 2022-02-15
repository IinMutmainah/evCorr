#' Computing normalized contour function cf (betpl) from a mass function.
#'
#'\code{mtobetpl} computes betpl from mass function.
#' @param m : mass function (with 2^Omega elements, where Omega is the frame of discernment)
#' @return betpl : normalized contour function.
#'@references P. Smets and R. Kennes. The Transferable Belief Model.
#'          Artificial Intelligence, 66:191–243, 1994.
#'@author S. Mutmainah, D. Mercier


mtobetpl <- function(m)
{
  pl <- mtopl(m);
  cf <- pltocf(pl);
  s <- sum(cf);
  return(cf/s);
}

