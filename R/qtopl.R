#' Computing a plausibility function from a commonality function.
#'
#'\code{qtopl} converts commonality function to plausibility function.
#' @param q : commonality function
#' @return pl : plausibility function
#' @import pracma
#'@references P. Smets and R. Kennes. The Transferable Belief Model.
#'          Artificial Intelligence, 66:191–243, 1994.
#' @references P. Smets. The application of the matrix calculus to belief functions.
#'         Interna-tional Journal of Approximate Reasoning, 31(1-2):1–30, 2002.
#'@author S. Mutmainah, D. Mercier
#'


# Compute FMT from q to pl using btom (See Smets 2002 Matrix Calculus)
# in : q
# out : pl
# inspired from Smets' qtopl.m

qtopl <- function(q){
  q[1] <- 0;
  return(abs(btom(q)));
}
