#' Computing corrected pl from mass functions as source outputs
#'
#'\code{plSourceToplCN} computes corrected pl from mass functions as source outputs.
#' @param mSource : mass functions as source outputs.
#' @param beta : reliability vector of contextual negating (CN).
#' @return pl : Corrected output.
#'@references F. Pichon, D. Mercier and E. Lefevre, F. Delmotte.
#'           Proposition and learning of somebelief function contextual correction mechanims.
#'           International Journal of Approximate Reasoning, 72:4-42, 2016.

#'@author S. Mutmainah
#' @examples
#' mSource <- c(0, 0, 0.9, 0.1) ##output of EkNN (singletons and Omega)
#' beta <- c(0.4, 0.9, 0.1) ##reliability vector assigned for 3 classes
#' ## return corrected pl by CN: 0.58, 0.18, 0.1


plSourceToplCN <- function(mSource, beta){
  nclass <- length(beta)
  mOutput <- matrix(data = mSource,nrow = 1,byrow = TRUE)
  plS <-mtopl(mtoM(mOutput))
  pl <- matrix(data = NA,ncol = nclass, byrow = TRUE)
  for(k in 0:(nclass-1)){
    pl[k+1] <- 0.5 + (plS[2^k+1] - 0.5)*(2*beta[k+1]-1)
  }
  return(pl)
}
