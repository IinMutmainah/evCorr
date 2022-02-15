#' Computing a 0-commonality function from a mass function.
#'
#'\code{mtoq0} converts mass function to 0-commonality function.
#' @param m : mass function
#' @return q0 : 0-commonality function
#'@references P. Smets and R. Kennes. The Transferable Belief Model.
#'          Artificial Intelligence, 66:191–243, 1994.
#'  @import pracma
#'@author S. Mutmainah, F. Pichon


mtoq0 <- function(m)
{

  lm = length(m)
  natoms = round(log2(lm))
  #bel <- mtobel(m) # change m to bel
  A <- matrix(c(1, -1, 1, 1),2,2)
  B <- matrix(c(1),1,1)

  if (2^natoms == lm)
  {
    for (i in 1:natoms) {

      B <-pracma::kron(A,B)

    }
    dim(m) <- c(lm,1)
    result <- B %*% m
    result <- round(result,digits = 4)
    dim(result) <- c(1,lm)
    return(result)
  }
  else print("length of input vector not OK: should be a power of 2")

}
