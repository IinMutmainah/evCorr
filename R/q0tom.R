#' Computing a mass function from a 0-commonality function.
#'
#'\code{q0tom} converts 0-commonality function to mass function.
#' @param q0 : 0-commonality function
#' @return m : mass function
#' @import pracma
#'@references P. Smets and R. Kennes. The Transferable Belief Model.
#'          Artificial Intelligence, 66:191–243, 1994.
#'@author S. Mutmainah, F. Pichon
#'


q0tom <- function(q0)
{

  lm = length(q0)
  natoms = round(log2(lm))
  A <- matrix(c(1, -1, 1, 1),2,2)
  B <- matrix(c(1),1,1)

  if (2^natoms == lm)
  {
    for (i in 1:natoms) {

      B <-pracma::kron(A,B)

    }
    dim(q0) <- c(lm,1)
    result <- pracma::inv(B) %*% q0
    #hasil <- beltom(hasil) #change bel to m
    result <- round(result,digits = 4)
    dim(result) <- c(1,lm)
    return(result)
  }
  else print("length of input vector not OK: should be a power of 2")

}
