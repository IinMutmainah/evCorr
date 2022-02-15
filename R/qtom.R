#' Computing a mass function from a commonality function.
#'
#'\code{qtom} converts commonality function to mass function.
#' @param q : commonality function
#' @return m : mass function
#' @import pracma
#'@references P. Smets and R. Kennes. The Transferable Belief Model.
#'          Artificial Intelligence, 66:191–243, 1994.
#'@author S. Mutmainah, D. Mercier
#'


# Function to compute MF from commonality function
qtom <- function(q)
{
  lm <- length(q)
  natoms <- round(log2(lm))
  if (2^natoms == lm)
  {
    for (step in 1:natoms)
    {
      i124 <- 2^(step-1)
      i842 <- 2^(natoms + 1 - step)
      i421 <- 2^(natoms - step)
      dim(q) <- c(i124,i842)
      q[,(1:i421)*2-1] <- q[,(1:i421)*2-1] - q[,(1:i421)*2]

    }
    dim(q) <- c(1,lm)
    return(q)
  }
  else print ("length of input vector not OK: should be a power of 2")
}
