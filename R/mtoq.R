#' Computing a commonality function from a mass function.
#'
#'\code{mtoq} converts mass function to commonality function.
#' @param m : mass function
#' @return q : commonality function
#'@references P. Smets and R. Kennes. The Transferable Belief Model.
#'          Artificial Intelligence, 66:191–243, 1994.
#'@author S. Mutmainah, D. Mercier
#'
# Function to compute communality function from mass function
mtoq <- function(m){
  lm = length(m)
  natoms <-round(log2(lm))

  if (2^natoms == lm)
  {
    for (step in 1:natoms)
    {
      i124 <- 2^(step-1)
      i842 <- 2^(natoms+1-step)
      i421 <- 2^(natoms - step)
      dim(m) <- c(i124,i842)
      m[,(1:i421)*2-1] <- m[,(1:i421)*2-1] + m[,(1:i421)*2]
    }
    dim(m) <- c(1,lm)
    return(m)
  }
  else print("length of input vector not OK: should be a power of 2")

}
