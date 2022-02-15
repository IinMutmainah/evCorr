#' Making a proper matrix of mass functions on the whole frame
#'
#'\code{mtoM} Return a matrix of mass functions on the whole frame
#' from a matrix of mass functions given on singletons and the universe
#' @param m : mass function (with 2^Omega elements, where Omega is the frame of discernment)
#' @return M : matrix of mass functions on the whole frame
#'@author D. Mercier


# Return a matrix of mass functions on the whole frame from a matrix of mass functions given on singletons and the universe
# in :
#     - m : a matrix of mass functions on singletons and the universe
# out :
#     - M : a matrix of mass functions on the whole frame
#


mtoM <- function(m){
  n <- nrow(m);
  c <- ncol(m);
  M <- matrix(0,nrow=n,ncol=2^(c-1));
  M[,2^(c-1)] <- m[,c];
  for(i in 1:(c-1)){
    M[,2^(i-1)+1] <- m[,i];
  }
  return(M);
}
