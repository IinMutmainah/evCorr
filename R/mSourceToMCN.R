#' Computing corrected mass function using Contextual Negating (CN) from classifier's output.
#'
#'\code{mSourceToMCN} improves mass functions provided by EkNN classifier using Contextual Negating (CN).
#' @param mSource : mass function which is an output of the classifier.
#' @param params : a vector containing parameters used to correct the mass function.
#' @return m : corrected mass function.
#'@references F. Pichon, D. Mercier, E. Lefèvre, and F. Delmotte.
#'            Proposition and learn-ing of some belief function contextual correction mechanisms.
#'            InternationalJournal Approximate Reasoning, 72:4–42, 2016.
#'@author S. Mutmainah, D. Mercier, F. Pichon
#' @examples
#' mSource <- c(0, 0, 0.9, 0.1); ## output of EkNN (singletons and Omega)
#' params <- c(0.4, 0.9, 0.1); ##assigned for 3 classes
#' mSourceToMCN(mSource, params);
#' ## Output: 0.297, 0.441, 0.081, 0.081, 0.033, 0.049, 0.009, 0.009


mSourceToMCN <- function(mSource, params){
  nclass <- length(params) # number of classes
  mOutput <- matrix(data = mSource,nrow = 1,byrow = TRUE)
  sbba <- rep(0,2^nclass)
  sbba[2^nclass] <- params[1] # mass for \Omega
  sbba[2^nclass-1] <- 1-params[1] # mass for \overline{singleton}

  if(length(mSource) == 2^nclass){
    q0S <- mtoq0(mOutput)
  } else q0S <- mtoq0((mtoM(mOutput)))
  #-----compute corrected MF using 0-commonality functions----
  #q0S <- mtoq0((mOutput))
  Q0 <- q0S * mtoq0(sbba)
  for(n in 1:(nclass-1)){
    sbba <- rep(0,2^nclass)
    sbba[2^nclass] <- params[n+1]
    sbba[2^nclass-2^n] <- 1-params[n+1]
    Q0 <- Q0 * mtoq0(sbba)
  }
  m <- q0tom(Q0)
  return(m)

}

