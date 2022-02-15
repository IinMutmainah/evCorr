#' Computing corrected mass function using Contextual Reinforcement (CR) from classifier's output.
#'
#'\code{mSourceToMCR} improves mass functions provided by EkNN classifier using Contextual Reinforcement (CR).
#' @param mSource : mass function which is an output of the classifier.
#' @param params : a vector containing parameters used to correct the mass function.
#' @return m : corrected mass function.
#'@references F. Pichon, D. Mercier, E. Lefèvre, and F. Delmotte.
#'            Proposition and learn-ing of some belief function contextual correction mechanisms.
#'            InternationalJournal Approximate Reasoning, 72:4–42, 2016.
#'@author S. Mutmainah, D. Mercier, F. Pichon
#' @examples
#' mSource <- c(0, 0, 0.9, 0.1); ## output of EkNN (singletons and Omega)
#' params <- c(0.4, 0.9, 0.1); ## assigned for 3 classes
#' mSourceToMCR(mSource, params);
#' ## Output: 0.8154, 0.0036, 0.0486, 0.0324, 0.0906, 4e-04, 0.0054, 0.0036


mSourceToMCR <- function(mSource, params){
  nclass <- length(params) # number of classes
  mOutput <- matrix(data = mSource,nrow = 1,byrow = TRUE)
  sbba <- rep(0,2^nclass)
  sbba[2^nclass] <- params[1]
  sbba[2^nclass-2^0] <- 1-params[1]
  #------compute corrected MF using commonality functions----
  if(length(mSource) == 2^nclass){
    qS <- mtoq(mOutput)
  } else qS <- mtoq((mtoM(mOutput)))
  Q <- qS * mtoq(sbba)
  for(n in 1:(nclass-1)){
    sbba <- rep(0,2^nclass)
    sbba[2^nclass] <- params[n+1]
    sbba[2^nclass-2^n] <- 1-params[n+1]
    Q <- Q * mtoq(sbba)
  }
  m <- qtom(Q)

  return(m)
}

