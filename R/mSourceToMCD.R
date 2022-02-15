#' Computing corrected mass function using Contextual Discounting (CD) from classifier's output.
#'
#'\code{mSourceToMCD} improves mass functions provided by EkNN classifier using Contextual Discounting (CD).
#' @param mSource : mass function which is an output of the classifier.
#' @param params : a vector containing parameters used to correct the mass function.
#' @return m : corrected mass function.
#'@references F. Pichon, D. Mercier, E. Lefèvre, and F. Delmotte.
#'            Proposition and learn-ing of some belief function contextual correction mechanisms.
#'            InternationalJournal Approximate Reasoning, 72:4–42, 2016.
#'@author S. Mutmainah, D. Mercier, F. Pichon
#' @examples
#' mSource <- c(0, 0, 0.9, 0.1) ## output of EkNN (singletons and Omega)
#' params <- c(0.4, 0.9, 0.1); ## assigned for 3 classes
#' mSourceToMCD(mSource, params);
#' ## output: 0, 0, 0, 0, 0.324, 0.486, 0.036, 0.154

mSourceToMCD <- function(mSource, params){
  nclass <- length(params) #number of classes
  mOutput <- matrix(data = mSource,nrow = 1,byrow = TRUE)
  nsbba <- rep(0,2^nclass)
  nsbba[1] <- params[1] #for the first focalset which is \emptyset
  nsbba[2] <- 1-params[1] #for the second focalset

  #-----compute corrected MF using disjunctive rule of combination----
  if (length(mSource) == 2^nclass){
    m <- dsum(mOutput,nsbba)
  } else m <- dsum(mtoM(mOutput),nsbba) #disjunctive rule of combination (output of source and nsbba)

  for(k in 1:(nclass-1)){
    nsbba <- rep(0,2^nclass)
    nsbba[1] <- params[k+1] #for the first focalset (\emptyset)
    nsbba[(2^k)+1] <- 1-params[k+1] #for singletons
    m <- dsum(m,nsbba) #disjunctive rule of combination (output of source and nsbba)
  }

  #------using implicability function------
  # if(length(mSource) == 2^nclass){
  #   bS <- mtob(mOutput)
  # }
  # else {
  #   bS <- mtob((mtoM(mOutput)))
  # }
  # B <- bS * mtob(nsbba)
  # for(n in 1:(nclass-1)){
  #   nsbba <- rep(0,2^nclass)
  #   nsbba[2^nclass] <- params[n+1]
  #   nsbba[2^n+1] <- 1-params[n+1]
  #   B <- B * mtoq(nsbba)
  # }
  # m <- btom(B)

  return(m)
}
