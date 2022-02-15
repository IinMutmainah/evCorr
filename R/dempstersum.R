#' Computing Dempster rule of combination of two mass functions (m1 and m2)
#'
#'\code{dempstersum} computes dempster rule of combination which is normalized conjunctive rule of combination
#' @param m1 : mass function from source 1
#' @param m2 : mass function from source 2
#' @return new mass function resulting from dempster rule of combination
#'@references P. Smets and R. Kennes. The Transferable Belief Model.
#'          Artificial Intelligence, 66:191–243, 1994.
#'@author S. Mutmainah, D. Mercier
#'


# Computing Dempster's combination of m1 and m2
# in : m1 and m2
# out : Dempster's combination of m1 and m2

dempstersum<-function(m1,m2){
  return(mnormalized(csum(m1,m2)));
}
