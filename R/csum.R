#' Computing conjunctive combination of two mass functions (m1 and m2)
#'
#'\code{csum} computes conjunctive rule of combination m1 and m2
#' @param m1 : mass function from source 1
#' @param m2 : mass function from source 2
#' @return new mass function resulting from conjunctive rule of combination
#'@references P. Smets and R. Kennes. The Transferable Belief Model.
#'          Artificial Intelligence, 66:191–243, 1994.
#'@author S. Mutmainah, D. Mercier
#'


# Computing conjunctive combination of m1 and m2
# in : m1 and m2
# out : conjunctive combination of m1 and m2

csum<-function(m1,m2){
  q1 <- mtoq(m1);
  q2 <- mtoq(m2);
  return(qtom(q1*q2));
}
