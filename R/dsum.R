#' Computing disjunctive combination of two mass functions (m1 and m2)
#'
#'\code{dsum} computes disjunctive rule of combination m1 and m2
#' @param m1 : mass function from source 1
#' @param m2 : mass function from source 2
#' @return new mass function resulting from disjunctive rule of combination
#'@references P. Smets and R. Kennes. The Transferable Belief Model.
#'          Artificial Intelligence, 66:191–243, 1994.
#'@author S. Mutmainah, D. Mercier
#'


# Computing disjunctive combination of m1 and m2
# in : m1 and m2
# out : disjunctive combination of m1 and m2

dsum<-function(m1,m2){
  b1 <- mtob(m1);
  b2 <- mtob(m2);
  return(btom(b1*b2));
}
