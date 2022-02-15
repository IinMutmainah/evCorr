#' Learning discount rate minimizing Epl by contextual reinforcement (CR)
#'
#'\code{tuningCR} optimizes reliability vector by minimizing Epl using
#' Equation 33 of reference [1] (equivalent of Equation 47 of [2] given for computing beta=1-alpha).
#' @param data : data from an information source, e.g., sensor, classifier. The matrix consists of lines containing mass functions
#             output by the source regarding the class of an object.
#' @param t : ground truth

#' @return A list with three elements:
#'   \describe{
#'   \item{alpha}{Computing contextual discount rate alpha minimizing the Euclidean distance between corrected contour functions
#'              (the discounted contour function) and the truth t using Equation 33 of [1]
#'              (equiv Equation 47 of [2] given for computing beta = 1-alpha)}
#'   \item{beta}{1-alpha}
#'   \item{mindist}{Minimum Euclidean distance obtained.}
#'  }
#' @references
#'   [1] D. Mercier, B. Quost, T. Denoeux
#'       Refined modeling of sensor reliability in the belief function framework using contextual discounting,
#'       Information Fusion, Vol. 9, Issue 2, pp 246-258, April 2008.
#'
#'   [2] F. Pichon, D. Mercier, É. Lefèvre, F. Delmotte
#'       Proposition and learning of some belief function contextual correction mechanisms,
#'       International Journal of Approximate Reasoning, Vol. 72, pp 4-42, May 2016
#'
#'   [3] Z. Elouedi, F. Mellouli, P. Smets
#'       Assessing sensor reliability for multisensor data fusion within the transferable belief model.
#'       IEEE Trans Syst Man Cybern B Cybern. 2004 Feb;34(1):782-7.
#' @import pracma
#'@author S.Mutmainah, D. Mercier, F. Pichon, S. Hachour
#' @examples ## with 4 objects (Data from sensor 1 of reference [3] Elouedi 2004):
#' data1=matrix(c(0,0,0,0,0.5,0,0.3,0.2, 0,0,0.5,0,0.2,0,0,0.3,0,0,0.4,0,0,0.6,
#' 0,0,0,0,0,0,0,0.6,0.4,0),nrow=4,byrow = TRUE);
#' t <- matrix(c(1,0,0, 0,1,0, 1,0,0, 0,0,1), nrow = 4, byrow = TRUE);
#' tuningCR(data1,t);
#' ## returns
#'  ##     $alpha: 0.05882353 0.33884298 0.61685824
#'  ##     $beta: 0.9411765 0.6611570 0.3831418
#'  ##     $mindist: 2.334991



# Tuning with contextual reinforcement a source from output data (BBAs)
# using an Euclidean distance between the contour function and the truth
#
# It computes alpha using Equation 52 of reference [1]
#
# References:
#   [1] F. Pichon, D. Mercier, É. Lefèvre, F. Delmotte
#       Proposition and learning of some belief function contextual correction mechanisms,
#       International Journal of Approximate Reasoning, Vol. 72, pp 4-42, May 2016
#   [2] Z. Elouedi, F. Mellouli, P. Smets
#       Assessing sensor reliability for multisensor data fusion within the transferable belief model.
#       IEEE Trans Syst Man Cybern B Cybern. 2004 Feb;34(1):782-7.
#
# in:
#     - data: data (BBAs) from a sensor, a matrix where each line contains a mass function
#             output by the source regarding the class of an object.
#     - t: true class of each object, a matrix such that:
#             t[i,j] = 0 iff object i is not of class j
#             t[i,j] = 1 iff object i is of class j
# out:
#     - alpha: Computing contextual reinforcement rate alpha minimizing the Euclidean distance between alpha^cf
#              (the reinforced contour function) and the truth t using Equation 52 of [1]
#     - beta: 1-alpha
#     - mindist: Minimum Euclidean distance obtained
#

tuningCR <- function(data,t){
  n=nrow(data);   # number of objects = number of rows of data matrix
  c=ncol(t);      # number of classes = number of colomns of the truth

  # Converting data of BBAs to data of contour functions cf
  cf=matrix(0,nrow=n,ncol=c)
  for(i in 1:n){
    cf[i,]=pltocf(mtopl(data[i,]));
  }

  # Computing contextual reinforcement rate beta=1-alpha minimizing the Euclidean distance between
  # the reinforced contour function and the truth t using Equation 52 of [1]
  C=diag(cf[1,]);
  d=matrix(t[1,],ncol=1);
  if (n>1){
    for(i in 2:n){
      C=rbind(C,diag(cf[i,]));
      d=rbind(d,matrix(t[i,],ncol=1));
    }
  }
  beta=pracma::lsqlincon(C,d,lb=0,ub=1);
  return(list("alpha"=1-beta, "beta"=beta, "distmin"=sum((C %*% beta - d)^2)));
}



