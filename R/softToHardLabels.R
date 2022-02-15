#' Generate hard labels from soft labels
#'
#'\code{softToHardLabels} returns a hard label from a soft one. The output is a matrix where an instance with the most plausible is chosen.
#'@references B. Quost, T. Denoeux and S. Li. Parametric Classification with
#'       Soft Labels using the Evidential EM Algorithm. Linear Discriminant Analysis vs.
#'       Logistic Regression. Advances in Data Analysis and Classification, Vol. 11, Issue 4, pp 659-690, 2017.
#'
#'       O. Kanjanatarakul, S. Kuson and T. Denoeux. An Evidential K-Nearest Neighbor Classifier
#'       based on Contextual Discounting. In: Destercke S., Denoeux T., Cuzzolin F., Martin A. (eds),
#'       Belief Functions: Theory and Applications. BELIEF 2018. Lecture Notes in Computer Science,
#'       vol 11069. Springer, pages 155-162, September 2018.
#'
#' @param cf : soft labels as contour functions, a matrix of size <number of objects> x <number of classes>
#' @return vector of hard labels of length <number of objects>.
#' @author  S. Mutmainah, D. Mercier, F. Pichon, S. Hachour
#' @examples ## using Iris dataset
#' data(iris);
#' cf<-hardToSoftLabels(iris[,5],nclass = 3);
#' softToHardLabels(cf);

# Return hard labels from soft labels (contour functions)
# One class picked at random in case of a tie (more than one class having the maximum of plausibility)
#
# in: soft labels as contour functions, a matrix of size <number of objects> x <number of classes>
#
# out: vector of hard labels of length <number of objects>
#
# Example: data(iris); cf<-hardToSoftLabels(iris[,5]); softToHardLabels(cf);

softToHardLabels <- function(cf){
  return(apply(cf,1,function(x) sample(which(x==max(x)), 1)));
}
