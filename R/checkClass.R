#' Checking if number of class in training set is proper to the number of class of the object in universe
#'
#'\code{checkClass} returns approprite matrix containing mass function which is output of classifier.
#' @param m_train  a matrix which are mass functions (e.g., EkNN output consisting of mass functions on singletons and the universe).
#' @param uniqueClass  vector of the unique class in universe
#' @param ytrain  labels of training set (ground truth)
#' @return a matrix which as mass functions with correct position according to the actual number of classes in universe.
#' @export
#' @import R.utils
#'@author S. Mutmainah
#' @examples
#' m_train = matrix(c(0.3,0.6,0.1,0.7,0.2,0.1),nrow=2,byrow=TRUE) ## for 2 objects
#' uniqueClass = c(1,2,3) ## the unique class in universe
#' ytrain = c(1,2) ## the ground truth of the 2 objects in training data
#' checkClass(m_train, uniqueClass, ytrain);
#' ## note that this function is suitable to output of evidential classifier
#' ## such as Evidential KNN, Evidential NN consiting of singletons and the universe.


checkClass <- function(m_train,uniqueClass,ytrain){
  #get unseen class in training set
  dif = setdiff(uniqueClass,unique(ytrain))
  if (length(dif) == 0) {
    m_tr = m_train #output of classifier
  } else {
    n_mass = ncol(m_train) + length(dif)
    m_tr = matrix(data = NA,ncol = n_mass, nrow = nrow(m_train), byrow = TRUE)
    #insert 0 in column for unseen class in training set and shift mass function
    for(i in 1:nrow(m_train)){
      m_tr[i,] = R.utils::insert(m_train[i,],at = dif, values = rep(0,length(dif)))
    }
  }
  return(m_tr)

}
