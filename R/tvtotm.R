#' Making the truth given as a matrix from the truth given as a vector
#'
#'\code{tmtotv} gives the truth given as a matrix from the truth given as a vector
#' @param tv true class of each object given as a vector: tv[i]=class of object i
#' @param nclass number of classes (some being possibly not in the vector)

#' @return tm: true class of each object given as a matrix such that:
#' tm[i,j] = 0 iff object i is not of class j
#' tm[i,j] = 1 iff object i is of class j
#'
#'@author S.Mutmainah, D. Mercier, F. Pichon, S. Hachour
#' @examples
#'   tv <- c(1,2,1,3);
#'   nclass <- 3;
#'   tvtotm(tv,nclass); ## change a vector to a matrix



# Return the truth given as a matrix from the truth given as a vector
#
# in:
#     - tv: true class of each object given as a vector: tv[i]=class of object i
#     - nclass: number of classes (some being possibly not in the vector)
# out:
#     - tm: true class of each object given as a matrix such that:
#             tm[i,j] = 0 iff object i is not of class j
#             tm[i,j] = 1 iff object i is of class j
#
# Example:
#   tv <- c(1,2,1,3);
#   nclass <- 3;
#   tvtotm(tv,nclass);
#   => return
#      [,1] [,2] [,3]
# [1,]    1    0    0
# [2,]    0    1    0
# [3,]    1    0    0
# [4,]    0    0    1
#
#


tvtotm <- function(tv,nclass){ #nclass should be taken from number of class in a whole data
  #nclass=length(unique(tv))
  nobj=length(tv); # Number of objets
  tm=matrix(0,nrow=nobj,ncol=nclass);
  for(i in 1:nobj){
    tm[i,tv[i]]=1;
  }
  return(tm);
}


# tvtotm <- function(nclass,tv){
#   nobj=length(tv);                       # Number of objets
#   #nclass=length(unique(tv));             # Number of classes
#   #nclass=max(tv)
#   #nclass = log2(ncol(data)) # data is a matrix containing mf
#   tm=matrix(0,nrow=nobj,ncol=nclass);
#   for(i in 1:nobj){
#     tm[i,tv[i]]=1;
#   }
#   return(tm);
# }
#


