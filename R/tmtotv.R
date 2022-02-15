#' Making the truth given as a vector from the truth given as a matrix.
#'
#'\code{tmtotv} gives the truth given as a vector from the truth given as a matrix.
#' @param tm : true class of each object given as a matrix such that:
#' tm[i,j] = 0 iff object i is not of class j
#' tm[i,j] = 1 iff object i is of class j
#' @return tv: true class of each object given as a vector:
#' tv[i]=class of object i
#'
#'@author S.Mutmainah, D. Mercier
#' @examples
#' tm <- matrix(c(1,0,0, 0,1,0, 1,0,0, 0,0,1),nrow = 4, byrow = TRUE);
#' tmtotv(tm);
#' ## returns 1 2 1 3


# Return the truth given as a vector from the truth given as a matrix
#
# in:
#     - tm: true class of each object given as a matrix such that:
#             tm[i,j] = 0 iff object i is not of class j
#             tm[i,j] = 1 iff object i is of class j
# out:
#     - tv: true class of each object given as a vector: tv[i]=class of object i
#

tmtotv <- function(tm){
  return(apply(tm,1,function(x) which.max(x)));
}



