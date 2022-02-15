#' Computing normalized mass function.
#'
#'\code{mnormalized} computes normalized mass function.
#' @param m : mass function
#' @return normalized mass function
#'@references P. Smets and R. Kennes. The Transferable Belief Model.
#'          Artificial Intelligence, 66:191–243, 1994.
#'@author D. Mercier



# normalize m
# in : m
# out : m normalized

mnormalized <- function(m)
{
  if(m[1] == 1){
    print("ACCIDENT in mnormalized: you try to normalize a categorical mass on the empty set");
  }else{
    m=m/(1-m[1]);
    m[1]=0;
    return(m);
  }
}

