#' Computing pignistic probability (betp) from a mass function.
#'
#'\code{mtobetp} computes pignistic probability (betp) from mass function.
#' @param m : mass function (with 2^Omega elements, where Omega is the frame of discernment)
#' @return betp : pignistic probability
#'@references P. Smets and R. Kennes. The Transferable Belief Model.
#'          Artificial Intelligence, 66:191–243, 1994.
#'@author S. Mutmainah, D. Mercier

# Computing BetP from the m vector
# in : m
# out : BetP vector (order a,b,c,...)
# beware: not optimalize, so can be slow for >10 atoms
# inspired from Smets betp.m

mtobetp <- function(m){
  lm <- length(m);
  natoms <- round(log2(lm));
  if (2^natoms == lm){
    out <- 0;
    if (m[1] == 1){
      out <- rep(1,natoms)/natoms;
    }else{
      betp <- rep(0,natoms);
      for (i in 2:lm){
        x <- as.numeric(intToBits(i-1)[1:natoms]); # x contains the 1 and 0 of i-1, for a,b,c...
        betp <- betp + m[i]/sum(x)*x;
      }
      out <- betp/(1-m[1]);
    }
    return(out);
  }
  else
  {
    print("ACCIDENT in mtobetp: length of input vector not OK: should be a power of 2");
  }
}

