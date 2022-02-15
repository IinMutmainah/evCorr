#' Computing a set of prediction using the maximality criterion.
#'
#'\code{mtosetmax} computes mass function to make a set of prediction using the maximality criterion.
#' @param m : mass function
#' @return out_vec : output vector which is a set of the prediction
#' @import pracma
#'@references Denoeux 2019 Decision-making with belief functions: A review (Section 5.1)
#'@references Ma and Denoeux 2021 Partial classification in the belief function framework (Section 3.4)
#'@author D. Mercier
#' @examples
#' m<-c(.1,.2,.5,.3); mtosetmax(m);
#' m<-c(.1,0,.5,.4); mtosetmax(m);

# Computing a set of predictions from m composed of singletons omega_i non dominated using
# the maximality criterion, meaning it does not exist omega_j s.t. the lower expectation under m of
# the gamble in favor of omega_j - omega_i is positive, i.e. it does not exist omega_j s.t. if one decision
# maker selects omega_j and another selects omega_i, the former is expected to gain a higher utility.
# [Universe = Omega = set of acts and 0-1 utilities]
#
# References:
#   Denoeux 2019 Decision-making with belief functions: A review (Section 5.1)
#   Ma and Denoeux 2021 Partial classification in the belief function framework (Section 3.4)
#
# in: a mass function m
# out:
# $out_vec : a vector of 0-1 elements s.t.
#            1 if singleton i is a non dominated singleton according to the maximality criterion
#            0 otherwise
#
# $MatrixE : Matrix of lower expectations
#
# Authors: S. Mutmainah, S. Hachour, F. Pichon, D. Mercier
#
# Examples:
# m<-c(.1,.2,.5,.3); mtosetmax(m);
# m<-c(.1,0,.5,.4); mtosetmax(m);
#
mtosetmax <- function(m){
  lm <- length(m);
  natoms <- round(log2(lm));
  if (2^natoms == lm){
    out_vec <- rep(1,natoms);

    # From Denoeux 2019 Decision-making with belief functions: A review (Section 5.1)
    # With Universe = Omega = set of acts
    # Xi(omega_j) : utility to decide omega_i while the truth is omega_j
    # With 0-1 utilities
    # Xi(omega_j)=1 iff i=j, 0 otherwise
    # Then
    # Xi - Xj (omega_i) = 1
    # Xi - Xj (omega_j) = -1
    # Xi - Xj (omega_k) = 0 for any k different from i and j
    # Lower expectation under m of Xi - Xj := Eij = Sum_A m(A) min_{omega_k in A} Xi - Xj (omega_k)
    # Matrix E = [Eij] i different j, and Eii is set arbitrary to -1 for all i to compute non dominated omega_i
    # A non dominated singleton omega_i will have its respective column [E.i] with no positive values (no Eki are >= 0)

    # Initialization of matrix E
    E = pracma::ones(natoms)*(-1);

    # Computing matrix E = [Eij]
    for (i in 1:natoms){
      for(j in 1:natoms){
        if(i!=j){
          # Computing Eij = Sum_A m(A) min_{omega_k in A} Xi - Xj (omega_k)
          E[i,j]=0;
          # iteration on the focal elements fe (diff from empty set) of m
          for (fe in 2:lm){
            if(m[fe]!=0){ # not useful to make next computations otherwise
              minXij <- 2; # init to an impossible value strictly higher that 1
              # iteration on the omega_k in fe
              for(k in 1:natoms){
                if((fe-1) %/% (2^(k-1)) %% 2 == 1){# if omega_k in fe
                  #Xi - Xj (omega_k) possible min
                  Xij=0;
                  if(k==i) Xij <- 1;
                  if(k==j) Xij <- -1;
                  if(Xij<minXij) minXij <- Xij;
                }
              }
              E[i,j] <- E[i,j] + m[fe]*minXij;
            }
          }
        }
      }
    }

    # Computing out_vec of non dominated singletons
    for (i in 1:natoms){
      if(max(E[,i])>=0){
        #omega_i is dominated by an omega_k s.t. Eki >= 0
        out_vec[i]=0;
      }
    }

    # To output a correct Matrix E
    for (i in 1:natoms){
      E[i,i] <- 0;
    }

    return(list("out_vec" = out_vec,"MatrixE"=E));
  }else{
    print("ACCIDENT in mtosetmax: length of input vector not OK: should be a power of 2");
  }
}
