#' Computing a set of partial decision using strong dominance criterion.
#'
#'\code{mtosetsd} computes mass function to make a set of partial decision using strong dominance criterion so called interval dominance.
#' @param m : mass function
#' @return out_vec : output vector which is a set of partial decision.
#'@references L. Ma, T. Denoeux. Making set-valued predictions in evidential classification:
#'       A comparison of different approach. International Symposium in Imprecise Probability: Theories and Application
#'       ISIPTA 2019, 3-6 July 2019, Volume 103 of Proceding of Machine Learning Research, p. 276-285.
#'@references T. Denœux. Decision-making with belief functions: A review.
#'       InternationalJournal of Approximate Reasoning, 109:87–110, 2019.
#'@author S. Mutmainah, S. Hachour, F. Pichon, D. Mercier
#' @examples
#' m<-c(.1,.2,.5,.3); mtosetsd(m);
#' m<-c(.1,0,.5,.4); mtosetsd(m);


# Computing a set of predictions from m composed of singletons omega_i non dominated using
# the strong dominance criterion, meaning such that it does not exist omega_j with bel(omega_j) >= pl(omega_i)
# [Universe = Omega = set of acts and 0-1 utilities]
#
# Reference:
#   [1] L. Ma, T. Denoeux. Making set-valued predictions in evidential classification:
#       A comparison of different approach. International Symposium in Imprecise Probability: Theories and Application
#       ISIPTA 2019, 3-6 July 2019, Volume 103 of Proceding of Machine Learning Research, p. 276-285.
#   [2] T. Denoeux 2019 Decision-making with belief functions: A review
#
#
# in: a mass function m
# out: a vector of 0-1 elements s.t.
#            1 if singleton i is a non dominated singleton according to the strong dominance criterion
#            0 otherwise
#
# Authors: S. Mutmainah, D. Mercier, F. Pichon, S. Hachour
#
# Examples:
# m<-c(.1,.2,.5,.3); mtosetsd(m);
# m<-c(.1,0,.5,.4); mtosetsd(m);
#
mtosetsd <- function(m){
  bel <- mtobel(m);
  pl <- mtopl(m);
  lm <- length(m);
  natoms <- round(log2(lm));
  if (2^natoms == lm){
    out_vec <- rep(1,natoms);
    for (i in 1:natoms){
      for(j in 1:natoms){
        if(i!=j){
          # Each singleton is in position 2^(i-1)+1 in m,pl,bel
          if(isTRUE(bel[2^(j-1)+1] >= pl[2^(i-1)+1]) ){
            # Singleton in position 2^(i-1)+1 is dominated by Singleton in position 2^(j-1)+1
            out_vec[i]=0;
            break;
          }
        }
      }
    }
    return(out_vec);
  }else{
    print("ACCIDENT in mtosetsd: length of input vector not OK: should be a power of 2");
  }
}
