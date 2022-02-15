#' Discounting mass functions using single value of discount rate alpha
#'
#'\code{disc} discounts mass function using single value of discount rate alpha
#' @param m : mass function that will be discounted
#' @param alpha : discount rate
#' @return discounted mass function
#'@references P. Smets and R. Kennes. The Transferable Belief Model.
#'          Artificial Intelligence, 66:191–243, 1994.
#'@author S. Mutmainah, D. Mercier
#'

# Discounting m with alpha in [0,1]
# in : m vector, alpha in [0,1]
# out : mdisc (m discounted)
# inspired from Smets disc.m

disc <- function(m,alpha){
  mdisc <- m * (1-alpha);
  mdisc[length(m)] <- mdisc[length(m)]+alpha;
  return(mdisc);
}
