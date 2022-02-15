#' Computing performace of classifier outputting partial decision (imprecise output)
#' using utility-discounted accuracies U65.
#'
#'\code{reward_U65} computes performance of classifier outputting partial outputs using U65.
#' @param x : tibble of partial predictions which are outputs of credal classifier.
#' @return sum of rewards of correct partial prediction of the classifier
#' @import stringr
#'@references G. Corani, M. Zafallon and D. D. Maua. Evaluating credal classifiers by
#'           utility discounted predictive accuracy.
#'           International Journal Approximation Reasoning, 53(8): 1282-1301, 2012

#'@author S. Mutmainah
#'


# Function to compute performace of classifier outputting partial decision (imprecise output)
# using utility-discounted accuracies U65
#
# Reference:
#           [1] G. Corani, M. Zafallon and D. D. Maua. Evaluating credal classifiers by utility discounted
#               predictive accuracy. International Journal Approximation Reasoning, 53(8): 1282-1301, 2012

# Authors: S. Mutmainah, D. Mercier, F. Pichon, S. Hachour

# in: x : tibble of partial predictions which are outputs of credal classifier
# out: sum of rewards of partial predictions

reward_U65<- function(x){
  sum_rew <- 0
  # get number of element in the set
  for(j in 1: nrow(x)){
    s <- nchar(x[j,1])
    if (isTRUE(stringr::str_detect(as.character(x[j,1]), as.character(x[j,2])))){  #check if element of prediction set contains class of hypothesis
      #sum_rew <- sum_rew + (1/nrow(x))*100 *(1.6/s - (0.6/(s^2)))
      sum_rew <- sum_rew + (1.6/s - (0.6/(s^2)))
    }
    else sum_rew <- sum_rew + 0
  }
  return(sum_rew)
}
