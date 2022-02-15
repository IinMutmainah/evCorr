#' Computing Betpl (normalized contour function) from a contour function which is plausibility on singletons.
#'
#'\code{cftobetpl} computes contour function to betpl.
#' @param cf : contour function
#' @return betpl : normalized contour function
#'@references B. R. Cobb and P. P. Shenoy.  On the plausibility transformation method
#'          for translating belief function models to probability models.
#'          InternationalJournal of Approximate Reasoning, 41(3):314–330, 2006.
#'@author S. Mutmainah, D. Mercier, F. Pichon, S. Hachour
#'


cftobetpl <- function(cf){
  s<-sum(cf);
  return(cf/s);
}

