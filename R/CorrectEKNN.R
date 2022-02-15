#' Correct EkNN classifier using beta vector containing parameters minimizing Epl
#'
#'\code{CorrectEKNN} improves outputs of EkNN classifier using beta vector learnt from data for training corrections.
#' @param x_train : predictors of data for training classifier
#' @param y_train : ground truths of data for training classifier
#' @param xtuning_train : predictors of data for training corrections
#' @param ytuning_train : ground truthts of data for training corrections
#' @param xtuning_test : predictors of data for testing
#' @param ytuning_test : ground truths of data for testing
#' @param nclass : number of class of object in universe
#' @param kn : number of neighbor
#' @return A list with mass functions output by EkNN and two elements of each correction (CD, CR, CN):
#'   \describe{
#'   \item{MF}{Outputs of EkNN classifier}
#'   \item{beta_CD}{parameters of CD learnt from data for training corrections}
#'   \item{CD}{Corrected mass functions by CD for the test data. The last column corresponds to the mass
#'   assigned to the whole set of classes. The other ones correspond
#'   to the mass assigned to each class.}
#'   \item{beta_CR}{parameters of CR learnt from data for training corrections}
#'   \item{CR}{Corrected mass functions by CR for the test data. The last column corresponds to the mass
#'   assigned to the whole set of classes. The other ones correspond
#'   to the mass assigned to each class.}
#'   \item{beta_CN}{parameters of CR learnt from data for training corrections}
#'   \item{CN}{Corrected mass functions by CR for the test data. The last column corresponds to the mass
#'   assigned to the whole set of classes. The other ones correspond
#'   to the mass assigned to each class.}
#'  }
#'@references F. Pichon, D. Mercier, E. Lefèvre, and F. Delmotte.
#'            Proposition and learn-ing of some belief function contextual correction mechanisms.
#'            InternationalJournal Approximate Reasoning, 72:4–42, 2016.
#'@author S. Mutmainah
#' @export
#' @import evclass


CorrectEKNN <- function(x_train,y_train,xtuning_train,ytuning_train,xtuning_test,ytuning_test, nclass, kn=5){

  # Learning EkNN
  param0 <- invisible(evclass::EkNNinit(x_train,y_train));
  options <- list(maxiter=300,eta=0.1,gain_min=1e-5,disp=FALSE);
  fit <- evclass::EkNNfit(x_train,y_train,param=param0,K=kn,options=options);

  # Learning Corrections for EKNN
  eknn_outputs_train <- EkNNval_dfi(x_train,y_train,xtuning_train,K=kn,ytuning_train,fit$param);
  m_source <- eknn_outputs_train$m;     # EkNN gives only mass functions on singletons and the universe
  M_source <- mtoM(m_source);           # To have mass functions given on the whole frame

  # Compute \beta using contextual corrections minimizing the Euclidean distance
  beta_CD_d <- tuningCD_rev3(data = M_source,t=tvtotm(nclass,ytuning_train))$beta #learning beta using contextual discounting
  beta_CR_d <- tuningCR(data = M_source,t=tvtotm(nclass,ytuning_train))$beta #learning complement of beta using contextual reinforcement
  beta_CN_d <- tuningCN(data = M_source,t=tvtotm(nclass,ytuning_train))$beta #learning complement of beta using contextual negating

  #Testing EKNN
  eknn_outputs_test <- EkNNval_dfi(x_train,y_train,xtuning_test,K=kn,ytuning_test,fit$param);
  output <- eknn_outputs_test$m
  M_output <- mtoM(output)

  #Compute corrected output in form of MF =====
  CorrMF_CD <- matrix(data = NA, nrow = nrow(output), ncol = 2^nclass, byrow = TRUE)
  CorrMF_CR <- matrix(data = NA, nrow = nrow(output), ncol = 2^nclass, byrow = TRUE)
  CorrMF_CN <- matrix(data = NA, nrow = nrow(output), ncol = 2^nclass, byrow = TRUE)


  for(j in 1:nrow(output)){
    CorrMF_CD[j,] <- mSourceToMCD(M_output[j,],beta_CD_d)  # Corrected MF by CD
    CorrMF_CR[j,] <- mSourceToMCR(M_output[j,],beta_CR_d)  # Corrected MF by CR
    CorrMF_CN[j,] <- mSourceToMCN(M_output[j,],beta_CN_d)  # Corrected MF by CN
  }
  return(list(MF = M_output,
              beta_CD = beta_CD_d,
              CD = CorrMF_CD,
              beta_CR = beta_CR_d,
              CR = CorrMF_CR,
              beta_CN = beta_CN_d,
              CN = CorrMF_CN
         ))
}
