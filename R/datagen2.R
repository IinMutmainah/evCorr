#' Generating synthetic data with 2 classes and 100 instances
#'
#'\code{datagen2} generates synthetic data with 2 classes and 100 instances.
#' @param mu1 mean of class 1
#' @param mu2 mean of class 2
#' @return synthetic data.
#' @export
#' @importFrom  graphics MASS
#'@author S. Mutmainah
## DATA GENERATOR ------------------------------
# Generate data as in Deneoux 2019 (KBS paper) Section 5.2 Figure 9
datagen2<-function(mu1=c(2,3), mu2=c(3,4)){
  set.seed(1234);
  #nbc <- 5;
  #rho <- 0.9;
  n <- 50; # number of data per classes
  sigma1 <- 0.1 * diag(2)
  sigma2 <- 0.4 * diag(2)
  #sigma1 <- 0.3 * diag(2)
  #sigma2 <- 0.4 * diag(2)
  data1 <- MASS::mvrnorm(n,mu1,sigma1);
  data2 <- MASS::mvrnorm(n,mu2,sigma2);

  plot(data1[,1],data1[,2],type="p",pch=1,col="blue",
       xlab="Feature 1", ylab="Feature 2",xlim=c(1,4), ylim=c(1,6),
       main="");
  graphics::lines(data2[,1],data2[,2],type="p",pch=6,col="green3");
  graphics::legend("topleft",c("class 1","class 2"),
         cex=.8,col=c("blue","green3" ),
         pch=c(1,6));
  data <- rbind(
    cbind(data1,rep(1,n)),
    cbind(data2,rep(2,n)))

  return(data);
}

