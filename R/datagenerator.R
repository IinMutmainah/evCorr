#' Generating synthetic data with 2 classes and 100 instances
#'
#'\code{datagenerator} generates synthetic data with 5 classes and 5000 instances.
#' @return data : synthetic data.
#' @references F. Pichon, D. Mercier, E. Lefèvre, and F. Delmotte.
#' Proposition and learning of some belief function contextual correction mechanisms.
#' InternationalJournal Approximate Reasoning, 72:4–42, 2016 (Section 8.6 Figure 4).
#' @importFrom graphic MASS
#'@author S. Mutmainah
## DATA GENERATOR ------------------------------
# Generate data as in Pichon et al. 2016 Section 8.6 Figure 4
datagenerator<-function(){
  #set.seed(123);
  set.seed(9999);

  mu1 <- c(0, 0);
  mu2 <- c(2, 0);
  mu3 <- c(0, 2);
  mu4 <- c(2, 2);
  mu5 <- c(1, 1);
  rho <- 0.9;
  n <- 1000; # number of data per classes
  sigma <- matrix(c(1,rho,rho,1),nrow = 2);
  data1 <-MASS::mvrnorm(n,mu1,sigma);
  data2 <- MASS::mvrnorm(n,mu2,sigma);
  data3 <- MASS::mvrnorm(n,mu3,sigma);
  data4 <- MASS::mvrnorm(n,mu4,sigma);
  data5 <- MASS::mvrnorm(n,mu5,sigma);

  plot(data1[,1],data1[,2],type="p",pch=3,col="red",
       xlab="Feature 1", ylab="Feature 2",xlim=c(-4,5), ylim=c(-4,5));
  # plot(data1[,1],data1[,2],type="p",pch=3,col="red",
  #      xlab="Feature 1", ylab="Feature 2",xlim=c(-4,5), ylim=c(-4,5),
  #      main="Generated Data");
  graphics::lines(data2[,1],data2[,2],type="p",pch=1,col="blue");
  graphics::lines(data3[,1],data3[,2],type="p",pch=2,col="green");
  graphics::lines(data4[,1],data4[,2],type="p",pch=8,col="yellow");
  graphics::lines(data5[,1],data5[,2],type="p",pch=20,col="black");
  graphics::legend(x=-4,y=4.5,c("class 1","class 2", "class 3","class 4","class 5"),
         cex=.8,col=c("red","blue","green","yellow","black"),pch=c(3,1,2,8,20));

  data <- rbind(
    cbind(data1,rep(1,n)),
    cbind(data2,rep(2,n)),
    cbind(data3,rep(3,n)),
    cbind(data4,rep(4,n)),
    cbind(data5,rep(5,n)))
  return(data);
}


