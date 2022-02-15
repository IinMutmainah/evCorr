#Correction of EKNN classifier as a black box
#split correction set using partial decision SD (strong dominance criterion)
#learn best contextual correction parameters in each region
#and correct the output of EkNN classifier in test set based on the region

library(evCorr)
library(caret)
library(evclass)
library(tibble)
library(rlist)


#DATA: GENERATED DATA==================
data <- datagenerator() #5000 items / 5 classes / 2 attributes
tcol = 3
acol = 1:2
# #DATA: IRIS==========================
 data("iris"); # 150 items / 3 classes / 4 attributes
 data=iris
 tcol=5
 acol=c(1,2,3,4)
#DATA: IONOSPHERE==========================
data("ionosphere"); # 350 / 2 / 34
data=ionosphere
tcol=35
acol=seq(1,34)
#DATA: SONAR==========================
data("Sonar"); # 208 / 2 / 60
data=Sonar
tcol=61
acol=seq(1,60)
#DATA: GLASS==========================
data("glass")
data = glass #214 / 6 / 10
tcol = 10
acol = c(1:9)#
#DATA: VEHICLES======================
data = vehicles #846 / 4 / 19
tcol = 19
acol = c(1:18)

#DATA: transfusion====================
data = read.table("transfusion.data", #748 / 2 / 3
                  sep=",",
                  fill=TRUE,
                  header = FALSE,
                  strip.white=TRUE)
data = data[-1,c(2,4,5)]
tcol = 3
acol = c(1:2)
#
#DATA: RED WINE=======================
data = read.table("winequality-red.csv", #1599 / 6 / 12
                  sep=";",
                  fill=TRUE,
                  header = FALSE,
                  strip.white=TRUE)
data = data[-1,]
c = sort(as.numeric(unique(data[,12])))
myclass =seq(1:length(c))
data[,12] = sapply(as.numeric(data[,12]),function(x) if(x) which(c==x) )
tcol = 12
acol = c(1:11)
#
#DATA: ECOLI==================== #remove class 4,3,7
 data = read.table("ecoli.data", #336 / 5 / 7ft
                   sep="",
                   fill=TRUE,
                   header = FALSE,
                   strip.white=TRUE)
 data = data[,-1]
 tcol = 8
 acol = c(1:7)
 #delete class imL,imS,omL
 d=which(data[,tcol]== "imL"|data[,tcol]== "imS"|data[,tcol]== "omL")
 data=data[-d,]
 data[,tcol] = as.numeric(as.factor(data[,tcol]))

 #DATA: BALANCE-SCALE====================
 data = read.table("balance-scale.data", #625 / 3 / 4 ft
                   sep=",",
                   fill=TRUE,
                   header = FALSE,
                   strip.white=TRUE)
 data = data[,c(2:5,1)]
 tcol = 5
 acol = c(1:4)
 data[,tcol] = as.numeric(as.factor(data[,tcol]))
 #data[,tcol] = sapply(data[,tcol],function(x) ifelse(x==2,1,2))

 #DATA: LYMPOGRAPHY====================
 data = read.table("lymphography.data", #148 / 4 / 18 ft
                   sep=",",
                   fill=TRUE,
                   header = FALSE,
                   strip.white=TRUE)

 data = data[,c(2:19,1)]
 tcol = 19
 acol = c(1:18)
 d=which(data[,tcol]==1)
 data=data[-d,] #delete class 1
 data[,tcol] = as.numeric(as.factor(data[,tcol]))

 #DATA: ZOO====================
 data = read.table("zoo.data", #101 / 4 / 17 ft
                   sep=",",
                   fill=TRUE,
                   header = FALSE,
                   strip.white=TRUE)

 data = data[,c(2:18)]
 tcol = 17
 acol = c(1:16)

 #DATA: SOYBEAN====================
 data = read.table("soybean-large.data", #307 / 19 / 35 ft
                   sep=",",
                   fill=TRUE,
                   header = FALSE,
                   strip.white=TRUE)

 data = data[,c(2:36,1)]
 tcol = 36
 acol = c(1:35)
 data[,tcol] = as.numeric(as.factor(data[,tcol]))
#
#  #DATA: BREAST1====================
#  #this data cannot work
 data = read.table("breast.data", #699 / 9 / 9 ft
                   sep=",",
                   fill=TRUE,
                   header = FALSE,
                   strip.white=TRUE)

 data = data[,c(2:10)]
 tcol = 9
 acol = c(1:8)
 data[,tcol] = as.numeric(as.factor(data[,tcol]))

#DATA: BREAST2====================
 data = read.table("breast2.data", #569 / 2 / 30 ft
                   sep=",",
                   fill=TRUE,
                   header = FALSE,
                   strip.white=TRUE)

 data = data[,c(3:32,2)]
 tcol = 31
 acol = c(1:30)
 data[,tcol] = as.numeric(as.factor(data[,tcol]))


#DATA: ECOLI2=========================
#this data can not work
 data = read.table("ecoli.data", #336 / 5 / 7ft
                   sep="",
                   fill=TRUE,
                   header = FALSE,
                   strip.white=TRUE)
 data = data[,-1]
 tcol = 8
 acol = c(1:7)
 data[,tcol] = as.numeric(as.factor(data[,tcol]))

#DATA: LIVER===========================
 data = read.table("liver.data", #345 / 2 / 6 ft
                   sep=",",
                   fill=TRUE,
                   header = FALSE,
                   strip.white=TRUE)

 tcol = 7
 acol = c(1:6)
 data[,tcol] = as.numeric(as.factor(data[,tcol]))

#DATA: HABERMAN===========================
 data = read.table("haberman.data", #306 / 2 / 3 ft
                   sep=",",
                   fill=TRUE,
                   header = FALSE,
                   strip.white=TRUE)

 tcol = 4
 acol = c(1:3)
 data[,tcol] = as.numeric(as.factor(data[,tcol]))

 #DATA: BALANCE-SCALE===========================
 data = read.table("balance-scale.data", #625 / 3 / 4 ft
                   sep=",",
                   fill=TRUE,
                   header = FALSE,
                   strip.white=TRUE)

 data=data[,c(2:5,1)]
 tcol = 5
 acol = c(1:4)
 data[,tcol] = as.numeric(as.factor(data[,tcol]))

 #DATA: PIMA===========================
 data = read.table("pima.txt", #768 / 2 / 8 ft
                   sep=",",
                   fill=TRUE,
                   header = FALSE,
                   strip.white=TRUE)

 tcol = 9
 acol = c(1:8)
 data1 = scale(data[,acol])
 data = cbind(data1,data[,9])
 data[,tcol] = as.numeric(as.factor(data[,tcol]))

 #DATA: VERTEBRAL===========================
 data = read.table("vertebral.dat", #310 / 3 / 6 ft
                   sep="",
                   fill=TRUE,
                   header = FALSE,
                   strip.white=TRUE)

 tcol = 7
 acol = c(1:6)
 data[,tcol] = as.numeric(as.factor(data[,tcol]))

#IMAGE DATA:

mainTesting_sd_eknn <- function(data,tcol,acol,kn=5,kk=10,r=10){
  data <- as.data.frame(data)
  data <- as.data.frame(sapply(data, function(x) as.numeric(x))) # For EkNN use, all data in numeric
  mydata<-na.exclude(data)
  nclass=length(unique(as.numeric(mydata[,tcol]))) #to get number of classes

  #save number of data points outside regions
  number_outside <- rep(NA,kk*r);

  #for Eknn followed by all CCs with only data INSIDE
  Epl_inside <- rep(NA,kk*r);
  Rew65_inside <- rep(NA,kk*r);
  Rew80_inside<- rep(NA,kk*r);

  #for Eknn followed by all CCs with splitting
  Epl_CD_reg <- rep(NA,kk*r);
  Epl_CR_reg <- rep(NA,kk*r);
  Epl_CN_reg <- rep(NA,kk*r);
  Rew_CD_reg <- rep(NA,kk*r);
  Rew_CR_reg <- rep(NA,kk*r);
  Rew_CN_reg <- rep(NA,kk*r);

  #for Eknn followed by all CCs and no splitting
  Epl_eknn <- rep(NA,kk*r);
  Epl_CD <- rep(NA,kk*r);
  Epl_CR <- rep(NA,kk*r);
  Epl_CN <- rep(NA,kk*r);
  #measure using U65
  Rew_eknn1 <- rep(NA,kk*r);
  Rew_CD1 <- rep(NA,kk*r);
  Rew_CR1 <- rep(NA,kk*r);
  Rew_CN1 <- rep(NA,kk*r);
  #measure using U80
  Rew_eknn2 <- rep(NA,kk*r);
  Rew_CD2 <- rep(NA,kk*r);
  Rew_CR2 <- rep(NA,kk*r);
  Rew_CN2 <- rep(NA,kk*r);

  #for Eknn no splitting and no correction => EkNN+
  Epl_eknn_nocorr <- rep(NA,kk*r);
  Rew_eknn_nocorr1 <- rep(NA,kk*r);
  Rew_eknn_nocorr2 <- rep(NA,kk*r);

  #for Eknn followed by best CC with splitting
  Epl_reg_sum<- rep(NA,kk*r);
  Rew_reg65 <- rep(NA,kk*r);
  Rew_reg80 <- rep(NA,kk*r);

  bestCorr <- vector(mode = "list", length = kk*r)
  #for making ROC curve
  trueclass <- vector(mode = "list", length = kk*r)
  preds_eknn1 <- vector(mode = "list", length = kk*r)
  preds_eknn2 <- vector(mode = "list", length = kk*r)
  preds_CD <- vector(mode = "list", length = kk*r)
  preds_CN <- vector(mode = "list", length = kk*r)
  preds_CR <- vector(mode = "list", length = kk*r)

  trueclass_reg <- vector(mode = "list", length = kk*r)
  preds_reg <- vector(mode = "list", length = kk*r)
  preds_CD_reg <- vector(mode = "list", length = kk*r)
  preds_CN_reg <- vector(mode = "list", length = kk*r)
  preds_CR_reg <- vector(mode = "list", length = kk*r)

  t <- vector(mode = "list", length = kk*r)
  cf_all_reg <- vector(mode = "list", length = kk*r)
  tr_all_reg <- vector(mode = "list", length = kk*r)

  if ((nclass == 2) && ('0' %in% mydata[,tcol])) {
    mydata[,tcol] <-ifelse(as.numeric(mydata[,tcol])==0,1,2) #it is ground truth, just for dataset having 2 classes consisting of 0 and 1, because EKNN outputs 1 and 2
  } else {
    mydata[,tcol] <- as.numeric(mydata[,tcol])        #else, use this ground truth
  }

  rep=1
  for (rep in 1:r) {
    #shuffle data
    set.seed(rep)
    dat <- mydata[sample(nrow(mydata)),]

    #1/2 data to train the classifier
    set.seed(rep+123)
    trainIndex <- createDataPartition(dat[,tcol], p = .5, list = FALSE, times = 1);
    xeknn_train <- dat[trainIndex,acol];
    yeknn_train <- dat[trainIndex,tcol];
    eknn_train <- cbind(xeknn_train,yeknn_train)
    colnames(eknn_train)[tcol] <- "class"
    #plot(eknn_train$V1, eknn_train$V2, pch=21, bg=c("red","blue","green","yellow","black")[unclass(eknn_train$y)], main="Classifier training dataset")

    dat2 <- dat[-trainIndex,];

    #Create 10 equally size folds
    folds <- cut(seq(1,nrow(dat2)),breaks=10,labels=FALSE)

    # Learning EkNN parameter
    param0 <- invisible(EkNNinit(xeknn_train,yeknn_train));
    options <- list(maxiter=300,eta=0.1,gain_min=1e-5,disp=FALSE);
    fit <- EkNNfit(xeknn_train,yeknn_train,param=param0,K=kn,options=options);

    #Perform a kk fold cross validation
    q=1
    for(q in 1:kk){
      ri <- q+(rep-1)*kk
      #1/2 remaining data for testing and training the correction
      #kfold for test
      testIndex <- which(folds==q,arr.ind=TRUE)
      xtuning_test <- dat2[testIndex,acol];
      ytuning_test <- dat2[testIndex,tcol];
      tuning_test <- cbind(xtuning_test,ytuning_test)
      #colnames(tuning_test)[tcol] <- "class"
      #plot(tuning_test$V1, tuning_test$V2, pch=21, bg=c("red","blue","green","yellow","black")[unclass(tuning_test$y)], main="Test dataset")

      xtuning_train <- dat2[-testIndex,acol];
      ytuning_train <- dat2[-testIndex,tcol];
      tuning_train <- cbind(xtuning_train,ytuning_train)
      #colnames(tuning_train)[tcol] <- "class"
      #plot(tuning_train$V1, tuning_train$V2, pch=21, bg=c("red","blue","green","yellow","black")[unclass(tuning_train$y)], main="Corrections training dataset")

      truth <- tvtotm(ytuning_test,nclass) #convert vector to matrix of the truth without splitting

      # function to convert "COUTIOUS" decision from matrix to vector
      vec_pred <- function(dec){
        d <- rep(NA, nrow(dec))
        for(xi in 1:nrow(dec)){
          #mydec <- unbinary(paste(dec[m,],collapse=""))
          x <- which(dec[xi,]==1)
          d[xi] <- paste(as.character(x),collapse = "")
        }
        return(d)
      }


      #CORRECTION WITHOUT SPLITTING
      # For leaning corrections
      eknn_outputs_train <- EkNNval(xeknn_train,yeknn_train,xtuning_train,K=kn,ytuning_train,fit$param);
      m_train <- eknn_outputs_train$m;     # EkNN gives only mass functions on singletons and the universe

      uniqueClass = unique(mydata[,tcol])

      #get proper classifier output according to number of class
      m_tr = checkClass(m_train,uniqueClass,yeknn_train)
      M_training <- mtoM(m_tr);           # To have mass functions given on the whole frame
      # For testing
      eknn_outputs_test <- EkNNval(xeknn_train,yeknn_train,xtuning_test,K=kn,ytuning_test,fit$param);
      m_test <- eknn_outputs_test$m;     # EkNN gives only mass functions on singletons and the universe
      m_tst = checkClass(m_test,uniqueClass,yeknn_train)
      M_testing <- mtoM(m_tst);           # To have mass functions given on the whole frame
      #get parameters
      beta_CD <- tuningCD(data = M_training,t = tvtotm(ytuning_train,nclass))$beta;
      beta_CR <- tuningCR(data = M_training,t = tvtotm(ytuning_train,nclass))$beta;
      beta_CN <- tuningCN(data = M_training,t = tvtotm(ytuning_train,nclass))$beta;
      #get minimum distance
      dis_CD <- tuningCD(data = M_training,t = tvtotm(ytuning_train,nclass))$distmin
      dis_CR <- tuningCR(data = M_training,t = tvtotm(ytuning_train,nclass))$distmin
      dis_CN <- tuningCN(data = M_training,t = tvtotm(ytuning_train,nclass))$distmin
      #get best default correction
      best_corr_def <- which.min(cbind(dis_CD,dis_CR,dis_CN))

      #convert the MF (testing) to cf
      cf_eknn <- t(apply(M_testing,1,mtocf))
      #CORRECT the output of test using the cf
      cf_CD <- t(apply(cf_eknn,1,function(x) 1-(1-x)*beta_CD))
      cf_CR <- t(apply(cf_eknn,1,function(x) x*beta_CR))
      cf_CN <- t(apply(cf_eknn,1,function(x) 0.5+(x-0.5)*(2*beta_CN-1)))
      #performance eknn without splitting region of correction
      Epl_eknn[ri] <- sum((cf_eknn - truth)^2);
      Epl_CD[ri] <- sum((cf_CD - truth)^2);
      Epl_CR[ri] <- sum((cf_CR - truth)^2);
      Epl_CN[ri] <- sum((cf_CN - truth)^2);

      #CORRECT the output of test using MF
      mCorr_CD <- matrix(data = NA, nrow = nrow(M_testing), ncol = 2^nclass, byrow = TRUE)
      mCorr_CR <- matrix(data = NA, nrow = nrow(M_testing), ncol = 2^nclass, byrow = TRUE)
      mCorr_CN <- matrix(data = NA, nrow = nrow(M_testing), ncol = 2^nclass, byrow = TRUE)
      for(ci in 1: nrow(M_testing)){
        mCorr_CD[ci,] <-mSourceToMCD(M_testing[ci,],beta_CD)
      }

      for(ci in 1: nrow(M_testing)){
        mCorr_CR[ci,] <-mSourceToMCR(M_testing[ci,],beta_CR)
      }

      for(ci in 1: nrow(M_testing)){
        mCorr_CN[ci,] <-mSourceToMCN(M_testing[ci,],beta_CN)
      }
      #Decision making using ID
      decID_eknn <- t(apply(M_testing,1,mtosetsd))
      decID_CD <- t(apply(mCorr_CD,1,mtosetsd))
      decID_CR <- t(apply(mCorr_CR,1,mtosetsd))
      decID_CN <- t(apply(mCorr_CN,1,mtosetsd))
      mytab_eknn <- tibble(as.numeric(vec_pred(decID_eknn)),ytuning_test)
      mytab_CD <- tibble(as.numeric(vec_pred(decID_CD)),ytuning_test)
      mytab_CR <- tibble(as.numeric(vec_pred(decID_CR)),ytuning_test)
      mytab_CN <- tibble(as.numeric(vec_pred(decID_CN)),ytuning_test)
      #compute U65
      Rew_eknn1[ri] <- reward_U65(mytab_eknn)/nrow(truth)
      Rew_CD1[ri] <- reward_U65(mytab_CD)/nrow(truth)
      Rew_CR1[ri] <- reward_U65(mytab_CR)/nrow(truth)
      Rew_CN1[ri] <- reward_U65(mytab_CN)/nrow(truth)
      #compute U80
      Rew_eknn2[ri] <- reward_U80(mytab_eknn)/nrow(truth)
      Rew_CD2[ri] <- reward_U80(mytab_CD)/nrow(truth)
      Rew_CR2[ri] <- reward_U80(mytab_CR)/nrow(truth)
      Rew_CN2[ri] <- reward_U80(mytab_CN)/nrow(truth)

      #EKNN without correction using dataset for learning EKNN and learning corrections => EkNN+
      #combine data for training eknn and for corrections to train eknn
      xtrain= rbind(xeknn_train,xtuning_train)
      ytrain= c(yeknn_train,ytuning_train)
      param1 <- invisible(EkNNinit(xtrain,ytrain));
      options1 <- list(maxiter=300,eta=0.1,gain_min=1e-5,disp=FALSE);
      fit1 <- EkNNfit(xtrain,ytrain,param=param1,K=kn,options=options1);
      # Learning EKNN
      eknn_no_corr <- EkNNval(xtrain,ytrain,xtuning_test,K=kn,ytuning_test,fit1$param);
      m_nocorr <- eknn_no_corr$m;
      m_nocorr <- checkClass(m_nocorr,uniqueClass,ytrain = ytrain)
      M_nocorr <- mtoM(m_nocorr);#for a whole frame
      # Convert corrected mf to cf
      cf_eknn_nocorr <- t(apply(M_nocorr,1,mtocf))
      #Decision making using ID
      decID_eknn_nocorr <- t(apply(M_nocorr,1,mtosetsd))
      mytab_eknn_nocorr <- tibble(as.numeric(vec_pred(decID_eknn_nocorr)),ytuning_test)
      #Compute Epl
      Epl_eknn_nocorr[ri] <- sum((cf_eknn_nocorr - truth)^2);
      #Compute U65
      Rew_eknn_nocorr1[ri] <- reward_U65(mytab_eknn_nocorr)/nrow(truth)
      #Compute U80
      Rew_eknn_nocorr2[ri] <- reward_U80(mytab_eknn_nocorr)/nrow(truth)


      # # #MAKE DECISION REGION AND PLOT FOR DATASET HAVING 2 FEATURES
      # rx1_test <- range(xtuning_train[,1])
      # rx2_test <- range(xtuning_train[,2])
      # # get lattice points in predictor space
      # px1_test <- seq(from = rx1_test[1], to = rx1_test[2], by = 0.3 )
      # px2_test <- seq(from = rx2_test[1], to = rx2_test[2], by = 0.3 )
      #
      # # rx1_min <- floor(min(xtuning_train[,1]))
      # # rx1_max <- ceil(max(xtuning_train[,1]))
      # # rx2_min <- floor(min(xtuning_train[,2]))
      # # rx2_max <- ceil(max(xtuning_train[,2]))
      # # # get lattice points in predictor space
      # # px1_test <- seq(from = rx1_min, to = rx1_max, by = 0.3 )
      # # px2_test <- seq(from = rx2_min, to = rx2_max, by = 0.3 )
      # # data for a whole space
      # xnew_corr <-as.data.frame(expand.grid(px1_test, px2_test))
      #
      # # Test all point in feature space to get decision region
      # eknn_outputs_DR <- EkNNval_dfi(xtrain=xeknn_train,ytrain=yeknn_train,xtst=xnew_corr,K=kn,param=fit$param);
      # mCorr <- eknn_outputs_DR$m;     # EkNN gives only mass functions on singletons and the universe
      # M_DR <- mtoM(mCorr);           # To have mass functions given on the whole frame
      #
      # # #making decision using Betpl
      # # Betpl_DR <- t(apply(M_DR,1,mtobetpl))
      # # pred_DR <- apply(Betpl_DR,1,which.max)
      # # pred <- unique(pred_DR)
      # # pred_bin =tvtotm(nclass,pred_DR) #binary to decimal to get column in mf
      # # #get predicted column of mf
      # # pr =apply(pred_bin,1,function(x)unbinary(paste(x,collapse=""))+1) #binary to decimal
      # # pr=unique(pr)
      #
      # #make decision using ID
      # decID_DR <- t(apply(M_DR,1,mtosetsd))
      # pred_DR <- as.numeric(vec_pred(decID_DR)) #decision region
      # pred <- unique(pred_DR)
      # #get predicted column of mf
      # pr =apply(decID_DR,1,function(x)unbinary(paste(x,collapse=""))+1) #binary to decimal to get column in mf
      # pr=unique(pr)
      #
      # #TEMPORARY
      # #SPLIT region using Eknn-ID for EXAMPLE of all points========================
      # # making decision region using ID for training corrections
      # pred_train <- as.numeric(vec_pred(decID_DR))
      # pr_train <- unique(pred_train)
      # nregion <- length(pr_train)#get number of regions
      #
      #
      # m_DecionReg <- vector(mode = "list", length = length(pr))
      # for(p in 1:length(pr)){
      #   m_DecionReg[p] = list(matrix(data=M_DR[,pr[p]],nrow=length(px1_test),ncol=length(px2_test)) )
      # }
      #
      # plot.new()
      # contour(px1_test,px2_test,(m_DecionReg[[1]]), levels=0.5,col = "white", lwd = 1, main="",xlab="Feature 1", ylab="Feature 2")
      # points(xnew_corr,pch=20, cex=1, col= ifelse(pred_DR == 1, "violet",
      #                                              ifelse(pred_DR == 2,"blue",
      #                                                     ifelse(pred_DR == 3,"green",
      #                                                            ifelse(pred_DR == 4,"yellow",
      #                                                                   ifelse(pred_DR == 5,"black",
      #                                                                          ifelse(pred_DR==12,"cyan",
      #                                                                                 ifelse(pred_DR==13,"green",
      #                                                                                        ifelse(pred_DR==15,"red",
      #                                                                                             #ifelse(pred_DR==34,"violet",
      #                                                                                               ifelse(pred_DR==45,"darkgrey",
      #                                                                                                 ifelse(pred_DR==145,"green4","darkorange")))))))))))
      #
      #
      # legend("topright", c("1", "2", "3", "4","5","12","15","45","145", "12345"), pch=20,
      #        xpd=TRUE,title="Decision regions",horiz=TRUE,cex=0.55,
      #        col=c("violet", "blue", "green","yellow","black","cyan","red","darkgrey","green4","darkorange")
      # )


      #SPLITTING- USING DECISION REGION OF EKNN
      # #SPLIT region using Eknn-Betpl=======================
      # # making decision region using Betpl for training corrections
      # decBetpl_train <- t(apply(M_training,1,mtobetpl))
      # pred_train <- t(apply(decBetpl_train,1,which.max))
      # pr_train <- unique(as.numeric(pred_train))
      # nregion <- length(pr_train)#get number of regions
      # # making decision region using Betpl for training corr parameters
      # decBetpl_test <- t(apply(M_testing,1,mtobetpl))
      # pred_test <- t(apply(decBetpl_test,1,which.max))
      # pr_test <- unique(as.numeric(pred_test))

      #SPLIT region using Eknn-ID========================
      # making decision region using ID for training corrections
      decID_train <- t(apply(M_training,1,mtosetsd))
      pred_train <- as.numeric(vec_pred(decID_train))
      pr_train <- unique(pred_train)
      nregion <- length(pr_train)#get number of regions


      # making decision region using ID for testing
      decID_test <- t(apply(M_testing,1,mtosetsd))
      pred_test <- as.numeric(vec_pred(decID_test))

      Epl_reknn <- rep(NA,nregion)
      Epl_reg1 <- rep(NA,nregion)
      Epl_CD_reg1 <- rep(NA,nregion)
      Epl_CR_reg1 <- rep(NA,nregion)
      Epl_CN_reg1 <- rep(NA,nregion)

      Rew_reknn <- rep(NA,nregion)
      Rew_reg1 <- rep(NA,nregion)
      Rew_CD_reg1 <- rep(NA,nregion)
      Rew_CR_reg1 <- rep(NA,nregion)
      Rew_CN_reg1 <- rep(NA,nregion)

      Rew_reg2 <- rep(NA,nregion)
      Rew_CD_reg2 <- rep(NA,nregion)
      Rew_CR_reg2 <- rep(NA,nregion)
      Rew_CN_reg2 <- rep(NA,nregion)

      best_param <- rep(NA,nregion)
      best_param_value <- vector(mode = "list", length = nregion)
      distance <- vector(mode = "list", length = nregion)

      cf_reg <- vector(mode = "list", length = nregion)
      cf_CD_reg <- vector(mode = "list", length = nregion)
      cf_CR_reg <- vector(mode = "list", length = nregion)
      cf_CN_reg <- vector(mode = "list", length = nregion)
      tr_reg <- vector(mode = "list", length = nregion)

      #LEARNING CORRECTION AND TESTING BASED ON SPLIT REGIONS
      qq=1
      for(qq in 1:nregion){
        region_qq <- tuning_train[which(pred_train==pr_train[qq]),]
        test_qq   <- tuning_test[which(pred_test==pr_train[qq]),]

        if((nrow(region_qq) != 0) && (nrow(test_qq) !=0)){
          # Learning Corrections using current region
          eknn_outputs_reg <- EkNNval(xeknn_train,yeknn_train,region_qq[,acol],K=kn,region_qq[,tcol],fit$param);
          m_corr_reg <- eknn_outputs_reg$m;         # EkNN gives only mass functions on singletons and the universe
          #get right position for unseen class on mf
          m_corr_reg <- checkClass(m_corr_reg,uniqueClass,ytrain = yeknn_train)
          M_corr_reg <- mtoM(m_corr_reg);           # To have mass functions given on the whole frame

          # Compute \beta using contextual corrections minimizing distance using Epl
          beta_CD_d <- tuningCD_rev1(data = M_corr_reg,t=tvtotm(region_qq[,tcol],nclass))$beta
          beta_CR_d <- tuningCR(data = M_corr_reg,t=tvtotm(region_qq[,tcol],nclass))$beta
          beta_CN_d <- tuningCN(data = M_corr_reg,t=tvtotm(region_qq[,tcol],nclass))$beta
          CD_dis <- tuningCD_rev1(data = M_corr_reg,t=tvtotm(region_qq[,tcol],nclass))$distmin
          CR_dis <- tuningCR(data = M_corr_reg,t=tvtotm(region_qq[,tcol],nclass))$distmin
          CN_dis <- tuningCN(data = M_corr_reg,t=tvtotm(region_qq[,tcol],nclass))$distmin

          #get best parameter
          distance[qq] <- list(cbind(CD_dis,CR_dis,CN_dis))
          best_param[qq] <- which.min(cbind(CD_dis,CR_dis,CN_dis))
          best_param_value[qq] <- list(rbind(beta_CD_d,beta_CR_d,beta_CN_d))

          #test using test dataset in current region
          eknn_outputs_test <- EkNNval(xeknn_train,yeknn_train,test_qq[,acol],K=kn,test_qq[,tcol],fit$param);
          output <- eknn_outputs_test$m
          output <- checkClass(output,uniqueClass,ytrain = yeknn_train)
          M_output <- mtoM(output)

          #Compute corrected output of EKNN using all CCs
          CorrMF_CD <- matrix(data = NA, nrow = nrow(output), ncol = 2^nclass, byrow = TRUE)
          CorrMF_CR <- matrix(data = NA, nrow = nrow(output), ncol = 2^nclass, byrow = TRUE)
          CorrMF_CN <- matrix(data = NA, nrow = nrow(output), ncol = 2^nclass, byrow = TRUE)
          for(ci in 1: nrow(output)){
            CorrMF_CD[ci,] <-mSourceToMCD(M_output[ci,],beta_CD_d)
          }

          for(ci in 1: nrow(output)){
            CorrMF_CR[ci,] <-mSourceToMCR(M_output[ci,],beta_CR_d)
          }

          for(ci in 1: nrow(output)){
            CorrMF_CN[ci,] <-mSourceToMCN(M_output[ci,],beta_CN_d)
          }
          #Compute corrected output of EKNN using best CC
          CorrMF <- matrix(data = NA, nrow = nrow(output), ncol = 2^nclass, byrow = TRUE)
          if(best_param[qq] == 1){
            for(ci in 1: nrow(output)){
              CorrMF[ci,] <-mSourceToMCD(M_output[ci,],beta_CD_d)
            }
          }
          if(best_param[qq] == 2){
            for(ci in 1: nrow(output)){
              CorrMF[ci,] <-mSourceToMCR(M_output[ci,],beta_CR_d)
            }
          }
          if(best_param[qq] == 3){
            for(ci in 1: nrow(output)){
              CorrMF[ci,] <-mSourceToMCN(M_output[ci,],beta_CN_d)
            }
          }

          truth_reg <- tvtotm(test_qq[,tcol],nclass)
          tr_reg[qq] <- list(as.numeric(test_qq[,tcol]))

          #compute performance Eknn with all CCs for each region
          cf_CD_reg[qq] <- list(t(apply(CorrMF_CD,1,mtocf)))
          cf_CR_reg[qq] <- list(t(apply(CorrMF_CR,1,mtocf)))
          cf_CN_reg[qq] <- list(t(apply(CorrMF_CN,1,mtocf)))

          decID_CD_reg <- t(apply(CorrMF_CD,1,mtosetsd))
          decID_CR_reg <- t(apply(CorrMF_CR,1,mtosetsd))
          decID_CN_reg <- t(apply(CorrMF_CN,1,mtosetsd))

          Epl_CD_reg1[qq] <- sum((cf_CD_reg[[qq]] - truth_reg)^2);
          Epl_CR_reg1[qq] <- sum((cf_CR_reg[[qq]] - truth_reg)^2);
          Epl_CN_reg1[qq] <- sum((cf_CN_reg[[qq]] - truth_reg)^2);

          mytab_CD_reg <- tibble(as.numeric(vec_pred(decID_CD_reg)),test_qq[,tcol])
          mytab_CD_reg %>% data.frame
          mytab_CR_reg <- tibble(as.numeric(vec_pred(decID_CR_reg)),test_qq[,tcol])
          mytab_CR_reg %>% data.frame
          mytab_CN_reg <- tibble(as.numeric(vec_pred(decID_CN_reg)),test_qq[,tcol])
          mytab_CN_reg %>% data.frame

          #Compute reward using U65
          Rew_CD_reg1[qq] <- reward_U65(mytab_CD_reg)
          Rew_CR_reg1[qq] <- reward_U65(mytab_CR_reg)
          Rew_CN_reg1[qq] <- reward_U65(mytab_CN_reg)

          #compute reward using U80
          Rew_CD_reg2[qq] <- reward_U80(mytab_CD_reg)
          Rew_CR_reg2[qq] <- reward_U80(mytab_CR_reg)
          Rew_CN_reg2[qq] <- reward_U80(mytab_CN_reg)

          #compute performance Eknn with best CCs for each region
          cf_reg[qq] <- list(t(apply(CorrMF,1,mtocf)))
          decID_reg <- t(apply(CorrMF,1,mtosetsd))
          Epl_reg1[qq] <- sum((cf_reg[[qq]] - truth_reg)^2);
          mytab_reg <- tibble(as.numeric(vec_pred(decID_reg)),test_qq[,tcol])
          Rew_reg1[qq] <- reward_U65(mytab_reg)
          Rew_reg2[qq] <- reward_U80(mytab_reg)
        }

      }

      #COMPUTE performance of Eknn followed by BEST CC with only data INSIDE
      Epl_inside[ri] <- sum(Epl_reg1,na.rm = T)
      Rew65_inside[ri] <- (sum(Rew_reg1,na.rm = T))/nrow(truth)
      Rew80_inside[ri] <- (sum(Rew_reg2,na.rm = T))/nrow(truth)


      #COMPUTE performance data point OUTSIDE the regions
      outside <- setdiff(pred_test,pr_train)
      if (length(outside) !=0){
        n_out = length(outside)
        Epl_outside <- rep(NA, n_out)
        Rew_out65 <- rep(NA, n_out)
        Rew_out80 <- rep(NA, n_out)
        cf_outside <- vector(mode = "list", length = n_out)
        tr_out <- vector(mode = "list", length = n_out)
        for(p in 1:n_out){
          test_outside <- tuning_test[which(pred_test == outside[p]),]
          if(nrow(test_outside!=0)){
            eknn_outside <- EkNNval(xeknn_train,yeknn_train,test_outside[,acol],K=kn,test_outside[,tcol],fit$param);
            m_outside <- eknn_outside$m;     # EkNN gives only mass functions on singletons and the universe
            m_outside <- checkClass(m_outside,uniqueClass,ytrain = yeknn_train)
            M_outside <- mtoM(m_outside);           # To have mass functions given on the whole frame

            #Correct M_outside using default best correction
            CorrMF_def <- matrix(data = NA, nrow = nrow(M_outside), ncol = 2^nclass, byrow = TRUE)
            if(best_corr_def == 1){
              for(ci in 1: nrow(M_outside)){
                CorrMF_def[ci,] <-mSourceToMCD(M_outside[ci,],beta_CD)
              }
            }
            if(best_corr_def == 2){
              for(ci in 1: nrow(M_outside)){
                CorrMF_def[ci,] <-mSourceToMCR(M_outside[ci,],beta_CR)
              }
            }
            if(best_corr_def == 3){
              for(ci in 1: nrow(M_outside)){
                CorrMF_def[ci,] <-mSourceToMCN(M_outside[ci,],beta_CN)
              }
            }

            #compute Epl
            cf_outside[p] <- list(t(apply(CorrMF_def,1,mtocf)))
            tr_out[p] <- list(test_outside[,tcol]) #truth of data outside region
            truth_outside <- tvtotm(tr_out[[p]],nclass)
            Epl_outside[p] <- sum((cf_outside[[p]] - truth_outside)^2)
            #compute U65
            decID_outside <- t(apply(CorrMF_def,1,mtosetsd))
            mytab_out <- tibble(as.numeric(vec_pred(decID_outside)),test_outside[,tcol])
            Rew_out65[p] <- reward_U65(mytab_out)
            #compute U80
            Rew_out80[p] <- reward_U80(mytab_out)

          }
        }
        cf_all_reg <- rbind(list.rbind(cf_reg),list.rbind(cf_outside))
        tr_all_reg <- c(unlist(tr_reg),unlist(tr_out))

        #performance of Eknn followed by CC with splitting using Epl
        Epl_CD_reg[ri] <- sum(Epl_CD_reg1,Epl_outside,na.rm = T)
        Epl_CR_reg[ri] <- sum(Epl_CR_reg1,Epl_outside,na.rm = T)
        Epl_CN_reg[ri] <- sum(Epl_CN_reg1,Epl_outside,na.rm = T)

        #performance of Eknn followed by CC with splitting using U65
        Rew_CD_reg1[ri] <- (sum(Rew_CD_reg1,Rew_out65,na.rm = T))/nrow(truth)
        Rew_CR_reg1[ri] <- (sum(Rew_CR_reg1,Rew_out65,na.rm = T))/nrow(truth)
        Rew_CN_reg1[ri] <- (sum(Rew_CN_reg1,Rew_out65,na.rm = T))/nrow(truth)

        #performance of Eknn followed by CC with splitting using U80
        Rew_CD_reg2[ri] <- (sum(Rew_CD_reg2,Rew_out80,na.rm = T))/nrow(truth)
        Rew_CR_reg2[ri] <- (sum(Rew_CR_reg2,Rew_out80,na.rm = T))/nrow(truth)
        Rew_CN_reg2[ri] <- (sum(Rew_CN_reg2,Rew_out80,na.rm = T))/nrow(truth)

        #performance of Eknn followed by BEST CC with splitting
        Epl_reg_sum[ri] <- sum(Epl_reg1,Epl_outside,na.rm = T)
        Rew_reg65[ri] <- (sum(Rew_reg1,Rew_out65,na.rm = T))/nrow(truth)
        Rew_reg80[ri] <- (sum(Rew_reg2,Rew_out80,na.rm = T))/nrow(truth)


      } else {
        cf_all_reg <- list.rbind(cf_reg)
        tr_all_reg <- unlist(tr_reg)

        #performance of Eknn followed by CC with splitting using Epl
        Epl_CD_reg[ri] <- sum(Epl_CD_reg1,na.rm = T)
        Epl_CR_reg[ri] <- sum(Epl_CR_reg1,na.rm = T)
        Epl_CN_reg[ri] <- sum(Epl_CN_reg1,na.rm = T)

        #performance of Eknn followed by CC with splitting using U65
        Rew_CD_reg1[ri] <- (sum(Rew_CD_reg1,na.rm = T))/nrow(truth)
        Rew_CR_reg1[ri] <- (sum(Rew_CR_reg1,na.rm = T))/nrow(truth)
        Rew_CN_reg1[ri] <- (sum(Rew_CN_reg1,na.rm = T))/nrow(truth)

        #performance of Eknn followed by CC with splitting using U80
        Rew_CD_reg2[ri] <- (sum(Rew_CD_reg2,na.rm = T))/nrow(truth)
        Rew_CR_reg2[ri] <- (sum(Rew_CR_reg2,na.rm = T))/nrow(truth)
        Rew_CN_reg2[ri] <- (sum(Rew_CN_reg2,na.rm = T))/nrow(truth)

        #performance of Eknn followed by BEST CC with splitting
        Epl_reg_sum[ri] <- sum(Epl_reg1,na.rm = T)
        Rew_reg65[ri] <- (sum(Rew_reg1,na.rm = T))/nrow(truth)
        Rew_reg80[ri] <- (sum(Rew_reg2,na.rm = T))/nrow(truth)

      }

      #GET best corrections in all regions
      bestCorr[ri] <- list(best_param)


      #for ROC curve with corrections using Betpl
      positive_class <- 1
      preds_reg[[ri]] <- apply(cf_all_reg,1,function(cl) cl[positive_class]/sum(cl));# BetPl(Class +) for each object
      preds_CD_reg[[ri]] <- apply(list.rbind(cf_CD_reg),1,function(cl) cl[positive_class]/sum(cl)) # beta_CD^BetPl(Class +) for each object
      preds_CR_reg[[ri]] <- apply(list.rbind(cf_CR_reg),1,function(cl) cl[positive_class]/sum(cl)) # beta_CR^BetPl(Class +) for each object
      preds_CN_reg[[ri]] <- apply(list.rbind(cf_CN_reg),1,function(cl) cl[positive_class]/sum(cl)) # beta_CN^BetPl(Class +) for each object


      preds_eknn1[[ri]] <- apply(cf_eknn,1,function(cl) cl[positive_class]/sum(cl));# BetPl(Class +) for each object
      preds_eknn2[[ri]] <- apply(cf_eknn_nocorr,1,function(cl) cl[positive_class]/sum(cl));# BetPl(Class +) for each object
      preds_CD[[ri]] <- apply(cf_CD,1,function(cl) cl[positive_class]/sum(cl)) # beta_CD^BetPl(Class +) for each object
      preds_CR[[ri]] <- apply(cf_CR,1,function(cl) cl[positive_class]/sum(cl)) # beta_CR^BetPl(Class +) for each object
      preds_CN[[ri]] <- apply(cf_CN,1,function(cl) cl[positive_class]/sum(cl)) # beta_CN^BetPl(Class +) for each object

      #Building truth : 1 for the positive class, 0 for the negative one
      trueclass_reg[[ri]] <- unlist(lapply(as.numeric(tr_all_reg), function(cl) if(cl==positive_class){cl=1}else{cl=0}));
      trueclass[[ri]] <- unlist(lapply(as.numeric(ytuning_test), function(cl) if(cl==positive_class){cl=1}else{cl=0}));
      #number data points outside the regions
      number_outside[ri] <- length(trueclass[[ri]]) -  length(unlist(tr_reg))
    }
  }

  # #ROC curve for EKNN followed with splitting and without splitting region
  # if(roc){
  #   # Plot the ROC curves
  #   library(pROC);
  #   title <- paste("ROC (positive class =",positive_class,")");
  #   roc_SplitCorr <- plot.roc(unlist(trueclass_reg), unlist(preds_reg),
  #                             main=title,
  #                             percent=TRUE,
  #                             col="blue",
  #                             lty = "dashed",
  #                             lwd=0.7,
  #                             legacy.axes=TRUE,
  #                             print.auc=FALSE,
  #                             xlab = "False positive rate (%)",
  #                             ylab = "True positive rate (%)");
  #
  #   # roc_SplitCD <- lines.roc(unlist(trueclass_reg), unlist(preds_CD_reg),
  #   #                          percent=TRUE,
  #   #                          col="red",
  #   #                          lty = "solid",lwd=0.7,
  #   #                          print.auc=FALSE);
  #   # roc_SplitCR <- lines.roc(unlist(trueclass_reg), unlist(preds_CR_reg),
  #   #                          percent=TRUE,
  #   #                          col="green",
  #   #                          lty = "solid",lwd=0.7,
  #   #                          print.auc=FALSE);
  #   # roc_SplitCN <- lines.roc(unlist(trueclass_reg), unlist(preds_CN_reg),
  #   #                          percent=TRUE,
  #   #                          col="orange",
  #   #                          lty = "solid",lwd=0.7,
  #   #                          print.auc=FALSE);
  #
  #   roc_Eknn1 <- lines.roc(unlist(trueclass), unlist(preds_eknn1),
  #                         percent=TRUE,
  #                         col="black",
  #                         lty = "solid",lwd=0.7,
  #                         print.auc=FALSE);
  #   roc_Eknn2 <- lines.roc(unlist(trueclass), unlist(preds_eknn2),
  #                          percent=TRUE,
  #                          col="orange",
  #                          lty = "solid",lwd=0.7,
  #                          print.auc=FALSE);
  #   roc_CD <- lines.roc(unlist(trueclass), unlist(preds_CD),
  #                       percent=TRUE,
  #                       col="red",
  #                       lty = "dashed",lwd=0.7,
  #                       print.auc=FALSE);
  #   roc_CR <- lines.roc(unlist(trueclass), unlist(preds_CR),
  #                       percent=TRUE,
  #                       col="yellow",
  #                       lty = "dashed",lwd=0.7,
  #                       print.auc=FALSE);
  #   roc_CN <- lines.roc(unlist(trueclass), unlist(preds_CN),
  #                       percent=TRUE,
  #                       col="green3",
  #                       lty = "dashed",lwd=0.7,
  #                       print.auc=FALSE);
  #
  #
  #   legend("bottomright",
  #          legend = c("SplitCorr", "Eknn1","Eknn2", "CD","CR","CN"),
  #          col=c("blue","black","orange", "red","yellow","green3"),
  #          lty = c("dashed","solid","solid", "dashed","dashed","dashed"), lwd=1)
  # }


  return(list(
    Epl_mean=c(round(mean(Epl_eknn),digits = 2),round(sd(Epl_eknn),digits = 2),
               round(mean(Epl_eknn_nocorr),digits = 2),round(sd(Epl_eknn_nocorr),digits = 2),
               round(mean(Epl_CD),digits = 2),round(sd(Epl_CD),digits = 2),
               round(mean(Epl_CR),digits = 2),round(sd(Epl_CR),digits = 2),
               round(mean(Epl_CN),digits = 2),round(sd(Epl_CN),digits = 2),
               #round(mean(Epl_inside),digits = 2),round(sd(Epl_inside),digits = 2),
               sum(number_outside),
               round(mean(Epl_reg_sum),digits = 2),round(sd(Epl_reg_sum),digits = 2)
    ),

    Rew_mean65=c(round(mean(Rew_eknn1),digits = 4),round(sd(Rew_eknn1),digits = 4),
               round(mean(Rew_eknn_nocorr1),digits = 4),round(sd(Rew_eknn_nocorr1),digits = 4),
               round(mean(Rew_CD1),digits = 4),round(sd(Rew_CD1),digits = 4),
               round(mean(Rew_CR1),digits = 4),round(sd(Rew_CR1),digits = 4),
               round(mean(Rew_CN1),digits = 4),round(sd(Rew_CN1),digits = 4),
               #round(mean(Rew65_inside),digits = 4),round(sd(Rew65_inside),digits = 4),
               sum(number_outside),
               round(mean(Rew_reg65),digits = 4),round(sd(Rew_reg65),digits = 4)
    ),
    Rew_mean80=c(round(mean(Rew_eknn2),digits = 4),round(sd(Rew_eknn2),digits = 4),
                 round(mean(Rew_eknn_nocorr2),digits = 4),round(sd(Rew_eknn_nocorr2),digits = 4),
                 round(mean(Rew_CD2),digits = 4),round(sd(Rew_CD2),digits = 4),
                 round(mean(Rew_CR2),digits = 4),round(sd(Rew_CR2),digits = 4),
                 round(mean(Rew_CN2),digits = 4),round(sd(Rew_CN2),digits = 4),
                 #round(mean(Rew80_inside),digits = 4),round(sd(Rew80_inside),digits = 4),
                 sum(number_outside),
                 round(mean(Rew_reg80),digits = 4),round(sd(Rew_reg80),digits = 4)
    ),

    bestCorrection = table(unlist(bestCorr))

  ));


}
