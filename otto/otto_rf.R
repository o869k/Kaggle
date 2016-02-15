#Initialization
{
  sessionInfo() #system performance
  gc() #clear unsued memory
  rm(list=ls()) # clear workspace
  mainDir <- "C:/Users/user/Documents/R/otto"
  setwd(mainDir)
  set.seed(1234) #setting seed for comparison
  library(fpp); library(UsingR)
  library(caret); library(kernlab)
  library(foreach); library(doParallel)
  library(ggplot2); library(ROCR)
  library(randomForest); library(date)
  library(data.table); library(bit64)
  library(matrixStats); library(e1071)
  library(FSelector); library(lubridate)
}

#Read Data
{
  train <- read.csv("trainNEW.csv")  
  test <- read.csv("testNEW.csv")
  Submission <- read.csv("sampleSubmission.csv")
  M <- ncol(train) #Number of features per sample (columns)
  N <- nrow(train) #Number of features per sample (columns)
  L <- nlevels(train$target) #Number of features per sample (columns)
  Ntest <- nrow(test) #Number of features per sample (columns)
}

#Function for evaluation
Kappa_eval <- function(true.y, pred) {
  return(1 - classAgreement(table(pred, true.y))$kappa)
}
LogLoss <- function(true.y, pred, eps=0.00001) {
  
  pred <- pmin(pmax(pred, eps), 1-eps)
  
  -(log(pred[true.y]))
}

#Train a Random Forest with respect to ROC and calcualte AUC. use 5fold CV and compare mean validation to OOB
{
  #Divide to train and eval - multifolds & other initializations
  {
    K <- 3 #number of folds (70-30)
    Times <- 2 #number of resamples (5)
    folds <- createMultiFolds(train$target,k=K,times = Times)
    M <- ncol(train)
    N <- nrow(train) #Number of real samples (rows)  
    ntrees <- c(10) # vector of trees, it is then multiplied by number of cpus (4)
    train_score_ntrees <- rep(0,length(ntrees))
    eval_score_ntrees <- rep(0,length(ntrees))
  }
  
  #Training RF and calulating AUC
  {
    for (j in 1:length(ntrees))
    { 
      ll_train <- rep(0,K*Times)
      ll_eval <- rep(0,K*Times)
      ll_OOB <- rep(0,K*Times)
      for (i in 1:(K*Times))
      {
        #Train
        cl<-makeCluster(4)
        registerDoParallel(cl)
        model_RF <- foreach(ntree=rep(ntrees[j],4), .combine=combine, .verbose=T, .packages='randomForest') %dopar% {
                    randomForest(y=train$target[folds[[i]]],x=train[folds[[i]],-M],xtest = train[folds[[i]],-M],
                    ytest = train$target[folds[[i]]],importance = T,ntree=ntree,keep.forest=T)}
        stopCluster(cl)
        
        #varImpPlot(model_RF)
        
        #Evaluate logloss on OOB set
        {
          sum <- 0
          for (l in 1:nrow(model_RF$votes))
          {
            sum <- sum + LogLoss(as.numeric(train$target[folds[[i]]][l]),as.numeric(model_RF$votes[l,]))  
          }
          ll_OOB[i] <- sum/nrow(train_predictions)
          classAgreement(model_RF$confusion)$kappa
          model_RF$err.rate 
        }        
        
        #Evaluate logloss on train set
        {
          train_predictions <- predict(model_RF,train[folds[[i]],-M],type="prob")
          sum <- 0
          for (l in 1:nrow(train_predictions))
          {
            sum <- sum + LogLoss(as.numeric(train$target[folds[[i]]][l]),as.numeric(train_predictions[l,]))  
          }
          ll_train[i] <- sum/nrow(train_predictions)
          classAgreement(model_RF$test$confusion)$kappa
          model_RF$test$err.rate
        }

        #Evaluate logloss on eval set
        {
          eval_predictions <- predict(model_RF,train[-folds[[i]],],"prob")
          sum <- 0
          for (l in 1:nrow(eval_predictions))
          {
            sum <- sum + LogLoss(as.numeric(train$target[-folds[[i]]][l]),as.numeric(eval_predictions[l,]))  
          }
          ll_eval[i] <- sum/nrow(eval_predictions)
        }

        #rm(train_predictions,eval_predictions,pred_auc_eval,pred_auc_train,eval.rf.perf,train.rf.perf,model_RF)
      }
      train_score_ntrees[j] <- mean(ll_train)
      eval_score_ntrees[j] <- mean(ll_eval)
    }
  }  
}

#Test Model and save new submission file
{
  samp_vector <- seq(100,1881,200)
  ntrees <- seq(10,250,30)
  auc_train <- matrix(0,length(ntrees),length(samp_vector))
  for (i in 1:length(ntrees))
  {
    for (j in 1:length(samp_vector))
    {
      cl<-makeCluster(4)
      registerDoParallel(cl)
      model_RF <- foreach(ntree=rep(ntrees[9], 4), .combine=combine, .verbose=T, .packages='randomForest') %dopar% {
        randomForest(y=train$outcome,x=train[,c(bfs_cols)],xtest = train[,c(bfs_cols)],sampsize = c(samp_vector[9],table(train$outcome)[[2]]),
                     ytest = train$outcome,metric = "ROC", importance = T,ntree=ntree,do.trace=T,keep.forest=T)}    
      stopCluster(cl)
      #importance(model_RF)
      train_predictions <- model_RF$votes[,2]/(model_RF$votes[,2]+model_RF$votes[,1]) #we look at the OOB error for comparing to eval
      
      pred_auc_train <- prediction(train_predictions,train$outcome)
      auc.tmp.train <- performance(pred_auc_train,"auc")
      auc_train[i,j] <- as.numeric(auc.tmp.train@y.values)
      train.rf.perf = performance(pred_auc_train,"tpr","fpr")    
      #plot the curve
      plot(train.rf.perf,main="ROC Curve, Final",col=2,lwd=2)
      abline(a=0,b=1,lwd=2,lty=2,col="gray")
    }
  }
  
  #plot AUC vs. samp size
  plot(samp_vector,auc_train,type = "b")
  
  #Testing
  test_predictions <- predict(model_RF,test[,c(bfs_cols)],"prob")
  #probability for bot
  for (i in 1:nrow(sampleSubmission))
  {
    sampleSubmission$prediction[i] <- test_predictions[which(test$bidder_id==as.integer(sampleSubmission$bidder_id[i])),2]
  }
  write.csv(sampleSubmission,"Submission_new.csv",row.names=F)
  
}

#Evaluate logloss on test set
{
  pred <- predict(rf,test,type="prob")
  submit <- data.frame(pred)
  write.csv(submit, file = "firstsubmit.csv", row.names = T)
}