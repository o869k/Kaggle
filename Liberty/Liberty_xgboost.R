#Initialization
{
  sessionInfo() #system performance
  gc() #clear unsued memory
  rm(list=ls()) # clear workspace
  setwd(mainDir)
  Sys.setlocale("LC_TIME", "English")
  set.seed(1234) #setting seed for comparison
  library(randomForest); library(caret);
  library(MASS); library(pls);
  library(readr); library(reshape2)
  library(xgboost)
}

#NormalizedGini is the other half of the metric. This function does most of the work, though
SumModelGini <- function(solution, submission) {
  df = data.frame(solution = solution, submission = submission)
  df <- df[order(df$submission, decreasing = TRUE),]
  df$random = (1:nrow(df))/nrow(df)
  totalPos <- sum(df$solution)
  df$cumPosFound <- cumsum(df$solution) # this will store the cumulative number of positive examples found (used for computing "Model Lorentz")
  df$Lorentz <- df$cumPosFound / totalPos # this will store the cumulative proportion of positive examples found ("Model Lorentz")
  df$Gini <- df$Lorentz - df$random # will store Lorentz minus random
  return(sum(df$Gini))
}
NormalizedGini <- function(solution, submission) {
  SumModelGini(solution, submission) / SumModelGini(solution, solution)
}
NormalizedGiniXgboost <- function(preds, dtrain) {
      labels <- getinfo(dtrain, "label")
      res <- SumModelGini(labels, preds) / SumModelGini(labels, labels)
      return(list(metric = "gini", value = as.numeric(res)))
}
evalerror <- function(preds, dtrain) {
      #classification error
      labels <- getinfo(dtrain, "label")
      err <- as.numeric(sum(labels != (preds > 0)))/length(labels)
      return(list(metric = "error", value = err))
}
logregobj <- function(preds, dtrain) {
      labels <- getinfo(dtrain, "label")
      preds <- 1/(1 + exp(-preds))
      grad <- preds - labels
      hess <- preds * (1 - preds)
      return(list(grad = grad, hess = hess))
}

#Read Data
{
  train <- read.csv("train.csv")
  train_interactions_no_correlation <- read.csv("train_interactions_no_correlation_smaller.csv")
  full_data_interactions_no_correlation <- read.csv("train_interactions_no_correlation_smaller.csv")
  full_data_interactions_no_correlation <- full_data_interactions_no_correlation[,-1]
  test <- read.csv("test.csv")
  sample_submission <- read.csv("sample_submission.csv")      
}

#Concatenate train and test so we can choose the same interations
{
   full_data <- rbind(train[,-1],test)   
}

#Exploritory Analysis on the data
{
  str(train)
  summary(train)
  M <- ncol(train)
  N <- nrow(train)
  for (i in seq(from=1,to=M,by=4))
  {
    par(mfrow=c(2,2))
    if (is.factor(train[,i])) {
      hist(table(train[,i]),10,main = names(train)[i])     
    } else {
      hist((train[,i]),10,main = names(train)[i])}
    if (is.factor(train[,i+1])) {
      hist(table(train[,i+1]),10,main = names(train)[i+1])     
    } else {
      hist((train[,i+1]),10,main = names(train)[i+1])}
    if (is.factor(train[,i+2])) {
      hist(table(train[,i+2]),10,main = names(train)[i+2])     
    } else {
      hist((train[,i+2]),10,main = names(train)[i+2])}
    if (is.factor(train[,i+3])) {
      hist(table(train[,i+3]),10,main = names(train)[i+3])     
    } else {
      hist((train[,i+3]),10,main = names(train)[i+3])}
  }
  par(mfrow=c(1,1))   
  
  M <- ncol(full_data_interactions)
  for (i in seq(from=1,to=M,by=4))
  {
        par(mfrow=c(2,2))
        if (is.factor(full_data_interactions[,i])) {
              hist(table(full_data_interactions[,i]),100,main = names(full_data_interactions)[i])     
        } else {
              hist((full_data_interactions[,i]),100,main = names(full_data_interactions)[i])}
        if (is.factor(full_data_interactions[,i+1])) {
              hist(table(full_data_interactions[,i+1]),100,main = names(full_data_interactions)[i+1])     
        } else {
              hist((full_data_interactions[,i+1]),100,main = names(full_data_interactions)[i+1])}
        if (is.factor(full_data_interactions[,i+2])) {
              hist(table(full_data_interactions[,i+2]),100,main = names(full_data_interactions)[i+2])     
        } else {
              hist((full_data_interactions[,i+2]),100,main = names(full_data_interactions)[i+2])}
        if (is.factor(full_data_interactions[,i+3])) {
              hist(table(full_data_interactions[,i+3]),100,main = names(full_data_interactions)[i+3])     
        } else {
              hist((full_data_interactions[,i+3]),100,main = names(full_data_interactions)[i+3])}
  }
  par(mfrow=c(1,1))
}

#Unite factors
{
  train$T1_V4[c(which(train$T1_V4=="G"),which(train$T1_V4=="H"),which(train$T1_V4=="S"))] <- "E"
  train$T1_V4 <- factor(train$T1_V4)
  train$T1_V5[c(which(train$T1_V5=="E"),which(train$T1_V5=="L"),which(train$T1_V5=="D"),which(train$T1_V5=="J"))] <- "B"
  train$T1_V5 <- factor(train$T1_V5)
  train$T1_V7[c(which(train$T1_V7=="C"),which(train$T1_V7=="A"))] <- "D"
  train$T1_V7 <- factor(train$T1_V7)
  train$T1_V8[c(which(train$T1_V8=="C"),which(train$T1_V8=="A"))] <- "D"
  train$T1_V8 <- factor(train$T1_V8)
  train$T1_V9[c(which(train$T1_V9=="C"),which(train$T1_V9=="G"),which(train$T1_V9=="B"))] <- "F"
  train$T1_V9 <- factor(train$T1_V9)
  train$T1_V11[c(which(train$T1_V11=="K"),which(train$T1_V11=="D"),which(train$T1_V11=="N"),which(train$T1_V11=="E"),which(train$T1_V11=="M"))] <- "F"
  train$T1_V11 <- factor(train$T1_V11)
  train$T1_V12[c(which(train$T1_V12=="C"),which(train$T1_V12=="A"))] <- "D"
  train$T1_V12 <- factor(train$T1_V12)
  train$T1_V15[c(which(train$T1_V15=="F"),which(train$T1_V15=="S"),which(train$T1_V15=="W"),which(train$T1_V15=="H"),which(train$T1_V15=="D"),which(train$T1_V15=="C"))] <- "N"
  train$T1_V15 <- factor(train$T1_V15)
  train$T1_V16[c(which(train$T1_V16=="F"),which(train$T1_V16=="O"),which(train$T1_V16=="Q"),which(train$T1_V16=="H"),which(train$T1_V16=="P"))] <- "G"
  train$T1_V16 <- factor(train$T1_V16)
  train$T2_V5[c(which(train$T2_V5=="F"),which(train$T2_V5=="E"),which(train$T2_V5=="D"))] <- "C"
  train$T2_V5 <- factor(train$T2_V5)
  train$T2_V13[c(which(train$T2_V13=="B"))] <- "D"
  train$T2_V13 <- factor(train$T2_V13)
  
  test$T1_V4[c(which(test$T1_V4=="G"),which(test$T1_V4=="H"),which(test$T1_V4=="S"))] <- "E"
  test$T1_V4 <- factor(test$T1_V4)
  test$T1_V5[c(which(test$T1_V5=="E"),which(test$T1_V5=="L"),which(test$T1_V5=="D"),which(test$T1_V5=="J"))] <- "B"
  test$T1_V5 <- factor(test$T1_V5)
  test$T1_V7[c(which(test$T1_V7=="C"),which(test$T1_V7=="A"))] <- "D"
  test$T1_V7 <- factor(test$T1_V7)
  test$T1_V8[c(which(test$T1_V8=="C"),which(test$T1_V8=="A"))] <- "D"
  test$T1_V8 <- factor(test$T1_V8)
  test$T1_V9[c(which(test$T1_V9=="C"),which(test$T1_V9=="G"),which(test$T1_V9=="B"))] <- "F"
  test$T1_V9 <- factor(test$T1_V9)
  test$T1_V11[c(which(test$T1_V11=="K"),which(test$T1_V11=="D"),which(test$T1_V11=="N"),which(test$T1_V11=="E"),which(test$T1_V11=="M"))] <- "F"
  test$T1_V11 <- factor(test$T1_V11)
  test$T1_V12[c(which(test$T1_V12=="C"),which(test$T1_V12=="A"))] <- "D"
  test$T1_V12 <- factor(test$T1_V12)
  test$T1_V15[c(which(test$T1_V15=="F"),which(test$T1_V15=="S"),which(test$T1_V15=="W"),which(test$T1_V15=="H"),which(test$T1_V15=="D"),which(test$T1_V15=="C"))] <- "N"
  test$T1_V15 <- factor(test$T1_V15)
  test$T1_V16[c(which(test$T1_V16=="F"),which(test$T1_V16=="O"),which(test$T1_V16=="Q"),which(test$T1_V16=="H"),which(test$T1_V16=="P"))] <- "G"
  test$T1_V16 <- factor(test$T1_V16)
  test$T2_V5[c(which(test$T2_V5=="F"),which(test$T2_V5=="E"),which(test$T2_V5=="D"))] <- "C"
  test$T2_V5 <- factor(test$T2_V5)
  test$T2_V13[c(which(test$T2_V13=="B"))] <- "D"
  test$T2_V13 <- factor(test$T2_V13)
  
  full_data$T1_V4[c(which(full_data$T1_V4=="G"),which(full_data$T1_V4=="H"),which(full_data$T1_V4=="S"))] <- "E"
  full_data$T1_V4 <- factor(full_data$T1_V4)
  full_data$T1_V5[c(which(full_data$T1_V5=="E"),which(full_data$T1_V5=="L"),which(full_data$T1_V5=="D"),which(full_data$T1_V5=="J"))] <- "B"
  full_data$T1_V5 <- factor(full_data$T1_V5)
  full_data$T1_V7[c(which(full_data$T1_V7=="C"),which(full_data$T1_V7=="A"))] <- "D"
  full_data$T1_V7 <- factor(full_data$T1_V7)
  full_data$T1_V8[c(which(full_data$T1_V8=="C"),which(full_data$T1_V8=="A"))] <- "D"
  full_data$T1_V8 <- factor(full_data$T1_V8)
  full_data$T1_V9[c(which(full_data$T1_V9=="C"),which(full_data$T1_V9=="G"),which(full_data$T1_V9=="B"))] <- "F"
  full_data$T1_V9 <- factor(full_data$T1_V9)
  full_data$T1_V11[c(which(full_data$T1_V11=="K"),which(full_data$T1_V11=="D"),which(full_data$T1_V11=="N"),which(full_data$T1_V11=="E"),which(full_data$T1_V11=="M"))] <- "F"
  full_data$T1_V11 <- factor(full_data$T1_V11)
  full_data$T1_V12[c(which(full_data$T1_V12=="C"),which(full_data$T1_V12=="A"))] <- "D"
  full_data$T1_V12 <- factor(full_data$T1_V12)
  full_data$T1_V15[c(which(full_data$T1_V15=="F"),which(full_data$T1_V15=="S"),which(full_data$T1_V15=="W"),which(full_data$T1_V15=="H"),which(full_data$T1_V15=="D"),which(full_data$T1_V15=="C"))] <- "N"
  full_data$T1_V15 <- factor(full_data$T1_V15)
  full_data$T1_V16[c(which(full_data$T1_V16=="F"),which(full_data$T1_V16=="O"),which(full_data$T1_V16=="Q"),which(full_data$T1_V16=="H"),which(full_data$T1_V16=="P"))] <- "G"
  full_data$T1_V16 <- factor(full_data$T1_V16)
  full_data$T2_V5[c(which(full_data$T2_V5=="F"),which(full_data$T2_V5=="E"),which(full_data$T2_V5=="D"))] <- "C"
  full_data$T2_V5 <- factor(full_data$T2_V5)
  full_data$T2_V13[c(which(full_data$T2_V13=="B"))] <- "D"
  full_data$T2_V13 <- factor(full_data$T2_V13)
}

#Divide the train set to numeric & factors (we can manipulate the numeric data)
{
  factor_columns <- sapply(full_data, function(x) {is.factor(x)})
  factor_columns <- unname(which(factor_columns==TRUE))
  
  full_data_numeric <- full_data[,-c(1,factor_columns)]
  full_data_factor <- full_data[,c(factor_columns)]
  full_data <- full_data_numeric
  
  #Make the factor variables dummy variables
  for(i in 1:ncol(full_data_factor))
  {
    factor_col <- data.frame(value = full_data_factor[,i])
    for(level in levels(full_data_factor[,i])[-length(levels(full_data_factor[,i]))])
    {
      factor_col[paste(colnames(full_data_factor)[i], level, sep = "_")] <- ifelse(factor_col$value == level,1,0)      
    }
    names <- names(factor_col)
    factor_col <- setNames(as.data.frame(factor_col[,-1]),names[-1])
    full_data <- cbind(full_data,factor_col)
  }
  rm(full_data_numeric,full_data_factor,factor_col)
}

#Transformations
{
  train$T2_V2 <- log(train$T2_V2)
  test$T2_V2 <- log(test$T2_V2) 
  
  #For full data case
  full_data_interactions$T2_V10.T2_V15 <- log(full_data_interactions$T2_V10.T2_V15)
  full_data_interactions$T2_V10.T2_V14 <- log(full_data_interactions$T2_V10.T2_V14)
  full_data_interactions$T2_V7.T2_V14 <- log(full_data_interactions$T2_V7.T2_V14)
  full_data_interactions$T2_V5.T2_V14 <- log(full_data_interactions$T2_V5.T2_V14)
  full_data_interactions$T2_V4.T2_V15 <- log(full_data_interactions$T2_V4.T2_V15)
  full_data_interactions$T2_V4.T2_V14 <- log(full_data_interactions$T2_V4.T2_V14)
  full_data_interactions$T2_V2.T2_V14 <- log(full_data_interactions$T2_V2.T2_V14)
  full_data_interactions$T2_V2.T2_V6 <- log(full_data_interactions$T2_V2.T2_V6)
  full_data_interactions$T2_V2.T2_V8 <- log(full_data_interactions$T2_V2.T2_V8)
  full_data_interactions$T2_V1.T2_V15 <- log(full_data_interactions$T2_V1.T2_V15)
  full_data_interactions$T2_V2.T2_V4 <- log(full_data_interactions$T2_V2.T2_V4)
  full_data_interactions$T2_V1.T2_V14 <- log(full_data_interactions$T2_V1.T2_V14)
  full_data_interactions$T2_V1.T2_V4 <- log(full_data_interactions$T2_V1.T2_V4)
  full_data_interactions$T2_V1.T2_V2 <- log(full_data_interactions$T2_V1.T2_V2)
  full_data_interactions$T1_V16.T2_V2 <- log(full_data_interactions$T1_V16.T2_V2)
  full_data_interactions$T1_V15.T2_V2 <- log(full_data_interactions$T1_V15.T2_V2)
  full_data_interactions$T1_V14.T2_V2 <- log(full_data_interactions$T1_V14.T2_V2)
  full_data_interactions$T1_V13.T2_V1 <- log(full_data_interactions$T1_V13.T2_V1)
  full_data_interactions$T1_V13.T2_V2 <- log(full_data_interactions$T1_V13.T2_V2)
  full_data_interactions$T1_V12.T2_V2 <- log(full_data_interactions$T1_V12.T2_V2)
  full_data_interactions$T1_V10.T2_V2 <- log(full_data_interactions$T1_V10.T2_V2)
  full_data_interactions$T1_V9.T2_V2 <- log(full_data_interactions$T1_V9.T2_V2)
  full_data_interactions$T1_V8.T2_V2 <- log(full_data_interactions$T1_V8.T2_V2)
  full_data_interactions$T1_V7.T2_V2 <- log(full_data_interactions$T1_V7.T2_V2)
  full_data_interactions$T1_V6.T2_V2 <- log(full_data_interactions$T1_V6.T2_V2)
  full_data_interactions$T1_V4.T2_V2 <- log(full_data_interactions$T1_V4.T2_V2)
}

#Intercations
{
  full_data_interactions <- data.frame(model.matrix(~.^2,full_data)) #the large matrix with all 2nd order interactions      
  full_data_interactions <- full_data_interactions[,-1]
  colVariance <- NULL
  for (i in 1:ncol(full_data_interactions)) {
    cat(i,"\n")
    varcol <- var(full_data_interactions[,i])
    if (varcol==0) {colVariance <- c(colVariance,i)}
  }
  full_data_interactions <- full_data_interactions[,-c(colVariance)] #columns with no variance - remvoe them
}

#Identify highly correlated features
{
  #calculate correlation matrix
  correlationMatrix <- abs(cor(full_data_interactions))
  #find attributes that are highly corrected
  highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75,verbose = F)
  #print(highlyCorrelated)
  #Remove those features from the full_dataset
  full_data_interactions_no_correlation <- full_data_interactions[,-c(highlyCorrelated)]
}

#Stepwise Regression - response
{
  #for response
  fit <- lm(train$Hazard~.,data=train_interactions_no_correlation)
  step <- stepAIC(fit, direction="both",trace = F)
  train_interactions_no_correlation_stepAIC <- step$model #the final model
}

#Train a RF machine and calcuate gini w/o feature selection
{
  #train <- cbind(Hazard=train$Hazard,train_interactions_no_correlation)
  #train <- train[,-1]
  #train <- cbind(Hazard=train$Hazard,train_interactions)
  
  #Divide to train and eval - multifolds & other initializations
  {
    K <- 3 #number of folds (70-30)
    Times <- 1 #number of resamples (5)
    folds <- createMultiFolds(train$Hazard,k=K,times = Times)
    M <- ncol(train)
    N <- nrow(train)  
    ntrees <- c(50) # vector of trees, it is then multiplied by number of cpus (4)
    gini_OOB_ntrees <- rep(0,length(ntrees))
    gini_eval_ntrees <- rep(0,length(ntrees))
  }
  
  #Training RF and calulating AUC in multi folds validation
  {
    for (j in 1:length(ntrees))
    { 
      gini_OOB <- rep(0,K*Times)
      gini_eval <- rep(0,K*Times)
      for (i in 1:(K*Times))
      {
        model_RF <- randomForest(y=train$Hazard[c(folds[[i]])],x=train[c(folds[[i]]),-1],xtest =train[-c(folds[[i]]),-1],
                                 ytest = train$Hazard[-c(folds[[i]])],ntree=ntrees[j],do.trace=T,keep.forest=T)    
        
        #Fitting results
        gini_OOB[i] <- NormalizedGini(train$Hazard[c(folds[[i]])],model_RF$predicted)
        gini_eval[i] <- NormalizedGini(train$Hazard[-c(folds[[i]])],model_RF$test$predicted)
        
        #Plot err rate of train and oob
        plot(1:model_RF$ntree,model_RF$test$mse,type="l",main=paste("MSE, Out-fold vs. OOB",sep=""),xlab="Number of Trees",ylab="MSE")
        lines(1:model_RF$ntree,model_RF$mse,col="blue")
        
        #Plot err rate of train and oob
        plot(1:model_RF$ntree,model_RF$test$rsq,type="l",main=paste("R2, Out-fold vs. OOB",sep=""),xlab="Number of Trees",ylab="R2")
        lines(1:model_RF$ntree,model_RF$rsq,col="blue")
        
      }
      gini_OOB_ntrees[j] <- mean(gini_OOB)
      gini_eval_ntrees[j] <- mean(gini_eval)
    }
    #Plot eval and train score vs. number of trees
    plot(y=gini_OOB_ntrees,ntrees,main="Gini vs. Number of Trees",col=2,lwd=2,xlab="Number of Trees",ylab="gini",type = "l")
    lines(y=gini_eval_ntrees,ntrees,col=3,lwd=2)
  }

  #RF classifier on all data & calcualte best date for crop per city
  {    
    model_RF <- randomForest(y=train$Hazard,x=train[,-1],ntree=ntrees[j],do.trace=T,keep.forest=T)    
    saveRDS(model_RF,"model_RF.RDS")   
  }
}

#Performing PLS 
{
      train <- train[,-1]
      #train <- train_interactions_no_correlation_stepAIC_for_g0
      P <- ncol(train) #max number of component for the PLS (after validation ithis is not the case)
      rsquare_fitted <- rep(0,P)
      rsquare <- rep(0,P)
      MSE_fitted <- rep(0,P)
      MSE <- rep(0,P)
      pls_fitted <- plsr(Hazard~.,data=train,ncomp=P,validation="LOO",model=T)
      for (i in 1:P)
      {
            rsquare_fitted[i] <- round(1-sum((train$Hazard-pls_fitted$fitted.values[,1,i])^2)/sum((train$Hazard-mean(train$Hazard))^2),2)
            rsquare[i] <- round(1-sum((train$Hazard-pls_fitted$validation$pred[,1,i])^2)/sum((train$Hazard-mean(train$Hazard))^2),2)
            MSE_fitted[i] <- mean((train$Hazard-pls_fitted$fitted.values[,1,i])^2)
            MSE[i] <- mean((train$Hazard-pls_fitted$validation$pred[,1,i])^2)
      }
      
      plot(1:P,MSE_fitted,'b')
      plot(1:P,MSE,'b')
      
      plot(1:P,rsquare_fitted,'b',col='red',xlab="components",ylab="R^2")
      title("R^2, mean of both parameters, fitted ", cex.main = 0.9);
      lines(1:P,rsquare_fitted,col = 'blue')
      
      plot(1:P,rsquare,'b',col='red',xlab="components",ylab="R^2")
      title("R^2, mean of both parameters, LOO ", cex.main = 0.9);
      lines(1:P,rsquare,col = 'blue')
      
      P_opt = which.max(rsquare) #it will be changed after validation
      
      plot(train$Hazard,pls_fitted$fitted.values[,1,P_opt],type = "p", xlab="Original", ylab = "Predicted")
      title(paste("Hazard - Obserevd vs. Fitted, R^2: ", rsquare_fitted[P_opt]), cex.main = 0.9)
      fit <- lm(pls_fitted$fitted.values[,1,P_opt] ~ train$Hazard) 
      abline(a = fit$coefficients[1],b = fit$coefficients[2],col="red",lwd = 2)
      
      plot(train$Hazard,pls_fitted$validation$pred[,1,P_opt],type = "p", xlab="Original", ylab = "Predicted")
      title(paste("Hazard - Obserevd vs. Predicted, R^2: ", rsquare[P_opt]), cex.main = 0.9)
      fit <- lm(pls_fitted$validation$pred[,1,P_opt] ~ train$Hazard) 
      abline(a = fit$coefficients[1],b = fit$coefficients[2],col="red",lwd = 2)      
}

#Preprocess the full_data
{
      for (i in 1:ncol(full_data_interactions_no_correlation)) {
      full_data_interactions_no_correlation[,i] <- factor(full_data_interactions_no_correlation[,i])
      }
      full_data_interactions_no_correlation$T1_V3.T1_V8_B <- as.numeric(as.character(full_data_interactions_no_correlation$T1_V3.T1_V8_B))
      full_data_interactions_no_correlation$T2_V1.T1_V17_N <- as.numeric(as.character(full_data_interactions_no_correlation$T2_V1.T1_V17_N))
      full_data_interactions_no_correlation$T2_V9.T1_V15_A <- as.numeric(as.character(full_data_interactions_no_correlation$T2_V9.T1_V15_A))
      full_data_interactions_no_correlation$T2_V15.T1_V5_H <- as.numeric(as.character(full_data_interactions_no_correlation$T2_V15.T1_V5_H))
      full_data_interactions_no_correlation$T2_V15.T1_V11_A <- as.numeric(as.character(full_data_interactions_no_correlation$T2_V15.T1_V11_A))
      full_data_interactions_no_correlation$T2_V15.T1_V11_B <- as.numeric(as.character(full_data_interactions_no_correlation$T2_V15.T1_V11_B))
      full_data_interactions_no_correlation$T2_V15.T2_V5_B <- as.numeric(as.character(full_data_interactions_no_correlation$T2_V15.T2_V5_B))
      full_data_interactions_no_correlation$T2_V15.T2_V11_N <- as.numeric(as.character(full_data_interactions_no_correlation$T2_V15.T2_V11_N))
}

#Performing XGBOOST 
{
      train <- train[,-1]
      test <- test[,-1]
      #train <- cbind(Hazard=train$Hazard,train_interactions_no_correlation[,-1])
      Hazard=train$Hazard
      train <- cbind(Hazard=Hazard,full_data_interactions_no_correlation[1:50999,])
      test <- full_data_interactions_no_correlation[51000:101999,]
      #train[,126] <- NULL
      #test[,125] <- NULL
      #train[,138] <- NULL
      #test[,137] <- NULL
      #train[,196] <- NULL
      #test[,195] <- NULL
      
      # replace factors with level mean hazard - only if not done before
      for (i in 2:ncol(train)) {
            if (class(train[,i])=="factor") {
                  mm <- aggregate(train$Hazard~train[,i], data=train, mean)
                  levels(train[,i]) <- as.numeric(mm[,2]) 
                  levels(test[,i-1]) <- mm[,2] 
                  train[,i] <- as.numeric(as.character(train[,i]))  
                  test[,i-1] <- as.numeric(as.character(test[,i-1]))
            }
      }

      # train & tune --skipped--
      logfile <- data.frame(shrinkage=c(0.1, 0.015, 0.015, 0.005, 0.01),
                            rounds = c(50, 50, 50, 55, 50),
                            depth = c(4, 4, 5, 5, 6),
                            gamma = c(0.05, 0, 0, 0.5, 0.25),
                            min.child = c(5, 5, 5, 5, 5),
                            colsample.bytree = c(0.6, 0.6, 0.65, 0.5, 0.85),
                            subsample = c(0.9, 0.9, 0.95, 0.95, 0.8))
      
      #Divide to train and eval - multifolds & other initializations
      {
            K <- 3 #number of folds (70-30)
            Times <- 2 #number of resamples (5)
            folds <- createMultiFolds(train$Hazard,k=K,times = Times)
            M <- ncol(train)
            N <- nrow(train)  
      }
      
      # generate final prediction -- bag of 50 models --
      models <- 5
      repeats <- 5 #only to replace cross validation
      gini.eval <- matrix(0,models,repeats)
      gini.train <- matrix(0,models,repeats)
      for (j in 1:repeats) {
            for (i in 1:models){
                  for (k in 1:(K*Times)) {
                        param <- list(max.depth = logfile$depth[i], eta = logfile$shrinkage[i], silent = 1, objective="reg:linear",eval_metric=NormalizedGiniXgboost,subsample=logfile$subsample[i],
                                      colsample_bytree=logfile$colsample.bytree[i], gamma=logfile$gamma[i], min.child.weight=logfile$min.child[i])
                        dtrain <- xgb.DMatrix(as.matrix(train[c(folds[[k]]),-1]), label = as.numeric(train$Hazard[c(folds[[k]])]))
                        dtest <- xgb.DMatrix(as.matrix(train[-c(folds[[k]]),-1]), label = as.numeric(train$Hazard[-c(folds[[k]])]))     
                        watchlist <- list(eval = dtest, train = dtrain)
                        xgboost.mod <- xgb.train(param, dtrain, nthread = 4, nround = logfile$rounds[i], watchlist,verbose=1,print.every.n=10)
                        
                        yhat.train  <- predict(xgboost.mod,as.matrix(train[c(folds[[k]]),-1])) 
                        yhat.eval  <- predict(xgboost.mod,as.matrix(train[-c(folds[[k]]),-1])) 
                        gini.eval[i,j] <- gini.eval[i,j] + NormalizedGini(train$Hazard[-c(folds[[k]])],yhat.eval)
                        cat(gini.eval[i,j]/k,"\n")
                        gini.train[i,j] <- gini.train[i,j] + NormalizedGini(train$Hazard[c(folds[[k]])],yhat.train)     
                        cat(gini.train[i,j]/k,"\n")
                  }
                  gini.eval[i,j] <-  gini.eval[i,j]/(K*Times)
                  gini.train[i,j] <-  gini.train[i,j]/(K*Times)
            }
      }
      
      best_model <- which.min(rmse.eval)
      best_model <- which.max(gini.eval)
      i <- best_model
      yhat.test  <- rep(0,nrow(test))
      for (j in 1:repeats) {
      param <- list(max.depth = logfile$depth[i], eta = logfile$shrinkage[i], silent = 1, objective="reg:linear",eval_metric=NormalizedGiniXgboost,subsample=logfile$subsample[i],
                    colsample_bytree=logfile$colsample.bytree[i], gamma=logfile$gamma[i], min.child.weight=logfile$min.child[i])
      dtrain <- xgb.DMatrix(as.matrix(train[,-1]), label = as.numeric(train$Hazard))
      watchlist <- list(train = dtrain)
      xgboost.mod <- xgb.train(param, dtrain, nthread = 4, nround = logfile$rounds[i],verbose=1,print.every.n=10,watchlist)
      yhat.test  <- yhat.test + predict(xgboost.mod,as.matrix(test))      
      }
      yhat.test <-  yhat.test/(repeats)
}

#save files
{
      write.csv(train_interactions_no_correlation,"train_interactions_no_correlation_smaller.csv")
      write.csv(full_data_interactions_no_correlation,"train_interactions_no_correlation_smaller.csv")
      write.csv(data.frame(Id=sample_submission$Id, Hazard=yhat.test),"R_xgboost_benchmark.csv",row.names=F, quote=FALSE)      
}
