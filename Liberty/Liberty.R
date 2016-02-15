#Initialization
{
  sessionInfo() #system performance
  gc() #clear unsued memory
  rm(list=ls()) # clear workspace
  mainDir <- "C:/Users/user/Documents/R/Liberty/"
  perlDir <- "C:/Strawberry/perl/bin/perl.exe"
  setwd(mainDir)
  Sys.setlocale("LC_TIME", "English")
  set.seed(1234) #setting seed for comparison
  library(randomForest); library(caret);
  library(MASS);
}

#"NormalizedGini" is the other half of the metric. This function does most of the work, though
SumModelGini <- function(solution, submission) {
  df = data.frame(solution = solution, submission = submission)
  df <- df[order(df$submission, decreasing = TRUE),]
  df
  df$random = (1:nrow(df))/nrow(df)
  df
  totalPos <- sum(df$solution)
  df$cumPosFound <- cumsum(df$solution) # this will store the cumulative number of positive examples found (used for computing "Model Lorentz")
  df$Lorentz <- df$cumPosFound / totalPos # this will store the cumulative proportion of positive examples found ("Model Lorentz")
  df$Gini <- df$Lorentz - df$random # will store Lorentz minus random
  print(df)
  return(sum(df$Gini))
}
NormalizedGini <- function(solution, submission) {
  SumModelGini(solution, submission) / SumModelGini(solution, solution)
}

#Read Data
{
  train <- read.csv("C:/Users/user/Documents/R/Liberty/train.csv")
  train_interactions_no_correlation <- read.csv("C:/Users/user/Documents/R/Liberty/train_interactions_no_correlation_smaller.csv")
  test <- read.csv("C:/Users/user/Documents/R/Liberty/test.csv")
  sample_submission <- read.csv("C:/Users/user/Documents/R/Liberty/sample_submission.csv")      
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
}

#Divide the train set to numeric & factors (we can manipulate the numeric data)
{
  factor_columns <- sapply(train, function(x) {is.factor(x)})
  factor_columns <- unname(which(factor_columns==TRUE))
  
  train_numeric <- train[,-c(1,factor_columns)]
  train_factor <- train[,c(factor_columns)]
  train <- train_numeric
  
  #Make the factor variables dummy variables
  for(i in 1:ncol(train_factor))
  {
    factor_col <- data.frame(value = train_factor[,i])
    for(level in levels(train_factor[,i])[-length(levels(train_factor[,i]))])
    {
      factor_col[paste(colnames(train_factor)[i], level, sep = "_")] <- ifelse(factor_col$value == level,1,0)      
    }
    names <- names(factor_col)
    factor_col <- setNames(as.data.frame(factor_col[,-1]),names[-1])
    train <- cbind(train,factor_col)
  }
  rm(train_numeric,train_factor,factor_col)
}

#Transformations
{
  train$T2_V2 <- log(train$T2_V2)
}

#Intercations
{
  train_interactions <- data.frame(model.matrix(~.^2,train[,-1])) #the large matrix with all 2nd order interactions      
  train_interactions <- train_interactions[,-1]
  colVariance <- NULL
  for (i in 1:ncol(train_interactions)) {
    cat(i,"\n")
    varcol <- var(train_interactions[,i])
    if (varcol==0) {colVariance <- c(colVariance,i)}
  }
  train_interactions <- train_interactions[,-c(colVariance)] #columns with no variance - remvoe them
}

#Identify highly correlated features
{
  #calculate correlation matrix
  correlationMatrix <- abs(cor(train_interactions))
  #find attributes that are highly corrected
  highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5,verbose = F)
  #print(highlyCorrelated)
  #Remove those features from the trainset
  train_interactions_no_correlation <- train_interactions[,-c(highlyCorrelated)]
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
  train <- cbind(Hazard=train$Hazard,train_interactions_no_correlation)
  #Divide to train and eval - multifolds & other initializations
  {
    K <- 3 #number of folds (70-30)
    Times <- 1 #number of resamples (5)
    folds <- createMultiFolds(train$Hazard,k=K,times = Times)
    M <- ncol(train)
    N <- nrow(train)  
    ntrees <- c(100) # vector of trees, it is then multiplied by number of cpus (4)
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
