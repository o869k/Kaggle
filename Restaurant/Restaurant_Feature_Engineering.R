#Initialization
{
  sessionInfo() #system pemodelormance
  gc() #clear unsued memory
  rm(list=ls()) # clear workspace
  mainDir <- "C:/Users/user/Documents/R/Restaurant"
  setwd(mainDir)
  set.seed(1234) #setting seed for comparison
  library(foreach); library(doParallel);
  library(ggplot2); library(randomForest);
  library(lubridate); library(e1071);
  library(FSelector); library(caret);
  library(RCurl); library(fpp);
  library(nnet);
}

#Read Data
{
  train <- read.csv("train.csv")  
  test <- read.csv("test.csv")
  Submission <- read.csv("sampleSubmission.csv")
  N <- nrow(train) #Number of examples train 
  Ntest <- nrow(test) #Number of examples test
  train <- train[,-1] #remove ID column
  test <- test[,-1] #remove ID column
}

#Create a factor of all cities names (train & test) and save as integers (too many facotrs)
{
  city_names <- sort(unique(c(levels(test$City),levels(train$City))))
  train$City <- as.integer(factor(train$City,levels = city_names))
  test$City <- as.integer(factor(test$City,levels = city_names)) 
}

#Create a factor of all restaurant types (train & test)
{
  rest_types <- sort(unique(c(levels(test$Type),levels(train$Type))))
  train$Type <- factor(train$Type,levels = rest_types)
  test$Type <- factor(test$Type,levels = rest_types)  
}

#Convert date field to separate columns: day, month and year (factorized) 
{
  train$day <- factor(day(as.POSIXlt(train$Open.Date, format="%m/%d/%Y")),levels = c(1:31))
  train$month <- factor(month(as.POSIXlt(train$Open.Date, format="%m/%d/%Y")),levels = c(1:12))
  train$year <- factor(year(as.POSIXlt(train$Open.Date, format="%m/%d/%Y")),levels = c(1995:2014))
  
  test$day <- factor(day(as.POSIXlt(test$Open.Date, format="%m/%d/%Y")),levels = c(1:31))
  test$month <- factor(month(as.POSIXlt(test$Open.Date, format="%m/%d/%Y")),levels = c(1:12))
  test$year <- factor(year(as.POSIXlt(test$Open.Date, format="%m/%d/%Y")),levels = c(1995:2014))    
  
  train <- train[,-1] #remove Open.Date column
  test <- test[,-1] #remove Open.Date colum
}

#Transform the Revenues Column - don't forget to transform back!
{
  train$revenue <- log10(train$revenue)
  hist(train$revenue)
  revenue <- train$revenue
  train <- train[,-41]
  train$revenue <- revenue
}

#Histograms of each feature in train - before transformations
for (i in seq(from=4,to=40,by=4)) {
  par(mfrow=c(2,2))
  hist((train[,i]),100,main = (i))
  hist((train[,i+1]),100,main = (i+1))
  hist((train[,i+2]),100,main = (i+2))
  hist((train[,i+3]),100,main = (i+3))
  par(mfrow=c(1,1))
}

#Histograms of each feature in test - before transformations
for (i in seq(from=4,to=40,by=4)) {
  par(mfrow=c(2,2))
  hist((test[,i]),100,main = (i))
  hist((test[,i+1]),100,main = (i+1))
  hist((test[,i+2]),100,main = (i+2))
  hist((test[,i+3]),100,main = (i+3))
  par(mfrow=c(1,1))
}

#Histograms of each feature in both train & test - before transformations
for (i in seq(from=4,to=40,by=4)) {
  par(mfrow=c(2,2))
  hist(c((test[,i]),(train[,i])),100,main = (i))
  hist(c((test[,i+1]),(train[,i+1])),100,main = (i+1))
  hist(c((test[,i+2]),(train[,i+2])),100,main = (i+2))
  hist(c((test[,i+3]),(train[,i+3])),100,main = (i+3))
  par(mfrow=c(1,1))
}

#Search features according to Mutual Information Criteria
{
  weights_mutual_info <- information.gain(revenue ~.,data=train)
  print(sort(weights_mutual_info$attr_importance))
  subset_mutual_info <- NULL
  subset_mutual_info <- row.names(weights_mutual_info)[which(weights_mutual_info$attr_importance > 0.1)]        
  features_MI <- as.simple.formula(subset_mutual_info, "revenue")
}

#Divide to train and eval - multifolds & other initializations
{
  K <- 3 #number of folds (70-30)
  Times <- 5 #number of resamples (5)
  folds <- createMultiFolds(train$revenue,k=K,times = Times)
  M <- ncol(train)
  ntree <- 1000
  rm(revenue,city_names,rest_types)
}

#function for feature selection evaluation on RF
evaluator_model <- function(subset) 
{  
  cat('Current Subset:' ,subset, "\n")
  ntree <- 1000
  rmse_train <- rep(0,(K*Times))
  rmse_eval <- rep(0,(K*Times))
  for (i in 1:(K*Times))
  {
    #predictors <- unname(c(sapply(subset,function(x) {which(x==names(train[,-M]))}))) #the specific predictor of current subset
    #tuneRF <- tuneRF(y = train[c(folds[[i]]),M],x = train[c(folds[[i]]),-M], stepFactor=1.1, ntreeTry=ntree,trace = F,plot = F)
    model <- randomForest(as.simple.formula(subset,"revenue"),data = train[c(folds[[i]]),], importance = T,ntree=ntree)  
    #varImpPlot(model)
    prediction <- predict(model,train[c(folds[[i]]),-M])
    rmse_train[i] <- sqrt(mean((10^train$revenue[c(folds[[i]])]-10^prediction)^2))
    prediction <- predict(model,train[-c(folds[[i]]),-M])
    rmse_eval[i] <- sqrt(mean((10^train$revenue[-c(folds[[i]])]-10^prediction)^2))
    rm(model,prediction)
  }
  cat('Current mean EVAL RMSE:' ,mean(rmse_eval), "\n")
  cat('Current sd EVAL RMSE:' ,sd(rmse_eval), "\n")
  cat('Current difference EVAL RMSE and TRAIN RMSE:' ,abs(mean(rmse_eval)-mean(rmse_train)), "\n")
  #return(-mean(rmse_eval)) #optimize on minimum mean RMSE
  #return(-sd(rmse_eval)) #optimize on minimum sd RMSE
  #return(-abs(mean(rmse_eval-rmse_train))) #optimize on RMSE difference between eval and train
  return(-log10(sd(rmse_eval))*log10(mean(rmse_eval))*log10(abs(mean(rmse_eval-rmse_train)))) #optimize on all metrics
}

#Backward search Feature Selection only
{
  subset_bs <- NULL
  subset_bs <- backward.search(sample(names(train[,-M])),evaluator_model)
  features_bs <- as.simple.formula(subset_bs, "revenue")          
  
  rmse_train <- rep(0,(K*Times))
  rmse_eval <- rep(0,(K*Times))
  for (i in 1:(K*Times))
  {
    model <- randomForest(features_bs,data = train[c(folds[[i]]),], importance = T,ntree=ntree)  
    #varImpPlot(model)
    prediction <- predict(model,train[c(folds[[i]]),-M])
    rmse_train[i] <- sqrt(mean((10^train$revenue[c(folds[[i]])]-10^prediction)^2))
    prediction <- predict(model,train[-c(folds[[i]]),-M])
    rmse_eval[i] <- sqrt(mean((10^train$revenue[-c(folds[[i]])]-10^prediction)^2))
  }
  cat('BFS EVAL RMSE:' ,mean(rmse_eval), "\n")
  cat('BFS TRAIN RMSE:' ,mean(rmse_train), "\n")
}

#BFS Feature Selection only
{
  subset_bfs <- NULL
  subset_bfs <- best.first.search(sample(names(train[,-M])),evaluator_model)
  features_bfs <- as.simple.formula(subset_bfs, "revenue") 
    
  rmse_train <- rep(0,(K*Times))
  rmse_eval <- rep(0,(K*Times))
  for (i in 1:(K*Times)) {
    #tuneRF <- tuneRF(y = train[c(folds[[i]]),M],x = train[c(folds[[i]]),-M], stepFactor=1.5, ntreeTry=ntree,trace = F,plot = F)
    model <- randomForest(features_bfs,data = train[c(folds[[i]]),], importance = T,ntree=ntree)
    #varImpPlot(model)
    prediction <- predict(model,train[c(folds[[i]]),-M])
    rmse_train[i] <- sqrt(mean((10^train$revenue[c(folds[[i]])]-10^prediction)^2))
    prediction <- predict(model,train[-c(folds[[i]]),-M])
    rmse_eval[i] <- sqrt(mean((10^train$revenue[-c(folds[[i]])]-10^prediction)^2))
  }
  cat('BFS EVAL RMSE:' ,mean(rmse_eval), "\n")
  cat('BFS TRAIN RMSE:' ,mean(rmse_train), "\n")
}

#Hill climbing search Feature Selection only
{
  subset_hc <- NULL
  subset_hc <- hill.climbing.search(sample(names(train[,-M])),evaluator_model)
  features_hc <- as.simple.formula(subset_hc, "revenue")          
  
  rmse_train <- rep(0,(K*Times))
  rmse_eval <- rep(0,(K*Times))
  for (i in 1:(K*Times))
  {
    model <- randomForest(features_hc,data = train[c(folds[[i]]),], importance = T,ntree=ntree)  
    #varImpPlot(model)
    prediction <- predict(model,train[c(folds[[i]]),-M])
    rmse_train[i] <- sqrt(mean((10^train$revenue[c(folds[[i]])]-10^prediction)^2))
    prediction <- predict(model,train[-c(folds[[i]]),-M])
    rmse_eval[i] <- sqrt(mean((10^(train$revenue[-c(folds[[i]])])-10^prediction)^2))
  }
  cat('BFS EVAL RMSE:' ,mean(rmse_eval), "\n")
  cat('BFS TRAIN RMSE:' ,mean(rmse_train), "\n")
}

#BFS Feature Selection after backword seacrh
{
  subset_bf_bfs <- NULL
  subset_bf_bfs <- best.first.search(subset_bs,evaluator_model)
  features_bf_bfs <- as.simple.formula(subset_bf_bfs, "revenue")          

  model <- randomForest(features_bf_bfs,data = train, importance = T,ntree=ntree)  
  varImpPlot(model)
  prediction <- predict(model,eval[,-M])
  rmse <- sqrt(mean((10^eval$revenue-10^prediction)^2))
  cat('BS_BFS EVAL RMSE:' ,rmse, "\n")
  prediction <- predict(model,train[,-M])
  rmse <- sqrt(mean((10^train$revenue-10^prediction)^2))
  cat('BS_BFS TRAIN RMSE:' ,rmse, "\n")
}

#Use selected features, then do grid search
{
  tuned <- tune.svm(revenue~., data = train, gamma = 10^(-6:-1), cost = 10^(-1:4),kernel="radial")
  mean_rmse_eval <- rep(0,length(parameter))
  sd_rmse_eval <- rep(0,length(parameter))
  diff_rmse_eval_rmse_train <- rep(0,length(parameter))
  for (j in 1:length(parameter))
  {
    rmse_train <- rep(0,(K*Times))
    rmse_eval <- rep(0,(K*Times))
    for (i in 1:(K*Times))
    {
      model <- svm(revenue~., data = train[c(folds[[i]]),], kernel="radial", gamma=tuned$best.parameters$gamma, cost=tuned$best.parameters$cost) 
      #varImpPlot(model)
      prediction <- predict(model,train[c(folds[[i]]),-M])
      rmse_train[i] <- sqrt(mean((10^train$revenue[c(folds[[i]])]-10^prediction)^2))
      prediction <- predict(model,train[-c(folds[[i]]),-M])
      rmse_eval[i] <- sqrt(mean((10^train$revenue[-c(folds[[i]])]-10^prediction)^2))
      rm(model,prediction)
    }
    mean_rmse_eval[j] <- mean(rmse_eval)
    sd_rmse_eval[j] <- sd(rmse_eval)
    diff_rmse_eval_rmse_train[j] <- abs(mean(rmse_eval)-mean(rmse_train))
    cat('Current mean EVAL RMSE:' ,mean_rmse_eval[j], "\n")
    cat('Current sd EVAL RMSE:' ,sd_rmse_eval[j], "\n")
    cat('Current difference EVAL RMSE and TRAIN RMSE:' ,diff_rmse_eval_rmse_train[j], "\n")
  }
}

#Tune model after feature selection, CV & resampling and grid search
{  
  model <- randomForest(features_bs,data = train, importance = T,ntree=ntree)  
  varImpPlot(model)
  saveRDS(model,file = 'model.Rdata')
  prediction <- predict(model,test)
  Submission$Prediction <- 10^prediction
  write.csv(Submission, file = "firstsubmit.csv", row.names = F)
  prediction <- predict(model,train[,-M])
  rmse <- sqrt(mean((10^train$revenue-10^prediction)^2))
  cat('FINAL TRAIN RMSE:' ,rmse, "\n")
}
