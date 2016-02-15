#Initialization
{
  sessionInfo() #system performance
  gc() #clear unsued memory
  rm(list=ls()) # clear workspace
  setwd("C:/Users/user/Documents/R/Bicycle")
  set.seed(1234) #setting seed for comparison
  library(date); library(fpp)
  library(ff); library(ggplot2)
  library(caret); library(kernlab)
  library(e1071); library(UsingR)
  library(rpart); library(rattle)  
  library(randomForest); library(party)
  library(car);
}

#Read Data & PreAllocation: 
{
  trainSet <- read.csv("train.csv") 
  N <- nrow(trainSet)  #Number of train samples
  testSet <- read.csv("test.csv") 
  Nt <- nrow(testSet) #Number of test samples
  M <- ncol(testSet)-1 #Number of features per sample
}

###Preprocessing###

#Seperate Date (year, month, weekday) & Time (hour) and factor them
{
  trainSet$Hour <- sapply(trainSet$datetime, FUN=function(x) {strsplit(as.character(x),split=" ")[[1]][2]})
  trainSet$Hour <- sapply(trainSet$Hour, FUN=function(x) factor(as.numeric({strsplit(as.character(x),split=":")[[1]][1]})))
  testSet$Hour <- sapply(testSet$datetime, FUN=function(x) {strsplit(as.character(x),split=" ")[[1]][2]})
  testSet$Hour <- sapply(testSet$Hour, FUN=function(x) factor(as.numeric({strsplit(as.character(x),split=":")[[1]][1]})))
  trainSet$Month <- sapply(trainSet$datetime, FUN=function(x) factor(as.numeric({strsplit(as.character(x),split="-")[[1]][2]})))
  testSet$Month <- sapply(testSet$datetime, FUN=function(x) factor(as.numeric({strsplit(as.character(x),split="-")[[1]][2]})))
  trainSet$Year <- sapply(trainSet$datetime, FUN=function(x) factor(as.numeric({strsplit(as.character(x),split="-")[[1]][1]})))
  testSet$Year <- sapply(testSet$datetime, FUN=function(x) factor(as.numeric({strsplit(as.character(x),split="-")[[1]][1]})))
  trainSet$Weekday = factor(weekdays(strptime(as.character(trainSet$datetime), format="%Y-%m-%d %H:%M:%S")))
  testSet$Weekday = factor(weekdays(strptime(as.character(testSet$datetime), format="%Y-%m-%d %H:%M:%S")))
}

#Factor holiday,workingday,season,weather
{
  trainSet$weather[which(trainSet$weather==4)] <- 3
  testSet$weather[which(testSet$weather==4)] <- 3
  trainSet$season <- factor(trainSet$season); testSet$season <- factor(testSet$season)
  trainSet$holiday <- factor(trainSet$holiday); testSet$holiday <- factor(testSet$holiday)
  trainSet$workingday <- factor(trainSet$workingday); testSet$workingday <- factor(testSet$workingday)
  trainSet$weather <- factor(trainSet$weather); testSet$weather <- factor(testSet$weather)  
}

#Grouping humidity,windspeed,atemp,temp
{
  trainSet$windspeed_new <- "20+"; testSet$windspeed_new <- "20+";
  trainSet$windspeed_new[trainSet$windspeed<20 & trainSet$windspeed>=15] <- "15-20"; testSet$windspeed_new[testSet$windspeed<20 & testSet$windspeed>=15] <- "15-20";
  trainSet$windspeed_new[trainSet$windspeed<15 & trainSet$windspeed>=10] <- "10-15"; testSet$windspeed_new[testSet$windspeed<15 & testSet$windspeed>=10] <- "10-15";
  trainSet$windspeed_new[trainSet$windspeed<10 & trainSet$windspeed>=5] <- "5-10"; testSet$windspeed_new[testSet$windspeed<10 & testSet$windspeed>=5] <- "5-10";
  trainSet$windspeed_new[trainSet$windspeed<5] <- "5-"; testSet$windspeed_new[testSet$windspeed<5] <- "5-";
  trainSet$windspeed_new <- factor(trainSet$windspeed_new); testSet$windspeed_new <- factor(testSet$windspeed_new);
  
  trainSet$humidity_new <- "80+"; testSet$humidity_new <- "80+"
  trainSet$humidity_new[trainSet$humidity<80 & trainSet$humidity>=60] <- "60-80"; testSet$humidity_new[testSet$humidity<80 & testSet$humidity>=60] <- "60-80"
  trainSet$humidity_new[trainSet$humidity<60 & trainSet$humidity>=40] <- "40-60"; testSet$humidity_new[testSet$humidity<60 & testSet$humidity>=40] <- "40-60"
  trainSet$humidity_new[trainSet$humidity<40] <- "40-"; testSet$humidity_new[testSet$humidity<40] <- "40-"
  trainSet$humidity_new <- factor(trainSet$humidity_new); testSet$humidity_new <- factor(testSet$humidity_new);
  
  trainSet$atemp_new <- "30+"; testSet$atemp_new <- "30+"
  trainSet$atemp_new[trainSet$atemp<30 & trainSet$atemp>=25] <- "25-30"; testSet$atemp_new[testSet$atemp<30 & testSet$atemp>=25] <- "25-30"
  trainSet$atemp_new[trainSet$atemp<25 & trainSet$atemp>=20] <- "20-25"; testSet$atemp_new[testSet$atemp<25 & testSet$atemp>=20] <- "20-25"
  trainSet$atemp_new[trainSet$atemp<20 & trainSet$atemp>=15] <- "15-20"; testSet$atemp_new[testSet$atemp<20 & testSet$atemp>=15] <- "15-20"
  trainSet$atemp_new[trainSet$atemp<15 & trainSet$atemp>=10] <- "10-15"; testSet$atemp_new[testSet$atemp<15 & testSet$atemp>=10] <- "10-15"
  trainSet$atemp_new[trainSet$atemp<10] <- "10-"; testSet$atemp_new[testSet$atemp<10] <- "10-"
  trainSet$atemp_new <- factor(trainSet$atemp_new); testSet$atemp_new <- factor(testSet$atemp_new);

  trainSet$temp_new <- "30+"; testSet$temp_new <- "30+"
  trainSet$temp_new[trainSet$temp<30 & trainSet$temp>=25] <- "25-30"; testSet$temp_new[testSet$temp<30 & testSet$temp>=25] <- "25-30"
  trainSet$temp_new[trainSet$temp<25 & trainSet$temp>=20] <- "20-25"; testSet$temp_new[testSet$temp<25 & testSet$temp>=20] <- "20-25"
  trainSet$temp_new[trainSet$temp<20 & trainSet$temp>=15] <- "15-20"; testSet$temp_new[testSet$temp<20 & testSet$temp>=15] <- "15-20"
  trainSet$temp_new[trainSet$temp<15 & trainSet$temp>=10] <- "10-15"; testSet$temp_new[testSet$temp<15 & testSet$temp>=10] <- "10-15"
  trainSet$temp_new[trainSet$temp<10] <- "10-"; testSet$temp_new[testSet$temp<10] <- "10-"
  trainSet$temp_new <- factor(trainSet$temp_new); testSet$temp_new <- factor(testSet$temp_new);
 
}

#Correlation of Spatial data and target data
{
  scatterplotMatrix(trainSet[c(2:9,13:16)])
  cor(trainSet[c(2:9,13:16)])
  skewness(trainSet[c(2:9,13:16)]) 
  
  scatterplotMatrix(trainSet[c(10:12)])
  cor(trainSet[c(10:12)])
  skewness(trainSet[10,])   
}

###TRAINING###

#Create a K-folds cross validation
{
  K = 3 #number of folds
  folds <- createFolds(trainSet$season,K)
  sapply(folds,length) #size of folds
}

#Random Forest - option I
{
  ErrCas <- matrix(data = 0,nrow = K,ncol = 20)
  ErrReg <- matrix(data = 0,nrow = K,ncol = 20)
  
  for (i in 1:K)
  {    
    for (j in 1:20)
    {
      ModelCas <- randomForest(casual ~ season + holiday + workingday + weather + temp_new + atemp_new + humidity_new + windspeed_new + Hour + Month + Year + Weekday, data=trainSet[-folds[[i]],], importance=TRUE, ntree=j*100)
      EvalCas <- predict(ModelCas,trainSet[folds[[i]],])
      varImpPlot(ModelCas); gc()
      ErrCas[i,j] <- round(sqrt(mean((log(EvalCas+1)-log(trainSet$casual[folds[[i]]]+1))^2)),digits=3) #RMSLE      
      ModelReg <- randomForest(registered ~ season + holiday + workingday + weather + temp_new + atemp_new + humidity_new + windspeed_new + Hour + Month + Year + Weekday, data=trainSet[-folds[[i]],], importance=TRUE, ntree=j*100)
      EvalReg <- predict(ModelReg,trainSet[folds[[i]],])
      varImpPlot(ModelReg); gc()
      ErrReg[i,j] <- round(sqrt(mean((log(EvalReg+1)-log(trainSet$registered[folds[[i]]]+1))^2)),digits=3) #RMSLE
      print(j)
    }
    print(k)
  }
  plot(ErrCas/Folds,type="l")  
  plot(ErrReg/Folds,type="l")
}

#Stochastic Gradient Boosting - option II
{
  ErrCas2 <- matrix(data = 0,nrow = K,ncol = 20)
  ErrReg2 <- matrix(data = 0,nrow = K,ncol = 20)
  
  for (i in 1:K)
  {    
    for (j in 1:20)
    {
      ModelCas <- train(casual ~ season + holiday + workingday + weather + temp_new + atemp_new + humidity_new + windspeed_new+ Hour + Month + Year + Weekday, data=trainSet[-folds[[i]],], method="gbm", n.trees=j*100)
      EvalCas <- predict(ModelCas,trainSet[folds[[i]],])
      ErrCas2[i,j] <- round(sqrt(mean((log(EvalCas+1)-log(trainSet$casual[folds[[i]]]+1))^2)),digits=3) #RMSLE      
      gc()
      ModelReg <- train(registered ~ season + holiday + workingday + weather + temp_new + atemp_new + humidity_new + windspeed_new + Hour + Month + Year + Weekday, data=trainSet[-folds[[i]],], method="gbm", n.trees=j*100)
      EvalReg <- predict(ModelReg,trainSet[folds[[i]],])
      ErrReg2[i,j] <- round(sqrt(mean((log(EvalReg+1)-log(trainSet$registered[folds[[i]]]+1))^2)),digits=3) #RMSLE
      gc()
      print(j)
    }
    print(k)
  }
  plot(ErrReg2/Folds,type="l")  
  plot(ErrCas2/Folds,type="l")
}

###TESTING###
{
  RFCas <- train(casual ~ season + holiday + workingday + weather + atemp + humidity + windspeed + Time + Month, data=trainSet,method="rf",importance=TRUE,ntree=500,forest=TRUE) #RandomForest
  RFReg <- train(registered ~ season + holiday + workingday + weather + atemp + humidity + windspeed + Time + Month, data=trainSet,method="rf",importance=TRUE,ntree=500,forest=TRUE) #RandomForest
  EvalCas <- predict(ModelCas$finalModel,testSet)
  EvalReg <- predict(ModelReg$finalModel,testSet)
  Eval <- EvalCas + EvalReg
  Eval[which(Eval<0)] <- 0
}

#Writing the result 
{
  result <- data.frame(datetime=testSet$datetime,count=Eval)
  write.csv(result,file="result_ori_kro.csv",row.names=FALSE)
}