#Africa mining Kaggle competition
{
  rm(list=ls()) # clear workspace
  setwd("C:/Users/user/Documents/R/Africa")
  set.seed(1234) #setting seed for comparison
  library(caret); library(ggplot2)
  library(class); library(kernlab) 
  library(rpart); library(rattle) 
  library(rpart.plot); library(RColorBrewer) 
  library(randomForest); library(party)
  library(UsingR); library(ISLR)
  library(ElemStatLearn); 
  N <- 4 #size to cross validate
}

#Read Data
{
  trainSet <- read.csv("training.csv")
  testSet <- read.csv("sorted_test.csv")
}

soil_properties <- c("Ca", "P", "pH", "SOC", "Sand")
# CO2_bands <- 2656:2670

#Correlation matrix
# CorMat <- cor(trainSet[,2:(length(trainSet)-6)]); diag(CorMat) <- 0
# which(CorMat > 0.9,arr.ind=T) # high correlated features
#It turns out that there is a lot of correlatins between the columns - maybe try to reduce them

#Models with cross validation about for Ca
{
  trainSetCa <- trainSet[,2:(length(trainSet)-4)]
  trainFoldsCa <- createFolds(y=trainSetCa$Ca,k=N,list=TRUE,returnTrain = TRUE)
  sapply(trainFoldsCa,length) #size of folds
  
  GLMmodelerrCa <- vector(mode = "numeric", length = N)
  RFmodelerrCa <- vector(mode = "numeric", length = N)
  ADABoostmodelerrCa <- vector(mode = "numeric", length = N)
  
  preProcCaPCA <- preProcess(trainSetCa[,2:(length(trainSetCa)-2)],method="pca",pcaComp=10)
  trainSetCaPCA <- predict(preProcCaPCA,trainSetCa[,2:(length(trainSetCa)-2)])
  
  for(i in 1:N) 
  {  
    GLMmodelCa <- train(trainSetCa[trainFoldsCa[[i]],]$Ca ~ . ,data=trainSetCaPCA[trainFoldsCa[[i]],],method="glm") 
    GLMmodelCapredictions <- predict(GLMmodelCa$finalModel,newdata=trainSetCaPCA[-trainFoldsCa[[i]],])
    GLMmodelerrCa[i] <- round(sqrt(mean((GLMmodelCapredictions-trainSetCa[-trainFoldsCa[[i]],]$Ca)^2)),digits=3)
    
    RFmodelCa <- train(trainSetCa[trainFoldsCa[[i]],]$Ca ~ . ,data=trainSetCaPCA[trainFoldsCa[[i]],],method="rf") 
    RFmodelCapredictions <- predict(RFmodelCa,newdata=trainSetCaPCA[-trainFoldsCa[[i]],])
    RFmodelerrCa[i] <- round(sqrt(mean((RFmodelCapredictions-trainSetCa[-trainFoldsCa[[i]],]$Ca)^2)),digits=3)
    
    ADABoostmodelCa <- train(trainSetCa[trainFoldsCa[[i]],]$Ca ~ . ,data=trainSetCaPCA[trainFoldsCa[[i]],],method="ada") 
    ADABoostmodelCapredictions <- predict(ADABoostmodelCa$finalModel,newdata=trainSetCaPCA[-trainFoldsCa[[i]],])
    ADABoostmodelerrCa[i] <- round(sqrt(mean((ADABoostmodelCapredictions-trainSetCa[-trainFoldsCa[[i]],]$Ca)^2)),digits=3)    
  }
  mean(GLMmodelerrCa); mean(RFmodelerrCa); mean(ADABoostmodelerrCa)
}

#Models with cross validation about for P
{
  trainSetP <- trainSet[,c(2:(length(trainSet)-5),(length(trainSet)-3))]
  trainFoldsP <- createFolds(y=trainSetP$P,k=N,list=TRUE,returnTrain = TRUE)
  sapply(trainFoldsP,length) #size of folds
  
  GLMmodelerrP <- vector(mode = "numeric", length = N)
  RFmodelerrP <- vector(mode = "numeric", length = N)
  ADABoostmodelerrP <- vector(mode = "numeric", length = N)
  
  preProcPPCA <- preProcess(trainSetP[,2:(length(trainSetP)-2)],method="pca",pcaComp=10)
  trainSetPPCA <- predict(preProcPPCA,trainSetP[,2:(length(trainSetP)-2)])
  
  for(i in 1:N) 
  {  
    GLMmodelP <- train(trainSetP[trainFoldsP[[i]],]$P ~ . ,data=trainSetPPCA[trainFoldsP[[i]],],method="glm") 
    GLMmodelPpredictions <- predict(GLMmodelP$finalModel,newdata=trainSetPPCA[-trainFoldsP[[i]],])
    GLMmodelerrP[i] <- round(sqrt(mean((GLMmodelPpredictions-trainSetP[-trainFoldsP[[i]],]$P)^2)),digits=3)
    
    RFmodelP <- train(trainSetP[trainFoldsP[[i]],]$P ~ . ,data=trainSetPPCA[trainFoldsP[[i]],],method="rf") 
    RFmodelPpredictions <- predict(RFmodelP,newdata=trainSetPPCA[-trainFoldsP[[i]],])
    RFmodelerrP[i] <- round(sqrt(mean((RFmodelPpredictions-trainSetP[-trainFoldsP[[i]],]$P)^2)),digits=3)
    
    ADABoostmodelP <- train(trainSetP[trainFoldsP[[i]],]$P ~ . ,data=trainSetPPCA[trainFoldsP[[i]],],method="ada") 
    ADABoostmodelPpredictions <- predict(ADABoostmodelP$finalModel,newdata=trainSetPPCA[-trainFoldsP[[i]],])
    ADABoostmodelerrP[i] <- round(sqrt(mean((ADABoostmodelPpredictions-trainSetP[-trainFoldsP[[i]],]$P)^2)),digits=3)    
  }
  mean(GLMmodelerrP); mean(RFmodelerrP); mean(ADABoostmodelerrP)
  
}

#Models with cross validation about for pH
{
  trainSetpH <- trainSet[,c(2:(length(trainSet)-5),(length(trainSet)-2))]
  trainFoldspH <- createFolds(y=trainSetpH$pH,k=N,list=TRUE,returnTrain = TRUE)
  sapply(trainFoldspH,length) #size of folds
  
  GLMmodelerrpH <- vector(mode = "numeric", length = N)
  RFmodelerrpH <- vector(mode = "numeric", length = N)
  ADABoostmodelerrpH <- vector(mode = "numeric", length = N)
  
  preProcpHPCA <- preProcess(trainSetpH[,2:(length(trainSetpH)-2)],method="pca",pcaComp=10)
  trainSetpHPCA <- predict(preProcpHPCA,trainSetpH[,2:(length(trainSetpH)-2)])
  
  for(i in 1:N) 
  {  
    GLMmodelpH <- train(trainSetpH[trainFoldspH[[i]],]$pH ~ . ,data=trainSetpHPCA[trainFoldspH[[i]],],method="glm") 
    GLMmodelpHpredictions <- predict(GLMmodelpH$finalModel,newdata=trainSetpHPCA[-trainFoldspH[[i]],])
    GLMmodelerrpH[i] <- round(sqrt(mean((GLMmodelpHpredictions-trainSetpH[-trainFoldspH[[i]],]$pH)^2)),digits=3)
    
    RFmodelpH <- train(trainSetpH[trainFoldspH[[i]],]$pH ~ . ,data=trainSetpHPCA[trainFoldspH[[i]],],method="rf") 
    RFmodelpHpredictions <- predict(RFmodelpH,newdata=trainSetpHPCA[-trainFoldspH[[i]],])
    RFmodelerrpH[i] <- round(sqrt(mean((RFmodelpHpredictions-trainSetpH[-trainFoldspH[[i]],]$pH)^2)),digits=3)
    
    ADABoostmodelpH <- train(trainSetpH[trainFoldspH[[i]],]$pH ~ . ,data=trainSetpHPCA[trainFoldspH[[i]],],method="ada") 
    ADABoostmodelpHpredictions <- predict(ADABoostmodelpH$finalModel,newdata=trainSetpHPCA[-trainFoldspH[[i]],])
    ADABoostmodelerrpH[i] <- round(sqrt(mean((ADABoostmodelpHpredictions-trainSetpH[-trainFoldspH[[i]],]$pH)^2)),digits=3)    
  }
  mean(GLMmodelerrpH); mean(RFmodelerrpH); mean(ADABoostmodelerrpH)
  
}

#Models with cross validation about for SOC
{
  trainSetSOC <- trainSet[,c(2:(length(trainSet)-5),(length(trainSet)-1))]
  trainFoldsSOC <- createFolds(y=trainSetSOC$SOC,k=N,list=TRUE,returnTrain = TRUE)
  sapply(trainFoldsSOC,length) #size of folds
  
  GLMmodelerrSOC <- vector(mode = "numeric", length = N)
  RFmodelerrSOC <- vector(mode = "numeric", length = N)
  ADABoostmodelerrSOC <- vector(mode = "numeric", length = N)
  
  preProcSOCPCA <- preProcess(trainSetSOC[,2:(length(trainSetSOC)-2)],method="pca",pcaComp=10)
  trainSetSOCPCA <- predict(preProcSOCPCA,trainSetSOC[,2:(length(trainSetSOC)-2)])
  
  for(i in 1:N) 
  {  
    GLMmodelSOC <- train(trainSetSOC[trainFoldsSOC[[i]],]$SOC ~ . ,data=trainSetSOCPCA[trainFoldsSOC[[i]],],method="glm") 
    GLMmodelSOCpredictions <- predict(GLMmodelSOC$finalModel,newdata=trainSetSOCPCA[-trainFoldsSOC[[i]],])
    GLMmodelerrSOC[i] <- round(sqrt(mean((GLMmodelSOCpredictions-trainSetSOC[-trainFoldsSOC[[i]],]$SOC)^2)),digits=3)
    
    RFmodelSOC <- train(trainSetSOC[trainFoldsSOC[[i]],]$SOC ~ . ,data=trainSetSOCPCA[trainFoldsSOC[[i]],],method="rf") 
    RFmodelSOCpredictions <- predict(RFmodelSOC,newdata=trainSetSOCPCA[-trainFoldsSOC[[i]],])
    RFmodelerrSOC[i] <- round(sqrt(mean((RFmodelSOCpredictions-trainSetSOC[-trainFoldsSOC[[i]],]$SOC)^2)),digits=3)
    
    ADABoostmodelSOC <- train(trainSetSOC[trainFoldsSOC[[i]],]$SOC ~ . ,data=trainSetSOCPCA[trainFoldsSOC[[i]],],method="ada") 
    ADABoostmodelSOCpredictions <- predict(ADABoostmodelSOC$finalModel,newdata=trainSetSOCPCA[-trainFoldsSOC[[i]],])
    ADABoostmodelerrSOC[i] <- round(sqrt(mean((ADABoostmodelSOCpredictions-trainSetSOC[-trainFoldsSOC[[i]],]$SOC)^2)),digits=3)    
  }
  mean(GLMmodelerrSOC); mean(RFmodelerrSOC); mean(ADABoostmodelerrSOC)
  
}

#Models with cross validation about for Sand
{
  trainSetSand <- trainSet[,c(2:(length(trainSet)-5),(length(trainSet)))]
  trainFoldsSand <- createFolds(y=trainSetSand$Sand,k=N,list=TRUE,returnTrain = TRUE)
  sapply(trainFoldsSand,length) #size of folds
  
  GLMmodelerrSand <- vector(mode = "numeric", length = N)
  RFmodelerrSand <- vector(mode = "numeric", length = N)
  ADABoostmodelerrSand <- vector(mode = "numeric", length = N)
  
  preProcSandPCA <- preProcess(trainSetSand[,2:(length(trainSetSand)-2)],method="pca",pcaComp=10)
  trainSetSandPCA <- predict(preProcSandPCA,trainSetSand[,2:(length(trainSetSand)-2)])
  
  for(i in 1:N) 
  {  
    GLMmodelSand <- train(trainSetSand[trainFoldsSand[[i]],]$Sand ~ . ,data=trainSetSandPCA[trainFoldsSand[[i]],],method="glm") 
    GLMmodelSandpredictions <- predict(GLMmodelSand$finalModel,newdata=trainSetSandPCA[-trainFoldsSand[[i]],])
    GLMmodelerrSand[i] <- round(sqrt(mean((GLMmodelSandpredictions-trainSetSand[-trainFoldsSand[[i]],]$Sand)^2)),digits=3)
    
    RFmodelSand <- train(trainSetSand[trainFoldsSand[[i]],]$Sand ~ . ,data=trainSetSandPCA[trainFoldsSand[[i]],],method="rf") 
    RFmodelSandpredictions <- predict(RFmodelSand,newdata=trainSetSandPCA[-trainFoldsSand[[i]],])
    RFmodelerrSand[i] <- round(sqrt(mean((RFmodelSandpredictions-trainSetSand[-trainFoldsSand[[i]],]$Sand)^2)),digits=3)
    
    ADABoostmodelSand <- train(trainSetSand[trainFoldsSand[[i]],]$Sand ~ . ,data=trainSetSandPCA[trainFoldsSand[[i]],],method="ada") 
    ADABoostmodelSandpredictions <- predict(ADABoostmodelSand$finalModel,newdata=trainSetSandPCA[-trainFoldsSand[[i]],])
    ADABoostmodelerrSand[i] <- round(sqrt(mean((ADABoostmodelSandpredictions-trainSetSand[-trainFoldsSand[[i]],]$Sand)^2)),digits=3)    
  }
  mean(GLMmodelerrSand); mean(RFmodelerrSand); mean(ADABoostmodelerrSand)
  
}

#Selected Model
{
#     pca <- prcomp(trainSet[,2:(length(trainSet)-6)],scale=TRUE)
#     plot(pca) #plotting the eigen values vs. variance they hold
#     trainSet_pca <- pca$x[,1:10] %*% t(pca$rotation[,1:10]) # build the data from the 10 first principal components
  
    preProcPCA <- preProcess(trainSet[,2:(length(trainSet)-6)],method="pca",pcaComp=10)
    
    trainSetPCA <- predict(preProcPCA,trainSet[,2:(length(trainSet)-6)])
    testSetPCA <- predict(preProcPCA,testSet[,2:(length(testSet)-1)])
    
    modelFitCa <- train(trainSet$Ca ~ . ,data=trainSetPCA,method="rf") 
    predictionsCa <- predict(modelFitCa,newdata=testSetPCA) 
    modelFitP <- train(trainSet$P ~ . ,data=trainSetPCA,method="rf") 
    predictionsP <- predict(modelFitP,newdata=testSetPCA) 
    modelFitpH <- train(trainSet$pH ~ . ,data=trainSetPCA,method="rf") 
    predictionspH <- predict(modelFitpH,newdata=testSetPCA) 
    modelFitSOC <- train(trainSet$SOC ~ . ,data=trainSetPCA,method="rf") 
    predictionsSOC <- predict(modelFitSOC,newdata=testSetPCA) 
    modelFitSand <- train(trainSet$Sand ~ . ,data=trainSetPCA,method="rf") 
    predictionsSand <- predict(modelFitSand,newdata=testSetPCA) 

}

#Writing the result 
{
  result <- data.frame(PIDN = testSet$PIDN,Ca=predictionsCa,P=predictionsP,pH=predictionspH,SOC=predictionsSOC,Sand=predictionsSand)
  write.csv(result,file="result_ori_kro.csv",row.names=FALSE)
}

dev.off()