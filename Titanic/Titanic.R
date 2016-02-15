#Initialization
{
  sessionInfo() #system performance
  gc() #clear unsued memory
  rm(list=ls()) # clear workspace
  setwd("C:/Users/user/Documents/R/Titanic")
  set.seed(1234) #setting seed for comparison
  library(rpart); library(rattle)
  library(date); library(fpp)
  library(ff); library(ggplot2)  
  library(rpart.plot); library(RColorBrewer)
  library(e1071); library(UsingR)  
  library(randomForest); library(caret)
  library(party); library(car)
}

#Read Data
{
  trainSet <- read.csv("train.csv")
  testSet <- read.csv("test.csv")
  testSet$Survived <- rep(0,418)
}

###Pre Processing###

#Add an indication of a child (they mostly survived)
{
  trainSet$Child <- 0
  trainSet$Child[trainSet$Age < 18] <- 1
  testSet$Child <- 0
  testSet$Child[testSet$Age < 18] <- 1
}

#Imputation: Fix the empty embarkments & fares (must be in order for predict to work)
{
  trainSet$Embarked[which(trainSet$Embarked=="")] <- "S"
  trainSet$Embarked <- factor(trainSet$Embarked) # don't forget to reFactorize
  testSet$Embarked[which(testSet$Embarked=="")] <- "S"
  testSet$Embarked <- factor(testSet$Embarked) # don't forget to reFactorize
  trainSet$Fare[which(is.na(trainSet$Fare))] <- median(trainSet$Fare,na.rm = TRUE)
  testSet$Fare[which(is.na(testSet$Fare))] <- median(testSet$Fare,na.rm = TRUE)  
}

#Categorized the fares to 5 equal groups
{
  trainSet$Fare2 <- "70+"
  trainSet$Fare2[trainSet$Fare<70 & trainSet$Fare>=30] <- "30-70"
  trainSet$Fare2[trainSet$Fare<30 & trainSet$Fare>=20] <- "20-30"
  trainSet$Fare2[trainSet$Fare<20 & trainSet$Fare>=10] <- "10-30"
  trainSet$Fare2[trainSet$Fare<10 & trainSet$Fare>=7.8] <- "7.8-10"
  trainSet$Fare2[trainSet$Fare<7.8] <- "-7.8"
  trainSet$Fare2 <- factor(trainSet$Fare2)
  
  testSet$Fare2 <- "70+"
  testSet$Fare2[testSet$Fare<70 & testSet$Fare>=30] <- "30-70"
  testSet$Fare2[testSet$Fare<30 & testSet$Fare>=20] <- "20-30"
  testSet$Fare2[testSet$Fare<20 & testSet$Fare>=10] <- "10-30"
  testSet$Fare2[testSet$Fare<10 & testSet$Fare>=7.8] <- "7.8-10"
  testSet$Fare2[testSet$Fare<7.8] <- "-7.8"
  testSet$Fare2 <- factor(testSet$Fare2)
}

#Categorized the title of passangers to 8 groups
{
  trainSet$Title <- sapply(trainSet$Name , FUN=function(x) {strsplit(as.character(x), split='[,.]')[[1]][2]})
  testSet$Title <- sapply(testSet$Name , FUN=function(x) {strsplit(as.character(x), split='[,.]')[[1]][2]})
  trainSet$Title[trainSet$Title %in% c(' Capt', ' Col', ' Don', ' Major', ' Sir', ' Jonkheer')] <- 'Sir'
  testSet$Title[testSet$Title %in% c(' Capt', ' Col', ' Don', ' Major', ' Sir', ' Jonkheer')] <- 'Sir'
  trainSet$Title[trainSet$Title %in% c(' Dona', ' Lady', ' the Countess')] <- 'Lady'
  testSet$Title[testSet$Title %in% c(' Dona', ' Lady', ' the Countess')] <- 'Lady'  
  trainSet$Title[trainSet$Title %in% c(' Mme', ' Mlle')] <- ' Mrs'
  testSet$Title[testSet$Title %in% c(' Mme', ' Mlle')] <- ' Mrs'
  trainSet$Title[trainSet$Title %in% c(' Ms')] <- ' Miss'
  testSet$Title[testSet$Title %in% c(' Ms')] <- ' Miss'
  trainSet$Title <- factor(trainSet$Title)
  testSet$Title <- factor(testSet$Title)
}

#Categorized he Cabin of passangers to Decks
{
  trainSet$Deck <- sapply(trainSet$Cabin , FUN=function(x) {substring(as.character(x),1,1)})
  testSet$Deck <- sapply(testSet$Cabin , FUN=function(x) {substring(as.character(x),1,1)})
  trainSet$Deck[trainSet$Deck=="T"] <- "G" #just a factor fix
  trainSet$Deck <- factor(trainSet$Deck)
  testSet$Deck <- factor(testSet$Deck)
}

#Get ticket number
{
  trainSet$TicketNumber <- sapply(trainSet$Ticket , FUN=function(x) {strsplit(as.character(x), split=' ')[[1]][2]})
  trainSet$TicketNumber[is.na(trainSet$TicketNumber)] <- (as.character(trainSet$Ticket[is.na(trainSet$TicketNumber)]))
  testSet$TicketNumber <- sapply(testSet$Ticket , FUN=function(x) {strsplit(as.character(x), split=' ')[[1]][2]})
  testSet$TicketNumber[is.na(testSet$TicketNumber)] <- (as.character(testSet$Ticket[is.na(testSet$TicketNumber)]))
  trainSet$TicketNumber <- as.integer(trainSet$TicketNumber)
  testSet$TicketNumber <- as.integer(testSet$TicketNumber)
}

#Add total family Size
{
  trainSet$FamilySize <- trainSet$SibSp + trainSet$Parch
  testSet$FamilySize <- testSet$SibSp + testSet$Parch
  trainSet$FamilySize <- factor(trainSet$FamilySize)
  testSet$FamilySize <- factor(testSet$FamilySize)
}

#Imputation: Fix the age NA by a decision tree (mustn't be NA's in order for predict to work)
{
  AgefitTrain <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare2 + Embarked + Child + Title ,data=trainSet[!is.na(trainSet$Age),], method="anova") 
  AgefitTest <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare2 + Embarked + Child + Title ,data=testSet[!is.na(trainSet$Age),], method="anova")   
  trainSet$Age[(is.na(trainSet$Age))] <- predict(AgefitTrain,trainSet[(is.na(trainSet$Age)),])
  testSet$Age[(is.na(testSet$Age))] <- predict(AgefitTest,testSet[(is.na(testSet$Age)),])
}

###Training###

#Create Cross Validation
{
  Folds <- 3
  trainSetInd <- createFolds(trainSet$Survived,Folds,list=T,returnTrain=T)
  sapply(trainSetInd,length) #size of folds  
}

#Run for each model & evaluate
{  
  Err <- rep(0,9)
  
  #Random Forest - option I
  ErrRF <- rep(0,20)
  for (i in 1:Folds)
  {    
    for (j in 1:20)
    {
      RFfit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare2 + Embarked + Child + Title +FamilySize + Deck, data=trainSet[trainSetInd[[i]],], controls=cforest_unbiased(ntree=j*100, mtry=3))
      Survived <- predict(RFfit,trainSet[-trainSetInd[[i]],],OOB=TRUE,type = "response")
      ErrRF[j] <- ErrRF[j]+round(mean(abs((as.numeric(Survived)-1)-trainSet$Survived[-trainSetInd[[i]]])),digits=3)    
    }
  }
  plot(ErrRF/Folds,type="l")
  ErrTot <- c(ErrRF,ErrRF2)
  plot(ErrTot/Folds,type="l",col="red")
  
    #Random Forest - option II
    {
      Err[2] <- Err[2]+round(mean(abs((as.numeric(Survived)-1)-trainSet$Survived[-trainSetInd[[i]]])),digits=3)        
    }
    
    #Random Forest - option III
    {
      RFfit <- train(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare2 + Embarked + Child + Title, data=trainSet[trainSetInd[[i]],],method="rf",ntrees=2000)           
      Survived <- predict(RFfit,newdata=trainSet[-trainSetInd[[i]],])
      Err[3] <- Err[3]+round(mean(abs((as.numeric(Survived)-1)-trainSet$Survived[-trainSetInd[[i]]])),digits=3)        
    }
    
    #Boosting of trees - option IV
    {
      RFfit <- train(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare2 + Embarked + Child + Title, data=trainSet[trainSetInd[[i]],],method="gbm",verbose=F)           
      Survived <- predict(RFfit,newdata=trainSet[-trainSetInd[[i]],])
      Err[4] <- Err[4]+round(mean(abs((as.numeric(Survived)-1)-trainSet$Survived[-trainSetInd[[i]]])),digits=3)        
    }
    
    #NaiveBayes - option V
    {
      RFfit <- train(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare2 + Embarked + Child + Title, data=trainSet[trainSetInd[[i]],],method="nb")           
      Survived <- predict(RFfit,newdata=trainSet[-trainSetInd[[i]],])
      Err[5] <- Err[5]+round(mean(abs((as.numeric(Survived)-1)-trainSet$Survived[-trainSetInd[[i]]])),digits=3)        
    }  
    
    #Adaboost - option VI
    {
      RFfit <- train(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare2 + Embarked + Child + Title, data=trainSet[trainSetInd[[i]],],method="ada")           
      Survived <- predict(RFfit,newdata=trainSet[-trainSetInd[[i]],])
      Err[6] <- Err[6]+round(mean(abs((as.numeric(Survived)-1)-trainSet$Survived[-trainSetInd[[i]]])),digits=3)        
    }  

    #GLM - option VII
    {
      RFfit <- train(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare2 + Embarked + Child + Title, data=trainSet[trainSetInd[[i]],],method="glm")           
      Survived <- predict(RFfit,newdata=trainSet[-trainSetInd[[i]],])
      Err[7] <- Err[7]+round(mean(abs((as.numeric(Survived)-1)-trainSet$Survived[-trainSetInd[[i]]])),digits=3)        
    } 
    
    #KNN - option VIII
    {
      RFfit <- train(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare2 + Embarked + Child + Title, data=trainSet[trainSetInd[[i]],],method="knn")           
      Survived <- predict(RFfit,newdata=trainSet[-trainSetInd[[i]],])
      Err[8] <- Err[8]+round(mean(abs((as.numeric(Survived)-1)-trainSet$Survived[-trainSetInd[[i]]])),digits=3)        
    } 
    
    #SVM - option VII
    {
      RFfit <- train(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare2 + Embarked + Child + Title, data=trainSet[trainSetInd[[i]],],method="svmRadial")           
      Survived <- predict(RFfit,newdata=trainSet[-trainSetInd[[i]],])
      Err[9] <- Err[9]+round(mean(abs((as.numeric(Survived)-1)-trainSet$Survived[-trainSetInd[[i]]])),digits=3)        
    } 
  }
  mean(Err)
}

###Testing###
{ 
  RFfit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare2 + Embarked + Child + Title +FamilySize + Deck, data=trainSet, controls=cforest_unbiased(ntree=3*100, mtry=3))
  testSet$Survived <- predict(RFfit,testSet,OOB=TRUE,type = "response")
  
}

#Writing the result 
{
  result <- data.frame(PassengerId = testSet$PassengerId,Survived = testSet$Survived)
  write.csv(result,file="result_ori_kro.csv",row.names=FALSE)
}