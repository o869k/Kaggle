#Initialization
{
  sessionInfo() #system performance
  gc() #clear unsued memory
  rm(list=ls()) # clear workspace
  mainDir <- "D:/Data/rossman"
  setwd(mainDir)
  Sys.setlocale("LC_TIME", "English")
  set.seed(1234) #setting seed for comparison
  library(caret); library(randomForest)
  library(lubridate); library(reshape2)
  library(foreach); library(doParallel)
  library(fpp); library(UsingR)
  library(kernlab); library(e1071)
  library(Hmisc); library(devtools)
}

#Read Data
{
  train <- read.csv("train.csv")
  test <- read.csv("test.csv")
  store <- read.csv("store.csv")
}

#Feature Engineering
{
  train$Date <- as.Date(train$Date)
  train <- train[order(train$Date),] #sort by date and store number
  train <- train[order(train$Store),]
  
  test$Date <- as.Date(test$Date)
  test <- test[order(test$Date),] #sort by date and store number
  test <- test[order(test$Store),]
  
  store$CompetitionDistance <- log(store$CompetitionDistance) #make a log transform
  store$CompetitionOpenSinceDate <- as.Date(paste0(store$CompetitionOpenSinceYear,"-",store$CompetitionOpenSinceMonth,"-01")) #real date of competitor open
  store$Promo2SinceDate <- c(as.Date(NA),as.Date(paste0(store$Promo2SinceYear[c(2:1115)],"-01","-01"))+store$Promo2SinceWeek[c(2:1115)]*7) #real date of promo2 starting

  #Add store type
  train$StoreType <- NA
  train$StoreType <- sapply(train$Store,function(x) {store$StoreType[store$Store==x]})
  train$StoreType <- factor(train$StoreType)
  train$Assortment <- NA
  train$Assortment <- sapply(train$Store,function(x) {store$Assortment[store$Store==x]})
  train$Assortment <- factor(train$Assortment)
  train$CompetitionDistance <- NA
  train$CompetitionDistance <- sapply(train$Store,function(x) {store$CompetitionDistance[store$Store==x]})
  train$Promo2 <- 0
  train$CompetitorOpen <- 0
  for (i in 1:nrow(store))
  {
    if(store$Promo2[i]==1) 
    {
      train$Promo2[which(train$Date>=store$Promo2SinceDate[i] & train$Store==store$Store[i])] <- 1
    }
    if(!is.na(store$CompetitionOpenSinceDate[i])) 
    {
      train$CompetitorOpen[which(train$Date>=store$CompetitionOpenSinceDate[i] & train$Store==store$Store[i])] <- 1
    } 
  }
  train$Promo2 <- factor(train$Promo2)
  train$CompetitorOpen <- factor(train$CompetitorOpen)
  train$DayOfWeek <- factor(train$DayOfWeek)
  train$Open <- factor(train$Open)
  train$Promo <- factor(train$Promo)
  train$SchoolHoliday <- factor(train$SchoolHoliday)
  train$AvgSaleCustomer <- train$Sales/train$Customers
  train$Month <- months(train$Date)
  train$Month <- factor(train$Month)
  train$OpenAfterClose <- factor(c(FALSE,diff(as.integer(train$Open))==1))
  train$Year <- year(train$Date)
  train$Year <- factor(train$Year)
  train$Store <- factor(train$Store)
  
  test$StoreType <- NA
  test$StoreType <- sapply(test$Store,function(x) {store$StoreType[store$Store==x]})
  test$StoreType <- factor(test$StoreType)
  test$Assortment <- NA
  test$Assortment <- sapply(test$Store,function(x) {store$Assortment[store$Store==x]})
  test$Assortment <- factor(test$Assortment)
  test$CompetitionDistance <- NA
  test$CompetitionDistance <- sapply(test$Store,function(x) {store$CompetitionDistance[store$Store==x]})
  test$Promo2 <- 0
  test$CompetitorOpen <- 0
  for (i in 1:nrow(store))
  {
    if(store$Promo2[i]==1) 
    {
      test$Promo2[which(test$Date>=store$Promo2SinceDate[i] & test$Store==store$Store[i])] <- 1
    }
    if(!is.na(store$CompetitionOpenSinceDate[i])) 
    {
      test$CompetitorOpen[which(test$Date>=store$CompetitionOpenSinceDate[i] & test$Store==store$Store[i])] <- 1
    } 
  }
  test$Promo2 <- factor(test$Promo2)
  test$CompetitorOpen <- factor(test$CompetitorOpen)
  test$DayOfWeek <- factor(test$DayOfWeek)
  test$Open <- factor(test$Open)
  test$Promo <- factor(test$Promo)
  test$SchoolHoliday <- factor(test$SchoolHoliday)
  test$Month <- months(test$Date)
  test$Month <- factor(test$Month,levels = levels(train$Month))
  test$OpenAfterClose <- factor(c(FALSE,diff(as.integer(test$Open))==1))
  test$Year <- year(test$Date)
  test$Year <- factor(test$Year,levels = levels(train$Year))
  test$Store <- factor(test$Store)
  levels(test$StateHoliday) <- levels(train$StateHoliday)

}

#Exploring store1 data
{
  store1 <- rbind(train[train$Store==1,-c(4,5,15)],test[test$Store==1,-1])
  store1$Sales <- c(train$Sales[train$Store==1],rep(NA,nrow(test[test$Store==1,])))
  store1$Customers <- c(train$Customers[train$Store==1],rep(NA,nrow(test[test$Store==1,])))
  plot(store1$Date,store1$Sales,type="l")
}

#Train a regression
{
  #Some more features manipulation
  train$CompetitionDistance[is.na(train$CompetitionDistance)] <- 12
  test$CompetitionDistance[is.na(test$CompetitionDistance)] <- 12
  test$OpenAfterClose[is.na(test$OpenAfterClose)] <- FALSE
  
  train <- train[which(train$Open=='1'),] # looking at only stores that were open (o.w. sales are 0)
  #test <- test[which(test$Open=='1'),] # looking at only stores that were open (o.w. sales are 0)
  
  train$Sales <- log(train$Sales+1)
  train$Customers <- log(train$Customers+1)
  
  #remove unwanted columns in the training/testing part
  train$Store <- NULL
  train$Date <- NULL
  train$AvgSaleCustomer <- NULL
  train$Open <- NULL
  test$Store <- NULL
  test$Date <- NULL
  #test$Open <- NULL
  
  #Divide to multifolds & other initializations
  {
    K <- 5 #number of folds (80-20)
    Times <- 1 #number of resamples (5)
    folds <- createMultiFolds(train$Sales,k=K,times = Times)
    M <- ncol(train)
    N <- nrow(train)
    RMSPE_oob <- rep(0,K)
    RMSPE_valid <- rep(0,K)
    
  }
  
  #Validation of scores
  cores <- 12
  cl <- makeCluster(cores) 
  registerDoParallel(cores = cores)
  getDoParWorkers() # Just checking, how many workers you have
  
  foreach (i=1:K)  %dopar%
  {
    rf_model <- randomForest(x=train[folds[[i]],-c(2,3)],
                             y=train$Sales[folds[[i]]],
                             xtest=train[-c(folds[[i]]),-c(2,3)],
                             ytest=train$Sales[-c(folds[[i]])],
                             mtry=5,
                             ntree=10,
                             sampsize=10000,
                             do.trace = T)
    
    predicted_valid <- exp(rf_model$test$predicted)-1
    predicted_oob <- exp(rf_model$predicted)-1
    observed_oob <- exp(train$Sales[folds[[i]]])-1
    observed_valid <- exp(train$Sales[-c(folds[[i]])])-1
    
    predicted_valid <- predicted_valid[which(observed_valid!=0)] #sales= 0 ingnored in scoring
    observed_valid <- observed_valid[which(observed_valid!=0)] #sales= 0 ingnored in scoring
    predicted_oob <- predicted_oob[which(observed_oob!=0)] #sales= 0 ingnored in scoring
    observed_oob <- observed_oob[which(observed_oob!=0)] #sales= 0 ingnored in scoring
    
    RMSPE_oob[i] <- sqrt(mean(((observed_oob-predicted_oob)/observed_oob)^2))
    RMSPE_valid[i] <- sqrt(mean(((observed_valid-predicted_valid)/observed_valid)^2))
  }
  
  #Run the final model
  t <- proc.time()
  samples <- sample(1:nrow(train),10000)
  rf_model <- foreach(ntree = rep(1, 12), .combine = combine, .multicombine = TRUE, .packages = 'randomForest') %dopar% {    
    randomForest(x=train[samples,-c(2,3)],y=train$Sales[samples],mtry=5,ntree=ntree)
  }
  stopCluster(cl)
  t2.scheme <- proc.time() - t
  
  t <- proc.time()
  rf_model <- randomForest(x=train[,-c(2,3)],
                           y=train$Sales,
                           mtry=5,
                           ntree=12,
                           sampsize=10000,
                           do.trace = T)
  t1.scheme <- proc.time() - t
  
  predicted_oob <- exp(rf_model$predicted)-1
  observed_oob <- exp(train$Sales)-1
  predicted_oob <- predicted_oob[which(observed_oob!=0)] #sales= 0 ingnored in scoring
  observed_oob <- observed_oob[which(observed_oob!=0)] #sales= 0 ingnored in scoring
  RMSPE_oob <- sqrt(mean(((observed_oob-predicted_oob)/observed_oob)^2))
  
  pred <- exp(predict(rf_model,test[,-c(1,3)]))-1
  pred[which(test$Open=='0')] <- 0
  submission <- data.frame(Id=test$Id, Sales=pred)
  submission <- submission[order(submission$Id),] #order again by ID
  write.csv(submission, "rf1.csv",row.names = F)
  
  #Scoring examples
  #MSE_oob = 0.11 -> RMSPE_oob = 0.3917546 -> LB socre = 0.33795
  #MSE_oob = 0.085 -> RMSPE_oob = 0.3401 -> LB socre = ?
  #MSE_oob = 0.025 -> RMSPE_oob = ? -> LB socre = 0.125
  
#   mtryGrid <- expand.grid(mtry = 5)
#   rf_model<-train(Sales~.,data=train[,-c(3)],method="rf",ntree = 10,sampsize=10000,
#                   prox=T,allowParallel=T,tuneGrid = mtryGrid,trControl=trainControl(method="oob"))
}

#Just testing
gc(reset=T)
cores <- 12
cl <- makeCluster(cores) 
registerDoParallel(cl)
getDoParWorkers() # Just checking, how many workers you have
x <- matrix(runif(500), 100)
y <- gl(2, 50)
rf <- foreach(ntree=rep(100, 12), .combine=combine,.multicombine=F, .packages="randomForest") %dopar% {
  randomForest(x, y, ntree=ntree,do.trace=T,importance=T, keep.forest=T,replace=T) }
stopCluster(cl)

data(iris)
cl <- makeCluster(12) #number of cores
registerDoParallel(cl)
model <- train(Species ~ ., data = iris, method = "rf")
stopCluster(cl)

cl <- makeCluster(12) #number of cores
registerDoParallel(cl)
model <- train(x,y, method = "rf")
stopCluster(cl)

cl <- makeCluster(12) #number of cores
registerDoParallel(cl)
model <- train(train[samples,-c(2,3)],train$Sales[samples], method = "rf")
stopCluster(cl)