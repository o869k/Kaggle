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
  library(xgboost); library(quantmod)
  library(zoo); library(weatherData)
}

#Read Data
{
  train <- read.csv("train.csv")
  test <- read.csv("test.csv")
  store <- read.csv("store.csv")
  data(iris) #just a test data
  
  #Read germany weather data
  #Read germany DAX data
  getSymbols("^GDAXI", from="2013-01-01", to="2015-09-15") 
  DAX <- as.data.frame(GDAXI)
  DAX$Date <- as.Date(row.names(DAX))
}

#Feature Engineering - will take a few minutes to compute
{
  #Store
  {  
    store$CompetitionDistance <- log(store$CompetitionDistance) #make a log transform
    store$CompetitionOpenSinceDate <- as.Date(paste0(store$CompetitionOpenSinceYear,"-",store$CompetitionOpenSinceMonth,"-01")) #real date of competitor open
    store$Promo2SinceDate <- c(as.Date(NA),as.Date(paste0(store$Promo2SinceYear[c(2:1115)],"-01","-01"))+store$Promo2SinceWeek[c(2:1115)]*7) #real date of promo2 starting
    levels(store$PromoInterval) <- c("","February,May,August,November","January,April,July,October","March,June,September,December")
  }

  #Train
  {
    train$Date <- as.Date(train$Date)
    train <- train[order(train$Date),] #sort by date and store number
    train <- train[order(train$Store),]
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
    train$Promo2Time <- 0
    train$CompetitorOpenTime <- 0
    for (i in 1:nrow(store))
    {
      if(store$Promo2[i]==1) 
      {
        train$Promo2[which(train$Date>=store$Promo2SinceDate[i] & train$Store==store$Store[i])] <- 1
        train$Promo2Time[which(train$Date>=store$Promo2SinceDate[i] & train$Store==store$Store[i])] <- train$Date[which(train$Date>=store$Promo2SinceDate[i] & train$Store==store$Store[i])]-store$Promo2SinceDate[i]
      }
      if(!is.na(store$CompetitionOpenSinceDate[i])) 
      {
        train$CompetitorOpen[which(train$Date>=store$CompetitionOpenSinceDate[i] & train$Store==store$Store[i])] <- 1
        train$CompetitorOpenTime[which(train$Date>=store$CompetitionOpenSinceDate[i] & train$Store==store$Store[i])] <- train$Date[which(train$Date>=store$CompetitionOpenSinceDate[i] & train$Store==store$Store[i])] - store$CompetitionOpenSinceDate[i]
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
    train$StateHoliday[(train$StateHoliday=="b") | (train$StateHoliday=="c")] <- "a" #narrow this feature
    train$StateHoliday <- factor(train$StateHoliday)
    train$Promo2Interval <- 0
    for (i in 1:nrow(store))
    {
      if(store$Promo2[i]==1) 
      {
        train$Promo2Interval[which((as.character(train$Month) %in% strsplit(as.character(store$PromoInterval[i]),",")[[1]]) & (train$Promo2==1) & (train$Store==store$Store[i]))] <- 1
      }
    }
    train$Promo2Interval <- factor(train$Promo2Interval)
  }

  #Test
  {
    test$Date <- as.Date(test$Date)
    test <- test[order(test$Date),] #sort by date and store number
    test <- test[order(test$Store),]
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
    test$Promo2Time <- 0
    test$CompetitorOpenTime <- 0
    for (i in 1:nrow(store))
    {
      if(store$Promo2[i]==1) 
      {
        test$Promo2[which(test$Date>=store$Promo2SinceDate[i] & test$Store==store$Store[i])] <- 1
        test$Promo2Time[which(test$Date>=store$Promo2SinceDate[i] & test$Store==store$Store[i])] <- test$Date[which(test$Date>=store$Promo2SinceDate[i] & test$Store==store$Store[i])]-store$Promo2SinceDate[i]
        
      }
      if(!is.na(store$CompetitionOpenSinceDate[i])) 
      {
        test$CompetitorOpen[which(test$Date>=store$CompetitionOpenSinceDate[i] & test$Store==store$Store[i])] <- 1
        test$CompetitorOpenTime[which(test$Date>=store$CompetitionOpenSinceDate[i] & test$Store==store$Store[i])] <- test$Date[which(test$Date>=store$CompetitionOpenSinceDate[i] & test$Store==store$Store[i])] - store$CompetitionOpenSinceDate[i]
        
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
    test$Promo2Interval <- 0
    for (i in 1:nrow(store))
    {
      if(store$Promo2[i]==1) 
      {
        test$Promo2Interval[which((as.character(test$Month) %in% strsplit(as.character(store$PromoInterval[i]),",")[[1]]) & (test$Promo2==1) & (test$Store==store$Store[i]))] <- 1
      }
    }
    test$Promo2Interval <- factor(test$Promo2Interval)
  }
}

#Train a regression
{
  #Some more features manipulation
  {  
    train$CompetitionDistance[is.na(train$CompetitionDistance)] <- 12 #NA is far distanced -> should be imputed really
    test$CompetitionDistance[is.na(test$CompetitionDistance)] <- 12 #NA is far distanced -> should be imputed really
    test$OpenAfterClose[is.na(test$OpenAfterClose)] <- FALSE # a fill-in
    test$Open[is.na(test$Open)] <- 1 # a fill-in


    train <- train[which(train$Open=='1'),] # looking at only stores that were open (o.w. sales are 0)
    #test <- test[which(test$Open=='1'),] # looking at only stores that were open (o.w. sales are 0)
  
    train$Sales <- log(train$Sales+1) #transformation
    train$Customers <- log(train$Customers+1) #transformation
  
    #remove unwanted columns in the training/testing part
    train$Store <- NULL
    train$Date <- NULL
    train$AvgSaleCustomer <- NULL
    train$Open <- NULL
    test$Store <- NULL
    test$Date <- NULL
    #test$Open <- NULL
  
    #Train only aug and september 2013,2014 (just like the testing months)
    train <- train[(train$Month=="August") | (train$Month=="September"),]
    train$Year <- NULL
    test$Year <- NULL
    train$Month <- factor(train$Month)
    test$Month <- factor(test$Month)
    
    #Also remove days with no sales in training as it is outlier
    train <- train[train$Sales!=0,]
  }

  #RF model
  {
    mtryGrid <- expand.grid(mtry = c(7,9,11))
    mytrControl <- trainControl(method="cv",number=3,repeats=1) # a training ctrl, o.w. it is bootstrapping iwth 10 repetitions
    GridFull <- expand.grid(mtry = c(5),ntree = c(10,50))
    
    #Run the models
    gc()
    rm(rf_model)
    t <- proc.time()
    samples <- sample(1:nrow(train),106000)
    cl <- makePSOCKcluster(12) #number of cores
    clusterEvalQ(cl, library(foreach))
    registerDoParallel(cl)
    getDoParWorkers() # Just checking, how many workers you have
    #rf_model <- train(train[samples,-c(2,3)],train$Sales[samples],method = "rf")
    #rf_model <- train(train[samples,-c(2,3)],train$Sales[samples],method = "rf",ntree = 10,trControl=mytrControl) #using 3 different mtry
    rf_model <- train(train[samples,-c(2,3)],train$Sales[samples],method = "rf",ntree = 100,tuneGrid = mtryGrid,trControl=mytrControl) #using constant mtry
    #rf_model <- train(train[samples,-c(2,3)],train$Sales[samples],method="parRF",ntree = 50,tuneGrid = mtryGrid,trControl=mytrControl,keep.forest=T,importance=T) #parallel - not working... 
    stopCluster(cl)
    t.scheme <- proc.time() - t
  
    print(rf_model)
    print(rf_model$finalModel)
    importance(rf_model$finalModel)
    plot(rf_model$finalModel) #MSE
    plot(importance(rf_model$finalModel), lty=2, pch=16)
    
    predicted_oob <- exp(rf_model$finalModel$predicted)-1
    observed_oob <- exp(train$Sales)-1
    observed_oob <- observed_oob[samples] #only the relvenat samples
    predicted_oob <- predicted_oob[which(observed_oob!=0)] #sales= 0 ingnored in scoring
    observed_oob <- observed_oob[which(observed_oob!=0)] #sales= 0 ingnored in scoring
    RMSPE_oob <- sqrt(mean(((observed_oob-predicted_oob)/observed_oob)^2))
    
    pred_rf <- exp(predict(rf_model,test[,-c(1,3)]))-1
    pred_rf[which(test$Open=='0')] <- 0
    submission <- data.frame(Id=test$Id, Sales=pred_rf)
    submission <- submission[order(submission$Id),] #order again by ID
    write.csv(submission, "rf1.csv",row.names = F)
    
    #Scoring examples
    #MSE_oob = 0.11 -> RMSPE_oob = 0.3917546 -> LB socre = 0.33795
    #MSE_oob = 0.085 -> RMSPE_oob = 0.3401 -> LB socre = ?
    #MSE_oob = 0.025 -> RMSPE_oob = ? -> LB socre = 0.125
    #MSE_oob = 0.00196 -> RMSPE_oob = 0.15 -> LB socre = 0.262 (only aug_sep)
  }

  #GBM model
  {
    gbmGrid <-  expand.grid(interaction.depth = c(10,20),
                            n.trees = c(10,20)*100,
                            shrinkage = c(0.1,0.2),
                            n.minobsinnode = c(1,10))
    gbmControl <- trainControl(method="cv",number=3,repeats=1) # a training ctrl, o.w. it is bootstrapping iwth 10 repetitions
    
    gc()
    rm(gbm_model)
    t <- proc.time()
    samples <- sample(1:nrow(train),106000)
    cl <- makePSOCKcluster(12) #number of cores
    clusterEvalQ(cl, library(foreach))
    registerDoParallel(cl)
    getDoParWorkers() # Just checking, how many workers you have
    gbm_model <- train(train[samples,-c(2,3)],train$Sales[samples],method = "gbm",tuneGrid = gbmGrid,trControl=gbmControl) #using full tune grid
    #gbm_model <- train(train[,-c(2,3)],train$Sales,method = "gbm",tuneGrid = gbmGrid,trControl=gbmControl) #using full tune grid
    stopCluster(cl)
    t.scheme <- proc.time() - t
    
    print(gbm_model)
    print(gbm_model$finalModel)
    plot(gbm_model$finalModel) #MSE
  
    predicted_valid <- exp(gbm_model$finalModel$fit)-1
    observed_valid <- exp(train$Sales)-1
    observed_valid <- observed_valid[samples]  #only the relvenat samples
    predicted_valid <- predicted_valid[which(observed_valid!=0)] #sales= 0 ingnored in scoring
    observed_valid <- observed_valid[which(observed_valid!=0)] #sales= 0 ingnored in scoring
    RMSPE_valid <- sqrt(mean(((observed_valid-predicted_valid)/observed_valid)^2))
    
    pred_gbm <- exp(predict(gbm_model,test[,-c(1,3)]))-1
    pred_gbm[which(test$Open=='0')] <- 0
    submission <- data.frame(Id=test$Id, Sales=pred_gbm)
    submission <- submission[order(submission$Id),] #order again by ID
    write.csv(submission, "gbm1.csv",row.names = F)
    
    #Scoring examples
    #MSE_valid = 0.046 -> RMSPE_valid = 0.199 -> LB socre = 0.1953
    #MSE_valid = 0.015 -> RMSPE_valid = 0.112 -> LB socre = 0.224 (only aug_sep)
  }

  #XGBOOST model - this might OF badley
  {  
    gc()
    rm(xgb_model)
    
    h <- sample(nrow(train), nrow(train)*.75)
    valid <- train[-h,]
    train <- train[h,]
  
    dtrain <- xgb.DMatrix(data.matrix(train), label=train$Sales) #Making train and validation matrices
    dvalid <- xgb.DMatrix(data.matrix(valid), label=valid$Sales)
    
    watchlist <- list(eval = dvalid, train = dtrain)
    
    rmspe.xgb <- function(preds, dtrain) {
      target <- getinfo(dtrain, "label")
      predicted <- preds
      x1 <- target - predicted
      x2 <- x1 / target
      x2[target==0] <- 0
      x3 <- x2*x2
      x4 <- sum(x3)
      x5 <- x4/length(target)
      x6 <- sqrt(x5)
      # Yes, this is needlessly complex, but I had debugging issues
      return(list(metric="error", value=x6))
    }
    
    param <- list(  objective           = "reg:linear", 
                    # booster = "gblinear",
                    eta                 = 0.01,
                    max_depth           = 8,  # changed from default of 6
                    # subsample           = 0.6,
                    # colsample_bytree    = 0.6,
                    eval_metric         = rmspe.xgb
                    # min_child_weight    = 50
                    # alpha = 0.0001, 
                    # lambda = 1
    )
    
    xgb_model <- xgb.train(   params              = param, 
                              data                = dtrain, 
                              nrounds             = 500,
                              verbose             = 1, 
                              early.stop.round    = 30,
                              watchlist           = watchlist,
                              maximize            = FALSE,
                              print.every.n       = 10
    )
  
    xgb.importance(feature_names = names(train), model = xgb_model)
    p.train <- predict(xgb_model, data.matrix(train))
    rmspe.xgb(p.train, dtrain)
    
    pred_xgb <- exp(predict(xgb_model, data.matrix(test)))-1
    pred_xgb[which(test$Open=='0')] <- 0
    submission <- data.frame(Id=test$Id, Sales=pred_xgb)
    submission <- submission[order(submission$Id),] #order again by ID
    write.csv(submission, "xgb1.csv",row.names = F)
  

    #Scoring examples
    #MSE_valid = 0.046 -> RMSPE_valid = 0.199 -> LB socre = 0.1953
  }

  #Mixing models
  {
    submission <- data.frame(Id=test$Id, Sales=(pred_rf+pred_gbm)/2)
    submission <- submission[order(submission$Id),] #order again by ID
    write.csv(submission, "mix1.csv",row.names = F)
    
    #Scoring examples
    #MSE_oob = ? -> RMSPE_oob = ? -> LB socre = ?
  }
}

#Exploring store1 data
{
  store1 <- rbind(train[train$Store==1,-c(4,5,15)],test[test$Store==1,-1])
  store1$Sales <- c(train$Sales[train$Store==1],rep(NA,nrow(test[test$Store==1,])))
  store1$Customers <- c(train$Customers[train$Store==1],rep(NA,nrow(test[test$Store==1,])))
  plot(store1$Date,store1$Sales,type="l")
}
