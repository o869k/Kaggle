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
  library(party); library(rpart)
}

#Read Data
{
  train <- read.csv("train.csv")
  test <- read.csv("test.csv")
  store <- read.csv("store.csv")
  data(iris) #just a test data
  
  #Read germany weather data (took berlin)
  w2013 <- getWeatherForDate("EDDT","2013-01-01","2013-12-31",opt_all_columns = TRUE)
  w2014 <- getWeatherForDate("EDDT","2014-01-01","2014-12-31",opt_all_columns = TRUE) 
  w2015 <- getWeatherForDate("EDDT","2015-01-01","2015-09-17",opt_all_columns = TRUE)
  names(w2015)[2] <- names(w2013)[2]
  weather <- rbind(w2013,w2014,w2015)
  rm(w2013,w2014,w2015)
  weather$Date <- as.Date(weather$Date)
  #Read germany DAX data (only for the trading days)
  getSymbols("^GDAXI", from="2013-01-01", to="2015-09-17") 
  DAX <- as.data.frame(GDAXI); rm(GDAXI)
  DAX$Date <- as.Date(row.names(DAX))
}

#Feature Engineering - will take a few minutes to compute
{
  #Store
  {  
    store$CompetitionDistance <- log(store$CompetitionDistance) #make a log transform
    store$CompetitionDistance[is.na(store$CompetitionDistance)] <- 12 #NA is far distanced -> should be imputed really
    store$CompetitionOpenSinceDate <- as.Date(paste0(store$CompetitionOpenSinceYear,"-",store$CompetitionOpenSinceMonth,"-01")) #real date of competitor open
    store$Promo2SinceDate <- c(as.Date(NA),as.Date(paste0(store$Promo2SinceYear[c(2:1115)],"-01","-01"))+store$Promo2SinceWeek[c(2:1115)]*7) #real date of promo2 starting
    levels(store$PromoInterval) <- c("","February,May,August,November","January,April,July,October","March,June,September,December")
  }

  #Train
  {
    train$Date <- as.Date(train$Date)
    train <- train[order(train$Date),] #sort by date and store number
    train <- train[order(train$Store),]
    train <- merge(store[,c(1:4)], train, by = c("Store"),sort = FALSE)
    train$weekend <- ((train$DayOfWeek==6) | (train$DayOfWeek==7))
    train$weekend <- factor(train$weekend)
    train$Month <- months(train$Date)
    train$Month <- factor(train$Month)
    train$Year <- year(train$Date)
    train$Year <- factor(train$Year)
    train$DayOfWeek <- factor(train$DayOfWeek)
    train$Promo2 <- 0
    train$CompetitorOpen <- 0
    train$Promo2Time <- 0
    train$CompetitorOpenTime <- 0 # should be imputed really (this is instead of NA)
    train$Promo2Interval <- 0
    for (i in 1:nrow(store))
    {
      cat(i,"\n")
      if(store$Promo2[i]==1) 
      {
        train$Promo2[which(train$Date>=store$Promo2SinceDate[i] & train$Store==store$Store[i])] <- 1
        train$Promo2Time[which(train$Date>=store$Promo2SinceDate[i] & train$Store==store$Store[i])] <- train$Date[which(train$Date>=store$Promo2SinceDate[i] & train$Store==store$Store[i])]-store$Promo2SinceDate[i]
        train$Promo2Interval[which((as.character(train$Month) %in% strsplit(as.character(store$PromoInterval[i]),",")[[1]]) & (train$Promo2==1) & (train$Store==store$Store[i]))] <- 1
      }
      if(!is.na(store$CompetitionOpenSinceDate[i])) 
      {
        train$CompetitorOpen[which(train$Date>=store$CompetitionOpenSinceDate[i] & train$Store==store$Store[i])] <- 1
        train$CompetitorOpenTime[which(train$Date>=store$CompetitionOpenSinceDate[i] & train$Store==store$Store[i])] <- train$Date[which(train$Date>=store$CompetitionOpenSinceDate[i] & train$Store==store$Store[i])] - store$CompetitionOpenSinceDate[i]
      } 
    }
    train$Promo2 <- factor(train$Promo2)
    train$Promo2Interval <- factor(train$Promo2Interval)
    train$CompetitorOpen <- factor(train$CompetitorOpen)
    train$Open <- factor(train$Open)
    train$Promo <- factor(train$Promo)
    train$SchoolHoliday <- factor(train$SchoolHoliday)
    train$StateHoliday[(train$StateHoliday=="b") | (train$StateHoliday=="c")] <- "a" #narrow this feature
    train$StateHoliday <- factor(train$StateHoliday)
    train$OpenAfterClose <- factor(c(FALSE,diff(as.integer(train$Open))==1))
    train$OpenBeforeClose <- factor(c(diff(as.integer(train$Open))==-1,FALSE))
    train$PromoFirstDay <- factor(c(FALSE,diff(as.integer(train$Promo))==1))
    train$PromoLastDay <- factor(c(diff(as.integer(train$Promo))==-1,FALSE))
    train$SchoolHolidayStart <- factor(c(FALSE,diff(as.integer(train$SchoolHoliday))==1))
    train$SchoolHolidayEnd <- factor(c(diff(as.integer(train$SchoolHoliday))==-1,FALSE))
    train$StateHolidayStart <- factor(c(FALSE,diff(as.integer(train$StateHoliday))==1))
    train$StatelHolidayEnd <- factor(c(diff(as.integer(train$StateHoliday))==-1,FALSE))
    train$Store <- factor(train$Store)
    train <- merge(weather[,c(1,4,10,19,21,23)], train, by = c("Date"),sort = FALSE)
    train$Events[train$Events %in% c("Fog-Rain-Hail-Thunderstorm","Fog-Rain-Snow","Fog-Snow","Rain-Hail","Rain-Hail-Thunderstorm","Rain-Snow","Rain-Snow-Hail","Snow")] <- "Snow"
    train$Events <- factor(train$Events)
    train <- merge(DAX[,c(4,5,7)], train, by = c("Date"),sort = FALSE,all.y = TRUE)
    train <- train[order(train$Date),] #sort by date and store number
    train <- train[order(train$Store),]
    train$GDAXI.Volume[train$GDAXI.Volume==0] <- NA
    train$GDAXI.Volume <- c(20000000,na.locf(train$GDAXI.Volume)) #fill in NA's
    train$GDAXI.Close <- c(7700,na.locf(train$GDAXI.Close)) #must be sorted by store and date
    train$Sales <- log(train$Sales+1) #transformation
    train$Customers <- log(train$Customers+1) #transformation
    
  }

  #Test
  {
    test$Open[is.na(test$Open)] <- 1 # a fill-in (certain)
    test$Date <- as.Date(test$Date)
    test <- test[order(test$Date),] #sort by date and store number
    test <- test[order(test$Store),]
    test <- merge(store[,c(1:4)], test, by = c("Store"),sort = FALSE)
    test$weekend <- ((test$DayOfWeek==6) | (test$DayOfWeek==7))
    test$weekend <- factor(test$weekend)
    test$Month <- months(test$Date)
    test$Month <- factor(test$Month,levels = levels(train$Month))
    test$Year <- year(test$Date)
    test$Year <- factor(test$Year,levels = levels(train$Year))
    test$DayOfWeek <- factor(test$DayOfWeek)
    test$Promo2 <- 0
    test$CompetitorOpen <- 0
    test$Promo2Time <- 0
    test$CompetitorOpenTime <- 0
    test$Promo2Interval <- 0
    for (i in 1:nrow(store))
    {
      cat(i,"\n")
      if(store$Promo2[i]==1) 
      {
        test$Promo2[which(test$Date>=store$Promo2SinceDate[i] & test$Store==store$Store[i])] <- 1
        test$Promo2Time[which(test$Date>=store$Promo2SinceDate[i] & test$Store==store$Store[i])] <- test$Date[which(test$Date>=store$Promo2SinceDate[i] & test$Store==store$Store[i])]-store$Promo2SinceDate[i]
        test$Promo2Interval[which((as.character(test$Month) %in% strsplit(as.character(store$PromoInterval[i]),",")[[1]]) & (test$Promo2==1) & (test$Store==store$Store[i]))] <- 1
      }
      if(!is.na(store$CompetitionOpenSinceDate[i])) 
      {
        test$CompetitorOpen[which(test$Date>=store$CompetitionOpenSinceDate[i] & test$Store==store$Store[i])] <- 1
        test$CompetitorOpenTime[which(test$Date>=store$CompetitionOpenSinceDate[i] & test$Store==store$Store[i])] <- test$Date[which(test$Date>=store$CompetitionOpenSinceDate[i] & test$Store==store$Store[i])] - store$CompetitionOpenSinceDate[i]
        
      } 
    }
    test$Promo2 <- factor(test$Promo2)
    test$Promo2Interval <- factor(test$Promo2Interval)
    test$CompetitorOpen <- factor(test$CompetitorOpen)
    test$Open <- factor(test$Open)
    test$Promo <- factor(test$Promo)
    test$SchoolHoliday <- factor(test$SchoolHoliday)
    test$OpenAfterClose <- factor(c(FALSE,diff(as.integer(test$Open))==1))
    test$OpenBeforeClose <- factor(c(diff(as.integer(test$Open))==-1,FALSE))
    test$PromoFirstDay <- factor(c(FALSE,diff(as.integer(test$Promo))==1))
    test$PromoLastDay <- factor(c(diff(as.integer(test$Promo))==-1,FALSE))
    test$SchoolHolidayStart <- factor(c(FALSE,diff(as.integer(test$SchoolHoliday))==1))
    test$SchoolHolidayEnd <- factor(c(diff(as.integer(test$SchoolHoliday))==-1,FALSE))
    test$StateHolidayStart <- factor(c(FALSE,diff(as.integer(test$StateHoliday))==1))
    test$StatelHolidayEnd <- factor(c(diff(as.integer(test$StateHoliday))==-1,FALSE))
    test$Store <- factor(test$Store)
    test <- merge(weather[,c(1,4,10,19,21,23)], test, by = c("Date"),sort = FALSE)
    test$Events <- factor(test$Events,levels = levels(train$Events))
    test <- merge(DAX[,c(4,5,7)], test, by = c("Date"),sort = FALSE,all.y = TRUE)
    test <- test[order(test$Date),] #sort by date and store number
    test <- test[order(test$Store),]
    test$GDAXI.Volume[test$GDAXI.Volume==0] <- NA
    test$GDAXI.Volume <- c(70000000,70000000,na.locf(test$GDAXI.Volume)) #fill in NA's
    test$GDAXI.Close <- c(11500,11500,na.locf(test$GDAXI.Close)) #must be sorted by store and date
    
  }
}

#Train a regression MODELS
{
  #Some more features manipulation
  {  
    train <- train[which(train$Open=='1'),] # looking at only stores that were open (o.w. sales are 0)
    #test <- test[which(test$Open=='1'),] # looking at only stores that were open (o.w. sales are 0)
  
    #remove unwanted columns in the training/testing part
    #train$Store <- NULL
    train$Date <- NULL
    #train$AvgSaleCustomer <- NULL
    train$Open <- NULL
    #test$Store <- NULL
    test$Date <- NULL
    #test$Open <- NULL
  
    #Also remove days with no sales in training as it is outlier
    train <- train[train$Sales!=0,]
    
    #Remove columns with little information (?)
    train$GDAXI.Volume <- NULL
    train$Mean_Humidity <- NULL
    train$Mean_Wind_SpeedKm_h <- NULL
    train$Customers <- NULL #why do we need it?
    train$StateHoliday <- NULL
    train$StateHolidayStart <- NULL
    train$StatelHolidayEnd <- NULL
    
    test$GDAXI.Volume <- NULL
    test$Mean_Humidity <- NULL
    test$Mean_Wind_SpeedKm_h <- NULL
    test$StateHoliday <- NULL
    test$StateHolidayStart <- NULL
    test$StatelHolidayEnd <- NULL
    
    orig_train <- train
  }

  #RF model
  {
    mtryGrid <- expand.grid(mtry = c(5,10,15,20))
    mytrControl <- trainControl(method="oob",number=1,repeats=1) # a training ctrl, o.w. it is bootstrapping iwth 10 repetitions
    GridFull <- expand.grid(mtry = c(5),ntree = c(10,50))
    h <- which(orig_train$Year==2015 & (orig_train$Month=="July"|orig_train$Month=="June")) #take 2 last months as validation
    valid <- orig_train[h,]
    train <- orig_train[-h,]
    
    gc()
    rm(rf_model)
    t <- proc.time()
    #samples <- sample(1:nrow(train),10600)
    cl <- makePSOCKcluster(12) #number of cores
    clusterEvalQ(cl, library(foreach))
    registerDoParallel(cl)
    getDoParWorkers() # Just checking, how many workers you have
    #rf_model <- train(train[samples,-c(8,13,14)],train$Sales[samples],method = "rf")
    rf_model <- train(train[,-c(5,10)],train$Sales,method = "rf",ntree = 100,trControl=mytrControl) #using 3 different mtry
    #rf_model <- train(train[samples,-c(8,13,14)],train$Sales[samples],method = "rf",ntree = 50,tuneGrid = mtryGrid,trControl=mytrControl) #using constant mtry
    stopCluster(cl)
    t.scheme <- proc.time() - t
  
    
    rf_model2 <- rpart(Sales~.,data=train[,-c(5)],method="anova")
    rf_model3 <- cforest(Sales~.,data = train[,-c(5)], controls=cforest_unbiased(ntree=10))
    rf_model4 <- randomForest(train[,-c(5,10)],train$Sales,ntree = 1,do.trace=T,proximity=T)
    
    Prediction <- predict(fit, test, OOB=TRUE, type = "response")
    
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
    
    pred_rf <- exp(predict(rf_model,test[,-c(5,9,11)]))-1
    pred_rf[which(test$Open=='0')] <- 0
    submission <- data.frame(Id=test$Id, Sales=pred_rf)
    submission <- submission[order(submission$Id),] #order again by ID
    write.csv(submission, "rf2.csv",row.names = F)
    
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
    samples <- sample(1:nrow(train),10600)
    cl <- makePSOCKcluster(12) #number of cores
    clusterEvalQ(cl, library(foreach))
    registerDoParallel(cl)
    getDoParWorkers() # Just checking, how many workers you have
    gbm_model <- train(train[samples,-c(8,13,14)],train$Sales[samples],method = "gbm",tuneGrid = gbmGrid,trControl=gbmControl) #using full tune grid
    #gbm_model <- train(train[,-c(8,13,14)],train$Sales,method = "gbm",tuneGrid = gbmGrid,trControl=gbmControl) #using full tune grid
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
    
    pred_gbm <- exp(predict(gbm_model,test[,-c(12,14)]))-1
    pred_gbm[which(test$Open=='0')] <- 0
    submission <- data.frame(Id=test$Id, Sales=pred_gbm)
    submission <- submission[order(submission$Id),] #order again by ID
    write.csv(submission, "gbm1.csv",row.names = F)
    
    #Scoring examples
    #MSE_valid = 0.046 -> RMSPE_valid = 0.199 -> LB socre = 0.1953
    #MSE_valid = 0.015 -> RMSPE_valid = 0.112 -> LB socre = 0.224 (only aug_sep)
  }

  #XGBOOST model - this might O.F. 
  {  
    gc()
    rm(xgb_model)
    t <- proc.time()
    cl <- makePSOCKcluster(12) #number of cores
    clusterEvalQ(cl, library(foreach))
    registerDoParallel(cl)
    
    h <- which(orig_train$Year==2015 & (orig_train$Month=="July"|orig_train$Month=="June")) #take 2 month as validation
    valid <- orig_train[h,]
    train <- orig_train[-h,]
  
    dtrain <- xgb.DMatrix(data.matrix(train[,-c(13,14)]), label=train$Sales) #Making train and validation matrices
    dvalid <- xgb.DMatrix(data.matrix(valid[,-c(13,14)]), label=valid$Sales)
    
    watchlist <- list(eval = dvalid, train = dtrain)
    
    rmspe.xgb <- function(preds, dtrain) {
      target <- exp(getinfo(dtrain, "label"))-1
      predicted <- exp(preds)-1
      predicted <- predicted[which(target!=0)] #sales= 0 ingnored in scoring
      target <- target[which(target!=0)] #sales= 0 ingnored in scoring
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
                    max_depth           = 12,  # changed from default of 6
                    # subsample           = 0.6,
                    # colsample_bytree    = 0.6,
                    eval_metric         = rmspe.xgb
                    # min_child_weight    = 50
                    # alpha = 0.0001, 
                    # lambda = 1
    )
    
    xgb_model <- xgb.train(   params              = param, 
                              data                = dtrain, 
                              nrounds             = 2000,
                              verbose             = 1, 
                              early.stop.round    = 30,
                              watchlist           = watchlist,
                              maximize            = FALSE,
                              print.every.n       = 10
    )
  
    stopCluster(cl)
    t.scheme <- proc.time() - t
    
    #xgb.importance(feature_names = names(train[,-c(13,14)]), model = xgb_model)
    p.train <- predict(xgb_model, data.matrix(train[,-c(13,14)]))
    rmspe.xgb(p.train, dtrain)
    
    pred_xgb <- exp(predict(xgb_model, data.matrix(test[,-c(12,14)])))-1
    pred_xgb[which(test$Open=='0')] <- 0
    submission <- data.frame(Id=test$Id, Sales=pred_xgb)
    submission <- submission[order(submission$Id),] #order again by ID
    write.csv(submission, "xgb1.csv",row.names = F)
  
    #Scoring examples
    #MSE_valid = ? -> RMSPE_valid = ? -> LB socre = ?
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

#Train a regression per store
{
  train$Store <- as.numeric(as.character(train$Store))
  test$Store <- as.numeric(as.character(test$Store))

  S <- length(unique(test$Store))
  RMSPE_oob <- rep(NA,S)  
  RMSE_oob <- rep(NA,S)
  RMSPE_valid <- rep(NA,S)
  RMSE_valid <- rep(NA,S)
  submission <- data.frame(Id=NA, Sales=NA)
  
  for (i in 1:S)
  {
    cat(unique(test$Store)[i],"\n")
    store_num <- unique(test$Store)[i] #choose store num [1..1115] 
    store <- train[train$Store==store_num,]
    #plot(store$Date,store$Sales,type="l",xlab="Date",ylab="Log(Sales)",main=paste("Store ",store_num," sales vs. time"))
    #plot(store$Date,store$Customers,type="l",xlab="Date",ylab="Log(Customers)",main=paste("Store ",store_num," customers vs. time"))
    
    h <- which(store$Year==2015 & (store$Month=="July"|store$Month=="June")) #take 2 last months as validation
    valid_store <- store[h,] #validation
    train_store <- store[-h,] #training
    test_store <- test[test$Store==store_num,] #validation
    
    #Model selection according to mtry
    gc()
    rm(rf_model)
    t <- proc.time()
    mtryGrid <- expand.grid(mtry = c(5,10,15,20))
    mytrControl <- trainControl(method="oob",number=1,repeats=1) # a training ctrl, o.w. it is bootstrapping iwth 10 repetitions
    cl <- makePSOCKcluster(12) #number of cores
    clusterEvalQ(cl, library(foreach))
    registerDoParallel(cl)
    getDoParWorkers() # Just checking, how many workers you have
    rf_model <- train(train_store[,-c(1,9,14,15)],
                      train_store$Sales,
                      method = "rf",
                      ntree = 100,
                      tuneGrid = mtryGrid,
                      trControl = mytrControl,
                      xtest=valid_store[,-c(1,9,14,15)],
                      ytest=valid_store$Sales)
    stopCluster(cl)
    t.scheme <- proc.time() - t
    
    predicted_oob <- exp(rf_model$finalModel$predicted)-1
    observed_oob <- exp(train_store$Sales)-1
    predicted_oob <- predicted_oob[which(observed_oob!=0)] #sales= 0 ingnored in scoring
    observed_oob <- observed_oob[which(observed_oob!=0)] #sales= 0 ingnored in scoring
    RMSPE_oob[i] <- sqrt(mean(((observed_oob-predicted_oob)/observed_oob)^2))
    RMSE_oob[i] <- sqrt(rf_model$finalModel$mse[100])
    
    predicted_valid <- exp(rf_model$finalModel$test$predicted)-1
    observed_valid <- exp(valid_store$Sales)-1
    predicted_valid <- predicted_valid[which(observed_valid!=0)] #sales= 0 ingnored in scoring
    observed_valid <- observed_valid[which(observed_valid!=0)] #sales= 0 ingnored in scoring
    RMSPE_valid[i] <- sqrt(mean(((observed_valid-predicted_valid)/observed_valid)^2))
    RMSE_valid[i] <- sqrt(rf_model$finalModel$test$mse[100])
    
    #Best model after validation
    rf_model <- randomForest(x=rbind(train_store[,-c(1,9,14,15)],valid_store[,-c(1,9,14,15)]),
                             y=c(train_store$Sales,valid_store$Sales),
                             ntree = 100,
                             do.trace=T,
                             proximity=T,
                             mtry=rf_model$finalModel$mtry,
                             keep.forest=T)
    
    pred_rf <- exp(predict(rf_model,test_store[,-c(1,9,13)]))-1
    pred_rf[which(test_store$Open=='0')] <- 0
    submission_new <- data.frame(Id=test_store$Id, Sales=pred_rf)
    submission <- rbind(submission,submission_new)

  }
  
  submission <- submission[order(submission$Id),] #order again by ID
  write.csv(submission, "rf3.csv",row.names = F)
  
}

#Train a forecasting model
{
  train$Store <- as.numeric(as.character(train$Store))
  test$Store <- as.numeric(as.character(test$Store))
    
  S <- length(unique(test$Store))
  RMSPE_train <- rep(NA,S)  
  RMSPE_valid <- rep(NA,S)
  submission <- data.frame(Id=NA, Sales=NA)
  
#   cl <- makePSOCKcluster(12) #number of cores
#   clusterEvalQ(cl, library(foreach))
#   registerDoParallel(cl)
#   getDoParWorkers() # Just checking, how many workers you have
  for (i in 1:S)
  {
    cat(unique(test$Store)[i],"\n")
    store_num <- unique(test$Store)[i] #choose store num [1..1115] 
    store <- train[train$Store==store_num,]
    h <- which(store$Year==2015 & (store$Month=="July"|store$Month=="June")) #take 2 last months as validation
    valid_store <- store[h,] #validation
    train_store <- store[-h,] #training
    test_store <- test[test$Store==store_num,] #validation
    
    #Use ARIMAX
    # Create matrix of numeric predictors
    xreg <- cbind(GDAXI.Close=train_store$GDAXI.Close,
                  Mean_TemperatureC=train_store$Mean_TemperatureC,
                  Precipitationmm=train_store$Precipitationmm,
                  Events=model.matrix(~as.factor(train_store$Events)),
                  StoreType=model.matrix(~as.factor(train_store$StoreType)),
                  Assortment=model.matrix(~as.factor(train_store$Assortment)),
                  CompetitionDistance=train_store$CompetitionDistance,
                  Open=model.matrix(~as.factor(train_store$Open)),
                  Promo=model.matrix(~as.factor(train_store$Promo)),
                  StateHoliday=model.matrix(~as.factor(train_store$StateHoliday)),
                  SchoolHoliday=model.matrix(~as.factor(train_store$SchoolHoliday)),
                  weekend=model.matrix(~as.factor(train_store$weekend)),
                  Year=model.matrix(~as.factor(train_store$Year)),
                  Promo2=model.matrix(~as.factor(train_store$Promo2)),
                  Promo2Time=train_store$Promo2Time,
                  CompetitorOpen=model.matrix(~as.factor(train_store$CompetitorOpen)),
                  Promo2Interval=model.matrix(~as.factor(train_store$Promo2Interval)),
                  OpenAfterClose=model.matrix(~as.factor(train_store$OpenAfterClose)),
                  OpenBeforeClose=model.matrix(~as.factor(train_store$OpenBeforeClose)),
                  PromoFirstDay=model.matrix(~as.factor(train_store$PromoFirstDay)),
                  PromoLastDay=model.matrix(~as.factor(train_store$PromoLastDay)),
                  SchoolHolidayStart=model.matrix(~as.factor(train_store$SchoolHolidayStart)),
                  SchoolHolidayEnd=model.matrix(~as.factor(train_store$SchoolHolidayEnd)),
                  StateHolidayStart=model.matrix(~as.factor(train_store$StateHolidayStart)),
                  StatelHolidayEnd=model.matrix(~as.factor(train_store$StatelHolidayEnd)))
  
    xregf <- cbind(GDAXI.Close=valid_store$GDAXI.Close,
                  Mean_TemperatureC=valid_store$Mean_TemperatureC,
                  Precipitationmm=valid_store$Precipitationmm,
                  Events=model.matrix(~as.factor(valid_store$Events)),
                  StoreType=model.matrix(~as.factor(valid_store$StoreType)),
                  Assortment=model.matrix(~as.factor(valid_store$Assortment)),
                  CompetitionDistance=valid_store$CompetitionDistance,
                  Open=model.matrix(~as.factor(valid_store$Open)),
                  Promo=model.matrix(~as.factor(valid_store$Promo)),
                  StateHoliday=model.matrix(~as.factor(valid_store$StateHoliday)),
                  SchoolHoliday=model.matrix(~as.factor(valid_store$SchoolHoliday)),
                  weekend=model.matrix(~as.factor(valid_store$weekend)),
                  Year=model.matrix(~as.factor(valid_store$Year)),
                  Promo2=model.matrix(~as.factor(valid_store$Promo2)),
                  Promo2Time=valid_store$Promo2Time,
                  CompetitorOpen=model.matrix(~as.factor(valid_store$CompetitorOpen)),
                  Promo2Interval=model.matrix(~as.factor(valid_store$Promo2Interval)),
                  OpenAfterClose=model.matrix(~as.factor(valid_store$OpenAfterClose)),
                  OpenBeforeClose=model.matrix(~as.factor(valid_store$OpenBeforeClose)),
                  PromoFirstDay=model.matrix(~as.factor(valid_store$PromoFirstDay)),
                  PromoLastDay=model.matrix(~as.factor(valid_store$PromoLastDay)),
                  SchoolHolidayStart=model.matrix(~as.factor(valid_store$SchoolHolidayStart)),
                  SchoolHolidayEnd=model.matrix(~as.factor(valid_store$SchoolHolidayEnd)),
                  StateHolidayStart=model.matrix(~as.factor(valid_store$StateHolidayStart)),
                  StatelHolidayEnd=model.matrix(~as.factor(valid_store$StatelHolidayEnd)))
    
    # Remove intercepts
    remove_intercept <- which(colnames(xreg)=="(Intercept)")
    xreg <- xreg[,-c(remove_intercept)]
    xregf <- xregf[,-c(remove_intercept)]
    
    #remove unwanted columns
    colVars <- apply(xreg, 2, var) 
    remove_variance <- which(colVars==0)
    xreg <- xreg[,-c(remove_variance)]
    xregf <- xregf[,-c(remove_variance)]
    
#     #Reduce correlation
#     xreg_cor = cor(xreg)
#     remove_correlation = findCorrelation(xreg_cor, cutoff=0.5) # putt any value as a "cutoff" 
#     xreg = xreg[,-c(remove_correlation)]
#     xregf <- xregf[,-c(remove_correlation)]
#     
    # ARIMAX model
    z <- fourier(ts(exp(train_store$Sales)-1, frequency=365.25), K=5)
    y <- ts(exp(train_store$Sales)-1, frequency=7) 
    zf <- fourier(ts(exp(valid_store$Sales)-1, frequency=365.25), K=5)
    rm(model)
    model <- auto.arima(y, xreg=cbind(z,xreg))
    #summary(model)
    
    #Plot fit
    plot(y[(length(y)-200):length(y)],type="l",main=paste("Training Store: ",store_num),xlab="Time",ylab="Sales")
    lines(fitted(model)[(length(y)-200):length(y)],col=2)
    
    #Make forecast
    fc <- forecast(model, xreg=cbind(zf,xregf))
    plot(exp(valid_store$Sales)-1,type="l",main=paste("Validation Store: ",store_num),xlab="Time",ylab="Sales")
    lines(as.numeric(fc$mean),col=2)
    
    #Calcualte RPMSE
    predicted_valid <- fc$mean
    observed_valid <- exp(valid_store$Sales)-1
    predicted_valid <- predicted_valid[which(observed_valid!=0)] #sales= 0 ingnored in scoring
    observed_valid <- observed_valid[which(observed_valid!=0)] #sales= 0 ingnored in scoring
    RMSPE_valid[i] <- sqrt(mean(((observed_valid-predicted_valid)/observed_valid)^2))
    RMSPE_valid[i]
    
    predicted_train <- fitted(model)
    observed_train <- exp(train_store$Sales)-1
    predicted_train <- predicted_train[which(observed_train!=0)] #sales= 0 ingnored in scoring
    observed_train <- observed_train[which(observed_train!=0)] #sales= 0 ingnored in scoring
    RMSPE_train[i] <- sqrt(mean(((observed_train-predicted_train)/observed_train)^2))
    RMSPE_train[i]
    
#     pred_rf <- exp(predict(rf_model,test_store[,-c(1,9,13)]))-1
#     pred_rf[which(test_store$Open=='0')] <- 0
#     submission_new <- data.frame(Id=test_store$Id, Sales=pred_rf)
#     submission <- rbind(submission,submission_new)
    
  }
  # stopCluster(cl)
  
  mean(RMSPE_train)
  mean(RMSPE_valid)
  
  # submission <- submission[order(submission$Id),] #order again by ID
  # write.csv(submission, "rf3.csv",row.names = F)
  
}

#Predcit random forest for each day
{
  h <- which(train$Year==2015 & (train$Month=="July"|train$Month=="June")) #take 2 last months as validation
  valid_set <- train[h,] #validation
  train_set <- train[-h,] #training

  for (i in 1:length(unique(train_set$Date)))
  {
    cat(unique(valid_set$Date)[i],"\n")
    date <- unique(train_set$Date)[i] #choose store num [1..1115] 

    train_date <- train_set[train_set$Date==date,] #training
    train_date$GDAXI.Volume <- NULL
    train_date$GDAXI.Close <- NULL
    train_date$Mean_TemperatureC <- NULL
    train_date$Mean_Humidity <- NULL
    train_date$Mean_Wind_SpeedKm_h <- NULL
    train_date$Precipitationmm <- NULL
    train_date$DayOfWeek <- NULL
    train_date$Year <- NULL
    train_date$Month <- NULL
    train_date$weekend <- NULL
    train_date$SchoolHoliday <- NULL
    train_date$StateHoliday <- NULL
    train_date$SchoolHolidayStart <- NULL
    train_date$SchoolHolidayEnd <- NULL
    train_date$StateHolidayStart <- NULL
    train_date$StatelHolidayEnd <- NULL
    train_date$Date <- NULL
    train_date$Events <- NULL
    train_date$Customers <- NULL
    
    mtryGrid <- expand.grid(mtry = c(5,10))
    mytrControl <- trainControl(method="CV",number=5,repeats=1) # a training ctrl, o.w. it is bootstrapping iwth 10 repetitions
    
    rf_model <- train(train_date[,-c(1,5)],
                      train_date$Sales,
                      method = "rf",
                      ntree = 50,
                      tuneGrid = mtryGrid,
                      trControl = mytrControl)
  }

}
