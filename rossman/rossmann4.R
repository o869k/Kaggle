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
  library(FSelector); library(ggplot2) 
}

#Read Data
{
  train <- read.csv("train.csv")
  test <- read.csv("test.csv")
  store <- read.csv("store.csv")

  #Read germany weather data (took berlin)
  germany_states <- read.csv("store_states.csv")
  
  #Berlin
  w2013 <- getWeatherForDate("EDDT","2013-01-01","2013-12-31",opt_all_columns = TRUE)
  w2014 <- getWeatherForDate("EDDT","2014-01-01","2014-12-31",opt_all_columns = TRUE) 
  w2015 <- getWeatherForDate("EDDT","2015-01-01","2015-09-17",opt_all_columns = TRUE)
  names(w2015)[2] <- names(w2013)[2]
  weather_new <- rbind(w2013,w2014,w2015)
  rm(w2013,w2014,w2015)
  weather_new$Date <- as.Date(weather_new$Date)
  weather_new$State <- "BE"
  weather <- weather_new
  
  #Bayern
  w2013 <- getWeatherForDate("EDDM","2013-01-01","2013-12-31",opt_all_columns = TRUE)
  w2014 <- getWeatherForDate("EDDM","2014-01-01","2014-12-31",opt_all_columns = TRUE) 
  w2015 <- getWeatherForDate("EDDM","2015-01-01","2015-09-17",opt_all_columns = TRUE)
  names(w2015)[2] <- names(w2013)[2]
  weather_new <- rbind(w2013,w2014,w2015)
  rm(w2013,w2014,w2015)
  weather_new$Date <- as.Date(weather_new$Date)
  weather_new$State <- "BY"
  weather <- rbind(weather,weather_new)
 
  #Nordrhein-Westfalen
  w2013 <- getWeatherForDate("EDDL","2013-01-01","2013-12-31",opt_all_columns = TRUE)
  w2014 <- getWeatherForDate("EDDL","2014-01-01","2014-12-31",opt_all_columns = TRUE) 
  w2015 <- getWeatherForDate("EDDL","2015-01-01","2015-09-17",opt_all_columns = TRUE)
  names(w2015)[2] <- names(w2013)[2]
  weather_new <- rbind(w2013,w2014,w2015)
  rm(w2013,w2014,w2015)
  weather_new$Date <- as.Date(weather_new$Date)
  weather_new$State <- "NW"
  weather <- rbind(weather,weather_new)
  
  #Baden Wurttemberg
  w2013 <- getWeatherForDate("LFSB","2013-01-01","2013-12-31",opt_all_columns = TRUE)
  w2014 <- getWeatherForDate("LFSB","2014-01-01","2014-12-31",opt_all_columns = TRUE) 
  w2015 <- getWeatherForDate("LFSB","2015-01-01","2015-09-17",opt_all_columns = TRUE)
  names(w2015)[2] <- names(w2013)[2]
  weather_new <- rbind(w2013,w2014,w2015)
  rm(w2013,w2014,w2015)
  weather_new$Date <- as.Date(weather_new$Date)
  weather_new$State <- "BW"
  weather <- rbind(weather,weather_new)
  
  #Hessen
  w2013 <- getWeatherForDate("ETOU","2013-01-01","2013-12-31",opt_all_columns = TRUE)
  w2014 <- getWeatherForDate("ETOU","2014-01-01","2014-12-31",opt_all_columns = TRUE) 
  w2015 <- getWeatherForDate("ETOU","2015-01-01","2015-09-17",opt_all_columns = TRUE)
  names(w2015)[2] <- names(w2013)[2]
  weather_new <- rbind(w2013,w2014,w2015)
  rm(w2013,w2014,w2015)
  weather_new$Date <- as.Date(weather_new$Date)
  weather_new$State <- "HE"
  weather <- rbind(weather,weather_new)
  
  #Schleswig-Holstein 
  w2013 <- getWeatherForDate("EDDH","2013-01-01","2013-12-31",opt_all_columns = TRUE)
  w2014 <- getWeatherForDate("EDDH","2014-01-01","2014-12-31",opt_all_columns = TRUE) 
  w2015 <- getWeatherForDate("EDDH","2015-01-01","2015-09-17",opt_all_columns = TRUE)
  names(w2015)[2] <- names(w2013)[2]
  weather_new <- rbind(w2013,w2014,w2015)
  rm(w2013,w2014,w2015)
  weather_new$Date <- as.Date(weather_new$Date)
  weather_new$State <- "SH"
  weather <- rbind(weather,weather_new)
  
  #Rheinland Pfalz
  w2013 <- getWeatherForDate("EDDM","2013-01-01","2013-12-31",opt_all_columns = TRUE)
  w2014 <- getWeatherForDate("EDDM","2014-01-01","2014-12-31",opt_all_columns = TRUE) 
  w2015 <- getWeatherForDate("EDDM","2015-01-01","2015-09-17",opt_all_columns = TRUE)
  names(w2015)[2] <- names(w2013)[2]
  weather_new <- rbind(w2013,w2014,w2015)
  rm(w2013,w2014,w2015)
  weather_new$Date <- as.Date(weather_new$Date)
  weather_new$State <- "RP"
  weather <- rbind(weather,weather_new)
  
  #Hamburg
  w2013 <- getWeatherForDate("EDDH","2013-01-01","2013-12-31",opt_all_columns = TRUE)
  w2014 <- getWeatherForDate("EDDH","2014-01-01","2014-12-31",opt_all_columns = TRUE) 
  w2015 <- getWeatherForDate("EDDH","2015-01-01","2015-09-17",opt_all_columns = TRUE)
  names(w2015)[2] <- names(w2013)[2]
  weather_new <- rbind(w2013,w2014,w2015)
  rm(w2013,w2014,w2015)
  weather_new$Date <- as.Date(weather_new$Date)
  weather_new$State <- "HH"
  weather <- rbind(weather,weather_new)
  
  #Thuringia
  w2013 <- getWeatherForDate("EDDE","2013-01-01","2013-12-31",opt_all_columns = TRUE)
  w2014 <- getWeatherForDate("EDDE","2014-01-01","2014-12-31",opt_all_columns = TRUE) 
  w2015 <- getWeatherForDate("EDDE","2015-01-01","2015-09-17",opt_all_columns = TRUE)
  names(w2015)[2] <- names(w2013)[2]
  weather_new <- rbind(w2013,w2014,w2015)
  rm(w2013,w2014,w2015)
  weather_new$Date <- as.Date(weather_new$Date)
  weather_new$State <- "TH"
  weather <- rbind(weather,weather_new)
  
  #Saxony-Anhalt 
  w2013 <- getWeatherForDate("EDDT","2013-01-01","2013-12-31",opt_all_columns = TRUE)
  w2014 <- getWeatherForDate("EDDT","2014-01-01","2014-12-31",opt_all_columns = TRUE) 
  w2015 <- getWeatherForDate("EDDT","2015-01-01","2015-09-17",opt_all_columns = TRUE)
  names(w2015)[2] <- names(w2013)[2]
  weather_new <- rbind(w2013,w2014,w2015)
  rm(w2013,w2014,w2015)
  weather_new$Date <- as.Date(weather_new$Date)
  weather_new$State <- "ST"
  weather <- rbind(weather,weather_new)
  
  #Saxony
  w2013 <- getWeatherForDate("EDDP","2013-01-01","2013-12-31",opt_all_columns = TRUE)
  w2014 <- getWeatherForDate("EDDP","2014-01-01","2014-12-31",opt_all_columns = TRUE) 
  w2015 <- getWeatherForDate("EDDP","2015-01-01","2015-09-17",opt_all_columns = TRUE)
  names(w2015)[2] <- names(w2013)[2]
  weather_new <- rbind(w2013,w2014,w2015)
  rm(w2013,w2014,w2015)
  weather_new$Date <- as.Date(weather_new$Date)
  weather_new$State <- "SN"
  weather <- rbind(weather,weather_new)
  
  #Lower Saxony
  w2013 <- getWeatherForDate("EDDW","2013-01-01","2013-12-31",opt_all_columns = TRUE)
  w2014 <- getWeatherForDate("EDDW","2014-01-01","2014-12-31",opt_all_columns = TRUE) 
  w2015 <- getWeatherForDate("EDDW","2015-01-01","2015-09-17",opt_all_columns = TRUE)
  names(w2015)[2] <- names(w2013)[2]
  weather_new <- rbind(w2013,w2014,w2015)
  rm(w2013,w2014,w2015)
  weather_new$Date <- as.Date(weather_new$Date)
  weather_new$State <- "HB,NI"
  weather <- rbind(weather,weather_new)
  
  #Read germany DAX data (only for the trading days)
  getSymbols("^GDAXI", from="2013-01-01", to="2015-09-17") 
  DAX <- as.data.frame(GDAXI); rm(GDAXI)
  DAX$Date <- as.Date(row.names(DAX))
  
  #Germany Macro data
  salesMoM <- read.csv("salesMoM.csv")
  salesMoM$Date <- as.Date(sapply(salesMoM$DateTime,function(x) {strsplit(as.character(x)," ")[[1]][1]}),"%m/%d/%Y")
  salesMoM$MonthYear <- as.yearmon(salesMoM$Date)
  salesMoM <- salesMoM[,c(2,7)]
  names(salesMoM)[1] <- "salesMoM"
  salesMoM <- salesMoM[!duplicated(salesMoM$MonthYear),]
  GfK_CCI <- read.csv("GfK_CCI.csv")
  GfK_CCI$Date <- as.Date(sapply(GfK_CCI$DateTime,function(x) {strsplit(as.character(x)," ")[[1]][1]}),"%m/%d/%Y")
  GfK_CCI$MonthYear <- as.yearmon(GfK_CCI$Date)
  GfK_CCI <- GfK_CCI[,c(2,7)]
  names(GfK_CCI)[1] <- "GfK_CCI"
  GfK_CCI <- GfK_CCI[!duplicated(GfK_CCI$MonthYear),]
  CPIMoM <- read.csv("CPIMoM.csv")
  CPIMoM$Date <- as.Date(sapply(CPIMoM$DateTime,function(x) {strsplit(as.character(x)," ")[[1]][1]}),"%m/%d/%Y")
  CPIMoM$MonthYear <- as.yearmon(CPIMoM$Date)
  CPIMoM <- CPIMoM[,c(2,7)]
  names(CPIMoM)[1] <- "CPIMoM"
  CPIMoM <- CPIMoM[!duplicated(CPIMoM$MonthYear),]
}

#Feature Engineering - will take a few minutes to compute
{
  #Store
  {  
    store$CompetitionDistance <- log(store$CompetitionDistance) #make a log transform
    store$CompetitionDistance[is.na(store$CompetitionDistance)] <- 12 #NA is far distanced -> should be imputed really
    store$CompetitionDistance <- cut(store$CompetitionDistance, 
                                     breaks = c(-Inf, 6, 7,8,9, 10, Inf), 
                                     labels = c("0-6","6-7","7-8","8-9","9-10","10+"), 
                                     right = FALSE)
    store$CompetitionOpenSinceDate <- as.Date(paste0(store$CompetitionOpenSinceYear,"-",store$CompetitionOpenSinceMonth,"-01")) #real date of competitor open
    store$Promo2SinceDate <- c(as.Date(NA),as.Date(paste0(store$Promo2SinceYear[c(2:1115)],"-01","-01"))+store$Promo2SinceWeek[c(2:1115)]*7) #real date of promo2 starting
    levels(store$PromoInterval) <- c("","February,May,August,November","January,April,July,October","March,June,September,December")
    store$State <- as.character(germany_states$State)
  }

  #Train
  {
    train$Date <- as.Date(train$Date)
    additional_data <- NULL
    for (i in 1:nrow(store)) #fill in missing days for some stores (with closed store data)
    {
      cat(i,"\n")
      if (nrow(train[train$Store==store$Store[i],])==758)
      {
        additional_data_store <- data.frame(Store=rep(store$Store[i],184),
                                      DayOfWeek=c(2:7,rep(c(1:7),25),1:3),
                                      Date=as.Date(as.Date("2014-07-01"):as.Date("2014-12-31")),
                                      Sales=rep(0,184),
                                      Customers=rep(0,184),
                                      Open=rep(0,184),
                                      Promo=rep(0,184),
                                      StateHoliday=train$StateHoliday[train$Store==1 & train$Date<=as.Date("2014-12-31") & train$Date>=as.Date("2014-07-01")],
                                      SchoolHoliday=train$SchoolHoliday[train$Store==1 & train$Date<=as.Date("2014-12-31") & train$Date>=as.Date("2014-07-01")])
        additional_data <- rbind(additional_data,additional_data_store)
      }
    }
    train <- rbind(train,additional_data)
    train <- train[order(train$Date),] #sort by date and store number
    train <- train[order(train$Store),]
    train <- merge(store[,c(1:4,13)], train, by = c("Store"),sort = FALSE)
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
    train$MonthYear <- as.yearmon(train$Date)
    train <- merge(salesMoM, train, by = c("MonthYear"),sort = FALSE,all.y = TRUE)
    train <- merge(GfK_CCI, train, by = c("MonthYear"),sort = FALSE,all.y = TRUE)
    train <- merge(CPIMoM, train, by = c("MonthYear"),sort = FALSE,all.y = TRUE)
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
    train <- merge(weather[,c(1,4,10,19,21,23,25)], train, by = c("Date","State"),sort = FALSE)
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
    train$StoreTypeOpenPercent <- NA
    train$AssortmentOpenPercent <- NA
    train$CompetitionDistanceOpenPercent <- NA
    tableAggStoreType <- aggregate( as.integer(as.character(Open)) ~ Date + StoreType, train , sum )
    tableAggAssortment <- aggregate( as.integer(as.character(Open)) ~ Date + Assortment, train , sum )
    tableAggCompetitionDistance <- aggregate( as.integer(as.character(Open)) ~ Date + CompetitionDistance, train , sum )
    for (i in 1:length(unique(train$Date)))
    {
      cat(i,"\n")
      train$StoreTypeOpenPercent[train$Date==unique(train$Date)[i]] <- tableAggStoreType$`as.integer(as.character(Open))`[tableAggStoreType$Date==unique(train$Date)[i] & tableAggStoreType$StoreType==train$StoreType[i]]/table(store$StoreType)[train$StoreType[i]]
      train$AssortmentOpenPercent[train$Date==unique(train$Date)[i]] <- tableAggAssortment$`as.integer(as.character(Open))`[tableAggAssortment$Date==unique(train$Date)[i] & tableAggAssortment$Assortment==train$Assortment[i]]/table(store$Assortment)[train$Assortment[i]]
      train$CompetitionDistanceOpenPercent[train$Date==unique(train$Date)[i]] <- tableAggCompetitionDistance$`as.integer(as.character(Open))`[tableAggCompetitionDistance$Date==unique(train$Date)[i] & tableAggCompetitionDistance$CompetitionDistance==train$CompetitionDistance[i]]/table(store$CompetitionDistance)[train$CompetitionDistance[i]]
    }
  }

  #Test
  {
    test$Open[is.na(test$Open)] <- 1 # a fill-in (certain)
    test$Date <- as.Date(test$Date)
    test <- test[order(test$Date),] #sort by date and store number
    test <- test[order(test$Store),]
    test <- merge(store[,c(1:4,13)], test, by = c("Store"),sort = FALSE)
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
    test$MonthYear <- as.yearmon(test$Date)
    test <- merge(salesMoM, test, by = c("MonthYear"),sort = FALSE,all.y = TRUE)
    test <- merge(GfK_CCI, test, by = c("MonthYear"),sort = FALSE,all.y = TRUE)
    test <- merge(CPIMoM, test, by = c("MonthYear"),sort = FALSE,all.y = TRUE)
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
    test <- merge(weather[,c(1,4,10,19,21,23,25)], test, by = c("Date","State"),sort = FALSE)
    test$Events <- factor(test$Events,levels = levels(train$Events))
    test <- merge(DAX[,c(4,5,7)], test, by = c("Date"),sort = FALSE,all.y = TRUE)
    test <- test[order(test$Date),] #sort by date and store number
    test <- test[order(test$Store),]
    test$GDAXI.Volume[test$GDAXI.Volume==0] <- NA
    test$GDAXI.Volume <- c(70000000,70000000,na.locf(test$GDAXI.Volume)) #fill in NA's
    test$GDAXI.Close <- c(11500,11500,na.locf(test$GDAXI.Close)) #must be sorted by store and date
    test$StoreTypeOpenPercent <- NA
    test$AssortmentOpenPercent <- NA
    test$CompetitionDistanceOpenPercent <- NA
    tableAggStoreType <- aggregate( as.integer(as.character(Open)) ~ Date + StoreType, test , sum )
    tableAggAssortment <- aggregate( as.integer(as.character(Open)) ~ Date + Assortment, test , sum )
    tableAggCompetitionDistance <- aggregate( as.integer(as.character(Open)) ~ Date + CompetitionDistance, test , sum )
    for (i in 1:length(unique(test$Date)))
    {
      cat(i,"\n")
      test$StoreTypeOpenPercent[test$Date==unique(test$Date)[i]] <- tableAggStoreType$`as.integer(as.character(Open))`[tableAggStoreType$Date==unique(test$Date)[i] & tableAggStoreType$StoreType==test$StoreType[i]]/table(store$StoreType)[test$StoreType[i]]
      test$AssortmentOpenPercent[test$Date==unique(test$Date)[i]] <- tableAggAssortment$`as.integer(as.character(Open))`[tableAggAssortment$Date==unique(test$Date)[i] & tableAggAssortment$Assortment==test$Assortment[i]]/table(store$Assortment)[test$Assortment[i]]
      test$CompetitionDistanceOpenPercent[test$Date==unique(test$Date)[i]] <- tableAggCompetitionDistance$`as.integer(as.character(Open))`[tableAggCompetitionDistance$Date==unique(test$Date)[i] & tableAggCompetitionDistance$CompetitionDistance==test$CompetitionDistance[i]]/table(store$CompetitionDistance)[test$CompetitionDistance[i]]
    }
  }

  rm(DAX,weather,CPIMoM,GfK_CCI,salesMoM,weather_new,additional_data,additional_data_store,germany_states,tableAggAssortment,tableAggStoreType,tableAggCompetitionDistance)
  
  #Imputation of missing columns (medians at the moment)
    train$Mean_TemperatureC[is.na(train$Mean_TemperatureC)] <- 11
    train$Mean_Humidity[is.na(train$Mean_Humidity)] <- 75
    train$Mean_Wind_SpeedKm_h[is.na(train$Mean_Wind_SpeedKm_h)] <- 11
    train$salesMoM[is.na(train$salesMoM)] <- (-0.2)
    test$Events[is.na(test$Events)] <- ""
    train$Events[is.na(train$Events)] <- ""
  
  
}

#Train a RF model
{
  train <- train[ which(train$Open=='1'),]
  train <- train[ which(train$Sales!='0'),]
  
  train$Customers <- NULL
  train$Date <- NULL
  test$Date <- NULL
  train$Open <- NULL
  #test$Open <- NULL  
  
  valid_rows <- which(train$Year==2015 & (train$Month=="July"|train$Month=="June")) #take 2 last months as validation
  train_rows <- sample(nrow(train[-valid_rows,]),100000)
  
  RF <- randomForest(x = train[train_rows,-c(8,13)],y = train$Sales[train_rows],xtest = train[valid_rows,-c(8,13)],ytest = train$Sales[valid_rows],ntree = 200,do.trace = T,keep.forest = T)
  
  #Calcualte RPMSE
  predicted_valid <- exp(RF$test$predicted)-1
  observed_valid <- exp(train$Sales[valid_rows])-1
  predicted_valid <- predicted_valid[which(observed_valid!=0)] #sales= 0 ingnored in scoring
  observed_valid <- observed_valid[which(observed_valid!=0)] #sales= 0 ingnored in scoring
  RMSPE_valid <- sqrt(mean(((observed_valid-predicted_valid)/observed_valid)^2))
  #cat("RMSPE_valid",RMSPE_valid,"\n")
  
  predicted_train <- exp(RF$predicted)-1
  observed_train <- exp(train$Sales[train_rows])-1
  predicted_train <- predicted_train[which(observed_train!=0)] #sales= 0 ingnored in scoring
  observed_train <- observed_train[which(observed_train!=0)] #sales= 0 ingnored in scoring
  RMSPE_train <- sqrt(mean(((observed_train-predicted_train)/observed_train)^2))
  #cat("RMSPE_train",RMSPE_train,"\n")
  
  predicted_test <- exp(predict(RF,test[,-c(8,12,14)]))-1
  predicted_test[test$Open==0] <- 0
  submission <- data.frame(Id=test$Id, Sales=pred1)
  cat("saving the submission file\n")
  write_csv(submission, "rf4.csv")
}

#Train a forecasting model per store
{
  train$Store <- as.numeric(as.character(train$Store))
  test$Store <- as.numeric(as.character(test$Store))
    
  S <- length(unique(test$Store))
  submission <- data.frame(Id=NA, Sales=NA)
  
  #Combinme Function to the parallel foreach
  comb <- function(x, ...) {
    lapply(seq_along(x),
           function(i) c(x[[i]], lapply(list(...), function(y) y[[i]])))
  }
  
  #Wrapper function to covariate selection
  evaluator <- function(subset) {
    #cat(subset,"\n")
    #ARIMAX model
    #rm(model)
    model <- auto.arima(y, xreg=cbind(z,xreg[,c(subset)]))
    #summary(model)
    
    #Make forecast
    fc <- forecast(model, xreg=cbind(zf,xregf[,c(subset)]))
    
    #Calcualte RPMSE
    predicted_valid <- exp(fc$mean)-1
    observed_valid <- exp(valid_store$Sales)-1
    predicted_valid <- predicted_valid[which(observed_valid!=0)] #sales= 0 ingnored in scoring
    observed_valid <- observed_valid[which(observed_valid!=0)] #sales= 0 ingnored in scoring
    RMSPE_valid <- sqrt(mean(((observed_valid-predicted_valid)/observed_valid)^2))
    #cat("RMSPE_valid",RMSPE_valid,"\n")
    
    predicted_train <- exp(fitted(model))-1
    observed_train <- exp(train_store$Sales)-1
    predicted_train <- predicted_train[which(observed_train!=0)] #sales= 0 ingnored in scoring
    observed_train <- observed_train[which(observed_train!=0)] #sales= 0 ingnored in scoring
    RMSPE_train <- sqrt(mean(((observed_train-predicted_train)/observed_train)^2))
    #cat("RMSPE_train",RMSPE_train,"\n")
    
    #Plot fit
#     plot(exp(y[(length(y)-200):length(y)])-1,type="l",main=paste("Training Store: ",store_num," RMSPE: ",round(RMSPE_train,4)),xlab="Time",ylab="Sales")
#     lines(exp(fitted(model)[(length(y)-200):length(y)])-1,col=2)
#     plot(exp(valid_store$Sales)-1,type="l",main=paste("Validation Store: ",store_num," RMSPE: ",round(RMSPE_valid,4)),xlab="Time",ylab="Sales")
#     lines(exp(as.numeric(fc$mean))-1,col=3)
#     Acf(residuals((model)), main="ACF of residuals")
    
    return(1-RMSPE_train)
  }
  
  cl <- makePSOCKcluster(12) #number of cores
  clusterEvalQ(cl, library(foreach))
  registerDoParallel(cl)
  getDoParWorkers() # Just checking, how many workers you have
  results <- foreach (i=1:12,.combine='comb', .multicombine=TRUE,.init=list(list(),list(),list()),.packages=(c("FSelector","forecast")),.verbose=TRUE) %dopar%
  {
    #Select the current store
    {    
      #cat(unique(test$Store)[i],"\n")
      store_num <- unique(test$Store)[i] #choose store num [1..1115] 
      store_tmp <- train[train$Store %in% store_num,]
      h <- which(store_tmp$Year==2015 & (store_tmp$Month=="July"|store_tmp$Month=="June")) #take 2 last months as validation
      valid_store <- store_tmp[h,] #validation
      train_store <- store_tmp[-h,] #training
      test_store <- test[test$Store %in% store_num,] #validation
    }
    
    #Use ARIMAX
    # Create matrix of numeric predictors (covariates)
    {
    xreg <- cbind(GDAXI.Close=train_store$GDAXI.Close,
                  CPIMoM=train_store$CPIMoM,
                  GfK_CCI=train_store$GfK_CCI,
                  salesMoM=train_store$salesMoM,
                  Mean_TemperatureC=train_store$Mean_TemperatureC,
                  Mean_Humidity=train_store$Mean_Humidity,
                  Mean_Wind_SpeedKm_h=train_store$Mean_Wind_SpeedKm_h,
                  Precipitationmm=train_store$Precipitationmm,
                  StoreType=model.matrix(~as.factor(train_store$StoreType)),
                  Assortment=model.matrix(~as.factor(train_store$Assortment)),
                  CompetitionDistance=model.matrix(~as.factor(train_store$CompetitionDistance)),
                  Open=model.matrix(~as.factor(train_store$Open)),
                  Promo=model.matrix(~as.factor(train_store$Promo)),
                  StateHoliday=model.matrix(~as.factor(train_store$StateHoliday)),
                  SchoolHoliday=model.matrix(~as.factor(train_store$SchoolHoliday)),
                  weekend=model.matrix(~as.factor(train_store$weekend)),
                  Promo2=model.matrix(~as.factor(train_store$Promo2)),
                  StoreTypeOpenPercent=train_store$StoreTypeOpenPercent,
                  AssortmentOpenPercent=train_store$AssortmentOpenPercent,
                  CompetitionDistanceOpenPercent=train_store$CompetitionDistanceOpenPercent,
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
                   CPIMoM=valid_store$CPIMoM,
                   GfK_CCI=valid_store$GfK_CCI,
                   salesMoM=valid_store$salesMoM,
                   Mean_TemperatureC=valid_store$Mean_TemperatureC,
                   Mean_Humidity=valid_store$Mean_Humidity,
                   Mean_Wind_SpeedKm_h=valid_store$Mean_Wind_SpeedKm_h,
                   Precipitationmm=valid_store$Precipitationmm,
                   StoreType=model.matrix(~as.factor(valid_store$StoreType)),
                   Assortment=model.matrix(~as.factor(valid_store$Assortment)),
                   CompetitionDistance=model.matrix(~as.factor(valid_store$CompetitionDistance)),
                   Open=model.matrix(~as.factor(valid_store$Open)),
                   Promo=model.matrix(~as.factor(valid_store$Promo)),
                   StateHoliday=model.matrix(~as.factor(valid_store$StateHoliday)),
                   SchoolHoliday=model.matrix(~as.factor(valid_store$SchoolHoliday)),
                   weekend=model.matrix(~as.factor(valid_store$weekend)),
                   Promo2=model.matrix(~as.factor(valid_store$Promo2)),
                   StoreTypeOpenPercent=valid_store$StoreTypeOpenPercent,
                   AssortmentOpenPercent=valid_store$AssortmentOpenPercent,
                   CompetitionDistanceOpenPercent=valid_store$CompetitionDistanceOpenPercent,
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
    
    xregt <- cbind(GDAXI.Close=test_store$GDAXI.Close,
                   CPIMoM=test_store$CPIMoM,
                   GfK_CCI=test_store$GfK_CCI,
                   salesMoM=test_store$salesMoM,
                   Mean_TemperatureC=test_store$Mean_TemperatureC,
                   Mean_Humidity=test_store$Mean_Humidity,
                   Mean_Wind_SpeedKm_h=test_store$Mean_Wind_SpeedKm_h,
                   Precipitationmm=test_store$Precipitationmm,
                   StoreType=model.matrix(~as.factor(test_store$StoreType)),
                   Assortment=model.matrix(~as.factor(test_store$Assortment)),
                   CompetitionDistance=model.matrix(~as.factor(test_store$CompetitionDistance)),
                   Open=model.matrix(~as.factor(test_store$Open)),
                   Promo=model.matrix(~as.factor(test_store$Promo)),
                   StateHoliday=model.matrix(~as.factor(test_store$StateHoliday)),
                   SchoolHoliday=model.matrix(~as.factor(test_store$SchoolHoliday)),
                   weekend=model.matrix(~as.factor(test_store$weekend)),
                   Promo2=model.matrix(~as.factor(test_store$Promo2)),
                   StoreTypeOpenPercent=test_store$StoreTypeOpenPercent,
                   AssortmentOpenPercent=test_store$AssortmentOpenPercent,
                   CompetitionDistanceOpenPercent=test_store$CompetitionDistanceOpenPercent,
                   CompetitorOpen=model.matrix(~as.factor(test_store$CompetitorOpen)),
                   Promo2Interval=model.matrix(~as.factor(test_store$Promo2Interval)),
                   OpenAfterClose=model.matrix(~as.factor(test_store$OpenAfterClose)),
                   OpenBeforeClose=model.matrix(~as.factor(test_store$OpenBeforeClose)),
                   PromoFirstDay=model.matrix(~as.factor(test_store$PromoFirstDay)),
                   PromoLastDay=model.matrix(~as.factor(test_store$PromoLastDay)),
                   SchoolHolidayStart=model.matrix(~as.factor(test_store$SchoolHolidayStart)),
                   SchoolHolidayEnd=model.matrix(~as.factor(test_store$SchoolHolidayEnd)),
                   StateHolidayStart=model.matrix(~as.factor(test_store$StateHolidayStart)),
                   StatelHolidayEnd=model.matrix(~as.factor(test_store$StatelHolidayEnd)))
    }
    
    #Manipulate matrix
    {    
        #Remove intercepts & change names
        remove_intercept <- which(colnames(xreg)=="(Intercept)")
        xreg <- xreg[,-c(remove_intercept)]
        xregf <- xregf[,-c(remove_intercept)]
        xregt <- xregt[,-c(remove_intercept)]
        colnames(xreg) <- gsub('as.factor\\(train_store\\$',"",colnames(xreg))
        colnames(xregf) <- gsub('as.factor\\(valid_store\\$',"",colnames(xregf))
        colnames(xregt) <- gsub('as.factor\\(test_store\\$',"",colnames(xregt))
        
        #remove unwanted columns
        colVars <- apply(xreg, 2, var) 
        remove_variance <- which(colVars==0)
        xreg <- xreg[,-c(remove_variance)]
        xregf <- xregf[,-c(remove_variance)]
        xregt <- xregt[,-c(remove_variance)]
        
    #     #Reduce correlation
    #     xreg_cor = cor(xreg)
    #     remove_correlation = findCorrelation(xreg_cor, cutoff=0.5) # putt any value as a "cutoff" 
    #     xreg = xreg[,-c(remove_correlation)]
    #     xregf <- xregf[,-c(remove_correlation)]
    }
    
    #Make timing series (inc Fourier)
    {    
        z <- fourier(ts(train_store$Sales, frequency=365.25), K=5) #choose the fourier coefficients wisely!
        zf <- fourier(ts(valid_store$Sales, frequency=365.25), K=5) #choose the fourier coefficients wisely!
        y <- ts(train_store$Sales, frequency=7) #choose the frequency wisely!
    }
 
    #Run forecast ARIMAX model with 2 months validation and BFS covariates selection
    {
      #Run BFS on ARIMA model with covariates selection, check validation
      subset <- 1: ncol(xreg) #default - no feature selection
      subset <- best.first.search(sample(colnames(xreg)), evaluator)

      #Run that model on validation to recompute validation score
      model <- auto.arima(y, xreg=cbind(z,xreg[,c(subset)]))
      
      #Make forecast (only validation)
      fc <- forecast(model, xreg=cbind(zf,xregf[,c(subset)]))
      
      #Calcualte RPMSE
      predicted_valid <- exp(fc$mean)-1
      observed_valid <- exp(valid_store$Sales)-1
      predicted_valid <- predicted_valid[which(observed_valid!=0)] #sales= 0 ingnored in scoring
      observed_valid <- observed_valid[which(observed_valid!=0)] #sales= 0 ingnored in scoring
      RMSPE_valid <- sqrt(mean(((observed_valid-predicted_valid)/observed_valid)^2))
      #cat("RMSPE_valid",RMSPE_valid,"\n")
      
      # ARIMAX model for the complete train
      rm(model)
      xreg <- rbind(xreg,xregf)
      z <- fourier(ts(exp(c(train_store$Sales,valid_store$Sales))-1, frequency=365.25), K=5)
      y <- ts(exp(c(train_store$Sales,valid_store$Sales))-1, frequency=7) 
      zt <- fourierf(ts(exp(c(train_store$Sales,valid_store$Sales))-1, frequency=365.25), K=5,h=nrow(test_store))
      model <- auto.arima(y, xreg=cbind(z,xreg[,c(subset)]))
      #summary(model)
      
      #Make forecast
      fc <- forecast(model, xreg=cbind(zt,xregt[,c(subset)]),h=nrow(test_store))
     
      #Calcualte RPMSE
      predicted_test <- as.numeric(fc$mean)
      predicted_test[which(test_store$Open==0)] <- 0 #sales= 0 ingnored in scoring

      predicted_train <- fitted(model)
      observed_train <- exp(c(train_store$Sales,valid_store$Sales))-1
      predicted_train <- predicted_train[which(observed_train!=0)] #sales= 0 ingnored in scoring
      observed_train <- observed_train[which(observed_train!=0)] #sales= 0 ingnored in scoring
      RMSPE_train <- sqrt(mean(((observed_train-predicted_train)/observed_train)^2))
      #cat("RMSPE_train",RMSPE_train,"\n")
      
      #Plot fit
#       plot(exp(y[(length(y)-200):length(y)])-1,type="l",main=paste("Training Store: ",store_num," RMSPE: ",round(RMSPE_train,4)),xlab="Time",ylab="Sales")
#       lines(exp(fitted(model)[(length(y)-200):length(y)])-1,col=2)
#       plot(predicted_test,type="l",col=2,main=paste("Testing Store: ",store_num),xlab="Time",ylab="Sales")
#       Acf(residuals((model)), main="ACF of residuals")
      
    }
    
    list(RMSPE_train,RMSPE_valid,predicted_test) #return results from foreach
  }
  stopCluster(cl)
  
  mean(RMSPE_train)
  mean(RMSPE_valid)
  
  pred_rf[which(test_store$Open=='0')] <- 0
  submission_new <- data.frame(Id=test_store$Id, Sales=pred_rf)
  submission <- rbind(submission,submission_new)
  submission <- submission[order(submission$Id),] #order again by ID
  write.csv(submission, "rf3.csv",row.names = F)
  
}

#Train a XGB . RF model
{
  RMPSE<- function(preds, dtrain) {
    labels <- getinfo(dtrain, "label")
    elab<-exp(as.numeric(labels))-1
    epreds<-exp(as.numeric(preds))-1
    err <- sqrt(mean((epreds/elab-1)^2))
    return(list(metric = "RMPSE", value = err))
  }
  
  train <- train[ which(train$Open=='1'),]
  train <- train[ which(train$Sales!='0'),]

  train$Customers <- NULL
  train$Date <- NULL
  test$Date <- NULL
  train$Open <- NULL
  #test$Open <- NULL  
  
  h <- which(train$Year==2015 & (train$Month=="July"|train$Month=="June")) #take 2 last months as validation

    dval<-xgb.DMatrix(data=data.matrix(train[h,-13]),label=train$Sales[h])
    dtrain<-xgb.DMatrix(data=data.matrix(train[-h,-13]),label=train$Sales[-h])
    
    watchlist<-list(val=dval,train=dtrain)
    param <- list(  objective           = "reg:linear", 
                    booster = "gbtree",
                    eta                 = 0.1, # 0.06, #0.01,
                    max_depth           = 7, #changed from default of 8
                    subsample           = 0.9, # 0.7
                    colsample_bytree    = 0.7 # 0.7
                    #num_parallel_tree   = 2
                    # alpha = 0.0001, 
                    # lambda = 1
    )
    
    clf <- xgb.train(   params              = param, 
                        data                = dtrain, 
                        nrounds             = 7000, #300, #280, #125, #250, # changed from 300
                        verbose             = 0,
                        early.stop.round    = 100,
                        watchlist           = watchlist,
                        maximize            = FALSE#,
                        #feval=RMPSE
    )
    
    pred1 <- exp(predict(clf, data.matrix(test[,-13])))-1
    pred1[test$Open==0] <- 0
    submission <- data.frame(Id=test$Id, Sales=pred1)
    cat("saving the submission file\n")
    write_csv(submission, "rf1.csv")



}

#Read IoTM data
trainset <- read.csv("C:\\IoTM\\Churn\\churn_30_12\\trainset_new.csv")
testset <- read.csv("C:\\IoTM\\Churn\\churn_30_12\\testset.csv")
trainset$month_year <- as.Date(as.character(trainset$month_year))
testset$month_year <- as.Date(as.character(testset$month_year))
trainset$dominant_state_month <- factor(trainset$dominant_state_month,levels=levels(testset$dominant_state_month))
trainset$label <- factor(trainset$label)
customers <- unique(as.character(trainset$end_customer))
customers_test <- unique(as.character(testset$end_customer))

##### USE RF classifier for prediction for 3 months time if still churn / not churn ####
#build new train and test with removal of first 6 months from each customer - those are biased data (mostly 0)
trainset_new <- NULL
for (i in 1:length(customers))
{
    cat(i,"\n")
    customer <- trainset[trainset$end_customer==customers[i],]
    customer_train <- customer[7:nrow(customer),]
    trainset_new <- rbind(trainset_new,customer_train)
}
trainset <- trainset_new

testset_new <- NULL
for (i in 1:length(customers_test))
{
    cat(i,"\n")
    customer <- testset[testset$end_customer==customers_test[i],]
    customer_test <- customer[7:nrow(customer),]
    testset_new <- rbind(testset_new,customer_test)
}
testset <- testset_new
testset <- testset[,-c(28:33)] 
testset <- testset[-which(is.na(testset$end_customer)),]

#build new train and test with lagged label for 3 months
trainset_new <- NULL
testset_new <- NULL
for (i in 1:length(customers))
{
    customer <- trainset[trainset$end_customer==customers[i],]
    label <- customer$label
    customer_train <- customer[-c((nrow(customer)-2):nrow(customer)),]
    customer_test <- customer[c((nrow(customer)-2):nrow(customer)),]
    customer_train$label <- label[4:length(label)]
    customer_test$label <- NULL
    trainset_new <- rbind(trainset_new,customer_train)
    testset_new <- rbind(testset_new,customer_test)
}
testset_new <- rbind(testset_new,testset)

#predict for the lagged labeled for 3 months
#Create train, validation
K <- 3; folds <- createMultiFolds(trainset_new$label,k = K,times = 1)
train_acc <- rep(NA,K); train_fpr <- rep(NA,K); train_fnr <- rep(NA,K); train_kappa <- rep(NA,K)
valid_acc <- rep(NA,K); valid_fpr <- rep(NA,K); valid_fnr <- rep(NA,K); valid_kappa <- rep(NA,K)

for (k in 1:K)
{
    rf_valid <- randomForest(x=trainset_new[folds[[k]],-c(1,2,ncol(trainset_new))],
                             y=trainset_new$label[folds[[k]]],
                             xtest=trainset_new[-folds[[k]],-c(1,2,ncol(trainset_new))],
                             ytest=trainset_new$label[-folds[[k]]],
                             ntree=20,
                             do.trace=T,
                             keep.forest=T)
    train_acc[k] <- classAgreement(rf_valid$confusion)$diag
    train_kappa[k] <- classAgreement(rf_valid$confusion)$kappa
    train_fpr[k] <- 1-rf_valid$confusion[4]/(rf_valid$confusion[3]+rf_valid$confusion[4])
    train_fnr[k] <- 1-rf_valid$confusion[1]/(rf_valid$confusion[2]+rf_valid$confusion[1])
    valid_acc[k] <- classAgreement(rf_valid$test$confusion)$diag
    valid_kappa[k] <- classAgreement(rf_valid$test$confusion)$kappa
    valid_fpr[k] <- 1-rf_valid$test$confusion[4]/(rf_valid$test$confusion[3]+rf_valid$test$confusion[4])
    valid_fnr[k] <- 1-rf_valid$test$confusion[1]/(rf_valid$test$confusion[2]+rf_valid$test$confusion[1])
}
scores_rf <- as.data.frame(rbind(train_acc,train_kappa,train_fpr,train_fnr,valid_acc,valid_kappa,valid_fpr,valid_fnr))
scores_rf$mean_folds <- c(mean(train_acc),mean(train_kappa),mean(train_fpr),mean(train_fnr),mean(valid_acc),mean(valid_kappa),mean(valid_fpr),mean(valid_fnr))
scores_rf$std_folds <- c(sd(train_acc),sd(train_kappa),sd(train_fpr),sd(train_fnr),sd(valid_acc),sd(valid_kappa),sd(valid_fpr),sd(valid_fnr))

rf <- randomForest(x=trainset_new[,-c(1,2,ncol(trainset_new))],
                   y=trainset_new$label,
                   ntree=20,
                   do.trace=T,
                   importance = T,
                   keep.forest=T)
final_acc <- classAgreement(rf$confusion)$diag
final_kappa <- classAgreement(rf$confusion)$kappa
final_fpr <- 1-rf$confusion[4]/(rf$confusion[3]+rf$confusion[4])
final_fnr <- 1-rf$confusion[1]/(rf$confusion[2]+rf$confusion[1])
scores_rf$final <- c(final_acc,final_kappa,final_fpr,final_fnr,rep(NA,4))
varImpPlot(rf)

testset_new$label_predcited <- predict(rf,testset_new[,-c(1,2)])
testset_new$churn_prob <- predict(rf,testset_new[,-c(1,2)],"prob")[,2]*100

#predict for the current month labeled
#Create train, validation
K <- 3; folds <- createMultiFolds(trainset$label,k = K,times = 1)
train_acc <- rep(NA,K); train_fpr <- rep(NA,K); train_fnr <- rep(NA,K); train_kappa <- rep(NA,K)
valid_acc <- rep(NA,K); valid_fpr <- rep(NA,K); valid_fnr <- rep(NA,K); valid_kappa <- rep(NA,K)

for (k in 1:K)
{
    rf_valid <- randomForest(x=trainset[folds[[k]],-c(1,2,ncol(trainset))],
                             y=trainset$label[folds[[k]]],
                             xtest=trainset[-folds[[k]],-c(1,2,ncol(trainset))],
                             ytest=trainset$label[-folds[[k]]],
                             ntree=20,
                             do.trace=T,
                             keep.forest=T)
    train_acc[k] <- classAgreement(rf_valid$confusion)$diag
    train_kappa[k] <- classAgreement(rf_valid$confusion)$kappa
    train_fpr[k] <- 1-rf_valid$confusion[4]/(rf_valid$confusion[3]+rf_valid$confusion[4])
    train_fnr[k] <- 1-rf_valid$confusion[1]/(rf_valid$confusion[2]+rf_valid$confusion[1])
    valid_acc[k] <- classAgreement(rf_valid$test$confusion)$diag
    valid_kappa[k] <- classAgreement(rf_valid$test$confusion)$kappa
    valid_fpr[k] <- 1-rf_valid$test$confusion[4]/(rf_valid$test$confusion[3]+rf_valid$test$confusion[4])
    valid_fnr[k] <- 1-rf_valid$test$confusion[1]/(rf_valid$test$confusion[2]+rf_valid$test$confusion[1])
}
scores_rf <- as.data.frame(rbind(train_acc,train_kappa,train_fpr,train_fnr,valid_acc,valid_kappa,valid_fpr,valid_fnr))
scores_rf$mean_folds <- c(mean(train_acc),mean(train_kappa),mean(train_fpr),mean(train_fnr),mean(valid_acc),mean(valid_kappa),mean(valid_fpr),mean(valid_fnr))
scores_rf$std_folds <- c(sd(train_acc),sd(train_kappa),sd(train_fpr),sd(train_fnr),sd(valid_acc),sd(valid_kappa),sd(valid_fpr),sd(valid_fnr))

rf <- randomForest(x=trainset[,-c(1,2,ncol(trainset))],
                   y=trainset$label,
                   ntree=20,
                   do.trace=T,
                   importance = T,
                   keep.forest=T)
final_acc <- classAgreement(rf$confusion)$diag
final_kappa <- classAgreement(rf$confusion)$kappa
final_fpr <- 1-rf$confusion[4]/(rf$confusion[3]+rf$confusion[4])
final_fnr <- 1-rf$confusion[1]/(rf$confusion[2]+rf$confusion[1])
scores_rf$final <- c(final_acc,final_kappa,final_fpr,final_fnr,rep(NA,4))
varImpPlot(rf)

testset$label_predcited <- predict(rf,testset[,-c(1,2)])
testset$churn_prob <- predict(rf,testset[,-c(1,2)],"prob")[,2]*100

##### Forecasting #### 
#Using Holt method
rmse_test.ready <- rep(0,length(customers))
rmse_Activation.Ready <- rep(0,length(customers))
rmse_Activated <- rep(0,length(customers))
rmse_Deactivated <- rep(0,length(customers))
rmse_Retired <- rep(0,length(customers))
rmse_Inventory <- rep(0,length(customers))
for (i in 1:length(customers))
{
    customer <- trainset[trainset$end_customer==customers[i],]
    
    par(mfrow=c(3,2))
    fit <- holt(window(customer$Test.Ready), initial="simple", h=3) 
    rmse_test.ready[i] <- as.numeric(accuracy(fit)[2])
    fit$fitted[fit$fitted<0] <- 0; fit$fitted[fit$fitted>1] <- 1
    plot(fit, type="o",fcol="black", plot.conf=F,main="Test.Ready")
    lines(fitted(fit), col="blue"); lines(fit$mean, col="blue", type="o") 
    
    fit <- holt(window(customer$Activation.Ready),initial="simple", h=3) 
    rmse_Activation.Ready[i] <- as.numeric(accuracy(fit)[2])
    fit$fitted[fit$fitted<0] <- 0; fit$fitted[fit$fitted>1] <- 1
    plot(fit, type="o",fcol="black", plot.conf=F,main="Activation.Ready")
    lines(fitted(fit), col="blue"); lines(fit$mean, col="blue", type="o") 
    
    fit <- holt(window(customer$Activated), initial="simple", h=3) 
    rmse_Activated[i] <- as.numeric(accuracy(fit)[2])
    fit$fitted[fit$fitted<0] <- 0; fit$fitted[fit$fitted>1] <- 1
    plot(fit, type="o",fcol="black", plot.conf=F,main="Activated")
    lines(fitted(fit), col="blue"); lines(fit$mean, col="blue", type="o") 
    
    fit <- holt(window(customer$Deactivated), initial="simple", h=3) 
    rmse_Deactivated[i] <- as.numeric(accuracy(fit)[2])
    fit$fitted[fit$fitted<0] <- 0; fit$fitted[fit$fitted>1] <- 1
    plot(fit, type="o",fcol="black", plot.conf=F,main="Deactivated")
    lines(fitted(fit), col="blue"); lines(fit$mean, col="blue", type="o") 
    
    fit <- holt(window(customer$Retired), initial="simple", h=3) 
    rmse_Retired[i] <- as.numeric(accuracy(fit)[2])
    fit$fitted[fit$fitted<0] <- 0; fit$fitted[fit$fitted>1] <- 1
    plot(fit, type="o",fcol="black", plot.conf=F,main="Retired")
    lines(fitted(fit), col="blue"); lines(fit$mean, col="blue", type="o") 
    
    fit <- holt(window(customer$Inventory), initial="simple", h=3) 
    rmse_Inventory[i] <- as.numeric(accuracy(fit)[2])
    fit$fitted[fit$fitted<0] <- 0; fit$fitted[fit$fitted>1] <- 1
    plot(fit, type="o",fcol="black", plot.conf=F,main="Inventory")
    lines(fitted(fit), col="blue"); lines(fit$mean, col="blue", type="o") 
    
    title(main=customers[i],outer=T)
    par(mfrow=c(1,1))
}
scores_holt <- data.frame(customers,rmse_test.ready,rmse_Activation.Ready,rmse_Activated,rmse_Deactivated,rmse_Retired,rmse_Inventory)
rowMeans(scores_holt[,-1])

#Using ets automatic method
rmse_test.ready <- rep(0,length(customers))
rmse_Activation.Ready <- rep(0,length(customers))
rmse_Activated <- rep(0,length(customers))
rmse_Deactivated <- rep(0,length(customers))
rmse_Retired <- rep(0,length(customers))
rmse_Inventory <- rep(0,length(customers))
for (i in 1:length(customers))
{
    customer <- trainset[trainset$end_customer==customers[i],]
    
    par(mfrow=c(3,2))
    fit <- ets(ts(customer$Test.Ready, start=c(2011, 10), end=c(2015, 12), frequency=12))
    rmse_test.ready[i] <- as.numeric(accuracy(fit)[2])
    fit$fitted[fit$fitted<0] <- 0; fit$fitted[fit$fitted>1] <- 1
    plot(forecast(fit, 3),main="Test.Ready")
    lines(fitted(fit),col="blue")
    
    fit <- ets(ts(customer$Activation.Ready, start=c(2011, 10), end=c(2015, 12), frequency=12))
    rmse_Activation.Ready[i] <- as.numeric(accuracy(fit)[2])
    fit$fitted[fit$fitted<0] <- 0; fit$fitted[fit$fitted>1] <- 1
    plot(forecast(fit, 3),main="Activation.Ready")
    lines(fitted(fit),col="blue")
    
    fit <- ets(ts(customer$Activated, start=c(2011, 10), end=c(2015, 12), frequency=12))
    rmse_Activated[i] <- as.numeric(accuracy(fit)[2])
    fit$fitted[fit$fitted<0] <- 0; fit$fitted[fit$fitted>1] <- 1
    plot(forecast(fit, 3),main="Activated")
    lines(fitted(fit),col="blue")

    fit <- ets(ts(customer$Deactivated, start=c(2011, 10), end=c(2015, 12), frequency=12))
    rmse_Deactivated[i] <- as.numeric(accuracy(fit)[2])
    fit$fitted[fit$fitted<0] <- 0; fit$fitted[fit$fitted>1] <- 1
    plot(forecast(fit, 3),main="Deactivated")
    lines(fitted(fit),col="blue")
    
    fit <- ets(ts(customer$Retired, start=c(2011, 10), end=c(2015, 12), frequency=12))
    rmse_Retired[i] <- as.numeric(accuracy(fit)[2])
    fit$fitted[fit$fitted<0] <- 0; fit$fitted[fit$fitted>1] <- 1
    plot(forecast(fit, 3),main="Retired")
    lines(fitted(fit),col="blue")
    
    fit <- ets(ts(customer$Inventory, start=c(2011, 10), end=c(2015, 12), frequency=12))
    rmse_Inventory[i] <- as.numeric(accuracy(fit)[2])
    fit$fitted[fit$fitted<0] <- 0; fit$fitted[fit$fitted>1] <- 1
    plot(forecast(fit, 3),main="Inventory")
    lines(fitted(fit),col="blue")
    
    title(main=customers[i],outer=T)
    par(mfrow=c(1,1))
}
scores_ets <- data.frame(customers,rmse_test.ready,rmse_Activation.Ready,rmse_Activated,rmse_Deactivated,rmse_Retired,rmse_Inventory)
rowMeans(scores_ets[,-1])

#Using ARIMA automatic model with covariates for one month prediction
rmse_test.ready <- rep(0,length(customers))
rmse_Activation.Ready <- rep(0,length(customers))
rmse_Activated <- rep(0,length(customers))
rmse_Deactivated <- rep(0,length(customers))
rmse_Retired <- rep(0,length(customers))
rmse_Inventory <- rep(0,length(customers))
for (i in 1:length(customers))
{
    customer <- trainset[trainset$end_customer==customers[i],]
    valid_customer <- customer[nrow(customer),] #validation
    train_customer <- customer[-nrow(customer),] #training
    
    xreg <- cbind()
    
    par(mfrow=c(3,2))
    
    fit <- ets(ts(customer$Test.Ready, start=c(2011, 10), end=c(2015, 12), frequency=12))
    rmse_test.ready[i] <- as.numeric(accuracy(fit)[2])
    fit$fitted[fit$fitted<0] <- 0; fit$fitted[fit$fitted>1] <- 1
    plot(forecast(fit, 3),main="Test.Ready")
    lines(fitted(fit),col="blue")
    
}
scores_arima <- cbind(customers,rmse_test.ready,rmse_Activation.Ready,rmse_Activated,rmse_Deactivated,rmse_Retired,rmse_Inventory)
