#Initialization
{
    sessionInfo() #system performance
    gc() #clear unsued memory
    rm(list=ls()) # clear workspace
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

#Read Data ####
dataset <- read.csv("campus_dataset_total_no_weather.csv",stringsAsFactors = F)
dataset <- dataset[dataset$organization_id==74990,] #select only Arizona for the moment
dataset$organization_id <- NULL
dataset$date <- as.Date(dataset$date)
dataset$hour <- as.factor(dataset$hour)
dataset$weekday <- as.factor(dataset$weekday)
dataset$month <- as.factor(dataset$month)
#dataset <- dataset[dataset$weekday=="Tuesday",]

plot(1:nrow(dataset),dataset$orders,type="l")
lines(1:nrow(dataset),dataset$semester,col="red")
trainset <- dataset[dataset$date<as.Date("2016-09-01"),]
trainset$date <- NULL
validset <- dataset[dataset$date>=as.Date("2016-09-01") & dataset$date<as.Date("2016-09-13"),]
validset$date <- NULL

#Kepp only relevant features
trainset <- trainset[,c("promo","orders","month","year","semester","day_in_semester","day_in_month","day_in_year","new_semester_marker","semester_count","median_orders_this_time_last_week","median_orders_this_time_this_day_last_week","orders_this_time_this_day_last_semester","median_orders_this_time_this_day_last_week_offset")]

#Use ARIMAX ####
# Create matrix of numeric predictors (covariates)
xreg <- NULL
for (i in 1:ncol(trainset))
{
    if (names(trainset)[i]!="orders")
    {
        if (class(trainset[,i]) =="factor")
        {
            tmp <- model.matrix(~as.factor(trainset[,i]))
            tmp <- tmp[,-c(which(colnames(tmp)=="(Intercept)"))]
            colnames(tmp) <- gsub("as\\.factor\\(trainset\\[\\, i\\]\\)",paste0(names(trainset)[i],"_"),colnames(tmp))
            xreg <- cbind(xreg,tmp)
        } else {
            xreg <- cbind(xreg,trainset[,i])
            colnames(xreg)[ncol(xreg)] <- names(trainset)[i] 
        }
    }
}

zd <- fourier(ts(trainset$orders,frequency=48), K=1) #daily half-hourly frequency seasonality
zw <- fourier(ts(trainset$orders,frequency=336), K=1) #weekly half-hourly frequency seasonality
y <- ts(trainset$orders, frequency=336) #choose the frequency wisely (see: http://robjhyndman.com/hyndsight/seasonal-periods/)

# plot(1:nrow(trainset),y,type="l")
# lines(1:nrow(trainset),as.numeric(trainset$semester),col="red")
# lines(1:nrow(trainset),as.numeric(zd[,1]),col="blue")

model <- auto.arima(y,trace=TRUE,stepwise=FALSE,approximation=TRUE,parallel=TRUE,num.cores=12,seasonal=FALSE)
#model <- Arima(y,order=c(3,0,1),seasonal=c(0,0,0))

predicted_train <- fitted(model)
predicted_train[predicted_train<0] <- 0
plot(1:nrow(trainset),y,type="l")
lines(1:nrow(trainset),predicted_train,col="red")
lines(1:nrow(trainset),prediction(model),col="blue")
plot(forecast(model))
RMPSE(predicted_train,y)
RMSE(predicted_train,y)

#Make forecast
zdv <- fourier(ts(validset$orders,frequency=48),K=1) #daily half-hourly frequency seasonality
zwv <- fourier(ts(validset$orders,frequency=336),K=1) #weekly half-hourly frequency seasonality
yf <- ts(validset$orders,frequency=336) #choose the frequency wisely (see: http://robjhyndman.com/hyndsight/seasonal-periods/)

fc <- forecast(model,xreg=cbind(zdv,zwv,validset$median_orders_this_time_this_day_last_2weeks))
plot(1:nrow(validset),yf,type="l")
lines(1:nrow(validset),fc$mean,col="blue")
RMPSE(fc$mean,yf)
RMSE(fc$mean,yf)


model <- auto.arima(y,trace=TRUE,stepwise=FALSE,approximation=TRUE,parallel=TRUE,num.cores=12,seasonal=FALSE)
tsdisplay(residuals(model))


plot(euretail, ylab="Retail index", xlab="Year")
fit <- auto.arima(euretail)
tsdisplay(residuals(fit))
res <- residuals(fit)
Box.test(res, lag=16, fitdf=4, type="Ljung")
plot(forecast(fit, h=12))

fit <- Arima(h02, order=c(3,0,1), seasonal=c(0,1,2), lambda=0)
plot(forecast(fit), ylab="H02 sales (million scripts)", xlab="Year")

plot(forecast(model), ylab="H02 sales (million scripts)", xlab="Year")
