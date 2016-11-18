#Initializaiotn ####
rm(list=ls()) # clear workspace
mainDir <- "/Accidents/"
setwd(mainDir)
set.seed(1)
options(java.parameters = "-Xmx8000m")
library(doParallel); library(RSocrata)
library(e1071); library(caret) 
library(ggplot2); library(Hmisc)
library(zoo); library(xlsx)
library(lubridate); library(nlme)
library(car); library(MASS)
library(weatherData); library(rjson)
library(plyr); library(lubridate)
library(timeDate); library(dplyr)
library(fossil); library(pROC)
library(ROCR);

#Read Data ####
#stations data
stations <- read.csv("nyc.csv",stringsAsFactors = F)

#collisions data
NYPD_Motor_Vehicle_Collisions <- read.csv("NYPD_Motor_Vehicle_Collisions.csv")
NYPD_Motor_Vehicle_Collisions$DATE <- as.Date(as.character(NYPD_Motor_Vehicle_Collisions$ï..DATE),format="%m/%d/%Y")
NYPD_Motor_Vehicle_Collisions$HOUR <- hour(strptime(as.character(NYPD_Motor_Vehicle_Collisions$TIME),"%H:%M"))
NYPD_Motor_Vehicle_Collisions$ï..DATE <- NULL
NYPD_Motor_Vehicle_Collisions$TIME <- NULL
NYPD_Motor_Vehicle_Collisions <- NYPD_Motor_Vehicle_Collisions[,c(1,2,3,4,28,29)]
NYPD_Motor_Vehicle_Collisions <- NYPD_Motor_Vehicle_Collisions[order(NYPD_Motor_Vehicle_Collisions$DATE,NYPD_Motor_Vehicle_Collisions$HOUR,NYPD_Motor_Vehicle_Collisions$ZIP.CODE),]
NYPD_Motor_Vehicle_Collisions <- NYPD_Motor_Vehicle_Collisions[!is.na(NYPD_Motor_Vehicle_Collisions$ZIP.CODE),]
first_date <- NYPD_Motor_Vehicle_Collisions$DATE[1]
last_date <- tail(NYPD_Motor_Vehicle_Collisions$DATE,1)
NYPD_Motor_Vehicle_Collisions <- NYPD_Motor_Vehicle_Collisions[!is.na(NYPD_Motor_Vehicle_Collisions$LONGITUDE),]
NYPD_Motor_Vehicle_Collisions$PLACE <- apply(NYPD_Motor_Vehicle_Collisions[,c("LATITUDE","LONGITUDE")],1,function(x) {stations$ICAO[which.min(deg.dist(x[2],x[1],stations$LONG,stations$LAT))]})

#get Weather From NOAA
#JFK
d1 <- getWeatherForDate("KJFK", start_date=first_date,end_date = last_date,opt_detailed = TRUE,opt_all_columns = TRUE)
d1$DATE <- as.Date(d1$DateUTC, tz = "EST")
d1$HOUR <- hour(d1$Time)
d1$Time <- NULL
d1$TimeEDT <- NULL
d1$TimeEST <- NULL
d1$DateUTC <- NULL
d1$Sea_Level_PressurehPa <- NULL
d1$Dew_PointC <- NULL
d1$WindDirDegrees <- NULL
d1$Gust_SpeedKm_h <- NULL
d1 <- d1[!is.na(d1$DATE),]
# d1$DATE[d1$HOUR==0] <- d1$DATE[d1$HOUR==0]+1
# d1$DATE[d1$HOUR==1] <- d1$DATE[d1$HOUR==1]+1
# d1$DATE[d1$HOUR==2] <- d1$DATE[d1$HOUR==2]+1
d1$PLACE <- "KJFK"

#LA Guardia
d2 <- getWeatherForDate("KLGA", start_date=first_date,end_date = last_date,opt_detailed = TRUE,opt_all_columns = TRUE)
d2$DATE <- as.Date(d2$DateUTC, tz = "EST")
d2$HOUR <- hour(d2$Time)
d2$Time <- NULL
d2$TimeEDT <- NULL
d2$TimeEST <- NULL
d2$DateUTC <- NULL
d2$Sea_Level_PressurehPa <- NULL
d2$Dew_PointC <- NULL
d2$WindDirDegrees <- NULL
d2$Gust_SpeedKm_h <- NULL
d2 <- d2[!is.na(d2$DATE),]
# d2$DATE[d2$HOUR==0] <- d2$DATE[d2$HOUR==0]+1
# d2$DATE[d2$HOUR==1] <- d2$DATE[d2$HOUR==1]+1
# d2$DATE[d2$HOUR==2] <- d2$DATE[d2$HOUR==2]+1
d2$PLACE <- "KLGA"

#Central Park
d3 <- getWeatherForDate("KNYC", start_date=first_date,end_date = last_date,opt_detailed = TRUE,opt_all_columns = TRUE)
d3$DATE <- as.Date(d3$DateUTC, tz = "EST")
d3$HOUR <- hour(d3$Time)
d3$Time <- NULL
d3$TimeEDT <- NULL
d3$TimeEST <- NULL
d3$DateUTC <- NULL
d3$Sea_Level_PressurehPa <- NULL
d3$Dew_PointC <- NULL
d3$WindDirDegrees <- NULL
d3$Gust_SpeedKm_h <- NULL
d3 <- d3[!is.na(d3$DATE),]
# d3$DATE[d3$HOUR==0] <- d3$DATE[d3$HOUR==0]+1
# d3$DATE[d3$HOUR==1] <- d3$DATE[d3$HOUR==1]+1
# d3$DATE[d3$HOUR==2] <- d3$DATE[d3$HOUR==2]+1
d3$PLACE <- "KNYC"

#NEWARK
d4 <- getWeatherForDate("KEWR", start_date=first_date,end_date = last_date,opt_detailed = TRUE,opt_all_columns = TRUE)
d4$DATE <- as.Date(d4$DateUTC, tz = "EST")
d4$HOUR <- hour(d4$Time)
d4$Time <- NULL
d4$TimeEDT <- NULL
d4$TimeEST <- NULL
d4$DateUTC <- NULL
d4$Sea_Level_PressurehPa <- NULL
d4$Dew_PointC <- NULL
d4$WindDirDegrees <- NULL
d4$Gust_SpeedKm_h <- NULL
d4 <- d4[!is.na(d4$DATE),]
# d4$DATE[d4$HOUR==0] <- d4$DATE[d4$HOUR==0]+1
# d4$DATE[d4$HOUR==1] <- d4$DATE[d4$HOUR==1]+1
# d4$DATE[d4$HOUR==2] <- d4$DATE[d4$HOUR==2]+1
d4$PLACE <- "KEWR"

write.csv(d1,"d1.csv",row.names=F)
write.csv(d2,"d2.csv",row.names=F)
write.csv(d3,"d3.csv",row.names=F)
write.csv(d4,"d4.csv",row.names=F)

d1 <- read.csv("d1.csv",stringsAsFactors = F)
d2 <- read.csv("d2.csv",stringsAsFactors = F)
d3 <- read.csv("d3.csv",stringsAsFactors = F)
d4 <- read.csv("d4.csv",stringsAsFactors = F)

#Merge weather and accidented ####
#WHEN YOU MERGE NOTE THE HOURS ZONES OF THE ACCIDENTS DATA (real new york time) and WEATHR DATA
#ALSO WHEN MERGE - THE WEATHER SHOULD FIT THE ZONE BY CLOSEST LANGITUDE AND LATITUDE haversine DISTNACE TO THE WEATHER STATION
d <- rbind(d1,d2,d3,d4)
rm(d1,d2,d3,d4)

#Count number of events
NYPD_Motor_Vehicle_Collisions$LATITUDE <- NULL
NYPD_Motor_Vehicle_Collisions$LONGITUDE <- NULL
tmp <- as.data.frame(table(NYPD_Motor_Vehicle_Collisions[,c("DATE","HOUR","ZIP.CODE")]))
tmp <- merge(tmp,unique(NYPD_Motor_Vehicle_Collisions[,c("BOROUGH","PLACE","ZIP.CODE")]),all.x=T)

NYPD_Motor_Vehicle_Collisions <- merge(tmp,d)
rm(tmp,d,stations)

NYPD_Motor_Vehicle_Collisions$PLACE <- NULL
NYPD_Motor_Vehicle_Collisions$Humidity <- as.numeric(NYPD_Motor_Vehicle_Collisions$Humidity)
NYPD_Motor_Vehicle_Collisions$TemperatureC[NYPD_Motor_Vehicle_Collisions$TemperatureC=="-9999"] <- NA
NYPD_Motor_Vehicle_Collisions$VisibilityKm[NYPD_Motor_Vehicle_Collisions$VisibilityKm=="-9999"] <- NA
NYPD_Motor_Vehicle_Collisions$Wind_SpeedKm_h[NYPD_Motor_Vehicle_Collisions$Wind_SpeedKm_h=="Calm"] <- 0
NYPD_Motor_Vehicle_Collisions$Wind_SpeedKm_h[NYPD_Motor_Vehicle_Collisions$Wind_SpeedKm_h=="-9999"] <- NA
NYPD_Motor_Vehicle_Collisions$Wind_SpeedKm_h[NYPD_Motor_Vehicle_Collisions$Wind_SpeedKm_h=="-9999.0"] <- NA
NYPD_Motor_Vehicle_Collisions$Wind_SpeedKm_h <- as.numeric(NYPD_Motor_Vehicle_Collisions$Wind_SpeedKm_h)
NYPD_Motor_Vehicle_Collisions$Wind_Direction <- factor(NYPD_Motor_Vehicle_Collisions$Wind_Direction)
NYPD_Motor_Vehicle_Collisions$Precipitationmm[NYPD_Motor_Vehicle_Collisions$Precipitationmm=="N/A"] <- NA
NYPD_Motor_Vehicle_Collisions$Precipitationmm <- as.numeric(NYPD_Motor_Vehicle_Collisions$Precipitationmm)
NYPD_Motor_Vehicle_Collisions$Conditions <- NULL
NYPD_Motor_Vehicle_Collisions$Events <- factor(NYPD_Motor_Vehicle_Collisions$Events)

NYPD_Motor_Vehicle_Collisions <- NYPD_Motor_Vehicle_Collisions[!is.na(NYPD_Motor_Vehicle_Collisions$Wind_SpeedKm_h),] #remove missing values!
NYPD_Motor_Vehicle_Collisions <- NYPD_Motor_Vehicle_Collisions[!is.na(NYPD_Motor_Vehicle_Collisions$VisibilityKm),] #remove missing values!
NYPD_Motor_Vehicle_Collisions <- NYPD_Motor_Vehicle_Collisions[!is.na(NYPD_Motor_Vehicle_Collisions$Humidity),] #remove missing values!
NYPD_Motor_Vehicle_Collisions <- NYPD_Motor_Vehicle_Collisions[!is.na(NYPD_Motor_Vehicle_Collisions$Precipitationmm),] #remove missing values!
NYPD_Motor_Vehicle_Collisions <- NYPD_Motor_Vehicle_Collisions[!is.na(NYPD_Motor_Vehicle_Collisions$Events),] #remove missing values!

tmp$DATE <- as.Date(as.character(tmp$DATE))
tmp$HOUR <- as.numeric(as.character(tmp$HOUR))

tmp$Weekday <- factor(weekdays(tmp$DATE))
tmp$Weekend <- factor(isWeekend(tmp$DATE))
tmp$Holiday <- factor(isHoliday(timeDate(tmp$DATE),holidayNYSE()))
tmp$Bizday <- factor(isBizday(timeDate(tmp$DATE),holidayNYSE()))
tmp$Month <- factor(month(tmp$DATE))
tmp$DATE <- NULL
tmp$Freq[tmp$Freq>1] <- 1
tmp$Freq <- factor(tmp$Freq)

#Build a poisson model ####
fit <- glm(Freq~.,data=tmp[,-c(2)],family=binomial())
summary(fit) # display results
confint(fit) # 95% CI for the coefficients
classAgreement(table(fit$fitted.values>0.1,tmp$Freq))
anova(fit, test="Chisq")

p <- predict(fit,newdata=tmp,type="response")
pr <- prediction(p,tmp$Freq)
prf <- performance(pr,measure = "tpr", x.measure = "fpr")
auc <- round(performance(pr, measure = "auc")@y.values[[1]],2)
plot(prf,main=paste0("ROC Curve, AUC: ",auc))
