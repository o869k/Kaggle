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
library(ROCR); library(FSelector)
library(glmulti); library(fmsb)
library(car); library(randomForest)
library(Hmisc)

vif_func <- function(in_frame,thresh=10,trace=T,...){
    if(class(in_frame) != 'data.frame') in_frame<-data.frame(in_frame)
    
    #get initial vif value for all comparisons of variables
    vif_init<-NULL
    var_names <- sample(names(in_frame))
    for(val in var_names){
        regressors <- var_names[-which(var_names == val)]
        form <- paste(regressors, collapse = '+')
        form_in <- formula(paste(val, '~', form))
        vif_init<-rbind(vif_init, c(val, VIF(lm(form_in, data = in_frame, ...))))
    }
    vif_max<-max(as.numeric(vif_init[,2]))
    
    if(vif_max < thresh){
        if(trace==T){ #print output of each iteration
            prmatrix(vif_init,collab=c('var','vif'),rowlab=rep('',nrow(vif_init)),quote=F)
            cat('\n')
            cat(paste('All variables have VIF < ', thresh,', max VIF ',round(vif_max,2), sep=''),'\n\n')
        }
        return(var_names)
    }
    else{
        
        in_dat<-in_frame
        
        #backwards selection of explanatory variables, stops when all VIF values are below 'thresh'
        while(vif_max >= thresh){
            
            vif_vals<-NULL
            var_names <- sample(names(in_dat))
            
            for(val in var_names){
                regressors <- var_names[-which(var_names == val)]
                form <- paste(regressors, collapse = '+')
                form_in <- formula(paste(val, '~', form))
                vif_add<-VIF(lm(form_in, data = in_dat, ...))
                vif_vals<-rbind(vif_vals,c(val,vif_add))
            }
            max_row<-which(vif_vals[,2] == max(as.numeric(vif_vals[,2])))[1]
            
            vif_max<-as.numeric(vif_vals[max_row,2])
            
            if(vif_max<thresh) break
            
            if(trace==T){ #print output of each iteration
                prmatrix(vif_vals,collab=c('var','vif'),rowlab=rep('',nrow(vif_vals)),quote=F)
                cat('\n')
                cat('removed: ',vif_vals[max_row,1],vif_max,'\n\n')
                flush.console()
            }
            
            in_dat<-in_dat[,!names(in_dat) %in% vif_vals[max_row,1]]
            
        }
        
        return(names(in_dat))
        
    }
    
}

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
d1 <- read.csv("d1.csv",stringsAsFactors = F)
d2 <- read.csv("d2.csv",stringsAsFactors = F)
d3 <- read.csv("d3.csv",stringsAsFactors = F)
d4 <- read.csv("d4.csv",stringsAsFactors = F)

#Merge weather and accidented ####
#WHEN YOU MERGE NOTE THE HOURS ZONES OF THE ACCIDENTS DATA (real new york time) and WEATHR DATA
#ALSO WHEN MERGE - THE WEATHER SHOULD FIT THE ZONE BY CLOSEST LANGITUDE AND LATITUDE haversine DISTNACE TO THE WEATHER STATION
d <- rbind(d1,d2,d3,d4)
rm(d1,d2,d3,d4)
#NYPD_Motor_Vehicle_Collisions <- NYPD_Motor_Vehicle_Collisions[NYPD_Motor_Vehicle_Collisions$BOROUGH=="MANHATTAN",]

#Count number of events
# NYPD_Motor_Vehicle_Collisions$LATITUDE <- NULL
# NYPD_Motor_Vehicle_Collisions$LONGITUDE <- NULL
tmp <- as.data.frame(table(NYPD_Motor_Vehicle_Collisions[,c("DATE","HOUR","ZIP.CODE")]))
tmp$DATE <- as.Date(as.character(tmp$DATE))
tmp$HOUR <- as.integer(as.character(tmp$HOUR))
tmp$ZIP.CODE <- as.integer(as.character(tmp$ZIP.CODE))
tmp <- merge(tmp,unique(NYPD_Motor_Vehicle_Collisions[,c("PLACE","ZIP.CODE")]),all.x=T)

NYPD_Motor_Vehicle_Collisions <- merge(tmp,d,all.x=T)
rm(tmp,d,stations)

#remove overlapping areas
NYPD_Motor_Vehicle_Collisions <- NYPD_Motor_Vehicle_Collisions[!(NYPD_Motor_Vehicle_Collisions$ZIP.CODE==11204 & NYPD_Motor_Vehicle_Collisions$PLACE=="KEWR"),]
NYPD_Motor_Vehicle_Collisions <- NYPD_Motor_Vehicle_Collisions[!(NYPD_Motor_Vehicle_Collisions$ZIP.CODE==11214 & NYPD_Motor_Vehicle_Collisions$PLACE=="KEWR"),]
NYPD_Motor_Vehicle_Collisions <- NYPD_Motor_Vehicle_Collisions[!(NYPD_Motor_Vehicle_Collisions$ZIP.CODE==11215 & NYPD_Motor_Vehicle_Collisions$PLACE=="KEWR"),]
NYPD_Motor_Vehicle_Collisions <- NYPD_Motor_Vehicle_Collisions[!(NYPD_Motor_Vehicle_Collisions$ZIP.CODE==11218 & NYPD_Motor_Vehicle_Collisions$PLACE=="KEWR"),]
NYPD_Motor_Vehicle_Collisions <- NYPD_Motor_Vehicle_Collisions[!(NYPD_Motor_Vehicle_Collisions$ZIP.CODE==11219 & NYPD_Motor_Vehicle_Collisions$PLACE=="KEWR"),]
NYPD_Motor_Vehicle_Collisions <- NYPD_Motor_Vehicle_Collisions[!(NYPD_Motor_Vehicle_Collisions$ZIP.CODE==11223 & NYPD_Motor_Vehicle_Collisions$PLACE=="KEWR"),]
NYPD_Motor_Vehicle_Collisions <- NYPD_Motor_Vehicle_Collisions[!(NYPD_Motor_Vehicle_Collisions$ZIP.CODE==11224 & NYPD_Motor_Vehicle_Collisions$PLACE=="KEWR"),]
NYPD_Motor_Vehicle_Collisions <- NYPD_Motor_Vehicle_Collisions[!(NYPD_Motor_Vehicle_Collisions$ZIP.CODE==11230 & NYPD_Motor_Vehicle_Collisions$PLACE=="KEWR"),]
NYPD_Motor_Vehicle_Collisions <- NYPD_Motor_Vehicle_Collisions[!(NYPD_Motor_Vehicle_Collisions$ZIP.CODE==11231 & NYPD_Motor_Vehicle_Collisions$PLACE=="KEWR"),]
NYPD_Motor_Vehicle_Collisions <- NYPD_Motor_Vehicle_Collisions[!(NYPD_Motor_Vehicle_Collisions$ZIP.CODE==11232 & NYPD_Motor_Vehicle_Collisions$PLACE=="KEWR"),]
NYPD_Motor_Vehicle_Collisions <- NYPD_Motor_Vehicle_Collisions[!(NYPD_Motor_Vehicle_Collisions$ZIP.CODE==11004 & NYPD_Motor_Vehicle_Collisions$PLACE=="KJFK"),]
NYPD_Motor_Vehicle_Collisions <- NYPD_Motor_Vehicle_Collisions[!(NYPD_Motor_Vehicle_Collisions$ZIP.CODE==11005 & NYPD_Motor_Vehicle_Collisions$PLACE=="KJFK"),]
NYPD_Motor_Vehicle_Collisions <- NYPD_Motor_Vehicle_Collisions[!(NYPD_Motor_Vehicle_Collisions$ZIP.CODE==11203 & NYPD_Motor_Vehicle_Collisions$PLACE=="KJFK"),]
NYPD_Motor_Vehicle_Collisions <- NYPD_Motor_Vehicle_Collisions[!(NYPD_Motor_Vehicle_Collisions$ZIP.CODE==11204 & NYPD_Motor_Vehicle_Collisions$PLACE=="KJFK"),]
NYPD_Motor_Vehicle_Collisions <- NYPD_Motor_Vehicle_Collisions[!(NYPD_Motor_Vehicle_Collisions$ZIP.CODE==11207 & NYPD_Motor_Vehicle_Collisions$PLACE=="KJFK"),]
NYPD_Motor_Vehicle_Collisions <- NYPD_Motor_Vehicle_Collisions[!(NYPD_Motor_Vehicle_Collisions$ZIP.CODE==11208 & NYPD_Motor_Vehicle_Collisions$PLACE=="KJFK"),]
NYPD_Motor_Vehicle_Collisions <- NYPD_Motor_Vehicle_Collisions[!(NYPD_Motor_Vehicle_Collisions$ZIP.CODE==11212 & NYPD_Motor_Vehicle_Collisions$PLACE=="KJFK"),]
NYPD_Motor_Vehicle_Collisions <- NYPD_Motor_Vehicle_Collisions[!(NYPD_Motor_Vehicle_Collisions$ZIP.CODE==11218 & NYPD_Motor_Vehicle_Collisions$PLACE=="KJFK"),]
NYPD_Motor_Vehicle_Collisions <- NYPD_Motor_Vehicle_Collisions[!(NYPD_Motor_Vehicle_Collisions$ZIP.CODE==11226 & NYPD_Motor_Vehicle_Collisions$PLACE=="KJFK"),]
NYPD_Motor_Vehicle_Collisions <- NYPD_Motor_Vehicle_Collisions[!(NYPD_Motor_Vehicle_Collisions$ZIP.CODE==11230 & NYPD_Motor_Vehicle_Collisions$PLACE=="KJFK"),]
NYPD_Motor_Vehicle_Collisions <- NYPD_Motor_Vehicle_Collisions[!(NYPD_Motor_Vehicle_Collisions$ZIP.CODE==11232 & NYPD_Motor_Vehicle_Collisions$PLACE=="KJFK"),]
NYPD_Motor_Vehicle_Collisions <- NYPD_Motor_Vehicle_Collisions[!(NYPD_Motor_Vehicle_Collisions$ZIP.CODE==11362 & NYPD_Motor_Vehicle_Collisions$PLACE=="KJFK"),]
NYPD_Motor_Vehicle_Collisions <- NYPD_Motor_Vehicle_Collisions[!(NYPD_Motor_Vehicle_Collisions$ZIP.CODE==11364 & NYPD_Motor_Vehicle_Collisions$PLACE=="KJFK"),]
NYPD_Motor_Vehicle_Collisions <- NYPD_Motor_Vehicle_Collisions[!(NYPD_Motor_Vehicle_Collisions$ZIP.CODE==11366 & NYPD_Motor_Vehicle_Collisions$PLACE=="KJFK"),]
NYPD_Motor_Vehicle_Collisions <- NYPD_Motor_Vehicle_Collisions[!(NYPD_Motor_Vehicle_Collisions$ZIP.CODE==11385 & NYPD_Motor_Vehicle_Collisions$PLACE=="KLGA"),]
NYPD_Motor_Vehicle_Collisions <- NYPD_Motor_Vehicle_Collisions[!(NYPD_Motor_Vehicle_Collisions$ZIP.CODE==11415 & NYPD_Motor_Vehicle_Collisions$PLACE=="KLGA"),]
NYPD_Motor_Vehicle_Collisions <- NYPD_Motor_Vehicle_Collisions[!(NYPD_Motor_Vehicle_Collisions$ZIP.CODE==11418 & NYPD_Motor_Vehicle_Collisions$PLACE=="KLGA"),]
NYPD_Motor_Vehicle_Collisions <- NYPD_Motor_Vehicle_Collisions[!(NYPD_Motor_Vehicle_Collisions$ZIP.CODE==11421 & NYPD_Motor_Vehicle_Collisions$PLACE=="KLGA"),]
NYPD_Motor_Vehicle_Collisions <- NYPD_Motor_Vehicle_Collisions[!(NYPD_Motor_Vehicle_Collisions$ZIP.CODE==11422 & NYPD_Motor_Vehicle_Collisions$PLACE=="KLGA"),]
NYPD_Motor_Vehicle_Collisions <- NYPD_Motor_Vehicle_Collisions[!(NYPD_Motor_Vehicle_Collisions$ZIP.CODE==11423 & NYPD_Motor_Vehicle_Collisions$PLACE=="KLGA"),]
NYPD_Motor_Vehicle_Collisions <- NYPD_Motor_Vehicle_Collisions[!(NYPD_Motor_Vehicle_Collisions$ZIP.CODE==11427 & NYPD_Motor_Vehicle_Collisions$PLACE=="KLGA"),]
NYPD_Motor_Vehicle_Collisions <- NYPD_Motor_Vehicle_Collisions[!(NYPD_Motor_Vehicle_Collisions$ZIP.CODE==11432 & NYPD_Motor_Vehicle_Collisions$PLACE=="KLGA"),]
NYPD_Motor_Vehicle_Collisions <- NYPD_Motor_Vehicle_Collisions[!(NYPD_Motor_Vehicle_Collisions$ZIP.CODE==11435 & NYPD_Motor_Vehicle_Collisions$PLACE=="KLGA"),]
NYPD_Motor_Vehicle_Collisions <- NYPD_Motor_Vehicle_Collisions[!(NYPD_Motor_Vehicle_Collisions$ZIP.CODE==10034 & NYPD_Motor_Vehicle_Collisions$PLACE=="KLGA"),]
NYPD_Motor_Vehicle_Collisions <- NYPD_Motor_Vehicle_Collisions[!(NYPD_Motor_Vehicle_Collisions$ZIP.CODE==10035 & NYPD_Motor_Vehicle_Collisions$PLACE=="KLGA"),]
NYPD_Motor_Vehicle_Collisions <- NYPD_Motor_Vehicle_Collisions[!(NYPD_Motor_Vehicle_Collisions$ZIP.CODE==10451 & NYPD_Motor_Vehicle_Collisions$PLACE=="KNYC"),]
NYPD_Motor_Vehicle_Collisions <- NYPD_Motor_Vehicle_Collisions[!(NYPD_Motor_Vehicle_Collisions$ZIP.CODE==10452 & NYPD_Motor_Vehicle_Collisions$PLACE=="KNYC"),]
NYPD_Motor_Vehicle_Collisions <- NYPD_Motor_Vehicle_Collisions[!(NYPD_Motor_Vehicle_Collisions$ZIP.CODE==10453 & NYPD_Motor_Vehicle_Collisions$PLACE=="KNYC"),]
NYPD_Motor_Vehicle_Collisions <- NYPD_Motor_Vehicle_Collisions[!(NYPD_Motor_Vehicle_Collisions$ZIP.CODE==10454 & NYPD_Motor_Vehicle_Collisions$PLACE=="KNYC"),]
NYPD_Motor_Vehicle_Collisions <- NYPD_Motor_Vehicle_Collisions[!(NYPD_Motor_Vehicle_Collisions$ZIP.CODE==10455 & NYPD_Motor_Vehicle_Collisions$PLACE=="KNYC"),]
NYPD_Motor_Vehicle_Collisions <- NYPD_Motor_Vehicle_Collisions[!(NYPD_Motor_Vehicle_Collisions$ZIP.CODE==10456 & NYPD_Motor_Vehicle_Collisions$PLACE=="KNYC"),]
NYPD_Motor_Vehicle_Collisions <- NYPD_Motor_Vehicle_Collisions[!(NYPD_Motor_Vehicle_Collisions$ZIP.CODE==11101 & NYPD_Motor_Vehicle_Collisions$PLACE=="KNYC"),]
NYPD_Motor_Vehicle_Collisions <- NYPD_Motor_Vehicle_Collisions[!(NYPD_Motor_Vehicle_Collisions$ZIP.CODE==11102 & NYPD_Motor_Vehicle_Collisions$PLACE=="KNYC"),]
NYPD_Motor_Vehicle_Collisions <- NYPD_Motor_Vehicle_Collisions[!(NYPD_Motor_Vehicle_Collisions$ZIP.CODE==11104 & NYPD_Motor_Vehicle_Collisions$PLACE=="KNYC"),]
NYPD_Motor_Vehicle_Collisions <- NYPD_Motor_Vehicle_Collisions[!(NYPD_Motor_Vehicle_Collisions$ZIP.CODE==11106 & NYPD_Motor_Vehicle_Collisions$PLACE=="KNYC"),]
NYPD_Motor_Vehicle_Collisions <- NYPD_Motor_Vehicle_Collisions[!(NYPD_Motor_Vehicle_Collisions$ZIP.CODE==11206 & NYPD_Motor_Vehicle_Collisions$PLACE=="KNYC"),]
NYPD_Motor_Vehicle_Collisions <- NYPD_Motor_Vehicle_Collisions[!(NYPD_Motor_Vehicle_Collisions$ZIP.CODE==11207 & NYPD_Motor_Vehicle_Collisions$PLACE=="KNYC"),]
NYPD_Motor_Vehicle_Collisions <- NYPD_Motor_Vehicle_Collisions[!(NYPD_Motor_Vehicle_Collisions$ZIP.CODE==11211 & NYPD_Motor_Vehicle_Collisions$PLACE=="KNYC"),]
NYPD_Motor_Vehicle_Collisions <- NYPD_Motor_Vehicle_Collisions[!(NYPD_Motor_Vehicle_Collisions$ZIP.CODE==11212 & NYPD_Motor_Vehicle_Collisions$PLACE=="KNYC"),]
NYPD_Motor_Vehicle_Collisions <- NYPD_Motor_Vehicle_Collisions[!(NYPD_Motor_Vehicle_Collisions$ZIP.CODE==11213 & NYPD_Motor_Vehicle_Collisions$PLACE=="KNYC"),]
NYPD_Motor_Vehicle_Collisions <- NYPD_Motor_Vehicle_Collisions[!(NYPD_Motor_Vehicle_Collisions$ZIP.CODE==11221 & NYPD_Motor_Vehicle_Collisions$PLACE=="KNYC"),]
NYPD_Motor_Vehicle_Collisions <- NYPD_Motor_Vehicle_Collisions[!(NYPD_Motor_Vehicle_Collisions$ZIP.CODE==11233 & NYPD_Motor_Vehicle_Collisions$PLACE=="KNYC"),]
NYPD_Motor_Vehicle_Collisions <- NYPD_Motor_Vehicle_Collisions[!(NYPD_Motor_Vehicle_Collisions$ZIP.CODE==11237 & NYPD_Motor_Vehicle_Collisions$PLACE=="KNYC"),]
NYPD_Motor_Vehicle_Collisions <- NYPD_Motor_Vehicle_Collisions[!(NYPD_Motor_Vehicle_Collisions$ZIP.CODE==11378 & NYPD_Motor_Vehicle_Collisions$PLACE=="KNYC"),]

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
NYPD_Motor_Vehicle_Collisions$Conditions <- factor(NYPD_Motor_Vehicle_Collisions$Conditions)
NYPD_Motor_Vehicle_Collisions$Events[is.na(NYPD_Motor_Vehicle_Collisions$Events) & NYPD_Motor_Vehicle_Collisions$Conditions=="Clear"] <- ""
NYPD_Motor_Vehicle_Collisions$Events[is.na(NYPD_Motor_Vehicle_Collisions$Events) & NYPD_Motor_Vehicle_Collisions$Conditions=="Mostly Cloudy"] <- ""
NYPD_Motor_Vehicle_Collisions$Events[is.na(NYPD_Motor_Vehicle_Collisions$Events) & NYPD_Motor_Vehicle_Collisions$Conditions=="Overcast"] <- ""
NYPD_Motor_Vehicle_Collisions$Events[is.na(NYPD_Motor_Vehicle_Collisions$Events) & NYPD_Motor_Vehicle_Collisions$Conditions=="Scattered Clouds"] <- ""
NYPD_Motor_Vehicle_Collisions$Events[is.na(NYPD_Motor_Vehicle_Collisions$Events) & NYPD_Motor_Vehicle_Collisions$Conditions=="Partly Cloudy"] <- ""
NYPD_Motor_Vehicle_Collisions$Events[is.na(NYPD_Motor_Vehicle_Collisions$Events) & NYPD_Motor_Vehicle_Collisions$Conditions=="Haze"] <- ""
NYPD_Motor_Vehicle_Collisions$Events[is.na(NYPD_Motor_Vehicle_Collisions$Events) & NYPD_Motor_Vehicle_Collisions$Conditions=="Unknown"] <- ""
NYPD_Motor_Vehicle_Collisions$Events[is.na(NYPD_Motor_Vehicle_Collisions$Events) & NYPD_Motor_Vehicle_Collisions$Conditions=="Light Drizzle"] <- ""
NYPD_Motor_Vehicle_Collisions$Events[is.na(NYPD_Motor_Vehicle_Collisions$Events) & NYPD_Motor_Vehicle_Collisions$Conditions=="Mist"] <- ""
NYPD_Motor_Vehicle_Collisions$Events[is.na(NYPD_Motor_Vehicle_Collisions$Events) & NYPD_Motor_Vehicle_Collisions$Conditions=="Squalls"] <- ""
NYPD_Motor_Vehicle_Collisions$Events <- factor(NYPD_Motor_Vehicle_Collisions$Events)
NYPD_Motor_Vehicle_Collisions$Precipitationmm[is.na(NYPD_Motor_Vehicle_Collisions$Precipitationmm) & NYPD_Motor_Vehicle_Collisions$Events==""] <- 0

NYPD_Motor_Vehicle_Collisions <- NYPD_Motor_Vehicle_Collisions[!is.na(NYPD_Motor_Vehicle_Collisions$VisibilityKm),] #remove missing values!
NYPD_Motor_Vehicle_Collisions <- NYPD_Motor_Vehicle_Collisions[!is.na(NYPD_Motor_Vehicle_Collisions$Humidity),] #remove missing values!
NYPD_Motor_Vehicle_Collisions <- NYPD_Motor_Vehicle_Collisions[!is.na(NYPD_Motor_Vehicle_Collisions$Events),] #remove missing values!
NYPD_Motor_Vehicle_Collisions <- NYPD_Motor_Vehicle_Collisions[!is.na(NYPD_Motor_Vehicle_Collisions$Precipitationmm),] #remove missing values!

# NYPD_Motor_Vehicle_Collisions$DATE <- as.Date(as.character(NYPD_Motor_Vehicle_Collisions$DATE))
# NYPD_Motor_Vehicle_Collisions$HOUR <- as.numeric(as.character(NYPD_Motor_Vehicle_Collisions$HOUR))

NYPD_Motor_Vehicle_Collisions$Weekday <- factor(weekdays(NYPD_Motor_Vehicle_Collisions$DATE))
NYPD_Motor_Vehicle_Collisions$Weekend <- factor(isWeekend(NYPD_Motor_Vehicle_Collisions$DATE))
NYPD_Motor_Vehicle_Collisions$Holiday <- factor(isHoliday(timeDate(NYPD_Motor_Vehicle_Collisions$DATE),holidayNYSE()))
NYPD_Motor_Vehicle_Collisions$Bizday <- factor(isBizday(timeDate(NYPD_Motor_Vehicle_Collisions$DATE),holidayNYSE()))
NYPD_Motor_Vehicle_Collisions$Month <- factor(month(NYPD_Motor_Vehicle_Collisions$DATE))
NYPD_Motor_Vehicle_Collisions$DATE <- NULL
NYPD_Motor_Vehicle_Collisions$Freq[NYPD_Motor_Vehicle_Collisions$Freq>1] <- 1 #we do classification onbly
NYPD_Motor_Vehicle_Collisions$Freq <- factor(NYPD_Motor_Vehicle_Collisions$Freq)
NYPD_Motor_Vehicle_Collisions$HOUR <- factor(NYPD_Motor_Vehicle_Collisions$HOUR)

#Build a Random forest model ####
#a model per ZOne 
N <- length(unique(NYPD_Motor_Vehicle_Collisions$ZIP.CODE))
auc <- rep(0,N)
results <- NULL
for (i in 137:N)
{
    ZIP <- sort(unique(NYPD_Motor_Vehicle_Collisions$ZIP.CODE))[i]
    cat(ZIP,"\n")
    dataset <- NYPD_Motor_Vehicle_Collisions[NYPD_Motor_Vehicle_Collisions$ZIP.CODE==ZIP,-2]
    dataset$Wind_SpeedKm_h <- na.locf(dataset$Wind_SpeedKm_h) #fill in winspeed with last value
    ratio <- sum(dataset$Freq==1)/nrow(dataset)
    
    model <- randomForest(x=dataset[,-2],
                          y=dataset$Freq,
                          ntree = 100,
                          do.trace=T,
                          cutoff = c(1-ratio,ratio),
                          importance = F,
                          keep.forest = T)
    
    p <- as.vector(model$votes[,2])
    pr <- prediction(p,dataset$Freq)
    prf <- performance(pr,measure = "tpr", x.measure = "fpr")
    auc <- round(performance(pr, measure = "auc")@y.values[[1]],2)
    #plot(prf,main=paste0("ROC Curve, AUC: ",auc),col=2,lwd=2,print.thres="best",print.thres.best.method="closest.topleft")
    #abline(a=0,b=1,lwd=2,lty=2,col="gray")
    
    result.coords <- pROC::coords(pROC::roc(dataset$Freq,p),"best",best.method="closest.topleft",ret=c("threshold","accuracy","specificity","sensitivity"))
    print(result.coords)
    
    results <- rbind(results,c(ZIP=ZIP,auc=auc,result.coords,kappa=classAgreement(table(p>result.coords[1],dataset$Freq))$kappa))

    saveRDS(model,paste0("model_",ZIP,".RDS"))
}
write.csv(results,"results.csv",row.names = F)
