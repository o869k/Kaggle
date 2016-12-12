#Initializaiotn ####
rm(list=ls()) # clear workspace
setwd(mainDir)
set.seed(1)
options(java.parameters = "-Xmx8000m")
library(forecast); library(xts)
library(caret); library(randomForest)
library(lubridate); library(reshape2)
library(foreach); library(doParallel)
library(fpp); library(UsingR)
library(kernlab); library(e1071)
library(Hmisc); library(devtools)
library(xgboost); library(quantmod)
library(zoo); library(timeSeries)
library(FSelector); library(ggplot2) 
library(dplyr); library(plyr)

#read data ####
campuses <- list(campuses1,campuses2,campuses3)
for (i in 1:length(campuses))
{
    campuses[[i]]$Fall_sem_start <- as.Date(campuses[[i]]$Fall_sem_start,format = "%d-%m-%y")
    campuses[[i]]$Fall_sem_middle_exams_start <- as.Date(campuses[[i]]$Fall_sem_middle_exams_start,format = "%d-%m-%y")
    campuses[[i]]$Fall_sem_middle_exams_end <- as.Date(campuses[[i]]$Fall_sem_middle_exams_end,format = "%d-%m-%y")
    campuses[[i]]$Fall_sem_end <- as.Date(campuses[[i]]$Fall_sem_end,format = "%d-%m-%y")
    campuses[[i]]$Spring_sem_start <- as.Date(campuses[[i]]$Spring_sem_start,format = "%d-%m-%y")
    campuses[[i]]$Spring_sem_middle_exams_start <- as.Date(campuses[[i]]$Spring_sem_middle_exams_start,format = "%d-%m-%y")
    campuses[[i]]$Spring_sem_middle_exams_end <- as.Date(campuses[[i]]$Spring_sem_middle_exams_end,format = "%d-%m-%y")
    campuses[[i]]$Spring_sem_end <- as.Date(campuses[[i]]$Spring_sem_end,format = "%d-%m-%y")
    campuses[[i]]$start_date <- as.Date(campuses[[i]]$start_date,format = "%d-%m-%y")
    campuses[[i]]$end_date <- as.Date(campuses[[i]]$end_date,format = "%d-%m-%y")
}
rm(campuses1,campuses2,campuses3)
campuses_list <- campuses_end$organization_id[as.Date(campuses_end$merchant_creation_time)>="2016-08-31"] #those are the campuses to focus on
names(promos_data)[1] <- "symbol"
promos_data <- merge(promos_data,campuses[[3]][,c("symbol","ID")])
promos_data$Datetime <- mdy_hm(promos_data$Datetime)
promos_data$date <- as.Date(promos_data$Datetime)
minute(promos_data$Datetime) <- floor(minute(promos_data$Datetime)/30)*30 #trunc to nearest half an hour
promos_data$hour <- paste(sprintf("%02d",hour(promos_data$Datetime)),sprintf("%02d",minute(promos_data$Datetime)),"00",sep = ":")
promos_data$promo <- 1

#For each campus ####
campus_dataset_total <- NULL
dataset_tot <- NULL
dataset_tot_offset <- NULL
dataset_tot_offset_log <- NULL
dataset_tot_log <- NULL
for (organization_id in campuses_list)
{
    cat(organization_id,"\n")
    campus_dataset <- NULL
    
    #Take campus data
    dataset <- dataset_all[dataset_all$organization_id==organization_id,]
    dataset <- dataset[,c("id","merchant_creation_time","organization_id")]
    dataset$merchant_creation_time <- as.POSIXct(dataset$merchant_creation_time)
    dataset$date <- as.Date(dataset$merchant_creation_time)
    minute(dataset$merchant_creation_time) <- floor(minute(dataset$merchant_creation_time)/30)*30 #trunc to nearest half an hour
    dataset$hour <- paste(sprintf("%02d",hour(dataset$merchant_creation_time)),sprintf("%02d",minute(dataset$merchant_creation_time)),"00",sep = ":")
    
    #Semseter info (individual per campus)
    semester_dates <- data.frame(date=seq(min(dataset$date)-90,max(dataset$date)+14,by="days")) #additional 14 days for predictions, and previous semester data
    semester_dates$date <- as.Date(semester_dates$date)
    semester_dates$weekday <- weekdays(semester_dates$date)
    semester_dates$month <- months(semester_dates$date)
    semester_dates$year <- year(semester_dates$date)
    semester_dates$weekend <- !isWeekday(semester_dates$date)
    semester_dates$workday <- isBizday(timeDate(semester_dates$date))
    semester_dates$holiday <- isHoliday(timeDate(semester_dates$date))
    semester_dates$semester <- FALSE #is there a school day
    semester_dates$day_in_semester <- 0
    for (i in 1:length(campuses))
    {
        if (organization_id %in% campuses[[i]]$ID)
        {
            semester_dates$semester[semester_dates$date>=campuses[[i]]$Fall_sem_start[campuses[[i]]$ID==organization_id] & semester_dates$date<=campuses[[i]]$Fall_sem_end[campuses[[i]]$ID==organization_id]] <- TRUE
            semester_dates$semester[semester_dates$date>=campuses[[i]]$Spring_sem_start[campuses[[i]]$ID==organization_id] & semester_dates$date<=campuses[[i]]$Spring_sem_end[campuses[[i]]$ID==organization_id]] <- TRUE
            semester_dates$semester[semester_dates$date>=campuses[[i]]$Fall_sem_middle_exams_start[campuses[[i]]$ID==organization_id] & semester_dates$date<=campuses[[i]]$Fall_sem_middle_exams_end[campuses[[i]]$ID==organization_id]] <- FALSE
            semester_dates$semester[semester_dates$date>=campuses[[i]]$Spring_sem_middle_exams_start[campuses[[i]]$ID==organization_id] & semester_dates$date<=campuses[[i]]$Spring_sem_middle_exams_end[campuses[[i]]$ID==organization_id]] <- FALSE
            if (!is.na(campuses[[i]]$Fall_sem_start[campuses[[i]]$ID==organization_id])) semester_dates$day_in_semester[semester_dates$date>=campuses[[i]]$Fall_sem_start[campuses[[i]]$ID==organization_id] & semester_dates$date<=campuses[[i]]$Fall_sem_end[campuses[[i]]$ID==organization_id]] <- c(1:sum(semester_dates$date>=campuses[[i]]$Fall_sem_start[campuses[[i]]$ID==organization_id] & semester_dates$date<=campuses[[i]]$Fall_sem_end[campuses[[i]]$ID==organization_id]))
            if (!is.na(campuses[[i]]$Spring_sem_start[campuses[[i]]$ID==organization_id])) semester_dates$day_in_semester[semester_dates$date>=campuses[[i]]$Spring_sem_start[campuses[[i]]$ID==organization_id] & semester_dates$date<=campuses[[i]]$Spring_sem_end[campuses[[i]]$ID==organization_id]] <- c(1:sum(semester_dates$date>=campuses[[i]]$Spring_sem_start[campuses[[i]]$ID==organization_id] & semester_dates$date<=campuses[[i]]$Spring_sem_end[campuses[[i]]$ID==organization_id]))
        }
    }
    semester_dates$day_in_month <- mday(semester_dates$date) #month trend
    semester_dates$day_in_year <- yday(semester_dates$date) #year trend
    semester_dates <- semester_dates[semester_dates$date>=min(dataset$date),] #only relevant dates
    semester_dates$day_in_year_log <- log(semester_dates$day_in_year) #add log term
    semester_dates$day_in_semester_log <- log(semester_dates$day_in_semester) #add log term
    semester_dates$day_in_semester_log[semester_dates$day_in_semester_log==-Inf] <- (-1)
    semester_dates$new_semester_marker <- 0 #add semester start boolean indicator (week before and 2 weeks after the semster starts)
    new_semesters_markers <- which(semester_dates$day_in_semester==1) 
    for (i in 1:length(new_semesters_markers))
    {
        semester_dates$new_semester_marker[(new_semesters_markers[i]):(new_semesters_markers[i]+13)] <- 1 #marker for happening of begining of semster
    }
    #Add yearly Fourier Terms for seasonality
    fourier_terms <- 5; Mcol <- ncol(semester_dates)
    semester_dates <- cbind(semester_dates,as.data.frame(fourier(ts(seq_along(seq(min(semester_dates$date),max(semester_dates$date),by="day")),frequency=365.25),K=fourier_terms)))
    colnames(semester_dates)[(Mcol+1):(Mcol+2*fourier_terms)] <- paste("Fourier_year",seq_len(fourier_terms*2),sep = '')
    
    #Combine with order and impute missing date hour values
    dates_hours <- data.frame(hour=seq(as.POSIXct(paste0(min(dataset$date)," 00:00"),tz = "UTC"),as.POSIXct(paste0(max(semester_dates$date)," 23:30"),tz = "UTC"),by = 1800))
    dates_hours$date <- as.Date(dates_hours$hour)
    dates_hours$hour <- strftime(dates_hours$hour,format="%H:%M:%S",tz = "UTC")
    #Add hourly Fourier Terms for seasonality
    fourier_terms <- 5; 
    hourly_Fourier_Terms <- data.frame(hour=unique(dates_hours$hour),fourier(ts(seq_along(unique(dates_hours$hour)),frequency=48),K=fourier_terms))
    colnames(hourly_Fourier_Terms)[2:(1+2*fourier_terms)] <- paste("Fourier_hour",seq_len(fourier_terms*2),sep = '')
    dates_hours <- merge(dates_hours,hourly_Fourier_Terms)
    orders <- dataset[dataset$organization_id==organization_id,]
    orders <- merge(orders,promos_data[promos_data$ID==organization_id,c("promo","date","hour")],all.x=T) #Add promo
    orders$promo[is.na(orders$promo)] <- 0
    if (nrow(orders)>0)
    {
        orders <- aggregate(id~date+hour+organization_id+promo,data = orders,FUN = length)
        names(orders)[names(orders)=="id"] <- "orders"
        campus_dataset <- merge(dates_hours,orders,all = T)
        campus_dataset$organization_id[is.na(campus_dataset$organization_id)] <- organization_id
        campus_dataset$orders[is.na(campus_dataset$orders)] <- 0
        campus_dataset$promo[is.na(campus_dataset$promo)] <- 0
        campus_dataset$promo <- factor(campus_dataset$promo)
    } else {
        campus_dataset <- dates_hours
        campus_dataset$organization_id <- organization_id
        campus_dataset$orders <- 0
        campus_dataset$promo <- 0
    }
    campus_dataset <- merge(campus_dataset,semester_dates,all.x = T)
    campus_dataset <- campus_dataset[order(campus_dataset$date,campus_dataset$hour),] #sort it again
    campus_dataset$semester_count <- 1 #add the semster count marker (1st semeter, 2nd semster and so on...)
    new_semesters_markers <- c(which(campus_dataset$day_in_semester==1 & campus_dataset$hour=="00:00:00"),nrow(campus_dataset)) 
    new_semesters_markers <- new_semesters_markers[new_semesters_markers>1]
    for (i in 1:(length(new_semesters_markers)-1))
    {
        campus_dataset$semester_count[new_semesters_markers[i]:(new_semesters_markers[i+1])] <- i+1
    }
    additional_promos <- campus_dataset[diff(campus_dataset$orders)>15,c("date","hour")]
    for (i in 1:nrow(additional_promos))
    {
        campus_dataset$promo[which(campus_dataset$date==additional_promos$date[i] & campus_dataset$hour==additional_promos$hour[i])+1] <- 1
    }
    
    #additonal timings featrues and trends
    campus_dataset$median_orders_this_time_last_week <- 0 #to see a local trend in orders at this time
    campus_dataset$median_orders_this_time_last_2weeks <- 0 #to see a local trend in orders at this time
    campus_dataset$median_orders_this_time_this_day_last_2weeks <- 0 #what ususally going on at this day at this time for near future prediction
    campus_dataset$median_orders_this_time_this_day_last_month <- 0 #what ususally going on at this day at this time for near future prediction
    campus_dataset$median_orders_this_time_this_day_last_2months <- 0 #what ususally going on at this day at this time for near future prediction
    campus_dataset$median_orders_this_time_this_day_last_2weeks_offset <- 0 #what ususally going on at this day at this time for far future prediction
    campus_dataset$median_orders_this_time_this_day_last_month_offset <- 0 #what ususally going on at this day at this time for far future prediction
    campus_dataset$median_orders_this_time_this_day_last_2months_offset <- 0 #what ususally going on at this day at this time for far future prediction
    campus_dataset$orders_this_time_this_day_last_semester <- 0  #for model of start of semester: just add the exact orders count from the previous semster in the same semester day and hour
    #do it parallely
    cl <- makePSOCKcluster(12)
    registerDoParallel(cl)
    campus_dataset <- foreach(i=1:nrow(campus_dataset),.combine=rbind,.verbose = FALSE,.inorder=TRUE) %dopar%
    {
        if (campus_dataset$new_semester_marker[i]==0)
        {
            campus_dataset$median_orders_this_time_last_week[i] <- median(tail(campus_dataset$orders[campus_dataset$hour==campus_dataset$hour[i] & campus_dataset$date<campus_dataset$date[i]],7), na.rm=TRUE)
            campus_dataset$median_orders_this_time_last_2weeks[i] <- median(tail(campus_dataset$orders[campus_dataset$hour==campus_dataset$hour[i] & campus_dataset$date<campus_dataset$date[i]],14), na.rm=TRUE)
            
            campus_dataset$median_orders_this_time_this_day_last_2weeks[i] <- median(tail(campus_dataset$orders[campus_dataset$hour==campus_dataset$hour[i] & campus_dataset$date<campus_dataset$date[i] & campus_dataset$weekday==campus_dataset$weekday[i]],2),na.rm=TRUE) #near future
            campus_dataset$median_orders_this_time_this_day_last_month[i] <- median(tail(campus_dataset$orders[campus_dataset$hour==campus_dataset$hour[i] & campus_dataset$date<campus_dataset$date[i] & campus_dataset$weekday==campus_dataset$weekday[i]],4),na.rm=TRUE) #near future
            campus_dataset$median_orders_this_time_this_day_last_2months[i] <- median(tail(campus_dataset$orders[campus_dataset$hour==campus_dataset$hour[i] & campus_dataset$date<campus_dataset$date[i] & campus_dataset$weekday==campus_dataset$weekday[i]],8),na.rm=TRUE) #near future
            
            campus_dataset$median_orders_this_time_this_day_last_2weeks_offset[i] <- median(tail(campus_dataset$orders[campus_dataset$hour==campus_dataset$hour[i] & campus_dataset$date<(campus_dataset$date[i]-1*7) & campus_dataset$weekday==campus_dataset$weekday[i]],2),na.rm=TRUE) #far future
            campus_dataset$median_orders_this_time_this_day_last_month_offset[i] <- median(tail(campus_dataset$orders[campus_dataset$hour==campus_dataset$hour[i] & campus_dataset$date<(campus_dataset$date[i]-1*7) & campus_dataset$weekday==campus_dataset$weekday[i]],4),na.rm=TRUE) #far future
            campus_dataset$median_orders_this_time_this_day_last_2months_offset[i] <- median(tail(campus_dataset$orders[campus_dataset$hour==campus_dataset$hour[i] & campus_dataset$date<(campus_dataset$date[i]-1*7) & campus_dataset$weekday==campus_dataset$weekday[i]],8),na.rm=TRUE) #far future
            
        } else {
            campus_dataset$median_orders_this_time_last_week[i] <- median(tail(campus_dataset$orders[campus_dataset$semester==T & campus_dataset$hour==campus_dataset$hour[i] & campus_dataset$date<campus_dataset$date[i]],7), na.rm=TRUE)
            campus_dataset$median_orders_this_time_last_2weeks[i] <- median(tail(campus_dataset$orders[campus_dataset$semester==T & campus_dataset$hour==campus_dataset$hour[i] & campus_dataset$date<campus_dataset$date[i]],14), na.rm=TRUE)
            
            campus_dataset$median_orders_this_time_this_day_last_2weeks[i] <- median(tail(campus_dataset$orders[campus_dataset$semester==T & campus_dataset$hour==campus_dataset$hour[i] & campus_dataset$date<campus_dataset$date[i] & campus_dataset$weekday==campus_dataset$weekday[i]],2),na.rm=TRUE) #near future
            campus_dataset$median_orders_this_time_this_day_last_month[i] <- median(tail(campus_dataset$orders[campus_dataset$semester==T & campus_dataset$hour==campus_dataset$hour[i] & campus_dataset$date<campus_dataset$date[i] & campus_dataset$weekday==campus_dataset$weekday[i]],4),na.rm=TRUE) #near future
            campus_dataset$median_orders_this_time_this_day_last_2months[i] <- median(tail(campus_dataset$orders[campus_dataset$semester==T & campus_dataset$hour==campus_dataset$hour[i] & campus_dataset$date<campus_dataset$date[i] & campus_dataset$weekday==campus_dataset$weekday[i]],8),na.rm=TRUE) #near future
            
            campus_dataset$median_orders_this_time_this_day_last_2weeks_offset[i] <- median(tail(campus_dataset$orders[campus_dataset$semester==T & campus_dataset$hour==campus_dataset$hour[i] & campus_dataset$date<(campus_dataset$date[i]-1*7) & campus_dataset$weekday==campus_dataset$weekday[i]],2),na.rm=TRUE) #far future
            campus_dataset$median_orders_this_time_this_day_last_month_offset[i] <- median(tail(campus_dataset$orders[campus_dataset$semester==T & campus_dataset$hour==campus_dataset$hour[i] & campus_dataset$date<(campus_dataset$date[i]-1*7) & campus_dataset$weekday==campus_dataset$weekday[i]],4),na.rm=TRUE) #far future
            campus_dataset$median_orders_this_time_this_day_last_2months_offset[i] <- median(tail(campus_dataset$orders[campus_dataset$semester==T & campus_dataset$hour==campus_dataset$hour[i] & campus_dataset$date<(campus_dataset$date[i]-1*7) & campus_dataset$weekday==campus_dataset$weekday[i]],8),na.rm=TRUE) #far future
        }
        if (campus_dataset$day_in_semester[i]>0 & campus_dataset$semester_count[i]>2)
        {
            campus_dataset$orders_this_time_this_day_last_semester[i] <- campus_dataset$orders[campus_dataset$hour==campus_dataset$hour[i] & campus_dataset$day_in_semester==campus_dataset$day_in_semester[i] & campus_dataset$semester_count==(campus_dataset$semester_count[i]-1)]
        } else {
            campus_dataset$orders_this_time_this_day_last_semester[i] <- 0
        }
        
        campus_dataset[i,,drop=FALSE]
    }
    stopCluster(cl)
    #Fill in missing info
    campus_dataset$orders_this_time_this_day_last_semester[is.na(campus_dataset$orders_this_time_this_day_last_semester)] <- 0
    campus_dataset$median_orders_this_time_last_week[is.na(campus_dataset$median_orders_this_time_last_week)] <- 0
    campus_dataset$median_orders_this_time_last_2weeks[is.na(campus_dataset$median_orders_this_time_last_2weeks)] <- 0
    campus_dataset$median_orders_this_time_this_day_last_2weeks[is.na(campus_dataset$median_orders_this_time_this_day_last_2weeks)] <- 0
    campus_dataset$median_orders_this_time_this_day_last_month[is.na(campus_dataset$median_orders_this_time_this_day_last_month)] <- 0
    campus_dataset$median_orders_this_time_this_day_last_2months[is.na(campus_dataset$median_orders_this_time_this_day_last_2months)] <- 0
    campus_dataset$median_orders_this_time_this_day_last_2weeks_offset[is.na(campus_dataset$median_orders_this_time_this_day_last_2weeks_offset)] <- 0
    campus_dataset$median_orders_this_time_this_day_last_month_offset[is.na(campus_dataset$median_orders_this_time_this_day_last_month_offset)] <- 0
    campus_dataset$median_orders_this_time_this_day_last_2months_offset[is.na(campus_dataset$median_orders_this_time_this_day_last_2months_offset)] <- 0
    campus_dataset$trend_orders_this_time_last_2weeks <- (campus_dataset$median_orders_this_time_last_week-campus_dataset$median_orders_this_time_last_2weeks)
    campus_dataset$trend_orders_this_time_this_day_last_month <- (campus_dataset$median_orders_this_time_this_day_last_2weeks-campus_dataset$median_orders_this_time_this_day_last_month)
    campus_dataset$trend_orders_this_time_this_day_last_2months <- (campus_dataset$median_orders_this_time_this_day_last_2weeks-campus_dataset$median_orders_this_time_this_day_last_2months)
    campus_dataset$trend_orders_this_time_this_day_last_month_offset <- (campus_dataset$median_orders_this_time_this_day_last_2weeks_offset-campus_dataset$median_orders_this_time_this_day_last_month_offset)
    campus_dataset$trend_orders_this_time_this_day_last_2months_offset <- (campus_dataset$median_orders_this_time_this_day_last_2weeks_offset-campus_dataset$median_orders_this_time_this_day_last_2months_offset)
    campus_dataset$trend_prop_orders_this_time_last_2weeks <- (campus_dataset$median_orders_this_time_last_week-campus_dataset$median_orders_this_time_last_2weeks)/campus_dataset$median_orders_this_time_last_2weeks
    campus_dataset$trend_prop_orders_this_time_this_day_last_month <- (campus_dataset$median_orders_this_time_this_day_last_2weeks-campus_dataset$median_orders_this_time_this_day_last_month)/campus_dataset$median_orders_this_time_this_day_last_month
    campus_dataset$trend_prop_orders_this_time_this_day_last_2months <- (campus_dataset$median_orders_this_time_this_day_last_2weeks-campus_dataset$median_orders_this_time_this_day_last_2months)/campus_dataset$median_orders_this_time_this_day_last_2months
    campus_dataset$trend_prop_orders_this_time_this_day_last_month_offset <- (campus_dataset$median_orders_this_time_this_day_last_2weeks_offset-campus_dataset$median_orders_this_time_this_day_last_month_offset)/campus_dataset$median_orders_this_time_this_day_last_month_offset
    campus_dataset$trend_prop_orders_this_time_this_day_last_2months_offset <- (campus_dataset$median_orders_this_time_this_day_last_2weeks_offset-campus_dataset$median_orders_this_time_this_day_last_2months_offset)/campus_dataset$median_orders_this_time_this_day_last_2months_offset
    campus_dataset$trend_prop_orders_this_time_last_2weeks[campus_dataset$trend_prop_orders_this_time_last_2weeks==Inf] <- 0
    campus_dataset$trend_prop_orders_this_time_this_day_last_month[campus_dataset$trend_prop_orders_this_time_this_day_last_month==Inf] <- 0
    campus_dataset$trend_prop_orders_this_time_this_day_last_2months[campus_dataset$trend_prop_orders_this_time_this_day_last_2months==Inf] <- 0
    campus_dataset$trend_prop_orders_this_time_this_day_last_month_offset[campus_dataset$trend_prop_orders_this_time_this_day_last_month_offset==Inf] <- 0
    campus_dataset$trend_prop_orders_this_time_this_day_last_2months_offset[campus_dataset$trend_prop_orders_this_time_this_day_last_2months_offset==Inf] <- 0
    campus_dataset$trend_prop_orders_this_time_last_2weeks[is.na(campus_dataset$trend_prop_orders_this_time_last_2weeks)] <- 0
    campus_dataset$trend_prop_orders_this_time_this_day_last_month[is.na(campus_dataset$trend_prop_orders_this_time_this_day_last_month)] <- 0
    campus_dataset$trend_prop_orders_this_time_this_day_last_2months[is.na(campus_dataset$trend_prop_orders_this_time_this_day_last_2months)] <- 0
    campus_dataset$trend_prop_orders_this_time_this_day_last_month_offset[is.na(campus_dataset$trend_prop_orders_this_time_this_day_last_month_offset)] <- 0
    campus_dataset$trend_prop_orders_this_time_this_day_last_2months_offset[is.na(campus_dataset$trend_prop_orders_this_time_this_day_last_2months_offset)] <- 0
  
    #Combine all campuses together (temprary save)
    campus_dataset_total <- rbind(campus_dataset,campus_dataset_total)
    # write.csv(campus_dataset_total,"campus_dataset_total_no_weather.csv",row.names = F) #save new dataset
    
    #Build training dataset
    #campus_dataset <- campus_dataset_total[campus_dataset_total$organization_id==organization_id,]
    campus_dataset$date <- as.Date(campus_dataset$date)
    campus_dataset$weekend <- factor(campus_dataset$weekend)
    campus_dataset$workday <- factor(campus_dataset$workday)
    campus_dataset$holiday <- factor(campus_dataset$holiday)
    campus_dataset$semester <- factor(campus_dataset$semester)
    campus_dataset$promo <- factor(campus_dataset$promo)
    campus_dataset$month <- factor(campus_dataset$month)
    campus_dataset$year <- factor(campus_dataset$year)
    campus_dataset$weekday <- factor(campus_dataset$weekday)
    campus_dataset$hour <- factor(campus_dataset$hour)
    campus_dataset$new_semester_marker <- factor(campus_dataset$new_semester_marker)
    
    #Make the dataset ready for prediction(2 type of models - with and without offset)
    dataset <- NULL
    dataset_offset <- NULL
    cl <- makePSOCKcluster(12)
    registerDoParallel(cl)
    dataset <- foreach(i=1:length(unique(campus_dataset$date)),.combine=rbind,.verbose = FALSE,.inorder=TRUE) %dopar%
    {
        date <- unique(campus_dataset$date)[i]
        #for each day we want to be able to forecast for 2 weeks ahead
        dataset_date <- NULL
        for (offset in 1:14)
        {
            if (sum(campus_dataset$date==(date+offset))!=0) #only if not reaching end of year
            {
                if (offset<=7) #near future
                {
                    dataset_tmp <- cbind(campus_dataset[campus_dataset$date==date,-c(2:13,15:41,44:50,52:55,57:60)],campus_dataset[campus_dataset$date==(date+offset),-c(3,42:43,47:49,51,54:56,59:60)])
                } else {  #more than a week offset (far future, cant take infor form next week)
                    dataset_tmp <- cbind(campus_dataset[campus_dataset$date==date,-c(2:13,15:41,44:50,52:55,57:60)],campus_dataset[campus_dataset$date==(date+offset),-c(3,42:46,51:53,56:58)])
                }
                names(dataset_tmp)[1:6] <- paste0(names(campus_dataset)[-c(2:13,15:41,44:50,52:55,57:60)],"_today")
                names(dataset_tmp)[7:54] <- paste0(names(campus_dataset)[-c(3,42:43,47:49,51,54:56,59:60)],"_future")
                dataset_tmp$diff_days <- offset
                dataset_tmp$organization_id <- organization_id
                dataset_date <- rbind(dataset_date,dataset_tmp)
            }
        }
        dataset_date
    }
    stopCluster(cl)
    #Add the future promos dates
    #Add the future promos dates
    future_promos_data <- promos_data[promos_data$ID==organization_id,c("promo","date","hour")]
    for (i in 1:nrow(future_promos_data))
    {
        dataset$promo_future[which(dataset$date_future==future_promos_data$date[i] & dataset$hour_future==future_promos_data$hour[i])+1] <- 1
    }
    #Merge to the whole dataset
    dataset_offset <- dataset[dataset$diff_days>7,] #with offset (build the models only based on the offset dataset - those are differernt models!)
    dataset <- dataset[dataset$diff_days<=7,]
    dataset_tot <- rbind(dataset_tot,dataset)
    dataset_tot_offset <- rbind(dataset_tot_offset,dataset_offset)
#     write.csv(dataset_tot,paste0("dataset_no_weather.csv"),row.names = F) #save the dataset to later use in graphlab
#     write.csv(dataset_tot_offset,paste0("dataset_offset_no_weather.csv"),row.names = F) #save the dataset to later use in graphlab
#     
    #Create XGBOOST model
    results_tot <- NULL
    results_tot_train <- NULL
    dates_validation <- rev(seq.Date(from = max(dataset_tot$date_future), to= min(dataset_tot$date_today),by = -14))
    dataset <- dataset_tot
    dataset$organization_id <- NULL
    for (i in 2:(length(dates_validation)-1))
    {
        trainset <- dataset[dataset$date_future<=dates_validation[i] & dataset$date_future>dates_validation[10],]
        validset <- dataset[dataset$date_future<=dates_validation[i+1] & dataset$date_future>dates_validation[i],]
        results <- data.frame(date_today=validset$date_today,date_future=validset$date_future,hour_future=validset$hour_future,diff_days=validset$diff_days)
        results_train <- data.frame(date_today=trainset$date_today,date_future=trainset$date_future,hour_future=trainset$hour_future,diff_days=trainset$diff_days)
        
        trainset$date_today <- NULL
        trainset$date_future <- NULL
        trainset$hour_future <- NULL
        validset$hour_future <- NULL
        validset$date_today <- NULL
        validset$date_future <- NULL
        dtrain <- xgb.DMatrix(data=data.matrix(trainset[,-which(names(trainset)=="orders_future")]),label=trainset$orders_future)
        dval<- xgb.DMatrix(data=data.matrix(validset[,-which(names(trainset)=="orders_future")]),label=validset$orders_future)
        
        #Best Setting fo training and validation no overfit
        watchlist <- list(val=dval,train=dtrain)
        param <- list(objective="reg:linear",eta=0.01,nthread = 12,max_depth=5,
                      colsample_bytree = 0.5,subsample = 0.5,min_child_weight=10)
        
        #Changable settings
        #         watchlist <- list(val=dval,train=dtrain)
        #         param <- list(objective="reg:linear",eta=0.1,nthread = 12,max_depth=10)
        #         
        num_rounds <- 1000
        mod <- xgb.train(params = param, data = dtrain, nrounds = num_rounds, watchlist = watchlist,print.every.n = 10,maximize = F)
        
        train_prediction <- round(predict(mod,dtrain))
        train_prediction[train_prediction<0] <- 0
        valid_prediction <- round(predict(mod,dval))
        valid_prediction[valid_prediction<0] <- 0
        #         RMSE(train_prediction,trainset$orders_future)
        #         RMSE(valid_prediction,validset$orders_future)
        
        results <- cbind(results,data.frame(orders_future=validset$orders_future,predicted_orders=valid_prediction))
        results_tot <- rbind(results_tot,results)
        
        results_train <- cbind(results_train,data.frame(orders_future=trainset$orders_future,predicted_orders=train_prediction))
        results_tot_train <- rbind(results_tot_train,results_train)
        
        day_diff_set <- 7
        
        plot(results_train$orders_future[results_train$diff_days==day_diff_set],type="l")
        lines(train_prediction[results_train$diff_days==day_diff_set],col="red")
        
        plot(results$orders_future[results$diff_days==day_diff_set],type="l")
        lines(valid_prediction[results$diff_days==day_diff_set],col="red")
    }
}
