#Libraries
library(timeDate)
library(randomForest)
library(ggplot2)
library(rgdal)
library(rgeos)
library(ggmap)
library(curl)
library(RCurl)
library(data.table)
library(rdrop2)
library(httr)
library(XML)

#Read local spetial data
stations <- read.csv("data/nyc.csv",stringsAsFactors = F)
Newyork <- get_googlemap(center = c(-73.985428,40.718817),zoom=11,maptype = "terrain",style = 'feature:road|element:labels|visibility:off')
nyc <- readOGR("data/ZIP_CODE_040114.shp", ogrListLayers("data/ZIP_CODE_040114.shp")[1],verbose = FALSE)
#nyc_map <- fortify(gSimplify(nyc, 0.05))
NYboundry <- spTransform(nyc,CRS("+proj=longlat +datum=NAD83"))

#Functions
weather_edit <- function(w) {
    names(w)[ncol(w)] <- "DateUTC"
    w$DateUTC <- gsub('<br />', '',w$DateUTC)
    names(w) <- gsub('.','_',names(w),fixed = TRUE)
    names(w) <- gsub('/','_',names(w),fixed = TRUE)
    names(w) <- gsub(' ','_',names(w),fixed = TRUE)
    
    w$DATE <- as.Date(w$DateUTC, tz = "EST")
    w$HOUR <- hour(date_time_ny)
    w$Time <- NULL
    w$TimeEDT <- NULL
    w$TimeEST <- NULL
    w$DateUTC <- NULL
    w$Sea_Level_PressurehPa <- NULL
    w$Dew_PointC <- NULL
    w$WindDirDegrees <- NULL
    w$Gust_SpeedKm_h <- NULL
    w$Wind_SpeedKm_h <- NULL
    w$TemperatureC <- NULL
    w$Precipitationmm <- NULL
    
    if(is.na(w$Events)) {w$Events <- ""}
    
    w$Humidity <- as.numeric(w$Humidity)
    w$Wind_Direction <- as.character(w$Wind_Direction)
    w$Conditions <- as.character(w$Conditions)
    w$Events <- as.character(w$Events)
    #w$VisibilityKm <- as.numeric(w$VisibilityKm)
    
    w$Weekday <- as.character(weekdays(w$DATE))
    w$Weekend <- as.character(isWeekend(w$DATE))
    w$Holiday <- as.character(isHoliday(timeDate(w$DATE),holidayNYSE()))
    w$Bizday <- as.character(isBizday(timeDate(w$DATE),holidayNYSE()))
    w$Month <- as.character(month(w$DATE))
    w$HOUR <- as.character(w$HOUR)
    w$DATE <- NULL

    w
}
deg.dist <- function (long1, lat1, long2, lat2) {
    rad <- pi/180
    a1 <- lat1 * rad
    a2 <- long1 * rad
    b1 <- lat2 * rad
    b2 <- long2 * rad
    dlon <- b2 - a2
    dlat <- b1 - a1
    a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
    c <- 2 * atan2(sqrt(a), sqrt(1 - a))
    R <- 40041.47/(2 * pi)
    d <- R * c
    return(d)
}
Prec_data <- function(ZIP) {
    url <- "http://graphical.weather.gov/xml/sample_products/browser_interface/ndfdXMLclient.php"
    response <- GET(url,query=list(zipCodeList=ZIP,
                                   product="time-series",
                                   begin=as.Date(date_time_ny, tz = "EST"),
                                   Unit="m",
                                   qpf="qpf"))
    doc   <- content(response,type="text/xml")   # XML document with the data
    data   <- doc["//parameters/*"]
    data   <- sapply(data,function(d)removeChildren(d,kids=list("name")))
    result <- do.call(data.frame,lapply(data,function(d)xmlSApply(d,xmlValue)))
    colnames(result) <- sapply(data,xmlName)
    result <- head(data.frame(result),1)
    result
}
Temp_data <- function(ZIP) {
    url <- "http://graphical.weather.gov/xml/sample_products/browser_interface/ndfdXMLclient.php"
    response <- GET(url,query=list(zipCodeList=ZIP,
                                   product="time-series",
                                   begin=as.Date(date_time_ny, tz = "EST"),
                                   Unit="m",
                                   temp="temp"))
    doc   <- content(response,type="text/xml")   # XML document with the data
    data   <- doc["//parameters/*"]
    data   <- sapply(data,function(d)removeChildren(d,kids=list("name")))
    result <- do.call(data.frame,lapply(data,function(d)xmlSApply(d,xmlValue)))
    colnames(result) <- sapply(data,xmlName)
    result <- head(data.frame(result),1)
    result
}
Wind_data <- function(ZIP) {
    url <- "http://graphical.weather.gov/xml/sample_products/browser_interface/ndfdXMLclient.php"
    response <- GET(url,query=list(zipCodeList=ZIP,
                                   product="time-series",
                                   begin=as.Date(date_time_ny, tz = "EST"),
                                   Unit="m",
                                   wspd="wspd"))
    doc   <- content(response,type="text/xml")   # XML document with the data
    data   <- doc["//parameters/*"]
    data   <- sapply(data,function(d)removeChildren(d,kids=list("name")))
    result <- do.call(data.frame,lapply(data,function(d)xmlSApply(d,xmlValue)))
    colnames(result) <- sapply(data,xmlName)
    result <- head(data.frame(result),1)
    result
}

#Constants
date_time_ny <- as.POSIXlt(Sys.time(), "America/New_York")  # in New York
Date <- format(as.Date(date_time_ny, tz = "EST"),'%Y/%m/%d')
baseURL <- 'https://www.wunderground.com/history/airport/' #MUST BE HTTPS
suffixURL <- 'DailyHistory.html?format=1'

#Connect to my dropbox
token <- readRDS("data/droptoken.rds")

#Read and Preprocess NYC current Weather data (many options to fetch data) get Weather From NOAA (were doing it from linux server)
url2fetch <- paste(baseURL,"KJFK",Date,suffixURL, sep='/')
d1 <- tail(read.csv(url(url2fetch,"r",method = "libcurl",encoding = "UTF-8"),stringsAsFactors = FALSE),1) #UCS-2LE
d1 <- weather_edit(d1) 

url2fetch <- paste(baseURL,"KLGA",Date,suffixURL, sep='/')
d2 <- tail(read.csv(url(url2fetch,"r",method = "libcurl",encoding = "UTF-8"),stringsAsFactors = FALSE),1)
d2 <- weather_edit(d2) 

url2fetch <- paste(baseURL,"KNYC",Date,suffixURL, sep='/')
d3 <- tail(read.csv(url(url2fetch,"r",method = "libcurl",encoding = "UTF-8"),stringsAsFactors = FALSE),1)
d3 <- weather_edit(d3) 

url2fetch <- paste(baseURL,"KEWR",Date,suffixURL, sep='/')
d4 <- tail(read.csv(url(url2fetch,"r",method = "libcurl",encoding = "UTF-8"),stringsAsFactors = FALSE),1)
d4 <- weather_edit(d4) 
