#Constants
date_time_ny <- as.POSIXlt(Sys.time(), "America/New_York")  # in New York
Date <- format(as.Date(date_time_ny, tz = "EST"),'%Y/%m/%d')
Hour <- hour(date_time_ny)
baseURL <- 'https://www.wunderground.com/history/airport/' #MUST BE HTTPS
suffixURL <- 'DailyHistory.html?format=1'

stations <- read.csv("data/nyc.csv",stringsAsFactors = F)

#Connect to my dropbox
token <- readRDS("data/droptoken.rds")

#Read and Preprocess NYC current Weather data (many options to fetch data) get Weather From NOAA (were doing it from linux server)
url2fetch <- paste(baseURL,"KJFK",Date,suffixURL, sep='/')
# url_content <- getURL(URLencode(url2fetch))
# url_content <- gsub('<br />', '', url_content)
# d4 <- tail(read.csv(textConnection(url_content)),1)
# d2 <- as.data.frame(tail(fread(url2fetch,stringsAsFactors = FALSE,verbose = FALSE),1))
# d3 <- tail(getDetailedWeather("KNYC",date=Date,opt_all_columns = TRUE),1) #package is not working srver
# d1 <- tail(read.csv(url(url2fetch),stringsAsFactors = FALSE),1)
#d5 <- content(GET(url2fetch), as="text")
#d6 <- paste(readLines(url), collapse=" ")

d1 <- gsub('<br />', '', readLines(url2fetch)[-1])
d1 <- data.frame(matrix(unlist(sapply(d1,function(x) {strsplit(x,",")})),ncol=14,byrow=T),stringsAsFactors=FALSE)
names(d1) <- d1[1,]; d1 <- tail(d1,1)
d1 <- weather_edit(d1) 

url2fetch <- paste(baseURL,"KLGA",Date,suffixURL, sep='/')
#d2 <- tail(read.csv(url(url2fetch),stringsAsFactors = FALSE),1)
d2 <- gsub('<br />', '', readLines(url2fetch)[-1])
d2 <- data.frame(matrix(unlist(sapply(d2,function(x) {strsplit(x,",")})),ncol=14,byrow=T),stringsAsFactors=FALSE)
names(d2) <- d2[1,]; d2 <- tail(d2,1)
d2 <- weather_edit(d2) 

url2fetch <- paste(baseURL,"KNYC",Date,suffixURL, sep='/')
#d3 <- tail(read.csv(url(url2fetch),stringsAsFactors = FALSE),1)
d3 <- gsub('<br />', '', readLines(url2fetch)[-1])
d3 <- data.frame(matrix(unlist(sapply(d3,function(x) {strsplit(x,",")})),ncol=14,byrow=T),stringsAsFactors=FALSE)
names(d3) <- d3[1,]; d3 <- tail(d3,1)
d3 <- weather_edit(d3) 

url2fetch <- paste(baseURL,"KEWR",Date,suffixURL, sep='/')
#d4 <- tail(read.csv(url(url2fetch),stringsAsFactors = FALSE),1)
d4 <- gsub('<br />', '', readLines(url2fetch)[-1])
d4 <- data.frame(matrix(unlist(sapply(d4,function(x) {strsplit(x,",")})),ncol=14,byrow=T),stringsAsFactors=FALSE)
names(d4) <- d4[1,]; d4 <- tail(d4,1)
d4 <- weather_edit(d4) 

#Get spetial data
Newyork <- get_map(location="New York City",zoom=11)

nyc <- readOGR("data/ZIP_CODE_040114.shp", ogrListLayers("data/ZIP_CODE_040114.shp")[1],verbose = FALSE)
nyc_map <- fortify(gSimplify(nyc, 0.05))
NYboundry <- spTransform(nyc,CRS("+proj=longlat +datum=NAD83"))
