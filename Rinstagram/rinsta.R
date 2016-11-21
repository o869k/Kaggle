#Initialization ####
rm(list=ls()) # clear workspace
mainDir <- "/Rinstagram/"
setwd(mainDir)
set.seed(1)
options(java.parameters = "-Xmx8000m")
library(plyr)
library(dplyr)
library(instaR)
library(rjson)
library(pageviews)
library(lubridate)
library(streamR)
library(ROAuth)
library(tuber)
library(walkscoreAPI)
library(clarifai)
library(wordcloud)
library(RColorBrewer)
library(twitteR)
library(tm)
library(devtools)

#Instagram stuff to be ready only after approval ####
save(my_oauth, file="my_oauth")

## Searching and downloading 100 public media that mention #obama
comments <- getComments(orikro$id[1], token=my_oauth )
mccain <- getFollowers( username="orikro", token=my_oauth )
likes <- getLikes(orikro$id[1], token=my_oauth)
popular_posts <- getPopular(token=my_oauth )

## Searching and downloading pictures sent from Times Square with a minimum date
## of 2013-12-31 and a maximum date of 2014-01-01
tsq <- searchInstagram( lat=40.7577, lng=-73.9857, distance=500,
                        token=my_oauth, n=500, folder="timessquare",
                        mindate="2013-12-31", maxdate="2014-01-01")

#Wikipedia API is working ####
today <- pageview_timestamps(timestamps = Sys.Date(), first = TRUE)
wiki_bar <- article_pageviews(project = "en.wikipedia", article = "Shawn Dawson",start = "2010100100", end = substr(today,1,nchar(today)-2))
# wiki_bar <- article_pageviews(project = "he.wikipedia", article = "Bar Refaeli",start = "2010100100", end = today)
wiki_bar$project <- NULL
wiki_bar$article <- NULL
wiki_bar$access <- NULL
wiki_bar$agent <- NULL
wiki_bar$timestamp <- as.Date(wiki_bar$timestamp,format = "%Y%m%d")
# alldates <- data.frame(timestamp=seq.Date(min(wiki_bar$timestamp),max(wiki_bar$timestamp), by="day")) #fill missing dates
plot(wiki_bar$timestamp,wiki_bar$views,type="l")
plot(wiki_bar$timestamp,log(wiki_bar$views),type="l")
wiki_bar$month <- month(wiki_bar$timestamp)
wiki_bar$weekdays <- weekdays(wiki_bar$timestamp)
aggregate(views~weekdays,data=wiki_bar,sum)
aggregate(views~month,data=wiki_bar,mean)
aggregate(views~month,data=wiki_bar,max)

#Linkeidn API #####
in.auth <- inOAuth("your_app_name", "your_consumer_key", "your_consumer_secret")

#Twitter API ####
setup_twitter_oauth("TzUlIoq602A19w0SzldjeOpU3", "fhPcteL9VfnPBcImYYCQ4HKpJEOETTtfAsKSmBCcOAaOqAfzmA")

token <- Token2.0$new(
    params = list(as_header=TRUE),
    app = oauth_app("fun.with.twitter","TzUlIoq602A19w0SzldjeOpU3", "fhPcteL9VfnPBcImYYCQ4HKpJEOETTtfAsKSmBCcOAaOqAfzmA"),
    endpoint = oauth_endpoints("twitter"),
    credentials = list(access_token = "51014527-gEKc9AgfWlO08dnpxuBvZ0pWprVVQ0vioSkM2zNAi"),
    cache = FALSE
)
use_oauth_token(token)
mach_tweets <- searchTwitter("machine learning", n=500, lang="en")

tuser <- getUser('KingJames')
tuser$profileImageUrl
tuserdf <- tuser@toDataFrame(tuser)

#Cloud Deep Learning with clarifai #####
secret_id(c("fVSoqOnDFI5EyHTdRgKZWy1ZODN8BSDntSHVcnrx","x6tnvw60fGFVo1iwA1tud0DLrQvlCqWz0G7Arl6I"))
get_token() #get the token
get_info() #Get information about your application:

#Get Tags from local or url
res <- tag_image_urls("http://www.clarifai.com/img/metro-north.jpg",meta = F,simplify = F)
res <- tag_image_urls(tuser$profileImageUrl,meta = F,simplify = F)
res <- tag_images("C:/images.jpg",meta = F,simplify = F)
res <- tag_images("C:/panda.jpg",meta = F,simplify = F)
res <- tag_images("C:/dog.jpg",meta = F,simplify = F)
cols <- get_color("C:/dog.jpg")

cbind(res$classes[[1]],res$probs[[1]])
wordcloud(res$classes[[1]],length(res$probs[[1]]):1, scale=c(3,0.5),min.freq=1,colors=brewer.pal(9,"BuGn"),random.order=F,rot.per=0) #Make wordcloud

#Get tags for a local image:

