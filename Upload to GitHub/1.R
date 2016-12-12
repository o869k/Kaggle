#Initializaiotn ####
rm(list=ls()) # clear workspace
setwd(mainDir)
set.seed(1)
options(java.parameters = "-Xmx8000m")
library(RGA)
library(RGoogleAnalytics)
library(googleAuthR)
library(foreach)
library(doParallel)
library(lubridate)
library(chron)
library(stringi)
library(zoo)

#Functions
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
fixProductName <- function(x,y) trim(gsub(x,"",y))

#Reading data ####
#Get access token
token <- Auth(client.id,client.secret)
authorize()

ga.profiles <- read.csv("data/ga.profiles.csv",stringsAsFactors = F)
ga.profiles <- ga.profiles[ga.profiles$id==48049536,]
#ga.segments <- read.csv("data/ga.segments.csv",stringsAsFactors = F)
#ga_meta <- read.csv("data/ga_meta.csv",stringsAsFactors = F)

dataset$minute <- NULL #cause lots of duplicates
dataset$itemQuantity <- NULL
dataset <- unique(dataset)

query_dataset <- read.csv("Query_data/48049536_query_dataset.csv",stringsAsFactors = F)
products <- read.csv("Products/products_bigals.csv",stringsAsFactors = F)
transactions <- read.csv("data/48049536_dataset_transaction.csv",stringsAsFactors = F)

#Exploritory Analysis of dataset ####
#dataset
#Explenation for each feature:
#searchkeyword #Search term used within the property. There are 16441K unique seaches
#sessionDurationBucket - The length of a session measured in seconds and reported in second increments.
#browserSize #The viewport size of users' browsers. A session-scoped dimension, browser size captures the initial dimensions of the viewport in pixels and is formatted as width x height. There are more than 4K unique sizes
#minute - the minute whithin an hour (0:59)
#pageDepth - The number of pages visited by users during a session
#secondPagePath - The second page in users' sessions
#users - The total number of users for the requested time period.
#newUsers - The number of users whose session was marked as a first-time session.
#sessions - The total number of sessions? HOW COME THIS IS ZEROS SOMETIMES?
#sessionDuration - Total duration (in seconds) of users' sessions.
#bounces - The total number of single page (or single interaction hit) sessions for the property.
#hits - Total number of hits for the view (profile). This metric sums all hit types, including pageview, custom event, ecommerce, and other types.
#socialInteractions - The total number of social interactions.
#searchResultViews - The number of times a search result page was viewed.
#searchUniques - total number of unique keywords from internal searches within a session
#searchSessions - The total number of sessions that included an internal search.
#searchDepth - The total number of subsequent page views made after a use of the site's internal search feature.
#searchRefinements - The total number of times a refinement (transition) occurs between internal keywords search within a session
#searchDuration - The session duration when the site's internal search feature is used.
#searchExits - The number of exits on the site that occurred following a search result from the site's internal search feature.
#organicSearches - The number of organic searches happened in a session. This metric is search engine agnostic
#transactions - The total number of transactions.
#transactionRevenue - The total sale revenue (excluding shipping and tax) of the transaction.
#itemQuantity - Total number of items purchased.
#productAddsToCart - Number of times the product was added to the shopping cart
#productCheckouts - Number of times the product was included in the check-out process
#internalPromotionCTR - The rate at which users clicked through to view the internal promotion
#productListCTR - The rate at which users clicked through on the product in a product list
#productRefunds - Number of times a refund was issued for the product
#userType - A boolean, either New Visitor or Returning Visitor, indicating if the users are new or returning.
#sessionCount - The session index for a user. 
#daysSinceLastSession - The number of days elapsed since users last visited the property, used to calculate user loyalty.
#userDefinedValue - The value provided when defining custom user segments for the property.
#referralPath - The path of the referring URL (e.g., document.referrer). If someone places on their webpage a link to the property, this is the path of the page containing the referring link.
#fullReferrer - The full referring URL including the hostname and path.
#campaign - For manual campaign tracking, it is the value of the utm_campaign campaign tracking parameter.
#source - The source of referrals. For manual campaign tracking, it is the value of the utm_source campaign tracking parameter. 
#medium - The type of referrals.
#keyword - For manual campaign tracking, it is the value of the utm_term campaign tracking parameter.
#sourceMedium - Combined values of ga:source and ga:medium.
#adContent - For manual campaign tracking, it is the value of the utm_content campaign tracking parameter. For AdWords autotagging, it is the first line of the text for the online Ad campaign
#socialNetwork - The social network name.
#hasSocialSourceReferral - A boolean, either Yes or No, indicates whether sessions to the property are from a social source.
#browser - The name of users' browsers
#browserVersion - The version of users' browsers
#operatingSystem - Users' operating system
#operatingSystemVersion - The version of users' operating system
#mobileDeviceBranding - Mobile manufacturer or branded name.
#mobileInputSelector - Selector (e.g., touchscreen, joystick, clickwheel, stylus) used on the mobile device.
#mobileDeviceInfo - The branding, model, and marketing name used to identify the mobile device.
#mobileDeviceMarketingName - The marketing name used for the mobile device.
#deviceCategory - The type of device: desktop, tablet, or mobile.
#continent - Users' continent, derived from users' IP addresses or Geographical IDs.
#subContinent - Users' sub-continent,
#city - Users' city,
#region - Users' region ID
#metro - The Designated Market Area (DMA) from where traffic arrived
#country - Users' country, 
#latitude - The approximate latitude of users' city, derived from their IP addresses or Geographical IDs
#longitude - The approximate latitude of users' city, derived from their IP addresses or Geographical IDs
#id - client id
#networkDomain - The domain name of users' ISP, derived from the domain name registered to the ISP's IP address.
#networkLocation - The names of the service providers used to reach the property
#flashVersion - The version of Flash, including minor versions, supported by users' browsers.
#javaEnabled - A boolean, either Yes or No, indicating whether Java is enabled in users' browsers.
#language - The language, in ISO-639 code format 
#screenColors - The color depth of users' monitors, 
#screenResolution - Resolution of users' screens
#hostname - The hostname from which the tracking request was made.
#pagePath - A page on the website specified by path and/or query parameters. 
#pagePathLevel1 - This dimension rolls up all the page paths in the first hierarchical level in pagePath.
#pagePathLevel2 - This dimension rolls up all the page paths in the second hierarchical level in pagePath.
#pagePathLevel3 - This dimension rolls up all the page paths in the third hierarchical level in pagePath.
#pageTitle - The page's title. Multiple pages might have the same page title.
#landingPagePath - The first page in users' sessions, or the landing page.
#exitPagePath - The last page or exit page in users' sessions.
#searchUsed - A boolean, either Visits With Site Search or Visits Without Site Search, to distinguish whether internal search was used in a session.
#searchKeywordRefinement - Subsequent keyword search term or string entered by users after a given initial string search
#searchCategory - The category used for the internal search if site search categories are enabled in the view. For example, the product category may be electronics, furniture, or clothing.
#searchDestinationPage - The page users immediately visited after performing an internal search on the site. This is usually the search results page.
#screenName - The name of the screen.
#screenDepth - The number of screenviews (reported as a string) per session, useful for historgrams.
#landingScreenName - The name of the first viewed screen.
#exitScreenName - The name of the screen where users exited the application.
#dateHour - Combined values of ga:date and ga:hour.
#channelGrouping - The default channel grouping shared within the View
#checkoutOptions - User options specified during the checkout process, e.g., FedEx, DHL, UPS for delivery options; Visa, MasterCard, AmEx for payment options
#dataSource - The data source of a hit. By default, hits sent from analytics.js are reported as "web" and hits sent from the mobile SDKs are reported as "app".
#Affiliation - A product affiliation to designate a supplying company or brick and mortar location.
#productName The product name, supplied by the ecommerce tracking application, for purchased items.
#Product Category Any product variation (size, color) supplied by the ecommerce application for purchased items, not compatible with Enhanced Ecommerce.

#dataset
dataset[dataset=="(not set)"] <- NA
dataset[dataset=="(not provided)"] <- NA
dataset[dataset=="(none)"] <- NA
dataset[dataset=="(direct)"] <- NA
dataset[dataset=="(other)"] <- NA
dataset$browserSize_width <- as.numeric(sapply(dataset$browserSize,function(x) {strsplit(x,"x")[[1]][1]}))
dataset$browserSize_height <- as.numeric(sapply(dataset$browserSize,function(x) {strsplit(x,"x")[[1]][2]}))
dataset$browserSize_ratio <- dataset$browserSize_width/dataset$browserSize_height
dataset$browserSize <- NULL
dataset$users <- NULL
dataset$minute <- NULL
dataset$sourceMedium <- NULL
dataset$browserVersion <- sapply(dataset$browserVersion,function(x) {strsplit(x,"[.]")[[1]][1]})
dataset$mobileDeviceInfo <- NULL
dataset$mobileDeviceMarketingName <- NULL
dataset$region <- NULL
dataset$metro <- NULL
dataset$city <- NULL
dataset$latitude <- NULL
dataset$longitude <- NULL
dataset$flashVersion <- sapply(dataset$flashVersion,function(x) {strsplit(x,"[.]")[[1]][1]})
dataset$screenResolution_width <- as.numeric(sapply(dataset$screenResolution,function(x) {strsplit(x,"x")[[1]][1]}))
dataset$screenResolution_height <- as.numeric(sapply(dataset$screenResolution,function(x) {strsplit(x,"x")[[1]][2]}))
dataset$screenResolution_ratio <- dataset$screenResolution_width/dataset$screenResolution_height
dataset$screenResolution <- NULL
dataset$checkoutOptions <- NULL
dataset$dateHour <- as.POSIXct(dataset$dateHour)
dataset$dataSource <- NULL
dataset$searchUsed <- NULL
dataset$searchCategory <- NULL
dataset$referralPath <- NULL
dataset$userDefinedValue <- NULL
dataset$fullReferrer <- NULL
dataset$iscampaign <- as.numeric(!(is.na(dataset$campaign)))
dataset$campaign <- NULL
dataset$isadContent <- as.numeric(!(is.na(dataset$adContent)))
dataset$iskeyword <- as.numeric(!(is.na(dataset$keyword)))
dataset$keyword <- NULL
dataset$adContent <- NULL
dataset$searchKeywordRefinement <- NULL
dataset$source <- NULL
dataset$networkDomain <- NULL
dataset$networkLocation <- NULL
dataset$language[grep("en",dataset$language)] <- "en"
dataset$language[grep("es",dataset$language)] <- "es"
dataset$language[grep("ru",dataset$language)] <- "ru"
dataset$language[grep("fr",dataset$language)] <- "fr"
dataset$language[grep("de",dataset$language)] <- "de"
dataset$language[grep("it",dataset$language)] <- "it"
dataset$language[grep("sv",dataset$language)] <- "sv"
dataset$language[grep("tr",dataset$language)] <- "tr"
dataset$language[grep("zh",dataset$language)] <- "zh"
dataset$language[grep("ro",dataset$language)] <- "ro"
dataset$language[grep("pt",dataset$language)] <- "pt"
dataset$language[grep("nl",dataset$language)] <- "nl"
dataset$language[grep("ko",dataset$language)] <- "ko"
dataset$language[grep("ja",dataset$language)] <- "ja"
dataset$language[grep("el",dataset$language)] <- "el"
dataset$language[grep("fi",dataset$language)] <- "fi"
dataset$language[grep("nb",dataset$language)] <- "nb"
dataset$productAddsToCart <- NULL #only in enhanced ecommerce
dataset$productCheckouts <- NULL #only in enhanced ecommerce
dataset$internalPromotionCTR <- NULL #only in enhanced ecommerce
dataset$productListCTR <- NULL #only in enhanced ecommerce
dataset$productRefunds <- NULL #only in enhanced ecommerce
dataset$productCategory <- NULL #only in enhanced ecommerce
dataset$socialInteractions <- NULL
# dataset$transactions <- NULL #?
# dataset$transactionRevenue <- NULL #?
dataset <- unique(dataset)

#transactions
transactions[transactions=="(not set)"] <- NA
transactions[transactions=="(not provided)"] <- NA
transactions[transactions=="(none)"] <- NA
transactions[transactions=="(direct)"] <- NA
transactions[transactions=="(other)"] <- NA
transactions$browserSize_width <- as.numeric(sapply(transactions$browserSize,function(x) {strsplit(x,"x")[[1]][1]}))
transactions$browserSize_height <- as.numeric(sapply(transactions$browserSize,function(x) {strsplit(x,"x")[[1]][2]}))
transactions$browserSize_ratio <- transactions$browserSize_width/transactions$browserSize_height
transactions$browserSize <- NULL
transactions$users <- NULL
transactions$minute <- NULL
transactions$sourceMedium <- NULL
transactions$mobileDeviceInfo <- NULL
transactions$mobileDeviceMarketingName <- NULL
transactions$region <- NULL
transactions$metro <- NULL
transactions$city <- NULL
transactions$latitude <- NULL
transactions$longitude <- NULL
transactions$flashVersion <- sapply(transactions$flashVersion,function(x) {strsplit(x,"[.]")[[1]][1]})
transactions$screenResolution_width <- as.numeric(sapply(transactions$screenResolution,function(x) {strsplit(x,"x")[[1]][1]}))
transactions$screenResolution_height <- as.numeric(sapply(transactions$screenResolution,function(x) {strsplit(x,"x")[[1]][2]}))
transactions$screenResolution_ratio <- transactions$screenResolution_width/transactions$screenResolution_height
transactions$screenResolution <- NULL
transactions$checkoutOptions <- NULL
transactions$dateHour <- as.POSIXct(transactions$dateHour)
transactions$dataSource <- NULL
transactions$searchUsed <- NULL
transactions$searchCategory <- NULL
transactions$referralPath <- NULL
transactions$userDefinedValue <- NULL
transactions$fullReferrer <- NULL
transactions$iscampaign <- as.numeric(!(is.na(transactions$campaign)))
transactions$campaign <- NULL
transactions$iskeyword <- as.numeric(!(is.na(transactions$keyword)))
transactions$keyword <- NULL
transactions$adContent <- NULL
transactions$searchKeywordRefinement <- NULL
transactions$source <- NULL
transactions$networkDomain <- NULL
transactions$networkLocation <- NULL
transactions$language[grep("en",transactions$language)] <- "en"
transactions$language[grep("es",transactions$language)] <- "es"
transactions$language[grep("ru",transactions$language)] <- "ru"
transactions$language[grep("fr",transactions$language)] <- "fr"
transactions$language[grep("de",transactions$language)] <- "de"
transactions$language[grep("it",transactions$language)] <- "it"
transactions$language[grep("sv",transactions$language)] <- "sv"
transactions$language[grep("tr",transactions$language)] <- "tr"
transactions$language[grep("zh",transactions$language)] <- "zh"
transactions$language[grep("ro",transactions$language)] <- "ro"
transactions$language[grep("pt",transactions$language)] <- "pt"
transactions$language[grep("nl",transactions$language)] <- "nl"
transactions$language[grep("ko",transactions$language)] <- "ko"
transactions$language[grep("ja",transactions$language)] <- "ja"
transactions$language[grep("el",transactions$language)] <- "el"
transactions$language[grep("fi",transactions$language)] <- "fi"
transactions$language[grep("nb",transactions$language)] <- "nb"
transactions$productAddsToCart <- NULL #only in enhanced ecommerce
transactions$productCheckouts <- NULL #only in enhanced ecommerce
transactions$internalPromotionCTR <- NULL #only in enhanced ecommerce
transactions$productListCTR <- NULL #only in enhanced ecommerce
transactions$productRefunds <- NULL #only in enhanced ecommerce
transactions$productCategory <- NULL #only in enhanced ecommerce
transactions$affiliation <- NULL
# transactions$searchResultViews <- NULL
# transactions$searchUniques <- NULL
# transactions$searchSessions <- NULL
# transactions$searchDepth <- NULL
# transactions$searchRefinements <- NULL
# transactions$searchDuration <- NULL
# transactions$searchExits <- NULL
# transactions$organicSearches <- NULL
# transactions$hits <- NULL
transactions$socialInteractions <- NULL
# transactions$bounces <- NULL
transactions <- unique(transactions)

#dataset <- merge(dataset,transactions,all = T) #merge transactions with dataset

#query_dataset
query_dataset$FoundInAnswerID <- NULL
query_dataset$FoundInAnswerPath <- NULL
query_dataset$mpn <- NULL
query_dataset$Price.1 <- NULL
query_dataset$ACDesc <- NULL
query_dataset$Link <- NULL
query_dataset$spec_compare_chart <- NULL
query_dataset$special_to_date <- as.Date(query_dataset$special_to_date)
query_dataset$Title <- NULL
query_dataset$Rank.1 <- NULL
query_dataset$spec_ingredients <- NULL
query_dataset$Image_link <- NULL
query_dataset$children_skus <- NULL
query_dataset$spec_crude_analysis <- NULL
query_dataset$special_from_date <- as.Date(query_dataset$special_from_date)
query_dataset$SKUTextual <- NULL
query_dataset$ACTitle <- NULL
query_dataset$spec_crude_analysis <- NULL
query_dataset$small_image <- NULL
query_dataset$category_position <- NULL
query_dataset$upc <- NULL
query_dataset$spec_directions <- NULL
query_dataset$spec_total_watts_textual <- NULL
query_dataset$IsBestSeller <- NULL 
query_dataset$id <- NULL 
query_dataset$special_to_date <- NULL #exist in products set
query_dataset$special_from_date <- NULL #exist in products set
query_dataset$is_in_stock <- NULL #exist in products set
query_dataset$special_price <- NULL #exist in products set
query_dataset$weight <- NULL #exist in products set
query_dataset$package_id <- NULL #exist in products set
query_dataset$SKU <- NULL 
query_dataset$mag_id <- NULL
names(query_dataset)[1] <- "sku"
query_dataset <- unique(query_dataset)

#Orders
# Orders_Bigals$X <- NULL
# Orders_Bigals$X.1 <- NULL
# Orders_Bigals$X.2 <- NULL
# Orders_Bigals$X.3 <- NULL
# Orders_Bigals$X.4 <- NULL
# Orders_Bigals$X.5 <- NULL
# Orders_Bigals$X.6 <- NULL
# Orders_Bigals$X.7 <- NULL
# Orders_Bigals$X.8 <- NULL
# Orders_Bigals$Date <- as.Date(as.character(Orders_Bigals$Date ))
# Orders_Bigals <- unique(Orders_Bigals)

#products
products$description <- NULL
products$news_to_date <- NULL
products$spec_flavor_op <- NULL
products$thumbnail <- NULL
products$spec_max_head_height <- NULL
products$special_to_date <- as.Date(products$special_to_date)
products$color <- NULL
products$Enabled <- NULL
products$spec_moon_lights <- NULL
products$gift_to <- NULL
products$spec_inlet_size <- NULL
products$link <- NULL
products$group_price <- NULL
products$spec_ingredients <- NULL
products$spec_directions <- NULL
products$children_skus <- NULL
products$staff_selected <- NULL
products$spec_crude_analysis <- NULL
products$meta_keyword <- NULL
products$scent <- NULL
products$flavor <- NULL
products$image_link <- NULL
products$spec_outlet_size <- NULL
products$small_image <- NULL
products$spec_hqi_hid_bulbs_included <- NULL
products$spec_size_op <- NULL
products$spec_pages <- NULL
products$spec_gph <- NULL
products$short_description <- NULL
products$url_key <- NULL
products$original_product_image_link <- NULL
products$meta_description <- NULL
products$spec_minimum_clearance <- NULL
products$spec_dimension <- NULL
products$upc <- NULL
products$category_position <- NULL
products$spec_kelvin_spectrum <- NULL
products$spec_fluo_bulbs_included <- NULL
products$spec_total_watts <- NULL
products$tax_class_id <- NULL
products$spec_compare_chart <- NULL
products$model <- NULL
products$mpn <- NULL
products$list_price <- NULL
products$price_view <- NULL
products$spec_color_op <- NULL
products$special_from_date <- as.Date(products$special_from_date)
products$spec_fan_cooled <- NULL
products$spec_mounting <- NULL
products$gift_from <- NULL
products$gift_value <- NULL
products$status <- NULL
products$news_from_date <- NULL
products$watts <- NULL
products$spec_power_cord_length <- NULL
products$product_sells_count <- NULL
products$minimal_price <- NULL
products$wac <- NULL
products$landed_cost <- NULL
products$manage_stock <- NULL
products$msrp <- NULL
products$type_id <- NULL
products$min_qty <- NULL
products$mag_id <- NULL

#Deal with products category which is clustered hirrecahialy
products_category <- unlist(strsplit(products$category,split = ",Default Category")) #some products belongs to a few categories
products_category <- sapply(products_category,function(x) {strsplit(x,split = ">")[[1]]}) #split by >
products_category <- as.data.frame(t(stri_list2matrix(products_category))) #make as filled data frame
products_category$sku <- NA
products_category$sku[products_category$V1=="Default Category"] <- products$sku[products$category!=""] #brign in the sku number
products_category$sku <- na.locf(products_category$sku) #impute by previous sku number
products_category$V1 <- NULL
names(products_category)[1:4] <- c("Cat1","Cat2","Cat3","Cat4")
products$category <- NULL
products <- merge(products,products_category) #merged back with products table
names(products)[8] <- "productName"
products$productName <- mapply(fixProductName,products$brand,products$productName)
rm(products_category)
products <- unique(products)

#dataset <- merge(dataset,products) #merge with basic dataset

#Seach query cleaning categorizing and word counting. If I'll have the product list can also do tf_idf and if I have ratings I can also check for the rating

#Merge Datasets ####
tmp <- merge(dataset,query_dataset,by="searchkeyword") #this is big merge!
tmp <- merge(tmp,products,by="sku")
tmp2 <- merge(transactions,products,by="productName")
tmp2 <- merge(tmp2,unique(query_dataset[,c("sku","Rank","Price","MapPrice","Popular")]),by="sku") 
tmp3 <- merge(tmp,tmp2,all = T) #we keep also the ones which weren't converged!

#Clean database
tmp3$Cat1 <- as.character(tmp3$Cat1)
tmp3$Cat2 <- as.character(tmp3$Cat2)
tmp3$Cat3 <- as.character(tmp3$Cat3)
tmp3$Cat4 <- as.character(tmp3$Cat4)
tmp3$Cat1[is.na(tmp3$Cat1)] <- "Other"
tmp3$Cat2[is.na(tmp3$Cat2)] <- "Other"
tmp3$Cat3[is.na(tmp3$Cat3)] <- "Other"
tmp3$Cat4[is.na(tmp3$Cat4)] <- "Other"
tmp3 <- tmp3[tmp3$newUsers!=2,]
tmp3 <- tmp3[tmp3$transactions!=2,]
tmp3$medium[tmp3$medium=="Comparison Shopping?gclid=CNbAz9iwl7kCFatj7AodWj4AmQ"] <- "Comparison Shopping"
tmp3$medium[is.na(tmp3$medium)] <- "Non"
tmp3$socialNetwork[is.na(tmp3$socialNetwork)] <- "Non"
tmp3$operatingSystemVersion[is.na(tmp3$operatingSystemVersion)] <- "Non"
tmp3$mobileDeviceBranding[is.na(tmp3$mobileDeviceBranding)] <- "Non"
tmp3$mobileDeviceModel <- NULL
tmp3$mobileInputSelector[is.na(tmp3$mobileInputSelector)] <- "Non"
tmp3$country <- NULL
tmp3$flashVersion[is.na(tmp3$flashVersion)] <- "Other"
tmp3$channelGrouping[tmp3$channelGrouping=="(Other)"] <- "Other"
tmp3$channelGrouping[is.na(tmp3$channelGrouping)] <- "Other"
tmp3$map_price <- NULL
tmp3$MapPrice <- NULL
tmp3$spec_cover_op <- NULL
tmp3$ebay_price <- NULL
tmp3$special_to_date <- NULL
tmp3$special_from_date <- NULL
tmp3$minimum_advertised_price <- NULL
tmp3$browserVersion <- NULL
tmp3$operatingSystemVersion <- NULL
tmp3$weight <- NULL
tmp3$itemQuantity <- NULL
tmp3$transactionId[is.na(tmp3$transactionId)] <- "Non"
tmp3$userType <- as.factor(tmp3$userType)
tmp3$medium <- as.factor(tmp3$medium)
tmp3$socialNetwork <- as.factor(tmp3$socialNetwork)
tmp3$hasSocialSourceReferral <- as.factor(tmp3$hasSocialSourceReferral)
tmp3$browser <- as.factor(tmp3$browser)
tmp3$javaEnabled <- as.factor(tmp3$javaEnabled)
tmp3$screenColors <- as.factor(tmp3$screenColors)
tmp3$flashVersion <- as.factor(tmp3$flashVersion)
tmp3$channelGrouping <- as.factor(tmp3$channelGrouping)
tmp3$month <- as.factor(month(tmp3$dateHour))
tmp3$weekdays <- as.factor(weekdays(tmp3$dateHour))
tmp3$hour <- as.factor(hour(tmp3$dateHour))
tmp3$weekend <- as.factor(is.weekend(tmp3$dateHour))
tmp3$operatingSystem <- as.factor(tmp3$operatingSystem)
tmp3$mobileInputSelector <- as.factor(tmp3$mobileInputSelector)
tmp3$deviceCategory <- as.factor(tmp3$deviceCategory)
tmp3$continent <- as.factor(tmp3$continent)
tmp3$subContinent <- as.factor(tmp3$subContinent)
tmp3$mobileDeviceBranding <- as.factor(tmp3$mobileDeviceBranding)
tmp3$operatingSystemVersion <- as.factor(tmp3$operatingSystemVersion)
tmp3$browserVersion <- as.factor(tmp3$browserVersion)
tmp3$language <- as.factor(as.character(tmp3$language))
tmp3$iscampaign <- as.factor(as.character(tmp3$iscampaign))
tmp3$isadContent <- as.factor(as.character(tmp3$isadContent))
tmp3$iskeyword <- as.factor(as.character(tmp3$iskeyword))
tmp3$is_in_stock <- factor(tmp3$is_in_stock)
tmp3$package_id <- factor(tmp3$package_id)
tmp3$visibility <- factor(tmp3$visibility)
tmp3$web_status_flag <- factor(tmp3$web_status_flag)
tmp3$webflag <- factor(tmp3$webflag)
tmp3$is_saleable <- factor(tmp3$is_saleable)
tmp3$Cat1 <- factor(tmp3$Cat1)
tmp3$Cat2 <- factor(tmp3$Cat2)
tmp3$Cat3 <- factor(tmp3$Cat3)
tmp3$Cat4 <- factor(tmp3$Cat4)
tmp3$year <- NULL #fixed for the moment
tmp3$holiday <- NULL #fixed for the moment
tmp3$news_from_date <- NULL
tmp3$visibility <- NULL
tmp3$cheaper_than_ebay <- NULL
tmp3$special_price <- NULL
tmp3$isadContent <- NULL
tmp3$hasSocialSourceReferral <- NULL
tmp3$transactions <- as.numeric(tmp3$transactions)
tmp3$MatchClassFound[is.na(tmp3$MatchClassFound)] <- "None"
tmp3$MatchClassFound <- factor(tmp3$MatchClassFound)
tmp3$cost <- NULL

#save databgase and make machine learnnig applciations out of it.
tmp3 <- unique(tmp3)
write.csv(tmp3,"data_base.csv",row.names = F)
