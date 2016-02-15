#Initialization
{
  sessionInfo() #system performance
  gc() #clear unsued memory
  rm(list=ls()) # clear workspace
  mainDir <- "C:/Users/user/Documents/R/Facebook"
  setwd(mainDir)
  Sys.setlocale("LC_TIME", "English")
  set.seed(1234) #setting seed for comparison
  library(fpp); library(UsingR)
  library(caret); library(kernlab)
  library(foreach); library(doParallel)
  library(ggplot2); library(h2o)
  library(randomForest); library(date)
  library(data.table); library(bit64)
  library(matrixStats); library(e1071)
  library(FSelector); library(lubridate)
  library(nnet); library(quantmod)
  library(neuralnet); library(minpack.lm)
  library(RSNNS); library(ROCR)
}

#Read Data
{
  train <- read.csv("train.csv",header=T)
  test <- read.csv("test.csv",header=T)
  sampleSubmission <- read.csv("sampleSubmission.csv",header=T)
  auctions <- read.csv("auctions.csv",header=T)
  features <- read.csv("features.csv",header=T)
  train$outcome <- factor(train$outcome) #factor the label
  #factor ID
  bidder_ids <- factor(c(as.character(train$bidder_id),as.character(test$bidder_id)))
  train$bidder_id <- factor(train$bidder_id,levels = levels(bidder_ids))
  test$bidder_id <- factor(test$bidder_id,levels = levels(bidder_ids))
  train$bidder_id <- as.integer(train$bidder_id)
  test$bidder_id <- as.integer(test$bidder_id)
  sampleSubmission$bidder_id <- factor(sampleSubmission$bidder_id,levels = levels(bidder_ids))
  #factor account
  payment_accounts <- factor(c(as.character(train$payment_account),as.character(test$payment_account)))
  train$payment_account <- factor(train$payment_account,levels = levels(payment_accounts))
  test$payment_account <- factor(test$payment_account,levels = levels(payment_accounts))
  train$payment_account <- as.integer(train$payment_account)
  test$payment_account <- as.integer(test$payment_account)
  #factor address
  addresses <- factor(c(as.character(train$address),as.character(test$address)))
  train$address <- factor(train$address,levels = levels(addresses))
  train$address <- as.integer(train$address)
  test$address <- factor(test$address,levels = levels(addresses))
  test$address <- as.integer(test$address)  
}

#Read bids (only once)
{
  #bids_example <- read.csv("bids.csv",header=T,nrows=1, skip=0)
  bids <- fread("bids.csv")
  bids$bid_id <- NULL #dont need those
  bids$bidder_id <- factor(bids$bidder_id,levels = levels(bidder_ids)) #same users from train & test
  bids$bidder_id <- as.integer(bids$bidder_id)
  bids$auction <- factor(bids$auction) #15051 types of auctions
  bids$auction <- as.integer(bids$auction)
  bids$merchandise <- factor(bids$merchandise) #10 merchanadis
  bids$device <- factor(bids$device) #7351 types of devices
  bids$country <- factor(bids$country) #200 countries
  bids$url <- factor(bids$url) #over 1M url's
  bids$url <- as.integer(bids$url)
}

#Extract data per auction from bids (only once)
{
  auctions <- data.frame(auction = rep(0,length(unique(bids$auction))))
  auctions$start <- rep(as.integer64(0),length(unique(bids$auction))) #time need to be in integer64 preallocated
  auctions$end <- rep(as.integer64(0),length(unique(bids$auction))) #time need to be in integer64 preallocated
  for (i in 1:length(unique(bids$auction)))
  {
    cat(i,"\n")
    auction_rows_in_bids <- which(bids$auction==unique(bids$auction)[i])
    auction_data <- bids[c(auction_rows_in_bids),]
    
    #Manipulate the new auction data
    auctions$auction[i] <- auction_data$auction[1]
    auctions$num_of_bids[i] <- nrow(auction_data)
    auctions$merchendise_types[i] <- length(unique(auction_data$merchandise))
    auctions$start[i] <- min.integer64(auction_data$time)
    auctions$end[i] <- max.integer64(auction_data$time)
  }
  write.csv(auctions,"auctions.csv",row.names=F)
}

#Extract data per user from bids (only once)
{
  rm(sampleSubmission,train,test,addresses,payment_accounts,bids_example)
  features <- data.frame(bidder_id = rep(0,length(bidder_ids)))
  features$mean_diff_between_bids <- rep(as.integer64(0),length(bidder_ids))
  features$sd_diff_between_bids <- rep(as.integer64(0),length(bidder_ids))
  features$diff_between_first_last_bids <- rep(as.integer64(0),length(bidder_ids))
  features$mean_diff_between_auction_first_bid_user_auction_first_bid <- rep(as.integer64(0),length(bidder_ids))
  features$mean_diff_between_auction_last_bid_user_auction_last_bid <- rep(as.integer64(0),length(bidder_ids))
  features$mean_bidding_frequency <- rep(as.integer64(0),length(bidder_ids))  
  for (i in 1:length(bidder_ids)) #
  {
    cat(i,"\n")
    user_rows_in_bids <- which(bids$bidder_id==as.integer(bidder_ids[i]))
    if (length(user_rows_in_bids)>0) #if there are any bids from this user
    {
      user_data <- bids[c(user_rows_in_bids),]
      auctions_per_user <- unique(user_data$auction)
      
      #Manipulate the new user data to new features vector 
      features$bidder_id[i] = user_data$bidder_id[1]
      features$num_of_bids[i] <- nrow(user_data)
      features$num_of_auctions[i] <- length(unique(user_data$auction))
      features$avg_num_of_bids_per_auction[i] <- features$num_of_bids[i]/features$num_of_auctions[i]
      features$max_bids_per_auctions[i] <- max(table(user_data$auction))
      features$min_bids_per_auctions[i] <- min(table(user_data$auction))
      features$num_of_merchandise[i] <- length(unique(user_data$merchandise))
      features$type_of_merchandise[i] <- unique(user_data$merchandise)
      features$num_of_auto_parts_bids[i] <- table(user_data$merchandise)[[1]]
      features$num_of_books_music_bids[i] <- table(user_data$merchandise)[[2]]
      features$num_of_clothing_bids[i] <- table(user_data$merchandise)[[3]]
      features$num_of_computers_bids[i] <- table(user_data$merchandise)[[4]]
      features$num_of_furniture_bids[i] <- table(user_data$merchandise)[[5]]
      features$num_of_home_goods_bids[i] <- table(user_data$merchandise)[[6]]
      features$num_of_jewelry_bids[i] <- table(user_data$merchandise)[[7]]
      features$num_of_mobile_bids[i] <- table(user_data$merchandise)[[8]]
      features$num_of_office_equipment_bids[i] <- table(user_data$merchandise)[[9]]
      features$num_of_sporting_goods_bids[i] <- table(user_data$merchandise)[[10]]
      features$num_of_devices[i] <- length(unique(user_data$device))
      features$avg_num_of_bids_per_device[i] <- features$num_of_bids[i]/features$num_of_devices[i]
      features$avg_num_of_auctions_per_device[i] <- features$num_of_auctions[i]/features$num_of_devices[i]
      features$max_bids_per_device[i] <- max(table(user_data$device))
      features$device_of_max_bids[i] <- names(which.max(table(user_data$device)))
      features$num_of_countries[i] <- length(unique(user_data$country))
      features$avg_num_of_bids_per_country[i] <- features$num_of_bids[i]/features$num_of_countries[i]
      features$avg_num_of_auctions_per_country[i] <- features$num_of_auctions[i]/features$num_of_countries[i]
      features$max_bids_per_country[i] <- max(table(user_data$country))
      features$country_with_most_bids[i] <- names(which.max(table(user_data$country)))
      features$num_of_urls[i] <- length(unique(user_data$url))
      features$avg_num_of_bids_per_url[i] <- features$num_of_bids[i]/features$num_of_urls[i]
      features$avg_num_of_auctions_per_url[i] <- features$num_of_auctions[i]/features$num_of_urls[i]
      features$max_bids_per_url[i] <- max(table(user_data$url))
      features$num_of_ips[i] <- length(unique(user_data$ip))
      features$avg_num_of_bids_per_ip[i] <- features$num_of_bids[i]/features$num_of_ips[i]
      features$avg_num_of_auctions_per_ip[i] <- features$num_of_auctions[i]/features$num_of_ips[i]
      features$max_bids_per_ip[i] <- max(table(user_data$ip))    
      features$avg_num_of_ip_per_country[i] <- features$num_of_ips[i]/features$num_of_countries[i]
      features$max_ip_per_country[i] <- max(unlist(lapply(split(user_data$ip,droplevels(user_data$country)), length)))
      features$country_of_max_ips[i] <- names(which.max(lapply((split(user_data$ip,droplevels(user_data$country))), length)))
      features$device_of_max_ips[i] <- names(which.max(lapply((split(user_data$ip,droplevels(user_data$device))), length)))
      features$mean_diff_between_bids[i] <- mean.integer64(diff.integer64(user_data$time))
      features$sd_diff_between_bids[i] <- sd(diff.integer64(user_data$time))
      features$diff_between_first_last_bids[i] <- (max.integer64(user_data$time)-min.integer64(user_data$time))
      features$mean_diff_between_auction_first_bid_user_auction_first_bid[i] <- mean.integer64(sapply(auctions_per_user, function(x) {min.integer64(user_data$time[user_data$auction==x]-auctions$start[auctions$auction==x])}),na.rm = T)
      features$mean_diff_between_auction_last_bid_user_auction_last_bid[i] <- mean.integer64(sapply(auctions_per_user, function(x) {auctions$end[auctions$auction==x]-max.integer64(user_data$time[user_data$auction==x])}),na.rm = T)
      features$mean_ratio_user_bids_total_bids_per_auction[i] <- mean(sapply(auctions_per_user, function(x) {length(user_data$time[user_data$auction==x])/auctions$num_of_bids[auctions$auction==x]}),na.rm = T)
      features$mean_bidding_frequency[i] <- mean.integer64(unlist(sapply(auctions_per_user, function(x) {diff.integer64(user_data$time[user_data$auction==x])})),na.rm = T)
      features$did_put_last_bid_in_auction[i] <- (0 %in% sapply(auctions_per_user, function(x) {auctions$end[auctions$auction==x]-max.integer64(user_data$time[user_data$auction==x])})) 
    } else { #no bids from this user - THOSE USERS SHOULD BE LATER REMOVED FROM TRAIN (OUTLIERS) BUT KEPT IN TEST!
      features$bidder_id[i] = as.integer(bidder_ids[i])
      features$num_of_bids[i] <- 0
      features$num_of_auctions[i] <- 0
      features$avg_num_of_bids_per_auction[i] <- 0
      features$max_bids_per_auctions[i] <- 0
      features$min_bids_per_auctions[i] <- 0
      features$num_of_merchandise[i] <- 0
      features$type_of_merchandise[i] <- NA
      features$num_of_auto_parts_bids[i] <- 0
      features$num_of_books_music_bids[i] <- 0
      features$num_of_clothing_bids[i] <- 0
      features$num_of_computers_bids[i] <- 0
      features$num_of_furniture_bids[i] <- 0
      features$num_of_home_goods_bids[i] <- 0
      features$num_of_jewelry_bids[i] <- 0
      features$num_of_mobile_bids[i] <- 0
      features$num_of_office_equipment_bids[i] <- 0
      features$num_of_sporting_goods_bids[i] <- 0
      features$num_of_devices[i] <- 0
      features$avg_num_of_bids_per_device[i] <- 0
      features$avg_num_of_auctions_per_device[i] <- 0
      features$max_bids_per_device[i] <- 0
      features$device_of_max_bids[i] <- NA
      features$num_of_countries[i] <- 0
      features$avg_num_of_bids_per_country[i] <- 0
      features$avg_num_of_auctions_per_country[i] <- 0
      features$max_bids_per_country[i] <- 0
      features$country_with_most_bids[i] <- NA
      features$num_of_urls[i] <- 0
      features$avg_num_of_bids_per_url[i] <- 0
      features$avg_num_of_auctions_per_url[i] <- 0
      features$max_bids_per_url[i] <- 0
      features$num_of_ips[i] <- 0
      features$avg_num_of_bids_per_ip[i] <- 0
      features$avg_num_of_auctions_per_ip[i] <- 0
      features$max_bids_per_ip[i] <- 0  
      features$avg_num_of_ip_per_country[i] <- 0
      features$max_ip_per_country[i] <- 0
      features$country_of_max_ips[i] <- NA
      features$device_of_max_ips[i] <- NA
      features$mean_diff_between_bids[i] <- NA
      features$sd_diff_between_bids[i] <- NA
      features$diff_between_first_last_bids[i] <- 0
      features$mean_diff_between_auction_first_bid_user_auction_first_bid[i] <- 0
      features$mean_diff_between_auction_last_bid_user_auction_last_bid[i] <- 0
      features$mean_bidding_frequency[i] <- NA
      features$did_put_last_bid_in_auction[i] <- "FALSE"
    }
    
  }
  write.csv(features,"features.csv",row.names=F)
}

#Exploritory Analysis on new data set 
{  
  #extract the outleirs (bidders with no bids) - later add them back, because they're part of the test, not train
  empty_bidders <- features[c(which(features$num_of_bids==0)),]
  features <- features[-c(which(features$num_of_bids==0)),]
    
  #Histograms of each feature in train - before transformations
  M <- ncol(features)
#   for (i in seq(from=24,to=M,by=4)) {
#     par(mfcol=c(2,2), oma=c(1,1,0,0), mar=c(1,1,1,0), tcl=-0.1, mgp=c(0,0,0))
#     hist((features[,i]),100,main = colnames(features)[i])
#     hist((features[,i+1]),100,main = (i+1))
#     hist((features[,i+2]),100,main = (i+2))
#     hist((features[,i+3]),100,main = (i+3))
#     par(mfrow=c(1,1))
#   }
   
  #Feature Engineering
  features$mean_diff_between_bids[features$mean_diff_between_bids<=0] <- 1
  features$mean_diff_between_bids[is.na(features$mean_diff_between_bids)] <- 1
  features$sd_diff_between_bids[features$sd_diff_between_bids<=0] <- 1
  features$sd_diff_between_bids[is.na(features$sd_diff_between_bids)] <- 1
  features$mean_bidding_frequency[features$mean_bidding_frequency<=0] <- 1
  features$mean_bidding_frequency[is.na(features$mean_bidding_frequency)] <- 1
  features$mean_diff_between_bids <- log10(features$mean_diff_between_bids)
  features$sd_diff_between_bids <- log10(features$sd_diff_between_bids)
  features$mean_bidding_frequency <- log10(features$mean_bidding_frequency)
  features$num_of_bids  <- log(features$num_of_bids)
  features$diff_between_first_last_bids[features$diff_between_first_last_bids<=0] <- 1
  features$diff_between_first_last_bids <- log10(features$diff_between_first_last_bids)
  features$diff_between_first_last_bids[which(features$diff_between_first_last_bids<13.5 & features$diff_between_first_last_bids>1)] <- 1
  features$diff_between_first_last_bids[which(features$diff_between_first_last_bids>13.5)] <- 2
  features$diff_between_first_last_bids <- factor(features$diff_between_first_last_bids)
  features$mean_diff_between_auction_first_bid_user_auction_first_bid[features$mean_diff_between_auction_first_bid_user_auction_first_bid<=0] <- 1
  features$mean_diff_between_auction_first_bid_user_auction_first_bid <- log10(features$mean_diff_between_auction_first_bid_user_auction_first_bid)
  features$mean_diff_between_auction_first_bid_user_auction_first_bid[features$mean_diff_between_auction_first_bid_user_auction_first_bid==0] <- 9
  features$mean_diff_between_auction_last_bid_user_auction_last_bid[features$mean_diff_between_auction_last_bid_user_auction_last_bid<=0] <- 1
  features$mean_diff_between_auction_last_bid_user_auction_last_bid <- log10(features$mean_diff_between_auction_last_bid_user_auction_last_bid)
  features$mean_diff_between_auction_last_bid_user_auction_last_bid[features$mean_diff_between_auction_last_bid_user_auction_last_bid==0] <- 8
  features$mean_ratio_user_bids_total_bids_per_auction  <- log(features$mean_ratio_user_bids_total_bids_per_auction)
  features$num_of_auctions  <- log(features$num_of_auctions)
  features$avg_num_of_bids_per_auction  <- log(features$avg_num_of_bids_per_auction)
  features$max_bids_per_auctions  <- log(features$max_bids_per_auctions)
  features$min_bids_per_auctions[features$min_bids_per_auctions>1]  <- 2
  features$min_bids_per_auctions <- factor(features$min_bids_per_auctions)
  features$num_of_merchandise  <- NULL
  features$num_of_auto_parts_bids  <- NULL
  features$num_of_books_music_bids  <- NULL
  features$num_of_clothing_bids  <- NULL
  features$num_of_computers_bids  <- NULL
  features$num_of_furniture_bids  <- NULL
  features$num_of_home_goods_bids  <- NULL
  features$num_of_jewelry_bids  <- NULL
  features$num_of_mobile_bids  <- NULL
  features$num_of_office_equipment_bids  <- NULL
  features$num_of_sporting_goods_bids  <- NULL
  features$num_of_devices  <- log(features$num_of_devices)
  features$avg_num_of_bids_per_device  <- log(features$avg_num_of_bids_per_device)  
  features$avg_num_of_auctions_per_device  <- log(features$avg_num_of_auctions_per_device)  
  features$max_bids_per_device  <- log(features$max_bids_per_device)
  features$num_of_countries  <- log(features$num_of_countries)
  features$avg_num_of_bids_per_country  <- log(features$avg_num_of_bids_per_country)
  features$avg_num_of_auctions_per_country  <- log(features$avg_num_of_auctions_per_country)
  features$max_bids_per_country  <- log(features$max_bids_per_country)
  features$num_of_urls  <- log(features$num_of_urls)
  features$avg_num_of_bids_per_url  <- log(features$avg_num_of_bids_per_url)
  features$avg_num_of_auctions_per_url  <- log(features$avg_num_of_auctions_per_url)
  features$num_of_ips  <- log(features$num_of_ips)
  features$max_bids_per_url  <- log(features$max_bids_per_url)
  features$avg_num_of_bids_per_ip  <- log(features$avg_num_of_bids_per_ip)
  features$avg_num_of_auctions_per_ip  <- log(features$avg_num_of_auctions_per_ip)
  features$max_bids_per_ip  <- log(features$max_bids_per_ip)
  features$avg_num_of_ip_per_country  <- log(features$avg_num_of_ip_per_country)
  features$max_ip_per_country  <- log(features$max_ip_per_country)
  features$type_of_merchandise <- factor(features$type_of_merchandise)  
  features$num_of_bids[features$num_of_bids==0] <- 1
  features$num_of_bids  <- log(features$num_of_bids)
  features$avg_num_of_bids_per_auction[features$avg_num_of_bids_per_auction==0] <- 0.1
  features$avg_num_of_bids_per_auction  <- log(features$avg_num_of_bids_per_auction)
  features$avg_num_of_bids_per_device[features$avg_num_of_bids_per_device==0] <- 0.01
  features$avg_num_of_bids_per_device  <- log(features$avg_num_of_bids_per_device)
  features$max_bids_per_device[features$max_bids_per_device==0] <- 0.5
  features$max_bids_per_device  <- log(features$max_bids_per_device)
  features$avg_num_of_bids_per_country[features$avg_num_of_bids_per_country==0] <- 0.01
  features$avg_num_of_bids_per_country  <- log(features$avg_num_of_bids_per_country)
  features$max_bids_per_country[features$max_bids_per_country==0] <- 0.25
  features$max_bids_per_country  <- log(features$max_bids_per_country)
  features$avg_num_of_bids_per_url[features$avg_num_of_bids_per_url==0] <- 0.01
  features$avg_num_of_bids_per_url  <- log(features$avg_num_of_bids_per_url)
  features$avg_num_of_bids_per_ip[features$avg_num_of_bids_per_ip==0] <- 0.01
  features$avg_num_of_bids_per_ip  <- log(features$avg_num_of_bids_per_ip)
  features$avg_num_of_ip_per_country[features$avg_num_of_ip_per_country<=0] <- 0.01
  features$avg_num_of_ip_per_country  <- log(features$avg_num_of_ip_per_country)
  features$max_ip_per_country[features$max_ip_per_country==0] <- 0.25
  features$max_ip_per_country  <- log(features$max_ip_per_country)
  features$did_put_last_bid_in_auction <- factor(features$did_put_last_bid_in_auction)
  #combine the next factors
  features$device_of_max_bids <- factor(features$device_of_max_bids)
  names_device_of_max_bids <- names(which(table(features$device_of_max_bids)<=20))
  levels(features$device_of_max_bids)[levels(features$device_of_max_bids) %in% names_device_of_max_bids] <- "Other"
  features$device_of_max_bids <- factor(features$device_of_max_bids)
  features$country_with_most_bids <- factor(features$country_with_most_bids)
  names_country_with_most_bids <- names(which(table(features$country_with_most_bids)<=5))
  levels(features$country_with_most_bids)[levels(features$country_with_most_bids) %in% names_country_with_most_bids] <- ""
  features$country_with_most_bids <- factor(features$country_with_most_bids)  
  features$country_of_max_ips <- factor(features$country_of_max_ips)
  names_country_of_max_ips <- names(which(table(features$country_of_max_ips)<=5))
  levels(features$country_of_max_ips)[levels(features$country_of_max_ips) %in% names_country_of_max_ips] <- ""
  features$country_of_max_ips <- factor(features$country_of_max_ips)  
  features$device_of_max_ips <- factor(features$device_of_max_ips)
  names_device_of_max_ips <- names(which(table(features$device_of_max_ips)<=21))
  levels(features$device_of_max_ips)[levels(features$device_of_max_ips) %in% names_device_of_max_ips] <- "Other"
  features$device_of_max_ips <- factor(features$device_of_max_ips)
  #Remvoe same features in empty bidders also
  empty_bidders$num_of_merchandise  <- NULL
  empty_bidders$num_of_auto_parts_bids  <- NULL
  empty_bidders$num_of_books_music_bids  <- NULL
  empty_bidders$num_of_clothing_bids  <- NULL
  empty_bidders$num_of_computers_bids  <- NULL
  empty_bidders$num_of_furniture_bids  <- NULL
  empty_bidders$num_of_home_goods_bids  <- NULL
  empty_bidders$num_of_jewelry_bids  <- NULL
  empty_bidders$num_of_mobile_bids  <- NULL
  empty_bidders$num_of_office_equipment_bids  <- NULL
  empty_bidders$num_of_sporting_goods_bids  <- NULL
  #Scaling
  features$mean_diff_between_auction_first_bid_user_auction_first_bid <- scale(features$mean_diff_between_auction_first_bid_user_auction_first_bid)
  features$mean_diff_between_auction_last_bid_user_auction_last_bid <- scale(features$mean_diff_between_auction_last_bid_user_auction_last_bid)
  features$avg_num_of_bids_per_auction <- scale(features$avg_num_of_bids_per_auction)
  features$avg_num_of_auctions_per_device <- scale(features$avg_num_of_auctions_per_device)
  features$avg_num_of_auctions_per_ip <- scale(features$avg_num_of_auctions_per_ip)  
}

#Build Train & Test
{  
  train_tmp <- subset(features,bidder_id %in% train$bidder_id)
  test_tmp <- subset(features,bidder_id %in% test$bidder_id)
  empty_bidders <- empty_bidders[which(empty_bidders$bidder_id %in% test$bidder_id),]
  test_tmp <- rbind(test_tmp,empty_bidders) #this will generate a warning
  train <- cbind(train_tmp,train[which(train$bidder_id %in% train_tmp$bidder_id),c(2:4)]) #new train is with no outliers
  train <- train[order(train$bidder_id),]
  train$outcome <- factor(train$outcome)
  test <- cbind(test_tmp[order(test$bidder_id),],test[order(test_tmp$bidder_id),c(2:3)]) #sort the new test accoring to numeric bidders id - not its not sumple submission order!
  test <- test[order(test$bidder_id),]
  test$did_put_last_bid_in_auction[is.na(test$did_put_last_bid_in_auction)] <- "FALSE" #this is for the 70 outlier bidders which inserted NA - but their scoer isn't matters  
  test$country_of_max_ips[is.na(test$country_of_max_ips)] <- "" #this is for the 70 outlier bidders which inserted NA - but their scoer isn't matters  
  test$device_of_max_ips[is.na(test$device_of_max_ips)] <- "Other" #this is for the 70 outlier bidders which inserted NA - but their scoer isn't matters  
  test$country_with_most_bids[is.na(test$country_with_most_bids)] <- "" #this is for the 70 outlier bidders which inserted NA - but their scoer isn't matters  
  test$device_of_max_bids[is.na(test$device_of_max_bids)] <- "Other" #this is for the 70 outlier bidders which inserted NA - but their scoer isn't matters  
  test$country_of_max_ips <- factor(test$country_of_max_ips)
  test$device_of_max_ips <- factor(test$device_of_max_ips)
  test$country_with_most_bids <- factor(test$country_with_most_bids)
  test$device_of_max_bids <- factor(test$device_of_max_bids)
  test[is.na(test)] <- 1 #this is for the 70 outlier bidders which inserted NA - but their scoer isn't matters  
  rm(train_tmp,test_tmp,features,empty_bidders,auctions,addresses,payment_accounts,names_country_of_max_ips,names_country_with_most_bids,names_device_of_max_bids,names_device_of_max_ips) #clear memory
}

#Search features according to Mutual Information Criteria
{
  weights_mutual_info <- information.gain(outcome~.,data=train[,-c(1,38,39)])
  print(sort(weights_mutual_info$attr_importance))
  subset_mutual_info <- NULL
  subset_mutual_info <- row.names(weights_mutual_info)[which(weights_mutual_info$attr_importance > 0.05)]        
  features_MI <- as.simple.formula(subset_mutual_info, "outcome")
}

#function for feature selection evaluation on RF
evaluator_model <- function(subset) 
{  
  #cat('Current Subset:' ,subset, "\n")
  auc_train <- rep(0,(K*Times))
  auc_eval <- rep(0,(K*Times))
  for (i in 1:(K*Times))
  {
    model <- randomForest(as.simple.formula(subset,"outcome"),data = train[c(folds[[i]]),-c(1,38,39)],
                          metric = "ROC",importance = T,ntree=ntrees[j]*4,keep.forest=T)  
    
    train_predictions <- model$votes[,2]/(model$votes[,2]+model$votes[,1]) #OOB       
    pred_auc_train <- prediction(train_predictions,train$outcome[c(folds[[i]])])
    auc.tmp.train <- performance(pred_auc_train,"auc")
    auc_train[i] <- as.numeric(auc.tmp.train@y.values)
    
    eval_predictions <- predict(model,train[-c(folds[[i]]),-c(1,38,39)],"prob")
    pred_auc_eval <- prediction(eval_predictions[,2],train$outcome[-c(folds[[i]])])
    auc.tmp.eval <- performance(pred_auc_eval,"auc")
    auc_eval[i] <- as.numeric(auc.tmp.eval@y.values)
    
    rm(model,train_predictions,eval_predictions)
  }
  cat('Current AUC OOB:' ,mean(auc_train), "\n")
  cat('Current AUC EVAL:' ,mean(auc_eval), "\n")  
  return(mean(auc_train)+mean(auc_eval)-abs(mean(auc_train)-mean(auc_eval))) #optimize on maximum (AUCtrain + AUCeval -|AUCtrain - AUCeval|)
}

#Train a Random Forest with respect to ROC and calcualte AUC. use 5fold CV and compare mean validation to OOB
{
  #Divide to train and eval - multifolds & other initializations
  {
    K <- 3 #number of folds (70-30)
    Times <- 2 #number of resamples (5)
    folds <- createMultiFolds(train$bidder_id,k=K,times = Times)
    M <- ncol(train)
    N <- nrow(train) #Number of real samples (rows)  
    ntrees <- c(250) # vector of trees, it is then multiplied by number of cpus (4)
    train_score_ntrees <- rep(0,length(ntrees))
    eval_score_ntrees <- rep(0,length(ntrees))
  }
  
  #Training RF and calulating AUC
  {
    for (j in 1:length(ntrees))
    { 
      subset_bfs <- NULL
      subset_bfs <- hill.climbing.search(sample(names(train[,-c(1,38,39,40)])),evaluator_model)
      features_bfs <- as.simple.formula(subset_bfs, "outcome") 
      bfs_cols <- which(colnames(train) %in% subset_bfs)
      
      train_Score <- rep(0,K*Times)
      eval_Score <- rep(0,K*Times)
      for (i in 1:(K*Times))
      {
        #Train
        cl<-makeCluster(4)
        registerDoParallel(cl)
        model_RF <- foreach(ntree=rep(1, 4), .combine=combine, .verbose=T, .packages='randomForest') %dopar% {
                 randomForest(y=train$outcome[c(folds[[i]])],x=train[c(folds[[i]]),-c(1,38,39,40)],xtest = train[c(folds[[i]]),-c(1,38,39,40)],
                 ytest = train$outcome[c(folds[[i]])],metric = "ROC", importance = T,ntree=ntree,do.trace=T,keep.forest=T)}    
        stopCluster(cl)
        #varImpPlot(model_RF)
        #sampsize = c(samp_vector[i],table(train$outcome[c(folds[[i]])])[[2]]),
        
        train_predictions <- model_RF$votes[,2]/(model_RF$votes[,2]+model_RF$votes[,1]) #we look at the OOB error for comparing to eval        
        pred_auc_train <- prediction(train_predictions,train$outcome[c(folds[[i]])])
        auc.tmp.train <- performance(pred_auc_train,"auc")
        auc_train <- as.numeric(auc.tmp.train@y.values)
        train_Score[i] <- auc_train
        train.rf.perf = performance(pred_auc_train,"tpr","fpr")    
        #plot the curve
        plot(train.rf.perf,main="ROC Curve",col=2,lwd=2)
        abline(a=0,b=1,lwd=2,lty=2,col="gray")
        
        #Validation
        eval_predictions <- predict(model_RF,train[-c(folds[[i]]),-c(1,38,39,40)],"prob")
        pred_auc_eval <- prediction(eval_predictions[,2],train$outcome[-c(folds[[i]])])
        auc.tmp.eval <- performance(pred_auc_eval,"auc")
        auc_eval <- as.numeric(auc.tmp.eval@y.values)
        eval_Score[i] <- auc_eval    
        eval.rf.perf = performance(pred_auc_eval,"tpr","fpr")    
        #plot the curve
        plot(eval.rf.perf,main="ROC Curve for Validation",col=3,lwd=2,add=T)
 
      #rm(train_predictions,eval_predictions,pred_auc_eval,pred_auc_train,eval.rf.perf,train.rf.perf,model_RF)
      }
      train_score_ntrees[j] <- mean(train_Score)
      eval_score_ntrees[j] <- mean(eval_Score)
    }
    #Plot eval and train score vs. number of trees
    plot(y=train_score_ntrees,ntrees,main="AUC vs. Number of Trees, 5-folds",col=2,lwd=2,xlab="Number of Trees",ylab="AUC",type = "l")
    lines(y=eval_score_ntrees,ntrees,col=3,lwd=2)
  }  
}

#Test RF Model and save new submission file
{
  samp_vector <- seq(100,1881,200)
  ntrees <- seq(10,250,30)
  auc_train <- matrix(0,length(ntrees),length(samp_vector))
  for (i in 1:length(ntrees))
  {
    for (j in 1:length(samp_vector))
    {
      cl<-makeCluster(4)
      registerDoParallel(cl)
      model_RF <- foreach(ntree=rep(ntrees[9], 4), .combine=combine, .verbose=T, .packages='randomForest') %dopar% {
                    randomForest(y=train$outcome,x=train[,c(bfs_cols)],xtest = train[,c(bfs_cols)],sampsize = c(samp_vector[9],table(train$outcome)[[2]]),
                     ytest = train$outcome,metric = "ROC", importance = T,ntree=ntree,do.trace=T,keep.forest=T)}    
      stopCluster(cl)
      #importance(model_RF)
      train_predictions <- model_RF$votes[,2]/(model_RF$votes[,2]+model_RF$votes[,1]) #we look at the OOB error for comparing to eval
      
      pred_auc_train <- prediction(train_predictions,train$outcome)
      auc.tmp.train <- performance(pred_auc_train,"auc")
      auc_train[i,j] <- as.numeric(auc.tmp.train@y.values)
      train.rf.perf = performance(pred_auc_train,"tpr","fpr")    
      #plot the curve
      plot(train.rf.perf,main="ROC Curve, Final",col=2,lwd=2)
      abline(a=0,b=1,lwd=2,lty=2,col="gray")
    }
  }
  
  #plot AUC vs. samp size
  plot(samp_vector,auc_train,type = "b")
  
  #Testing
  test_predictions <- predict(model_RF,test[,c(bfs_cols)],"prob")
  #probability for bot
  for (i in 1:nrow(sampleSubmission))
  {
    sampleSubmission$prediction[i] <- test_predictions[which(test$bidder_id==as.integer(sampleSubmission$bidder_id[i])),2]
  }
  write.csv(sampleSubmission,"Submission_new.csv",row.names=F)
  
}

#Read different submissions and analyze
{
  Submission1 <- read.csv("Submission_new_500_h2o_deepNN_50_50_50_0.898.csv",header=T)
  Submission2 <- read.csv("Submission_new1_200_trees_0.918.csv",header=T)
  Submission3 <- read.csv("Submission_new_1000_tress_0.90723.csv",header=T)
  Submission4 <- read.csv("Submission_new_500_trees_0.909.csv",header=T)
  Submission5 <- read.csv("Submission_200trees_400_103_festures_selection_0.902.csv",header=T)
  total_Submission <- cbind(Submission1,Submission2[,2])
  total_Submission$mean <- rowMeans(total_Submission[,c(2:3)])
  total_Submission$sd <- rowSds(as.matrix(total_Submission[,c(2:3)]))
  #find the best theroshold for 1 and 0
  sampleSubmission[,2] <- total_Submission$mean
  write.csv(sampleSubmission,"Submission_new.csv",row.names=F)  
}

#Train H2O NN 
{
  #Divide to train and eval - multifolds & other initializations
  {
    K <- 3 #number of folds (70-30)
    Times <- 2 #number of resamples (5)
    folds <- createMultiFolds(train$bidder_id,k=K,times = Times)
    M <- ncol(train)
    N <- nrow(train) #Number of real samples (rows)  
  }
    
  ## Start a local cluster & load model
  #h2o.shutdown(localH2O)
  localH2O <- h2o.init(nthreads = -1,ip = "localhost", port = 54321, startH2O = TRUE) #start h2o connection
  #model <- h2o.loadModel(localH2O, "./mymodel")
  test_hex <- as.h2o(localH2O, test, key = 'test') #make test set h2o object
  
  ## Create a set of netwok topologies
  hidden_layers <- list(c(100,100,100))
  epochs_layers <- c(1,20)
  l1_regularizations <- c(1e-5)
  input_dropout_ratios_list <- c(0.1)
  hidden_dropout_ratios_list <- list(c(0.2,0.2,0.2))
  
  train_Score <- rep(0,K*Times)
  eval_Score <- rep(0,K*Times)
  for (i in 1:(K*Times))
  {
    ## Import Data to H2O Cluster
    train_hex <- as.h2o(localH2O,train[c(folds[[i]]),], key = 'train') #make train set h2o object 
    eval_hex <- as.h2o(localH2O,train[-c(folds[[i]]),], key = 'eval') #make train set h2o object 

    ## Train a Deep Neural Network
    #h2o.rm(localH2O, keys = c("train.hex"))
    model <- h2o.deeplearning(x = 2:37,
                              y = 40,
                              data = train_hex,
                              validation = eval_hex,
                              activation = "RectifierWithDropout",
                              input_dropout_ratio = input_dropout_ratios_list, 
                              hidden_dropout_ratios = hidden_dropout_ratios_list,
                              hidden = hidden_layers,
                              l1 = l1_regularizations,
                              epochs = epochs_layers*50,
                              classification = T,
                              balance_classes = F,
                              nesterov_accelerated_gradient = T,
                              override_with_best_model = T)
    
    train_Score[i] <- model@model[[2]]@model$train_auc
    eval_Score[i] <- model@model[[2]]@model$auc
    #rm(train_hex,eval_hex,model)
    #for (m in 1:20) {cat(model@model[[m]]@model$auc,"\n")}
  }
  train_mean_auc_h2o <- mean(train_Score)
  eval_mean_auc_h2o <- mean(eval_Score)
}

#Test H2O NN 
{
  ## Import Data to H2O Cluster
  train_hex <- as.h2o(localH2O,train, key = 'train') #make train set h2o object 
    
  ## Train a Deep Neural Network
  #h2o.rm(localH2O, keys = c("train.hex"))
  model <- h2o.deeplearning(x = 2:37,
                            y = 40,
                            data = train_hex,
                            activation = "RectifierWithDropout",
                            input_dropout_ratio = input_dropout_ratios_list, 
                            hidden_dropout_ratios = hidden_dropout_ratios_list,
                            hidden = hidden_layers,
                            l1 = l1_regularizations,
                            epochs = epochs_layers*50,
                            classification = T,
                            balance_classes = F,
                            nesterov_accelerated_gradient = T,
                            override_with_best_model = T)

  result_test <- as.data.frame(h2o.predict(model@model[[2]], test_hex))
  
  #probability for bot
  for (i in 1:nrow(sampleSubmission))
  {
    sampleSubmission$prediction[i] <- result_test[which(test$bidder_id==as.integer(sampleSubmission$bidder_id[i])),3]
  }
  write.csv(sampleSubmission,"Submission_new.csv",row.names=F)
  
}