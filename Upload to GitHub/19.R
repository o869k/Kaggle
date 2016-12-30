#Initialization
{
  sessionInfo() #system performance
  gc() #clear unsued memory
  rm(list=ls()) # clear workspace
  setwd(mainDir)
  Sys.info()
  set.seed(1234) #setting seed for comparison
  library(RPostgreSQL); library(rJava)
  library(RJDBC); library(reshape2)
  library(dplyr); library(zoo)
  library(lubridate); library(plyr)
  library(doParallel); library(foreach)
  options(java.parameters = "- Xmx8g")
  library(mvoutlier);
  options(java.parameters = "- Xmx4g")
  options(digits=12)
  op <- options()
}

#Input arguments : should be updated to relevant inputs and deafult values
{
args=(commandArgs(TRUE))
if(length(args)==0){
  print("No arguments supplied.")
  ##supply default values
  th <-  100 #default therehold is 100
  max_window <- 5 #number of cycles back to compare the bill shock, upper bound
  min_window <- 2 #number of cycles back to compare the bill shock, lower bound
  save_all <- 0 #save all results in pdfs - Added omerpe 2016-06-07
  reg_zone_threshold <- 5 #numbe rof max units per day regarded as regular zone / n outlier or statisticla analysis is problematic
  plot_graphs <- 0 #plotter
}else{
  for(i in 1:length(args)){
    eval(parse(text=args[[i]]))
  }
}
}

#Functions
{
cycle_row_data <- function(x) {
which(x<=bill_shock_cycles$cycle_end_date & x>=bill_shock_cycles$cycle_start_date)
}

}

#Using RJDBC with AWS RS optimized driver (using SQL queries)
{
# connect to Amazon Redshift
driver <- JDBC("com.amazon.redshift.jdbc41.Driver", "/home/USERNAME/RedshiftJDBC41-1.1.13.1013.jar", identifier.quote="`")
>>>>>>> 5d1e471ac8fad5db30dc4af6342a52569bbe3a3b
conn <- dbConnect(driver, url)
}

##### Read Data from RS to R and build datasets ####
#General data
dim_cdp <- dbGetQuery(conn, "select * from dim_cdp")
dim_cdp <- dim_cdp[order(dim_cdp$n_cdp_id),]
dim_country <- dbGetQuery(conn, "select * from dim_country")
dim_period <- dbGetQuery(conn, "select * from dim_period")
dim_cdp <- dim_cdp[!(dim_cdp$n_cdp_id==21),] #Remove Providors CDP!

#Cycles data
dim_cdp_cycle_dates <- dbGetQuery(conn, "select * from dim_cdp_cycle_dates")
dim_cdp_cycle_dates$cycle_start_date <- as.Date(dim_cdp_cycle_dates$cycle_start_date)
dim_cdp_cycle_dates$cycle_end_date <- as.Date(dim_cdp_cycle_dates$cycle_end_date)
dim_cdp_cycle_dates$last_day_of_cycle <- as.Date(dim_cdp_cycle_dates$last_day_of_cycle)
dim_cdp_cycle_dates$cycle_month <- as.Date(dim_cdp_cycle_dates$cycle_month)
dim_cdp_cycle_dates <- dim_cdp_cycle_dates[order(dim_cdp_cycle_dates$n_cdp_id,dim_cdp_cycle_dates$cycle_start_date),]

#Invoice data - this is only apriori - can't use it in real time
fact_cdp_invoice <- dbGetQuery(conn, "select * from fact_cdp_invoice")
fact_cdp_invoice$t_cycle_start_date <- as.Date(fact_cdp_invoice$t_cycle_start_date)
fact_cdp_invoice$t_invoice_date <- as.Date(fact_cdp_invoice$t_invoice_date)
fact_cdp_invoice$t_due_date <- as.Date(fact_cdp_invoice$t_due_date)
fact_cdp_invoice$t_cycle_end_date <- as.Date(fact_cdp_invoice$t_cycle_end_date)
fact_cdp_invoice <- fact_cdp_invoice[order(fact_cdp_invoice$n_cdp_id,fact_cdp_invoice$t_cycle_start_date),]

#Usage data per CDP and zone, each day
r_cdp_zone_date_usage <- dbGetQuery(conn, "select * from r_cdp_zone_date_usage")
r_cdp_zone_date_usage$t_start_time_date <- as.Date(r_cdp_zone_date_usage$t_start_time_date)
r_cdp_zone_date_usage[is.na(r_cdp_zone_date_usage)] <- 0 #all NA are infact 0 consumption
r_cdp_zone_date_usage <- r_cdp_zone_date_usage[order(r_cdp_zone_date_usage$n_cdp_id,r_cdp_zone_date_usage$t_start_time_date),]
r_cdp_zone_date_usage$s_zone[r_cdp_zone_date_usage$s_zone=="AT&T Domestic Premium"] <- "AT&T - Domestic Premium"

#Activations data per CDP each day
r_cdp_cycle_day_active <- dbGetQuery(conn, "select * from r_cdp_cycle_day_active")
r_cdp_cycle_day_active$cycle_month <- as.Date(r_cdp_cycle_day_active$cycle_month)
r_cdp_cycle_day_active$date <- as.Date(r_cdp_cycle_day_active$date)
r_cdp_cycle_day_active <- r_cdp_cycle_day_active[order(r_cdp_cycle_day_active$n_cdp_id,r_cdp_cycle_day_active$date),]
names(r_cdp_cycle_day_active)[3] <- "t_start_time_date"

##### Go Over each cdp and analyze bill shock ####
results_tot <- NULL #final result dataset
cdp_list <- intersect(unique(r_cdp_zone_date_usage$n_cdp_id),unique(dim_cdp_cycle_dates$n_cdp_id))
<<<<<<< HEAD
for (k in 1:length(cdp_list))
=======
for (cdp in cdp_list)
>>>>>>> 5d1e471ac8fad5db30dc4af6342a52569bbe3a3b
{
  #### Consumption Data
  cat(cdp,"\n")
  bill_shock_cycles <- dim_cdp_cycle_dates[dim_cdp_cycle_dates$n_cdp_id==cdp,]  #read the cdp cycles data
  bill_shock_consumption <- r_cdp_zone_date_usage[r_cdp_zone_date_usage$n_cdp_id==cdp,] #read the cdp consumption history
  bill_shock_consumption$s_zone <- tolower(gsub(" ", "",bill_shock_consumption$s_zone, fixed = TRUE)) #remove empty spacing + lower case strings
  bill_shock_consumption$s_country_code <- tolower(gsub(" ", "",bill_shock_consumption$s_country_code, fixed = TRUE)) #remove empty spacing + lower case strings
  bill_shock_invoice <- fact_cdp_invoice[fact_cdp_invoice$n_cdp_id==cdp,] #using invoice data
  
  #Remove unwatned billable columns
  bill_shock_consumption$total_duration <- NULL
  bill_shock_consumption$total_session_count <- NULL
  bill_shock_consumption$total_zero_data_session_count <- NULL
  bill_shock_consumption$billable_duration <- NULL
  bill_shock_consumption$billable_session_count <- NULL
  bill_shock_consumption$total_incoming_sms <- NULL
  bill_shock_consumption$billable_incoming_sms <- NULL
  
  #Normalize to MB (Was KB) the data
  bill_shock_consumption$total_data_kb <- bill_shock_consumption$total_data_kb/1000
  bill_shock_consumption$billable_data_kb <- bill_shock_consumption$billable_data_kb/1000

  #decide how many first cycle to remove (cdp relevant)
  if (cdp==11) {bill_shock_consumption <- bill_shock_consumption[-c(1:5),]}
  if (cdp==23) {bill_shock_consumption <- bill_shock_consumption[-c(1:4),]}
  if (cdp==30) {bill_shock_consumption <- bill_shock_consumption[-c(1:2),]}
  if (cdp==35) {bill_shock_consumption <- bill_shock_consumption[-c(1:1),]}
  
  #Complete the missing dates for each of the pricing zones, and fill missing data with 0 consumptions.
  bill_shock_consumption <- merge(bill_shock_consumption,
    expand.grid(n_cdp_id=unique(bill_shock_consumption$n_cdp_id),
      t_start_time_date=seq.Date(min(bill_shock_consumption$t_start_time_date),max(bill_shock_consumption$t_start_time_date),by="day"),
      s_zone=unique(bill_shock_consumption$s_zone)),all=TRUE)
  bill_shock_consumption[is.na(bill_shock_consumption)] <- 0
  
  #Add the cycle data (this is the reference at each time)
  bill_shock_consumption$cycle_start_date <- as.Date(sapply(bill_shock_consumption$t_start_time_date,function(x) {as.character(bill_shock_cycles$cycle_start_date[cycle_row_data(x)])}))
  bill_shock_consumption$cycle_end_date <- as.Date(sapply(bill_shock_consumption$t_start_time_date,function(x) {as.character(bill_shock_cycles$cycle_end_date[cycle_row_data(x)])}))
  bill_shock_consumption$cycle_month <- as.Date(sapply(bill_shock_consumption$t_start_time_date,function(x) {as.character(bill_shock_cycles$cycle_month[cycle_row_data(x)])}))
  bill_shock_consumption$last_day_of_cycle <- as.Date(sapply(bill_shock_consumption$t_start_time_date,function(x) {as.character(bill_shock_cycles$last_day_of_cycle[cycle_row_data(x)])})) #indicator of last day in cycle
  bill_shock_consumption$day_in_cycle <- bill_shock_consumption$t_start_time_date - bill_shock_consumption$cycle_start_date + 1 #indicator of day in cycle
  bill_shock_consumption$last_day_of_cycle <- bill_shock_consumption$last_day_of_cycle==bill_shock_consumption$t_start_time_date #indicator of last day of cycle
  #remove the first cycle for CDP (it is not compelete), or: start the analysis from the first compelte cycle availble - if even exist
  start_idx <- 1
  if (as.numeric(bill_shock_consumption$day_in_cycle[1])!=1) 
  {
    start_idx <- which(bill_shock_consumption$day_in_cycle==1)[1]-1
    if (!is.na(start_idx)) {bill_shock_consumption <- bill_shock_consumption[-c(1:start_idx),]}
  } 
  bill_shock_consumption <- bill_shock_consumption[order(bill_shock_consumption$s_zone,bill_shock_consumption$cycle_month),] #order by cycle_month and s_zone for aggregations
  if (is.na(start_idx)) {next}
  
  #Add invoice data as reference per CDP - if availble - if want to remove invocice integration we can remvoe this part
  reference_date_for_invoice <- bill_shock_consumption$t_start_time_date[1]
  invoice_merge <- FALSE
  if (nrow(bill_shock_invoice)>0 & sum(bill_shock_invoice$t_cycle_end_date<=reference_date_for_invoice)>1) {
    bill_shock_invoice <- bill_shock_invoice[bill_shock_invoice$t_cycle_end_date<=reference_date_for_invoice,c("t_cycle_end_date","f_data_volume","n_sms_volume","n_voice_volume","o_total_terminals","n_cdp_id")] #until the first cycle
    names(bill_shock_invoice) <- c("t_start_time_date","billable_data_kb","billable_outgoing_sms","billable_voice_duration","total_working_units","n_cdp_id")
    bill_shock_invoice$s_country_code <- max(bill_shock_consumption$s_country_code)
    bill_shock_invoice$s_zone <- max(bill_shock_consumption$s_zone)
    bill_shock_invoice$total_data_kb <- bill_shock_invoice$billable_data_kb
    bill_shock_invoice$total_outgoing_sms <- bill_shock_invoice$billable_outgoing_sms
    bill_shock_invoice$total_voice_duration <- bill_shock_invoice$billable_voice_duration
    cycles_for_invoice <- !(bill_shock_invoice$total_working_units==0)
    cycles_for_invoice[length(cycles_for_invoice)-1] <- TRUE
    bill_shock_invoice <- bill_shock_invoice[cycles_for_invoice,] #only with avaible units
    if (nrow(bill_shock_invoice)>max_window) {bill_shock_invoice <- bill_shock_invoice[(nrow(bill_shock_invoice)-max_window-1):nrow(bill_shock_invoice),]} #save the last max_windows invoice data
    
    bill_shock_invoice <- merge(bill_shock_invoice,
      expand.grid(n_cdp_id=unique(bill_shock_invoice$n_cdp_id),
        t_start_time_date=seq.Date(min(bill_shock_invoice$t_start_time_date),max(bill_shock_invoice$t_start_time_date),by="day"),
        s_zone=unique(bill_shock_invoice$s_zone)),all=TRUE)
    bill_shock_invoice[is.na(bill_shock_invoice)] <- 0
    
    #Add the cycle data (this is the reference at each time)
    bill_shock_invoice$cycle_start_date <- as.Date(sapply(bill_shock_invoice$t_start_time_date,function(x) {as.character(bill_shock_cycles$cycle_start_date[cycle_row_data(x)])}))
    bill_shock_invoice$cycle_end_date <- as.Date(sapply(bill_shock_invoice$t_start_time_date,function(x) {as.character(bill_shock_cycles$cycle_end_date[cycle_row_data(x)])}))
    bill_shock_invoice$cycle_month <- as.Date(sapply(bill_shock_invoice$t_start_time_date,function(x) {as.character(bill_shock_cycles$cycle_month[cycle_row_data(x)])}))
    bill_shock_invoice$last_day_of_cycle <- as.Date(sapply(bill_shock_invoice$t_start_time_date,function(x) {as.character(bill_shock_cycles$last_day_of_cycle[cycle_row_data(x)])})) #indicator of last day in cycle
    bill_shock_invoice$day_in_cycle <- bill_shock_invoice$t_start_time_date - bill_shock_invoice$cycle_start_date + 1 #indicator of day in cycle
    bill_shock_invoice$last_day_of_cycle <- bill_shock_invoice$last_day_of_cycle==bill_shock_invoice$t_start_time_date #indicator of last day of cycle
    bill_shock_invoice <- bill_shock_invoice[-c(1:(which(bill_shock_invoice$day_in_cycle==1)[1]-1)),] #remove the first cycle for CDP (it is not compelete), or: start the analysis fromt he first compelte cycle availble
    bill_shock_invoice <- bill_shock_invoice[order(bill_shock_invoice$s_zone,bill_shock_invoice$cycle_month),] #order by cycle_month and s_zone for aggregations
    
    for (cycle_month in sort(unique(bill_shock_invoice$cycle_month)[unique(bill_shock_invoice$cycle_month)<reference_date_for_invoice]))
    {
      bill_shock_invoice$billable_data_kb[bill_shock_invoice$cycle_month==cycle_month] <- rep(tail(bill_shock_invoice$billable_data_kb[bill_shock_invoice$cycle_month==cycle_month],1)/length(bill_shock_invoice$day_in_cycle[bill_shock_invoice$cycle_month==cycle_month]),tail(bill_shock_invoice$day_in_cycle[bill_shock_invoice$cycle_month==cycle_month],1))
      bill_shock_invoice$billable_outgoing_sms[bill_shock_invoice$cycle_month==cycle_month] <- rep(tail(bill_shock_invoice$billable_outgoing_sms[bill_shock_invoice$cycle_month==cycle_month],1)/length(bill_shock_invoice$day_in_cycle[bill_shock_invoice$cycle_month==cycle_month]),tail(bill_shock_invoice$day_in_cycle[bill_shock_invoice$cycle_month==cycle_month],1))
      bill_shock_invoice$billable_voice_duration[bill_shock_invoice$cycle_month==cycle_month] <- rep(tail(bill_shock_invoice$billable_voice_duration[bill_shock_invoice$cycle_month==cycle_month],1)/length(bill_shock_invoice$day_in_cycle[bill_shock_invoice$cycle_month==cycle_month]),tail(bill_shock_invoice$day_in_cycle[bill_shock_invoice$cycle_month==cycle_month],1))
      bill_shock_invoice$total_working_units[bill_shock_invoice$cycle_month==cycle_month] <- rep(tail(bill_shock_invoice$total_working_units[bill_shock_invoice$cycle_month==cycle_month],1),tail(bill_shock_invoice$day_in_cycle[bill_shock_invoice$cycle_month==cycle_month],1))
      bill_shock_invoice$total_data_kb[bill_shock_invoice$cycle_month==cycle_month] <- bill_shock_invoice$billable_data_kb[bill_shock_invoice$cycle_month==cycle_month]
      bill_shock_invoice$total_outgoing_sms[bill_shock_invoice$cycle_month==cycle_month] <- bill_shock_invoice$billable_outgoing_sms[bill_shock_invoice$cycle_month==cycle_month]
      bill_shock_invoice$total_voice_duration[bill_shock_invoice$cycle_month==cycle_month] <- bill_shock_invoice$billable_voice_duration[bill_shock_invoice$cycle_month==cycle_month]
    }
    
    #merge its data
    bill_shock_consumption <- rbind(bill_shock_invoice,bill_shock_consumption)
    bill_shock_consumption <- bill_shock_consumption[order(bill_shock_consumption$s_zone,bill_shock_consumption$cycle_month),] #order by cycle_month and s_zone for aggregations
    
    invoice_merge <- TRUE
  }
  
  #Aggregate the consumption and units# per day and price zone (in case of multiple countries)
  total_working_units <- aggregate(total_working_units ~ t_start_time_date + s_zone, data = bill_shock_consumption,sum)
  billable_voice_duration <- aggregate(billable_voice_duration ~ t_start_time_date + s_zone, data = bill_shock_consumption,sum)
  billable_outgoing_sms <- aggregate(billable_outgoing_sms ~ t_start_time_date + s_zone, data = bill_shock_consumption,sum)
  billable_data_kb <- aggregate(billable_data_kb ~ t_start_time_date + s_zone, data = bill_shock_consumption,sum)

  #In the case of different countries in the same s_zone and date, remove one instance (they were already aggregated)
  bill_shock_consumption <- bill_shock_consumption[!duplicated(cbind(bill_shock_consumption$t_start_time_date,bill_shock_consumption$s_zone),fromLast = TRUE),]
  bill_shock_consumption$s_country_code <- NULL #no need now for the countries
  bill_shock_consumption$change_of_cycle <- c(TRUE,diff(bill_shock_consumption$cycle_month)!=0) #indicator of cycle change, starting from 1st cycle
  
  #Add the sum aggreagations per price zone
  bill_shock_consumption$total_working_units <- total_working_units$total_working_units #add sums of units in each day
  bill_shock_consumption$billable_voice_duration <- billable_voice_duration$billable_voice_duration #add sums of units in each day
  bill_shock_consumption$billable_outgoing_sms <- billable_outgoing_sms$billable_outgoing_sms #add sums of units in each day
  bill_shock_consumption$billable_data_kb <- billable_data_kb$billable_data_kb #add sums of units in each day
  
  #Aggregate the usage per cdp, per zone for each cycle: cumsum inside the cycle and max per cycle
  total_working_units_cummax <- aggregate(total_working_units ~ cycle_month + s_zone, data = bill_shock_consumption,cummax)
  billable_voice_duration_cycle_cumsum <- aggregate(billable_voice_duration ~ cycle_month + s_zone, data = bill_shock_consumption,cumsum)
  billable_outgoing_sms_cycle_cumsum <- aggregate(billable_outgoing_sms ~ cycle_month + s_zone, data = bill_shock_consumption,cumsum)
  billable_data_kb_cycle_cumsum <- aggregate(billable_data_kb ~ cycle_month + s_zone, data = bill_shock_consumption,cumsum)
  
  #Add the cumsum aggreagations per price zone
  bill_shock_consumption$total_working_units_cummax <- melt(total_working_units_cummax$total_working_units)[,1]
  bill_shock_consumption$billable_voice_duration_cycle_cumsum <- melt(billable_voice_duration_cycle_cumsum$billable_voice_duration)[,1]
  bill_shock_consumption$billable_outgoing_sms_cycle_cumsum <- melt(billable_outgoing_sms_cycle_cumsum$billable_outgoing_sms)[,1]
  bill_shock_consumption$billable_data_kb_cycle_cumsum <- melt(billable_data_kb_cycle_cumsum$billable_data_kb)[,1]
  
  rm(billable_voice_duration_cycle_cumsum,billable_outgoing_sms_cycle_cumsum,billable_data_kb_cycle_cumsum,total_working_units_cummax,total_working_units,billable_voice_duration,billable_outgoing_sms,billable_data_kb)

  #This code part is for the distinguish between regular to irregular zones accoridng to units number at the moment
  s_zone_table <- aggregate(total_working_units~s_zone+cycle_month,data=bill_shock_consumption,max) #showing the real outlier price zone!
  regular_s_zones <- unique(s_zone_table$s_zone[s_zone_table$total_working_units>reg_zone_threshold]) #those are "regular" price zones - we remove those cycles which are zero for those s_zones. put 0 if want to remove it.

  #Preprocess the regular zones
#   for (s_zone in regular_s_zones)
#   {  
#     cycle_month_to_remove <- bill_shock_consumption$cycle_month[bill_shock_consumption$total_working_units_cummax==0 & bill_shock_consumption$change_of_cycle==TRUE & bill_shock_consumption$s_zone==s_zone]
#     bill_shock_consumption <- bill_shock_consumption[!(bill_shock_consumption$cycle_month %in% cycle_month_to_remove & bill_shock_consumption$s_zone==s_zone),]
#   }
  
  #Allocate new columns
  bill_shock_consumption$billable_voice_duration_ratio <- 1
  bill_shock_consumption$billable_outgoing_sms_ratio <- 1
  bill_shock_consumption$billable_data_kb_ratio <- 1

  #For each cdp, s_zone, in a daily level write the absolute change of consumption (using the cumsum columns) from the total of previous cycle (which might be not existed and than its 0)
  for (i in 1:nrow(bill_shock_consumption))
  {
    s_zone_tmp <- bill_shock_consumption$s_zone[i] #the price zone
    cycle_month <- bill_shock_consumption$cycle_month[i] #curreny cycle
    cycles_sequence <- seq(cycle_month, length = max_window+1, by = "-1 months") #delta time to check the score (input)
    prev_cycles <-  bill_shock_consumption[bill_shock_consumption$cycle_month>=cycles_sequence[max_window+1] & bill_shock_consumption$cycle_month<cycles_sequence[min_window] & bill_shock_consumption$last_day_of_cycle==TRUE & bill_shock_consumption$s_zone==s_zone_tmp,] #previous cycles up to now in price zone
    
    #For each of the consumptions type calcaulte the ratio and the avg. consumption per day
    for (j in 1:3)
    {
      prev_cycle <- NULL
      if (nrow(prev_cycles)>0) {prev_cycle <- prev_cycles$cycle_month[which.max(prev_cycles[,17+j])[1]]} #cycle with max consumption of that type in the check window in that price zone
      prev_cycle_cumsum <- bill_shock_consumption[bill_shock_consumption$s_zone==s_zone_tmp & bill_shock_consumption$cycle_month==prev_cycle,]
      
      if (nrow(prev_cycle_cumsum)>0)
      {
        day_in_cycle <- as.numeric(bill_shock_consumption$day_in_cycle[i])
        max_day_in_prev_cycle <- max(as.numeric(prev_cycle_cumsum$day_in_cycle))
        max_consump_in_prev_cycle <- max(prev_cycle_cumsum[,17+j])
        
        if (day_in_cycle==max_day_in_prev_cycle) 
        {
          if (prev_cycle_cumsum[prev_cycle_cumsum$day_in_cycle==day_in_cycle,17+j]<1) {
            bill_shock_consumption[i,20+j] <- bill_shock_consumption[i,17+j] #the consumption itself form perv cycle
          } else {bill_shock_consumption[i,20+j] <- (bill_shock_consumption[i,17+j]/prev_cycle_cumsum[prev_cycle_cumsum$day_in_cycle==day_in_cycle,17+j])} #the ratio of consumption form perv cycle 
        } else {
          if (day_in_cycle<max_day_in_prev_cycle) 
          {
            if (max_consump_in_prev_cycle<1) {
              bill_shock_consumption[i,20+j] <- bill_shock_consumption[i,17+j] #the ratio of consumption form perv cycle in case of more than 1 day
            } else {bill_shock_consumption[i,20+j] <- (bill_shock_consumption[i,17+j]/max_consump_in_prev_cycle)}
          } else {
            if (max_consump_in_prev_cycle<1) {
              bill_shock_consumption[i,20+j] <- bill_shock_consumption[i,17+j] #the consumption itself form perv cycle
            } else {bill_shock_consumption[i,20+j] <- (bill_shock_consumption[i,17+j]/max_consump_in_prev_cycle*day_in_cycle/max_day_in_prev_cycle)} #the ratio of consumption form perv cycle
          }
        }
      } 
    }
  }

  #Remove unwatned billable columns - we extrcated data from them already
  bill_shock_consumption$total_voice_duration <- NULL
  bill_shock_consumption$total_outgoing_sms <- NULL
  bill_shock_consumption$total_data_kb <- NULL
  
  bill_shock_consumption$billable_voice_duration <- NULL
  bill_shock_consumption$billable_outgoing_sms <- NULL
  bill_shock_consumption$billable_data_kb <- NULL
  
  #Remove unwatned cumsum columns - we extrcated data from them already
  bill_shock_consumption$billable_voice_duration_cycle_cumsum <- NULL
  bill_shock_consumption$billable_outgoing_sms_cycle_cumsum <- NULL
  bill_shock_consumption$billable_data_kb_cycle_cumsum <- NULL
  
  #Calculated the weighted average per unit number of each price zone for each CDP.
  bill_shock_consumption[,c(12:ncol(bill_shock_consumption))] <- bill_shock_consumption[,c(12:ncol(bill_shock_consumption))]/bill_shock_consumption$total_working_units_cummax #avg consumption per units number in price zone
  bill_shock_consumption[is.na(bill_shock_consumption)] <- 0; bill_shock_consumption[bill_shock_consumption==Inf] <- 0
  bill_shock_consumption <- aggregate(cbind(billable_voice_duration_ratio,billable_outgoing_sms_ratio,billable_data_kb_ratio) ~ 
      n_cdp_id+t_start_time_date+cycle_start_date+cycle_end_date+cycle_month+last_day_of_cycle+day_in_cycle,FUN = mean,data=bill_shock_consumption) #aggregate each column of consumption, keep the shared columns
  bill_shock_consumption <- bill_shock_consumption[order(bill_shock_consumption$t_start_time_date),] #order by date
  bill_shock_consumption$total_working_units_cummax <- NULL
  
  #### Activations Data
  activations_per_day <- r_cdp_cycle_day_active[r_cdp_cycle_day_active$n_cdp_id==cdp,]
  #activations_per_day$activated[nrow(activations_per_day)] <- activations_per_day$bs_related_inventory[nrow(activations_per_day)]
  activations_per_day <- activations_per_day[activations_per_day$cycle_month %in% unique(bill_shock_consumption$cycle_month),]
<<<<<<< HEAD
  activations_per_day$new_activated_utilize <- c(0,diff(activations_per_day$activated))/(activations_per_day$bs_related_inventory+activations_per_day$activated)
  activations_per_day$new_activated_utilize[is.na(activations_per_day$new_activated_utilize)] <- 0
  
=======
  #activations_per_day$new_activated_utilize <- activations_per_day$activated*c(0,diff(activations_per_day$activated))/(activations_per_day$bs_related_inventory+activations_per_day$activated)
  activations_per_day$new_activated_utilize <- c(0,diff(activations_per_day$activated))/(activations_per_day$bs_related_inventory+activations_per_day$activated)
  #activations_per_day$new_activated_utilize <- activations_per_day$activated*activations_per_day$activated/(activations_per_day$bs_related_inventory+activations_per_day$activated)
  activations_per_day$new_activated_utilize[is.na(activations_per_day$new_activated_utilize)] <- 0
    
>>>>>>> 5d1e471ac8fad5db30dc4af6342a52569bbe3a3b
  #Add the cycle data (this is the reference at each time)
  activations_per_day$cycle_start_date <- as.Date(sapply(activations_per_day$t_start_time_date,function(x) {as.character(bill_shock_cycles$cycle_start_date[cycle_row_data(x)])}))
  activations_per_day$cycle_end_date <- as.Date(sapply(activations_per_day$t_start_time_date,function(x) {as.character(bill_shock_cycles$cycle_end_date[cycle_row_data(x)])}))
  activations_per_day$last_day_of_cycle <- as.Date(sapply(activations_per_day$t_start_time_date,function(x) {as.character(bill_shock_cycles$last_day_of_cycle[cycle_row_data(x)])})) #indicator of last day in cycle
  activations_per_day$day_in_cycle <- activations_per_day$t_start_time_date - activations_per_day$cycle_start_date + 1 #indicator of day in cycle
  activations_per_day$last_day_of_cycle <- activations_per_day$last_day_of_cycle==activations_per_day$t_start_time_date #indicator of last day of cycle

  #Allocate new columns
  activations_cycle_cumsum <- aggregate(new_activated_utilize ~ cycle_month, data = activations_per_day,cummax)
<<<<<<< HEAD
  activations_per_day$activations_cycle_cumsum <- melt(activations_cycle_cumsum$new_activated_utilize)[,1]
=======
  activations_per_day$activations_cycle_cumsum <- melt(activations_cycle_cumsum$new_activated_utilize)[,1] 
>>>>>>> 5d1e471ac8fad5db30dc4af6342a52569bbe3a3b
  activations_per_day$activations_units_ratio <- 1
  activations_per_day$new_activated_utilize <- NULL
  activations_per_day$bs_related_inventory <- NULL
  
  if (as.numeric(activations_per_day$day_in_cycle[1])!=1) {activations_per_day <- activations_per_day[-c(1:(which(activations_per_day$day_in_cycle==1)[1]-1)),]} #remove the first cycle for CDP (it is not compelete), or: start the analysis fromt he first compelte cycle availble
  
  #Calcualte ratio in activations
  for (i in 1:nrow(activations_per_day))
  {
    cycle_month <- activations_per_day$cycle_month[i] #curreny cycle
    cycles_sequence <- seq(cycle_month, length = max_window+1, by = "-1 months") #delta time to check the score (input)
    prev_cycles <-  activations_per_day[activations_per_day$cycle_month>=cycles_sequence[max_window+1] & activations_per_day$cycle_month<cycles_sequence[min_window] & activations_per_day$last_day_of_cycle==TRUE,] #previous cycles up to now
    
    prev_cycle <- NULL
    if (nrow(prev_cycles)>0) {prev_cycle <- prev_cycles$cycle_month[which.max(prev_cycles[,9])[1]]} 
    prev_cycle_cumsum <- activations_per_day[activations_per_day$cycle_month==prev_cycle,]
    
    if (nrow(prev_cycle_cumsum)>0)
    {
      day_in_cycle <- as.numeric(activations_per_day$day_in_cycle[i])
      max_day_in_prev_cycle <- max(as.numeric(prev_cycle_cumsum$day_in_cycle))
      if (day_in_cycle<=max_day_in_prev_cycle) 
      {
        if (prev_cycle_cumsum[prev_cycle_cumsum$day_in_cycle==day_in_cycle,9]==0) {
          activations_per_day[i,10] <- activations_per_day[i,9] #the activations itself form perv cycle
        } else {activations_per_day[i,10] <- (activations_per_day[i,9]/prev_cycle_cumsum[prev_cycle_cumsum$day_in_cycle==day_in_cycle,9])} #the ratio of activations form perv cycle 
      } else {
        if (max(prev_cycle_cumsum[,9])==0) {
          activations_per_day[i,10] <- activations_per_day[i,9] #the ratio of activations form perv cycle i case of more than 1 day
        } else {activations_per_day[i,10] <- (activations_per_day[i,9]/max(prev_cycle_cumsum[,9]))}
      }
    } 
  }
  
  #Remove unwatned cumsum columns - we extrcated data from them already
  activations_per_day$activations_cycle_cumsum <- NULL
  activations_per_day$activations_units_ratio[activations_per_day$activations_units_ratio<0] <- 0
  activations_per_day$activations_units_ratio[activations_per_day$activations_units_ratio!=1] <- activations_per_day$activations_units_ratio[activations_per_day$activations_units_ratio!=1]+1
  
  ### Merge them now
  bill_shock_consumption <- merge(bill_shock_consumption,activations_per_day)

  #scale and normalization
  bill_shock_consumption$activations_units_ratio <- log10(bill_shock_consumption$activations_units_ratio)/log10(th/0.1) #scale activations
  bill_shock_consumption$billable_voice_duration_ratio <- log10(bill_shock_consumption$billable_voice_duration_ratio)/log10(th/10) #scale voice
  bill_shock_consumption$billable_outgoing_sms_ratio <- log10(bill_shock_consumption$billable_outgoing_sms_ratio)/log10(th/10) #scale SMS
  bill_shock_consumption$billable_data_kb_ratio <- log10(bill_shock_consumption$billable_data_kb_ratio)/log10(th) #scale data
  
  bill_shock_consumption[bill_shock_consumption==Inf] <- 0
  bill_shock_consumption[bill_shock_consumption==-Inf] <- 0
  
  bill_shock_consumption$activations_units_ratio[bill_shock_consumption$activations_units_ratio<0] <- 0
  bill_shock_consumption$billable_data_kb_ratio[bill_shock_consumption$billable_data_kb_ratio<0] <- 0
  bill_shock_consumption$billable_voice_duration_ratio[bill_shock_consumption$billable_voice_duration_ratio<0] <- 0
  bill_shock_consumption$billable_outgoing_sms_ratio[bill_shock_consumption$billable_outgoing_sms_ratio<0] <- 0
  
  bill_shock_consumption$activations_units_ratio[bill_shock_consumption$activations_units_ratio>1] <- 1
  bill_shock_consumption$billable_data_kb_ratio[bill_shock_consumption$billable_data_kb_ratio>1] <- 1
  bill_shock_consumption$billable_voice_duration_ratio[bill_shock_consumption$billable_voice_duration_ratio>1] <- 1
  bill_shock_consumption$billable_outgoing_sms_ratio[bill_shock_consumption$billable_outgoing_sms_ratio>1] <- 1
  
  bill_shock_consumption[,c(8:12)] <- round(bill_shock_consumption[,c(8:12)],2) #round the score
  
  if (invoice_merge) {
    bill_shock_consumption <- bill_shock_consumption[bill_shock_consumption$t_start_time_date>=reference_date_for_invoice,]
  }
  
  #Bill shock indicator per consumtpion per cycle (save pdf)
  if (save_all == 1) {pdf(paste0("cdp_",cdp,".pdf"))}
  if (plot_graphs == 1) {
    for (j in c(12,8:10))
    {
      par(mfrow=c(3,4),oma = c(0, 0, 2, 0))
      for (cycle_month in unique(bill_shock_consumption$cycle_month))
      {
        plot(as.numeric(bill_shock_consumption$day_in_cycle[bill_shock_consumption$cycle_month==cycle_month]),
          bill_shock_consumption[bill_shock_consumption$cycle_month==cycle_month,j],
          main=as.Date(cycle_month),cex.main=1,xlab="Day in cycle",ylab="Prob. to BS",ylim=c(0,1),type="l"); box()
      }
      mtext(paste0("CDP",cdp,", Looking ",min_window,"-",max_window+1," Cycles Back, with threshold: ",th," of ",strsplit(names(bill_shock_consumption)[j],"_")[[1]][2]), outer = TRUE, cex = 1)
      par(mfrow=c(1,1))
    }
  }
  if (save_all == 1) {dev.off()}
  
  #Calcualte final score per day, create results csv and save the files into S3
  results <- bill_shock_consumption[,c(1:2,5,7,12,8:11)]
  results_tot <- rbind(results_tot,results)

  rm(activations_cycle_cumsum,activations_per_day,bill_shock_consumption,bill_shock_invoice,bill_shock_cycles,prev_cycles,prev_cycle_cumsum,s_zone_table)
}
<<<<<<< HEAD
=======

# #Look for FP's
# cdp <- 113
# results <- results_tot[results_tot$n_cdp_id==cdp,]
# for (j in c(5:8))
# {
#   par(mfrow=c(3,4),oma = c(0, 0, 2, 0))
#   for (cycle_month in unique(results$cycle_month))
#   {
#     plot(as.numeric(results$day_in_cycle[results$cycle_month==cycle_month]),
#       results[results$cycle_month==cycle_month,j],
#       main=as.Date(cycle_month),cex.main=1,xlab="Day in cycle",ylab="Prob. to BS",ylim=c(0,1),type="l"); box()
#   }
#   mtext(paste0(dim_cdp$w_ent_account_username[dim_cdp$n_cdp_id==cdp],", Looking ",min_window,"-",max_window+1," Cycles Back, with threshold: ",th," of ",strsplit(names(results_tot)[j],"_")[[1]][2]), outer = TRUE, cex = 1)
#   par(mfrow=c(1,1))
# }
# 
# #Look for FN's
# par(mfrow=c(3,4),oma = c(0, 0, 2, 0))
# for (k in 1:length(cdp_list))
# {
#   cdp <- cdp_list[k]
#   if (length(fact_cdp_invoice$t_cycle_start_date[fact_cdp_invoice$n_cdp_id==cdp])) {
#     subscription_charge <- fact_cdp_invoice$f_subscription_charge[fact_cdp_invoice$n_cdp_id==cdp]
#     overage_charge <- fact_cdp_invoice$f_overage_charge[fact_cdp_invoice$n_cdp_id==cdp]
#     sms_charge <- fact_cdp_invoice$f_sms_charge[fact_cdp_invoice$n_cdp_id==cdp]
#     voice_charge <- fact_cdp_invoice$f_voice_charge[fact_cdp_invoice$n_cdp_id==cdp]
#     activatation_charge <- fact_cdp_invoice$f_activation_charge[fact_cdp_invoice$n_cdp_id==cdp]
#     total_charge <- subscription_charge+overage_charge+sms_charge+voice_charge+activatation_charge
#     cycles_list <- fact_cdp_invoice$t_cycle_start_date[fact_cdp_invoice$n_cdp_id==cdp]
#     if (length(which(total_charge==0))>0)
#     {    
#       cycles_list <- cycles_list[-which(total_charge==0)]
#       total_charge <- total_charge[-which(total_charge==0)]
#     }
#     if (length(cycles_list)>0)
#     {
#       ratios <- total_charge[-1]/head(total_charge,-1)
#       if (!is.nan(max(ratios)) & max(ratios)!=Inf) {if (max(ratios)>25) {plot(cycles_list[-1],ratios,xlab="cycle",ylab="cost change",
#         main=dim_cdp$w_ent_account_username[dim_cdp$n_cdp_id==cdp])}}
#     }
#   }
# }
# par(mfrow=c(1,1))
>>>>>>> 5d1e471ac8fad5db30dc4af6342a52569bbe3a3b

#### Script Ending ####
write.csv(results_tot,paste0("results.csv"),row.names = FALSE)
write.csv(results_tot,paste0("results_",Sys.Date(),".csv"),row.names = FALSE)
dbDisconnect(conn)
q(save="no")
