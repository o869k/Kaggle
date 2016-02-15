#Initialization
{
  sessionInfo() #system performance
  gc() #clear unsued memory
  rm(list=ls()) # clear workspace
  setwd("C:/Users/user/Documents/R/Santa")
  set.seed(1234) #setting seed for comparison
  library(date); library(fpp)
  library(FNN); library(ggplot2)
  library(caret); library(kernlab)
  library(e1071); library(UsingR)
}

#Read Data & Preallocation: 
{
  Toys <- read.csv("toys_rev2.csv")  
  N <- nrow(Toys)  #Number of toys
  M <- ncol(Toys)  #Number of features
  start <- as.Date("01/01/2014",format="%m/%d/%Y")
}

#Initialize the elfs - keep this data frame sorted by the rate factor at all times
{
  E <- 900 #Number of elves
  Elfs <- data.frame(ElfId=seq(1:E),Rate=rep(1,E),Busy=rep(0,E),ToyId=rep(0,E),Start=as.POSIXct("2014-01-01 09:00:00",tz = "UTC"),Duration=rep(0,E),End=as.POSIXct("2014-01-01 09:00:00",tz = "UTC"),NextRate=rep(1,E),row.names = NULL)
}

#Allocating
{
  for (i in 1:1000)
  {
    Y <- as.numeric({strsplit(as.character(Toys$Arrival_time[i]),split=" ")[[1]][1]})
    M <- as.numeric({strsplit(as.character(Toys$Arrival_time[i]),split=" ")[[1]][2]})
    D <- as.numeric({strsplit(as.character(Toys$Arrival_time[i]),split=" ")[[1]][3]})
    H <- as.numeric({strsplit(as.character(Toys$Arrival_time[i]),split=" ")[[1]][4]})
    min <- as.numeric({strsplit(as.character(Toys$Arrival_time[i]),split=" ")[[1]][5]})
    Arrive_time <- ISOdatetime(year = Y,month = M,day = D,hour = H,min = min,sec = 0,tz = "UTC") 
    Job_Start_time <- ISOdatetime(year = Y,month = M,day = D,hour = 9,min = 0,sec = 0,tz = "UTC") 
    Job_End_time <- ISOdatetime(year = Y,month = M,day = D,hour = 19,min = 0,sec = 0,tz = "UTC") 
    
#     #Checking for the ones which finished the job
#     idx <- which((Elfs$Busy==1) && (Elfs$End < Arrive_time)) #indexes
#     if (length(idx) > 0)
#     {
#       Elfs$Rate[idx] <- Elfs$NextRate[idx]
#       Elfs$Busy[idx] <- 0
#       Elfs$ToyId[idx] <- 0
#     }
    
    Elfs$Busy[1] <- 1
    Elfs$ToyId[1] <- Toys$ToyId[i]
    if (Arrive_time < Job_Start_time) {Elfs$Start[1] <- Job_Start_time} else {Elfs$Start[1] <- Arrive_time}
    Elfs$Duration[1] <- Toys$Duration[i]/Elfs$Rate[1] #duration in minutes
    Elfs$End[1] <- Elfs$Start[1] + Elfs$Duration[1]*60
    
    n <- as.double(10 - (Job_End_time - Elfs$End[1]))
    m <- as.double(Elfs$End[1] - Job_End_time)
    if (m < 0) {m <- 0}
    Elfs$NextRate[1] <- Elfs$Rate[1]*1.02^n*0.9^m #calculating next rate
    
    Elfs <- Elfs[order(Elfs$Busy),] #sort the elfs by who is free and by current rate
  }
}

#Writing the result 
{
  result <- data.frame(ToyId=Toys$ToyId,Arrival_time=Toys$Arrival,Duration=Toys$Duration)
  write.csv(result,file="Toys.csv",row.names=FALSE)
}
