#Initialization
{
  sessionInfo() #system performance
  gc() #clear unsued memory
  rm(list=ls()) # clear workspace
  mainDir <- "D:/Data/springleaf"
  setwd(mainDir)
  Sys.setlocale("LC_TIME", "English")
  set.seed(1234) #setting seed for comparison
}

#Read Data
{
  train <- read.csv("train.csv")
}

#