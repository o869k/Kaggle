#Initialization
{
  sessionInfo() #system performance
  gc() #clear unsued memory
  rm(list=ls()) # clear workspace
  mainDir <- "C:/Users/user/Documents/R/Restaurant"
  setwd(mainDir)
  set.seed(1234) #setting seed for comparison
  library(foreach); library(doParallel);
  library(ggplot2); library(randomForest);
}
