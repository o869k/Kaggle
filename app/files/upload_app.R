rm(list=ls()) # clear workspace
mainDir <- "C:\\FacebookPosts\\app"
setwd(mainDir)
library(shiny)
library(rsconnect)
rsconnect::setAccountInfo(name='orikronfeld', token='C56BA815A51CC699AAA092243CBB4222', secret='RtnyrJNl1zXrQSHJQQvSp4mGja0b/jDPSbwZtmsY')

#deployApp()

#Uplaod specific files
deployApp(appFiles=c("helpers.R"),appName="IMDB_Movie_Review")
