rm(list=ls()) # clear workspace
mainDir <- "C:\\Accidents\\app3"
setwd(mainDir)
set.seed(1)
options(java.parameters = "-Xmx8000m")
library(shiny)
#Upload apps to shinyapps.io
library(rsconnect)
rsconnect::setAccountInfo(name='orikronfeld', token='C56BA815A51CC699AAA092243CBB4222', secret='RtnyrJNl1zXrQSHJQQvSp4mGja0b/jDPSbwZtmsY')

deployApp()

#Uplaod specific files
deployApp(appFiles=c("helpers.R"),appName="app2")
