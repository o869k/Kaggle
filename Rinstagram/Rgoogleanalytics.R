#Initializaiotn ####
rm(list=ls()) # clear workspace
mainDir <- "/Celebros/"
setwd(mainDir)
set.seed(1)
options(java.parameters = "-Xmx8000m")

library(RGA)
library(RGoogleAnalytics)

# Authorize the Google Analytics account
# This need not be executed in every session once the token object is created 
# and saved
client.id <- "480736127814-37415mojr9c0l8a7g4jpq5dpnmbugv59.apps.googleusercontent.com"
client.secret <- "68IM39xrXnbh4xtjDD9whsJE"
token <- Auth(client.id,client.secret)

# Save the token object for future sessions
save(token,file="./token_file")

query.list <- Init(start.date = "2014-08-01",
                   end.date = "2014-09-01",
                   dimensions = "ga:sourceMedium",
                   metrics = "ga:sessions,ga:transactions",
                   max.results = 10000,
                   sort = "-ga:transactions",
                   table.id = "ga:123456")

# Create the Query Builder object so that the query parameters are validated
ga.query <- QueryBuilder(query.list)

# Extract the data and store it in a data-frame
ga.data <- GetReportData(ga.query, token)

# Sanity Check for column names
dimnames(ga.data)

# Check the size of the API Response
dim(ga.data)

# Validate and refresh the token
ValidateToken(token)

# For future tokens
load("./token_file")

