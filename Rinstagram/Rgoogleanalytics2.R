library(RGA)
library(RGoogleAnalytics)

# Create a new Google Analytics API object
ga <- RGoogleAnalytics()

# Authorize your account and paste the accesstoken 
query <- QueryBuilder()
access_token <- query$authorize()

# Create a new Google Analytics API object
ga <- RGoogleAnalytics()
ga.profiles <- ga$GetProfileData(access_token)

# List the GA profiles 
ga.profiles

#Build the query string 
query$Init(start.date = "2014-04-09",
           end.date = "2014-12-09",
           dimensions = "ga:dimension1,ga:medium,ga:landingPagePath,ga:exitPagePath,ga:userType,ga:sessionCount,ga:daysSinceLastSession",
           metrics = "ga:sessions,ga:pageviews,ga:uniquePageviews,ga:sessionDuration",
           #sort = "ga:visits",
           max.results = 11000,
           table.id = paste("ga:",ga.profiles$id[3],sep="",collapse=","),
           access_token=access_token)

# Make a request to get the data from the API
ga.data <- ga$GetReportData(query)

# Look at the returned data
head(ga.data)

#Save extracted data points
write.csv(ga.data,"data.csv",row.names=F)
