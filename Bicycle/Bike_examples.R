sessionInfo() #system performance
gc() #clear unsued memory
rm(list=ls()) # clear workspace
setwd("C:/Users/user/Documents/R/Bicycle")
set.seed(1234) #setting seed for comparison
library(ggplot2)
library(lubridate)
library(readr)
library(scales)

## Bike Rentals By Time And Temperature ##

# The competition data is stored in the ../input directory
train <- read.csv("train.csv")
test  <- read.csv("test.csv")

# Write some basic stats to the log
cat("Number of training rows ", nrow(train), "\n")
cat("Number of test rows ", nrow(test), "\n")
head(train)

train$hour  <- hour(ymd_hms(train$datetime))
train$times <- as.POSIXct(strftime(ymd_hms(train$datetime), format="%H:%M:%S"), format="%H:%M:%S")
train$jitterTimes <- train$times+minutes(round(runif(nrow(train),min=0,max=59)))
train$day <- wday(ymd_hms(train$datetime), label=TRUE)

p <- ggplot(train[train$workingday==1,], aes(x=jitterTimes, color=temp*9/5+32, y=count)) +
  geom_point(position=position_jitter(w=0.0, h=0.4)) +
  theme_light(base_size=20) +
  xlab("Hour of the Day") +
  scale_x_datetime(breaks = date_breaks("4 hours"), labels=date_format("%I:%M %p")) + 
  ylab("Number of Bike Rentals") +
  scale_colour_gradientn("Temp (°F)", colours=c("#5e4fa2", "#3288bd", "#66c2a5", "#abdda4", "#e6f598", "#fee08b", "#fdae61", "#f46d43", "#d53e4f", "#9e0142")) +
  ggtitle("On workdays, most bikes are rented on warm mornings and evenings\n") +
  theme(plot.title=element_text(size=18))

ggsave("bike_rentals_by_time_and_temperature.png", p)

## Bike Rentals By Time ##

train$hour  <- hour(ymd_hms(train$datetime))
train$times <- as.POSIXct(strftime(ymd_hms(train$datetime), format="%H:%M:%S"), format="%H:%M:%S")
train$day   <- wday(ymd_hms(train$datetime), label=TRUE)

p <- ggplot(train, aes(x=times, y=count, color=day)) +
  geom_smooth(ce=FALSE, fill=NA, size=2) +
  theme_light(base_size=20) +
  xlab("Hour of the Day") +
  scale_x_datetime(breaks = date_breaks("4 hours"), labels=date_format("%I:%M %p")) + 
  ylab("Number of Bike Rentals") +
  scale_color_discrete("") +
  ggtitle("People rent bikes for morning/evening commutes on weekdays, and daytime rides on weekends\n") +
  theme(plot.title=element_text(size=18))

ggsave("bike_rentals_by_time_of_day.png")
