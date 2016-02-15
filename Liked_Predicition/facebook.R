sessionInfo() #system performance
gc() #clear unsued memory
rm(list=ls()) # clear workspace
mainDir <- "D:/Data/Liked_Predicition"
setwd(mainDir)
Sys.setlocale("LC_TIME", "English")
set.seed(1234) #setting seed for comparison
library(Rfacebook)
library(devtools)

fb_oauth <- fbOAuth("1508118382834465", "c4ae80cae52b588f22a06536f1cb273c", extended_permissions = T)
save(fb_oauth, file="fb_oauth")
load("fb_oauth")
me <- getUsers("me", token=fb_oauth)
me$name

my_friends <- getFriends(fb_oauth, simplify = TRUE)
head(my_friends$id, n = 1) # get lowest user ID
mat <- getNetwork(fb_oauth, format = "adj.matrix")

my_likes <- getLikes(user="me", token=fb_oauth)
