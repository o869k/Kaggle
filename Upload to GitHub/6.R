#Initializaiotn ####
rm(list=ls()) # clear workspace
setwd(mainDir)
set.seed(1)
options(java.parameters = "-Xmx8000m")
library(github)
library(httr)
library(bigrquery)
library(dplyr)
library(rlist)
library(pipeR)
library(sna)
library(igraph)
library(visNetwork)
library(RNeo4j)

users_pr <- users_pr[order(users_pr$login,users_pr$created_at),]

#The python data (as reference)
dataset$state <- factor(dataset$state)
dataset$city <- factor(dataset$city)
dataset$name.1 <- factor(dataset$name.1)
dataset$email.2 <- factor(dataset$email.2)
dataset$location.1 <- factor(dataset$location.1)
dataset$Primary.Language <- factor(dataset$Primary.Language)
dataset$X..of.Repositories.in.Primary.Language[is.na(dataset$X..of.Repositories.in.Primary.Language)] <- 0 #complete missing info
users <- sort(unique(dataset$username)) #all of our users extracted from the python program

#Manipualte Data ####
#Basic Information
basic_info <- c("login","id","type","site_admin","name","company","location","created_at","updated_at","hireable","bio","email","blog","public_repos","public_gists","followers","following","forks","starred","watchers","user_starred","user_watchers",
                "PushEvent","PullRequestEvent","PullRequestReviewCommentEvent","IssueCommentEvent","CreateEvent","ForkEvent","IssuesEvent","DeleteEvent","WatchEvent","PublicEvent","CommitCommentEvent","ReleaseEvent","MemberEvent","GollumEvent","primary_language","secondary_language")

users_data_basic <- users_data[,basic_info]
users_data_basic$hireable[is.na(users_data_basic$hireable)] <- FALSE
for (i in c(14:36)) {users_data_basic[is.na(users_data_basic[,i]),i] <- 0}

#Handle languages
users_data_languages <- users_data[,!(names(users_data) %in% basic_info)]
users_data_languages[is.na(users_data_languages)] <- 0
primary_languages <- t(apply(users_data_languages,1,function(x) {names(tail(sort(x),5))})) #primary language
repositories_in_languages <- t(apply(users_data_languages,1,function(x) {tail(sort(x),5)})) #num of repositories in a language
for (i in c(1:5))
{
    primary_languages[primary_languages[,i]=="C...1",i] <- "C++"
    primary_languages[primary_languages[,i]=="Objective.C.1",i] <- "Objective-C"
    primary_languages[primary_languages[,i]=="C..1",i] <- "C#"
    primary_languages[primary_languages[,i]=="F.",i] <- "F#"
}
users_data_basic$primary_language <- primary_languages[,5]
users_data_basic$primary_language[repositories_in_languages[,5]==0] <- NA
users_data_basic$secondary_language <- primary_languages[,4]
users_data_basic$secondary_language[repositories_in_languages[,4]==0] <- NA
users_data_basic$third_language <- primary_languages[,3]
users_data_basic$third_language[repositories_in_languages[,3]==0] <- NA
users_data_basic$fourth_language <- primary_languages[,2]
users_data_basic$fourth_language[repositories_in_languages[,2]==0] <- NA
users_data_basic$fifth_language <- primary_languages[,1]
users_data_basic$fifth_language[repositories_in_languages[,1]==0] <- NA
users_data_basic$repositories_primary_language <- repositories_in_languages[,5]
users_data_basic$repositories_secondary_language <- repositories_in_languages[,4]
users_data_basic$repositories_third_language <- repositories_in_languages[,3]
users_data_basic$repositories_fourth_language <- repositories_in_languages[,2]
users_data_basic$repositories_fifth_language <- repositories_in_languages[,1]

#Select only NY users
#users_data_basic <- users_data_basic[unique(sort(c(grep("NY",users_data_basic$location),grep("York",users_data_basic$location)))),] #lsit of users from NY only

#Timing featreus
users_data_basic <- users_data_basic[users_data_basic$type=="User",] #we do not deal with organizations
users_data_basic$type <- NULL
users_data_basic$site_admin <- NULL
users_data_basic$created_at <- as.Date(users_data_basic$created_at)
users_data_basic$updated_at <- as.Date(users_data_basic$updated_at)
users_data_basic$days_from_creation <- as.numeric(Sys.Date()-users_data_basic$created_at)
users_data_basic$days_from_last_update <- as.numeric(Sys.Date()-users_data_basic$updated_at)
users_data_basic$days_from_creation_to_last_update <- as.numeric(users_data_basic$updated_at-users_data_basic$created_at)

#Events data
users_events_complete <- expand.grid(Date=unique(users_events$Date)[1:3],login=sort(unique(users_data_basic$login)))
users_events <- merge(users_events_complete,users_events,all.x = T)
users_events[is.na(users_events)] <- 0
users_events <- users_events[order(users_events$login),]
users_events_changes <- NULL
for (i in 1:length(unique(users_events$login)))
{
    user_tmp_events <- users_events[users_events$login==as.character(unique(users_events$login)[i]),c(3:ncol(users_events))][c(1,3),]
    user_events_changes <- (user_tmp_events[2,]-user_tmp_events[1,])/user_tmp_events[1,] #changes in precent
    names(user_events_changes) <- paste0(names(user_events_changes),"_change_90_days_pr")
    user_events_changes[which((user_tmp_events[1,]==0) & (user_tmp_events[2,]==0))] <- 0
    users_events_changes <- rbind(users_events_changes,user_events_changes)
}
users_events_changes[users_events_changes==Inf] <- NA
users_data_basic <- cbind(users_data_basic,users_events_changes)

#Pull requests data
users_pr <- merge(data.frame(login=users_data_basic$login),users_pr,all.x = T)
users_pr[is.na(users_pr)] <- 0
users_pr$repo[users_pr$repo==0] <- NA
users_data_basic$pr_merged_count_last_90_days <- aggregate(merged~login,data=users_pr,sum)$merged
users_data_basic$pr_comments_count_last_90_days <- aggregate(comments~login,data=users_pr,sum)$comments
users_data_basic <- merge(users_data_basic,aggregate(repo~login,data=users_pr,function(x) length(unique(x))),all.x = T)
names(users_data_basic)[ncol(users_data_basic)] <- "pr_repos_last_90_days"
users_data_basic$pr_repos_last_90_days[is.na(users_data_basic$pr_repos_last_90_days)] <- 0
users_data_basic$pr_commits_count_last_90_days <- aggregate(commits~login,data=users_pr,sum)$commits
users_data_basic$pr_additions_count_last_90_days <- aggregate(additions~login,data=users_pr,sum)$additions
users_data_basic$pr_deletions_count_last_90_days <- aggregate(deletions~login,data=users_pr,sum)$deletions
users_data_basic$pr_changed_files_count_last_90_days <- aggregate(changed_files~login,data=users_pr,sum)$changed_files
users_data_basic$merged_per_pr_last_90_days <- users_data_basic$pr_merged_count_last_90_days/users_data_basic$PullRequestEvent
users_data_basic$comments_per_pr_last_90_days <- users_data_basic$pr_comments_count_last_90_days/users_data_basic$PullRequestEvent
users_data_basic$commits_per_pr_last_90_days <- users_data_basic$pr_commits_count_last_90_days/users_data_basic$PullRequestEvent
users_data_basic$additions_per_pr_last_90_days <- users_data_basic$pr_additions_count_last_90_days/users_data_basic$PullRequestEvent
users_data_basic$deletions_per_pr_last_90_days <- users_data_basic$pr_deletions_count_last_90_days/users_data_basic$PullRequestEvent
users_data_basic$changed_files_per_pr_last_90_days <- users_data_basic$pr_changed_files_count_last_90_days/users_data_basic$PullRequestEvent
users_data_basic$merged_per_repos_last_90_days <- users_data_basic$pr_merged_count_last_90_days/users_data_basic$pr_repos_last_90_days
users_data_basic$comments_per_repos_last_90_days <- users_data_basic$pr_comments_count_last_90_days/users_data_basic$pr_repos_last_90_days
users_data_basic$commits_per_repos_last_90_days <- users_data_basic$pr_commits_count_last_90_days/users_data_basic$pr_repos_last_90_days
users_data_basic$additions_per_repos_last_90_days <- users_data_basic$pr_additions_count_last_90_days/users_data_basic$pr_repos_last_90_days
users_data_basic$deletions_per_repos_last_90_days <- users_data_basic$pr_deletions_count_last_90_days/users_data_basic$pr_repos_last_90_days
users_data_basic$changed_files_per_repos_last_90_days <- users_data_basic$pr_changed_files_count_last_90_days/users_data_basic$pr_repos_last_90_days
users_data_basic$additions_per_changed_files_last_90_days <- users_data_basic$pr_additions_count_last_90_days/users_data_basic$pr_changed_files_count_last_90_days
users_data_basic$deletions_per_changed_files_last_90_days <- users_data_basic$pr_deletions_count_last_90_days/users_data_basic$pr_changed_files_count_last_90_days
users_data_basic$current_date <- Sys.Date() #the current date of the dataset status
users_data_basic[is.nan(users_data_basic)] <- 0
for (i in 48:(ncol(users_data_basic)-1)) { users_data_basic[is.na(users_data_basic[,i]),i] <- 0 } #NA is 0 at the moment

#Exploritory analysis ####
#build egdes and nodes
langueages_to_take <- sort(unique(names(which(sort(table(users_data_basic$primary_language))>1)),names(which(sort(table(users_data_basic$secondary_language))>1))))
edges <- data.frame(from=as.character(users_data_basic$primary_language),to=as.character(users_data_basic$secondary_language),weight=1,stringsAsFactors = F)
edges <- edges[!is.na(edges$to),]
edges <- unique(edges)
edges <- edges[edges$to %in% langueages_to_take,]
edges <- edges[edges$from %in% langueages_to_take,]
while (i<nrow(edges))
{
    rm_idx <- which(edges$to==edges$from[i] & edges$from==edges$to[i])
    if (length(rm_idx)>0) {edges <- edges[-rm_idx,]}
    i <- i+1
}

nodes <- data.frame(id=unique(c(edges$from,edges$to)))
nodes$label <- nodes$id
ig <- graph_from_data_frame(edges,directed=F)
nodes$value <- betweenness(ig)
clusters <- cluster_edge_betweenness(ig)
length(clusters)
nodes$group <- clusters$membership
visNetwork(nodes,edges)  %>% visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) #an interactive viewer item

#histograms
hist(users_data_basic$days_from_creation,100,xlab="Days",main="Histogram of active acount days"); box()
hist(users_data_basic$days_from_last_update,100,xlab="Days",main="Histogram of days since last update"); box()
hist(users_data_basic$days_from_creation_to_last_update,100,xlab="Days",main="Histogram of days since last update"); box()

plot(cumsum(sort(users_data_basic$public_repos,decreasing = T)/sum(users_data_basic$public_repos)))

plot(users_events$PushEvent[users_events$login==users_events$login[1]],type="l",ylab="Push Events",main=users_events$login[1],xaxt='n',xlab="Date"); axis(1,at=1:3,labels=levels(users_events$Date))
plot(users_events$PullRequestEvent[users_events$login==users_events$login[1]],type="l",ylab="Pull Events",main=users_events$login[1],xaxt='n',xlab="Date"); axis(1,at=1:3,labels=levels(users_events$Date))
plot(users_pr$commits[users_pr$login==users_pr$login[1]],type="l",ylab="Commits",main=users_pr$login[1],xaxt='n',xlab="Date"); axis(1,at=1:length(users_pr$merged[users_pr$login==users_pr$login[1]]),labels=users_pr$created_at[users_pr$login==users_pr$login[1]])
plot(users_pr$comments[users_pr$login==users_pr$login[1]],type="l",ylab="comments",main=users_pr$login[1],xaxt='n',xlab="Date"); axis(1,at=1:length(users_pr$merged[users_pr$login==users_pr$login[1]]),labels=users_pr$created_at[users_pr$login==users_pr$login[1]])
plot(users_pr$additions[users_pr$login==users_pr$login[1]],type="l",ylab="additions",main=users_pr$login[1],xaxt='n',xlab="Date"); axis(1,at=1:length(users_pr$merged[users_pr$login==users_pr$login[1]]),labels=users_pr$created_at[users_pr$login==users_pr$login[1]])
plot(users_pr$deletions[users_pr$login==users_pr$login[1]],type="l",ylab="deletions",main=users_pr$login[1],xaxt='n',xlab="Date"); axis(1,at=1:length(users_pr$merged[users_pr$login==users_pr$login[1]]),labels=users_pr$created_at[users_pr$login==users_pr$login[1]])

#Save data ####
write.csv(users_data_basic,paste0("dataset_",Sys.Date(),".csv"),row.names = F)
