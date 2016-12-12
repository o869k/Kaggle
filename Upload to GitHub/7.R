#Initializaiotn ####
rm(list=ls()) # clear workspace
setwd(mainDir)
set.seed(1)
options(java.parameters = "-Xmx4g")
library(github)
library(plyr)
library(zoo)
library(httr)
library(dplyr)
library(rlist)
library(pipeR)
library(rjson)

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#Input arguments
{
args=(commandArgs(TRUE))
if(length(args)==0){
    print("No arguments supplied.")
    ##supply default values
}else{
    dataset <- read.table(args[1], sep=",")
}
}

#Read Users Data ####
dataset$Purchaser.Email <- trim(dataset$Purchaser.Email)
dataset$Purchase.Last.Name <- trim(dataset$Purchase.Last.Name)
dataset$Purchase.First.Name <- trim(dataset$Purchase.First.Name)
dataset$State <- NA # trim(dataset$State)
dataset$City <- NA #trim(dataset$City)

#Type of 42 languanges out there (importatn languages are last)
languages <- c("ActionScript","Arduino","ASP","Clojure","CoffeeScript",
               "D","Dart","Eagle","EmacsLisp","Erlang","Go","Haskell",        
               "HTML","JupyterNotebook","Liquid","Lua","Mathematica","Matlab",         
               "OCaml","OpenSCAD","Perl","PowerShell","Processing","PureData",       
               "Python","R","Ruby","Scala","Scheme","Shell","Swift","TeX",      
               "VimL","XSLT","C","C#","C++","CSS","Java","JavaScript","Objective-C","PHP")

basic_info <- c("login","id","type","site_admin","name","company","location","created_at","updated_at","hireable","bio","email","blog","public_repos","public_gists","followers","following","forks","starred","watchers","user_starred","user_watchers",
                "PushEvent","PullRequestEvent","PullRequestReviewCommentEvent","IssueCommentEvent","CreateEvent","ForkEvent","IssuesEvent","DeleteEvent","WatchEvent","PublicEvent","CommitCommentEvent","ReleaseEvent","MemberEvent","GollumEvent")

def_languages <- c("fifth","fourth","third","secondary","primary")

#Using the GiHub package
ctx <- interactive.login('713bf83bd510ba72222c', 'c9d17d5c8368b111cf683cb8b3e022311e080360',scopes=c("gist","repo","user")) #login

#Extracing data ####
for (j in 1:nrow(dataset))
{
    cat(dataset$Purchase.First.Name[j],dataset$Purchase.Last.Name[j],"...")
    cat(dataset$Purchaser.Email[j],"...")
    
    #Matching those peaople username on github
    users_login <- NULL
    #users_login <- availble$login[j]
    if (as.numeric(searcher$headers$`x-ratelimit-remaining`)<3)
    {
        Sys.sleep(30)
    }
    searcher <- search.users(q=paste(dataset$Purchaser.Email[j],"in:email",collapse = " "),ctx=ctx,per_page=300)
    if ((searcher$ok) & (searcher$content$total_count>0))
    {
        users_login <- searcher$content$items[[1]]$login
    } else {
        if ((dataset$Purchase.First.Name[j]!="") & (dataset$Purchase.Last.Name[j]!=""))
        {
            searcher <- search.users(q=paste(dataset$Purchase.First.Name[j],dataset$Purchase.Last.Name[j],"in:fullname",collapse = " "),ctx=ctx,per_page=300)
            if ((searcher$ok) & (searcher$content$total_count==1))
            {
                users_login <- searcher$content$items[[1]]$login
            } else
            {
                if ((searcher$ok) & (searcher$content$total_count>1) & !is.na(dataset$City[j]))
                {
                    potential_matches <- searcher$content$items %>>% list.mapv(login)
                    for (potential_match in potential_matches)
                    {
                        potential_match_location <- get.user(user=potential_match,ctx=ctx)$content$location
                        if (!is.null(potential_match_location))
                        {
                            if (potential_match_location==dataset$City[j])
                            {
                                users_login <- searcher$content$items[[1]]$login
                                cat(users_login,"\n")
                            }   
                        }
                    }
                } 
            }
        }
    }
    
    #Scrapper
    if (!is.null(users_login))
    {
        #Demographics data
        githubUser <- get.user(user=users_login,ctx=ctx) #specific user data
        user_data <- NULL
        if (githubUser$ok)
        {
            user_data <- data.frame(t(unlist(githubUser$content[c(1:2,16:30)])),stringsAsFactors=FALSE)
            if(is.null(githubUser$content$hireable)) {user_data$hireable <- NA}
            if(is.null(githubUser$content$bio)) {user_data$bio <- NA}
            user_data$created_at <- as.Date(strsplit(user_data$created_at,"T")[[1]][1])
            user_data$updated_at <- as.Date(strsplit(user_data$updated_at,"T")[[1]][1])
        } else {cat("No Demog token \n")}
        if (is.null(user_data$email)) user_data$email <- dataset$Purchaser.Email[j]
        if (is.na(user_data$email)) user_data$email <- dataset$Purchaser.Email[j]
        if (is.null(user_data$blog)) user_data$blog <- NA
        if (is.null(user_data$bio)) user_data$bio <- NA
        if (is.null(user_data$hireable)) user_data$hireable <- NA
        if (is.null(user_data$location)) user_data$location <- NA
        if (is.na(user_data$location)) user_data$location <- dataset$City[j]
        if (is.null(user_data$company)) user_data$company <- NA
        if (is.null(user_data$name)) user_data$name <- paste(dataset$Purchase.First.Name[j],dataset$Purchase.Last.Name[j])
        
        #Repo data
        user.repositories <- get.user.repositories(user=users_login,per_page=300,ctx=ctx)
        if (user.repositories$ok)
        {
            user_data[,c(18:59)] <- 0
            names(user_data)[c(18:59)] <- languages
            user_languages <- user.repositories$content %>>% list.filter(!is.null(language)) %>>% list.table(language) %>>% list.sort(-.) #languages per repo
            user_languages <- user_languages[c(names(user_languages) %in% names(user_data))]
            user_data[,names(user_languages)] <- user_languages
            user_data$forks <- sum(user.repositories$content %>>% list.mapv(forks_count))
            user_data$starred <- sum(user.repositories$content %>>% list.mapv(stargazers_count)) 
            user_data$watchers <- sum(user.repositories$content %>>% list.mapv(watchers_count)) 
            repositories.starred.by.user <- get.repositories.starred.by.user(user=users_login,per_page=300,ctx=ctx)
            user_data$user_starred <- length(repositories.starred.by.user$content)
            repositories.watched.by.user <- get.repositories.watched.by.user(user=users_login,ctx=ctx)
            user_data$user_watchers <- length(repositories.watched.by.user$content)
        } else {cat("No repos token \n")}
        
        #User events
        user.performed.events <- get.user.performed.events(user=users_login,per_page=300,ctx=ctx)
        user_pr <- NULL
        user_events <- NULL
        if (user.performed.events$ok)
        {
            #Events counts
            user_data[,c(65:78)] <- 0
            names(user_data)[c(65:78)] <- c("PushEvent","PullRequestEvent","PullRequestReviewCommentEvent","IssueCommentEvent","CreateEvent","ForkEvent","IssuesEvent","DeleteEvent","WatchEvent","PublicEvent","CommitCommentEvent","ReleaseEvent","MemberEvent","GollumEvent")
            user_events_data <- as.data.frame(t(user.performed.events$content %>>% list.table(type) %>>% list.sort(-.)))
            user_data[names(user_events_data)] <- user_events_data
            
            #Pul request time series
            pr_number<- length(user.performed.events$content %>>% list.filter(type == "PullRequestEvent"))
            if (pr_number>0)
            {
                user_pr <- data.frame(login = rep(users_login,pr_number))
                user_pr$repo <- user.performed.events$content %>>% list.filter(type == "PullRequestEvent") %>>% list.mapv(repo$name)
                user_pr$created_at <- unlist(lapply(strsplit(user.performed.events$content %>>% list.filter(type == "PullRequestEvent") %>>% list.mapv(payload$pull_request$created_at),"T"),function(x) x[[1]][1]))
                user_pr$updated_at <- unlist(lapply(strsplit(user.performed.events$content %>>% list.filter(type == "PullRequestEvent") %>>% list.mapv(payload$pull_request$updated_at),"T"),function(x) x[[1]][1]))
                user_pr$merged <- user.performed.events$content %>>% list.filter(type == "PullRequestEvent") %>>% list.mapv(payload$pull_request$merged)
                user_pr$merged_at <- NA
                if (sum(user_pr$merged)>0) {user_pr$merged_at[user_pr$merged==TRUE] <- unlist(lapply(strsplit(user.performed.events$content %>>% list.filter(type == "PullRequestEvent") %>>% list.mapv(payload$pull_request$merged_at),"T"),function(x) x[[1]][1]))}
                user_pr$comments <- user.performed.events$content %>>% list.filter(type == "PullRequestEvent") %>>% list.mapv(payload$pull_request$comments)
                user_pr$commits <- user.performed.events$content %>>% list.filter(type == "PullRequestEvent") %>>% list.mapv(payload$pull_request$commits)
                user_pr$additions <- user.performed.events$content %>>% list.filter(type == "PullRequestEvent") %>>% list.mapv(payload$pull_request$additions)
                user_pr$deletions <- user.performed.events$content %>>% list.filter(type == "PullRequestEvent") %>>% list.mapv(payload$pull_request$deletions)
                user_pr$changed_files <- user.performed.events$content %>>% list.filter(type == "PullRequestEvent") %>>% list.mapv(payload$pull_request$changed_files)
                user_pr <- user_pr[order(user_pr$login,user_pr$created_at),] #sort
                user_pr$language <- NA
                for (repo in unique(user_pr$repo))
                {
                    repo_lang <- get.repository.languages(owner=strsplit(repo,"/")[[1]][1],repo=strsplit(repo,"/")[[1]][2],ctx=ctx)
                    if (length(repo_lang$content)>0) {user_pr$language[user_pr$repo==repo] <- names(repo_lang$content)}
                }
                
            }
            
            #All events data
            user_events <- as.data.frame.matrix(user.performed.events$content %>>% list.table(created_at,type))
            user_events$Date <- unlist(lapply(strsplit(row.names(user_events),"T"),function(x) x[[1]][1]))
            user_events$Date <- as.yearmon(user_events$Date)
            if (nrow(user_events)>0)
            {
                user_events <- aggregate(. ~ Date,data=user_events,sum)
                user_events[is.na(user_events)] <- 0
            } else {user_events <- NULL}
            
        } else {cat("No pull requests token \n")}
        
        #Manipualte Data
        #Basic Information
        user_data_basic <- user_data[,basic_info]
        user_data_basic$hireable[is.na(user_data_basic$hireable)] <- FALSE
        
        #Handle languages
        user_data_languages <- user_data[,!(names(user_data) %in% basic_info)]
        user_data_languages[is.na(user_data_languages)] <- 0
        primary_languages <- t(apply(user_data_languages,1,function(x) {names(tail(sort(x),5))})) #primary languages
        repositories_in_languages <- t(apply(user_data_languages,1,function(x) {tail(sort(x),5)})) #num of repositories in a language
        user_data_basic$primary_language <- primary_languages[,5]
        user_data_basic$primary_language[repositories_in_languages[,5]==0] <- NA
        user_data_basic$secondary_language <- primary_languages[,4]
        user_data_basic$secondary_language[repositories_in_languages[,4]==0] <- NA
        user_data_basic$third_language <- primary_languages[,3]
        user_data_basic$third_language[repositories_in_languages[,3]==0] <- NA
        user_data_basic$fourth_language <- primary_languages[,2]
        user_data_basic$fourth_language[repositories_in_languages[,2]==0] <- NA
        user_data_basic$fifth_language <- primary_languages[,1]
        user_data_basic$fifth_language[repositories_in_languages[,1]==0] <- NA
        user_data_basic$repositories_primary_language <- repositories_in_languages[,5]
        user_data_basic$repositories_secondary_language <- repositories_in_languages[,4]
        user_data_basic$repositories_third_language <- repositories_in_languages[,3]
        user_data_basic$repositories_fourth_language <- repositories_in_languages[,2]
        user_data_basic$repositories_fifth_language <- repositories_in_languages[,1]
        
        #Timing featreus
        user_data_basic$type <- NULL
        user_data_basic$site_admin <- NULL
        user_data_basic$created_at <- as.Date(user_data_basic$created_at)
        user_data_basic$updated_at <- as.Date(user_data_basic$updated_at)
        user_data_basic$days_from_creation <- as.numeric(Sys.Date()-user_data_basic$created_at)
        user_data_basic$days_from_last_update <- as.numeric(Sys.Date()-user_data_basic$updated_at)
        user_data_basic$days_from_creation_to_last_update <- as.numeric(user_data_basic$updated_at-user_data_basic$created_at)
        
        #Events data
        if (!is.null(user_events))
        {
            user_events <- user_events[1:nrow(user_events),]
            user_events[is.na(user_events)] <- 0
            user_events_changes <- NULL
            user_events_changes <- (user_events[nrow(user_events),-1]-user_events[1,-1])/user_events[1,-1] #changes in precent
            names(user_events_changes) <- paste0(names(user_events_changes),"_change_90_days_pr")
            user_events_changes[which((user_events[1,-1]==0) & (user_events[2,-1]==0))] <- 0
            user_events_changes[user_events_changes==Inf] <- NA
            user_data_basic <- cbind(user_data_basic,user_events_changes)
        }
        
        #Pull requests data
        if (!is.null(user_pr))
        {
            user_pr[is.na(user_pr)] <- 0
            user_pr$repo[user_pr$repo==0] <- NA
            user_data_basic$pr_merged_count_last_90_days <- aggregate(merged~login,data=user_pr,sum)$merged
            user_data_basic$pr_comments_count_last_90_days <- aggregate(comments~login,data=user_pr,sum)$comments
            user_data_basic <- merge(user_data_basic,aggregate(repo~login,data=user_pr,function(x) length(unique(x))),all.x = T)
            names(user_data_basic)[ncol(user_data_basic)] <- "pr_repos_last_90_days"
            user_data_basic$pr_repos_last_90_days[is.na(user_data_basic$pr_repos_last_90_days)] <- 0
            user_data_basic$pr_commits_count_last_90_days <- aggregate(commits~login,data=user_pr,sum)$commits
            user_data_basic$pr_additions_count_last_90_days <- aggregate(additions~login,data=user_pr,sum)$additions
            user_data_basic$pr_deletions_count_last_90_days <- aggregate(deletions~login,data=user_pr,sum)$deletions
            user_data_basic$pr_changed_files_count_last_90_days <- aggregate(changed_files~login,data=user_pr,sum)$changed_files
            user_data_basic$merged_per_pr_last_90_days <- user_data_basic$pr_merged_count_last_90_days/user_data_basic$PullRequestEvent
            user_data_basic$comments_per_pr_last_90_days <- user_data_basic$pr_comments_count_last_90_days/user_data_basic$PullRequestEvent
            user_data_basic$commits_per_pr_last_90_days <- user_data_basic$pr_commits_count_last_90_days/user_data_basic$PullRequestEvent
            user_data_basic$additions_per_pr_last_90_days <- user_data_basic$pr_additions_count_last_90_days/user_data_basic$PullRequestEvent
            user_data_basic$deletions_per_pr_last_90_days <- user_data_basic$pr_deletions_count_last_90_days/user_data_basic$PullRequestEvent
            user_data_basic$changed_files_per_pr_last_90_days <- user_data_basic$pr_changed_files_count_last_90_days/user_data_basic$PullRequestEvent
            user_data_basic$merged_per_repos_last_90_days <- user_data_basic$pr_merged_count_last_90_days/user_data_basic$pr_repos_last_90_days
            user_data_basic$comments_per_repos_last_90_days <- user_data_basic$pr_comments_count_last_90_days/user_data_basic$pr_repos_last_90_days
            user_data_basic$commits_per_repos_last_90_days <- user_data_basic$pr_commits_count_last_90_days/user_data_basic$pr_repos_last_90_days
            user_data_basic$additions_per_repos_last_90_days <- user_data_basic$pr_additions_count_last_90_days/user_data_basic$pr_repos_last_90_days
            user_data_basic$deletions_per_repos_last_90_days <- user_data_basic$pr_deletions_count_last_90_days/user_data_basic$pr_repos_last_90_days
            user_data_basic$changed_files_per_repos_last_90_days <- user_data_basic$pr_changed_files_count_last_90_days/user_data_basic$pr_repos_last_90_days
            user_data_basic$additions_per_changed_files_last_90_days <- user_data_basic$pr_additions_count_last_90_days/user_data_basic$pr_changed_files_count_last_90_days
            user_data_basic$deletions_per_changed_files_last_90_days <- user_data_basic$pr_deletions_count_last_90_days/user_data_basic$pr_changed_files_count_last_90_days
            user_data_basic$current_date <- Sys.Date() #the current date of the dataset status
        }
        
        user_data_basic[is.na(user_data_basic)] <- 0
        
        #languages_events_table
        if (!is.null(user_events))
        {
            language_events <- data.frame(repo=user.performed.events$content %>>% list.mapv(repo$name),type=user.performed.events$content %>>% list.mapv(type),language=NA)
            language_events$repo <- as.character(language_events$repo)
            language_events$type <- as.character(language_events$type)
            for (repo in unique(language_events$repo))
            {
                repo_lang <- get.repository.languages(owner=strsplit(repo,"/")[[1]][1],repo=strsplit(repo,"/")[[1]][2],ctx=ctx)
                if (length(repo_lang$content)>0) {language_events$language[language_events$repo==repo] <- names(repo_lang$content)}
            }
            language_events <- language_events[!is.na(language_events$language),]
            if (nrow(language_events)>0)
            {
                table_language_events <- aggregate(repo~language+type,data=language_events,length)
                #For top 5 languages extract the information again
                def_languages <- c("fifth","fourth","third","secondary","primary")
                for (i in c(5:1))
                {
                    language_tmp <- primary_languages[,i]
                    language_additional_info <- NULL
                    if (!is.null(language_tmp))
                    {
                        forks <- sum(user.repositories$content %>>% list.filter(language==language_tmp) %>>% list.mapv(forks_count)) 
                        starred <- sum(user.repositories$content %>>% list.filter(language==language_tmp) %>>% list.mapv(stargazers_count)) 
                        watchers <- sum(user.repositories$content %>>% list.filter(language==language_tmp) %>>% list.mapv(watchers_count)) 
                        table_language_events_tmp <- table_language_events[table_language_events$language==language_tmp,]
                        language_tmp_events <- NULL
                        if (nrow(table_language_events_tmp)>0)
                        {
                            language_tmp_events <- table_language_events_tmp$repo
                            names(language_tmp_events) <- table_language_events_tmp$type
                        }
                        language_additional_info <- c(forks=forks,starred=starred,watchers=watchers,language_tmp_events)
                        names(language_additional_info) <- paste0(names(language_additional_info),"_",def_languages[i])
                    }
                    user_data_basic <- cbind(user_data_basic,as.data.frame(t(language_additional_info)))
                }
            }
        }
        cat("Done\n")
        
        #Save user data - json
        user_data_basic$bio <- NA
        write(jsonlite::toJSON(user_data_basic),paste0(user_data_basic$login,"_",Sys.Date(),".json"))
        
    } else {cat("User did not found\n")}

}

# q(save="no")
