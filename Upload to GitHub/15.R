#Init ####
rm(list=ls()) # clear workspace
setwd(mainDir)
set.seed(1)
library(lubridate)
library(httr)
library(plyr)
library(dplyr)
library(zoo)
library(rlist)
library(pipeR)
library(stringr)
library(rjson)
library(jsonlite)

#Getting the data ####
users_list <- read.csv("List.csv")
users_list$email <- as.character(users_list$email)
users_list$name <- as.character(users_list$name)
users_list$location <- as.character(users_list$location)
users_list$primary_language <- as.character(users_list$primary_language)
users_list$secondary_language <- as.character(users_list$secondary_language)
users_list$third_language <- as.character(users_list$third_language)
users_list$fourth_language <- as.character(users_list$fourth_language)
users_list$fifth_language <- as.character(users_list$fifth_language)
users_list$login <- as.character(users_list$login)
users_list$X <- NULL
    
# create a list of candidates' emails:
Emails <- users_list$email
Emails <- Emails[Emails!="0"]
Emails <- unique(Emails)

# replace the character "@" with "%40" to create the url:
NewEmails = gsub("@", "%40", Emails)

# create the list of candidates' details by calling the API:
L=list()
for (i in 1:length(NewEmails)){
    cat(i,"\n")
    URL = paste0("https://api.pipl.com/search/?email=", NewEmails[i], "&key=BUSINESS-f5at0sezejb52jlp783nayc4")
    L[[i]]=content(GET(url=URL))
}
#save that list into json files! (some might be not saved for some reson)
for (i in 1:length(Emails))
{
    write(toJSON(L[[i]]),paste0(getwd(),"/pipl json data/",users_list$login[users_list$email==Emails[i]][1],".json"))
}

# a candidate will be labeled a "good candidate" if he has anything under the "jobs" object
GoodCandidates=list()
k=0
for (i in 1:length(L)) #
{
    cat(i,"\n")
    possible_person <- NULL
    if (length(L[[i]]$possible_persons)>0) 
    {
        cat("multiple people\n")
        for (j in 1:length(L[[i]]$possible_persons)) #
        {
            idx_users_list <- NULL
            idx_users_list <- which(users_list$name %in% lapply(L[[i]]$possible_persons[[j]]$names,'[[', "display")) #the person to look for
            possible_persons_email <- L[[i]]$query$emails[[1]]$address
            idx_users_list <- intersect(idx_users_list,which(users_list$email %in% possible_persons_email)) #in case of a few names
            idx_users_list <- idx_users_list[users_list$location[idx_users_list]!=""]
            if (length(idx_users_list)>0)
            {
                user_location <- users_list$location[idx_users_list]
                if (length(grep("NY|New York City|NYC",user_location))>0) {user_location <- "New York"}
                idx_possible_persons_list <- NULL
                idx_possible_persons_list <- grep(user_location,lapply(L[[i]]$possible_persons[[j]]$addresses,'[[', "display")) #index in the multiple that contains the location as well 
                if (length(idx_possible_persons_list)>0)
                {
                    possible_person <- L[[i]]$possible_persons[[j]]
                    break
                }
            }
            
        }
    }
    if (length(L[[i]]$person)>0) 
    {
        cat("Found a user\n")
        possible_person <- L[[i]]$person
    }
    if (length(possible_person$jobs)>0) 
    {
        k=k+1
        FullName = lapply(possible_person$names, '[[', "display")
        Emails = lapply(possible_person$emails, '[[', "address")
        Usernames = lapply(possible_person$usernames, '[[', "content")
        Addresses = lapply(possible_person$addresses, '[[', "display")
        Gender = possible_person$gender$content
        Language = lapply(possible_person$languages, '[[', "language")
        #NumJobs = length(possible_person$jobs)
        DatesVec = lapply(possible_person$jobs, '[[', "date_range")
        #Start=strptime(lapply(DatesVec, '[[', "start"), "%Y-%m-%d")
        #End=strptime(lapply(DatesVec, '[[', "end"), "%Y-%m-%d")
        #YearsOfExp = as.integer(difftime(End,Start)/365) # vector with years of experience in each job, 
        Degrees=unlist(lapply(possible_person$educations, '[[', "degree"))
        School=unlist(lapply(possible_person$educations, '[[', "school"))
        Titles=unlist(lapply(possible_person$jobs, '[[', "title"))
        Organizations = unlist(lapply(possible_person$jobs, '[[', "organization"))
        Displays = unlist(lapply(possible_person$jobs, '[[', "display"))
        DatesVecSchool = lapply(possible_person$educations, '[[', "date_range")
        URLs = unlist(lapply(possible_person$urls, '[[', "url"))
        GoodCandidates[[k]] = list(FullName,Emails,Addresses,Gender,Language,Organizations,DatesVec,Titles,Displays,Degrees,School,DatesVecSchool,URLs,Usernames)
        rm(FullName,Language,Organizations,DatesVec,Degrees,Titles,Displays,School,DatesVecSchool,Emails,Addresses,URLs,Usernames)
    } 
}
write(toJSON(GoodCandidates), "Candidates.json")

#Analize data ####
world_university_ranking$School.name <- as.character(world_university_ranking$School.name)
world_university_ranking <- world_university_ranking[!duplicated(world_university_ranking$School.name),]
companies_ranking$Company <- as.character(companies_ranking$Company)
companies_ranking <- companies_ranking[!duplicated(companies_ranking$Company),]
dataset <- NULL
#For each person we extract the data so it will fit and merged to the github data
for (i in 1:length(GoodCandidates))
{
    #Deal with demographcis data
    name <- GoodCandidates[[i]][[1]][[1]]
    if (length(GoodCandidates[[i]][[14]])>0) 
    {
        username <- GoodCandidates[[i]][[14]][[1]]
    } else {
        username <- NA
    }
    if (length(GoodCandidates[[i]][[2]])>0) 
    {
        email <- GoodCandidates[[i]][[2]][[1]]
    } else {
        email <- NA
    }
    if (length(GoodCandidates[[i]][[3]])>0) 
    {
        location <- GoodCandidates[[i]][[3]][[1]]
    } else {
        location <- NA
    }
    if (length(GoodCandidates[[i]][[4]])>0) 
    {
        gender <- GoodCandidates[[i]][[4]][[1]]
    } else {
        gender <- NA
    }
    if (length(GoodCandidates[[i]][[5]])>0) 
    {
        spoken_language <- GoodCandidates[[i]][[5]][[1]]
    } else {
        spoken_language <- NA
    }
    
    #Deal with previous work experience
    if (!is.null(GoodCandidates[[i]][[6]]))
    {
        precent_of_ranked_comapnies <- sum(sapply(GoodCandidates[[i]][[6]],function(x) {companies_ranking$Company==x},simplify = T,USE.NAMES = F))/length(GoodCandidates[[i]][[6]])
    } else {
        precent_of_ranked_comapnies <- 0
    }
    
    rank_list <- c("intern","junior","senior","low_managerial","high_managerial")
    intern <- grepl("Intern|Student",GoodCandidates[[i]][[8]])
    junior <- grepl("Support|Service|Application|Jr\\.|Junior|Designer|Analyst|Developer|Programmer|Engineer|Architect",GoodCandidates[[i]][[8]])
    senior <- grepl("Sr|Senior|Assistant|Lead|Specialist|Sr\\.|Leader|Associate",GoodCandidates[[i]][[8]])
    low_managerial <- grepl("Principal|Manager|Supervisor|Fellow|Officer|Research",GoodCandidates[[i]][[8]])
    high_managerial <- grepl("Director|VP|CEO|President|Executive|CTO|Chief",GoodCandidates[[i]][[8]])
    higest_rank <- rank_list[max(which(unlist(lapply(list(intern,junior,senior,low_managerial,high_managerial),function(x) {"TRUE" %in% x}))))]
    
    independent <- sum(grepl("Contractor|Consultant|Freelance|Freelancer|Contract",GoodCandidates[[i]][[8]]))
    was_independent <- (independent>0)
    entrepreneurship <- sum(grepl("Founder|Co-Founder|Partner|Co-founder|Owner",GoodCandidates[[i]][[8]]))
    number_of_roles <- length(GoodCandidates[[i]][[8]])
    
    Start <- as.Date(strptime(lapply(GoodCandidates[[i]][[7]],'[[',"start"),"%Y-%m-%d"))
    End <- as.Date(strptime(lapply(GoodCandidates[[i]][[7]],'[[',"end"),"%Y-%m-%d"))
    years_of_experience <- sum(unique(as.numeric(difftime(End,Start,units = "days")/365)),na.rm=T)
    avg_time_in_role <- mean(unique(as.numeric(difftime(End,Start,units = "days")/365)),na.rm=T)
    if (years_of_experience>0)
    {
        if (Start[!is.na(Start)]<Sys.Date() & is.na(End[!is.na(Start)]))
        {
            time_in_current_role <- as.numeric(Sys.Date()-max(Start,na.rm=T))/365
            is_working_now <- 1
            current_company <- GoodCandidates[[i]][[6]][1]
            current_role <- GoodCandidates[[i]][[8]][1]
        } else {
            time_in_current_role <- 0
            is_working_now <- 0
            current_company <- NA
            current_role <- NA
        }
    } else {
        time_in_current_role <- 0
        is_working_now <- 0
        current_company <- NA
        current_role <- NA
    }

    #Deal with academic studies
    degree_list <- c("diploma","bachelor","law","master","mba","medical","doctor")
    diploma <- grepl("Diploma|School|HS",GoodCandidates[[i]][[10]])
    bachelor <- grepl("\\.B\\.|Bs|BA|Bachelor|B\\.A\\.|BS|BFA|B\\.S\\.|A\\.B\\.|B\\.E.|B\\.S|Engineer|B\\.Tech\\.|BSA|BSE|Bachlors|B-Tech|BSEE",GoodCandidates[[i]][[10]])
    mba <- grepl("MBA",GoodCandidates[[i]][[10]])
    master <- grepl("\\.M\\.|Ms|MA|Master|Masters|M\\.A\\.|MS|M\\.S\\.|M\\.Tech\\.|MFA|M\\.Sc\\.|MSE| M\\.Sc\\.",GoodCandidates[[i]][[10]])
    law <- grepl("JD|J\\.D\\.|Law|LLB|LLM",GoodCandidates[[i]][[10]])
    doctor <- grepl("Ph\\.|Phd|Doctor|PhD|Ph\\.D",GoodCandidates[[i]][[10]])
    medical <- grepl("M\\.Ed\\.",GoodCandidates[[i]][[10]])
    higest_degree <- degree_list[max(which(unlist(lapply(list(diploma,bachelor,law,master,mba,medical,doctor),function(x) {"TRUE" %in% x}))))] #higest degree rank
    if (length(higest_degree)==0) {higest_degree <- NA}
    
    avg_universities_rank <- mean(unlist(sapply(GoodCandidates[[i]][[11]],function(x) {world_university_ranking$Rank[world_university_ranking$School.name==x]},simplify = T,USE.NAMES = F)),na.rm = T)
    if (is.na(avg_universities_rank)|is.null(avg_universities_rank)|is.nan(avg_universities_rank)) avg_universities_rank <- mean(unlist(sapply(GoodCandidates[[i]][[11]],function(x) {world_university_ranking$Rank[agrep(x,world_university_ranking$School.name)]},simplify = T,USE.NAMES = F)),na.rm = T)

    Start <- as.Date(strptime(lapply(GoodCandidates[[i]][[12]],'[[',"start"),"%Y-%m-%d"))
    End <- as.Date(strptime(lapply(GoodCandidates[[i]][[12]],'[[',"end"),"%Y-%m-%d"))
    years_of_study <- sum(unique(as.numeric(difftime(End,Start,units = "days")/365)),na.rm=T)
    time_from_end_of_study <- as.numeric(Sys.Date()-max(End,na.rm=T))/365
    is_studying_now <- 0
    if (length(time_from_end_of_study)==0 | time_from_end_of_study==Inf | is.na(time_from_end_of_study)) 
    {
        time_from_end_of_study <- NA
    } else {
        if (time_from_end_of_study<0) 
        {
            is_studying_now <- 1
            time_from_end_of_study <- 0
        }
    }
    
    #User URL's
    url <- paste0(unique(GoodCandidates[[i]][[13]]),collapse = ";")
    
    dataset <- rbind(dataset,data.frame(name=name,username=username,email=email,location=location,gender=gender,spoken_language=spoken_language,current_company=current_company,current_role=current_role,time_in_current_role=time_in_current_role,is_working_now=is_working_now,
                                        number_of_roles=number_of_roles,higest_rank=higest_rank,was_independent=was_independent,entrepreneurship=entrepreneurship,precent_of_ranked_comapnies=precent_of_ranked_comapnies,years_of_experience=years_of_experience,avg_time_in_role=avg_time_in_role,
                                        higest_degree=higest_degree,avg_universities_rank=avg_universities_rank,years_of_study=years_of_study,time_from_end_of_study=time_from_end_of_study,is_studying_now=is_studying_now,url=url))
    rm(name,username,email,location,gender,spoken_language,higest_degree,avg_universities_rank,years_of_study,time_from_end_of_study,is_studying_now,years_of_experience,avg_time_in_role,time_in_current_role,is_working_now,precent_of_ranked_comapnies,higest_rank,was_independent,entrepreneurship,number_of_roles,current_role,current_company,url)
}

write.csv(dataset,"dataset_Linkedin_CourseHorse.csv",row.names = F)
