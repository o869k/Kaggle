#Initializaiotn ####
rm(list=ls()) # clear workspace
#mainDir <- "./"
setwd(mainDir)
set.seed(1)
options(java.parameters = "-Xmx4g")
library(randomForest)
library(tm)

#Inputs ####
args=(commandArgs(TRUE))
if(length(args)==0){
  print("No arguments supplied.")
  outputfile <- 'sample-out.json'
}else{
  inputfile_github <- as.character(args[1])
  inputfile_github <- as.character(args[2])
  outputfile <- as.character(args[3])
}

#Read models data

#Read additional data
world_university_ranking$School.name <- as.character(world_university_ranking$School.name)
world_university_ranking <- world_university_ranking[!duplicated(world_university_ranking$School.name),]
companies_ranking$Company <- as.character(companies_ranking$Company)
companies_ranking <- companies_ranking[!duplicated(companies_ranking$Company),]

#Constants ####
#Type of 42 languanges out there (importatn languages are last)
languages <- c("0","ActionScript","Arduino","ASP","Clojure","CoffeeScript",
               "D","Dart","Eagle","EmacsLisp","Erlang","Go","Haskell",        
               "HTML","JupyterNotebook","Liquid","Lua","Mathematica","Matlab",         
               "OCaml","OpenSCAD","Perl","PowerShell","Processing","PureData",       
               "Python","R","Ruby","Scala","Scheme","Shell","Swift","TeX",      
               "VimL","XSLT","C","C#","C++","CSS","Java","JavaScript","Objective-C","PHP")

basic_info <- c("login","id","type","site_admin","name","company","location","created_at","updated_at","hireable","bio","email","blog","public_repos","public_gists","followers","following","forks","starred","watchers","user_starred","user_watchers",
                "PushEvent","PullRequestEvent","PullRequestReviewCommentEvent","IssueCommentEvent","CreateEvent","ForkEvent","IssuesEvent","DeleteEvent","WatchEvent","PublicEvent","CommitCommentEvent","ReleaseEvent","MemberEvent","GollumEvent")

def_languages <- c("fifth","fourth","third","secondary","primary")

rank_list <- c("intern","junior","senior","low_managerial","high_managerial")

degree_list <- c("diploma","bachelor","law","master","mba","medical","doctor")

# list of positions hierarchy-related terms
R1 <- c("intern", "student")
R2 <- c("support", "service", "application","jr\\.","junior","designer","analys","develop","program","engine","architect")
R3 <- c("sr","senior","assistant","lead","specialist","sr\\.","leader","assoc")
R4 <- c("principal","manag","supervis","fellow","officer","research")
R5 <- c("direct","vp", "ceo","presid", "execut", "cto", "chief", "corp")
R6 <- c("founder","cofound", "partner","owner")

# list of full stack developer terms & languages. This list can be expanded
L11 <- c("app", "agil", "architect", "comput", "data", "design", "dev",
         "digit", "engin", "genom", "gis", "helpdesk", "java", "linux", "unix", "perl",
         "program", "rubi", "softwar", "solut", "sys", "technolog", "user", "web",
         "environ", "stack", "full", "end", "front", "back", "mobil", "\\bit\\b", "analys")
S1 <- c("Linux","Unix","DevOps","SCSS","LESS",
        "css","HTML","Java","JavaScript","JupyterNotebook",         
        "Objective-C","Perl","PHP",       
        "Python","Ruby","React","NodeJS","Shell",     
        "VimL","XSLT", "\\bC+\\b", "b\\C#\\b", "\\bC\\b")
S1 <- tolower(S1)
L1 <- unique(c(L11, S1))

#Functions ####
#A fucntion to check the promotions
advance_in_company <- function(CandRoles,idx) {
    CandRoles[3*idx] <- names(A[which(A>=1)])[idx]
    CandRoles[3*idx+1] <- as.integer(A[which(A>=1)])[idx]
    if (!is.na(CandRoles[3*idx]) & (CandRoles[3*idx+1]>1))
    {
        a1 <- as.character(corp2_stemmed[[1]])
        a2 <- a1[which(grepl(CandRoles[3*idx], a1))]
        vec <- rep(FALSE,length(a2))
        for (j in 1:6)
        {
            vec <- vec + grepl(paste(eval(parse(text=paste0("R",j))), collapse = "|"), a2)
        }
        CandRoles[3*idx+2] <- ifelse(prod(diff(vec)<0)==0,1,0)
    } else
    {
        CandRoles[3*idx+2] <- 0
    }
    return(CandRoles)
}
range01 <- function(x) {
    (x-min(x))/(max(x)-min(x))
    }

#Read & make data ready Data ####
#GitHub
users_data_github <- as.data.frame(jsonlite::fromJSON(inputfile_github),stringsAsFactors = FALSE)
users_data_github$blog <- as.character(users_data_github$blog)
basic_data <- users_data_github[,c("login","id","name","company","location","email","blog")]

#Remove unwanted columns
users_data_github$id <- NULL; users_data_github$name <- NULL; users_data_github$company <- NULL
users_data_github$location <- NULL; users_data_github$created_at <- NULL; users_data_github$updated_at <- NULL
users_data_github$hireable <- NULL; users_data_github$bio <- NULL; users_data_github$email <- NULL
users_data_github$blog <- NULL; users_data_github$current_date <- NULL; users_data_github$X_row <- NULL
users_data_github$public_repos <- as.numeric(users_data_github$public_repos)
users_data_github$public_gists <- as.numeric(users_data_github$public_gists)
users_data_github$followers <- as.numeric(users_data_github$followers)
users_data_github$following <- as.numeric(users_data_github$following)
users_data_github$primary_language <- factor(as.character(users_data_github$primary_language),levels = languages)
users_data_github$secondary_language <- factor(as.character(users_data_github$secondary_language),levels = languages)
users_data_github$third_language <- factor(as.character(users_data_github$third_language),levels = languages)
users_data_github$fourth_language <- factor(as.character(users_data_github$fourth_language),levels = languages)
users_data_github$fifth_language <- factor(as.character(users_data_github$fifth_language),levels = languages)
users_data_github[is.na(users_data_github)] <- 0 #missing info is 0 at the moment

#Pipl
users_data_pipl <- jsonlite::fromJSON(inputfile_pipl)
possible_person <- NULL
if (length(users_data_pipl$possible_persons)>0) 
{
    cat("multiple people\n")
    for (j in 1:length(users_data_pipl$possible_persons)) #
    {
        idx_users_list <- NULL
        idx_users_list <- which(users_list$name %in% lapply(users_data_pipl$possible_persons[[j]]$names,'[[', "display")) #the person to look for
        possible_persons_email <- users_data_pipl$query$emails[[1]]$address
        idx_users_list <- intersect(idx_users_list,which(users_list$email %in% possible_persons_email)) #in case of a few names
        idx_users_list <- idx_users_list[users_list$location[idx_users_list]!=""]
        if (length(idx_users_list)>0)
        {
            user_location <- users_list$location[idx_users_list]
            if (length(grep("NY|New York City|NYC",user_location,ignore.case = T))>0) {user_location <- "New York"}
            idx_possible_persons_list <- NULL
            idx_possible_persons_list <- grep(user_location,lapply(users_data_pipl$possible_persons[[j]]$addresses,'[[', "display"),ignore.case = T) #index in the multiple that contains the location as well 
            if (length(idx_possible_persons_list)>0)
            {
                possible_person <- users_data_pipl$possible_persons[[j]]
                break
            }
        }
        
    }
}
if (length(users_data_pipl$person)>0) 
{
    cat("Found a user\n")
    possible_person <- users_data_pipl$person
}
if (length(possible_person$jobs)>0) 
{
    FullName <- unlist(possible_person$names$display)
    Emails <- unlist(possible_person$emails$address)
    Usernames <- unlist(possible_person$usernames$content)
    Addresses <- unlist(possible_person$addresses$display)
    Gender <- possible_person$gender$content
    Language <- unlist(possible_person$languages$language)
    DatesVec <- possible_person$jobs$date_range
    Degrees <- unlist(possible_person$educations$degree)
    School <- unlist(possible_person$educations$school)
    Titles <- unlist(possible_person$jobs$title)
    Organizations <- unlist(possible_person$jobs$organization)
    Displays <- unlist(possible_person$jobs$display)
    DatesVecSchool <- possible_person$educations$date_range
    URLs <- unlist(possible_person$urls$url)
    possible_person <- list(FullName,Emails,Addresses,Gender,Language,Organizations,DatesVec,Titles,Displays,Degrees,School,DatesVecSchool,URLs,Usernames)
    rm(FullName,Language,Organizations,DatesVec,Degrees,Titles,Displays,School,DatesVecSchool,Emails,Addresses,URLs,Usernames)
} 

#Deal with some demographcis data
name <- possible_person[[1]][ifelse(grep(basic_data$name,possible_person[[1]],ignore.case = T),grep(basic_data$name,possible_person[[1]],ignore.case = T),1)]
username <- ifelse(length(possible_person[[14]])>0,possible_person[[14]][[1]],NA)
email <- possible_person[[2]][ifelse(grep(basic_data$email,possible_person[[2]],ignore.case = T),grep(basic_data$email,possible_person[[2]],ignore.case = T),1)]
location <- possible_person[[3]][ifelse(agrep(basic_data$location,possible_person[[3]],ignore.case = T),agrep(basic_data$location,possible_person[[3]],ignore.case = T),1)]
gender <- ifelse(length(possible_person[[4]])>0,possible_person[[4]][[1]],NA)
spoken_language <- ifelse(length(possible_person[[5]])>0,possible_person[[5]][[1]],NA)

#Deal with previous work experience
precent_of_ranked_comapnies <- ifelse(!is.null(possible_person[[6]]),sum(sapply(possible_person[[6]],function(x) {companies_ranking$Company==x},simplify = T,USE.NAMES = F))/length(possible_person[[6]]),0)

intern <- grepl(paste(R1,collapse="|"),possible_person[[8]],ignore.case = T)
junior <- grepl(paste(R2,collapse="|"),possible_person[[8]],ignore.case = T)
senior <- grepl(paste(R3,collapse="|"),possible_person[[8]],ignore.case = T)
low_managerial <- grepl(paste(R4,collapse="|"),possible_person[[8]],ignore.case = T)
high_managerial <- grepl(paste(R5,collapse="|"),possible_person[[8]],ignore.case = T)
higest_rank <- rank_list[max(which(unlist(lapply(list(intern,junior,senior,low_managerial,high_managerial),function(x) {"TRUE" %in% x}))))]
independent <- sum(grepl("Contractor|Consultant|Freelance|Freelancer|Contract",possible_person[[8]]),ignore.case = T)
was_independent <- (independent>0)
entrepreneurship <- sum(grepl(paste(R6,collapse="|"),possible_person[[8]]),ignore.case = T)
number_of_roles <- length(possible_person[[8]])

Start <- as.Date(strptime(possible_person[[7]]$start,"%Y-%m-%d"))
End <- as.Date(strptime(possible_person[[7]]$end,"%Y-%m-%d"))
years_of_experience <- sum(unique(as.numeric(difftime(End,Start,units = "days")/365)),na.rm=T)
avg_time_in_role <- mean(unique(as.numeric(difftime(End,Start,units = "days")/365)),na.rm=T)
if (years_of_experience>0)
{
    if ((Start[!is.na(Start)]<Sys.Date())[1] & (is.na(End[!is.na(Start)]))[1])
    {
        time_in_current_role <- as.numeric(Sys.Date()-max(Start,na.rm=T))/365
        is_working_now <- 1
        current_company <- possible_person[[6]][1]
        current_role <- possible_person[[8]][1]
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
diploma <- grepl("Diploma|School|HS",possible_person[[10]],ignore.case = T)
bachelor <- grepl("\\.B\\.|Bs|BA|Bachelor|B\\.A\\.|BS|BFA|B\\.S\\.|A\\.B\\.|B\\.E.|B\\.S|Engineer|B\\.Tech\\.|BSA|BSE|Bachlors|B-Tech|BSEE",possible_person[[10]],ignore.case = T)
mba <- grepl("MBA",possible_person[[10]],ignore.case = T)
master <- grepl("\\.M\\.|Ms|MA|Master|Masters|M\\.A\\.|MS|M\\.S\\.|M\\.Tech\\.|MFA|M\\.Sc\\.|MSE| M\\.Sc\\.",possible_person[[10]],ignore.case = T)
law <- grepl("JD|J\\.D\\.|Law|LLB|LLM",possible_person[[10]],ignore.case = T)
doctor <- grepl("Ph\\.|Phd|Doctor|PhD|Ph\\.D",possible_person[[10]],ignore.case = T)
medical <- grepl("M\\.Ed\\.",possible_person[[10]],ignore.case = T)
higest_degree <- degree_list[max(which(unlist(lapply(list(diploma,bachelor,law,master,mba,medical,doctor),function(x) {"TRUE" %in% x}))))] #higest degree rank
if (length(higest_degree)==0) {higest_degree <- NA}

avg_universities_rank <- mean(unlist(sapply(possible_person[[11]],function(x) {world_university_ranking$Rank[world_university_ranking$School.name==x]},simplify = T,USE.NAMES = F)),na.rm = T)
if (is.na(avg_universities_rank)|is.null(avg_universities_rank)|is.nan(avg_universities_rank)) avg_universities_rank <- mean(unlist(sapply(possible_person[[11]],function(x) {world_university_ranking$Rank[agrep(x,world_university_ranking$School.name)]},simplify = T,USE.NAMES = F)),na.rm = T)

Start <- as.Date(strptime(possible_person[[12]]$start,"%Y-%m-%d"))
End <- as.Date(strptime(possible_person[[12]]$end,"%Y-%m-%d"))
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
url <- paste0(unique(possible_person[[13]]),collapse = ";")

users_data_pipl <- data.frame(name=name,username=username,email=email,location=location,gender=gender,spoken_language=spoken_language,current_company=current_company,current_role=current_role,time_in_current_role=time_in_current_role,is_working_now=is_working_now,
                              number_of_roles=number_of_roles,higest_rank=higest_rank,was_independent=was_independent,entrepreneurship=entrepreneurship,precent_of_ranked_comapnies=precent_of_ranked_comapnies,years_of_experience=years_of_experience,avg_time_in_role=avg_time_in_role,
                              higest_degree=higest_degree,avg_universities_rank=avg_universities_rank,years_of_study=years_of_study,time_from_end_of_study=time_from_end_of_study,is_studying_now=is_studying_now,url=url)

#Build the corpuses of each user
corp <- Corpus(VectorSource(list(possible_person[[9]])))
corp <- tm_map(corp,content_transformer(tolower))
exceptions   <- c("it")
my_stopwords <- setdiff(stopwords("english"), exceptions)
corp <- tm_map(corp, removeWords, my_stopwords)
corp <- tm_map(corp, removeNumbers)
corp <- tm_map(corp, removePunctuation, preserve_intra_word_dashes = FALSE)
corp_stemmed <- tm_map(corp,stemDocument) #Stem list

#companies
corp1 <- Corpus(VectorSource(list(possible_person[[6]])))
corp1 <- tm_map(corp1,content_transformer(tolower))
corp1 <- tm_map(corp1, removeWords, stopwords("english"))
corp1 <- tm_map(corp1, removePunctuation, preserve_intra_word_dashes = FALSE)
corp1_stemmed <- tm_map(corp1,stemDocument)

#roles and companies
corp2 <- Corpus(VectorSource(list(possible_person[[9]])))
corp2 <- tm_map(corp2,content_transformer(tolower))
corp2 <- tm_map(corp2, removeNumbers)
corp2 <- tm_map(corp2, removeWords, stopwords("english"))
corp2 <- tm_map(corp2, removePunctuation, preserve_intra_word_dashes = FALSE)
corp2_stemmed <- tm_map(corp2,stemDocument)

CandScores <- users_data_pipl[,c(1,3)]
for (i in 1:length(L1)){
    CandScores[L1[i]] <- sum(grepl(L1[i],corp_stemmed[[1]]),ignore.case = T)-1
}
# as a first test, see if the sum of all the software-related terms is correlated with the expert's tagging.
CandScores$SumSoftwarRelated <- rowSums(CandScores[,3:(2+length(L1))])

CandRoles <- users_data_pipl[,c(1,3)]
# Assume there are at most 5 companies in which the candidate had several positions
CandRoles$RepeatingCompany1 <- NA; CandRoles$Roles1 <- NA; CandRoles$DidAdvance1 <- NA
CandRoles$RepeatingCompany2 <- NA; CandRoles$Roles2 <- NA; CandRoles$DidAdvance2 <- NA
CandRoles$RepeatingCompany3 <- NA; CandRoles$Roles3 <- NA; CandRoles$DidAdvance3 <- NA
CandRoles$RepeatingCompany4 <- NA; CandRoles$Roles4 <- NA; CandRoles$DidAdvance4 <- NA
CandRoles$RepeatingCompany5 <- NA; CandRoles$Roles5 <- NA; CandRoles$DidAdvance5 <- NA

A <- sort(sapply(unique(as.character(corp1_stemmed[[1]])),function(y) sum(as.character(corp1_stemmed[[1]]) %in% y)),decreasing = T) # vector of occurances of positions in each company
if (length(A)==0) {next}
for (i in 1:5)
{
    CandRoles <- advance_in_company(CandRoles,i)
}

#Check the consistency as a developer status
CandRoles$Last3JobsPhrase1<- NA; CandRoles$FreqPhrase1<- NA; CandRoles$Phrase1RelatedToSoftware <- NA
CandRoles$Last3JobsPhrase2<- NA; CandRoles$FreqPhrase2<- NA; CandRoles$Phrase2RelatedToSoftware <- NA
CandRoles$Last3JobsPhrase3<- NA; CandRoles$FreqPhrase3<- NA; CandRoles$Phrase3RelatedToSoftware <- NA
CandRoles$Last3JobsPhrase4<- NA; CandRoles$FreqPhrase4<- NA; CandRoles$Phrase4RelatedToSoftware <- NA
CandRoles$Last3JobsPhrase5<- NA; CandRoles$FreqPhrase5<- NA; CandRoles$Phrase5RelatedToSoftware <- NA

x1 <- as.character(corp_stemmed[[1]])[1:3]
x1 <- x1[!is.na(x1)]
y1 <- unlist(strsplit(x1, " "))
# remove empty terms:
y11 <- y1 [! y1 %in% ""]
z1 <- as.data.frame(table(y11))
z11 <- z1[order(z1$Freq, decreasing = T),]
if (length(z11)==0) {next}
CandRoles$Last3JobsPhrase1 <- as.character(z11$y11[1])
CandRoles$FreqPhrase1 <- z11$Freq[1]
CandRoles$Phrase1RelatedToSoftware <- ifelse(grepl(paste(L1, collapse="|"), CandRoles$Last3JobsPhrase1,ignore.case = T), 1,0)
CandRoles$Last3JobsPhrase2 <- as.character(z11$y11[2])
CandRoles$FreqPhrase2 <- z11$Freq[2]
CandRoles$Phrase2RelatedToSoftware <- ifelse(grepl(paste(L1, collapse="|"), CandRoles$Last3JobsPhrase2,ignore.case = T), 1,0)
CandRoles$Last3JobsPhrase3 <- as.character(z11$y11[3])
CandRoles$FreqPhrase3 <- z11$Freq[3]
CandRoles$Phrase3RelatedToSoftware <- ifelse(grepl(paste(L1, collapse="|"), CandRoles$Last3JobsPhrase3,ignore.case = T), 1,0)
CandRoles$Last3JobsPhrase4 <- as.character(z11$y11[4])
CandRoles$FreqPhrase4 <- z11$Freq[4]
CandRoles$Phrase4RelatedToSoftware <- ifelse(grepl(paste(L1, collapse="|"), CandRoles$Last3JobsPhrase4,ignore.case = T), 1,0)
CandRoles$Last3JobsPhrase5 <- as.character(z11$y11[5])
CandRoles$FreqPhrase5 <- z11$Freq[5]
CandRoles$Phrase5RelatedToSoftware <- ifelse(grepl(paste(L1, collapse="|"), CandRoles$Last3JobsPhrase5,ignore.case = T), 1,0)

candidate_data <- cbind(CandScores,CandRoles)

users_data_pipl <- users_data_pipl[c("name","email","number_of_roles","higest_rank","was_independent","entrepreneurship","precent_of_ranked_comapnies",
                                                 "years_of_experience","higest_degree","avg_universities_rank","years_of_study","time_from_end_of_study")]
names(users_data_pipl)[names(users_data_pipl)=="name"] <- "name_linkedin"
users_data_pipl$name_linkedin <- as.character(users_data_pipl$name_linkedin)
users_data_pipl$email <- as.character(users_data_pipl$email)

names(candidate_data)[names(candidate_data)=="name"] <- "name_linkedin"
tmp <- as.data.frame(t(apply(candidate_data,1,function(x) {
    wordcount <- x[c(3:(which(names(candidate_data)=="SumSoftwarRelated")-1))]
    wordcount <- wordcount[wordcount>0]
    tail(names(sort(wordcount)),5) })))
names(tmp) <- gsub("V","Frequenct_word_",names(tmp)) 
candidate_data <- candidate_data[,-c(3:(which(names(candidate_data)=="SumSoftwarRelated")-1))]
candidate_data <- cbind(candidate_data,tmp)
names(candidate_data)[grep("FreqPhrase",names(candidate_data))] <- paste0("Last3Jobs_",names(candidate_data)[grep("FreqPhrase",names(candidate_data))]) 
names(candidate_data)[grep("RelatedToSoftware",names(candidate_data))] <- paste0("Last3Jobs_",names(candidate_data)[grep("RelatedToSoftware",names(candidate_data))]) 
candidate_data$RepeatingCompany1 <- NULL; candidate_data$RepeatingCompany2 <- NULL; candidate_data$RepeatingCompany3 <- NULL
candidate_data$RepeatingCompany4 <- NULL; candidate_data$RepeatingCompany5 <- NULL
candidate_data$Last3Jobs_FreqPhrase1 <- NULL; candidate_data$Last3JobsPhrase1 <- NULL
candidate_data$Last3Jobs_FreqPhrase2 <- NULL; candidate_data$Last3JobsPhrase2 <- NULL
candidate_data$Last3Jobs_FreqPhrase3 <- NULL; candidate_data$Last3JobsPhrase3 <- NULL
candidate_data$Last3Jobs_FreqPhrase4 <- NULL; candidate_data$Last3JobsPhrase4 <- NULL
candidate_data$Last3Jobs_FreqPhrase5 <- NULL; candidate_data$Last3JobsPhrase5 <- NULL
candidate_data$name_linkedin <- as.character(candidate_data$name_linkedin)
candidate_data$email <- as.character(candidate_data$email)

users_data_github$company <- NULL #well get it from linkedin
users_data_github$location <- NULL #well get it from linkedin
users_data_github$created_at <- NULL #well get it from linkedin
users_data_github$updated_at <- NULL #well get it from linkedin
users_data_github$bio <- NULL #well get it from linkedin
users_data_github$blog <- NULL #well get it from linkedin
users_data_github$id <- NULL #well get it from linkedin
users_data_github$hireable <- NULL #well get it from linkedin
names(users_data_github)[names(users_data_github)=="name"] <- "name_github"
users_data_github$login <- as.character(users_data_github$login)
users_data_github$public_repos <- as.numeric(users_data_github$public_repos)
users_data_github$public_gists <- as.numeric(users_data_github$public_gists)
users_data_github$public_gists <- as.numeric(users_data_github$public_gists)
users_data_github$followers <- as.numeric(users_data_github$followers)
users_data_github$following <- as.numeric(users_data_github$following)

#Merged the data to one dataset
dataset <- cbind(users_data_github[,c(1:34)],users_data_pipl)
dataset <- cbind(dataset,candidate_data)

#Complete missign info
dataset$PublicEvent <- NULL
dataset$CommitCommentEvent <- NULL
dataset$ReleaseEvent <- NULL
dataset$PullRequestReviewCommentEvent <- NULL
dataset$IssueCommentEvent <- NULL
dataset$IssuesEvent <- NULL
dataset$ForkEvent <- NULL
dataset$DeleteEvent <- NULL
dataset$WatchEvent <- NULL
dataset$MemberEvent <- NULL
dataset$GollumEvent <- NULL
dataset$User.s.lifetime.number.of.orders <- NULL
dataset$higest_rank <- as.character(dataset$higest_rank); dataset$higest_rank[is.na(dataset$higest_rank)] <- "Unknown"
dataset$higest_degree <- as.character(dataset$higest_degree); dataset$higest_degree[is.na(dataset$higest_degree)] <- "Unknown"
dataset$avg_universities_rank[is.na(dataset$avg_universities_rank)] <- 150
dataset$time_from_end_of_study[is.na(dataset$time_from_end_of_study)] <- 6 #the mean\median is ~6 years according to previous dataset
dataset$Roles2[is.na(dataset$Roles2)] <- 0
dataset$Roles3[is.na(dataset$Roles3)] <- 0
dataset$Roles4[is.na(dataset$Roles4)] <- 0
dataset$Roles5[is.na(dataset$Roles5)] <- 0
dataset$Frequenct_word_1 <- as.character(dataset$Frequenct_word_1)
dataset$Frequenct_word_2 <- as.character(dataset$Frequenct_word_2)
dataset$Frequenct_word_3 <- as.character(dataset$Frequenct_word_3)
dataset$Frequenct_word_4 <- as.character(dataset$Frequenct_word_4)
dataset$Frequenct_word_5 <- as.character(dataset$Frequenct_word_5)
dataset$Frequenct_word_1[is.na(dataset$Frequenct_word_1)] <- "Unknown"
dataset$Frequenct_word_2[is.na(dataset$Frequenct_word_2)] <- "Unknown"
dataset$Frequenct_word_3[is.na(dataset$Frequenct_word_3)] <- "Unknown"
dataset$Frequenct_word_4[is.na(dataset$Frequenct_word_4)] <- "Unknown"
dataset$Frequenct_word_5[is.na(dataset$Frequenct_word_5)] <- "Unknown"

dataset$name_linkedin <- NULL
dataset$name_github <- NULL
dataset$name_coursehorse <- NULL
dataset$email <- NULL
dataset$name_linkedin.1 <- NULL
dataset$email.1 <- NULL
dataset$login <- NULL

#Test A classifier ####
#Testing for position Full stack engineer
#Additional columns
add_columns <- NULL
add_columns <- names(rf_model_full_stack_engineer$importance[,1])[!(names(rf_model_full_stack_engineer$importance[,1]) %in% names(dataset))]
for (column in add_columns)
{
    dataset$x <- 0
    names(dataset)[ncol(dataset)] <- column
}
dataset <- dataset[,names(rf_model_full_stack_engineer$forest$xlevels)]

#Factor again based on model
dataset$higest_rank <- factor(dataset$higest_rank,levels = rf_model_full_stack_engineer$forest$xlevels$higest_rank)
dataset$higest_degree <- factor(dataset$higest_degree,levels = rf_model_full_stack_engineer$forest$xlevels$higest_degree)
dataset$was_independent <- factor(dataset$was_independent,levels = rf_model_full_stack_engineer$forest$xlevels$was_independent)
dataset$entrepreneurship <- factor(dataset$entrepreneurship,levels = rf_model_full_stack_engineer$forest$xlevels$entrepreneurship)
dataset$DidAdvance1 <- factor(dataset$DidAdvance1,levels = rf_model_full_stack_engineer$forest$xlevels$DidAdvance1)
dataset$DidAdvance2 <- factor(dataset$DidAdvance2,levels = rf_model_full_stack_engineer$forest$xlevels$DidAdvance2)
dataset$DidAdvance3 <- factor(dataset$DidAdvance3,levels = rf_model_full_stack_engineer$forest$xlevels$DidAdvance3)
dataset$DidAdvance4 <- factor(dataset$DidAdvance4,levels = rf_model_full_stack_engineer$forest$xlevels$DidAdvance4)
dataset$DidAdvance5 <- factor(dataset$DidAdvance5,levels = rf_model_full_stack_engineer$forest$xlevels$DidAdvance5)
dataset$Last3Jobs_Phrase1RelatedToSoftware <- factor(dataset$Last3Jobs_Phrase1RelatedToSoftware,levels = rf_model_full_stack_engineer$forest$xlevels$Last3Jobs_Phrase1RelatedToSoftware)
dataset$Last3Jobs_Phrase2RelatedToSoftware <- factor(dataset$Last3Jobs_Phrase2RelatedToSoftware,levels = rf_model_full_stack_engineer$forest$xlevels$Last3Jobs_Phrase2RelatedToSoftware)
dataset$Last3Jobs_Phrase3RelatedToSoftware <- factor(dataset$Last3Jobs_Phrase3RelatedToSoftware,levels = rf_model_full_stack_engineer$forest$xlevels$Last3Jobs_Phrase3RelatedToSoftware)
dataset$Last3Jobs_Phrase4RelatedToSoftware <- factor(dataset$Last3Jobs_Phrase4RelatedToSoftware,levels = rf_model_full_stack_engineer$forest$xlevels$Last3Jobs_Phrase4RelatedToSoftware)
dataset$Last3Jobs_Phrase5RelatedToSoftware <- factor(dataset$Last3Jobs_Phrase5RelatedToSoftware,levels = rf_model_full_stack_engineer$forest$xlevels$Last3Jobs_Phrase5RelatedToSoftware)

pred_full_stack <- round(predict(rf_model_full_stack_engineer,dataset,type="prob")[,2],2) #prdiction for full stack engineer

#Testing for languages
pred_language <- NULL
for (l in 5:1)
{
    users_data_github_language <- NULL
    language <- def_languages[l]
    users_data_github_language <- users_data_github[,c(1,grep(language,names(users_data_github),ignore.case = T))]
    names(users_data_github_language) <- unlist(lapply(strsplit(names(users_data_github_language),paste0("_",language)),function(x) {x[[1]][1]}))
    names(users_data_github_language)[2] <- "language"
    users_data_github_language$watchers <- NULL
    
    add_columns <- NULL
    add_columns <- names(rf_language$importance[,1])[!(names(rf_language$importance[,1]) %in% names(users_data_github_language))]
    for (column in add_columns)
    {
        users_data_github_language$x <- 0
        names(users_data_github_language)[ncol(users_data_github_language)] <- column
    }
    
    #Add more featrues
    users_data_github_language$PullRequestCommentEventRatio <- users_data_github_language$PullRequestReviewCommentEvent/users_data_github_language$PullRequestEvent
    users_data_github_language$IssueCommentEventRatio <- users_data_github_language$IssueCommentEvent/users_data_github_language$IssuesEvent
    users_data_github_language$PushEventperRepo <- users_data_github_language$PushEvent/users_data_github_language$repositories
    users_data_github_language$CreateEventperRepo <- users_data_github_language$CreateEvent/users_data_github_language$repositories
    users_data_github_language$WatchEventperRepo <- users_data_github_language$WatchEvent/users_data_github_language$repositories
    users_data_github_language$starredperRepo <- users_data_github_language$starred/users_data_github_language$repositories
    users_data_github_language$forksperRepo <- users_data_github_language$forks/users_data_github_language$repositories
    users_data_github_language$starredperforks <- users_data_github_language$starred/users_data_github_language$forks
    users_data_github_language$PushDeleteRatio <- users_data_github_language$PushEvent/users_data_github_language$DeleteEvent
    users_data_github_language[is.na(users_data_github_language)] <- 0 # rest of values are just 0, not really missing
    users_data_github_language[users_data_github_language==Inf] <- 0 # rest of values are just 0, not really missing

    pred_language[[l]] <- round(predict(rf_language,users_data_github_language,type="prob")[,2],2) #prdiction for full stack engineer
    
}

results <- data.frame(basic_data,users_data_github[,c("primary_language","secondary_language","third_language","fourth_language","fifth_language")],prob_to_full_stack=pred_full_stack,
                      prob_primary=pred_language[[5]],prob_secondary=pred_language[[4]],prob_third=pred_language[[3]],prob_fourth=pred_language[[2]],prob_fifth=pred_language[[1]])

#Save data ####
results$prob_fifth[results$fifth_language=="0"] <- 0
results$prob_fourth[results$fourth_language=="0"] <- 0
results$prob_third[results$third_language=="0"] <- 0
results$prob_secondary[results$secondary_language=="0"] <- 0
results$prob_primary[results$primary_language=="0"] <- 0
write(jsonlite::toJSON(results), outputfile)

q(save="no")
