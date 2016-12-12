rm(list=ls()) # clear workspace
setwd(mainDir)
set.seed(1)
require(lubridate)
require(httr)

###############

# create a list of candidates' emails:
# replace the character "@" with "%40" to create the url:
NewEmails = gsub("@", "%40", Emails)

# create the list of candidates' details by calling the API:
L=list()
for (i in 1:length(NewEmails)){
  URL = paste0("https://api.pipl.com/search/?email=", NewEmails[i], "&key=sample_key")
  L[[i]]=content(GET(url=URL))
}


# a candidate will be labeled a "good candidate" if he has anything under the "jobs" object
GoodCandidates=list()
k=0
IndexToRefine=NULL #emails with multiple people associated with them --> a refined search is required

for (i in 1:length(L)){
  if (length(L[[i]]$possible_persons)>0) {IndexToRefine=c(IndexToRefine, i)
  next}
  if (length(L[[i]]$available_data$premium$jobs)>0) {
    k=k+1
    FullName = lapply(L[[i]]$person$names, '[[', "display")
    NumJobs = length(L[[i]]$person$jobs)
    DatesVec = lapply(L[[i]]$person$jobs, '[[', "date_range")
    S = lapply(DatesVec, '[[', "start")
    E = lapply(DatesVec, '[[', "end")
    Start=strptime(S, "%Y-%m-%d")
    End=strptime(E, "%Y-%m-%d")
    YearsOfExp = as.integer(difftime(End,Start)/365) # vector with years of experience in each job, 
    # jobs with no end date will result in NA work experience
    Degrees=unlist(lapply(L[[i]]$person$educations, '[[', "degree"))
    Titles=unlist(lapply(L[[i]]$person$jobs, '[[', "title"))
    Organizations = unlist(lapply(L[[i]]$person$jobs, '[[', "organization"))
    GoodCandidates[[k]] = list(FullName, NumJobs, Organizations, YearsOfExp, Degrees, Titles)
  } 
}

# create the list of candidates' details by calling the API:
L=list()
for (i in 1:length(NewEmails)){
    URL = paste0("https://api.pipl.com/search/?email=", NewEmails[i], "&key=BUSINESS-f5at0sezejb52jlp783nayc4")
    L[[i]]=content(GET(url=URL))
}

