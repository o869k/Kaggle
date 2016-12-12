#Initializaiotn ####
rm(list=ls()) # clear workspace
setwd(mainDir)
set.seed(1)
options(java.parameters = "-Xmx8000m")
library(caret); library(rjson)
library(dplyr); library(randomForest)
library(epiR); library(pROC)
library(plyr)

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

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

#Read Data (json) ####
myJSON1 <- lapply(filenames1, function(x) fromJSON(file=x)) # a list in which each element is one of your original JSON files
users_data1 <- do.call("rbind.fill", lapply(myJSON1, as.data.frame))

myJSON2 <- lapply(filenames2, function(x) fromJSON(file=x)) # a list in which each element is one of your original JSON files
users_data2 <- do.call("rbind.fill", lapply(myJSON2, as.data.frame))

#Merge all data
users_data <- merge(users_data1,users_data2,all = T)

#Read tagging from difference sources
tag1 <- tag1[!is.na(tag1$Good..1..or.Not.Good..0.),c("login","Good..1..or.Not.Good..0.")]
tag2 <- tag2[!is.na(tag2$X),c("login","X")]
tag2 <- tag2[tag2$X!=-1,]
tag3 <- tag3[,c("login","label")]

labeling <- merge(tag3,tag1,all.x = T)
labeling <- merge(labeling,tag2,all.x = T)
labeling$final_tag <- labeling$X
labeling$final_tag[is.na(labeling$final_tag)] <- labeling$Good..1..or.Not.Good..0.[is.na(labeling$final_tag)]
labeling$final_tag[is.na(labeling$final_tag)] <- labeling$label[is.na(labeling$final_tag)]


#Remove unwanted columns
users_data$blog <- as.character(users_data$blog)
basic_data <- users_data[,c("login","id","name","company","location","hireable","email","blog")]
users_data$id <- NULL
users_data$name <- NULL
users_data$company <- NULL
users_data$location <- NULL
users_data$created_at <- NULL
users_data$updated_at <- NULL
users_data$hireable <- NULL
users_data$bio <- NULL
users_data$email <- NULL
users_data$blog <- NULL
users_data$current_date <- NULL
users_data$public_repos <- as.numeric(as.character(users_data$public_repos))
users_data$public_gists <- as.numeric(as.character(users_data$public_gists))
users_data$followers <- as.numeric(as.character(users_data$followers))
users_data$following <- as.numeric(as.character(users_data$following))
users_data$login <- as.character(users_data$login)
users_data$primary_language <- factor(as.character(users_data$primary_language),levels = languages)
users_data$secondary_language <- factor(as.character(users_data$secondary_language),levels = languages)
users_data$third_language <- factor(as.character(users_data$third_language),levels = languages)
users_data$fourth_language <- factor(as.character(users_data$fourth_language),levels = languages)
users_data$fifth_language <- factor(as.character(users_data$fifth_language),levels = languages)
users_data[is.na(users_data)] <- 0 #missing info is 0 at the moment
users_data$X_row <- NULL

rm(users_data1,users_data2,tag1,tag2,tag3,filenames2,filenames1,myJSON2,myJSON1)

#Dataset densities and historgrams, transformations
pdf("density_dataset.pdf")
par(mfrow=c(2,2))
for (i in c(7:ncol(users_data)))
{
    if (class(users_data[,i])=="numeric" | class(users_data[,i])=="integer")
    {
        d <- density(users_data[,i],na.rm=T)
        plot(d,main=names(users_data)[i],xlab="",xlim=c(min(d$x,d$x),max(d$x,d$x)),ylim=c(0,max(d$y,d$y)))
        polygon(d,col=rgb(0,0,0,0.5))
    }
}
par(mfrow=c(1,1))
dev.off()

#Train A classifier for a full stack engineer position ####
#Divide trainset and testset
trainset <- users_data[users_data$login %in% labeling$login,]
trainset <- merge(trainset,labeling[,c("login","final_tag")])
testset <- users_data[!(users_data$login %in% labeling$login),]
trainset <- trainset[,-c(35:37,64:83,90:112,114:120,122:155)] #non per language featrues
trainset <- rbind(trainset,trainset[trainset$final_tag==1,]) #even classes

sink("rf_model.txt")

ntrees <- 100
#Training - select the optimal cutoff
cutoff <- seq(0.01,0.99,0.01)
rf_train_acc <- rep(0,length(cutoff))
rf_model <- randomForest(x = trainset[,-c(1,69)],
                         y = factor(trainset$final_tag),
                         ntree=ntrees,
                         importance=T,
                         do.trace=F,
                         keep.forest=T)
pred <- range01(rf_model$votes[1:100,2]) #use OOB
for (i in 1:length(cutoff))
{

    stats <- confusionMatrix(as.numeric(pred>cutoff[i]),trainset$final_tag[1:100])
    rf_train_acc[i] <- round(stats$overall["Kappa"],2)
}
cat("Training Statistics:\n")
plot(rf_train_acc,type="l",ylab="Kappa.",main="Kappa vs. Cutoff",xlab="Cutoff")
opt_cutoff <- cutoff[which.max(rf_train_acc)]
cat("Optimal Cutoff: ",opt_cutoff,"\n")
epi.tests(table(pred>opt_cutoff,trainset$final_tag[1:100]), conf.level = 0.95)
round(ci.auc(trainset$final_tag[1:100],pred),2) #auc delong approx.
rf_model$importance; varImpPlot(rf_model)

#Validation
Times <- 10 #repetitions
K <- 10 #folds
folds <- createMultiFolds(trainset$final_tag,k = K,times = Times)
auc <- rep(NA,Times); auc_low <- rep(NA,Times); auc_high <- rep(NA,Times)
se <- rep(NA,Times); se_low <- rep(NA,Times); se_high <- rep(NA,Times)
sp <- rep(NA,Times); sp_low <- rep(NA,Times); sp_high <- rep(NA,Times)
for (i in 1:Times)
{
    pred <- NULL
    for (j in 1:K)
    {
        rf_model_valid <- randomForest(x = trainset[folds[[j+(i-1)*K]],-c(1,69)],
                                 y = factor(trainset$final_tag[folds[[j+(i-1)*K]]]),
                                 cutoff = c(1-opt_cutoff,opt_cutoff),
                                 ntree=ntrees,
                                 importance=T,
                                 do.trace=F,
                                 keep.forest=T)
        pred <- c(pred,predict(rf_model_valid,trainset[-folds[[j+(i-1)*K]],-c(1,ncol(trainset))],type="prob")[,2])
        
    }
    pred <- range01(pred[order(as.numeric(names(pred)))]) #order again (dont do for LOO)
    stats <- epi.tests(table(as.numeric(pred>opt_cutoff),trainset$final_tag), conf.level = 0.95)
    se[i] <- stats$rval$se[1][[1]]; sp[i] <- stats$rval$sp[1][[1]]
    se_low[i] <- stats$rval$se[2][[1]]; sp_low[i] <- stats$rval$sp[2][[1]]
    se_high[i] <- stats$rval$se[3][[1]]; sp_high[i] <- stats$rval$sp[3][[1]]
    auc[i] <- ci.auc(trainset$final_tag,pred)[2] #delong
    auc_low[i] <- ci.auc(trainset$final_tag,pred)[1] #delong
    auc_high[i] <- ci.auc(trainset$final_tag,pred)[3] #delong
}
cat("Validation Statistics:\n")
cat("mean AUC\n"); round(mean(auc),2); round(mean(auc_low),2); round(mean(auc_high),2);
cat("mean Sensitivity\n"); round(mean(se),2); round(mean(se_low),2); round(mean(se_high),2); 
cat("mean Specificty\n"); round(mean(sp),2); round(mean(sp_low),2); round(mean(sp_high),2); 

#Testing
pred_test <- round(range01((predict(rf_model,users_data,type="prob"))[,2]),2)
saveRDS(rf_model,"rf_model_full_stack_engineer.rds")
sink()

#Train A classifier for a Data Scientist position ####
#Divide trainset and testset
trainset <- users_data[users_data$login %in% labeling_data$login,]
trainset <- merge(trainset,labeling_data[,c("login","tag")])
testset <- users_data[!(users_data$login %in% labeling_data$login),]
trainset <- trainset[,-c(35:37,64:83,90:155)] #non per language featrues
trainset <- rbind(trainset,trainset[trainset$tag==1,])
trainset <- rbind(trainset,trainset[trainset$tag==1,])

sink("rf_model_data_scientist.txt")
ntrees <- 100
#Training - select the optimal cutoff
cutoff <- seq(0.01,0.99,0.01)
rf_train_acc <- rep(0,length(cutoff))
rf_model <- randomForest(x = trainset[,-c(1,ncol(trainset))],
                         y = factor(trainset$tag),
                         ntree=ntrees,
                         sampsize = 35,
                         mtry = 2,
                         importance=T,
                         do.trace=F,
                         keep.forest=T)
pred <- rf_model$votes[,2] #use OOB
for (i in 1:length(cutoff))
{
    
    stats <- confusionMatrix(as.numeric(pred>cutoff[i]),trainset$tag)
    rf_train_acc[i] <- round(stats$overall["Kappa"],2)
}
cat("Training Statistics:\n")
plot(rf_train_acc,type="l",ylab="Kappa.",main="Kappa vs. Cutoff",xlab="Cutoff")
opt_cutoff <- cutoff[which.max(rf_train_acc)]
cat("Optimal Cutoff: ",opt_cutoff,"\n")
epi.tests(table(pred>opt_cutoff,trainset$tag), conf.level = 0.95)
round(ci.auc(trainset$tag,pred),2) #auc delong approx.
#rf_model$importance; varImpPlot(rf_model)

#Validation
Times <- 5 #repetitions
K <- 3 #folds
folds <- createMultiFolds(trainset$tag,k = K,times = Times)
auc <- rep(NA,Times); auc_low <- rep(NA,Times); auc_high <- rep(NA,Times)
se <- rep(NA,Times); se_low <- rep(NA,Times); se_high <- rep(NA,Times)
sp <- rep(NA,Times); sp_low <- rep(NA,Times); sp_high <- rep(NA,Times)
for (i in 1:Times)
{
    pred <- NULL
    for (j in 1:K)
    {
        rf_model_valid <- randomForest(x = trainset[folds[[j+(i-1)*K]],-c(1,ncol(trainset))],
                                       y = factor(trainset$tag[folds[[j+(i-1)*K]]]),
                                       cutoff = c(1-opt_cutoff,opt_cutoff),
                                       ntree=ntrees,
                                       sampsize = 25,
                                       mtry = 2,
                                       importance=T,
                                       do.trace=F,
                                       keep.forest=T)
        pred <- c(pred,predict(rf_model_valid,trainset[-folds[[j+(i-1)*K]],-c(1,ncol(trainset))],type="prob")[,2])
        
    }
    pred <- pred[order(as.numeric(names(pred)))] #order again (dont do for LOO)
    stats <- epi.tests(table(as.numeric(pred>opt_cutoff),trainset$tag), conf.level = 0.95)
    se[i] <- stats$rval$se[1][[1]]; sp[i] <- stats$rval$sp[1][[1]]
    se_low[i] <- stats$rval$se[2][[1]]; sp_low[i] <- stats$rval$sp[2][[1]]
    se_high[i] <- stats$rval$se[3][[1]]; sp_high[i] <- stats$rval$sp[3][[1]]
    auc[i] <- ci.auc(trainset$tag,pred)[2] #delong
    auc_low[i] <- ci.auc(trainset$tag,pred)[1] #delong
    auc_high[i] <- ci.auc(trainset$tag,pred)[3] #delong
}
cat("Validation Statistics:\n")
cat("mean AUC\n"); round(mean(auc),2); round(mean(auc_low),2); round(mean(auc_high),2);
cat("mean Sensitivity\n"); round(mean(se),2); round(mean(se_low),2); round(mean(se_high),2); 
cat("mean Specificty\n"); round(mean(sp),2); round(mean(sp_low),2); round(mean(sp_high),2); 

#Testing
pred_test <- round((predict(rf_model,users_data,type="prob")[,2]),2)
saveRDS(rf_model,"rf_model_data_scientist.rds")
sink()

#Train A classifier for a language ####
#Create the dataset for training
users_data_language <- NULL
for (l in 5:1)
{
    language <- def_languages[l]
    users_data_language_tmp <- users_data[,c(1,grep(language,names(users_data)))]
    names(users_data_language_tmp) <- unlist(lapply(strsplit(names(users_data_language_tmp),paste0("_",language)),function(x) {x[[1]][1]}))
    names(users_data_language_tmp)[2] <- "language"
    users_data_language <- rbind.fill(users_data_language,users_data_language_tmp)
    rm(users_data_language_tmp)
}
users_data_language <- users_data_language[users_data_language$language!=0,] #remove missing languages
users_data_language$watchers <- NULL

#Add more featrues
users_data_language$PullRequestCommentEventRatio <- users_data_language$PullRequestReviewCommentEvent/users_data_language$PullRequestEvent
users_data_language$IssueCommentEventRatio <- users_data_language$IssueCommentEvent/users_data_language$IssuesEvent
users_data_language$PushEventperRepo <- users_data_language$PushEvent/users_data_language$repositories
users_data_language$CreateEventperRepo <- users_data_language$CreateEvent/users_data_language$repositories
users_data_language$WatchEventperRepo <- users_data_language$WatchEvent/users_data_language$repositories
users_data_language$starredperRepo <- users_data_language$starred/users_data_language$repositories
users_data_language$forksperRepo <- users_data_language$forks/users_data_language$repositories
users_data_language$starredperforks <- users_data_language$starred/users_data_language$forks
users_data_language$PushDeleteRatio <- users_data_language$PushEvent/users_data_language$DeleteEvent
users_data_language[is.na(users_data_language)] <- 0 # rest of values are just 0, not really missing
users_data_language[users_data_language==Inf] <- 0 # rest of values are just 0, not really missing

#Tag the users by rankings
row.names(users_data_language) <- c(1:nrow(users_data_language))
M <- ncol(users_data_language)
for (i in 3:M)
{
    users_data_language[,i+(M-2)] <- rank(-users_data_language[,i],ties.method = "first")
}

users_data_language$final_tag <- 0
M <- ncol(users_data_language)
N <- 10 #treshold to tag as '1' (example: top 10)
for (i in 30:(M-1))
{
    users_data_language$final_tag[users_data_language[,i]<=N] <- 1
    users_data_language$final_tag[users_data_language[,i]>=(nrow(users_data_language)-N*N)] <- -1
}
table(users_data_language$final_tag)
users_data_language <- users_data_language[,-c(30:56)]
original_dataset <- users_data_language
users_data_language <- users_data_language[users_data_language$final_tag!=0,] #training set
users_data_language$final_tag[users_data_language$final_tag==-1] <- 0

sink("language_rf_model.txt")

#Build the classifer
#Training
cutoff <- seq(0.01,0.99,0.01)
rf_train_acc <- rep(0,length(cutoff))
ntrees <- 500
rf_model_language <- randomForest(x = users_data_language[,-c(1,2,ncol(users_data_language))],
                                  y = factor(users_data_language$final_tag),
                                  ntree=ntrees,
                                  importance=T,
                                  do.trace=F,
                                  keep.forest=T)
#Select the optimal cutoff
for (i in 1:length(cutoff))
{
    
    stats <- confusionMatrix(as.numeric(rf_model_language$votes[,2]>cutoff[i]),users_data_language$final_tag)
    rf_train_acc[i] <- round(stats$overall["Kappa"],2)
}
cat("Training Statistics:\n")
plot(rf_train_acc,type="l",ylab="Kappa.",main="Kappa vs. Cutoff",xlab="Cutoff")
opt_cutoff <- cutoff[which.max(rf_train_acc)]
cat("Optimal Cutoff: ",opt_cutoff,"\n")
cat(table(users_data_language$final_tag)); cat("\n")
stats <- epi.tests(table(rf_model_language$votes[,2]>opt_cutoff,users_data_language$final_tag), conf.level = 0.95); cat("\n")
cat("Sensetivity:\n"); cat(stats$rval$se[1][[1]]); cat(stats$rval$se[2][[1]]); cat(stats$rval$se[3][[1]]); cat("\n")
cat("Specificty:\n"); cat(stats$rval$sp[1][[1]]); cat(stats$rval$sp[2][[1]]); cat(stats$rval$sp[3][[1]]); cat("\n")
cat("AUC:\n"); cat(round(ci.auc(users_data_language$final_tag,rf_model_language$votes[,2]),2)); cat("\n") #auc delong approx.
#rf_model_language$importance; varImpPlot(rf_model_language)

#Empty languages
results$prob_fifth[results$fifth_language=="0"] <- 0
results$prob_fourth[results$fourth_language=="0"] <- 0
results$prob_third[results$third_language=="0"] <- 0
results$prob_secondary[results$secondary_language=="0"] <- 0
results$prob_primary[results$primary_language=="0"] <- 0

#Validation
Times <- 5 #repetitions
K <- 3 #folds
folds <- createMultiFolds(users_data_language$final_tag,k = K,times = Times)
auc <- rep(NA,Times); auc_low <- rep(NA,Times); auc_high <- rep(NA,Times)
se <- rep(NA,Times); se_low <- rep(NA,Times); se_high <- rep(NA,Times)
sp <- rep(NA,Times); sp_low <- rep(NA,Times); sp_high <- rep(NA,Times)
for (i in 1:Times)
{
    pred <- NULL
    for (j in 1:K)
    {
        rf_model_valid <- randomForest(x = users_data_language[folds[[j+(i-1)*K]],-c(1:2,ncol(users_data_language))],
                                       y = factor(users_data_language$final_tag[folds[[j+(i-1)*K]]]),
                                       ntree=ntrees,
                                       importance=T,
                                       do.trace=F,
                                       keep.forest=T)
        pred <- c(pred,predict(rf_model_valid,users_data_language[-folds[[j+(i-1)*K]],-c(1:2,ncol(users_data_language))],type="prob")[,2])
        
    }
    pred <- pred[order(as.numeric(names(pred)))] #order again (dont do for LOO)
    stats <- epi.tests(table(as.numeric(pred>opt_cutoff),users_data_language$final_tag), conf.level = 0.95)
    se[i] <- stats$rval$se[1][[1]]; sp[i] <- stats$rval$sp[1][[1]]
    se_low[i] <- stats$rval$se[2][[1]]; sp_low[i] <- stats$rval$sp[2][[1]]
    se_high[i] <- stats$rval$se[3][[1]]; sp_high[i] <- stats$rval$sp[3][[1]]
    auc[i] <- ci.auc(users_data_language$final_tag,pred)[2] #delong
    auc_low[i] <- ci.auc(users_data_language$final_tag,pred)[1] #delong
    auc_high[i] <- ci.auc(users_data_language$final_tag,pred)[3] #delong
}
cat("Validation Statistics:\n")
cat("mean AUC\n"); cat(round(mean(auc),2)); cat(round(mean(auc_low),2)); cat(round(mean(auc_high),2)); cat("\n")
cat("mean Sensitivity\n"); cat(round(mean(se),2)); cat(round(mean(se_low),2)); cat(round(mean(se_high),2)); cat("\n")
cat("mean Specificty\n"); cat(round(mean(sp),2)); cat(round(mean(sp_low),2)); cat(round(mean(sp_high),2)); cat("\n")

saveRDS(rf_model_language,"rf_model_language.rds")

sink()
