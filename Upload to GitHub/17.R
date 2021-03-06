#### Initialization ####
sessionInfo() #system performance
gc() #clear unsued memory
rm(list=ls()) # clear workspace
Sys.setlocale("LC_TIME", "English")
Sys.setenv(TZ='GMT')
set.seed(1234) #setting seed for comparison
op <- options()
par(mfrow=c(1,1)); plotter <- 1

#Packages
library(xgboost)
library(doParallel)
library(e1071)
library(caret)
library(ggplot2)
library(data.table)
library(sqldf)

#functions
hypo_analysis <- function(pat) {
    barplot(table(dataset_old$Hypo[dataset_old$Patient_ID==pat],dataset_old$Hours[dataset_old$Patient_ID==pat]),
            col=c("darkblue","red"),
            beside=TRUE,
            xlab="Hour at night",
            main=paste0("Hypo count by hour in all nights, Patient ",pat),
            legend = c("No","Yes"))
    barplot(table(dataset_old$Hypo[dataset_old$Patient_ID==pat],dataset_old$Hours_Normelized[dataset_old$Patient_ID==pat]),
            col=c("darkblue","red"),
            beside=TRUE,
            xlab="Hour at night",
            main=paste0("Hypo count by normalized hour in all nights, Patient ",pat),
            legend = c("No","Yes"))
    xyplot(CGM~Timestamp_Normelized|ID,
           groups = Hypo,
           main=paste0("CGM vs. Normelized Time by Nights, Patient ",pat),
           xlab="CGM",
           ylab = unique(dataset_old$Night_ID[dataset_old$Patient_ID==pat]),
           layout=c(2,5),
           plot.points=FALSE,
           cex  = 0.1,
           type="l",
           data = dataset_old[dataset_old$Patient_ID==pat,])
} #hypo analysis in figures
range01 <- function(x){(x-min(x))/(max(x)-min(x))} #standerize reuslts to [0..1]
evalFPR <- function(preds, dtrain) {
    labels <- getinfo(dtrain, "label")
    preds <- as.integer(round(range01(preds)))
    preds <- preds[labels==0]
    labels <- labels[labels==0]
    fpr <- as.numeric(sum(labels != preds))/length(labels)
    return(list(metric = "FPR", value = fpr))
} #function to optimize the xgboost
evalFNR <- function(preds, dtrain) {
    labels <- getinfo(dtrain, "label")
    preds <- as.integer(round(range01(preds)))
    preds <- preds[labels==1]
    labels <- labels[labels==1]
    fnr <- as.numeric(sum(labels != preds))/length(labels)
    return(list(metric = "FNR", value = fnr)) }
evalerror <- function(preds, dtrain) {
    labels <- getinfo(dtrain, "label")
    preds <- as.integer(round(range01(preds)))
    err <- as.numeric(sum(labels != (preds > 0)))/length(labels)
    return(list(metric = "error", value = err))
}
evalKappa <- function(preds, dtrain) {
    labels <- getinfo(dtrain, "label")
    preds <- as.integer(round(range01(preds)))
    kappa <- classAgreement(table(preds,labels))$kappa
    return(list(metric = "Kappa", value = kappa)) }
filter_spikes <- function(out,k) {
    start_idx <- 1
    i <- 1
    while (i<length(out))
    {
        if (out[i]==1)
        {
            start_idx <- i;
            end_idx <- i;
            for (j in (start_idx+1):length(out))
            {
                if (out[j]==0)
                {
                    break
                }
            }
            end_idx <- j
            if ((end_idx - start_idx)<k)
            {
                out[start_idx:end_idx] <- 0;
            }
            i <- end_idx
        } else {
            i <- i+1
        }
    }
    return(out)
}
evalAccuracy <- function(preds, dtrain) {
    ID_list <- getinfo(dtrain, "base_margin")
    Timestamp_list <- getinfo(dtrain, "weight")
    K <- length(unique(ID_list))
    night_success <- rep(1,K)
    labels_tmp <- getinfo(dtrain, "label")
    preds <- as.integer(preds>th)
    preds <- filter_spikes(preds,spike_filtering) #filter hypo detection
    for (k in 1:K)
    {
        night_tmp <- unique(ID_list)[k]
        labels_night <- labels_tmp[ID_list==night_tmp]
        preds_night <- preds[ID_list==night_tmp]
        Timestamp_night <- Timestamp_list[ID_list==night_tmp]
        
        time_first_real_hypo <- Timestamp_night[head(which(labels_night==max(labels_night)),1)] 
        time_first_predicted_hypo <- Timestamp_night[head(which(preds_night==max(preds_night)),1)]
        time_last_real_hypo <- Timestamp_night[tail(which(labels_night==max(labels_night)),1)] 
        if ((time_first_real_hypo-time_first_predicted_hypo) > 0.5 | (time_first_predicted_hypo-time_last_real_hypo) > 0.25 | max(preds_night)==0)
        {
            night_success[k] <- 0 #error
        }
    }
    acc <- sum(night_success)/K
    return(list(metric = "Accuracy", value = acc)) 
}

#### Load data set , if ready ####
#read dataset
selected_nights <- read.csv("reported_hypos.csv",header = F,stringsAsFactors = F)
dataset_new <- read.csv("data_22_04_16_basic.csv",stringsAsFactors = F) #also includes the latest tagging
dataset_new$Patient_ID <- as.character(dataset_new$Patient_ID)
dataset_original <- fread("C:\\Gili\\DataSet8\\data_28_03_16_extended_new.csv", sep = ",", header= TRUE)
dataset_original <- dataset_original[!(dataset_original$Patient_ID=="014"),]
dataset_original_only_14 <- fread("C:\\Gili\\DataSet8\\data_28_03_16_extended_pat14_only.csv", sep = ",", header= TRUE)
dataset_original <- rbind(dataset_original,dataset_original_only_14)
rm(dataset_original_only_14)

dataset_new <- dataset_new[,c("Timestamp","Patient_ID","Hypo")] #only keep unique identifires, all the rest of features comes from the main table

dataset <- subset(dataset_original,Timestamp %in% dataset_new$Timestamp)
dataset <- dataset[!c(dataset$Patient_ID=="009" | dataset$Patient_ID=="001"),]
dataset <- as.data.frame(dataset)
rm(dataset_original)

dataset$ID <- factor(dataset$ID)
dataset$Timestamp <- as.character(dataset$Timestamp)
dataset$Hours <- factor(dataset$Hours,levels = c(0:23))
dataset$Hours_Normelized <- factor(dataset$Hours_Normelized,levels=c(0:13))
dataset$Patient_ID <- as.integer(dataset$Patient_ID)
dataset$Patient_ID <- as.character(dataset$Patient_ID)
dataset$Hypo <- NULL #THIS IS THE OLD , BAD HYPO TAG!

dataset <- merge(dataset,dataset_new)
rm(dataset_new)

write.csv(dataset,"dataset_22_04.csv",row.names = F) #save the datset
validset <- dataset

nights_hypo <- unique(dataset$ID[dataset$Hypo==1]) #nights with hypo
dataset <- dataset[dataset$ID %in% nights_hypo,] #suppose to be all (1 night was remveod:)

dataset_old <- dataset
dataset <- dataset[,c(2:12,15,19:25,27:165)]  
labels <- dataset$Hypo  # save the labels asaide
dataset$Hypo <- NULL
dataset$ID <- as.numeric(as.character(dataset$ID))

validset_old <- validset
validset <- validset[,c(2:12,15,19:25,27:165)] 
labels_valid <- validset$Hypo  # save the labels asaide
validset$Hypo <- NULL
validset$ID <- as.numeric(as.character(validset$ID))

#### Train XGBOOST model for all patients ####
cat("training a XGBoost classifier\n")
#We do it for each patient
#LOO Validaiton
N <- length(unique(validset$ID))
train_acc <- rep(NA,N)
valid_acc <- rep(NA,N)
train_auc <- rep(NA,N)
valid_auc <- rep(NA,N)
train_fpr <- rep(NA,N)
valid_fpr <- rep(NA,N)
train_fnr <- rep(NA,N)
valid_fnr <- rep(NA,N)

#Parameters
param <- list(objective="binary:logistic", eta=0.05,nthread = 12, min_child_weight=1,scale_pos_weight=10,
              max_depth=4,eval_metric = "logloss",colsample_bytree = 1,subsample = 1)
num_rounds <- 3000
spike_filtering <- 10 #filter hypo detection of less than 5 minutes (10 samples)
th <- 0.6 #threshold
night_success <- rep(1,N)
pred_valid_tot <- NULL
mod_Ind <- 0 #sum for the mean steps

#validation is LOO
pdf(paste0("Validation_patient_new.pdf"))
#par(mfrow=c(2,2))
for (n in 1:N)
{
    night <- unique(validset$ID)[n]
    pat <- unique(validset_old$Patient_ID[validset$ID==night])
    cat("Night: ",as.character(night),"\n")
    
    features_take <-c(3:ncol(dataset)) #best_features #c(2:ncol(dataset))
    
    #not a model per patient
    dtrain <- xgb.DMatrix(data.matrix(dataset[!(dataset$ID==night),features_take]),label = labels[!dataset$ID==night],missing=NA) #all nights
    if (all(labels_valid[validset$ID==night]==0)) 
    {
        dtest <- xgb.DMatrix(data.matrix(validset[validset$ID==night,features_take]),label = labels_valid[validset$ID==night],missing=NA) #validation night w/o hypo event
    } else {
        last_obs <- tail(which(labels_valid[validset$ID==night]==1),1)+100 #validation uop until the hypo event
        last_obs <- min(last_obs,length(labels_valid[validset$ID==night]==0))
        dtest <- xgb.DMatrix(data.matrix(validset[validset$ID==night,features_take][1:last_obs,]),label = labels_valid[validset$ID==night][1:last_obs],missing=NA)
    }
    watchlist <- list("val"=dtest,"train"=dtrain)
    
    mod <- NULL
    if (all(labels_valid[validset$ID==night]==0)) 
    {
        mod <- xgb.train(params = param, data = dtrain, nrounds = num_rounds, watchlist = watchlist,print.every.n = 100,maximize = F,early.stop.round = 100)
        #th <- 0.4 #threshold
    } else {
        mod <- xgb.train(params = param, data = dtrain, nrounds = num_rounds, watchlist = watchlist,print.every.n = 100,maximize = F,early.stop.round = 100) 
        #th <- 0.3 #threshold
    }
    mod_Ind <- mod_Ind+mod$bestInd
    
    train_results <- predict(mod,dtrain)
    train_results <- as.integer(train_results>th)
    train_results <- filter_spikes (train_results,spike_filtering) #filter hypo detection
    train_confusion <- confusionMatrix(train_results,labels[!dataset$ID==night]) 
    train_fpr[n] <- round(1-train_confusion$byClass["Specificity"],3)
    train_fnr[n] <- round(1-train_confusion$byClass["Sensitivity"],3)
    
    valid_results <- predict(mod,dtest) #in prediction we standerize the results and set 0.5 as cutoff
    valid_results <- valid_results - min(valid_results,na.rm = T)
    valid_results[c(1:min(tail(which(validset_old$Timestamp_Normelized[validset$ID==night]<1),1),length(valid_results)))] <- 0 #first hour of night is always 0 for hypo prob!
    valid_results <- filter(valid_results,rep(1/10,10),method = "convolution",sides = 1) #filter noise from probability
    valid_results[is.na(valid_results)] <- 0 #if NA exist
    valid_results[valid_results<0] <- 0 #if negative prob exist
    
    #normalizatio in case of hypo night (range01 which is obviously not Kosher)
    if (!all(labels_valid[validset$ID==night]==0) & !all(valid_results==0))  {valid_results <- range01(valid_results)}
        
    valid_results_th <- as.integer(valid_results>th) #in prediction we standerize the results and set 0.5 as cutoff
    valid_results_th <- filter_spikes(valid_results_th,spike_filtering) #filter hypo detectio
    #if(valid_results_th[1]==1) {valid_results_th[c(1:(which(duplicated(valid_results_th)==FALSE)[2]-1))] <- 0} #if starts with 1 - make it 0!
    if(all(labels_valid[validset$ID==night]==0) & (!all(valid_results_th==0))) #2nd type error
    {
        night_success[n] <- 0 #error
    }
    
    valid_confusion <- confusionMatrix(factor(valid_results_th,levels=c("0","1")),factor(labels_valid[validset$ID==night][1:last_obs],levels=c("0","1")))
    valid_fpr[n] <- round(1-valid_confusion$byClass["Specificity"],3)
    valid_fnr[n] <- round(1-valid_confusion$byClass["Sensitivity"],3)
    
    pred_valid_tot <- c(pred_valid_tot,valid_results)
    
    #calcualte error in classifcaiton for this night according to our score (half an hour)
    time_first_real_hypo <- NULL; time_last_real_hypo<- NULL
    time_first_predicted_hypo <- NULL; time_last_predicted_hypo <- NULL
    time_first_real_hypo <- validset_old$Timestamp_Normelized[validset$ID==night][head(which(labels_valid[validset$ID==night]==max(labels_valid[validset$ID==night])),1)] 
    time_first_predicted_hypo <- validset_old$Timestamp_Normelized[validset$ID==night][head(which(valid_results_th==max(valid_results_th)),1)]
    time_last_real_hypo <- validset_old$Timestamp_Normelized[validset$ID==night][tail(which(labels_valid[validset$ID==night]==max(labels_valid[validset$ID==night])),1)] 
    time_last_predicted_hypo <- validset_old$Timestamp_Normelized[validset$ID==night][tail(which(valid_results_th==max(valid_results_th)),1)]
    if(((time_first_real_hypo-time_first_predicted_hypo) > 0.5 | (time_first_predicted_hypo-time_last_real_hypo) > 0.25 | max(valid_results_th)==0) & !all(labels_valid[validset$ID==night]==0))
    {
        night_success[n] <- 0 #error
    }

    #plot validation (without range01)
    par(mar=c(5, 4, 4, 6) + 0.1)
    plot(validset_old$Timestamp_Normelized[validset$ID==night],labels_valid[validset$ID==night],
         main=paste0("Pat: ",unique(validset_old$Patient_ID[validset$ID==night])," Night: ",tail(strsplit(validset_old$Timestamp[validset$ID==night]," "),1)[[1]][1]," Success: ",night_success[n]),
         type="l",col="blue",xlab="Hour at Night",ylab="Hypo Prob",ylim=c(0,1),pch=19)
    lines(validset_old$Timestamp_Normelized[validset$ID==night],c(valid_results,rep(0,length(labels_valid[validset$ID==night])-length(valid_results))),col="red",cex=0.5)
    par(new=TRUE)
    plot(validset_old$Timestamp_Normelized[validset$ID==night],validset_old$CGM[validset$ID==night],lty=2,lwd=0.5,type="l",col="black",xlab="",ylab="",axes=FALSE)
    axis(4,col.axis="black",las=1); mtext(side = 4, line = 3, 'CGM')
    legend("topleft",c("Obsreved","Predicted","CGM"),pch=1,col=c("blue","red","black"),cex=0.5,bg = "white")
    
    #plot validation (with threshold)
    par(mar=c(5, 4, 4, 6) + 0.1)
    plot(validset_old$Timestamp_Normelized[validset$ID==night],labels_valid[validset$ID==night],
         main=paste0("Pat: ",unique(validset_old$Patient_ID[validset$ID==night])," Night: ",tail(strsplit(validset_old$Timestamp[validset$ID==night]," "),1)[[1]][1]," Success: ",night_success[n]),
         type="l",col="blue",xlab="Hour at Night",ylab="Hypo Yes/No",ylim=c(0,1),pch=19,
         sub=paste0("FNR: ",valid_fpr[n],", FPR: ",valid_fnr[n]))
    lines(validset_old$Timestamp_Normelized[validset$ID==night],c(valid_results_th,rep(0,length(labels_valid[validset$ID==night])-length(valid_results_th))),col="red",cex=0.5)
    par(new=TRUE)
    plot(validset_old$Timestamp_Normelized[validset$ID==night],validset_old$CGM[validset$ID==night],lty=2,lwd=0.5,type="l",col="black",xlab="",ylab="",axes=FALSE)
    axis(4,col.axis="black",las=1); mtext(side = 4, line = 3, 'CGM')
    legend("topleft",c("Obsreved","Predicted","CGM"),pch=1,col=c("blue","red","black"),cex=0.5,bg = "white")
}
#Final validation resutls
# valid_results <- pred_valid_tot #in prediction we standerize the results and set 0.5 as cutoff
# valid_confusion <- confusionMatrix(factor(as.numeric(valid_results>th),levels=c("0","1")),factor(labels,levels=c("0","1")))
# valid_fpr_final <- round(1-valid_confusion$byClass["Specificity"],3)
# valid_fnr_final <- round(1-valid_confusion$byClass["Sensitivity"],3)
sum(night_success)/N #final prediction accuracy
dev.off()

#final model
dtrain <- xgb.DMatrix(data.matrix(dataset[,features_take]),label = labels,missing=NA)
watchlist <- list("train"=dtrain)
mod <- xgb.train(params = param, data = dtrain, nrounds = 300, watchlist = watchlist,print.every.n = 100,maximize = F)
train_results_all <- predict(mod,dtrain)
train_results_all <- as.integer(predict(mod,dtrain)>th)
train_results_all <- filter_spikes(train_results_all,spike_filtering) #filter hypo detection
train_confusion <- confusionMatrix(factor(train_results_all,levels=c("0","1")),factor(labels,levels=c("0","1")))
train_fpr_final <- round(1-train_confusion$byClass["Specificity"],3)
train_fnr_final <- round(1-train_confusion$byClass["Sensitivity"],3)
saveRDS(mod,"xgboost_model_22_04.RDS") #save model

N <- length(unique(dataset_old$ID[dataset_old$Patient_ID==pat]))
night_success_train <- rep(1,N)
pdf(paste0("training_all_patients.pdf"))
par(mfrow=c(2,2))
for (n in 1:N)
{
    night <- unique(dataset$ID)[n]
    cat("Night: ",as.character(night),"\n")
    
    train_results <- train_results_all[dataset$ID==night]
    #train_confusion <- confusionMatrix(train_results,labels[dataset$ID==night]) 
    #train_fpr[n] <- round(1-train_confusion$byClass["Specificity"],3)
    #train_fnr[n] <- round(1-train_confusion$byClass["Sensitivity"],3)
    
    #calcualte error in classifcaiton for this night according to our score (half an hour)
    time_first_real_hypo <- NULL; time_last_real_hypo<- NULL
    time_first_predicted_hypo <- NULL; time_last_predicted_hypo <- NULL
    time_first_real_hypo <- dataset$Timestamp_Normelized[dataset$ID==night][head(which(labels[dataset$ID==night]==max(labels[dataset$ID==night])),1)] 
    time_first_predicted_hypo <- dataset$Timestamp_Normelized[dataset$ID==night][head(which(train_results==max(train_results)),1)]
    time_last_real_hypo <- dataset$Timestamp_Normelized[dataset$ID==night][tail(which(labels[dataset$ID==night]==max(labels[dataset$ID==night])),1)] 
    time_last_predicted_hypo <- dataset$Timestamp_Normelized[dataset$ID==night][tail(which(train_results==max(train_results)),1)]
    if ((time_first_real_hypo-time_first_predicted_hypo) > 0.5 | (time_first_predicted_hypo-time_last_real_hypo) > 0.25 | max(train_results)==0)
    {
        night_success_train[n] <- 0 #error
    }
    
    #plot training
    plot(dataset$Timestamp_Normelized[dataset$ID==night],labels[dataset$ID==night],
         main=paste0("Pat: ",unique(dataset$Patient_ID[dataset$ID==night])," Night: ",tail(strsplit(dataset$Timestamp[dataset$ID==night]," "),1)[[1]][1]," Success: ",night_success_train[n]),
         type="l",col="blue",xlab="Hour at Night",ylab="Hypo Prob",ylim=c(0,1),pch=19)
    lines(dataset$Timestamp_Normelized[dataset$ID==night],train_results,col="red",cex=0.5)
    legend("topleft",c("Obsreved","Predicted"),pch=1,col=c("blue","red"),cex=0.5)
    
}
sum(night_success_train)/N #final prediction accuracy
dev.off()

#Featreus Importance
# importance_matrix <- xgb.importance(names(dataset)[-1], model = mod)
# xgb.plot.importance(importance_matrix)
# best_features <- sort(intersect(intersect(head(importance_matrix$Feature,20),head(importance_matrix$Feature[order(importance_matrix$Cover,decreasing = T)],20)),head(importance_matrix$Feature[order(importance_matrix$Frequence,decreasing = T)],20)))
