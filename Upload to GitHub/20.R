#Initialization
{
  sessionInfo() #system performance
  gc() #clear unsued memory
  rm(list=ls()) # clear workspace
  setwd(mainDir)
  Sys.info()
  set.seed(1234) #setting seed for comparison
  library("RPostgreSQL"); library("rJava")
  library("caret"); library("RJDBC")
  library("randomForest"); library("e1071")
  library("matrixStats");
  options(java.parameters = "- Xmx8g")
  options(digits=12) 
  op <- options()
}

#Input arguments
{
args=(commandArgs(TRUE))
if(length(args)==0){
  print("No arguments supplied.")
  ##supply default values
  th <-  80
}else{
  for(i in 1:length(args)){
    eval(parse(text=args[[i]]))
  }
}
}

#Using RJDBC with AWS RS optimized driver (using SQL queries)
{
# connect to Amazon Redshift
driver <- JDBC("com.amazon.redshift.jdbc41.Driver", "../../../RedshiftJDBC41-1.1.9.1009.jar", identifier.quote="`")
conn <- dbConnect(driver, url)
}

#Read data , if ready
{
  trainset_original <- read.csv("../../cdp/monthly/churn_07_01/trainset_new.csv")

  trainset_original$month_date <- as.Date(as.character(trainset_original$month_date))
  trainset_original$end_customer <- as.character(trainset_original$end_customer)
  trainset_original$label <- factor(trainset_original$label)
  
  trainset_original <- trainset_original[order(trainset_original$end_customer,trainset_original$n_cdp_id,trainset_original$month_date),] #sort it
  label <- trainset_original$label
}

##### Read Data from RS to R and build datasets ####
r_acc_end_customer_month_flat <- dbGetQuery(conn, "select * from r_acc_end_customer_month_flat") #should take few seconds to read
r_acc_end_customer_month_flat <- r_acc_end_customer_month_flat[r_acc_end_customer_month_flat$month_date!="2016-01-01",] #we don't have label for that!
r_acc_end_customer_month_flat$month_date <- as.Date(r_acc_end_customer_month_flat$month_date)
r_acc_end_customer_month_flat$dominant_state <- factor(r_acc_end_customer_month_flat$dominant_state)

unwanted_columns <- c("test_ready","activation_ready","deactivated","retired","inventory",
  "test_ready_nr","activation_ready_nr","activated_nr","deactivated_nr","retired_nr","inventory_nr")

r_acc_end_customer_month_flat <- r_acc_end_customer_month_flat[,!(names(r_acc_end_customer_month_flat) %in% unwanted_columns)]

column_9m <- names(r_acc_end_customer_month_flat)[grep("9m",names(r_acc_end_customer_month_flat))] #remove the 9 months changes columns
#r_acc_end_customer_month_flat <- r_acc_end_customer_month_flat[,!(names(r_acc_end_customer_month_flat) %in% column_9m)]
column_12m <- names(r_acc_end_customer_month_flat)[grep("12m",names(r_acc_end_customer_month_flat))] #remove the 12 months changes columns
r_acc_end_customer_month_flat <- r_acc_end_customer_month_flat[,!(names(r_acc_end_customer_month_flat) %in% column_12m)]
column_data <- names(r_acc_end_customer_month_flat)[grep("data",names(r_acc_end_customer_month_flat))] #remove the data columns
r_acc_end_customer_month_flat <- r_acc_end_customer_month_flat[,!(names(r_acc_end_customer_month_flat) %in% column_data)]
column_voice <- names(r_acc_end_customer_month_flat)[grep("voice",names(r_acc_end_customer_month_flat))] #remove the voice columns
r_acc_end_customer_month_flat <- r_acc_end_customer_month_flat[,!(names(r_acc_end_customer_month_flat) %in% column_voice)]
column_sms <- names(r_acc_end_customer_month_flat)[grep("sms",names(r_acc_end_customer_month_flat))] #remove the SMS columns
r_acc_end_customer_month_flat <- r_acc_end_customer_month_flat[,!(names(r_acc_end_customer_month_flat) %in% column_sms)]
column_age_75 <- names(r_acc_end_customer_month_flat)[grep("age_75",names(r_acc_end_customer_month_flat))] #remove the age_75 columns
#r_acc_end_customer_month_flat <- r_acc_end_customer_month_flat[,!(names(r_acc_end_customer_month_flat) %in% column_age_75)]
column_age_25 <- names(r_acc_end_customer_month_flat)[grep("age_25",names(r_acc_end_customer_month_flat))] #remove the age_25 columns
#r_acc_end_customer_month_flat <- r_acc_end_customer_month_flat[,!(names(r_acc_end_customer_month_flat) %in% column_age_25)]
column_age_max <- names(r_acc_end_customer_month_flat)[grep("age_max",names(r_acc_end_customer_month_flat))] #remove the age_max columns
r_acc_end_customer_month_flat <- r_acc_end_customer_month_flat[,!(names(r_acc_end_customer_month_flat) %in% column_age_max)]
column_age_min <- names(r_acc_end_customer_month_flat)[grep("age_min",names(r_acc_end_customer_month_flat))] #remove the age_min columns
r_acc_end_customer_month_flat <- r_acc_end_customer_month_flat[,!(names(r_acc_end_customer_month_flat) %in% column_age_min)]

r_acc_end_customer_month_flat[is.na(r_acc_end_customer_month_flat)] <- 0 #all other NA are correnly 0

end_customers_labeled <- c("Dunbar Armored","Metro PCS","Dunbar Armored Inc - Norcross","inOne Technology","Trakpro - 98145964263",
  "Air Solution","Glacier Water","AIr Valet","Excel Tire Gauge Inc.","Autobank Financial Service Controlit","Quality Acceptance Controlit",
  "Cellutrak","Nouria Energy","Selecta","Hupo","Canteen Vending by Pittman Inc","Smart Source Vending","2 Amigos Auto Sales",
  "Coin Acceptors - Mexico","Hressing","Evend","Nayax Romania","Versalles Auto Sales")

trainset <- r_acc_end_customer_month_flat[which(r_acc_end_customer_month_flat$end_customer %in% end_customers_labeled),]
trainset <- trainset[order(trainset$end_customer,trainset$o_ent_account_id,trainset$month_date),] #sort it
testset <- r_acc_end_customer_month_flat[-c(which(r_acc_end_customer_month_flat$end_customer %in% end_customers_labeled)),]
testset <- testset[order(testset$end_customer,testset$o_ent_account_id,testset$month_date),] #sort it

#Add more labels manualy of examples who got tested wrong eventually:
# new_data_rows <- which(testset$end_customer=="Trakpro - 98145964263")
# trainset <- rbind(trainset,cbind(testset[new_data_rows,],label=rep(0,length(new_data_rows))))
# trainset <- trainset[order(trainset$end_customer,trainset$o_ent_account_id,trainset$month_date),] #sort it again
# testset <- testset[-new_data_rows,]

rm(r_acc_end_customer_month_flat,trainset_original,column_12m,column_9m,unwanted_columns,column_data,end_customers_labeled,
  column_voice,column_sms,column_age_75,column_age_25,column_age_max,column_age_min)

#Remove the first X rows from each end-customer, they are biased, as there is missing information (repeat duplicate for X times)
for (i in 1:6)
{
  rows_rm <- which(!duplicated(trainset[,c(1,2)])) #find first row of each customer in the trainset
  trainset <- trainset[-rows_rm,]
  rows_rm <- which(!duplicated(testset[,c(1,2)])) #find first row of each customer in the testset
  testset <- testset[-rows_rm,]
}
# column_6m <- names(trainset)[grep("6m",names(trainset))] #remove the 6 months changes columns
# trainset <- trainset[,!(names(trainset) %in% column_6m)]
# column_6m <- names(testset)[grep("6m",names(testset))] #remove the 6 months changes columns
# testset <- testset[,!(names(testset) %in% column_6m)]

trainset$label <- label

#If 9 or 12 months, use it also
for (i in 1:3)
{
  rows_rm <- which(!duplicated(trainset[,c(1,2)])) #find first row of each customer in the trainset
  trainset <- trainset[-rows_rm,]
  rows_rm <- which(!duplicated(testset[,c(1,2)])) #find first row of each customer in the testset
  testset <- testset[-rows_rm,]
}

#### Train & Test RF on data ####
K <- 3; t <- 2;
folds <- createMultiFolds(trainset$label,k = K,times = t) #Create train, validation
ntrees <- 50

#Initialize a scoring matrix
train_acc <- rep(NA,K); train_fpr <- rep(NA,K); train_fnr <- rep(NA,K); train_kappa <- rep(NA,K)
valid_acc <- rep(NA,K); valid_fpr <- rep(NA,K); valid_fnr <- rep(NA,K); valid_kappa <- rep(NA,K)

for (k in 1:(K*t))
{
  rf_valid <- randomForest(x=trainset[folds[[k]],-c(1:5,ncol(trainset))],
    y=trainset$label[folds[[k]]],
    xtest=trainset[-folds[[k]],-c(1:5,ncol(trainset))],
    ytest=trainset$label[-folds[[k]]],
    ntree=ntrees,
    do.trace=FALSE,
    keep.forest=TRUE)
  train_acc[k] <- classAgreement(rf_valid$confusion)$diag
  train_kappa[k] <- classAgreement(rf_valid$confusion)$kappa
  train_fpr[k] <- 1-rf_valid$confusion[4]/(rf_valid$confusion[3]+rf_valid$confusion[4])
  train_fnr[k] <- 1-rf_valid$confusion[1]/(rf_valid$confusion[2]+rf_valid$confusion[1])
  valid_acc[k] <- classAgreement(rf_valid$test$confusion)$diag
  valid_kappa[k] <- classAgreement(rf_valid$test$confusion)$kappa
  valid_fpr[k] <- 1-rf_valid$test$confusion[4]/(rf_valid$test$confusion[3]+rf_valid$test$confusion[4])
  valid_fnr[k] <- 1-rf_valid$test$confusion[1]/(rf_valid$test$confusion[2]+rf_valid$test$confusion[1])
}
scores <- as.data.frame.matrix(rbind(train_acc,train_kappa,train_fpr,train_fnr,valid_acc,valid_kappa,valid_fpr,valid_fnr))
scores$Mean_folds <- rowMeans(scores)
scores$Sd_folds <- rowSds(as.matrix(scores[,-ncol(scores)]))

rf <- randomForest(x=trainset[,-c(1:5,ncol(trainset))],
  y=trainset$label,
  ntree=ntrees,
  do.trace=FALSE,
  keep.forest=TRUE,
  importance=TRUE)
scores$Final <- c(classAgreement(rf$confusion)$diag,classAgreement(rf$confusion)$kappa,1-rf$confusion[4]/(rf$confusion[3]+rf$confusion[4]),1-rf$confusion[1]/(rf$confusion[2]+rf$confusion[1]),rep(NA,4))
#varImpPlot(rf)

results <- trainset[,c(1:5,ncol(trainset))]
results$predicted_label <- predict(rf,trainset[,-c(1:5,ncol(trainset))])
results$probability_to_churn_month <- predict(rf,trainset[,-c(1:5,ncol(trainset))],"prob")[,2]*100
results$predicted_label <- as.numeric(results$probability_to_churn_month>=th) #thershold is 80%

results_test <- testset[,c(1:5)]
results_test$predicted_label <- predict(rf,testset[,-c(1:5)])
results_test$probability_to_churn_month <- predict(rf,testset[,-c(1:5)],"prob")[,2]*100
results_test$predicted_label <- as.numeric(results_test$probability_to_churn_month>=th) #thershold is 80%

#### Script Ending ####
# write.csv(trainset,"trainset.csv",row.names = FALSE)
# write.csv(testset,"testset.csv",row.names = FALSE)
saveRDS(rf,"churn_model_account_monthly.RDS")
# write.csv(results,"results_train_account_monthly.csv",row.names = FALSE)
# write.csv(results_test,"results_test_account_monthly.csv",row.names = FALSE)
# write.csv(scores,"scores_train_account_monthly.csv",row.names = FALSE)
dbDisconnect(conn)
q(save="no")
