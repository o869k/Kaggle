#Initialization
{
  sessionInfo() #system performance
  gc() #clear unsued memory
  rm(list=ls()) # clear workspace
  mainDir <- "C:/Users/user/Documents/R/otto"
  setwd(mainDir)
  set.seed(1234) #setting seed for comparison 
}

#Read Data
{
  train <- read.csv("train.csv")  
  test <- read.csv("test.csv")
  Submission <- read.csv("sampleSubmission.csv")
  M <- ncol(train) #Number of features per sample (columns)
  N <- nrow(train) #Number of features per sample (columns)
  L <- nlevels(train$target) #Number of features per sample (columns)
  Ntest <- nrow(test) #Number of features per sample (columns)
  classes_nums <- table(train$target)
}

#Scale events per sample - train
for (i in 1:N) {
  train[i,2:(M-1)] <- train[i,2:(M-1)]/sum(train[i,2:(M-1)])
}

#Save new trainset
{
  write.csv(train[,-1], file="trainNEW.csv")
}

#Scale events per sample - test
for (i in 1:Ntest) {
  test[i,2:(M-1)] <- test[i,2:(M-1)]/sum(test[i,2:(M-1)])
}

#Save new testset
{
  write.csv(test[,-1], file="testNEW.csv")
}

#Histograms of each feature - before transformations
for (i in seq(from=2,to=(M-1),by=4)) {
  par(mfrow=c(2,2))
  hist((train[train$target=="Class_1",i]),100,main = (i))
  hist((train[train$target=="Class_1",i+1]),100,main = (i+1))
  hist((train[train$target=="Class_1",i+2]),100,main = (i+2))
  hist((train[train$target=="Class_1",i+3]),100,main = (i+3))
  par(mfrow=c(1,1))
}

#Present the counts per example (scaled)
plot(2:(M-1),train[1,2:(M-1)],type="s")

#Present the counts of events per class (scaled)
{
 par(mfrow=c(3,3))
 plot(2:(M-1),colSums(train[train$target=="Class_1",2:(M-1)]),type="s",main = 1)
 plot(2:(M-1),colSums(train[train$target=="Class_2",2:(M-1)]),type="s",main = 2)
 plot(2:(M-1),colSums(train[train$target=="Class_3",2:(M-1)]),type="s",main = 3)
 plot(2:(M-1),colSums(train[train$target=="Class_4",2:(M-1)]),type="s",main = 4)
 plot(2:(M-1),colSums(train[train$target=="Class_5",2:(M-1)]),type="s",main = 5)
 plot(2:(M-1),colSums(train[train$target=="Class_6",2:(M-1)]),type="s",main = 6)
 plot(2:(M-1),colSums(train[train$target=="Class_7",2:(M-1)]),type="s",main = 7)
 plot(2:(M-1),colSums(train[train$target=="Class_8",2:(M-1)]),type="s",main = 8)
 plot(2:(M-1),colSums(train[train$target=="Class_9",2:(M-1)]),type="s",main = 9)
 par(mfrow=c(1,1))}

#Function for evaluation
Kappa_eval <- function(true.y, pred) {
  return(1 - classAgreement(table(pred, true.y))$kappa)
}
LogLoss <- function(true.y, pred, eps=0.00001) {
  
  pred <- pmin(pmax(pred, eps), 1-eps)
  
  -sum(true.y*log(pred))
}

#Set number of lines to train
{
  K <- round(N*0.2) #number of simulaton lines
  #lines <- sample(x = N,size = K,replace = F) #randomize lines
  lines <- 1:K #direct lines
}

#Search features according to Mutual Information Criteria
{
  weights_mutual_info <- information.gain(target~.,data=train[lines,2:M])
  print(sort(weights_mutual_info$attr_importance))
  subset_mutual_info <- NULL
  subset_mutual_info <- row.names(weights_mutual_info)[which(weights_mutual_info$attr_importance > 0.1)]        
  features_MI <- as.simple.formula(subset_mutual_info, "target")
}
