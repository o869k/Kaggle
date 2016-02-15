#Initialization
{
  sessionInfo() #system performance
  gc() #clear unsued memory
  rm(list=ls()) # clear workspace
  mainDir <- "C:/Users/user/Documents/R/otto"
  setwd(mainDir)
  set.seed(1234) #setting seed for comparison
  library(e1071); library(FSelector);
  library(foreach); library(doParallel);
  library(ggplot2); library(h2o);  
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

#Scale events per sample
for (i in 1:N) {
  train[i,2:(M-1)] <- train[i,2:(M-1)]/sum(train[i,2:(M-1)])
}

#Save new trainset
{
  write.csv(train, file="trainNEW.csv")
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

## Start a local cluster & load model
{
  #h2o.shutdown(localH2O)
  localH2O <- h2o.init(nthreads = -1,ip = "localhost", port = 54321, startH2O = TRUE) #start h2o connection
  #model <- h2o.loadModel(localH2O, "./mymodel")
}

## Import Data to H2O Cluster
train_hex <- as.h2o(localH2O, train, key = 'train') #make train set h2o object 

## Split the dataset into 80:20 for training and validation
train_hex_split <- h2o.splitFrame(train_hex, ratios = 0.8, shuffle = T)
y_train <- as.factor(as.matrix(train_hex_split[[1]]$target))
y_valid <- as.factor(as.matrix(train_hex_split[[2]]$target))

## Create a set of netwok topologies
hidden_layers <- list(c(100,100))
epochs_layers <- c(1)
l1_regularizations <- c(1e-5)
input_dropout_ratios_list <- c(0.2)
hidden_dropout_ratios_list <- list(c(0.25,0.25))

## Train a Deep Neural Network
#h2o.rm(localH2O, keys = c("train.hex"))
#rm(model,train_hex)
model <- h2o.deeplearning(x = 2:94,
                          y = 95,
                          data = train_hex_split[[1]],
                          validation = train_hex_split[[2]],
                          activation = "RectifierWithDropout",
                          input_dropout_ratio = input_dropout_ratios_list, 
                          hidden_dropout_ratios = hidden_dropout_ratios_list,
                          hidden = hidden_layers,
                          l1 = l1_regularizations,
                          epochs = epochs_layers*50,
                          classification = T,
                          balance_classes = T,
                          override_with_best_model = T,
                          max_confusion_matrix_size = 9,
                          variable_importances = T)

## Print & save the Model Summary
print(model)
model@model$confusion
h2o.saveModel(object = model,dir = "./")}

## Use the model for prediction and store the results in submission template
result_train <- as.data.frame(h2o.predict(model, train_hex_split[[1]]))
result_valid <- as.data.frame(h2o.predict(model, train_hex_split[[2]]))
 