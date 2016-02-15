###PREPROCESS##
{
  #Initialization
  {
    sessionInfo() #system performance
    gc() #clear unsued memory
    rm(list=ls()) # clear workspace
    setwd("C:/Users/user/Documents/R/digits")
    set.seed(1234) #setting seed for comparison
    library(date); library(fpp)
    library(ff); library(ggplot2)
    library(caret); library(kernlab)
    library(e1071); library(UsingR)
    library(deepnet); library(FNN)
    library(h2o);
  }
  
  #Read Data 
  {
    trainSet <- read.csv("train.csv")  
    N <- nrow(trainSet)  #Number of train samples
    testSet <- read.csv("test.csv")  
    Nt <- nrow(testSet) #Number of test samples
    M <- ncol(trainSet)-1 #Number of pixles per sample
  }
  
  #Displaying a number
  {
    pic_col <- as.numeric(trainSet[4,1:M])
    pic <- matrix(rev(pic_col),nrow=sqrt(M))
    image(pic,col=gray((0:255/255))) #show the image
  }
  
  #Start a local cluster & load model
  {
    #h2o.shutdown(localH2O)
    localH2O <- h2o.init(nthreads = -1,ip = "localhost", port = 54321, startH2O = TRUE) #start h2o connection
    #model <- h2o.loadModel(localH2O, "./mymodel")
  }
  
  #Import Data to H2O Cluster
  {
    train_hex <- as.h2o(localH2O, trainSet, key = 'trainSet')
    test_hex <- as.h2o(localH2O, testSet, key = 'testSet')
  }
  
  #Split the dataset into 80:20 for training and validation
  {
    train_hex_split <- h2o.splitFrame(train_hex, ratios = 0.8, shuffle = T)
    y_train <- as.factor(as.matrix(train_hex_split[[1]]$label))
    y_valid <- as.factor(as.matrix(train_hex_split[[2]]$label))
  }
  
  #Create a set of netwok topologies
  {
    hidden_layers <- list(c(100,100))
    epochs_layers <- c(1)
    l1_regularizations <- c(1e-5)
    input_dropout_ratios_list <- c(0.2)
    hidden_dropout_ratios_list <- list(c(0.25,0.25))
  }
}

###TRAINING##
{
  # Anomaly detection using auto - encoding
  {
    
  }

  #Train a Deep Neural Network
  {
    #h2o.rm(localH2O, keys = c("train.hex"))
    #rm(model,train_hex)
    model <- h2o.deeplearning(x = 2:M,
                              y = 1,
                              data = train_hex,
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
  }
}

###EVALUATION##
{

}

###TESTING###
{
  
  #Writing the result 
  {
    result <- data.frame(ImageId=seq(1,Nt),label=Eval)
    write.csv(result,file="result_ori_kro.csv",row.names=FALSE)
  }

}
