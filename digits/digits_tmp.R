#Initialization
{
      rm(list=ls()) # clear workspace
      library(date); library(fpp)
      library(ff); library(ggplot2)
      library(caret); library(kernlab)
      library(e1071); library(UsingR)
      library(car); library(rgl)
      library(FNN); library(randomForest)
      dev.off()
}

#Read train & test and speperate the labels
{
      train <- read.csv("train.csv", header=T)
      test <- read.csv("test.csv", header=T)
      N <- nrow(train)  
      Nt <- nrow(test)
      labels <- as.factor(train[,1])
      train <- train[,-1]
      M <- ncol(train)
      L <- unique(labels)
      #divide the given train to train & eval in order to choose the best settings for SVM
      evalindex <- sample(1:N,trunc(N/4))
      evalSet <- train[evalindex,]
      trainSet <- train[-evalindex,]
      evalLabels <- labels[evalindex]
      trainLabels <- labels[-evalindex]
}

#run PCA analysis
{
      train.pca <- prcomp(trainSet,center = T,scale. = T)
      summary(train.pca); plot(train.pca, type = "l")
      train.pca.var <- train.pca$sdev^2/sum(train.pca$sdev^2)
      cumsum(train.pca.var); sum(train.pca.var)
      #we can see that by taking the first 260 components, we explain 98% of data (1/3 of original size)
      Mpca <- 260
      train.pca.data <- as.data.frame(train.pca$x[,1:Mpca])
      eval.pca <- predict(train.pca,newdata=evalSet)
      eval.pca.data <- as.data.frame(eval.pca[,1:Mpca])
      test.pca <- predict(train.pca,newdata=test)
      test.pca.data <- as.data.frame(test.pca[,1:Mpca])
      rm(eval.pca,test.pca,train.pca,train.pca.var)
}

#Train a Multi-Class SVM with a probability approximation on the PCA data + parameters tuning
{
      #use the train set to do grid search over the hyperparameters of statistical methods of the svm
      tuned <- tune.svm(trainLabels~., data = train.pca.data, gamma = 10^(-6:-1), cost = 10^(-1:1))
      summary(tuned)

      #train svm with 4-fold CV, with different kernels in order to choose the best classifier
      model_linear  <- svm(trainLabels~., data = train.pca.data, kernel="linear", cost=1, probability = T,cross = 4) 
      model_poly  <- svm(trainLabels~., data = train.pca.data, kernel="polynomial", degree= , coef0 = gamma=, cost=, probability = T,cross = 4) 
      model_radial  <- svm(trainLabels~., data = train.pca.data, kernel="radial", gamma=, cost=, probability = T,cross = 4) 
      model_sigmoid  <- svm(trainLabels~., data = train.pca.data, kernel="sigmoid", coef0 = gamma=, cost=, probability = T,cross = 4) 
      #NEED TO TUNE THE ABOVE CLASSIFIERS!!!
      
      #run the models on the eval set for evaluation
      prediction_linear <- predict(model_linear,eval.pca.data,probability = T)
      prediction_poly <- predict(model_poly,eval.pca.data,probability = T)
      prediction_radial <- predict(model_radial,eval.pca.data,probability = T)
      prediction_sigmoid <- predict(model_sigmoid,eval.pca.data,probability = T)
}

#Testing
{
      #Run the best model the test set
      results <- predict(best_model,test.pca.data)
}

#write the result to file
{
      write(results, file="mc_svm_digits.csv", ncolumns=1) 
}
