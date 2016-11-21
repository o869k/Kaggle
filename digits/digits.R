#Initialization
{
      rm(list=ls()) # clear workspace
      library(fpp); library(ggplot2)
      library(caret); library(kernlab)
      library(e1071); library(UsingR)
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
      train.pca <- prcomp(trainSet,center = T,sclae = T)
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
      train.pca.data$Labels <- factor(trainLabels)
      eval.pca.data$Labels <- factor(evalLabels)
      train.pca.data$Labels[which(is.na(train.pca.data$Labels))] <- 0
      eval.pca.data$Labels[which(is.na(eval.pca.data$Labels))] <- 0
      rm(eval.pca,test.pca,train.pca,train.pca.var,train,evalindex,evalSet,test,trainSet) #clear memory
      N <- nrow(train.pca.data)
      Ne <- nrow(eval.pca.data)
}

#Train a Multi-Class SVM with a probability approximation on the PCA data + parameters tuning for each 2 classes (1 vs 1) total 45 optimizations
{
      for (i in 1)
      {
            for (j in 4:7)
            {
                  idx <- sort(c(which(train.pca.data$Labels==i),which(train.pca.data$Labels==j)))
                  #use the train set to do grid search over the hyperparameters of statistical methods of the svm (NOTE: labels must be part of data!!!)
                  tuned <- tune.svm(factor(Labels) ~., data = train.pca.data[idx,], gamma = c(10^-3,10^-2,10^-1), cost = c(10^-1,1,10^1))
                  summary(tuned)
                  
                  #train svm with 4-fold CV, with different kernels in order to choose the best classifier
                  model_linear  <- svm(factor(Labels) ~., data = train.pca.data[idx,], kernel="linear", cost=tuned$best.parameters[2],cross = 4) 
                  model_poly_2  <- svm(factor(Labels) ~., data = train.pca.data[idx,], kernel="polynomial", degree=2 , gamma=tuned$best.parameters[1], cost=tuned$best.parameters[2],cross = 4) 
                  model_poly_3  <- svm(factor(Labels) ~., data = train.pca.data[idx,], kernel="polynomial", degree=3 , gamma=tuned$best.parameters[1], cost=tuned$best.parameters[2],cross = 4)                   
                  model_radial  <- svm(factor(Labels) ~., data = train.pca.data[idx,], kernel="radial", gamma=tuned$best.parameters[1], cost=tuned$best.parameters[2],cross = 4) 
                  model_sigmoid_0  <- svm(factor(Labels) ~., data = train.pca.data[idx,], kernel="sigmoid", coef0 =0, gamma=tuned$best.parameters[1], cost=tuned$best.parameters[2] ,cross = 4) 
                  model_sigmoid_1  <- svm(factor(Labels) ~., data = train.pca.data[idx,], kernel="sigmoid", coef0 =1, gamma=tuned$best.parameters[1], cost=tuned$best.parameters[2] ,cross = 4)                   
                  
                  eval_idx <- sort(c(which(eval.pca.data$Labels==i),which(eval.pca.data$Labels==j)))                  
                  #run the models on the eval set for evaluation
                  prediction_linear <- predict(model_linear,eval.pca.data[eval_idx,])
                  prediction_poly_2 <- predict(model_poly_2,eval.pca.data[eval_idx,])
                  prediction_poly_3 <- predict(model_poly_3,eval.pca.data[eval_idx,])                  
                  prediction_radial <- predict(model_radial,eval.pca.data[eval_idx,])
                  prediction_sigmoid_0 <- predict(model_sigmoid_0,eval.pca.data[eval_idx,])
                  prediction_sigmoid_1 <- predict(model_sigmoid_1,eval.pca.data[eval_idx,])
                  
                  a <- rep(0,6)
                  #prediction results
                  tab <- table(prediction_linear,true=factor(evalLabels[eval_idx])); a[1] <- classAgreement(tab)$diag
                  tab <- table(prediction_poly_2,true=factor(evalLabels[eval_idx])); a[2] <- classAgreement(tab)$diag  
                  tab <- table(prediction_poly_3,true=factor(evalLabels[eval_idx])); a[3] <- classAgreement(tab)$diag  
                  tab <- table(prediction_radial,true=factor(evalLabels[eval_idx])); a[4] <- classAgreement(tab)$diag  
                  tab <- table(prediction_sigmoid_0,true=factor(evalLabels[eval_idx])); a[5] <- classAgreement(tab)$diag  
                  tab <- table(prediction_sigmoid_1,true=factor(evalLabels[eval_idx])); a[6] <- classAgreement(tab)$diag                
                  
                  best_model_idx <- which(a==max(a))
                  if (best_model_idx == 1) {best_model <- model_linear}
                  if (best_model_idx == 2) {best_model <- model_poly_2}
                  if (best_model_idx == 3) {best_model <- model_poly_3}
                  if (best_model_idx == 4) {best_model <- model_radial}
                  if (best_model_idx == 5) {best_model <- model_sigmoid_0}
                  if (best_model_idx == 6) {best_model <- model_sigmoid_1}
                  #write.svm(best_model,paste(i,"vs",j,".svm",sep = ''))
                  saveRDS(best_model,paste(i,"vs",j,".svm",sep = ''))
            }            
      }            
}

ImageId <- 1:Nt
Label <- rep(0,Nt)
results <- data.frame(ImageId,Label)
#Testing
{
      for (t in 14586:Nt)
      {      
            votes <- rep(0,length(L))#initialize a voting vector
            
            for (i in 0:(length(L)-1))
            {
                  if (i < (length(L)-1))
                  {
                        for (j in (i+1):(length(L)-1))
                        {
                              #Run the best model on the test set
                              best_model <- readRDS(paste(i,"vs",j,".svm",sep = ''))
                              prediction <- as.integer(as.character(predict(best_model,test.pca.data[t,])))
                              votes[prediction+1] <- votes[prediction+1]+1
                        }
                  }
                  if (i > 0)
                  {     
                        for (j in (i-1):0)
                        {
                              #Run the best model on the test set
                              best_model <- readRDS(paste(j,"vs",i,".svm",sep = ''))
                              prediction <- as.integer(as.character(predict(best_model,test.pca.data[t,])))
                              votes[prediction+1] <- votes[prediction+1]+1
                        }
                  }
            }
            results$Label[t]=which(max(votes)==votes)[1]-1
      }
}

#write the result to file
{
      write.csv(results, file="mc_svm_digits.csv") 
}
