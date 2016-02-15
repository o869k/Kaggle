#Africa mining Kaggle competition
{
  gc() #clear unsued memory
  rm(list=ls()) # clear workspace
  setwd("C:/Users/user/Documents/R/Africa")
  set.seed(1234) #setting seed for comparison
  library(caret); library(ggplot2)
  library(UsingR); library(fpp) #for ML
  library(reshape2); library(prospectr) #for spectroscopy
  library(caret); library(UsingR)
  library(doParallel); registerDoParallel() #for parallalizaton of chenging images from vectors to matrixes
  library(class); library(kernlab) 
  library(rpart); library(rattle) 
  library(rpart.plot); library(RColorBrewer) 
  library(randomForest); library(party)
  library(car);
}

#Read Data & Preprocessing the sets
{
  trainSet <- read.csv("training.csv")
  CO2_bands <- 2656:2670
  drops <- colnames(trainSet[CO2_bands])
  trainSet <- trainSet[,!names(trainSet) %in% drops] #drop CO2 bands in train set
  testSet <- read.csv("sorted_test.csv")
  testSet <- testSet[,!names(testSet) %in% drops] #drop CO2 bands in test set
  N <- nrow(trainSet)  #Number of train rows
  Nt <- nrow(testSet) #Number of test rows
  M <- ncol(trainSet) #Number of train data column
  target_data <- trainSet[,c(1,(M-4):M)]
  target_data_top <- target_data[which(trainSet$Depth=="Topsoil"),]
  target_data_sub <- target_data[which(trainSet$Depth=="Subsoil"),]
  Mt <- ncol(target_data) #Number of train data column
  Ntop <- nrow(target_data_top) #Number of top soil rows
  Nsub <- nrow(target_data_sub) #Number of subsoil rows
  spatial_data <- trainSet[,c(1,(M-20):(M-6))]
  spatial_data_top <- spatial_data[which(trainSet$Depth=="Topsoil"),]
  spatial_data_sub <- spatial_data[which(trainSet$Depth=="Subsoil"),]
  Mspatial <- ncol(spatial_data) #Number of spatial data column
  spectral_data <- trainSet[,1:(M-21)]
  spectral_data_top <- spectral_data[which(trainSet$Depth=="Topsoil"),]
  spectral_data_sub <- spectral_data[which(trainSet$Depth=="Subsoil"),]
  Mspectral <- ncol(spectral_data) #Number of spectral data column
  names <- sort(trainSet$PIDN)
  soil_properties <- c("Ca", "P", "pH", "SOC", "Sand")
}

#Producing the wavelength vector - more sensable way to look at stuff
{
  wl <- unlist(strsplit(colnames(spectral_data[2:Mspectral]),"m"))
  wl <- (1/as.numeric(wl[c(seq(2,2*(Mspectral-1),2))]))*10^7 #calcualte wavelength
}

###PREPROCESSING###

#Producing the Derivatives Spectral Dataframe
{
  spectral_data_sub_deriv1 <- as.data.frame(t(diff(t(spectral_data_sub[,2:Mspectral]), differences = 1)))
  spectral_data_sub_deriv2 <- as.data.frame(t(diff(t(spectral_data_sub[,2:Mspectral]), differences = 2)))
  spectral_data_top_deriv1 <- as.data.frame(t(diff(t(spectral_data_top[,2:Mspectral]), differences = 1)))
  spectral_data_top_deriv2 <- as.data.frame(t(diff(t(spectral_data_top[,2:Mspectral]), differences = 2)))  
}

#Correlation of Spatial data and target data
{
  scatterplotMatrix(spatial_data)
  cor(spatial_data)
  scatterplotMatrix(target_data[2:Mt])
  cor(target_data[2:Mt])
  skewness(target_data[2:Mt]) #Ca, P & SOC are highly skewed - we want to transform those skewd
  
}

# Apply PCA to the IR sample values to see how well X dimensions can be used to describe them all
{
  #PCA
  PCA <- prcomp(spectral_data[,2:Mspectral],thresh = 0.9999)
  PCAvariance <- sort(PCA$sdev^2/sum(PCA$sdev^2),decreasing = T) #proportion of variance vector
  plot(PCAvariance[1:10]) #plotting the eigen values vs. variance they hold - only a few can reflect the data
  plot(PCA) #The same
}

#Some basic clustering according to target data - hard to cluster
{
  kmeansOpt <- matrix(data = 0,nrow = 60,ncol = 100)
  for (k in 1:60)
  {
    for (n in 1:100)
    {
      kmeansObj <- kmeans(spatial_data_top[,2:16],centers=60,iter.max = 9999) 
      plot(spatial_data_top$BSAN,col=kmeansObj$cluster,pch=19); #plotting k-means
      kmeansOpt[k,n] <- table(kmeansObj$cluster)[1]
    }
    print(k)
  }
}

#Trying to do a multivariate regression - it's possible but need more regresion knowledge
{
  require(stats); require(graphics)
  fit <- lm(target_data_sub$Ca ~ spatial_data$BSAN + spatial_data$ELEV + spatial_data$BSAV + spatial_data$BSAS
            + spatial_data$LSTD + spatial_data$REF3 + spatial_data$REF7 + spatial_data$TMAP + spatial_data$TMFI)
  summary(fit); plot(target_data$Ca,pch=21); abline(fit,col="red")
  fit$coefficients
}

#Some basic clustering according to spatial data: we can find the Sentinel Landscapes accrding to TMAP & TMFI
{
  #NOTE: IT CHANGES COLORS (CLUSTERS) - so can't compare between iterations
  kmeansObj <- kmeans(spatial_data[,15:16],centers=56,iter.max = 99) 
  plot(spatial_data$TMAP,spatial_data$TMFI,col=kmeansObj$cluster,pch=19); #plotting k-means
  
  table(kmeansObj$cluster)
  spatial_data$cluster <- kmeansObj$cluster
}

#Ploting spectral data for a row :raw data & 1st & 2nd derivatives
{
  test_vec <- as.vector(t(spectral_data_sub[1,2:Mspectral])) #spectral row data for 1 example vs. wavelength[nm]
  plot(wl,test_vec,type="l"); 
  sampling <- c(seq(2,1800,200),seq(1800,2300,30),seq(2300,2900,100),seq(2900,(Mspectral-1),10))
  points(wl[sampling],test_vec[sampling],col="red") #adding a smapled raw data 
  
  plot(test_vec,type="l"); plot(test_vec[seq(1,(Mspectral-1),100)],col="red") #adding a smapled raw data 
  
  sg <- savitzkyGolay(test_vec, p = 3, w = 11, m = 0) # smoothing filter. note it will loose the edges of spectrum
  plot(sg,type="l",col="red")
  
  d1 <- diff(test_vec, differences = 1) # first derivative as is (it is noisy)
  plot(wl[seq(2:(Mspectral-1))],d1,type="l")
  sampling_deriv1 <- c(seq(2,1900,300),seq(1900,2100,10),seq(2100,3000,100),seq(3000,(Mspectral-1),8))
  points(wl[sampling_deriv1],d1[sampling_deriv1],col="red") #adding a smapled raw data 
  
  gsd1 <- gapDer(test_vec, m = 1, w = 11, s = 10) # gap derivative will smooth the 1st derivative
  lines(gsd1,type="l",col="red")
  
  d2 <- diff(test_vec, differences = 2) # second derivative as is (it is noisy)
  plot(wl[seq(2:(Mspectral-2))],d2,type="l")
  sampling_deriv2 <- c(seq(2,1900,300),seq(1900,2100,10),seq(2100,3000,100),seq(3000,(Mspectral-1),8))
  points(wl[sampling_deriv2],d2[sampling_deriv2],col="red") #adding a smapled raw data 
  
  gsd2 <- gapDer(test_vec, m = 1, w = 11, s = 10) # gap derivative will smooth the 1st derivative
  lines(gsd2,type="l",col="red")   
  #The qusetion is wether to take the spectral data as is or the derivative spectral data as the prediction of molecule - feature extraction
}

#Plotting the target data (top & sub) + transformations for better seperation
{
  plot(log(target_data_sub$Ca+2),type="l"); lines(log(target_data_top$Ca+2),col="red")
  plot(log(target_data_sub$P+2),type="l"); lines(log(target_data_top$P+2),col="red")
  plot(log(target_data_sub$pH+2),type="l"); lines(log(target_data_top$pH+2),col="red")
  plot(log(target_data_sub$SOC+2),type="l"); lines(log(target_data_top$SOC+2),col="red")
  plot(log(target_data_sub$Sand+2),type="l"); lines(log(target_data_top$Sand+2),col="red")
}

###TRAINING###

#Seperate the each training (top & sub soils) set to a train (70%) & evaluation (30%) cross validation
{
#   trainSetSubIndTrain <- sort(unlist(createDataPartition(target_data_sub$Ca,p=0.7),use.names = F))
#   trainSetTopIndTrain <- sort(unlist(createDataPartition(target_data_top$Ca,p=0.7),use.names = F))
#
  Folds <- 4
  trainSetTopIndTrain <- createFolds(target_data_top$Ca,Folds,list=T,returnTrain=T)
  sapply(trainSetTopIndTrain,length) #size of folds
  trainSetSubIndTrain <- createFolds(target_data_sub$Ca,Folds,list=T,returnTrain=T)
  sapply(trainSetSubIndTrain,length) #size of folds  
}

#For each soil property & for each data set - train a random forest (optimization on the derivation of data & transformation of target soil) by CV
{
  Optimization_Table_Top <- data.frame(Soil=rep(NA,Mt-1),deriv_type=rep(NA,Mt-1),trans_type=rep(NA,Mt-1),error=rep(NA,Mt-1)) #optimization table 
  Optimization_Table_Sub <- data.frame(Soil=rep(NA,Mt-1),deriv_type=rep(NA,Mt-1),trans_type=rep(NA,Mt-1),error=rep(NA,Mt-1)) #optimization table 
  
  for (i in 1:length(soil_properties)) #for each soil . use only for P. type length(soil_properties)
  {
    soil <- soil_properties[i] #soil property to focus on
    ErrMtxSub <- matrix(0,nrow=3,ncol=3)      
    ErrMtxTop <- matrix(0,nrow=3,ncol=3)
    
    for (j in 1:Folds) #cross validation
    {      
      #For top soil
      {
        #for raw spectral data: 3 types of transformations
        {
#           trainModelPCA <- preProcess(cbind(spectral_data_top[trainSetTopIndTrain[[j]],2:Mspectral],spatial_data_top[trainSetTopIndTrain[[j]],2:Mspatial]),method="pca",pcaComp=100)
#           trainSetPCA <- predict(trainModelPCA,cbind(spectral_data_top[trainSetTopIndTrain[[j]],2:Mspectral],spatial_data_top[trainSetTopIndTrain[[j]],2:Mspatial]))
#           evalModelPCA <- preProcess(cbind(spectral_data_top[trainSetTopIndTrain[[j]],2:Mspectral],spatial_data_top[trainSetTopIndTrain[[j]],2:Mspatial]),method="pca",pcaComp=100)
#           evalSetPCA <- predict(evalModelPCA,cbind(spectral_data_top[-trainSetTopIndTrain[[j]],2:Mspectral],spatial_data_top[-trainSetTopIndTrain[[j]],2:Mspatial])) 
          trainSetPCA <- cbind(spectral_data_top[trainSetTopIndTrain[[j]],sampling],spatial_data_top[trainSetTopIndTrain[[j]],2:Mspatial])
          evalSetPCA <- cbind(spectral_data_top[-trainSetTopIndTrain[[j]],sampling],spatial_data_top[-trainSetTopIndTrain[[j]],2:Mspatial])
          print("pca"); gc()
                    
          RFmodel <- train(target_data_top[trainSetTopIndTrain[[j]],soil] ~  . ,data=trainSetPCA,method="rf",ntrees=1)           
          RFmodelpredictions <- predict(RFmodel$finalModel,newdata=evalSetPCA)
          ErrMtxTop[1,1] <- ErrMtxTop[1,1]+round(sqrt(mean((RFmodelpredictions-target_data_top[-trainSetTopIndTrain[[j]],soil])^2)),digits=3)
          print("top,raw,no trans"); gc()   

#           RFmodel <- train(exp(target_data_top[trainSetTopIndTrain[[j]],soil]) ~  . ,data=trainSetPCA,method="rf",ntrees=100) 
#           RFmodelpredictions <- predict(RFmodel$finalModel,newdata=evalSetPCA)
#           ErrMtxTop[1,2] <- ErrMtxTop[1,2]+round(sqrt(mean((log(RFmodelpredictions)-(target_data_top[-trainSetTopIndTrain[[j]],soil]))^2)),digits=3)
#           print("top,raw,exp trans"); gc()
#           
#           RFmodel <- train(log(target_data_top[trainSetTopIndTrain[[j]],soil]+2) ~  . ,data=trainSetPCA,method="rf",ntrees=100) 
#           RFmodelpredictions <- predict(RFmodel$finalModel,newdata=evalSetPCA)
#           ErrMtxTop[1,3] <- ErrMtxTop[1,3]+round(sqrt(mean(((exp(RFmodelpredictions)-2)-(target_data_top[-trainSetTopIndTrain[[j]],soil]))^2)),digits=3)                                                                                
#           print("top,raw,log trans"); gc()
        }
        
        #for deriv1 spectral data: 3 types of transformations
        {
          trainModelPCA <- prcomp(spectral_data_top_deriv1[trainSetTopIndTrain[[j]],],)
          evalModelPCA <- predict(trainModelPCA,newdata=spectral_data_top_deriv1[-trainSetTopIndTrain[[j]],])
          
#           trainModelPCA <- preProcess(cbind(spectral_data_top_deriv1[trainSetTopIndTrain[[j]],2:Mspectral],spatial_data_top[trainSetTopIndTrain[[j]],2:Mspatial]),method="pca",pcaComp=100)
#           trainSetPCA <- predict(trainModelPCA,cbind(spectral_data_top_deriv1[trainSetTopIndTrain[[j]],2:Mspectral],spatial_data_top[trainSetTopIndTrain[[j]],2:Mspatial]))
#           evalModelPCA <- preProcess(cbind(spectral_data_top_deriv1[trainSetTopIndTrain[[j]],2:Mspectral],spatial_data_top[trainSetTopIndTrain[[j]],2:Mspatial]),method="pca",pcaComp=100)
#           evalSetPCA <- predict(evalModelPCA,cbind(spectral_data_top_deriv1[-trainSetTopIndTrain[[j]],2:Mspectral],spatial_data_top[-trainSetTopIndTrain[[j]],2:Mspatial])) 

          trainSetPCA <- cbind(trainModelPCA$x[,1:50],spatial_data_top[trainSetTopIndTrain[[j]],2:Mspatial])
          evalSetPCA <- cbind(evalModelPCA[,1:50],spatial_data_top[-trainSetTopIndTrain[[j]],2:Mspatial])
          print("pca"); gc()
          
          RFmodel <- train(target_data_top[trainSetTopIndTrain[[j]],soil] ~  . ,data=trainSetPCA,method="rf",ntrees=100) 
          RFmodelpredictions <- predict(RFmodel$finalModel,newdata=evalSetPCA)
          ErrMtxTop[2,1] <- ErrMtxTop[2,1]+round(sqrt(mean((RFmodelpredictions-target_data_top[-trainSetTopIndTrain[[j]],soil])^2)),digits=3)
          print("top,deriv1,no trans"); gc()                  
          
#           RFmodel <- train(exp(target_data_top[trainSetTopIndTrain[[j]],soil]) ~  . ,data=trainSetPCA,method="rf",ntrees=100) 
#           RFmodelpredictions <- predict(RFmodel$finalModel,newdata=evalSetPCA)
#           ErrMtxTop[2,2] <- ErrMtxTop[2,2]+round(sqrt(mean((log(RFmodelpredictions)-(target_data_top[-trainSetTopIndTrain[[j]],soil]))^2)),digits=3)
#           print("top,deriv1,exp trans"); gc()
#           
#           RFmodel <- train(log(target_data_top[trainSetTopIndTrain[[j]],soil]+2) ~  . ,data=trainSetPCA,method="rf",ntrees=100) 
#           RFmodelpredictions <- predict(RFmodel$finalModel,newdata=evalSetPCA)
#           ErrMtxTop[2,3] <- ErrMtxTop[2,3]+round(sqrt(mean(((exp(RFmodelpredictions)-2)-(target_data_top[-trainSetTopIndTrain[[j]],soil]))^2)),digits=3)                                                                                
#           print("top,deriv1,log trans"); gc()
        } 
        
        #for deriv2 spectral data: 3 types of transformations
        {
#           trainModelPCA <- preProcess(cbind(spectral_data_top_deriv2[trainSetTopIndTrain[[j]],2:Mspectral],spatial_data_top[trainSetTopIndTrain[[j]],2:Mspatial]),method="pca",pcaComp=100)
#           trainSetPCA <- predict(trainModelPCA,cbind(spectral_data_top_deriv2[trainSetTopIndTrain[[j]],2:Mspectral],spatial_data_top[trainSetTopIndTrain[[j]],2:Mspatial]))
#           evalModelPCA <- preProcess(cbind(spectral_data_top_deriv2[trainSetTopIndTrain[[j]],2:Mspectral],spatial_data_top[trainSetTopIndTrain[[j]],2:Mspatial]),method="pca",pcaComp=100)
#           evalSetPCA <- predict(evalModelPCA,cbind(spectral_data_top_deriv2[-trainSetTopIndTrain[[j]],2:Mspectral],spatial_data_top[-trainSetTopIndTrain[[j]],2:Mspatial])) 
          trainSetPCA <- cbind(spectral_data_top_deriv2[trainSetTopIndTrain[[j]],sampling_deriv2],spatial_data_top[trainSetTopIndTrain[[j]],2:Mspatial])
          evalSetPCA <- cbind(spectral_data_top_deriv2[-trainSetTopIndTrain[[j]],sampling_deriv2],spatial_data_top[-trainSetTopIndTrain[[j]],2:Mspatial])
          print("pca"); gc()
          
          RFmodel <- train(target_data_top[trainSetTopIndTrain[[j]],soil] ~  . ,data=trainSetPCA,method="pls") 
          RFmodelpredictions <- predict(RFmodel$finalModel,newdata=evalSetPCA)
          ErrMtxTop[3,1] <- ErrMtxTop[3,1]+round(sqrt(mean((RFmodelpredictions-target_data_top[-trainSetTopIndTrain[[j]],soil])^2)),digits=3)
          print("top,deriv2,no trans"); gc()                  
          
#           RFmodel <- train(exp(target_data_top[trainSetTopIndTrain[[j]],soil]) ~  . ,data=trainSetPCA,method="rf",ntrees=100) 
#           RFmodelpredictions <- predict(RFmodel$finalModel,newdata=evalSetPCA)
#           ErrMtxTop[3,2] <- ErrMtxTop[3,2]+round(sqrt(mean((log(RFmodelpredictions)-(target_data_top[-trainSetTopIndTrain[[j]],soil]))^2)),digits=3)
#           print("top,deriv2,exp trans") 
#           
#           RFmodel <- train(log(target_data_top[trainSetTopIndTrain[[j]],soil]+2) ~  . ,data=trainSetPCA,method="rf",ntrees=100) 
#           RFmodelpredictions <- predict(RFmodel$finalModel,newdata=evalSetPCA)
#           ErrMtxTop[3,3] <- ErrMtxTop[3,3]+round(sqrt(mean(((exp(RFmodelpredictions)-2)-(target_data_top[-trainSetTopIndTrain[[j]],soil]))^2)),digits=3)                                                                                
#           print("top,deriv2,log trans"); gc()
        }      
      }
                          
      #For sub soil
      {        
        #for raw spectral data: 3 types of transformations
        {
#           trainModelPCA <- preProcess(cbind(spectral_data_sub[trainSetSubIndTrain[[j]],2:Mspectral],spatial_data_sub[trainSetSubIndTrain[[j]],2:Mspatial]),method="pca",pcaComp=100)
#           trainSetPCA <- predict(trainModelPCA,cbind(spectral_data_sub[trainSetSubIndTrain[[j]],2:Mspectral],spatial_data_sub[trainSetSubIndTrain[[j]],2:Mspatial]))
#           evalModelPCA <- preProcess(cbind(spectral_data_sub[trainSetSubIndTrain[[j]],2:Mspectral],spatial_data_sub[trainSetSubIndTrain[[j]],2:Mspatial]),method="pca",pcaComp=100)
#           evalSetPCA <- predict(evalModelPCA,cbind(spectral_data_sub[-trainSetSubIndTrain[[j]],2:Mspectral],spatial_data_sub[-trainSetSubIndTrain[[j]],2:Mspatial])) 
          trainSetPCA <- cbind(spectral_data_sub[trainSetSubIndTrain[[j]],sampling],spatial_data_sub[trainSetSubIndTrain[[j]],2:Mspatial])
          evalSetPCA <- cbind(spectral_data_sub[-trainSetSubIndTrain[[j]],sampling],spatial_data_sub[-trainSetSubIndTrain[[j]],2:Mspatial])
          print("pca"); gc()
          
          RFmodel <- train(target_data_sub[trainSetSubIndTrain[[j]],soil] ~  . ,data=trainSetPCA,method="pls") 
          RFmodelpredictions <- predict(RFmodel$finalModel,newdata=evalSetPCA)
          ErrMtxSub[1,1] <- ErrMtxSub[1,1]+round(sqrt(mean((RFmodelpredictions-target_data_sub[-trainSetSubIndTrain[[j]],soil])^2)),digits=3)
          print("sub,raw,no trans"); gc()
          
#           RFmodel <- train(exp(target_data_sub[trainSetSubIndTrain[[j]],soil]) ~  . ,data=trainSetPCA,method="rf",ntrees=100) 
#           RFmodelpredictions <- predict(RFmodel$finalModel,newdata=evalSetPCA)
#           ErrMtxSub[1,2] <- ErrMtxSub[1,2]+round(sqrt(mean((log(RFmodelpredictions)-(target_data_sub[-trainSetSubIndTrain[[j]],soil]))^2)),digits=3)
#           print("sub,raw,exp trans"); gc()
#           
#           RFmodel <- train(log(target_data_sub[trainSetSubIndTrain[[j]],soil]+2) ~  . ,data=trainSetPCA,method="rf",ntrees=100) 
#           RFmodelpredictions <- predict(RFmodel$finalModel,newdata=evalSetPCA)
#           ErrMtxSub[1,3] <- ErrMtxSub[1,3]+round(sqrt(mean(((exp(RFmodelpredictions)-2)-(target_data_sub[-trainSetSubIndTrain[[j]],soil]))^2)),digits=3)                                                        
#           print("sub,raw,log trans"); gc()
        }
        
        #for deriv1 spectral data: 3 types of transformations
        {
#           trainModelPCA <- preProcess(cbind(spectral_data_sub_deriv1[trainSetSubIndTrain[[j]],2:Mspectral],spatial_data_sub[trainSetSubIndTrain[[j]],2:Mspatial]),method="pca",pcaComp=100)
#           trainSetPCA <- predict(trainModelPCA,cbind(spectral_data_sub_deriv1[trainSetSubIndTrain[[j]],2:Mspectral],spatial_data_sub[trainSetSubIndTrain[[j]],2:Mspatial]))
#           evalModelPCA <- preProcess(cbind(spectral_data_sub_deriv1[trainSetSubIndTrain[[j]],2:Mspectral],spatial_data_sub[trainSetSubIndTrain[[j]],2:Mspatial]),method="pca",pcaComp=100)
#           evalSetPCA <- predict(evalModelPCA,cbind(spectral_data_sub_deriv1[-trainSetSubIndTrain[[j]],2:Mspectral],spatial_data_sub[-trainSetSubIndTrain[[j]],2:Mspatial])) 
          trainSetPCA <- cbind(spectral_data_sub_deriv1[trainSetSubIndTrain[[j]],sampling_deriv1],spatial_data_sub[trainSetSubIndTrain[[j]],2:Mspatial])
          evalSetPCA <- cbind(spectral_data_sub_deriv1[-trainSetSubIndTrain[[j]],sampling_deriv1],spatial_data_sub[-trainSetSubIndTrain[[j]],2:Mspatial])
          print("pca"); gc()
          
          RFmodel <- train(target_data_sub[trainSetSubIndTrain[[j]],soil] ~  . ,data=trainSetPCA,method="pls") 
          RFmodelpredictions <- predict(RFmodel$finalModel,newdata=evalSetPCA)
          ErrMtxSub[2,1] <- ErrMtxSub[2,1]+round(sqrt(mean((RFmodelpredictions-target_data_sub[-trainSetSubIndTrain[[j]],soil])^2)),digits=3)
          print("sub,deriv1,no trans"); gc()
          
#           RFmodel <- train(exp(target_data_sub[trainSetSubIndTrain[[j]],soil]) ~  . ,data=trainSetPCA,method="rf",ntrees=100) 
#           RFmodelpredictions <- predict(RFmodel$finalModel,newdata=evalSetPCA)
#           ErrMtxSub[2,2] <- ErrMtxSub[2,2]+round(sqrt(mean((log(RFmodelpredictions)-(target_data_sub[-trainSetSubIndTrain[[j]],soil]))^2)),digits=3)
#           print("sub,deriv1,exp trans"); gc()
#           
#           RFmodel <- train(log(target_data_sub[trainSetSubIndTrain[[j]],soil]+2) ~  . ,data=trainSetPCA,method="rf",ntrees=100) 
#           RFmodelpredictions <- predict(RFmodel$finalModel,newdata=evalSetPCA)
#           ErrMtxSub[2,3] <- ErrMtxSub[2,3]+round(sqrt(mean(((exp(RFmodelpredictions)-2)-(target_data_sub[-trainSetSubIndTrain[[j]],soil]))^2)),digits=3)                                                        
#           print("sub,deriv1,log trans"); gc()
        }
        
        #for deriv2 spectral data: 3 types of transformations
        {
#           trainModelPCA <- preProcess(cbind(spectral_data_sub_deriv2[trainSetSubIndTrain[[j]],2:Mspectral],spatial_data_sub[trainSetSubIndTrain[[j]],2:Mspatial]),method="pca",pcaComp=100)
#           trainSetPCA <- predict(trainModelPCA,cbind(spectral_data_sub_deriv2[trainSetSubIndTrain[[j]],2:Mspectral],spatial_data_sub[trainSetSubIndTrain[[j]],2:Mspatial]))
#           evalModelPCA <- preProcess(cbind(spectral_data_sub_deriv2[trainSetSubIndTrain[[j]],2:Mspectral],spatial_data_sub[trainSetSubIndTrain[[j]],2:Mspatial]),method="pca",pcaComp=100)
#           evalSetPCA <- predict(evalModelPCA,cbind(spectral_data_sub_deriv2[-trainSetSubIndTrain[[j]],2:Mspectral],spatial_data_sub[-trainSetSubIndTrain[[j]],2:Mspatial])) 
          trainSetPCA <- cbind(spectral_data_sub_deriv2[trainSetSubIndTrain[[j]],sampling_deriv2],spatial_data_sub[trainSetSubIndTrain[[j]],2:Mspatial])
          evalSetPCA <- cbind(spectral_data_sub_deriv2[-trainSetSubIndTrain[[j]],sampling_deriv2],spatial_data_sub[-trainSetSubIndTrain[[j]],2:Mspatial])
          print("pca"); gc()
          
          RFmodel <- train(target_data_sub[trainSetSubIndTrain[[j]],soil] ~  . ,data=trainSetPCA,method="pls") 
          RFmodelpredictions <- predict(RFmodel$finalModel,newdata=evalSetPCA)
          ErrMtxSub[3,1] <- ErrMtxSub[3,1]+round(sqrt(mean((RFmodelpredictions-target_data_sub[-trainSetSubIndTrain[[j]],soil])^2)),digits=3)
          print("sub,deriv2,no trans"); gc()
          
#           RFmodel <- train(exp(target_data_sub[trainSetSubIndTrain[[j]],soil]) ~  . ,data=trainSetPCA,method="rf",ntrees=100) 
#           RFmodelpredictions <- predict(RFmodel$finalModel,newdata=evalSetPCA)
#           ErrMtxSub[3,2] <- ErrMtxSub[3,2]+round(sqrt(mean((log(RFmodelpredictions)-(target_data_sub[-trainSetSubIndTrain[[j]],soil]))^2)),digits=3)
#           print("sub,deriv2,exp trans"); gc() 
#           
#           RFmodel <- train(log(target_data_sub[trainSetSubIndTrain[[j]],soil]+2) ~  . ,data=trainSetPCA,method="rf",ntrees=100) 
#           RFmodelpredictions <- predict(RFmodel$finalModel,newdata=evalSetPCA)
#           ErrMtxSub[3,3] <- ErrMtxSub[3,3]+round(sqrt(mean(((exp(RFmodelpredictions)-2)-(target_data_sub[-trainSetSubIndTrain[[j]],soil]))^2)),digits=3)                                                        
#           print("sub,deriv2,log trans"); gc()
        } 
        
      } 
    
      print(j)
    }
    
    #Evaluation
    {
      bestErrTop <- arrayInd(which.min(ErrMtxTop),dim(ErrMtxTop))         
      Optimization_Table_Top$Soil[i] <- soil
      Optimization_Table_Top$deriv_type[i] <- bestErrTop[1]
      Optimization_Table_Top$trans_type[i] <- bestErrTop[2]
      Optimization_Table_Top$error[i] <- ErrMtxTop[bestErrTop]/Folds
      
      bestErrSub <- arrayInd(which.min(ErrMtxSub),dim(ErrMtxSub))         
      Optimization_Table_Sub$Soil[i] <- soil
      Optimization_Table_Sub$deriv_type[i] <- bestErrSub[1]
      Optimization_Table_Sub$trans_type[i] <- bestErrSub[2]
      Optimization_Table_Sub$error[i] <- ErrMtxSub[bestErrSub]/Folds
    }

    print(i)
  }
}

save(list=ls(), file='data.Rd') #save ws after data & preprocess
load(file = 'data.Rd') #load the dB

###TESTING###

#Preprocess the testSet like trainSet
{
  spatial_test_data <- testSet[,c(1,(Mspectral+1):(Mspectral+Mspatial-1))]
  spatial_test_data_top <- spatial_test_data[which(testSet$Depth=="Topsoil"),]
  spatial_test_data_sub <- spatial_test_data[which(testSet$Depth=="Subsoil"),]
  spectral_test_data <- testSet[,1:Mspectral]
  spectral_test_data_top <- spectral_test_data[which(testSet$Depth=="Topsoil"),]
  spectral_test_data_sub <- spectral_test_data[which(testSet$Depth=="Subsoil"),]
  Ntest_top <- nrow(spatial_test_data_top) #Number of top soil rows of test set
  Ntest_sub <- nrow(spatial_test_data_sub) #Number of subsoil rows of test set
  spectral_test_data_sub_deriv1 <- as.data.frame(t(diff(t(spectral_test_data_sub[,2:Mspectral]), differences = 1)))
  spectral_test_data_sub_deriv2 <- as.data.frame(t(diff(t(spectral_test_data_sub[,2:Mspectral]), differences = 2)))
  spectral_test_data_top_deriv1 <- as.data.frame(t(diff(t(spectral_test_data_top[,2:Mspectral]), differences = 1)))
  spectral_test_data_top_deriv2 <- as.data.frame(t(diff(t(spectral_test_data_top[,2:Mspectral]), differences = 2))) 
}

#Testing according to Optimization matrix - transformation & deriv
{
  #For top soil
  {
    trainModelPCA <- prcomp(spectral_data_top_deriv1)
    testModelPCA <- predict(trainModelPCA,newdata=spectral_test_data_top_deriv1)
    trainSetPCA <- cbind(trainModelPCA$x[,1:50],spatial_data_top[,2:Mspatial])
    testSetPCA <- cbind(testModelPCA[,1:50],spatial_test_data_top[,2:Mspatial])
    
    soil <- "Ca"
    print("pca"); gc()    
    RFmodel <- train(target_data_top[,soil] ~  . ,data=trainSetPCA,method="rf",ntrees=250)           
    predictionsCa_top <- predict(RFmodel$finalModel,newdata=testSetPCA)
    print(c("top ",soil)); gc()
    
    soil <- "P"  
    RFmodel <- train(target_data_top[,soil] ~  . ,data=trainSetPCA,method="rf",ntrees=250)           
    predictionsP_top <- predict(RFmodel$finalModel,newdata=testSetPCA)
    print(c("top ",soil)); gc()
    
    soil <- "pH"   
    RFmodel <- train(target_data_top[,soil] ~  . ,data=trainSetPCA,method="rf",ntrees=250)           
    predictionspH_top <- predict(RFmodel$finalModel,newdata=testSetPCA)
    print(c("top ",soil)); gc()
    
    soil <- "SOC"   
    RFmodel <- train(target_data_top[,soil] ~  . ,data=trainSetPCA,method="rf",ntrees=250)           
    predictionsSOC_top <- predict(RFmodel$finalModel,newdata=testSetPCA)
    print(c("top ",soil)); gc()
    
    soil <- "Sand"   
    RFmodel <- train(target_data_top[,soil] ~  . ,data=trainSetPCA,method="rf",ntrees=250)           
    predictionsSand_top <- predict(RFmodel$finalModel,newdata=testSetPCA)
    print(c("top ",soil)); gc()
  }
  
  #For sub soil
  {
    trainModelPCA <- prcomp(spectral_data_sub_deriv1)
    testModelPCA <- predict(trainModelPCA,newdata=spectral_test_data_sub_deriv1)
    trainSetPCA <- cbind(trainModelPCA$x[,1:50],spatial_data_sub[,2:Mspatial])
    testSetPCA <- cbind(testModelPCA[,1:50],spatial_test_data_sub[,2:Mspatial])
    
    soil <- "Ca"  
    RFmodel <- train(target_data_sub[,soil] ~  . ,data=trainSetPCA,method="rf",ntrees=250)           
    predictionsCa_sub <- predict(RFmodel$finalModel,newdata=testSetPCA)
    print(c("sub ",soil)); gc()
    
    soil <- "P"   
    RFmodel <- train(target_data_sub[,soil] ~  . ,data=trainSetPCA,method="rf",ntrees=250)           
    predictionsP_sub <- predict(RFmodel$finalModel,newdata=testSetPCA)
    print(c("sub ",soil)); gc()
    
    soil <- "pH"  
    RFmodel <- train(target_data_sub[,soil] ~  . ,data=trainSetPCA,method="rf",ntrees=250)           
    predictionspH_sub <- predict(RFmodel$finalModel,newdata=testSetPCA)
    print(c("sub ",soil)); gc()
    
    soil <- "SOC"   
    RFmodel <- train(target_data_sub[,soil] ~  . ,data=trainSetPCA,method="rf",ntrees=250)           
    predictionsSOC_sub <- predict(RFmodel$finalModel,newdata=testSetPCA)
    print(c("sub ",soil)); gc()
    
    soil <- "Sand"   
    RFmodel <- train(target_data_sub[,soil] ~  . ,data=trainSetPCA,method="rf",ntrees=250)           
    predictionsSand_sub <- predict(RFmodel$finalModel,newdata=testSetPCA)
    print(c("sub ",soil)); gc()
  }  
}

#Binding the predictions for top & sub soils for each target
{
  predictionsCa <- vector(mode="numeric",length = Nt)
  predictionsCa[which(testSet$Depth=="Topsoil")] <- predictionsCa_top
  predictionsCa[which(testSet$Depth=="Subsoil")] <- predictionsCa_sub 
  predictionsP <- vector(mode="numeric",length = Nt)
  predictionsP[which(testSet$Depth=="Topsoil")] <- predictionsP_top
  predictionsP[which(testSet$Depth=="Subsoil")] <- predictionsP_sub   
  predictionspH <- vector(mode="numeric",length = Nt)
  predictionspH[which(testSet$Depth=="Topsoil")] <- predictionspH_top
  predictionspH[which(testSet$Depth=="Subsoil")] <- predictionspH_sub 
  predictionsSOC <- vector(mode="numeric",length = Nt)
  predictionsSOC[which(testSet$Depth=="Topsoil")] <- predictionsSOC_top
  predictionsSOC[which(testSet$Depth=="Subsoil")] <- predictionsSOC_sub 
  predictionsSand <- vector(mode="numeric",length = Nt)
  predictionsSand[which(testSet$Depth=="Topsoil")] <- predictionsSand_top
  predictionsSand[which(testSet$Depth=="Subsoil")] <- predictionsSand_sub 
}

#Writing the result 
{
  result <- data.frame(PIDN = testSet$PIDN,Ca=predictionsCa,P=predictionsP,pH=predictionspH,SOC=predictionsSOC,Sand=predictionsSand)
  write.csv(result,file="result_ori_kro.csv",row.names=FALSE)
}

dev.off()