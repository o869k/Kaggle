#Initialization
{
  gc() #clear unsued memory
  rm(list=ls()) 
  setwd("C:/Users/user/Documents/R/face")
  set.seed(1234) 
  library(ff); library(ggplot2)
  library(caret); library(fpp) #for ML
  library(doParallel); registerDoParallel() #for parallalizaton of chenging images from vectors to matrixes
  library(jpeg);
}

#Read Data & Preprocess of images 
{
  trainSet <- read.csv("training.csv")
  #trainSet <- read.csv.ffdf(file="train.csv", header=TRUE, VERBOSE=TRUE) #virtual data frame
  #seperate the image from data
  trainFaces <<- foreach(im = as.character(trainSet$Image), .combine=rbind) %dopar% {as.integer(unlist(strsplit(im, " ")))}
  trainSet$Image <- {}
  
  testSet <- read.csv("test.csv") 
  #testSet <- read.csv.ffdf(file="test.csv", header=TRUE, VERBOSE=TRUE) #virtual data frame
  #seperate the image from data  
  testFaces <<- foreach(im = as.character(testSet$Image), .combine=rbind) %dopar% {as.integer(unlist(strsplit(im, " ")))}
  #test_pics_tmp <- sapply(test_pics, FUN=function(x) {as.integer(unlist(strsplit(x," ")))}) #sapply not working- memory
  testSet$Image <- {}
  
  N <- nrow(trainSet)  #Number of train images
  M <- ncol(trainSet) #Number of keypoints per image
  Nt <- nrow(testSet) #Number of test images
  R <- sqrt(ncol(testFaces)) #number of pixels in face row/column
  
  result <- read.csv("IdLookupTable.csv") #SUBMISSION FILE - Note that you will not be detecting all 30 features for each image
  keypoints <- unlist(strsplit(levels(result$FeatureName),split='_x',fixed=T))[c(seq(1,M,2))] #Names of different keypoints
}

#Show a face example and its keypoints
{
  i <- 4264 #image number
  im <- matrix(rev(trainFaces[i,]),nrow=R)
  image(1:R,1:R,im,col=gray((0:255/255)),xlab="X",ylab="Y") #show the image + axis in gray
  points(R-trainSet$nose_tip_x[i],R-trainSet$nose_tip_y[i],col=7)
  points(R-trainSet$left_eye_center_x[i],R-trainSet$left_eye_center_y[i],col=2)
  points(R-trainSet$left_eye_inner_corner_x[i],R-trainSet$left_eye_inner_corner_y[i],col=2)
  points(R-trainSet$left_eye_outer_corner_x[i],R-trainSet$left_eye_outer_corner_y[i],col=2)  
  points(R-trainSet$right_eye_center_x[i],R-trainSet$right_eye_center_y[i],col=3)
  points(R-trainSet$right_eye_inner_corner_x[i],R-trainSet$right_eye_inner_corner_y[i],col=3)
  points(R-trainSet$right_eye_outer_corner_x[i],R-trainSet$right_eye_outer_corner_y[i],col=3)  
  points(R-trainSet$mouth_left_corner_x[i],R-trainSet$mouth_left_corner_y[i],col=4)
  points(R-trainSet$mouth_right_corner_x[i],R-trainSet$mouth_right_corner_y[i],col=4)
  points(R-trainSet$mouth_center_top_lip_x[i],R-trainSet$mouth_center_top_lip_y[i],col=4)
  points(R-trainSet$mouth_center_bottom_lip_x[i],R-trainSet$mouth_center_bottom_lip_y[i],col=4)
  points(R-trainSet$right_eyebrow_inner_end_x[i],R-trainSet$right_eyebrow_inner_end_y[i],col=5)
  points(R-trainSet$right_eyebrow_outer_end_x[i],R-trainSet$right_eyebrow_outer_end_y[i],col=5)
  points(R-trainSet$left_eyebrow_inner_end_x[i],R-trainSet$left_eyebrow_inner_end_y[i],col=6)
  points(R-trainSet$left_eyebrow_outer_end_x[i],R-trainSet$left_eyebrow_outer_end_y[i],col=6)  
}

#Preprocess: there are 2 datasets in the train & test. Divide them by number of keypoints (can be optimized)
{
  #hist(result$ImageId) #Show that there are 2 clusters by the number keypoints in the images of the test set
  #sort(table(result$FeatureName)) #the dividing keypoints are: nose_tip, right_eye_center, left_eye_center, mouth_center_bottom_lip
  trainSetNA <- apply(trainSet,1,FUN=function(x) {sum(is.na(x))})   #will check the NA number of keypoint for each image in the train set
  #hist(trainSetNA) #Show that there are 2 clusters by the number keypoints in the images of the train set
  trainSet1Ind <- which(trainSetNA<20) #Indices of images with <20 NA's of keypoints in TrainSet
  trainSet2Ind <- which(!trainSetNA<20)  #Indices of images with >19 NA's of keypoints in TrainSet
  testSet1Ind <- which(table(result$ImageId)>10) #Indices of images with <20 NA's of keypoints in TestSet
  testSet2Ind <- which(table(result$ImageId)<=10) #Indices of images with >19 NA's of keypoints in TestSet  
}

#Preprocess: remove "bad" data from training set (replace with NA accroding to visual inspection of training data)
{
  trainSet[1908,] <- NA #Leonardo DiCaprio - cause bias
  trainSet[1748,] <- NA #Not in place and cause bias
  trainSet[4264,] <- NA #very small face in many facec pic
  trainSet[6493,] <- NA #very very small face in many facec pic
  trainSet[6494,] <- NA #very very small face in many facec pic  
}

###TRAINING###

#Seperate the each training set to a train (80%) & evaluation (20%) and than apply on relevant testset
{
  trainSet1par <- unlist(createDataPartition(trainSet1Ind,p=0.8),use.names = F)
  trainSet2par <- unlist(createDataPartition(trainSet2Ind,p=0.8),use.names = F)  
  trainSet1IndTrain <- trainSet1Ind[trainSet1par]
  trainSet2IndTrain <- trainSet2Ind[trainSet2par]
  trainSet1IndEval <- trainSet1Ind[-trainSet1par]
  trainSet2IndEval <- trainSet2Ind[-trainSet2par]
}

#Function: Create the avg coordinate sets based on keypoint and search size
ParamsFunction <- function(Data,coord_x,coord_y,s) 
{
  search_size <- s #search step size
  mean_x <- round(mean(trainSet[c(Data),coord_x],na.rm=T),digits=3)
  mean_y <- round(mean(trainSet[c(Data),coord_y],na.rm=T),digits=3)
  x1 <- as.integer(mean_x)-search_size
  x2 <- as.integer(mean_x)+search_size
  y1 <- as.integer(mean_y)-search_size
  y2 <- as.integer(mean_y)+search_size
  params <- expand.grid(x=x1:x2,y=y1:y2)
  return(params)
}

#Train on test set Use the image patch technique (as described in kaggle) for each data set
{
  Keypoints_Optimization_Table1 <- data.frame(keypoint=rep(NA,M/2),patch_size=rep(NA,M/2),search_size=rep(NA,M/2),error=rep(NA,M/2),patch=rep(NA,M/2)) #optimization table of pathces and searches per key
  Keypoints_Optimization_Table2 <- data.frame(keypoint=rep(NA,M/2),patch_size=rep(NA,M/2),search_size=rep(NA,M/2),error=rep(NA,M/2),patch=rep(NA,M/2)) #optimization table of pathces and searches per key
  Q <- c(5,10,15) #Number of pixels around the keypoint for optimization
  S <- 4 #number of search size patches to optimize on (1:S)  
  for (m in 1:(M/2))
  {
    coord <- keypoints[m] #keypoint to focus on 
    ErrMtx1 <- matrix(0,nrow=length(Q),ncol=S)
    ErrMtx2 <- matrix(0,nrow=length(Q),ncol=S)    
    #Bulding the mean patch for each keypoint
    for (q in 1:length(Q))
    {
      patch_size <- Q[q]
      P <- 2*patch_size+1 #Patch length
      coord_x <- paste(coord,"x",sep="_"); coord_y <- paste(coord,"y",sep="_")      
      patches <- foreach (i=1:length(trainSet1IndTrain), .combine=rbind) %dopar% {
        im <- matrix(trainFaces[trainSet1IndTrain[i],],nrow=R)
        x <- trainSet[trainSet1IndTrain[i],coord_x]
        y <- trainSet[trainSet1IndTrain[i],coord_y]
        x1 <- (x-patch_size); x2 <- (x+patch_size)
        y1 <- (y-patch_size); y2 <- (y+patch_size)
        if ( (!is.na(x)) && (!is.na(y)) && (x1>=1) && (x2<=R) && (y1>=1) && (y2<=R) ) {as.vector(im[x1:x2,y1:y2])}
        else {NULL}
      }
      meanPatch1 <- matrix(data=colMeans(patches),nrow=P)
      patches <- foreach (i=1:length(trainSet2IndTrain), .combine=rbind) %dopar% {
        im <- matrix(trainFaces[trainSet2IndTrain[i],],nrow=R)
        x <- trainSet[trainSet2IndTrain[i],coord_x]
        y <- trainSet[trainSet2IndTrain[i],coord_y]
        x1 <- (x-patch_size); x2 <- (x+patch_size)
        y1 <- (y-patch_size); y2 <- (y+patch_size)
        if ( (!is.na(x)) && (!is.na(y)) && (x1>=1) && (x2<=R) && (y1>=1) && (y2<=R) ) {as.vector(im[x1:x2,y1:y2])}
        else {NULL}
      }
      meanPatch2 <- matrix(data=colMeans(patches),nrow=P)          
      #Defining the search coordinates in the evaluation images, optmize on the search step size, per keypoint
      for (s in 1:S)
      {
        search_size <- s #search step size
        params1 <- ParamsFunction(trainSet1IndTrain,coord_x,coord_y,search_size) 
        params2 <- ParamsFunction(trainSet2IndTrain,coord_x,coord_y,search_size)           
        #Evaluating by finding best coordinate to each face in eval set, place the patch on it and calculate RMSE with original data
        err <- vector(mode="numeric",length=length(trainSet1IndEval))
        for (k in 1:length(trainSet1IndEval)) 
        {
          im <- matrix(trainFaces[trainSet1IndEval[k],],nrow=R)
          eval <- foreach(j=1:nrow(params1), .combine=rbind) %dopar% {
            x <- params1$x[j]; y <- params1$y[j] #coordinate
            if (((x-patch_size)>=1) && ((x+patch_size)<=R) && ((y-patch_size)>=1) && ((y+patch_size)<=R))
            { p <- im[(x-patch_size):(x+patch_size),(y-patch_size):(y+patch_size)] 
              score <- round(cor(as.vector(p),as.vector(meanPatch1),digits=3) }
            else {score <- NA}
            score <- ifelse(is.na(score),0,score)
            data.frame(x,y,score)
          }  
          bestCoor <- eval[which.max(eval$score),c("x","y")] #best coordinates with max score
          err[k] <- round(sqrt(mean(c(((trainSet[trainSet1IndEval[k],coord_x]-bestCoor$x)^2),((trainSet[trainSet1IndEval[k],coord_y]-bestCoor$y)^2)),na.rm=T)),digits=3) #RMSE
        }
        ErrMtx1[q,s] <-  mean(err,na.rm=T) #fill error matrix for optimization
        
        err <- vector(mode="numeric",length=length(trainSet2IndEval))
        for (k in 1:length(trainSet2IndEval)) 
        {
          im <- matrix(trainFaces[trainSet2IndEval[k],],nrow=R)
          eval <- foreach(j=1:nrow(params2), .combine=rbind) %dopar% {
            x <- params2$x[j]; y <- params2$y[j] #coordinate
            if (((x-patch_size)>=1) && ((x+patch_size)<=R) && ((y-patch_size)>=1) && ((y+patch_size)<=R))
            { p <- im[(x-patch_size):(x+patch_size),(y-patch_size):(y+patch_size)] 
              score <- round(cor(as.vector(p),as.vector(meanPatch2),digits=3) }
            else {score <- NA}
            score <- ifelse(is.na(score),0,score)
            data.frame(x,y,score)
          }  
          bestCoor <- eval[which.max(eval$score),c("x","y")] #best coordinates with max score
          err[k] <- round(sqrt(mean(c(((trainSet[trainSet2IndEval[k],coord_x]-bestCoor$x)^2),((trainSet[trainSet2IndEval[k],coord_y]-bestCoor$y)^2)),na.rm=T)),digits=3) #RMSE
        }        
        ErrMtx2[q,s] <-  mean(err,na.rm=T)] #fill error matrix for optimization          
      }      
    }
    #best (min) error coordinates - to be used for that facial keypoint at the real test
    bestErr1 <- arrayInd(which.min(ErrMtx1),dim(ErrMtx1))     
    Keypoints_Optimization_Table1$keypoint[m] <- coord
    Keypoints_Optimization_Table1$patch_size[m] <- Q[bestErr1[1]]
    Keypoints_Optimization_Table1$search_size[m] <- bestErr1[2]
    Keypoints_Optimization_Table1$error[m] <- ErrMtx1[bestErr1]
    bestErr2 <- arrayInd(which.min(ErrMtx2),dim(ErrMtx2)) 
    Keypoints_Optimization_Table2$keypoint[m] <- coord
    Keypoints_Optimization_Table2$patch_size[m] <- Q[bestErr2[1]]
    Keypoints_Optimization_Table2$search_size[m] <- bestErr2[2]
    Keypoints_Optimization_Table2$error[m] <- ErrMtx2[bestErr2]
  }
}

###TESTING###

#Build image patch for each relevant keypoint for each set
{
  for (m in 1:(M/2))
  {
    coord <- keypoints[m]
    coord_x <- paste(coord,"x",sep="_"); coord_y <- paste(coord,"y",sep="_")
    
    patch_size <- Keypoints_Optimization_Table1$patch_size[m]
    P <- 2*patch_size+1 #Patch length
    patches <- foreach (i=1:length(trainSet1Ind), .combine=rbind) %dopar% {
      im <- matrix(trainFaces[trainSet1Ind[i],],nrow=R)
      x <- trainSet[trainSet1Ind[i],coord_x]
      y <- trainSet[trainSet1Ind[i],coord_y]
      x1 <- (x-patch_size); x2 <- (x+patch_size)
      y1 <- (y-patch_size); y2 <- (y+patch_size)
      if ( (!is.na(x)) && (!is.na(y)) && (x1>=1) && (x2<=R) && (y1>=1) && (y2<=R) ) {as.vector(im[x1:x2,y1:y2])}
      else {NULL}
    }
    mean_patch <- round(colMeans(patches),digits=3)
    Keypoints_Optimization_Table1$patch[m] <- as.character(mean_patch[1])
    for (j in 2:length(mean_patch))
    {
      Keypoints_Optimization_Table1$patch[m] <- paste(Keypoints_Optimization_Table1$patch[m],as.character(mean_patch[j]))
    }    
    
    patch_size <- Keypoints_Optimization_Table2$patch_size[m]
    P <- 2*patch_size+1 #Patch length
    patches <- foreach (i=1:length(trainSet2Ind), .combine=rbind) %dopar% {
      im <- matrix(trainFaces[trainSet2Ind[i],],nrow=R)
      x <- trainSet[trainSet2Ind[i],coord_x]
      y <- trainSet[trainSet2Ind[i],coord_y]
      x1 <- (x-patch_size); x2 <- (x+patch_size)
      y1 <- (y-patch_size); y2 <- (y+patch_size)
      if ( (!is.na(x)) && (!is.na(y)) && (x1>=1) && (x2<=R) && (y1>=1) && (y2<=R) ) {as.vector(im[x1:x2,y1:y2])}
      else {NULL}
    }
    if(!is.null(patches))
    {
      mean_patch <- round(colMeans(patches),digits=3)
      Keypoints_Optimization_Table2$patch[m] <- as.character(mean_patch[1])
      for (j in 2:length(mean_patch))
      {
        Keypoints_Optimization_Table2$patch[m] <- paste(Keypoints_Optimization_Table2$patch[m],as.character(mean_patch[j]))
      } 
    }
  }
}

#Run on test set
{  
  for (t in 1:(nrow(result)/2))
  {
    imageId <- result$ImageId[2*t-1]
    coord <- strsplit(as.character(result$FeatureName[2*t-1]),split='_x',fixed=T)[1]
    if (imageId %in% testSet1Ind) #if belongs to the first test group
    {
      patch_size <- Keypoints_Optimization_Table1$patch_size[which(Keypoints_Optimization_Table1$keypoint==coord)]
      search_size<- Keypoints_Optimization_Table1$search_size[which(Keypoints_Optimization_Table1$keypoint==coord)]
      coord_x <- paste(coord,"x",sep="_"); coord_y <- paste(coord,"y",sep="_")
      params <- ParamsFunction(trainSet1Ind,coord_x,coord_y,search_size)
      im <- matrix(testFaces[imageId,],nrow=R)
      eval <- foreach(j=1:nrow(params), .combine=rbind) %dopar% {
        x <- params$x[j]; y <- params$y[j] #coordinate
        if (((x-patch_size)>=1) && ((x+patch_size)<=R) && ((y-patch_size)>=1) && ((y+patch_size)<=R))
        { p <- im[(x-patch_size):(x+patch_size),(y-patch_size):(y+patch_size)] 
          score <- round(cor(as.vector(p),as.integer(unlist(strsplit(Keypoints_Optimization_Table1$patch[which(Keypoints_Optimization_Table1$keypoint==coord)], " ")))),digits=3)}
        else {score <- NA}
        score <- ifelse(is.na(score),0,score)
        data.frame(x,y,score)
      }  
      bestCoor <- eval[which.max(eval$score),c("x","y")] #best coordinates with max score
    } else { #other group
      patch_size <- Keypoints_Optimization_Table2$patch_size[which(Keypoints_Optimization_Table2$keypoint==coord)]
      search_size <- Keypoints_Optimization_Table2$search_size[which(Keypoints_Optimization_Table2$keypoint==coord)]
      coord_x <- paste(coord,"x",sep="_"); coord_y <- paste(coord,"y",sep="_")
      params <- ParamsFunction(trainSet2Ind,coord_x,coord_y,search_size)
      im <- matrix(testFaces[imageId,],nrow=R)
      eval <- foreach(j=1:nrow(params), .combine=rbind) %dopar% {
        x <- params$x[j]; y <- params$y[j] #coordinate
        if (((x-patch_size)>=1) && ((x+patch_size)<=R) && ((y-patch_size)>=1) && ((y+patch_size)<=R))
        { p <- im[(x-patch_size):(x+patch_size),(y-patch_size):(y+patch_size)] 
          score <- round(cor(as.vector(p),as.integer(unlist(strsplit(Keypoints_Optimization_Table2$patch[which(Keypoints_Optimization_Table2$keypoint==coord)], " ")))),digits=3)}
        else {score <- NA}
        score <- ifelse(is.na(score),0,score)
        data.frame(x,y,score)
      }  
      bestCoor <- eval[which.max(eval$score),c("x","y")] #best coordinates with max score
    } 
    #fill result output
    result$Location[2*t-1] <- bestCoor$x
    result$Location[2*t] <- bestCoor$y
    print(t)
  }
}

#Show a test face example and its keypoints
{
  ori1 <- as.vector(readJPEG("1.jpg")*255)
  ori2 <- as.vector(readJPEG("2.jpg")*255)
  im_test <- matrix(rev(ori1),nrow=R)
  image(1:R,1:R,im_test,col=gray((0:255)/255)) #show the mean patch for that keypoint
  
  i <- 1000 #image number
  im <- matrix(rev(testFaces[i,]),nrow=R)
  image(1:R,1:R,im,col=gray((0:255/255)),xlab="X",ylab="Y") #show the image + axis in gray
  points(R-result$Location[which(result$ImageId==i)[1]],R-result$Location[which(result$ImageId==i)[1]+1],col=7)
}

#Writing the result 
{
  write.csv(result$Location,file="result_ori_kro.csv")
}

save(list=ls(), file='data.Rd') #save ws after data & preprocess
load(file = 'data.Rd') #load the dB

dev.off()