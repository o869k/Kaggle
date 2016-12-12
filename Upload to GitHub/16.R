#Initializaiotn & Inputs ####
rm(list=ls()) # clear workspace
model_training <- 1 #wheter to train a new model
featrues_type <- 0 #0 - all features / 1 - only blood features / 2 - only MBT (icluding interactions). used for training only
max_num_of_featrues <- 2 #usually 1 2 or 3 (but takes exponentialy more time to compute use with causion).
half_hour_MBT <- 0 #wether this is a 30 minutes MBT or complete 1 hour
only_hcv <- 0 #wether to take only hcv pop or all etiologies. used for training only
set.seed(1)
setwd(mainDir)
options(java.parameters = "-Xmx8000m")
library(e1071); library(caret) 
library(Hmisc); library(xlsx)
library(lubridate); library(nlme)
library(car); library(MASS)
library(nlstools); library(nls2)
library(minpack.lm); library(doParallel)
library(ROCR); library(epiR)
library(FSelector); library(ggplot2)
library(foreach); library(pROC)
library(OptimalCutpoints); library(matrixcalc)

#Functions ####
as.lm.nls <- function(object, ...) {
    if (!inherits(object, "nls")) {
        w <- paste("expected object of class nls but got object of class:",
                   paste(class(object), collapse = " "))
        warning(w)
    }
    
    gradient <- object$m$gradient()
    if (is.null(colnames(gradient))) {
        colnames(gradient) <- names(object$m$getPars())
    }
    
    response.name <- if (length(formula(object)) == 2) "0" else
        as.character(formula(object)[[2]])
    
    lhs <- object$m$lhs()
    L <- data.frame(lhs, gradient)
    names(L)[1] <- response.name
    
    fo <- sprintf("%s ~ %s - 1", response.name,
                  paste(colnames(gradient), collapse = "+"))
    fo <- as.formula(fo, env = as.proto.list(L))
    
    do.call("lm", list(fo, offset = substitute(fitted(object))))
}
lorenztian1 <- function (t, tau, beta, Tmax, tD) {
    .expr2 <- beta*tau*atan((t-tD)/tau)
    .expr2[(t>(tD+Tmax)) | (t<tD)] <- 0
    .expr3 <- beta*tau*(atan((t-tD)/tau)-atan((t-(tD+Tmax))/tau))
    .expr3[t<(tD+Tmax)] <- 0
    .value <- .expr2+.expr3
    .value
}
nls_fit_plot_save_data_lorenztian1 <- function(x,y,m,id,CSPH) {
    #Convergence Info
    isConv <- m$convInfo$isConv #is converged
    finIter <- m$convInfo$finIter #number of iterations
    finTol <- m$convInfo$finTol #final tolerance
    tau_info <- summary(m)$coefficients[1,] #parametr tau
    names(tau_info) <- paste0("tau_",names(tau_info))
    beta_info <- summary(m)$coefficients[2,] #parametr beta
    names(beta_info) <- paste0("beta_",names(beta_info))
    Tmax_info <- summary(m)$coefficients[3,] #parameter Tmax
    names(Tmax_info) <- paste0("Tmax_",names(Tmax_info))
    tD_info <- summary(m)$coefficients[4,] #parameter tD
    names(tD_info) <- paste0("tD_",names(tD_info))
    sk_test <- test.nlsResiduals(nlsResiduals(m)) #residuals normality
    res_mean <- mean(residuals(m)) #residuals mean
    res_sd <- sd(residuals(m)) #residuals std
    aic <- round(AIC(m),2) #aic criteria
    bic <- round(BIC(m),2) #bic criteria
    LogLik <- round(logLik(m),2) #Log likelihood
    RSS <- sum(residuals(m)^2)  # Residual sum of squares
    TSS <- sum((y - mean(y))^2)  # Total sum of squares
    Rsquare <- round(r_square <- (1 - (RSS/TSS)),2)  # R-squared measure - a measure of goodness of fit
    conv_data <- c(isConv=isConv,finIter=finIter,tau_info,beta_info,Tmax_info,tD_info,sk_residuals=sk_test$statistic,sk_residuals_p_value=sk_test$p.value,
                   res_mean=res_mean,res_sd=res_sd,AIC=aic,BIC=bic,LogLik=LogLik,RSS=RSS,TSS=TSS,Rsquare=Rsquare)
    
    predCI <- predict(as.lm.nls(m), interval = "confidence", level = 0.95) #add confidence interval as well
    pred1 <- approx(x, predCI[,1], xout = seq(0,max(x),0.01)) ## fitted values
    pred2 <- approx(x, predCI[,2], xout = seq(0,max(x),0.01)) ## lower CI
    pred3 <- approx(x, predCI[,3], xout = seq(0,max(x),0.01)) ## upper CI
    
    #plot
    plot(x,y,xlab="Time [Minutes]",ylab="MHR [ug/min]",main=paste0("ID: ",id,", R^2: ",Rsquare,", AIC: ",aic),ylim=c(0,500))
    lines(x,fitted(m),col=CSPH,lwd=2)
    lines(x,predCI[,2],col=CSPH,lwd=1,lty=2)
    lines(x,predCI[,3],col=CSPH,lwd=1,lty=2)
    
    return(list(conv_data=conv_data,fitted=fitted(m))) #return
}
lorenztian2 <- function (t, tau, beta, tD) {
    .expr2 <- beta*tau*atan((t-tD)/tau)
    .expr2[t<tD] <- 0
    .value <- .expr2
    .value
}
nls_fit_plot_save_data_lorenztian2 <- function(x,y,m,id,CSPH) {
    #Convergence Info
    isConv <- m$convInfo$isConv #is converged
    finIter <- m$convInfo$finIter #number of iterations
    finTol <- m$convInfo$finTol #final tolerance
    tau_info <- summary(m)$coefficients[1,] #parametr tau
    names(tau_info) <- paste0("tau_",names(tau_info))
    beta_info <- summary(m)$coefficients[2,] #parametr beta
    names(beta_info) <- paste0("beta_",names(beta_info))
    tD_info <- summary(m)$coefficients[3,] #parameter tD
    names(tD_info) <- paste0("tD_",names(tD_info))
    sk_test <- test.nlsResiduals(nlsResiduals(m)) #residuals normality
    res_mean <- mean(residuals(m)) #residuals mean
    res_sd <- sd(residuals(m)) #residuals std
    aic <- round(AIC(m),2) #aic criteria
    bic <- round(BIC(m),2) #bic criteria
    LogLik <- round(logLik(m),2) #Log likelihood
    RSS <- sum(residuals(m)^2)  # Residual sum of squares
    TSS <- sum((y - mean(y))^2)  # Total sum of squares
    Rsquare <- round(r_square <- (1 - (RSS/TSS)),2)  # R-squared measure - a measure of goodness of fit
    conv_data <- c(isConv=isConv,finIter=finIter,tau_info,beta_info,Tmax0,rep(0,3),tD_info,sk_residuals=sk_test$statistic,sk_residuals_p_value=sk_test$p.value,
                   res_mean=res_mean,res_sd=res_sd,AIC=aic,BIC=bic,LogLik=LogLik,RSS=RSS,TSS=TSS,Rsquare=Rsquare)
    
    predCI <- predict(as.lm.nls(m), interval = "confidence", level = 0.95) #add confidence interval as well
    pred1 <- approx(x, predCI[,1], xout = seq(0,max(x),0.01)) ## fitted values
    pred2 <- approx(x, predCI[,2], xout = seq(0,max(x),0.01)) ## lower CI
    pred3 <- approx(x, predCI[,3], xout = seq(0,max(x),0.01)) ## upper CI
    
    #plot
    plot(x,y,xlab="Time [Minutes]",ylab="MHR [ug/min]",main=paste0("ID: ",id,", R^2: ",Rsquare,", AIC: ",aic),ylim=c(0,500))
    lines(x,fitted(m),col=CSPH,lwd=2)
    lines(x,predCI[,2],col=CSPH,lwd=1,lty=2)
    lines(x,predCI[,3],col=CSPH,lwd=1,lty=2)
    
    return(list(conv_data=conv_data,fitted=fitted(m))) #return
}
remove_outliers <- function(x, na.rm = TRUE, ...) {
    qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
    H <- 1.5 * IQR(x, na.rm = na.rm)
    y <- x
    y[x < (qnt[1] - H)] <- NA
    y[x > (qnt[2] + H)] <- NA
    y
}
vif_func <- function(in_frame,thresh=10,trace=T,...){
    if(class(in_frame) != 'data.frame') in_frame<-data.frame(in_frame)
    
    #get initial vif value for all comparisons of variables
    vif_init<-NULL
    var_names <- sample(names(in_frame))
    for(val in var_names){
        regressors <- var_names[-which(var_names == val)]
        form <- paste(regressors, collapse = '+')
        form_in <- formula(paste(val, '~', form))
        vif_init<-rbind(vif_init, c(val, VIF(lm(form_in, data = in_frame, ...))))
    }
    vif_max<-max(as.numeric(vif_init[,2]))
    
    if(vif_max < thresh){
        if(trace==T){ #print output of each iteration
            prmatrix(vif_init,collab=c('var','vif'),rowlab=rep('',nrow(vif_init)),quote=F)
            cat('\n')
            cat(paste('All variables have VIF < ', thresh,', max VIF ',round(vif_max,2), sep=''),'\n\n')
        }
        return(var_names)
    }
    else{
        
        in_dat<-in_frame
        
        #backwards selection of explanatory variables, stops when all VIF values are below 'thresh'
        while(vif_max >= thresh){
            
            vif_vals<-NULL
            var_names <- sample(names(in_dat))
            
            for(val in var_names){
                regressors <- var_names[-which(var_names == val)]
                form <- paste(regressors, collapse = '+')
                form_in <- formula(paste(val, '~', form))
                vif_add<-VIF(lm(form_in, data = in_dat, ...))
                vif_vals<-rbind(vif_vals,c(val,vif_add))
            }
            max_row<-which(vif_vals[,2] == max(as.numeric(vif_vals[,2])))[1]
            
            vif_max<-as.numeric(vif_vals[max_row,2])
            
            if(vif_max<thresh) break
            
            if(trace==T){ #print output of each iteration
                prmatrix(vif_vals,collab=c('var','vif'),rowlab=rep('',nrow(vif_vals)),quote=F)
                cat('\n')
                cat('removed: ',vif_vals[max_row,1],vif_max,'\n\n')
                flush.console()
            }
            
            in_dat<-in_dat[,!names(in_dat) %in% vif_vals[max_row,1]]
            
        }
        
        return(names(in_dat))
        
    }
    
}

#Read Data ####
#Galectin Data
fulldata_new <- read.xlsx2(Galectin_study,1,stringsAsFactors = F)
fulldata_new$Weight <- as.numeric(fulldata_new$Weight)
fulldata_new$Height <- as.numeric(fulldata_new$Height)
fulldata_new$CSPH <- factor(fulldata_new$CSPH)
fulldata_new <- fulldata_new[order(fulldata_new$Pt),]
fulldata_new$Site <- factor(fulldata_new$Site)
fulldata_new$DOB <- as.Date(as.numeric(fulldata_new$DOB),origin="1899-12-30")
fulldata_new$Date.of.first.MBT <- as.Date(as.numeric(fulldata_new$Date.of.first.MBT),origin="1899-12-30")
names(fulldata_new)[4] <- "Date.of.MBT"
names(fulldata_new)[9] <- "Ethnicity.Written"

dataset_new <- read.xlsx2(Galectin_study,2,stringsAsFactors = F)
dataset_new$MBTTIM <- as.numeric(as.character(dataset_new$MBTTIM))
dataset_new$MBTDOB <- as.numeric(as.character(dataset_new$MBTDOB))
dataset_new <- aggregate(cbind(MBTTIM,MBTDOB) ~ Subject,data=dataset_new,c)
dataset_new$num_of_timings <- sapply(dataset_new$MBTTIM,length)
dataset_new$num_of_values <- sapply(dataset_new$MBTDOB,length)

#CSPH Data
fulldata_old <- read.xlsx2(CSPH_study,1,stringsAsFactors=F)
fulldata_old$Weight <- as.numeric(as.character(fulldata_old$MBT.Weight))
fulldata_old$Height <- as.numeric(as.character(fulldata_old$Height))
fulldata_old$CSPH <- factor(fulldata_old$CSPH)
fulldata_old <- fulldata_old[order(fulldata_old$Pt),]
fulldata_old$Site <- factor(fulldata_old$Site)
fulldata_old$Enrollement.Date <- as.Date(as.numeric(fulldata_old$Enrollement.Date),origin="1899-12-30")
fulldata_old$DOB <- as.Date(as.numeric(fulldata_old$DOB),origin="1899-12-30")
fulldata_old$Date.of.MBT <- as.Date(as.numeric(fulldata_old$Date.of.MBT),origin="1899-12-30")
fulldata_old$HCV.treatment.completion <- as.Date(as.numeric(fulldata_old$HCV.treatment.completion),origin="1899-12-30")
fulldata_old$Other.Tests.date  <- as.Date(as.numeric(fulldata_old$Other.Tests.date),origin="1899-12-30")
fulldata_old$Recent.Immaging.date  <- as.Date(as.numeric(fulldata_old$Recent.Immaging.date),origin="1899-12-30")
fulldata_old$Recent.Biopsy.date  <- as.Date(as.numeric(fulldata_old$Recent.Biopsy.date),origin="1899-12-30")
fulldata_old$Recent.Endoscopy.date  <- as.Date(as.numeric(fulldata_old$Recent.Endoscopy.date),origin="1899-12-30")
fulldata_old$Previous.HVPG.Date  <- as.Date(as.numeric(fulldata_old$Previous.HVPG.Date),origin="1899-12-30")

dataset_old <- read.xlsx2(CSPH_study,3,stringsAsFactors=F)
dataset_old$StartTime <- as.POSIXct(strptime(as.character(dataset_old$StartTime), "%d%b%Y %H:%M"))
dataset_old$EndTime <- as.POSIXct(strptime(as.character(dataset_old$EndTime), "%d%b%Y %H:%M"))
dataset_old$DOBListTimes <- sapply(dataset_old$DOBListTimes,function(x){as.numeric(strsplit(as.character(x),",")[[1]])})
dataset_old$DOBListValues <- sapply(dataset_old$DOBListValues,function(x){as.numeric(strsplit(as.character(x),",")[[1]])})
dataset_old$ID <- as.character(dataset_old$ID)
dataset_old$Number <- as.integer(as.character(dataset_old$Number))
dataset_old$num_of_timings <- sapply(dataset_old$DOBListTimes,length)
dataset_old$num_of_values <- sapply(dataset_old$DOBListValues,length)
dataset_old <- dataset_old[order(dataset_old$ID),] #sort the dataset_old by ID
dataset_old <- dataset_old[,c("ID","DOBListTimes","DOBListValues","num_of_timings","num_of_values")]
names(dataset_old) <- names(dataset_new)

#Combine
fulldata <- rbind(fulldata_old[,c("Pt","Age","Gender.M.F","Height","Weight","CSPH")],fulldata_new[,c("Pt","Age","Gender.M.F","Height","Weight","CSPH")])
row.names(fulldata) <- 1:nrow(fulldata)
dataset <- rbind(dataset_old,dataset_new)
row.names(dataset) <- 1:nrow(dataset)
rm(dataset_new,dataset_old)

#Features engineering ####
#Features engineering of CSPH dataset
#General experiment Information
fulldata_old$Initials <- NULL; fulldata_old$Patient.Termination <- NULL
fulldata_old$Index <- NULL; fulldata_old$Index2 <- NULL
fulldata_old$Gender <- NULL; fulldata_old$Ethnicity <- NULL
fulldata_old$DOB <- NULL; fulldata_old$Weight <- NULL
#Information from other tests
fulldata_old$Additional.Liver.test.results <- NULL; fulldata_old$Additional.Liver.test.results.1 <- NULL
fulldata_old$TE <- NULL; fulldata_old$Fibroscan.Txt <- NULL
fulldata_old$Other.Tests.date <- NULL; fulldata_old$Immaging.method <- NULL
fulldata_old$Recent.Immaging.date <- NULL; fulldata_old$Immaging.results <- NULL
fulldata_old$Recent.Biopsy.date <- NULL; fulldata_old$Recent.Endoscopy.date <- NULL
fulldata_old$Biopsy.results <- NULL; fulldata_old$Endoscopy.results <- NULL
fulldata_old$HVPG.Comments <- NULL; fulldata_old$HVPG.indication <- NULL
fulldata_old$Previous.HVPG.Date <- NULL; fulldata_old$Previous.HVPG.Result <- NULL
#Information from blood tests
fulldata_old$Abumin.Clean <- NULL; fulldata_old$Creatinine.Clean <- NULL
fulldata_old$GGT <- NULL; fulldata_old$AST <- NULL
fulldata_old$ALT <- NULL; fulldata_old$ALP <- NULL
fulldata_old$CTP <- NULL; fulldata_old$MELD <- NULL
#Information related to HVPG to not use at the moment (this is only binary classification)
fulldata_old$HVPG12 <- NULL; fulldata_old$PH <- NULL
#Information history of medical events & habits (detailed text)
fulldata_old$Smoking.details <- NULL; fulldata_old$Alcohol.Details <- NULL
fulldata_old$Non.Liver.Related.Medical.History.and.Interventions <- NULL; fulldata_old$Liver.Related.Events <- NULL
fulldata_old$Liver.related.interventions <- NULL; fulldata_old$Medications <- NULL
fulldata_old$Comments <- NULL;  

#Add new information and factor columns in main fulldata_old
fulldata_old$CSPH <- factor(fulldata_old$CSPH)
fulldata_old$NEW.Generation.HCV.Treatment[is.na(fulldata_old$NEW.Generation.HCV.Treatment)] <- 0
fulldata_old$HCC[is.na(fulldata_old$HCC)] <- 0
fulldata_old$Excluded.from.Analysis[is.na(fulldata_old$Excluded.from.Analysis)] <- 0
fulldata_old$Excluded.from.Analysis <- factor(fulldata_old$Excluded.from.Analysis)
fulldata_old$Patient.has.taken.amiodarone.or.statins.within.30.days.prior.to.MBT.or.HVPG.procedure <- factor(fulldata_old$Patient.has.taken.amiodarone.or.statins.within.30.days.prior.to.MBT.or.HVPG.procedure)
fulldata_old$Using.Beta.Blocker <- factor(fulldata_old$Using.Beta.Blocker)
fulldata_old$NEW.Generation.HCV.Treatment <- factor(fulldata_old$NEW.Generation.HCV.Treatment)
fulldata_old$HCC <- factor(fulldata_old$HCC)
fulldata_old$Etiology <- factor(fulldata_old$Etiology)
fulldata_old$Device.. <- factor(fulldata_old$Device..)
fulldata_old$Test.. <- factor(fulldata_old$Test..)
fulldata_old$BSA <- 0.007184*fulldata_old$Height^0.725*as.numeric(fulldata_old$MBT.Weight)^0.425 #Dubois BSA
fulldata_old$Time.between.enrollement.and.MBT.completion <- as.numeric(fulldata_old$Date.of.MBT -fulldata_old$Enrollement.Date)
fulldata_old$Date.of.MBT <- NULL
fulldata_old$Enrollement.Date <- NULL
fulldata_old$Previous.HVPG.Date <- NULL
fulldata_old$HCV.treatment.completion <- NULL
fulldata_old$Previous.HVPG.Result <- NULL
fulldata_old$Ethnicity.Written <- as.character(fulldata_old$Ethnicity.Written)
fulldata_old$Ethnicity.Written[fulldata_old$Ethnicity.Written %in% c("Asian","Black","Hispanic")] <- "Non-White"
fulldata_old$Ethnicity.Written <- factor(fulldata_old$Ethnicity.Written)
fulldata_old$Smoker <- as.character(fulldata_old$Smoker)
fulldata_old$Smoker[fulldata_old$Smoker %in% c("Stopped","Yes")] <- "Yes/Was"
fulldata_old$Smoker <- factor(fulldata_old$Smoker)
fulldata_old$Alcohol <- as.character(fulldata_old$Alcohol)
fulldata_old$Alcohol[fulldata_old$Alcohol %in% c("Stopped")] <- ""
fulldata_old$Alcohol <- factor(fulldata_old$Alcohol)
fulldata_old$Abnormal.Physical.Findings <- as.character(fulldata_old$Abnormal.Physical.Findings)
fulldata_old$Abnormal.Physical.Findings[fulldata_old$Abnormal.Physical.Findings %in% c("scattered spider angiomas on chest wall","Obesity","Telangectasias")] <- "Yes"
fulldata_old$Abnormal.Physical.Findings <- factor(fulldata_old$Abnormal.Physical.Findings)
fulldata_old$Presence.of.Varices..EGD. <- factor(fulldata_old$Presence.of.Varices..EGD.)
fulldata_old$Ascites <- factor(fulldata_old$Ascites)
fulldata_old$Encephalopathy <- factor(fulldata_old$Encephalopathy)
fulldata_old$Site <- NULL
fulldata_old$Test.. <- NULL
fulldata_old$Device.. <- NULL
names(fulldata_old)[7] <- "Weight"
names(fulldata_old)[13] <- "Statins"
names(fulldata_old)[54:56] <- paste0(names(fulldata_old)[54:56],".15")
names(fulldata_old)[68] <- "Platelets"
names(fulldata_old)[70] <- "Bilirubin"

#Features engineering of Galectin dataset
#Remove the follwing columsn at the moment
#General experiment Information
fulldata_new$Gender <- NULL; fulldata_new$Reason <- NULL
fulldata_new$DOB <- NULL; fulldata_new$MBT.Weight <- NULL
#Information from other tests
fulldata_new$Fibroscan.PreScreen..Wk..9. <- NULL; fulldata_new$Fibroscan.Visit1..Wk1. <- NULL
fulldata_new$Fibroscan.Visit13..Wk25...1. <- NULL; fulldata_new$Biopsy.ISHAK.Score <- NULL
fulldata_new$Comments.on.Breath.Test <- NULL; fulldata_new$Biopsy.Comments <- NULL
fulldata_new$Other.Histologic.Findings. <- NULL; fulldata_new$Other.Histologic.Findings..1 <- NULL
fulldata_new$Mallory.Bodies.present. <- NULL; fulldata_new$Ballooning.degeneration.present. <- NULL
fulldata_new$X..5..Fat. <- NULL; fulldata_new$Active.NASH..at.least.one.point.steatosis..ballooning..and.inflammation.. <- NULL
# fulldata_new$Encephalopathy <- NULL; fulldata_new$Ascites <- NULL
fulldata_new$Varices.Classification <- NULL; # fulldata_new$Presence.of.Varices..EGD. <- NULL
#Information from blood tests
fulldata_new$Albumin <- NULL; fulldata_new$Creatinine <- NULL
fulldata_new$GGT <- NULL; fulldata_new$AST <- NULL
fulldata_new$ALT <- NULL; fulldata_new$ALP <- NULL
fulldata_new$CTP <- NULL; fulldata_new$MELD <- NULL
#Information related to HVPG to not use at the moment (this is only binary classification)
fulldata_new$HVPG12 <- NULL; fulldata_new$PH <- NULL
fulldata_new$HVPG <- NULL
#Information history of medical events & habits (detailed text)
fulldata_new$Medical.History <- NULL; fulldata_new$Concomintant.Medications <- NULL
fulldata_new$Active.NASH..at.least.one.point.steatosis..ballooning..and.inflammation... <- NULL; fulldata_new$X..5..Fat.. <- NULL
fulldata_new$Ballooning.degeneration.present.. <- NULL; fulldata_new$Mallory.Bodies.present.. <- NULL
fulldata_new$Other.Histologic.Findings.. <- NULL

#Add new information and factor columns in main fulldata_new
fulldata_new$CSPH <- factor(fulldata_new$CSPH)
fulldata_new$Excluded.from.Analysis[is.na(fulldata_new$Excluded.from.Analysis)] <- 0
fulldata_new$Excluded.from.Analysis <- factor(fulldata_new$Excluded.from.Analysis)
fulldata_new$Device.. <- factor(fulldata_new$Device..)
fulldata_new$Test.. <- factor(fulldata_new$Test..)
fulldata_new$BSA <- 0.007184*fulldata_new$Height^0.725*fulldata_new$Weight^0.425 #Dubois BSA
fulldata_new$Date.of.MBT <- NULL
fulldata_new$Ethnicity.Written[!(fulldata_new$Ethnicity.Written %in% c("White Not Hispanic or Latino"))] <- "Non-White"
fulldata_new$Ethnicity.Written[fulldata_new$Ethnicity.Written %in% c("White Not Hispanic or Latino")] <- "White"
fulldata_new$Ethnicity.Written <- factor(fulldata_new$Ethnicity.Written)
fulldata_new$Gender.M.F <- factor(fulldata_new$Gender.M.F)
fulldata_new$Presence.of.Varices..EGD. <- factor(fulldata_new$Presence.of.Varices..EGD.)
fulldata_new$Varices.Classification <- NULL
fulldata_new$Ascites <- factor(fulldata_new$Ascites)
fulldata_new$Encephalopathy <- factor(fulldata_new$Encephalopathy)
fulldata_new$Site <- NULL
fulldata_new$Test.. <- NULL
fulldata_new$Device.. <- NULL

names(fulldata_new)[43:45] <- paste0(names(fulldata_new)[43:45],".15")

#Fit Lorentzian Model ####
#NLS settings
control <- nls.control(maxiter = 1000,warnOnly = TRUE)
schofield_eq <- read.csv("schofield equatios.csv",stringsAsFactors = F)
const_UHR <- 0.9337/134.25*1000/24/60*1.4 #the first constant is time dependent... (x)
conv_data <- NULL
fitted_data <- NULL;
if (half_hour_MBT) 
{
    pdf(paste0("individual_curve_fitting_PDR_",Sys.Date(),"_half_hour.pdf"))
} else {
    pdf(paste0("individual_curve_fitting_PDR_",Sys.Date(),".pdf"))
}
par(mfrow=c(2,2))
for (i in c(1:(nrow(dataset))))
{
    x <- dataset$MBTTIM[i][[1]]
    schofield_idx <- tail(which(schofield_eq$Gender==fulldata$Gender.M.F[i] & schofield_eq$Age.Min<=as.numeric(fulldata$Age[i])),1)
    y <- dataset$MBTDOB[i][[1]] #DOB only
    y <- y-y[1] #start from 0 DOB
    y <- const_UHR*y*(fulldata$Height[i]/100*schofield_eq$Height_coeff[schofield_idx]+fulldata$Weight[i]*schofield_eq$Weight_coeff[schofield_idx]+schofield_eq$Free_coeff[schofield_idx]) #trans. to MHR
    #y <- 0.018717853*y*(fulldata$Height[i]^0.3963)*(fulldata$Weight[i]^0.5378) #trans. to PDR
    id <- dataset$Subject[i]
    
    #MBT up to 30 minutes or up to PEAK And minimum 20 minutes
    if (half_hour_MBT==1)
    {
        y <- y[x<=30]
        x <- x[x<=30]
        twenty_min_idx <- head(which(x>20),1)
        if (!any(twenty_min_idx)) {twenty_min_idx <- length(x)} #in case of less than 20 minutes test
        consecutive_drops <-  rle(diff(y)<0) #find repeated 2 drops - consider that point as the peak and stop MBT if we are over 20 minutes
        idx_to_sum <- which(consecutive_drops$lengths>=2 & consecutive_drops$values==1)
        if(any(idx_to_sum))
        {
            peak_idx <- sum(consecutive_drops$lengths[1:c(idx_to_sum-1)])+1 #peak index in original vector
            end_of_mbt_idx <- peak_idx+2 #time of MBT should end
        } else {end_of_mbt_idx <- length(x)}
        if (end_of_mbt_idx<twenty_min_idx) {end_of_mbt_idx <- twenty_min_idx} #if the peak and 2 consecutive points is before 20 minutes
        y <- y[1:end_of_mbt_idx]
        x <- x[1:end_of_mbt_idx]
    }
    
    #Extract initial starting points for optimization
    Amax0 <- y[which.max(y)] #Peak Point
    Tmax0 <- x[which.max(y)] #Peak Time
    tD0 <- x[1] #time when signal starts to rise
    tau0 <- c(seq(1,200,10)) #decay initial guess
    beta0 <- Amax0/(tau0*atan(Tmax0/tau0)) #beta initial guess
    st1 <- data.frame(tau=tau0,beta=beta0,Tmax=Tmax0,tD=tD0)
    st2 <- data.frame(tau=tau0,beta=beta0,tD=tD0)
    
    #consider problematic cases
    m <- NULL; res <- NULL; flag <- 0
    m <- try(nls2(y ~ lorenztian1(x,tau,beta,Tmax,tD),trace=F,start=st1,lower=c(0,0,0,0),control = control,algorithm = "port"))
    if (class(m) != "try-error")
    {
        if (!is.singular.matrix(m$m$Rmat()))
        {
            res <- nls_fit_plot_save_data_lorenztian1(x,y,m,id,as.numeric(fulldata$CSPH[fulldata$Pt==id]))
            conv_data <- rbind(conv_data,c(ID=id,res$conv_data))
            noise <- c(id,abs(as.numeric((y-res$fitted)/y))) #noise as proportional absolute value
            noise[noise=="NaN"] <- "0"
            while (length(noise)<12) {noise <- c(noise,NA)}
            fitted_data <- rbind(fitted_data,noise)
            flag <- 1
        }
    }
    if (!flag) 
    {
        m <- NULL; res <- NULL
        m <- nls2(y ~ lorenztian2(x,tau,beta,tD),trace=F,start=st2,lower=c(0,0,0),control = control,algorithm = "port")
        if ((!is.null(m)) & (!is.singular.matrix(m$m$Rmat()))) 
        {
            res <- nls_fit_plot_save_data_lorenztian2(x,y,m,id,as.numeric(fulldata$CSPH[fulldata$Pt==id]))
            conv_data <- rbind(conv_data,c(ID=id,res$conv_data))
            noise <- c(id,abs(as.numeric((y-res$fitted)/y))) #noise as proportional absolute value
            noise[noise=="NaN"] <- "0"
            while (length(noise)<12) {noise <- c(noise,NA)}
            fitted_data <- rbind(fitted_data,noise)
            flag <- 1
        } else {
            conv_data <- rbind(conv_data,c(ID=id,rep(NA,28)))
            fitted_data <- rbind(fitted_data,c(ID=id,rep(NA,11)))
            
        }  
    }
    
    rm(Amax0,beta0,end_of_mbt_idx,id,idx_to_sum,noise,peak_idx,m,res,tau0,Tmax0,twenty_min_idx,x,y,st1,st2,consecutive_drops,tD0,flag)
    
}
par(mfrow=c(1,1))
dev.off()

conv_data <- data.frame(conv_data,stringsAsFactors = F)
names(conv_data)[1] <- "Pt"
for (i in 2:ncol(conv_data))
{
    conv_data[,i] <- as.numeric(conv_data[,i])
}
fitted_data <- data.frame(fitted_data,stringsAsFactors = F)
names(fitted_data)[1] <- "Pt"
fitted_data[fitted_data==Inf] <- 0
for (i in 2:ncol(fitted_data))
{
    fitted_data[,i] <- as.numeric(fitted_data[,i])
}

#Calcualte the model fit featrues ####
#Calcualte the integral of the PDR fit until Tmax
if (half_hour_MBT)
{
    L_samples <- 13
    t_final <- seq(0,30,len=L_samples) #time stamps to sample the lines 
} else {
    L_samples <- 25
    t_final <- seq(0,60,len=L_samples) #time stamps to sample the lines   
}
names_t_final <- as.character(t_final)
t_final <- t_final
names(t_final) <- names_t_final
Ncol <- ncol(conv_data)
for (i in 1:nrow(conv_data)) 
{
    for (j in 1:(L_samples-1))
    {
        conv_data[i,Ncol+j] <- lorenztian1(t_final[j+1],conv_data$tau_Estimate[i],conv_data$beta_Estimate[i],conv_data$Tmax_Estimate[i],conv_data$tD_Estimate[i])
    }
    for (j in 1:(L_samples-1))
    {
        integrand <- function(t) {lorenztian1(t,conv_data$tau_Estimate[i],conv_data$beta_Estimate[i],conv_data$Tmax_Estimate[i],conv_data$tD_Estimate[i])}
        if (is.na(conv_data$isConv[i])) 
        {
            conv_data[i,Ncol+L_samples-1+j] <- NA
        } else {
            conv_data[i,Ncol+L_samples-1+j] <- integrate(integrand,0,t_final[j+1],stop.on.error = FALSE)["value"][[1]] 
        }    
    }
}
Ncol_new <- ncol(conv_data)
names(conv_data)[(Ncol+1):(Ncol+L_samples-1)] <- paste0("PDR_",names(t_final)[-1])
names(conv_data)[(Ncol+L_samples):Ncol_new] <- paste0("CPDR_",names(t_final)[-1])
conv_data <- cbind(conv_data,t(apply(cbind(0,conv_data[,c((Ncol+1):(Ncol+L_samples-1))]),1,function(x) {diff(x,1,1)})))
conv_data <- cbind(conv_data,t(apply(cbind(0,conv_data[,c((Ncol+L_samples):Ncol_new)]),1,function(x) {diff(x,1,1)})))
Ncol_new2 <- ncol(conv_data)
names(conv_data)[(Ncol_new+1):(Ncol_new+L_samples-1)] <- paste0("DPDR_",names(t_final)[-1])
names(conv_data)[(Ncol_new+L_samples):Ncol_new2] <- paste0("CDPDR_",names(t_final)[-1])

#Add log transforms
conv_data <- cbind(conv_data,log(conv_data[,c((Ncol+1):(Ncol+L_samples-1))]) )
conv_data <- cbind(conv_data,log(conv_data[,c((Ncol+L_samples):Ncol_new)]) )
conv_data <- cbind(conv_data,log(conv_data[,c((Ncol_new+1):(Ncol_new+L_samples-1))]))
Ncol_new3 <- ncol(conv_data)
names(conv_data)[(Ncol_new2+1):(Ncol_new2+L_samples-1)] <- paste0("log_",names(conv_data)[c((Ncol+1):(Ncol+L_samples-1))]) #log of the original PDR valeus
names(conv_data)[(Ncol_new2+L_samples):(Ncol_new2+L_samples*2-2)] <- paste0("log_",names(conv_data)[c((Ncol+L_samples):Ncol_new)]) #log of the original CPDR valeus
names(conv_data)[(Ncol_new2+L_samples*2-1):Ncol_new3] <- paste0("log_",names(conv_data)[c((Ncol_new+L_samples):Ncol_new2)]) #log of the original CDPDR valeus

#Merge the noise
names(fitted_data)[c(2:ncol(fitted_data))] <- paste0("Noise_",names(t_final)[c(2:ncol(fitted_data))])
conv_data <- merge(conv_data,fitted_data)
conv_data$Noise_2.5 <- NULL

#Add noise commulative values
noise_commulative <- as.data.frame(t(apply(fitted_data,1,function(x) {cumsum(as.numeric(x[2:ncol(fitted_data)]))}))[,c(2:ncol(fitted_data)-1)])
names(noise_commulative) <- paste0("CNoise_",names(t_final[c(2:ncol(fitted_data))]))
conv_data <- cbind(conv_data,noise_commulative)
conv_data$CNoise_2.5 <- NULL

#Peak information on MBT
if (half_hour_MBT)
{
    Max_time <- 30
} else {
    Max_time <- 60
}
for (i in 1:nrow(conv_data)) 
{
    pdr_local <- lorenztian1(seq(0,Max_time,0.1),conv_data$tau_Estimate[i],conv_data$beta_Estimate[i],conv_data$Tmax_Estimate[i],conv_data$tD_Estimate[i])
    integrand <- function(t) {lorenztian1(t,conv_data$tau_Estimate[i],conv_data$beta_Estimate[i],conv_data$Tmax_Estimate[i],conv_data$tD_Estimate[i])}
    if (is.na(conv_data$isConv[i])) 
    {
        conv_data$PDR.Peak[i] <- NA
        conv_data$Peak.Time[i] <- NA
        conv_data$PDR.Peak.div.Peak.Time[i] <- NA
        conv_data$log_PDR.Peak.div.Peak.Time[i] <- NA
        conv_data$CPDR.at.Peak.Time[i] <- NA
    } else {
        conv_data$PDR.Peak[i] <- max(pdr_local)
        conv_data$Peak.Time[i] <- seq(0,Max_time,0.1)[which.max(pdr_local)] #Minutes
        conv_data$PDR.Peak.div.Peak.Time[i] <- conv_data$PDR.Peak[i]/conv_data$Peak.Time[i]
        conv_data$log_PDR.Peak.div.Peak.Time[i] <- log(conv_data$PDR.Peak.div.Peak.Time[i])
        conv_data$CPDR.at.Peak.Time[i] <- integrate(integrand,0,conv_data$Peak.Time[i],stop.on.error = FALSE)["value"][[1]]
    }
}
conv_data$parameters_peak_time <- conv_data$tD_Estimate+conv_data$Tmax_Estimate

rm(L_samples,names_t_final,pdr_local,i,j,Ncol,Ncol_new,Ncol_new2,Ncol_new3)

#Merge & Build dataset ####
dataset <- merge(fulldata_old,fulldata_new,all=T) 
for (i in 5:ncol(dataset))
{
    if (class(dataset[,i])=="character")
    {
        dataset[,i] <- as.numeric(dataset[,i])
    }
}

#Transofrmations Additions
dataset$log_Platelets <- log(dataset$Platelets) #transform
dataset$log_Bilirubin <- log(dataset$Bilirubin) #transform
dataset$log_Sodium <- log(dataset$Sodium) #transform
dataset$log_INR <- log(dataset$INR) #transform

conv_data$log_tD_Estimate <- log(conv_data$tD_Estimate) #transform
conv_data$log_tau_Estimate <- log(conv_data$tau_Estimate) #transform
conv_data$log_beta_Estimate <- log(conv_data$beta_Estimate) #transform
conv_data$log_Tmax_Estimate <- log(conv_data$Tmax_Estimate) #transform

#Merge with model featrues
dataset <- merge(dataset,conv_data[,c(1,4,8,12,16,30:ncol(conv_data))],all.x = T,by = "Pt")

#Gil data: Peak information on MBT
if (half_hour_MBT)
{
    dataset$PDR.peak[dataset$Peak.time>30] <- dataset$PDR.30[dataset$Peak.time>30]
    dataset$CPDR.PDRPeakTime[dataset$Peak.time>30] <- dataset$CPDR.30[dataset$Peak.time>30]
    dataset$Peak.time[dataset$Peak.time>30] <- 30
    rm_cols <- grep(c("35|40|45|50|55|60"),names(dataset))
    dataset <- dataset[,-c(rm_cols)]
}

#Complete Inf values becasue of log transofm (they were 0 before...)
if (half_hour_MBT) 
{
    log_index <- 58
} else {
    log_index <- 77
}
for (i in log_index:ncol(dataset))
{
    dataset[which(dataset[,i]==-Inf),i] <- floor(min(dataset[!(dataset[,i]==-Inf),i]))
}

#Fix Outliers / missing values
dataset$Age[which(dataset$Age<30)] <- NA #Age outliers
dataset$Weight[which(dataset$Weight>160)] <- NA #INR outliers
dataset$INR[which(dataset$INR==1.8)] <- NA #INR outliers
dataset$Sodium[which(dataset$Sodium<130)] <- NA #not possible

#Remove outleirs
dataset$NEW.Generation.HCV.Treatment[is.na(dataset$NEW.Generation.HCV.Treatment)] <- ""
dataset$HCC[is.na(dataset$HCC)] <- ""
dataset <- dataset[dataset$NEW.Generation.HCV.Treatment!=1,]
dataset$NEW.Generation.HCV.Treatment <- NULL
dataset$Time.between.enrollement.and.HCV.treatment.completion <- NULL
dataset <- dataset[dataset$HCC!=1,]
dataset$HCC <- NULL
dataset <- dataset[dataset$Excluded.from.Analysis!=1,]
dataset$Excluded.from.Analysis <- NULL
dataset$Abnormal.Physical.Findings <- factor(dataset$Abnormal.Physical.Findings)
dataset$Ascites <- NULL
dataset$Encephalopathy <- NULL
dataset$CSPH <- factor(dataset$CSPH)
dataset$Etiology <- factor(dataset$Etiology)
dataset$Gender.M.F <- factor(dataset$Gender.M.F)
dataset <- dataset[,colSums(is.na(dataset))<(nrow(dataset)*0.25)] #Remove columns from dataframe where NA values are 25% or more
dataset$Abnormal.Physical.Findings <- NULL
dataset$Presence.of.Varices..EGD. <- NULL

#Change factors to numeric
for (i in 1:ncol(dataset))
{
    if(class(dataset[,i])=="factor" & length(levels(dataset[,i]))==2 & names(dataset)[i]!="CSPH")
    {
        dataset[,i] <- as.numeric(dataset[,i])-1
    }
}

#Boxplots
boxplot((dataset[dataset$CSPH==0,c(66:89)]),col=rgb(1,0,0,0.5),xaxt ="n",main="MHR",xlab="Minues"); axis(1,at=1:length(t_final[-1]),labels=t_final[-1])
boxplot((dataset[dataset$CSPH==1,c(66:89)]),col=rgb(0,0,1,0.5),xaxt ="n",add=T)

boxplot((dataset[dataset$CSPH==0 & dataset$Gender.M.F==0,c(66:89)]),col=rgb(1,0,0,0.5),xaxt ="n",main="MHR Females",xlab="Minues"); axis(1,at=1:length(t_final[-1]),labels=t_final[-1])
boxplot((dataset[dataset$CSPH==1 & dataset$Gender.M.F==0,c(66:89)]),col=rgb(0,0,1,0.5),xaxt ="n",add=T)

boxplot((dataset[dataset$CSPH==0 & dataset$Gender.M.F==1,c(66:89)]),col=rgb(1,0,0,0.5),xaxt ="n",main="MHR Males",xlab="Minues"); axis(1,at=1:length(t_final[-1]),labels=t_final[-1])
boxplot((dataset[dataset$CSPH==1 & dataset$Gender.M.F==1,c(66:89)]),col=rgb(0,0,1,0.5),xaxt ="n",add=T)

boxplot((dataset[dataset$CSPH==0,"Platelets"]),col=rgb(1,0,0,0.5),main="Platelets")
boxplot((dataset[dataset$CSPH==1,"Platelets"]),col=rgb(0,0,1,0.5),add=T)

boxplot((dataset[dataset$CSPH==0 & dataset$Gender.M.F==0,"Platelets"]),col=rgb(1,0,0,0.5),main="Platelets Females")
boxplot((dataset[dataset$CSPH==1 & dataset$Gender.M.F==0,"Platelets"]),col=rgb(0,0,1,0.5),add=T)

boxplot((dataset[dataset$CSPH==0 & dataset$Gender.M.F==1,"Platelets"]),col=rgb(1,0,0,0.5),main="Platelets Males")
boxplot((dataset[dataset$CSPH==1 & dataset$Gender.M.F==1,"Platelets"]),col=rgb(0,0,1,0.5),add=T)

if (max_num_of_featrues!=1) #if we are going to 2 features we must reduce the number of features (inc interactions) to 500 or so..
{
    dataset <- dataset[,-c(grep(pattern = "_[0-9]+",x = names(dataset)))]
}

#Add the interactions features
rm_cols <- grep("HVPG|CSPH",names(dataset))
dataset2 <- as.data.frame(dataset[,-c(1,4,rm_cols,grep(pattern = "_[0-9]+",x = names(dataset)))]^(-1))
for (i in 1:ncol(dataset2))
{
    dataset2[which(dataset2[,i]==Inf),i] <- 0
}
names(dataset2) <- paste0(names(dataset2),"_inv")
dataset2 <- cbind(dataset[,-c(1,4,rm_cols,grep(pattern = "_[0-9]+",x = names(dataset)))],dataset2)
temp_names <- names(dataset2)
if (max_num_of_featrues==1) #if we are going to 2 features we must reduce the number of features (inc interactions) to 500 or so..
{
    dataset2 <- as.data.frame(t(apply(dataset2,1,combn,2,prod)))
    colnames(dataset2) <- combn(temp_names,2,paste,collapse=":")
    Ncol <- ncol(dataset2)
    dataset2 <- cbind(dataset2,log(dataset2))
    names(dataset2)[(Ncol+1):ncol(dataset2)] <- paste0("log_",names(dataset2)[1:Ncol]) #log of the original PDR valeus
}
Ncol <- ncol(dataset2)
dataset2 <- cbind(dataset2,dataset2*dataset$Peak.time,dataset2/dataset$Peak.time)
names(dataset2)[(Ncol+1):(2*Ncol)] <- paste0(names(dataset2)[1:Ncol],":Peak.time") #log of the original PDR valeus
names(dataset2)[(2*Ncol+1):ncol(dataset2)] <- paste0(names(dataset2)[1:Ncol],":Peak.time_inv") #log of the original PDR valeus
Ncol <- ncol(dataset2)
dataset2 <- cbind(dataset2,dataset2*dataset$CPDR.at.Peak.Time)
names(dataset2)[(Ncol+1):ncol(dataset2)] <- paste0(names(dataset2)[1:Ncol],":CPDR.at.Peak.Time") #log of the original PDR valeus
#Complete Inf values becasue of log transofm (they were 0 before...)
for (i in 1:ncol(dataset2)) #takes too long...
{
    dataset2[which(dataset2[,i]==-Inf),i] <- floor(min(dataset2[!(dataset2[,i]==-Inf),i]))
    dataset2[which(dataset2[,i]==Inf),i] <- 0 #if it doesnt exist
    dataset2[which(dataset2[,i]==-Inf),i] <- 0 #if it doesnt exist
}

dataset <- cbind(dataset,dataset2[,c(72:ncol(dataset2))])
dataset <- dataset[,colSums(is.na(dataset))<(nrow(dataset)*0.25)] #Remove columns from dataframe where NA values are 25% or more

rm(conv_data,dataset_old,fitted_data,fulldata,fulldata_old,noise_commulative,dataset2,temp_names,Ncol,Max_time,log_index,rm_cols,i)

dataset_orig <- dataset #save a copy

#Logistic regression Training ####
if (model_training==1)
{
    #Dataset according to etiology selection
    dataset <- dataset_orig
    dataset$Etiology <- as.character(dataset$Etiology)
    dataset$Etiology[dataset$Etiology!="HCV"&dataset$Etiology!="NASH"] <- "Other"
    dataset$Etiology <- factor(dataset$Etiology)
    #if (only_hcv) dataset <- dataset[dataset$Etiology=="HCV",]
    #dataset$Etiology <- NULL
    
    #all combinations of featreus to maximize the train AUC (do parallel)
    if (file.exists(paste0(feature_selection_path,"exahstive_search_auc.csv")))
    {
        exahstive_search_auc <- read.csv(paste0(feature_selection_path,"exahstive_search_auc.csv"),stringsAsFactors = F)
    } else {
        exahstive_search_auc <- data.frame(Features=character(),AUC=numeric(),AUC_FEMALE=numeric(),AUC_MALE=numeric(),NUM_feat=numeric(),NUM_pats=numeric(),AIC=numeric(),Mean_Pval=numeric())
        for (t in 1:max_num_of_featrues)
        {
            cat(t,"\n")
            rm_cols <- grep("HVPG|CSPH|Etiology|Gender.M.F",names(dataset))
            feats_comb <- combn(names(dataset[,-c(1,rm_cols)]),t)
            cl <- makePSOCKcluster(12)
            registerDoParallel(cl)
            r <- foreach(j=1:ncol(feats_comb),.combine=rbind,.packages = c("pROC","OptimalCutpoints"),.verbose = T,.inorder=FALSE) %dopar% 
            {
                cat(j,"\n")
                dataset_tmp <- dataset[,c(feats_comb[,j],"CSPH","Gender.M.F")]
                dataset_tmp <- dataset_tmp[complete.cases(dataset_tmp),]
                if (t>1) {fixed_column_not_exist <- (sum(apply(dataset_tmp[,-c(ncol(dataset_tmp),ncol(dataset_tmp)-1)],2,var,na.rm=TRUE)!=0)==t)} #check for fixed columns - don't run then...
                if (t==1) {fixed_column_not_exist <- (var(dataset_tmp[,-c(ncol(dataset_tmp),ncol(dataset_tmp)-1)],na.rm=TRUE)!=0)}
                if (nrow(dataset_tmp)>=100 & fixed_column_not_exist) {
                    #Weighting
                    weights_classes_tmp <- rep(1,nrow(dataset_tmp))
                    weights_classes_tmp[dataset_tmp$CSPH==0] <- round(sum(dataset_tmp$CSPH==1)/sum(dataset_tmp$CSPH==0))
                    
                    #Change factors to numeric
                    for (i in 1:(ncol(dataset_tmp)-2))
                    {
                        if(class(dataset_tmp[,i])=="factor" & length(levels(dataset_tmp[,i]))==2 & names(dataset_tmp)[i]!="CSPH")
                        {
                            dataset_tmp[,i] <- as.numeric(dataset_tmp[,i])-1
                        }
                    }
                    
                    fit <- glm(CSPH~.,data=dataset_tmp[,-ncol(dataset_tmp)],family=binomial(),weights = weights_classes_tmp)
                    pred <- predict(fit,newdata=dataset_tmp,type="response")
                    auc_train <- ci.auc(dataset_tmp$CSPH,pred) #delong
                    
                    cutpoint <- optimal.cutpoints(X="fitted",
                                                  status = "CSPH",
                                                  data=data.frame(CSPH=as.numeric(as.character(dataset_tmp$CSPH)),fitted=pred,gender=as.factor(dataset_tmp$Gender.M.F)),
                                                  methods=c("MaxSpSe"),
                                                  ci.fit = TRUE,
                                                  categorical.cov = "gender",
                                                  tag.healthy = 0)
                    auc_females <- cutpoint$MaxSpSe$`0`$measures.acc$AUC[1]
                    auc_males <- cutpoint$MaxSpSe$`1`$measures.acc$AUC[1]
                    
                    new_data <- data.frame(Features=paste0(feats_comb[,j],collapse=','),AUC=round(auc_train[2],3),AUC_FEMALE=round(auc_females,3),AUC_MALE=round(auc_males,3),NUM_feat=length(feats_comb[,j]),NUM_pats=nrow(dataset_tmp),AIC=AIC(fit),Mean_Pval=mean(summary(fit)$coefficients[,4]))
                    new_data$Features <- as.character(new_data$Features)
                    time_strings <- unique(na.omit(as.numeric(unlist(strsplit(unlist(new_data$Features),"[^0-9]+")))))
                    if (length(time_strings)>0) {new_data$Max_Time <- max(time_strings)
                    } else {new_data$Max_Time <- 0}
                    new_data$Is_Peak <- length(grep("Peak",new_data$Features,ignore.case = T))|length(grep("Estimate",new_data$Features,ignore.case = T))
                    new_data
                }
            }
            stopCluster(cl)
            exahstive_search_auc <- rbind(exahstive_search_auc,r)
        }
        rm(r,feats_comb)
        #Add info of blood or not
        exahstive_search_auc$Is_Blood <- FALSE
        for (blood_feat in c("Platelets","INR","ALT","AST","GGT","ALP","Bilirubin","Creatinine","Albumin","CTP","MELD","Sodium")) {exahstive_search_auc$Is_Blood[grep(blood_feat,as.character(exahstive_search_auc$Features))] <- TRUE}
        exahstive_search_auc$Is_Interaction <- FALSE
        exahstive_search_auc$Is_Interaction[grep(":",as.character(exahstive_search_auc$Features))] <- TRUE
        exahstive_search_auc <- exahstive_search_auc[order(-exahstive_search_auc$AUC,exahstive_search_auc$AIC),] #sort it
        write.csv(exahstive_search_auc,"exahstive_search_auc.csv",row.names = F)
    }
    
    #Find best set of features
    if (featrues_type==0) {optimzie_featrues <- strsplit(exahstive_search_auc$Features[which.max(exahstive_search_auc$AUC)],",")[[1]]} #all featrues
    if (featrues_type==1) {optimzie_featrues <- strsplit(exahstive_search_auc$Features[exahstive_search_auc$Is_Blood==1 & exahstive_search_auc$Is_Peak==0 & exahstive_search_auc$Max_Time==0][which.max(exahstive_search_auc$AUC[exahstive_search_auc$Is_Blood==1 & exahstive_search_auc$Is_Peak==0 & exahstive_search_auc$Max_Time==0])],",")[[1]]} #Blood+demog only featrues
    if (featrues_type==2) {optimzie_featrues <- strsplit(exahstive_search_auc$Features[exahstive_search_auc$Is_Blood==0][which.max(exahstive_search_auc$AUC[exahstive_search_auc$Is_Blood==0])],",")[[1]]} #MBT+demog only featrues
    if (featrues_type==3) {optimzie_featrues <- strsplit(exahstive_search_auc$Features[exahstive_search_auc$Is_Interaction==0][which.max(exahstive_search_auc$AUC[exahstive_search_auc$Is_Interaction==0])],",")[[1]]} #all featrues - no interactions
    if (featrues_type==4) {optimzie_featrues <- strsplit(exahstive_search_auc$Features[exahstive_search_auc$Is_Blood==1 & exahstive_search_auc$Is_Peak==0 & exahstive_search_auc$Max_Time==0 & exahstive_search_auc$Is_Interaction==0][which.max(exahstive_search_auc$AUC[exahstive_search_auc$Is_Blood==1 & exahstive_search_auc$Is_Peak==0 & exahstive_search_auc$Max_Time==0 & exahstive_search_auc$Is_Interaction==0])],",")[[1]]} #Blood+demog only featrues - no interactions
    if (featrues_type==5) {optimzie_featrues <- strsplit(exahstive_search_auc$Features[exahstive_search_auc$Is_Blood==0 & exahstive_search_auc$Is_Interaction==0][which.max(exahstive_search_auc$AUC[exahstive_search_auc$Is_Blood==0 & exahstive_search_auc$Is_Interaction==0])],",")[[1]]} #MBT+demog only featrues - no interactions
    dataset <- dataset[,c("Pt",optimzie_featrues,"CSPH","Gender.M.F","Etiology")]
    dataset <- dataset[complete.cases(dataset),]
    names(dataset)[2] <- "a" #for the case of interactions
    if (max_num_of_featrues==2) names(dataset)[3] <- "b"
    testing <- dataset #this dataset will be used for the validation!
    
    #Weighting
    weights_classes <- rep(1,nrow(dataset))
    weights_classes[dataset$CSPH==0] <- round(sum(dataset$CSPH==1)/sum(dataset$CSPH==0))
    
    #Change factors to numeric
    for (i in 1:(which(names(dataset)=="CSPH")-1))
    {
        if(class(dataset[,i])=="factor" & length(levels(dataset[,i]))==2 & names(dataset)[i]!="CSPH")
        {
            dataset[,i] <- as.numeric(dataset[,i])-1
        }
    }
    
    #Fit of final model - LOGISTIC REGRESSION
    f <- as.simple.formula(names(dataset)[2:(which(names(dataset)=="CSPH")-1)],"dataset$CSPH")
    final_model <- glm(f,data=dataset,family=binomial(),weights = weights_classes)
    sink("Model.txt")
    optimzie_featrues
    table(dataset$CSPH)
    summary(final_model) # display results
    confint(final_model) # 95% CI for the coefficients
    anova(final_model, test="Chisq")
    sink()

    #Cut off optimization of training results
    #AUC calculation + CI
    sink("Statistics.txt")
    cat("Training Statistics\n")
    cat("Number of Patients: ",nrow(dataset),"\n"); 
    cat("Number of Male Patients: ",nrow(dataset[dataset$Gender.M.F==1,]),"\n"); 
    cat("Number of Female Patients: ",nrow(dataset[dataset$Gender.M.F==0,]),"\n"); 
    cat("Gender/Etiology Distribution: \n"); 
    table(dataset$Gender.M.F,dataset$Etiology)
    
    cutpoint0 <- optimal.cutpoints(X="fitted",
                                   status = "CSPH",
                                   data=data.frame(CSPH=as.numeric(as.character(dataset$CSPH)),fitted=final_model$fitted,Gender_Etiology=as.factor(paste0(dataset$Gender.M.F,dataset$Etiology))),
                                   methods=c("MaxSpSe"),
                                   ci.fit = TRUE,
                                   categorical.cov = c("Gender_Etiology"),
                                   tag.healthy = 0)
    #print(cutpoint0)
    #summary(cutpoint0)
    
    cutpoint1 <- optimal.cutpoints(X="fitted",
                                   status = "CSPH",
                                   data=data.frame(CSPH=as.numeric(as.character(dataset$CSPH)),fitted=final_model$fitted,gender=as.factor(dataset$Gender.M.F),Etiology=dataset$Etiology),
                                   methods=c("MaxSpSe"),
                                   ci.fit = TRUE,
                                   categorical.cov = c("gender"),
                                   tag.healthy = 0)
    print(cutpoint1)
    #summary(cutpoint1)
    
    cat("***************************\n")
    cat("All\n")
    cat("***************************\n")
    dat <- NULL
    dat[[1]] <- table(cutpoint1$data$CSPH[cutpoint1$data$gender==0],cutpoint1$data$fitted[cutpoint1$data$gender==0]>tail(cutpoint1$MaxSpSe$`0`$optimal.cutoff$cutoff,1)) #NEW CALCULATION FOR SP AND SE
    dat[[2]] <- table(cutpoint1$data$CSPH[cutpoint1$data$gender==1],cutpoint1$data$fitted[cutpoint1$data$gender==1]>tail(cutpoint1$MaxSpSe$`1`$optimal.cutoff$cutoff,1)) #NEW CALCULATION FOR SP AND SE
    dat <- dat[[1]]+dat[[2]]
    diag(dat) <- rev(diag(dat)) #NEW CALCULATION FOR SP AND SE
    print(epi.tests(dat, conf.level = 0.95)) #NEW CALCULATION FOR SP AND SE

    cat("***************************\n")
    cat("HCV\n")
    cat("***************************\n")
    dat <- NULL
    dat[[1]] <- table(cutpoint1$data$CSPH[cutpoint1$data$gender==0 & dataset$Etiology=="HCV"],cutpoint1$data$fitted[cutpoint1$data$gender==0 & dataset$Etiology=="HCV"]>tail(cutpoint1$MaxSpSe$`0`$optimal.cutoff$cutoff,1)) #NEW CALCULATION FOR SP AND SE
    dat[[2]] <- table(cutpoint1$data$CSPH[cutpoint1$data$gender==1 & dataset$Etiology=="HCV"],cutpoint1$data$fitted[cutpoint1$data$gender==1 & dataset$Etiology=="HCV"]>tail(cutpoint1$MaxSpSe$`1`$optimal.cutoff$cutoff,1)) #NEW CALCULATION FOR SP AND SE
    dat <- dat[[1]]+dat[[2]]
    diag(dat) <- rev(diag(dat)) #NEW CALCULATION FOR SP AND SE
    print(epi.tests(dat, conf.level = 0.95)) #NEW CALCULATION FOR SP AND SE
    
    cat("***************************\n")
    cat("NASH\n")
    cat("***************************\n")
    dat <- NULL
    dat[[1]] <- table(cutpoint1$data$CSPH[cutpoint1$data$gender==0 & dataset$Etiology=="NASH"],cutpoint1$data$fitted[cutpoint1$data$gender==0 & dataset$Etiology=="NASH"]>tail(cutpoint1$MaxSpSe$`0`$optimal.cutoff$cutoff,1)) #NEW CALCULATION FOR SP AND SE
    dat[[2]] <- table(cutpoint1$data$CSPH[cutpoint1$data$gender==1 & dataset$Etiology=="NASH"],cutpoint1$data$fitted[cutpoint1$data$gender==1 & dataset$Etiology=="NASH"]>tail(cutpoint1$MaxSpSe$`1`$optimal.cutoff$cutoff,1)) #NEW CALCULATION FOR SP AND SE
    dat <- dat[[1]]+dat[[2]]
    diag(dat) <- rev(diag(dat)) #NEW CALCULATION FOR SP AND SE
    print(epi.tests(dat, conf.level = 0.95)) #NEW CALCULATION FOR SP AND SE
    
    cat("***************************\n")
    cat("Other\n")
    cat("***************************\n")
    dat <- NULL
    dat[[1]] <- table(cutpoint1$data$CSPH[cutpoint1$data$gender==0 & dataset$Etiology=="Other"],cutpoint1$data$fitted[cutpoint1$data$gender==0 & dataset$Etiology=="Other"]>tail(cutpoint1$MaxSpSe$`0`$optimal.cutoff$cutoff,1)) #NEW CALCULATION FOR SP AND SE
    dat[[2]] <- table(cutpoint1$data$CSPH[cutpoint1$data$gender==1 & dataset$Etiology=="Other"],cutpoint1$data$fitted[cutpoint1$data$gender==1 & dataset$Etiology=="Other"]>tail(cutpoint1$MaxSpSe$`1`$optimal.cutoff$cutoff,1)) #NEW CALCULATION FOR SP AND SE
    dat <- dat[[1]]+dat[[2]]
    diag(dat) <- rev(diag(dat)) #NEW CALCULATION FOR SP AND SE
    print(epi.tests(dat, conf.level = 0.95)) #NEW CALCULATION FOR SP AND SE
    
    #With grey zones
    cat("Optimized Grey Zones Statistics- Training:\n")
    optimized_grey_zone_tot <- NULL
    dat <- list()
    for (j in c(0:1))
    {
        grids <- expand.grid(top=seq(0.99,0.01,-0.01),low=seq(0.01,0.99,0.01))
        auc_grey_zones <- rep(0,nrow(grids))
        low_auc_ci <- rep(0,nrow(grids))
        high_auc_ci <- rep(0,nrow(grids))
        unknown_grey_zones <- rep(0,nrow(grids))
        if (j==0) grids$opt_cut_off <- tail(cutpoint1$MaxSpSe$`0`$optimal.cutoff$cutoff,1)
        if (j==1) grids$opt_cut_off <- tail(cutpoint1$MaxSpSe$`1`$optimal.cutoff$cutoff,1)
        for (i in 1:nrow(grids))
        {
            if (grids$top[i]>grids$low[i] & grids$top[i]>=grids$opt_cut_off[i] & grids$low[i]<=grids$opt_cut_off[i])
            {
                selection <- (final_model$fitted>grids$top[i] | final_model$fitted<=grids$low[i]) & (dataset$Gender.M.F==j)
                if (length(unique(dataset$CSPH[selection]))>1)
                {
                    auc_ci <- ci.auc(dataset$CSPH[selection],final_model$fitted[selection]) #delong
                    low_auc_ci[i] <- round(auc_ci[1],3)
                    auc_grey_zones[i] <- round(auc_ci[2],3)
                    high_auc_ci[i] <- round(auc_ci[3],3)
                    unknown_grey_zones[i] <- nrow(dataset[dataset$Gender.M.F==j,])-length(final_model$fitted[selection])
                }
            }
        }
        grids$gender <- j
        grids$auc <- auc_grey_zones
        grids$low_auc_ci <- low_auc_ci 
        grids$high_auc_ci <- high_auc_ci
        grids$unknowns <- unknown_grey_zones
        grids <- grids[grids$auc!=0,]
        grids <- grids[!is.na(grids$auc),]
        threshold_patietns_number <- nrow(dataset[dataset$Gender.M.F==j,])*0.15 #max of 15% of optimized grey zones
        
        optimized_grey_zone <- grids[grids$unknowns<=threshold_patietns_number,][which.max(grids$auc[grids$unknowns<=threshold_patietns_number]),]
        pr <- final_model$fitted[(final_model$fitted>optimized_grey_zone$top | final_model$fitted<optimized_grey_zone$low) & (dataset$Gender.M.F==j)]
        labels_csph <- dataset$CSPH[(final_model$fitted>optimized_grey_zone$top | final_model$fitted<optimized_grey_zone$low) & (dataset$Gender.M.F==j)]
        Etiology <- dataset$Etiology[(final_model$fitted>optimized_grey_zone$top | final_model$fitted<optimized_grey_zone$low) & (dataset$Gender.M.F==j)]
        dat[[j+1]] <- list()
        dat[[j+1]][[1]] <- table(labels_csph,pr>optimized_grey_zone$opt_cut_off) #NEW CALCULATION FOR SP AND SE
        dat[[j+1]][[2]] <- table(labels_csph[Etiology=="HCV"],pr[Etiology=="HCV"]>optimized_grey_zone$opt_cut_off) #NEW CALCULATION FOR SP AND SE
        dat[[j+1]][[3]] <- table(labels_csph[Etiology=="NASH"],pr[Etiology=="NASH"]>optimized_grey_zone$opt_cut_off) #NEW CALCULATION FOR SP AND SE
        dat[[j+1]][[4]] <- table(labels_csph[Etiology=="Other"],pr[Etiology=="Other"]>optimized_grey_zone$opt_cut_off) #NEW CALCULATION FOR SP AND SE
        
        optimized_grey_zone_tot <- rbind(optimized_grey_zone_tot,optimized_grey_zone)
    }
    optimized_grey_zone_tot
    cat("***************************\n")
    cat("All\n")
    cat("***************************\n")
    dat_tmp <- dat[[1]][[1]]+dat[[2]][[1]]
    diag(dat_tmp) <- rev(diag(dat_tmp)) #NEW CALCULATION FOR SP AND SE
    print(epi.tests(dat_tmp, conf.level = 0.95)) #NEW CALCULATION FOR SP AND SE
    
    cat("***************************\n")
    cat("HCV\n")
    cat("***************************\n")
    dat_tmp <- dat[[1]][[2]]+dat[[2]][[2]]
    diag(dat_tmp) <- rev(diag(dat_tmp)) #NEW CALCULATION FOR SP AND SE
    print(epi.tests(dat_tmp, conf.level = 0.95)) #NEW CALCULATION FOR SP AND SE
    
    cat("***************************\n")
    cat("NASH\n")
    cat("***************************\n")
    dat_tmp <- dat[[1]][[3]]+dat[[2]][[3]]
    diag(dat_tmp) <- rev(diag(dat_tmp)) #NEW CALCULATION FOR SP AND SE
    print(epi.tests(dat_tmp, conf.level = 0.95)) #NEW CALCULATION FOR SP AND SE
    
    cat("***************************\n")
    cat("Other\n")
    cat("***************************\n")
    dat_tmp <- dat[[1]][[4]]+dat[[2]][[4]]
    diag(dat_tmp) <- rev(diag(dat_tmp)) #NEW CALCULATION FOR SP AND SE
    print(epi.tests(dat_tmp, conf.level = 0.95)) #NEW CALCULATION FOR SP AND SE
    
    #Save stuff
    save(final_model,file="regression_models.RData")
    write.csv(optimized_grey_zone_tot,"grey_zone_check.csv",row.names = F)
    
    #3 multifolds CV check
    Times <- 5
    K <- 3
    folds <- createMultiFolds(testing$CSPH,k = K,times = Times)
    auc <- list(rep(NA,Times),rep(NA,Times)); auc_low <- list(rep(NA,Times),rep(NA,Times)); auc_high <- list(rep(NA,Times),rep(NA,Times))
    se <- rep(NA,Times); se_low <- rep(NA,Times); se_high <- rep(NA,Times)
    sp <- rep(NA,Times); sp_low <- rep(NA,Times); sp_high <- rep(NA,Times)
    auc_gz <- list(rep(NA,Times),rep(NA,Times)); auc_low_gz <- list(rep(NA,Times),rep(NA,Times)); auc_high_gz <- list(rep(NA,Times),rep(NA,Times))
    se_gz <- rep(NA,Times); se_low_gz <- rep(NA,Times); se_high_gz <- rep(NA,Times)
    sp_gz <- rep(NA,Times); sp_low_gz <- rep(NA,Times); sp_high_gz <- rep(NA,Times)
    for (p in 1:Times)
    {
        #Trainign and testing K fold
        pred <- NULL
        auc_fold <- list(rep(NA,K),rep(NA,K)); auc_low_fold <- list(rep(NA,K),rep(NA,K)); auc_high_fold <- list(rep(NA,K),rep(NA,K))
        auc_fold_gz <- list(rep(NA,K),rep(NA,K)); auc_low_fold_gz <- list(rep(NA,K),rep(NA,K)); auc_high_fold_gz <- list(rep(NA,K),rep(NA,K))
        se_fold <- rep(NA,K); se_low_fold <- rep(NA,K); se_high_fold <- rep(NA,K)
        sp_fold <- rep(NA,K); sp_low_fold <- rep(NA,K); sp_high_fold <- rep(NA,K)
        se_fold_gz <- rep(NA,K); se_low_fold_gz <- rep(NA,K); se_high_fold_gz <- rep(NA,K)
        sp_fold_gz <- rep(NA,K); sp_low_fold_gz <- rep(NA,K); sp_high_fold_gz <- rep(NA,K)
        for (k in 1:K)
        {
            validset <- testing[folds[[k+(p-1)*K]],]

            #Weighting
            weights_classes_validset <- rep(1,nrow(validset))
            weights_classes_validset[validset$CSPH==0] <- round(sum(validset$CSPH==1)/sum(validset$CSPH==0))

            #Change factors to numeric
            for (i in 1:(which(names(dataset)=="CSPH")-1))
            {
                if(class(validset[,i])=="factor" & length(levels(validset[,i]))==2 & names(validset)[i]!="CSPH")
                {
                    validset[,i] <- as.numeric(validset[,i])-1
                }
            }
            
            #Training
            f <- as.simple.formula(names(validset)[2:(which(names(dataset)=="CSPH")-1)],"validset$CSPH")
            fit <- glm(f,data=validset,family=binomial(),weights = weights_classes_validset)
            
            #Testing on the set the left aside
            testset <- testing[-folds[[k+(p-1)*K]],]

            pred_fold <- predict.glm(fit,testset,"response")
            pred <- c(pred,pred_fold)
            
            #Cut off optimization of training results
            cutpoint1 <- optimal.cutpoints(X="fitted",
                                           status = "CSPH",
                                           data=data.frame(CSPH=as.numeric(as.character(validset$CSPH)),fitted=fit$fitted,gender=as.factor(validset$Gender.M.F)),
                                           methods=c("MaxSpSe"),
                                           ci.fit = TRUE,
                                           categorical.cov = "gender",
                                           tag.healthy = 0)
            
            dat <- NULL
            dat[[1]] <- table(testset$CSPH[testset$Gender.M.F==0],factor(pred_fold[testset$Gender.M.F==0]>tail(cutpoint1$MaxSpSe$`0`$optimal.cutoff$cutoff,1),levels = c("FALSE","TRUE"))) #NEW CALCULATION FOR SP AND SE
            dat[[2]] <- table(testset$CSPH[testset$Gender.M.F==1],factor(pred_fold[testset$Gender.M.F==1]>tail(cutpoint1$MaxSpSe$`1`$optimal.cutoff$cutoff,1),levels = c("FALSE","TRUE"))) #NEW CALCULATION FOR SP AND SE
            dat <- dat[[1]]+dat[[2]]
            diag(dat) <- rev(diag(dat))
            stats <- epi.tests(dat, conf.level = 0.95)
            se_fold[k] <- stats$rval$se[1][[1]]; sp_fold[k] <- stats$rval$sp[1][[1]]
            se_low_fold[k] <- stats$rval$se[2][[1]]; sp_low_fold[k] <- stats$rval$sp[2][[1]]
            se_high_fold[k] <- stats$rval$se[3][[1]]; sp_high_fold[k] <- stats$rval$sp[3][[1]]
            
            auc_fold[[1]][k] <- ci.auc(testset$CSPH[testset$Gender.M.F==0],pred_fold[testset$Gender.M.F==0])[2]
            auc_low_fold[[1]][k] <- ci.auc(testset$CSPH[testset$Gender.M.F==0],pred_fold[testset$Gender.M.F==0])[1]
            auc_high_fold[[1]][k] <- ci.auc(testset$CSPH[testset$Gender.M.F==0],pred_fold[testset$Gender.M.F==0])[3]
            auc_fold[[2]][k] <- ci.auc(testset$CSPH[testset$Gender.M.F==1],pred_fold[testset$Gender.M.F==1])[2]
            auc_low_fold[[2]][k] <- ci.auc(testset$CSPH[testset$Gender.M.F==1],pred_fold[testset$Gender.M.F==1])[1]
            auc_high_fold[[2]][k] <- ci.auc(testset$CSPH[testset$Gender.M.F==1],pred_fold[testset$Gender.M.F==1])[3]
            
            #With grey zones
            optimized_grey_zone_tot <- NULL
            dat_gz <- NULL
            for (j in c(0:1))
            {
                grids <- expand.grid(top=seq(0.99,0.01,-0.01),low=seq(0.01,0.99,0.01))
                auc_grey_zones <- rep(0,nrow(grids))
                low_auc_ci <- rep(0,nrow(grids))
                high_auc_ci <- rep(0,nrow(grids))
                unknown_grey_zones <- rep(0,nrow(grids))
                if (j==0) grids$opt_cut_off <- tail(cutpoint1$MaxSpSe$`0`$optimal.cutoff$cutoff,1)
                if (j==1) grids$opt_cut_off <- tail(cutpoint1$MaxSpSe$`1`$optimal.cutoff$cutoff,1)
                for (i in 1:nrow(grids))
                {
                    if (grids$top[i]>grids$low[i] & grids$top[i]>=grids$opt_cut_off[i] & grids$low[i]<=grids$opt_cut_off[i])
                    {
                        selection <- (fit$fitted.values>grids$top[i] | fit$fitted.values<=grids$low[i]) & (validset$Gender.M.F==j)
                        if (length(unique(validset$CSPH[selection]))>1)
                        {
                            auc_ci <- ci.auc(validset$CSPH[selection],fit$fitted.values[selection]) #delong
                            low_auc_ci[i] <- round(auc_ci[1],3)
                            auc_grey_zones[i] <- round(auc_ci[2],3)
                            high_auc_ci[i] <- round(auc_ci[3],3)
                            unknown_grey_zones[i] <- nrow(validset[validset$Gender.M.F==j,])-length(fit$fitted.values[selection])
                        }
                    }
                }
                grids$gender <- j
                grids$auc <- auc_grey_zones
                grids$low_auc_ci <- low_auc_ci 
                grids$high_auc_ci <- high_auc_ci
                grids$unknowns <- unknown_grey_zones
                grids <- grids[grids$auc!=0,]
                grids <- grids[!is.na(grids$auc),]
                threshold_patietns_number <- nrow(validset[validset$Gender.M.F==j,])*0.15 #max of 15% of optimized grey zones
                
                optimized_grey_zone <- grids[grids$unknowns<=threshold_patietns_number,][which.max(grids$auc[grids$unknowns<=threshold_patietns_number]),]
                optimized_grey_zone_tot <- rbind(optimized_grey_zone_tot,optimized_grey_zone)
                
                pr <- pred_fold[(pred_fold>optimized_grey_zone$top | pred_fold<optimized_grey_zone$low) & (testset$Gender.M.F==j)]
                labels_csph <- testset$CSPH[(pred_fold>optimized_grey_zone$top | pred_fold<optimized_grey_zone$low) & (testset$Gender.M.F==j)]
                dat_gz[[j+1]] <- table(labels_csph,factor(pr>optimized_grey_zone$opt_cut_off,levels = c("FALSE","TRUE"))) #NEW CALCULATION FOR SP AND SE
                
                selection <- (pred_fold>optimized_grey_zone$top | pred_fold<=optimized_grey_zone$low) & (testset$Gender.M.F==j)
                auc_ci <- ci.auc(testset$CSPH[selection],pred_fold[selection])
                auc_fold_gz[[j+1]][k] <- round(auc_ci[2],3)
                auc_low_fold_gz[[j+1]][k] <- round(auc_ci[1],3)
                auc_high_fold_gz[[j+1]][k] <- round(auc_ci[3],3)
            }
            dat_gz <- dat_gz[[1]]+dat_gz[[2]]
            diag(dat_gz) <- rev(diag(dat_gz))
            stats <- epi.tests(dat_gz, conf.level = 0.95)
            se_fold_gz[k] <- stats$rval$se[1][[1]]; sp_fold_gz[k] <- stats$rval$sp[1][[1]]
            se_low_fold_gz[k] <- stats$rval$se[2][[1]]; sp_low_fold_gz[k] <- stats$rval$sp[2][[1]]
            se_high_fold_gz[k] <- stats$rval$se[3][[1]]; sp_high_fold_gz[k] <- stats$rval$sp[3][[1]]
        }
        pred <- pred[order(as.numeric(names(pred)))] #order again
        
        se[p] <- mean(se_fold); sp[p] <- mean(sp_fold)
        se_low[p] <- mean(se_low_fold); sp_low[p] <- mean(sp_low_fold)
        se_high[p] <- mean(se_high_fold); sp_high[p] <- mean(sp_high_fold)
        
        auc[[1]][p] <- lapply(auc_fold,mean)[[1]][1]
        auc_low[[1]][p] <- lapply(auc_low_fold,mean)[[1]][1]
        auc_high[[1]][p] <- lapply(auc_high_fold,mean)[[1]][1]
        auc[[2]][p] <- lapply(auc_fold,mean)[[2]][1]
        auc_low[[2]][p] <- lapply(auc_low_fold,mean)[[2]][1]
        auc_high[[2]][p] <- lapply(auc_high_fold,mean)[[2]][1]
        
        se_gz[p] <- mean(se_fold_gz); sp_gz[p] <- mean(sp_fold_gz)
        se_low_gz[p] <- mean(se_low_fold_gz); sp_low_gz[p] <- mean(sp_low_fold_gz)
        se_high_gz[p] <- mean(se_high_fold_gz); sp_high_gz[p] <- mean(sp_high_fold_gz)
        
        auc_gz[[1]][p] <- lapply(auc_fold_gz,mean,na.rm=T)[[1]][1]
        auc_low_gz[[1]][p] <- lapply(auc_low_fold_gz,mean,na.rm=T)[[1]][1]
        auc_high_gz[[1]][p] <- lapply(auc_high_fold_gz,mean,na.rm=T)[[1]][1]
        auc_gz[[2]][p] <- lapply(auc_fold_gz,mean,na.rm=T)[[2]][1]
        auc_low_gz[[2]][p] <- lapply(auc_low_fold_gz,mean,na.rm=T)[[2]][1]
        auc_high_gz[[2]][p] <- lapply(auc_high_fold_gz,mean,na.rm=T)[[2]][1]
        
        rm(labels_csph,pred,stats,grids,threshold_patietns_number,optimized_grey_zone,opt_cut_off_valid)
    }
    cat("Validation Statistics:\n")
    cat("AUC\n"); round(unlist(lapply(auc,mean,na.rm=T)),3); round(unlist(lapply(auc_low,mean,na.rm=T)),3); round(unlist(lapply(auc_high,mean,na.rm=T)),3);
    cat("Sensitivity\n"); round(mean(se),2); round(mean(se_low,na.rm=T),2); round(mean(se_high,na.rm=T),2); 
    cat("Specificty\n"); round(mean(sp),2); round(mean(sp_low,na.rm=T),2); round(mean(sp_high,na.rm=T),2); 
    
    cat("Optimized Grey Zones Statistics- Validation:\n")
    cat("AUC\n"); round(unlist(lapply(auc_gz,mean,na.rm=T)),3); round(unlist(lapply(auc_low_gz,mean,na.rm=T)),3); round(unlist(lapply(auc_high_gz,mean,na.rm=T)),3);
    cat("Sensitivity\n"); round(mean(se_gz,na.rm=T),2); round(mean(se_low_gz,na.rm=T),2); round(mean(se_high_gz,na.rm=T),2); 
    cat("Specificty\n"); round(mean(sp_gz,na.rm=T),2); round(mean(sp_low_gz,na.rm=T),2); round(mean(sp_high_gz,na.rm=T),2); 

    sink()
    
    rm(grids,acc_cutt_off,auc,auc_ci,auc_grey_zones,auc_high,auc_low,cutt_off,f,fit,final_model,folds,high_auc_ci,K,k,labels_csph,low_auc_ci,opt_cut_off,optimized_grey_zone,auc_train,i,j,t,time_strings,weights_classes_tmp,rm_cols,
       optimzie_featrues,p,pr,pred,se,se_high,se_low,selection,sp,sp_high,sp_low,threshold_patietns_number,Times,unknown_grey_zones,stats,scaling_dataset,blood_feat,testing)
}

#Logistic regression Testing ####
dataset <- dataset_orig #choose the dataset to work with
if (only_hcv) dataset <- dataset[dataset$Etiology=="HCV",]
dataset$Etiology <- NULL

load(paste0(model_path,"regression_models.RData")) #final_model
optimized_grey_zone <- read.csv(paste0(model_path,"grey_zone_check.csv"),stringsAsFactors = F) #load grey zone data
scaling_dataset <- read.csv(paste0(model_path,"scaling_dataset.csv"),stringsAsFactors = F) #load scaling data

optimzie_featrues <- scaling_dataset$feature
dataset <- dataset[,c("Pt",optimzie_featrues,"CSPH")]
dataset <- dataset[complete.cases(dataset),]

#Scaling - based on the model
for (i in 1:ncol(dataset))
{
    if(class(dataset[,i])=="numeric" | class(dataset[,i])=="integer")
    {
        dataset[,i] <- (dataset[,i] - scaling_dataset$center[scaling_dataset$feature==names(dataset)[i]])/scaling_dataset$scale[scaling_dataset$feature==names(dataset)[i]]
    }
}

#Change factors to numeric
for (i in 1:ncol(dataset))
{
    if(class(dataset[,i])=="factor" & length(levels(dataset[,i]))==2 & names(dataset)[i]!="CSPH")
    {
        dataset[,i] <- as.numeric(dataset[,i])-1
    }
}

#Testing
sink("Testing_Results.txt", append=FALSE, split=FALSE)
cat("Testing Statistics:\n")
pred <- predict.glm(final_model,dataset,"response")
#pred <- exp(final_model$coefficients[1]+final_model$coefficients[2]*dataset[,2])/(1+exp(final_model$coefficients[1]+final_model$coefficients[2]*dataset[,2]))
stats <- epi.tests(table(dataset$CSPH,as.numeric(pred>optimized_grey_zone$opt_cut_off)), conf.level = 0.95)
auc(dataset$CSPH,pred); ci.auc(dataset$CSPH,pred)
stats

cat("Grey Zone Statistics:\n")
#grey zone
labels_csph <- dataset$CSPH[pred>optimized_grey_zone$top | pred<=optimized_grey_zone$low]
pred_grey_zones <- pred[pred>optimized_grey_zone$top | pred<=optimized_grey_zone$low]
unknown_grey_zones <- nrow(dataset)-length(labels_csph)

stats_grey_zones <- epi.tests(table(labels_csph,as.numeric(pred_grey_zones>optimized_grey_zone$opt_cut_off)), conf.level = 0.95)
auc(labels_csph,pred_grey_zones); ci.auc(labels_csph,pred_grey_zones)
stats_grey_zones
cat("Unknown Patients:\n"); cat(unknown_grey_zones)
sink()

rm(i,stats_grey_zones,final_model,labels_csph,pred_grey_zones,pred,stats,optimzie_featrues,scaling_dataset,optimized_grey_zone,unknown_grey_zones)

#plot scatter plots ####
conv_data_dob <- conv_data
dataset_dob <- dataset
fitted_data_dob <- fitted_data
fulldata_dob <- fulldata

conv_data_pdr <- conv_data
dataset_pdr <- dataset
fitted_data_pdr <- fitted_data
fulldata_pdr <- fulldata

par(mfrow=c(4,6))
for (i in c(66:89))
{
    plot(dataset[,i],dataset_pdr[,i],xlab="MHR",ylab="PDR",main=strsplit(names(dataset)[i],"_")[[1]][2])
}
par(mfrow=c(1,1))

par(mfrow=c(4,6))
for (i in c(66:89))
{
    plot(dataset[,i],dataset_dob[,i],xlab="MHR",ylab="DOB",main=strsplit(names(dataset)[i],"_")[[1]][2])
}
par(mfrow=c(1,1))

par(mfrow=c(4,6))
for (i in c(66:89))
{
    plot(dataset_pdr[,i],dataset_dob[,i],xlab="PDR",ylab="DOB",main=strsplit(names(dataset)[i],"_")[[1]][2])
}
par(mfrow=c(1,1))

par(mfrow=c(4,6))
for (i in c(66:89))
{
    plot(dataset[dataset$CSPH==0 & dataset$Gender.M.F==0,i],dataset_pdr[dataset$CSPH==0 & dataset$Gender.M.F==0,i],col=rgb(1,0,0,0.5),xlab="MHR",ylab="PDR",main=strsplit(names(dataset)[i],"_")[[1]][2])
    points(dataset[dataset$CSPH==1 & dataset$Gender.M.F==0,i],dataset_pdr[dataset$CSPH==1 & dataset$Gender.M.F==0,i],col=rgb(0,0,1,0.5))
}
par(mfrow=c(1,1))

par(mfrow=c(4,6))
for (i in c(66:89))
{
    plot(dataset[dataset$Gender.M.F==1,i],dataset_pdr[dataset$Gender.M.F==1,i],col=rgb(1,0,0,0.5),xlab="MHR",ylab="PDR",main=strsplit(names(dataset)[i],"_")[[1]][2])
    points(dataset[dataset$Gender.M.F==0,i],dataset_pdr[dataset$Gender.M.F==0,i],col=rgb(0,0,1,0.5))
}
par(mfrow=c(1,1))

boxplot((dataset[dataset$CSPH==0,c(66:89)]),col=rgb(1,0,0,0.5),xaxt ="n",main="MHR",xlab="Minues"); axis(1,at=1:length(t_final[-1]),labels=t_final[-1])
boxplot((dataset[dataset$CSPH==1,c(66:89)]),col=rgb(0,0,1,0.5),xaxt ="n",add=T)

boxplot((dataset[dataset$CSPH==0 & dataset$Gender.M.F==0,c(66:89)]),col=rgb(1,0,0,0.5),xaxt ="n",main="MHR Females",xlab="Minues"); axis(1,at=1:length(t_final[-1]),labels=t_final[-1])
boxplot((dataset[dataset$CSPH==1 & dataset$Gender.M.F==0,c(66:89)]),col=rgb(0,0,1,0.5),xaxt ="n",add=T)
