#### Initialization ####
sessionInfo() #system performance
gc() #clear unsued memory
rm(list=ls()) # clear workspace
mainDir <- "C:\\expedia"; setwd(mainDir)
Sys.setlocale("LC_TIME", "English")
Sys.setenv(TZ='GMT')
set.seed(1234) #setting seed for comparison
op <- options()
par(mfrow=c(1,1))

library(tm); library(openNLP)
library(stringi); library(textir)
library(SnowballC); library(wordcloud)
library(doParallel); library(parallel)
library(caret); library(e1071)

data(we8there)

## some multinomial inverse regression
## we'll regress counts onto 5-star overall rating
## cl=NULL implies a serial run.
## To use a parallel library fork cluster,
## uncomment the relevant lines below.
## Forking is unix only; use PSOCK for windows
# cl <- NULL
cl <- makeCluster(detectCores())

## small nlambda for a fast example
fits <- mnlm(cl, we8thereRatings[,'Overall',drop=FALSE],we8thereCounts, bins=5, gamma=1, nlambda=10)
# stopCluster(cl)

## plot fits for a few individual terms
terms <- c("first date","chicken wing","ate here", "good food","food fabul","terribl servic")
par(mfrow=c(3,2))
for(j in terms)
{ plot(fits[[j]]); mtext(j,font=2,line=2) }

## extract coefficients
B <- coef(fits)
mean(B[2,]==0) # sparsity in loadings
## some big loadings in IR
B[2,order(B[2,])[1:10]]
B[2,order(-B[2,])[1:10]]
## do MNIR projection onto factors
z <- srproj(B,we8thereCounts)
## fit a fwd model to the factors
summary(fwd <- lm(we8thereRatings$Overall ~ z))
## truncate the fwd predictions to our known range
fwd$fitted[fwd$fitted<1] <- 1
fwd$fitted[fwd$fitted>5] <- 5
## plot the fitted rating by true rating
par(mfrow=c(1,1))
plot(fwd$fitted ~ factor(we8thereRatings$Overall),varwidth=TRUE, col="lightslategrey")
plot(fwd$fitted ~ (we8thereRatings$Overall))

confusionMatrix(as.integer(fwd$fitted),we8thereRatings$Overall)

## 20 high-variance tf-idf terms
colnames(we8thereCounts)[order(-sdev(tfidf(we8thereCounts)))[1:20]]
