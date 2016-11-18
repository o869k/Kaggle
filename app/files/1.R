#Initialization ####
gc(); rm(list=ls()) # clear workspace
mainDir <- "C:\\FacebookPosts"; setwd(mainDir)
Sys.setlocale("LC_TIME", "English"); Sys.setenv(TZ='GMT')
library(text2vec)
library(glmnet)
library(tm)
library(SnowballC)
library(data.table)
library(dplyr)
library(plyr)
library(RTextTools)
library(e1071)
library(caret)

#Read Data ####
movie_review = read.table('labeledTrainData.tsv', header=TRUE, sep="\t",stringsAsFactors = F) #0 = bad review, 1 = good review
#data("movie_review")
setDT(movie_review); setkey(movie_review, id)
all_ids = movie_review$id

#Create Train and Test set  ####
train_ids = sample(all_ids, round(nrow(movie_review)*0.8))
test_ids = setdiff(all_ids, train_ids)
train = movie_review[J(train_ids)]
test = movie_review[J(test_ids)]

#Define preprocessing function and tokenization fucntion   ####
stem_tokenizer = function(x) {
    word_tokenizer(x) %>% lapply(wordStem)
}
stop_words = c("i", "me", "my", "myself", "we", "our", "ours", "ourselves", "you", "your", "yours")
it_train = itoken(train$review, 
                  preprocessor = tolower, 
                  tokenizer = stem_tokenizer, 
                  ids = train$id, 
                  progressbar = FALSE)
it_test = itoken(test$review, 
                  preprocessor = tolower, 
                  tokenizer = stem_tokenizer, 
                  ids = test$id, 
                  progressbar = FALSE)
vocab = create_vocabulary(it_train, ngram = c(1,2), stopwords = stopwords("en"))
#Prune - only consider the top 90,000 most common words in half of reviews, but eliminate the top 10 most common words
# vocab = prune_vocabulary(vocab,doc_proportion_min = 0.001)
# vocab = prune_vocabulary(vocab,term_count_min=sort(vocab$vocab$terms_counts,decreasing = T)[9000],term_count_max=sort(vocab$vocab$terms_counts,decreasing = T)[10]) 
bigram_vectorizer = vocab_vectorizer(vocab)
# create dtm_train with new pruned vocabulary vectorizer
dtm_train = create_dtm(it_train, bigram_vectorizer)
dtm_test = create_dtm(it_test, bigram_vectorizer)
dtm_train_binary = sign(dtm_train)
dtm_test_binary = sign(dtm_test)

# define tfidf model
tfidf = TfIdf$new()
# fit model to train data and transform train data with fitted model
dtm_train_tfidf = fit_transform(dtm_train,tfidf)
# tfidf modified by fit_transform() call!
# apply pre-trained tf-idf transformation to test data
dtm_test_tfidf  = dtm_test %>% transform(tfidf)

#Train the model (can be also NB) and validate ####
#Logistic regression with regularization
glmnet_classifier = cv.glmnet(x = dtm_train,
                              y = train[['sentiment']],
                              family = 'binomial',
                              alpha = 1,
                              type.measure = "auc",
                              nfolds = 5,
                              thresh = 1e-3,
                              maxit = 1e3)
#plot(glmnet_classifier)
print(paste("Validation AUC =", round(max(glmnet_classifier$cvm), 4)))
preds = predict(glmnet_classifier,dtm_train, type = 'response')[,1]
preds_test = predict(glmnet_classifier,dtm_test, type = 'response')[,1]
print(paste("Test AUC =", round(auc(test$sentiment,preds_test), 4)))
print(paste("Train AUC =", round(auc(train$sentiment,preds), 4)))

#Build the final Model ####
it_all = itoken(movie_review$review, 
                  preprocessor = tolower, 
                  tokenizer = stem_tokenizer, 
                  ids = movie_review$id, 
                  progressbar = FALSE)
vocab = create_vocabulary(it_all, ngram = c(1,2), stopwords = stopwords("en"))
#Prune
#vocab = prune_vocabulary(vocab,doc_proportion_min = 0.01)
#vocab = prune_vocabulary(vocab,term_count_min=sort(vocab$vocab$terms_counts,decreasing = T)[3000]) 
bigram_vectorizer = vocab_vectorizer(vocab)
dtm_all = create_dtm(it_all, bigram_vectorizer)
dtm_all_binary = sign(dtm_all)
tfidf = TfIdf$new()
dtm_all_tfidf = fit_transform(dtm_all,tfidf)

glmnet_classifier = cv.glmnet(x = dtm_all_tfidf,
                              y = movie_review[['sentiment']],
                              family = 'binomial',
                              alpha = 1,
                              type.measure = "auc",
                              nfolds = 5,
                              thresh = 1e-3,
                              maxit = 1e3)
print(paste("Validation AUC =", round(max(glmnet_classifier$cvm), 4)))
preds = predict(glmnet_classifier,dtm_all_tfidf, type = 'response')[,1]
print(paste("Train AUC =", round(auc(movie_review$sentiment,preds), 4)))

saveRDS(glmnet_classifier,"model.RDS") #save model
saveRDS(tfidf,"tfidf.RDS") #save tfidf
saveRDS(bigram_vectorizer,"bigram_vectorizer.RDS") #save bigram_vectorizer

#Naive bayes model ####
#library(doParallel)
#cl <- makeCluster(detectCores())
#registerDoParallel(cl)
#mat2 = as.matrix(dtm_all_tfidf)
#train_control = trainControl(method="cv", number=5)
#classifier = train(mat2,as.factor(movie_review[['sentiment']]),method ="nb",trControl=train_control)
#classifier = naiveBayes(mat2,as.factor(movie_review[['sentiment']]))
#print(classifier)
#stopCluster(cl)

#Test Case ####
Input = "The effect were great. Very good acting but bad directing"
it_test_case = itoken(Input, 
                 preprocessor = tolower, 
                 tokenizer = stem_tokenizer, 
                 progressbar = FALSE)
# create dtm_train with new pruned vocabulary vectorizer
dtm_test_case = create_dtm(it_test_case,bigram_vectorizer)
dtm_test_case_binary = sign(dtm_test_case)
dtm_test_case_tfidf  = dtm_test_case %>% transform(tfidf)
preds_test_case = predict(glmnet_classifier,dtm_test_case_tfidf, type = 'response')[,1]
print(paste0("Probaility for a good review = ", round(preds_test_case*100,2),"%"))

#Naive Bayes check
# mat_test_case = as.matrix(dtm_test_case_tfidf)
# predicted = predict(classifier,mat_test_case,"raw") #test it
# print(paste0("Probaility for a good review = ", round(predicted[,2]*100,2),"%"))
