#Initialization ####
gc(); rm(list=ls()) # clear workspace
mainDir <- "C:\\FacebookPosts"; setwd(mainDir)
Sys.setlocale("LC_TIME", "English"); Sys.setenv(TZ='GMT')
library(tm); library(openNLP)
library(drat); library(imager)
library(stringi); library(textir)
library(SnowballC); library(wordcloud)
library(doParallel); library(parallel)
library(caret); library(e1071)
library(Rfacebook); library(readr)
library(dplyr); library(plyr)
library(readr); library(ggvis)
library(shiny); library(rsconnect)
library(mlbench); library(mxnet);
library(deeplearning); library(rpud)
library(zoo); library(Hmisc)
library(lubridate); library(nlme)
library(car); library(MASS)
library(RTextTools); library(text2vec)
library(data.table); library(glmnet)
library(arabicStemR)

#MLXnet ####
a <- mx.nd.zeros(c(2, 3)) # create a 2-by-3 matrix on cpu
b <- mx.nd.zeros(c(2, 3), mx.cpu()) # create a 2-by-3 matrix on cpu

#Sentiment analysis with RTextTools ####
pos_tweets =  rbind(
    c('I love this car', 'positive'),
    c('This view is amazing', 'positive'),
    c('I feel great this morning', 'positive'),
    c('I am so excited about the concert', 'positive'),
    c('He is my best friend', 'positive')
)

neg_tweets = rbind(
    c('I do not like this car', 'negative'),
    c('This view is horrible', 'negative'),
    c('I feel tired this morning', 'negative'),
    c('I am not looking forward to the concert', 'negative'),
    c('He is my enemy', 'negative')
)

test_tweets = rbind(
    c('feel happy this morning', 'positive'),
    c('larry friend', 'positive'),
    c('not like that man', 'negative'),
    c('house not great', 'negative'),
    c('your song annoying', 'negative')
)
tweets = rbind(pos_tweets, neg_tweets, test_tweets)


# build dtm - Document-Term matrix
matrix= create_matrix(tweets[,1],language="english", 
                      removeStopwords=FALSE,removeNumbers=TRUE, 
                      stemWords=FALSE)

# train the model
mat = as.matrix(matrix)
classifier = naiveBayes(mat[1:10,],as.factor(tweets[1:10,2]) )

# test the validity
predicted = predict(classifier, mat[11:15,]); 
classAgreement(table(tweets[11:15, 2], predicted))

#Sentiment analysis with text2vec - should be more memory efficient####
data("movie_review")
setDT(movie_review)
setkey(movie_review, id)
set.seed(2016L)
all_ids = movie_review$id
train_ids = sample(all_ids, 4000)
test_ids = setdiff(all_ids, train_ids)
train = movie_review[J(train_ids)]
test = movie_review[J(test_ids)]

# define preprocessing function and tokenization fucntion
prep_fun = tolower
tok_fun = word_tokenizer
it_train = itoken(train$review, 
                  preprocessor = prep_fun, 
                  tokenizer = tok_fun, 
                  ids = train$id, 
                  progressbar = FALSE)
stop_words = c("i", "me", "my", "myself", "we", "our", "ours", "ourselves", "you", "your", "yours")
vocab = create_vocabulary(it_train, ngram = c(1L, 2L), stopwords = stop_words)
vocab = vocab %>% prune_vocabulary(term_count_min = 10, 
                                   doc_proportion_max = 0.5)
bigram_vectorizer = vocab_vectorizer(vocab)
# create dtm_train with new pruned vocabulary vectorizer
dtm_train = create_dtm(it_train, bigram_vectorizer)

# define tfidf model
tfidf = TfIdf$new()
# fit model to train data and transform train data with fitted model
dtm_train_tfidf = fit_transform(dtm_train, tfidf)
# tfidf modified by fit_transform() call!
# apply pre-trained tf-idf transformation to test data
dtm_test_tfidf  = create_dtm(it_test, bigram_vectorizer) %>% 
    transform(tfidf)

NFOLDS <- 5
glmnet_classifier = cv.glmnet(x = dtm_train_tfidf, y = train[['sentiment']], 
                              family = 'binomial', 
                              alpha = 1,
                              type.measure = "auc",
                              nfolds = NFOLDS,
                              thresh = 1e-3,
                              maxit = 1e3)
plot(glmnet_classifier)
print(paste("max AUC =", round(max(glmnet_classifier$cvm), 4)))
preds = predict(glmnet_classifier, dtm_train_tfidf, type = 'response')[,1]

#Facebook Authentication ##### 
me <- getUsers("me", token=fb_oauth)
me$name

my_friends <- getFriends(fb_oauth, simplify = TRUE)
head(my_friends$id, n = 1) # get lowest user ID
mat <- getNetwork(fb_oauth, format = "adj.matrix")

my_likes <- getLikes(user="me", token=fb_oauth)

#Arabic language####
# Load data
data(aljazeera)
## stem and transliterate the results
## stem and return the stemlist
out <- stem(aljazeera,returnStemList=TRUE)
out <- removeStopWords(out$stemlist)

it_train = itoken(out$arabicStopwordList, 
                  progressbar = FALSE)

vocab = create_vocabulary(it_train, ngram = c(1L, 2L))
vocab = vocab %>% prune_vocabulary(term_count_min = 10, 
                                   doc_proportion_max = 0.5)
bigram_vectorizer = vocab_vectorizer(vocab)
# create dtm_train with new pruned vocabulary vectorizer
dtm_train = create_dtm(it_train, bigram_vectorizer)

# define tfidf model
tfidf = TfIdf$new()
# fit model to train data and transform train data with fitted model
dtm_train_tfidf = fit_transform(dtm_train, tfidf)
# tfidf modified by fit_transform() call!

#Spam filter####
dataset <- read.csv("data.csv",header=FALSE,sep=";")
names <- read.csv("names.csv",header=FALSE,sep=";")
names(dataset) <- sapply((1:nrow(names)),function(i) toString(names[i,1]))
dataset$y <- as.factor(dataset$y)
sample <- dataset[sample(nrow(dataset), 1000),]




