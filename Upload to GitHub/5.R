#Initialization
{
  sessionInfo() #system performance
  gc() #clear unsued memory
  rm(list=ls()) # clear workspace
  mainDir <- "D:/Kaggle/CrowdFlower/"
  setwd(mainDir)
  Sys.setlocale("LC_TIME", "English")
  set.seed(66719543) #setting seed for comparison
  library(fpp); library(UsingR)
  library(caret); library(kernlab)
  library(foreach); library(doParallel)
  library(ggplot2); library(h2o)
  library(randomForest); library(date)
  library(data.table); library(bit64)
  library(matrixStats); library(e1071)
  library(FSelector); library(lubridate)
  library(nnet); library(quantmod)
  library(neuralnet); library(minpack.lm)
  library(RSNNS); library(ROCR)
  library(Metrics); library(tm)
  library(SnowballC); library(e1071)
  library(Matrix); library(SparseM)
  library(readr); library(ggvis)
  library(tm); library(wordcloud)
}

#Read Data
{
  train <- read_csv("train.csv")
  test <- read_csv("test.csv")
  sampleSubmission <- read.csv("sampleSubmission.csv",header=T)
  M <- ncol(train)
  N <- nrow(train)
  Ntest <- nrow(test)
}

#Helper Functions
{
  # Creating a helper function plot_word_counts to plot counts of word occurences in different sets
  plot_word_counts <- function(documents) {
    # Keep only unique documents and convert them to lowercase
    corpus <- Corpus(VectorSource(tolower(unique(documents))))
    # Remove punctuation from the documents
    corpus <- tm_map(corpus, removePunctuation)
    # Remove english stopwords, such as "the" and "a"
    corpus <- tm_map(corpus, removeWords, stopwords("english"))
    
    doc_terms <- DocumentTermMatrix(corpus)
    doc_terms <- as.data.frame(as.matrix(doc_terms))
    word_counts <- data.frame(Words=colnames(doc_terms), Counts=colSums(doc_terms))
    # Sort from the most frequent words to the least frequent words
    word_counts <- word_counts[order(word_counts$Counts, decreasing=TRUE),]
    
    top_words <- word_counts[1:10,]
    top_words$Words <- factor(top_words$Words, levels=top_words$Words)
    
    # Plot the 10 most frequent words with ggvis
    top_words %>%
      ggvis(~Words, ~Counts) %>%
      layer_bars(fill:="#20beff")
  }
  
  # remove html syntax in product description
  rm_garbage = function(string){
    garbage = c("<.*?>", "http", "www","img","border","style","px","margin","left", "right","font","solid","This translation tool is for your convenience only.*?Note: The accuracy and accessibility of the resulting translation is not guaranteed")
    for (i in 1:length(garbage)){
      string = gsub(garbage[i], "", string)
    }
    return (string)
  }
  
}

#Exploritory Analysis
{
  # The number of unique train queries
  length(unique(train$query))
  # The number of unique test queries
  length(unique(test$query))
  # Are any queries different between the sets?
  length(setdiff(unique(train$query), unique(test$query)))
  # It looks like all the queries we see in the training set are also in the test set - so we can just factor them!
  
  # The number of unique product titles in the training set
  length(unique(train$product_title))
  # The number of product titles that are only in the train set or only in the test set
  length(setdiff(unique(train$product_title), unique(test$product_title)))
  # The number of product titles that are in both the train and test sets
  length(intersect(unique(train$product_title), unique(test$product_title)))
  #This tells us that we only see most of the product titles once, and that the product titles are mostly different between the train and test sets.
  
  #look at the most common terms in the query, the product title, and the product description.
  # The top words in the query 
  plot_word_counts(c(train$query, test$query))
  # The top words in the product title (from a random sample for computational reasons)
  plot_word_counts(sample(c(train$product_title, test$product_title), 1000))
  # The top words in the product description (from a random sample for computational reasons)
  plot_word_counts(sample(c(train$product_description, test$product_description), 1000))
  
  #Word count in query
  query <- VCorpus(VectorSource(train$query)) #Selects only query variable
  #Term-Document Matrix
  tdm <- TermDocumentMatrix(query,control=list(tolower=TRUE,removePunctuation=TRUE,
                                             removeNumbers=TRUE,stopwords=TRUE,stemming=TRUE))
  tdm <- as.matrix(tdm)
  word_freq <- sort(rowSums(tdm),decreasing=TRUE)#Obtains word frequencies
  query_word_freq_df <- data.frame(word=names(word_freq), freq=word_freq)#Builds a data-frame
  
}

#Data Preprocessing
{
  train$product_description = lapply(train$product_description,rm_garbage)
  test$product_description = lapply(test$product_description,rm_garbage)
  
  train$median_relevance = as.factor(train$median_relevance)
  train$query = as.factor(train$query)
  test$query = factor(test$query,levels = levels(train$query))
}
