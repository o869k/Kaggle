# Exploring the Crowdflower Data
library(readr)
library(ggvis)
library(tm)
set.seed(0)

setwd("C:/Users/user/Documents/R/CrowdFlower")
train <- read_csv("train.csv")
test  <- read_csv("test.csv")
M <- ncol(train)
N <- nrow(train)
Ntest <- nrow(test)

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

#Some basic text analysis on the queries
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

#look at the most common terms in the query, the product title, and the product description.
# The top words in the query 
plot_word_counts(c(train$query, test$query))
# The top words in the product title (from a random sample for computational reasons)
plot_word_counts(sample(c(train$product_title, test$product_title), 1000))
# The top words in the product description (from a random sample for computational reasons)
plot_word_counts(sample(c(train$product_description, test$product_description), 1000))