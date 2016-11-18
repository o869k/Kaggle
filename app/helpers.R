#Libraries
library(text2vec)
library(glmnet)
library(SnowballC)

model <- readRDS("data/model.RDS")
tfidf <- readRDS("data/tfidf.RDS")
vectorizer <- readRDS("data/vectorizer.RDS")

stem_tokenizer = function(x) {
    word_tokenizer(x) %>% lapply(wordStem)
}

