# Wordclouds in R with the Crowdflower Data
# We'll use the readr library to load the data, the tm library to tokenize the text and convert it to a document-term matrix, and the wordcloud library to visualize it.
library(readr)
library(tm)
library(wordcloud)
library(dplyr)
set.seed(0)

# Reading in the training data
setwd("C:/Users/user/Documents/R/CrowdFlower")
train <- read_csv("train.csv")
test  <- read_csv("test.csv")
M <- ncol(train)
N <- nrow(train)
Ntest <- nrow(test)

# The make_word_cloud function uses the tm library to tokenize and remove stopwords from the documents. We then use the wordcloud function to create a wordcloud with the 100 most common words.
make_word_cloud <- function(documents) {
  corpus = Corpus(VectorSource(tolower(documents)))
  corpus = tm_map(corpus, removePunctuation)
  corpus = tm_map(corpus, removeWords, stopwords("english"))
  
  frequencies = DocumentTermMatrix(corpus)
  word_frequencies = as.data.frame(as.matrix(frequencies))
  
  words <- colnames(word_frequencies)
  freq <- colSums(word_frequencies)
  wordcloud(words, freq,
            min.freq=sort(freq, decreasing=TRUE)[[100]],
            colors=brewer.pal(8, "Dark2"),
            random.color=TRUE)  
}

# The wordcloud below shows the 100 most common words used in the **search query**.
make_word_cloud(train$query)

# The wordcloud below shows the 100 most common words used in the **product title**.
# sampling to use less CPU / memory
make_word_cloud(sample(train$product_title, 1000))

#The wordcloud below shows the 100 most common words used in the **product description**.
make_word_cloud(sample(train$product_description, 1000))