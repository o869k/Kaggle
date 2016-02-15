#Packages
libs<-c("readr","SnowballC","tm","wordcloud")
lapply(libs,require,character.only=TRUE)

#Loading Your working directory here
setwd("C:/Users/user/Documents/R/CrowdFlower")
train <- read_csv("train.csv")
test  <- read_csv("test.csv")
M <- ncol(train)
N <- nrow(train)
Ntest <- nrow(test)

query<-VCorpus(VectorSource(train$query))#Selects only query variable

#Term-Document Matrix
tdm<-TermDocumentMatrix(query,control=list(tolower=TRUE,removePunctuation=TRUE,
                                           removeNumbers=TRUE,stopwords=TRUE,
                                           stemming=TRUE))
tdm<-as.matrix(tdm)

word_freq<-sort(rowSums(tdm),decreasing=TRUE)#Obtains word frequencies
df<-data.frame(word=names(word_freq), freq=word_freq)#Builds a data-frame

#Wordcloud
png("wordcloud.png", width=12, height=8, units="in", res=300)
wordcloud(df$word, df$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()
