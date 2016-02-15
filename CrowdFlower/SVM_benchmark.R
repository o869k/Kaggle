library(Metrics)
library(tm)
library(SnowballC)
library(e1071)
library(Matrix)
library(SparseM)

# Get the data
train = read_csv("train.csv")
test  = read_csv("test.csv")

ids = test$id
rtrain = nrow(train)
rtest =nrow(test)

relevance = as.factor(train$median_relevance)
variance = train$relevance_variance

# We don't need you anymoreeee
train$median_relevance = NULL
train$relevance_variance = NULL

# Combine train and test set for the dragons
combi=rbind(train,test)

#-------------------------------------------------------------------------------------
# Feature Engineering
# Here be Dragons

# Create Vector Space Model for query, product_title and product_description
all_text <- Corpus(VectorSource(combi$query))
dtm<-DocumentTermMatrix(all_text,control=list(tolower=TRUE,removePunctuation=TRUE,
                                              removeNumbers=TRUE,stopwords=TRUE,
                                              stemming=TRUE,weighting=function(x) weightTfIdf(x,normalize=T)))
dtm <- removeSparseTerms(dtm,0.99)
df_q<-Matrix(as.matrix(dtm),sparse=T)
df_q<-as.data.frame(as.matrix(dtm))
colnames(df_q)=paste("q_",colnames(df_q),sep="")

all_text <- Corpus(VectorSource(combi$product_title))
dtm<-DocumentTermMatrix(all_text,control=list(tolower=TRUE,removePunctuation=TRUE,
                                              removeNumbers=TRUE,stopwords=TRUE,
                                              stemming=TRUE,weighting=function(x) weightTfIdf(x,normalize=T)))
dtm <- removeSparseTerms(dtm,0.95)
df_pt<-Matrix(as.matrix(dtm),sparse=T)
df_pt<-as.data.frame(as.matrix(dtm))
colnames(df_pt)=paste("pt_",colnames(df_pt),sep="")

all_text <- Corpus(VectorSource(combi$product_description))
dtm<-DocumentTermMatrix(all_text,control=list(tolower=TRUE,removePunctuation=TRUE,
                                              removeNumbers=TRUE,stopwords=TRUE,
                                              stemming=TRUE,weighting=function(x) weightTfIdf(x,normalize=T)))
dtm <- removeSparseTerms(dtm,0.9)
df_pd<-as.data.frame(as.matrix(dtm))
colnames(df_pd)=paste("pd_",colnames(df_pd),sep="")

# Combine all columns into a single dataframe
combi=cbind(df_q,df_pt,df_pd)

# Get rid of the garbage
rm(df_q)
rm(df_pt)
rm(df_pd)
rm(all_text)
rm(corpus)
rm(dtm)

# Create sparse matrix
combi<-Matrix(as.matrix(combi),sparse=T)

#-------------------------------------------------------------------------------------
# Apply model and predict
train = combi[1:10158,]
test = combi[10159:32671,]

rm(combi)

model <- svm(train,relevance, kernel="linear", cost=1)

tpred = as.data.frame(ids)
pred <- predict(model,test)
tpred$prediction  <- pred
colnames(tpred)=c("id","prediction")
write.csv(tpred,"svm_sparse_model.csv",row.names=F)

print("Everthing done and your cofee is cold")