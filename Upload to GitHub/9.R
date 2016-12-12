#Initializaiotn ####
rm(list=ls()) # clear workspace
setwd(mainDir)
#set.seed(1)
options(java.parameters = "-Xmx8000m")
library(jsonlite)
library(igraph)

#Read tweeter data ####
json_small <- jsonlite::fromJSON('twitter clustering/exp2/clusterInput.json',flatten=T) # a list in which each element is one of your original JSON files
dataset_small_nodes <- json_small$nodes
dataset_small_nodes <- dataset_small_nodes[c("screen_name","favourites_count","followers_count","friends_count","statuses_count","location","lang","time_zone","id_str")]
names(dataset_small_nodes)[1] <- "source"
names(dataset_small_nodes)[7] <- "main_lang"
dataset_small_edges <- json_small$edges
dataset_small_edges <- dataset_small_edges[c("source","target","start","relationship","favorite_count","enti_hashtags","lang","retweet_count")]
dataset_small <- merge(dataset_small_edges,dataset_small_nodes)
dataset_small <- dataset_small[dataset_small$relationship!="Fictive",]
rm(dataset_small_edges,json_small)

#Finding community structure by multi-level optimization of modularity
dataset_small_graph <- dataset_small[!duplicated(dataset_small[,c("source","target","start")]),c("source","target")]
dataset_small.network <- graph.data.frame(dataset_small_graph,directed=F) #the 'directed' attribute specifies whether the edges are directed
bad.vs <- V(dataset_small.network)[degree(dataset_small.network)<2] #identify those vertices part of less than X edges
dataset_small.network <- delete.vertices(dataset_small.network, bad.vs) #exclude them from the graph
length(V(dataset_small.network))
length(E(dataset_small.network))
rm(dataset_small_graph,bad.vs,dataset_small)

louvain_clust_small <- cluster_louvain(dataset_small.network)
louvain_clust_small$modularity
louvain_clust_dataset_small <- data.frame(source=louvain_clust_small$names,t(louvain_clust_small$memberships))
V(dataset_small.network)$color <- louvain_clust_dataset_small$X2 #color the nodes useful for highlighting the communitites
dataset_small.network$palette <- brewer.pal(length(unique(V(dataset_small.network)$color)),"Set3")
plot(dataset_small.network, layout = layout.fruchterman.reingold)
louvain_clust_dataset_small <- merge(louvain_clust_dataset_small,unique(dataset_small_nodes[,c("source","id_str")]))
louvain_clust_dataset_small$source <- NULL

cluster_result_itzik <- jsonlite::fromJSON('twitter clustering/exp2/clusterOutput.json',flatten=T) # a list in which each element is one of your original JSON files

write(jsonlite::toJSON(louvain_clust_dataset_small),"myOutput.json") #json 
