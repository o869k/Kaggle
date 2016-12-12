#Initializaiotn ####
rm(list=ls()) # clear workspace
#set.seed(1)
#options(java.parameters = "-Xmx8000m")
library(jsonlite)
library(igraph)

#Input/Output ####
args <- commandArgs(TRUE)
# test if there is at least one argument: if not, return an error
if (length(args)<1) {
  stop("At least one argument must be supplied (input file).n", call.=FALSE)
} else {
  #default
  min_vertex_nodes <- args[3] #identify those vertices part of less than X edges
}

#Read tweeter ISIS data ####
json_small <- jsonlite::fromJSON(args[1],flatten=T) # a list in which each element is one of your original JSON files
dataset_small_nodes <- json_small$nodes
dataset_small_nodes <- dataset_small_nodes[c("screen_name","favourites_count","followers_count","friends_count","statuses_count","location","lang","time_zone","id_str")]
names(dataset_small_nodes)[1] <- "source"
names(dataset_small_nodes)[7] <- "main_lang"
dataset_small_edges <- json_small$edges
dataset_small_edges <- dataset_small_edges[c("source","target","start","tweet","relationship","favorite_count","enti_hashtags","lang","retweet_count")]
dataset_small_edges$tweet <- NULL
dataset_small <- merge(dataset_small_edges,dataset_small_nodes)
dataset_small <- dataset_small[dataset_small$relationship!="Fictive",]
rm(dataset_small_edges,json_small)

#Finding community structure by multi-level optimization of modularity
dataset_small_graph <- dataset_small[!duplicated(dataset_small[,c("source","target","start")]),c("source","target")]
dataset_small.network <- graph.data.frame(dataset_small_graph,directed=F) #the 'directed' attribute specifies whether the edges are directed
bad.vs <- V(dataset_small.network)[degree(dataset_small.network)<min_vertex_nodes] #identify those vertices part of less than X edges
dataset_small.network <- delete.vertices(dataset_small.network, bad.vs) #exclude them from the graph
rm(dataset_small_graph,bad.vs,dataset_small)

louvain_clust_small <- cluster_louvain(dataset_small.network)
louvain_clust_dataset_small <- data.frame(source=louvain_clust_small$names,t(louvain_clust_small$memberships))
louvain_clust_dataset_small <- merge(louvain_clust_dataset_small,unique(dataset_small_nodes[,c("source","id_str")]))
louvain_clust_dataset_small$source <- NULL
write(jsonlite::toJSON(louvain_clust_dataset_small),args[2]) #json 
rm(dataset_small_nodes,louvain_clust_dataset_small,dataset_small.network,louvain_clust_small)

quit()
