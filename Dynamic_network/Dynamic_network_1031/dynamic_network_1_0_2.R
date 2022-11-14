# install.packages('ndtv', dependencies=T)

# dynamic network

rm(list = ls())

setwd("~/Documents/Code/Network structure based portfolio/Gephi project/dynamic network/Dynamic_network_1031")

# Load Functions and other Files
source('./PackagesNetworkPortfolio.R')
source('./FunctionsNetworkPortfolio.R')

# load data
prices<-read_excel("SP500 securities.xlsx")
ZOO <- zoo(prices[,-1], order.by=as.Date(as.character(prices$Dates), format='%Y-%m-%d'))

#return
return<- Return.calculate(ZOO, method="log")
return<- return[-1, ]
returnstd<-xts(return)
p=dim(return)[2]

# set label
node.label=colnames(returnstd)
node.label<-gsub("Equity","",node.label)
node.label<-gsub("UN","",node.label)
node.label<-gsub("UW","",node.label)
names(returnstd) = node.label

# rolling window
W<-list()
for(t in 0: 166){
  W[[(t+1)]]=returnstd[(1+t*7):(500+t*7),]
}
W_in<-list()
W_out<-list()
for(t in 0: 166){
  W_in[[(t+1)]]=W[[t+1]][c(1:493),]
  W_out[[(t+1)]]=W[[t+1]][c(494:500),]
}
T.windows<-length(W)
# correlation matrix, Expected return, covariance matrix
C_in <- list()     
ER_in <- list()     
COV_in <- list()     
EC_in <- list()  
EC.posi_in <- list()  
EC.nega_in <- list()  
BetweennessCentrality<-list()
ClosenessCentrality<-list()
DegreeCentrality<-list()
network_port <- list()
for(t in 1: length(W_in)){
  C_in[[(t)]] =cor(W_in[[(t)]])
  ER_in[[(t)]] = colMeans(W_in[[(t)]])
  COV_in[[(t)]] = cov(W_in[[(t)]])
  Dist_whole<-sqrt(2-2*C_in[[(t)]])
  network_whole=graph_from_adjacency_matrix(Dist_whole,weighted=T,
                                            mode="undirected", diag=F)  # network of filtered correlation matrix
  Edgelist_whole<-get.edgelist(network_whole)                           # edges of network
  weight_whole<-E(network_whole)$weight                                 # weight of network
  A<-cbind(Edgelist_whole,weight_whole)
  A<-as.matrix(A)
  links2_whole<-as.data.frame(A)                                # links of network
  colnames(links2_whole)<-c("from","to","weight")
  net_whole<- graph_from_data_frame(d=links2_whole, directed=F) # net of whole data
  mst_whole<- minimum.spanning.tree(net_whole)                  # minimum spanning tree
  Edgelist_whole<-get.edgelist(mst_whole)                           
  weight_whole<-1-as.numeric(c(E(mst_whole)$weight))^2/2
  A<-cbind(Edgelist_whole,weight_whole)
  A<-as.matrix(A)
  links2_whole<-as.data.frame(A)
  colnames(links2_whole)<-c("from","to","weight")
  network_port[[t]] = graph_from_data_frame(d=links2_whole, directed=F)
}

#### dynamic network graphml output ####
for (t in 1: length(W_in)) {
  net_cor <- network_port[[t]]
  write_graph(net_cor, paste0("net_cor_",t,".graphml"), "graphml")
}

