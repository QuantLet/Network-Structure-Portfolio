# In this file, we want to draw some network as simple examples.

# We want to choose 8 stock, and construct a network with high centrality, and construct another network with low centrality. 
# By these two networks, we want to show stocks with high centrality are connected to more other stocks, while stocks with low centrality are connected to fewer stocks

setwd("~/Documents/Code/Network_structure_based_portfolio/Submission codes/Network 8-point example")
# Load Functions and other Files
source('./PackagesNetworkPortfolio.R')
source('./FunctionsNetworkPortfolio.R')

###### Low centrality network ######
prices<-read.csv("SP500 securities_up_20230306.csv")
selected_columns <- c("Dates","MSFT","AKAM","NLOK","KO","AAPL","EBAY","ORCL","PRU")
prices1<-prices[,selected_columns]
ZOO <- zoo(prices1[,-1], order.by=as.Date(as.character(prices$Dates), format='%Y-%m-%d'))
#return
return<- Return.calculate(ZOO, method="log")
return<- return[-1, ]
returnstd<-xts(return)
p=dim(return)[2]
# set label
node.label=colnames(returnstd)
names(returnstd) = node.label
# rolling window
W<-list()
for(t in 0: (floor((1857-500)/22)-1)){
  W[[(t+1)]]=returnstd[(1+t*22):(522+t*22),]
}
W_in<-list()
W_out<-list()
for(t in 0: (floor((1857-500)/22)-1)){
  W_in[[(t+1)]]=W[[t+1]][c(1:500),]
  W_out[[(t+1)]]=W[[t+1]][c(501:522),]
}
T.windows<-length(W)
# correlation matrix, Expected return, covariance matrix
C_in <- list()
ER_in <- list()
COV_in <- list()
EC_in <- list()
for(t in 1: length(W_in)){
  C_in[[(t)]] =cor(W_in[[(t)]])
  ER_in[[(t)]] = colMeans(W_in[[(t)]])
  COV_in[[(t)]] = cov(W_in[[(t)]])
  network_port = network.correlation(W_in[[(t)]])
  EC_in[[(t)]] <- eigen_centrality(network_port,directed = FALSE, scale = TRUE)$vector
  max(eigen_centrality(network_port,directed = FALSE, scale = TRUE)$vector)
  min(eigen_centrality(network_port,directed = FALSE, scale = TRUE)$vector)
  boxplot(eigen_centrality(network_port,directed = FALSE, scale = TRUE)$vector)
}

C = C_in[[(1)]]
C[abs(C)<0.23] <-0
g <- graph_from_adjacency_matrix(C, mode = "undirected", weighted = TRUE, diag = FALSE)
eigen_centr = evcent(g)$vector
pngname<-paste0(getwd(),"/lowcentrality",".png")
png(file = pngname, width=750, height=600, bg = "transparent")
plot(g, vertex.label = c(1:8), vertex.label.cex = 2,
     vertex.size = eigen_centr*80, edge.width = E(g)$weight*20, layout = layout_in_circle )
dev.off()
sum(eigen_centr)



###### High centrality network  ######
prices<-read.csv("SP500 securities_up_20230306.csv")
selected_columns <- c("Dates","MSFT","AKAM","NLOK","INTU","AAPL","EBAY","ORCL","CSCO")
prices1<-prices[,selected_columns]
ZOO <- zoo(prices1[,-1], order.by=as.Date(as.character(prices$Dates), format='%Y-%m-%d'))
#return
return<- Return.calculate(ZOO, method="log")
return<- return[-1, ]
returnstd<-xts(return)
p=dim(return)[2]
# set label
node.label=colnames(returnstd)
names(returnstd) = node.label
# rolling window
W<-list()
for(t in 0: (floor((1857-500)/22)-1)){
  W[[(t+1)]]=returnstd[(1+t*22):(522+t*22),]
}
W_in<-list()
W_out<-list()
for(t in 0: (floor((1857-500)/22)-1)){
  W_in[[(t+1)]]=W[[t+1]][c(1:500),]
  W_out[[(t+1)]]=W[[t+1]][c(501:522),]
}
T.windows<-length(W)
# correlation matrix, Expected return, covariance matrix
C_in <- list()
ER_in <- list()
COV_in <- list()
EC_in <- list()
for(t in 1: length(W_in)){
  C_in[[(t)]] =cor(W_in[[(t)]])
  ER_in[[(t)]] = colMeans(W_in[[(t)]])
  COV_in[[(t)]] = cov(W_in[[(t)]])
  network_port = network.correlation(W_in[[(t)]])
  EC_in[[(t)]] <- eigen_centrality(network_port,directed = FALSE, scale = TRUE)$vector
  max(eigen_centrality(network_port,directed = FALSE, scale = TRUE)$vector)
  min(eigen_centrality(network_port,directed = FALSE, scale = TRUE)$vector)
  boxplot(eigen_centrality(network_port,directed = FALSE, scale = TRUE)$vector)
}

C = C_in[[(1)]]
C[abs(C)<0.23] <-0
g <- graph_from_adjacency_matrix(C, mode = "undirected", weighted = TRUE, diag = FALSE)
eigen_centr = evcent(g)$vector
pngname<-paste0(getwd(),"/highcentrality",".png")
png(file = pngname, width=750, height=600, bg = "transparent")
plot(g, vertex.label = c(1:8), vertex.label.cex = 2,
     vertex.size = eigen_centr*80, edge.width = E(g)$weight*20, layout = layout_in_circle )
dev.off()
sum(eigen_centr)
