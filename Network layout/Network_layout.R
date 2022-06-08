rm(list = ls())

library(readxl)
library(xts)
library(PerformanceAnalytics)
library(quantmod)
library(timeSeries)
library(xtable)
library(igraph)
library(tcltk2)
library(MTS)
library(matrixcalc)
library(Matrix)
#library(fPortfolio)
library(IntroCompFinR)  #install.packages("IntroCompFinR", repos="http://R-Forge.R-project.org")
require(quadprog)
library(pracma)
library(glasso)
library(MST)

setwd("~/Documents/Code/Network structure based portfolio/Network layout")
prices<-read_excel("SP500 securities.xlsx")
ZOO <- zoo(prices[,-1], order.by=as.Date(as.character(prices$Dates), format='%Y-%m-%d'))

#return
return<- Return.calculate(ZOO, method="log")
return<- return[-1, ]
returnstd<-xts(return)

## plot MST of the whole sample time from Sep14, 2017 to Oct 17, 2019
#ciao_whole<-list()
# correlation matrix
C_whole<-cor(returnstd)                        # correlation matrix
node.label=colnames(returnstd)
node.label<-gsub("Equity","",node.label)
node.label<-gsub("UN","",node.label)
node.label<-gsub("UW","",node.label)
colnames(C_whole)<-node.label
rownames(C_whole)<-node.label
# distance matrix
Dist_whole<-sqrt(2-2*C_whole)                          # distance matrix
Dist_whole<-as.matrix(Dist_whole)
Dist_whole[is.nan(Dist_whole)]<-0                        # why nan? I think it may be set as inf, anywhere it cannot be nan
colnames(Dist_whole)<-node.label
rownames(Dist_whole)<-node.label
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
verticesdegreeall<-degree(mst_whole)
node.size<-as.matrix(verticesdegreeall)*0.5
node.label[which(node.size<4)]<-""

# lets plot
# gem
png("Stock_MST_whole_layout_gem.png", width=1000, height=800, bg = "transparent")
set.seed(123)
plot(mst_whole, edge.color= "black", vertex.size= node.size, vertex.color="orange",
     vertex.label=node.label,vertex.label.dist=1.5,layout=layout_with_gem)
dev.off()
#star
png("Stock_MST_whole_layout_star1.png", width=1000, height=800, bg = "transparent")
set.seed(123)
plot(mst_whole, edge.color= "black", vertex.size= node.size, vertex.color="orange",
     vertex.label=node.label,vertex.label.dist=1.5,layout=layout_as_star)
dev.off()
# tree
png("Stock_MST_whole_layout_tree1.png", width=1000, height=800, bg = "transparent")
set.seed(123)
plot(mst_whole, edge.color= "black", vertex.size= node.size, vertex.color="orange",
     vertex.label=node.label,vertex.label.dist=1,layout=layout_as_tree)
dev.off()
# lgl
png("Stock_MST_whole_layout_lgl1.png", width=1000, height=800, bg = "transparent")
set.seed(123)
plot(mst_whole, edge.color= "black", vertex.size= node.size, vertex.color="orange",
     vertex.label=node.label,vertex.label.dist=1,vertex.label.font=4,
     vertex.label.cex=2,layout=layout_with_lgl)
dev.off()
png("Stock_MST_whole_layout_lgl2.png", width=1000, height=800, bg = "transparent")
set.seed(123)
plot(mst_whole, edge.color= "black", vertex.size= node.size, vertex.color="orange",
     vertex.label=node.label,vertex.label.dist=1,vertex.label.font=4,
     vertex.label.cex=2,layout=layout_with_lgl)
dev.off()
# gem
png("Stock_MST_whole_layout_gem1.png", width=1000, height=800, bg = "transparent")
set.seed(123)
plot(mst_whole, edge.color= "black", vertex.size= node.size, vertex.color="orange",
     vertex.label=node.label,vertex.label.dist=1,vertex.label.font=4,
     vertex.label.cex=2,layout=layout_with_gem)
dev.off()

# layout=component_wise: error
# layout=component_wise: error
# layout=layout_as_bipartite: error
# layout=layout_as_star: good, same as layout_in_circle
# layout=layout_as_tree: good, tree
# layout=layout_in_circle: good, circle, better than layout_on_sphere
# layout=layout_nicely: circle, not clear, like layout_with_fr
# layout=layout_on_grid: bad, messy
# layout=layout_on_sphere: bad, messy
# layout=layout_randomly: bad, messy
# layout=layout_with_dh: bad, messy
# layout=layout_with_fr: not clear, circle
# layout=layout_with_gem: good, overlap
# layout=layout_with_graphopt: branches, not clear
# layout=layout_with_kk: bad
# layout=layout_with_lgl: good, little overlapping
# layout=layout_with_sugiyama: error
# layout=merge_coords: error
# layout=norm_coords: error
# layout=normalize: error


#install.packages("remotes")
#remotes::install_github("zunderlab/FLOWMAP")
write_graph(mst_whole, "mst_whole_graphml.graphml", "graphml")
write_graph(net_whole, "net_whole_graphml.graphml", "graphml")



