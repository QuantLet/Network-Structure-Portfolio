
##### SETTINGS #####

# Clean the environment 
graphics.off()
rm(list = ls(all = TRUE))

setwd("~/Documents/Code/Network_structure_based_portfolio/Submission codes/Possible_solution_Efficient_frontier")

# Load Functions and other Files
source('./PackagesNetworkPortfolio.R')
source('./FunctionsNetworkPortfolio.R')


#Choose dataset to analyse
prices<-read_excel("SP500 securities.xlsx")
ZOO <- zoo(prices[,-1], order.by=as.Date(as.character(prices$Dates), format='%Y-%m-%d'))
#return
return<- Return.calculate(ZOO, method="log")
return<- return[-1, ]
returnstd<-xts(return)

# set label
node.label=colnames(returnstd)
node.label<-gsub("Equity","",node.label)
node.label<-gsub("UN","",node.label)
node.label<-gsub("UW","",node.label)

### Efficient frontier ###
er = colMeans(returnstd)
names(er) = node.label
evar = colSds(as.matrix(returnstd[sapply(returnstd, is.numeric)]))
names(evar) = node.label
covmat = cov(returnstd)
dimnames(covmat) = list(node.label, node.label)
r.free = 0.00005
# tangency portfolio
tan.port = tangency.portfolio(er, covmat, r.free)
# compute global minimum variance portfolio
gmin.port = globalMin.portfolio(er, covmat)
# compute portfolio frontier
ef  = efficient.frontier(er, covmat, alpha.min=-2,
                         alpha.max=1.5, nport=500)

# network of portfolio based on correlation matrix
network_port = network.portfolio(returnstd)
# eigenvalue
ec_port2 <- eigen_centrality(network_port,directed = FALSE, scale = TRUE)$vector
# compute portfolio frontier of network
covmat = cov(returnstd)
net.ef  = network.efficient.frontier(ec_port2, covmat, alpha.min=-0.1,
                                     alpha.max=2, nport=1000)

# install.packages("gganimate","gapminder")
# library("gganimate","gapminder")

###  Possible solution.png ####
xx=net.ef$sd
yy=net.ef$nc
d=cbind(as.data.frame(xx),as.data.frame(yy))
p<-ggplot(d, mapping = aes(xx, yy))+
  geom_point(size=0.1)+
  theme_bw()+
  xlim(0.00445,0.00672)+
  ylim(0.5,1.3)+
  xlab("Standard deviation")+
  ylab("Centrality")+
  theme(axis.line = element_line(), 
        axis.text = element_text(size=20),
        axis.title = element_text(size=30,face = "bold"),
        panel.background = element_blank(), 
        panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none",
        rect = element_rect(fill = "transparent"))
png("Possible solution.png", width = 800, height = 640, bg = "transparent")
p
dev.off()

####  Efficient frontier.png ####
xx=net.ef$sd
yy=net.ef$nc
d=cbind(as.data.frame(xx),as.data.frame(yy))
p<-ggplot(d[1:524,], mapping = aes(xx, yy))+
  geom_point(size=0.1)+
  theme_bw()+
  xlim(0.00445,0.00672)+
  ylim(0.5,1.3)+
  xlab("Standard deviation")+
  ylab("Centrality")+
  theme(axis.line = element_line(), 
        axis.text = element_text(size=20),
        axis.title = element_text(size=30,face = "bold"),
        panel.background = element_blank(), 
        panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none",
        rect = element_rect(fill = "transparent"))
png("Efficient_frontier.png", width = 800, height = 640, bg = "transparent")
p
dev.off()

