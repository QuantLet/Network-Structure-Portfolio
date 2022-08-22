
##### SETTINGS #####

# Clean the environment 
graphics.off()
rm(list = ls(all = TRUE))

setwd("~/Documents/Code/Network structure based portfolio/Eigenvector centrality with ER, SD, SR, BETA")

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
names(returnstd) = node.label

# network of portfolio based on correlation matrix
network_port = network.portfolio(returnstd)
# MST of portfolio
mst_port = MST.portfolio(returnstd)

# eigenvalue
centralization <- centr_eigen(mst_port)$centralization 
ec_port <- centr_eigen(mst_port)$vector
centralization1 <- eigen_centrality(mst_port,directed = FALSE, scale = TRUE)$value
ec_port1 <- eigen_centrality(mst_port,directed = FALSE, scale = TRUE)$vector
centralization2 <- eigen_centrality(network_port,directed = FALSE, scale = TRUE)$value
ec_port2 <- eigen_centrality(network_port,directed = FALSE, scale = TRUE)$vector

# setting
er = colMeans(returnstd)
names(er) = node.label
estd = colSds(as.matrix(returnstd[sapply(returnstd, is.numeric)]))
names(estd) = node.label
sr = er/estd
names(sr) = node.label
covmat = cov(returnstd)
dimnames(covmat) = list(node.label, node.label)
rm = rowMeans(returnstd)
sigma_m = sd(rm)
sigma_im = cov(returnstd,rm)
beta_port = sigma_im/sigma_m^2
corr_im=cor(returnstd,rm)
r.free = 0

# plot eigenvalue centrality 
pngname =  paste0("Eigenvalue centrality vs Mean.png")
png(file = pngname, width=500, height=400, bg = "transparent")
plot(ec_port2,er,xlab="Eigenvalue centrality",ylab="Expected return")
line.model<-lm(er~ec_port2)
summary(line.model)
abline(line.model, lwd=2, col="red")
dev.off()
pngname =  paste0("Eigenvalue centrality vs Std.png")
png(file = pngname, width=500, height=400, bg = "transparent")
plot(ec_port2,estd,xlab="Eigenvalue centrality",ylab="Standard deviation")
line.model<-lm(estd~ec_port2)
summary(line.model)
abline(line.model, lwd=2, col="red")
dev.off()
pngname =  paste0("Eigenvalue centrality vs Sharp ratio.png")
png(file = pngname, width=500, height=400, bg = "transparent")
plot(ec_port2,sr,xlab="Eigenvalue centrality",ylab="Sharp ratio")
line.model<-lm(sr~ec_port2)
summary(line.model)
abline(line.model, lwd=2, col="red")
dev.off()
pngname =  paste0("Eigenvalue centrality vs Beta.png")
png(file = pngname, width=500, height=400, bg = "transparent")
plot(ec_port2,beta_port,xlab="Eigenvalue centrality",ylab="Beta")
line.model<-lm(beta_port~ec_port2)
summary(line.model)
abline(line.model, lwd=2, col="red")
dev.off()
pngname =  paste0("Mean vs Std.png")
png(file = pngname, width=500, height=400, bg = "transparent")
plot(estd,er,xlab="Standard deviation",ylab="Expected return")
line.model<-lm(er~estd)
abline(line.model, lwd=2, col="red")
dev.off()
pngname =  paste0("Std. vs Beta.png")
png(file = pngname, width=500, height=400, bg = "transparent")
plot(estd,beta_port,xlab="Std.",ylab="Beta")
line.model<-lm(beta_port~estd)
summary(line.model)
abline(line.model, lwd=2, col="red")
dev.off()
pngname =  paste0("Std. vs Corr.png")
png(file = pngname, width=500, height=400, bg = "transparent")
plot(estd,corr_im,xlab="Std.",ylab="Correlation")
line.model<-lm(corr_im~estd)
summary(line.model)
abline(line.model, lwd=2, col="red")
dev.off()

