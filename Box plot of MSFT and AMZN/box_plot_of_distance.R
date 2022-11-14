# Box plot of distance


# In this file,  we draw the box plot of distance between MSFT and AMZN in 167 time window

rm(list = ls())

setwd("~/Documents/Code/Network structure based portfolio/Box plot of MSFT and AMZN")

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

d=c()
for (t in 1:T.windows) {
  d[t]=sqrt(2-2*cor(W_in[[(t)]])[c("MSFT  "),c("AMZN  ")])
}

pngname<-paste0(getwd(),"/boxplot_MSFT_AMZN",".png")
png(file = pngname, width=500, height=400, bg = "transparent")
boxplot(d)
dev.off()
