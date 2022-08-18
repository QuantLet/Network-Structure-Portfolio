
rm(list = ls())

library(PerformanceAnalytics)
library(xts)
library(quantmod)
library(timeSeries)
library(xtable)
library(igraph)
library(tcltk2)
library(MTS)
library(matrixcalc)
library(Matrix)
library(fPortfolio)
library(IntroCompFinR)  #install.packages("IntroCompFinR", repos="http://R-Forge.R-project.org")
require(quadprog)
library(pracma)
library(glasso)
library(readxl)
library(MST)
#library(pheatmap)

setwd("~/Documents/Code/Network structure based portfolio/Dantzig-selector estimation covariance/Different strategies comparison")

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
# correlation matrix, Expected return, covariance matrix
C_in <- list()     
ER_in <- list()     
COV_in <- list()     
EC_in <- list()   
for(t in 1: length(W_in)){
  C_in[[(t)]] =cor(W_in[[(t)]])
  ER_in[[(t)]] = colMeans(W_in[[(t)]])
  COV_in[[(t)]] = cov(W_in[[(t)]])
  # network_port = network.portfolio(W_in[[(t)]])
  EC_in[[(t)]] <- eigen_centrality(network_port,directed = FALSE, scale = TRUE)$vector
}

# generate timeline "cri" for later portfolio
date<-prices$Dates
date<-as.Date.factor(date[-c(1:501)], format="%Y-%m-%d")
meanlayer1<-list()
for(t in 1: 167){
  meanlayer1[[t]]<-date[[t*7]]
}
meanlayer1<-as.Date(unlist(meanlayer1))
psi<-data.frame(meanlayer1)
psi<-t(psi)
gloxi<-t(data.frame(psi))  # can not combine
gloxia<-data.frame(gloxi)            # can not combine
glox<-as.data.frame(gloxia$meanlayer1) # can not combine
glox<-data.frame(glox)                 # can not combine
glox<-as.matrix(glox$gloxia.meanlayer1)
glox<-glox[-168]
cri<-as.Date(as.character(glox)) # from "2017-12-13" to "2022-05-27"

##### weights of different portfolios in each rolling window #####
#### minimum variance portfolio  ####
w_minVar<-list()
cumureturn_minVar<-list()
for(t in 1: length(W_in)){
  portf_minVar =globalMin.portfolio(ER_in[[(t)]],COV_in[[(t)]])
  w_minVar[[(t)]] =portf_minVar$weights
  aus<-as.matrix(repmat(w_minVar[[(t)]],7,1)*W_out[[t]])
  cumureturn_minVar[[t]]<-rowSums(aus)
}
cumureturn_minVar<-as.matrix(cbind(unlist(cumureturn_minVar)))
cumureturn_minVar<-cumsum(cumureturn_minVar)

#### minimum variance portfolio no short ####
w_minVar_noshort<-list()
cumureturn_minVar_noshort<-list()
for(t in 1: length(W_in)){
  portf_minVar =globalMin.portfolio(ER_in[[(t)]],COV_in[[(t)]],FALSE)
  w_minVar_noshort[[(t)]] =portf_minVar$weights
  aus<-as.matrix(repmat(w_minVar_noshort[[(t)]],7,1)*W_out[[t]])
  cumureturn_minVar_noshort[[t]]<-rowSums(aus)
}
cumureturn_minVar_noshort<-as.matrix(cbind(unlist(cumureturn_minVar_noshort)))
cumureturn_minVar_noshort<-cumsum(cumureturn_minVar_noshort)

#### equally portfolio ####
w_equal<-list()
cumureturn_equal<-list()
centrality_equal_portfolio<-list()
for(t in 1: length(W_in)){
  w_equal[[(t)]] =matrix(1/465,1,465)
  aus<-as.matrix(repmat(w_equal[[(t)]],7,1)*W_out[[t]])
  cumureturn_equal[[t]]<-rowSums(aus)
  centrality_equal_portfolio[[t]]<-num(w_equal[[(t)]]%*%EC_in[[(t)]])
}
cumureturn_equal<-as.matrix(cbind(unlist(cumureturn_equal)))
cumureturn_equal<-cumsum(cumureturn_equal)
centrality_equal_portfolio<-as.matrix(cbind(unlist(centrality_equal_portfolio)))

#### network portfolio setting centrality constraint 0.8 ####
### network portfolio no short ###
w_network_noshort<-list()
cumureturn_network_noshort<-list()
for(t in 1: length(W_in)){
  ## compute global minimum variance portfolio without short ##
  net.gmin.port = network.efficient.portfolio(EC_in[[(t)]], COV_in[[(t)]],0.84,FALSE)
  w_network_noshort[[(t)]] =net.gmin.port$weights
  aus<-as.matrix(repmat(w_network_noshort[[(t)]],7,1)*W_out[[t]])
  cumureturn_network_noshort[[t]]<-rowSums(aus)
}
cumureturn_network_noshort<-as.matrix(cbind(unlist(cumureturn_network_noshort)))
cumureturn_network_noshort<-cumsum(cumureturn_network_noshort)
### network portfolio varying constraint ###
cumureturn_network_vary_with_phi<-list()
phi_star<-seq(1,0,-0.05)
for (i in 1:length(phi_star)) {
  w_network<-list()
  cumureturn_network<-list()
  for(t in 1: length(W_in)){
    ## compute global minimum variance portfolio ##
    net.gmin.port = network.efficient.portfolio(EC_in[[(t)]], COV_in[[(t)]],phi_star[i],TRUE)
    w_network[[(t)]] =net.gmin.port$weights
    aus<-as.matrix(repmat(w_network[[(t)]],7,1)*W_out[[t]])
    cumureturn_network[[t]]<-rowSums(aus)
  }
  cumureturn_network<-as.matrix(cbind(unlist(cumureturn_network)))
  cumureturn_network_vary_with_phi[[i]]<-cumsum(cumureturn_network)
}
### network portfolio data-driven constraint ###
cumureturn_network_datadriven_phistar<-list()
phi_star<-centrality_equal_portfolio
w_network<-list()
cumureturn_network_datadriven_phistar<-list()
for(t in 1: length(W_in)){
    ## compute global minimum variance portfolio ##
    net.gmin.port = network.efficient.portfolio(EC_in[[(t)]], COV_in[[(t)]],phi_star[t],TRUE)
    w_network[[(t)]] =net.gmin.port$weights
    aus<-as.matrix(repmat(w_network[[(t)]],7,1)*W_out[[t]])
    cumureturn_network_datadriven_phistar[[t]]<-rowSums(aus)
  }
cumureturn_network_datadriven_phistar<-as.matrix(cbind(unlist(cumureturn_network_datadriven_phistar)))
cumureturn_network_datadriven_phistar<-cumsum(cumureturn_network_datadriven_phistar)

### network constraint portfolio using Dantzig type selector ###
linfun=function(Sn,b,lambda)
{
  #equivalent to solving   min 1'u
  #such that		u-x>=0
  #						u+x>=0
  #						-hatS x>=-(lambda 1+b)
  #						hatS x>=-lambda 1+b
  #						x^T1=1
  a=rep(0,2*p)
  a[c(1,1+p)]=1
  A0=toeplitz(a)
  A0[upper.tri(A0)]=-A0[upper.tri(A0)]
  A1=cbind(matrix(0,p,p),-Sn)
  A2=-A1
  A=rbind(A0,A1,A2)
  rhs=c(rep(0,2*p),c(-b,b)-lambda*rep(1,2*p))
  C=rep(c(1,0),c(p,p))
  #	EE=rep(c(0,1),c(p,p))
  #	FF=1
  solvetheta=linp(#E=EE,F=FF,
    G=A,H=rhs,Cost=C,ispos=FALSE)$X[(p+1):(2*p)]
  return(solvetheta)
}

phi_star<-centrality_equal_portfolio
w_network_Dantzig<-list()
cumureturn_network_Dantzig<-list()
for(t in 1: length(W_in)){
  ## compute global minimum variance portfolio ##
  w_network_Dantzig[[(t)]] =linfun(COV_in[[t]],EC_in[[t]],phi_star[t])
  aus<-as.matrix(repmat(w_network[[(t)]],7,1)*W_out[[t]])
  cumureturn_network_Dantzig[[t]]<-rowSums(aus)
}
cumureturn_network_Dantzig<-as.matrix(cbind(unlist(cumureturn_network_Dantzig)))
cumureturn_network_Dantzig<-cumsum(cumureturn_network_Dantzig)

#### Figures and animation output ####
##### Figure: cumulative returns of #####
### minVar, minVar no short, equal, network with phistar 0.6, ###
### network with phistar 0.6 no short, network with data-driven phistar  ###
### network with Danztig  ###
Colors_Ret<-rainbow(7)
CumRet<-zoo(cbind(data.frame(cumureturn_minVar),
                    data.frame(cumureturn_minVar_noshort),
                    data.frame(cumureturn_equal),
                    data.frame(cumureturn_network_vary_with_phi[[9]]),
                    data.frame(cumureturn_network_noshort),
                    data.frame(cumureturn_network_datadriven_phistar),
                    data.frame(cumureturn_network_Dantzig)
                    ),
              order.by=as.Date(as.character(date), format="%Y-%m-%d"))
pngname<-paste0(getwd(),"/portfolio_comparison/portfolio_comparison_datadriven_constraint_Dantzig",".png")
png(file = pngname, width=500, height=400, bg = "transparent")
plot(CumRet, screens=1, col=Colors_Ret, ylab="", xlab="",title=paste0("Cumulate return with network Dantzig"))
legend("topleft",legend=c("minVar", "minVar no short", "Equal", "network", "network no short","network data-driven constrain","network Dantzig"),
         col=Colors_Ret, lty=1:2, cex=0.8)
dev.off()
##### Figure: cumulative returns of #####
### minVar, minVar no short, equal, network with phistar 0.6, ###
### network with phistar 0.8 no short  ###
### network with Danztig  ###
Colors_Ret<-rainbow(7)
CumRet<-zoo(cbind(data.frame(cumureturn_minVar),
                  data.frame(cumureturn_minVar_noshort),
                  data.frame(cumureturn_equal),
                  data.frame(cumureturn_network_vary_with_phi[[9]]),
                  data.frame(cumureturn_network_noshort),
                  data.frame(cumureturn_network_Dantzig)
),
order.by=as.Date(as.character(date), format="%Y-%m-%d"))
pngname<-paste0(getwd(),"/portfolio_comparison/portfolio_comparison_with_Dantzig",".png")
png(file = pngname, width=500, height=400, bg = "transparent")
plot(CumRet, screens=1, col=Colors_Ret[c(1:3,6,5,7)], ylab="", xlab="",title=paste0("Cumulate return with network Dantzig"))
legend("topleft",legend=c("minVar", "minVar no short", "Equal", "network", "network no short","network Dantzig"),
       col=Colors_Ret[c(1:3,6,5,7)], lty=1:2, cex=0.8)
dev.off()
##### Animation: cumulative returns with varying constraint  of #####
### minVar, minVar no short, equal, network varying constraint, ###
### network with phistar 0.8 no short  ###
Colors_Ret<-rainbow(7)
phi_star<-seq(1,0,-0.05)
fig = image_graph(width = 800, height = 640, res = 150, bg = "transparent")
for (i in 1:length(phi_star) ) {
  CumRet<-zoo(cbind(data.frame(cumureturn_minVar),
                    data.frame(cumureturn_minVar_noshort),
                    data.frame(cumureturn_equal),
                    data.frame(cumureturn_network_vary_with_phi[[i]]),
                    data.frame(cumureturn_network_noshort)),
              order.by=as.Date(as.character(date), format="%Y-%m-%d"))
  plot(CumRet, screens=1, col=Colors_Ret[c(1:3,6,5)], ylab="", xlab="",title=paste0("phi*=",phi_star[i]))
  # legend("topleft",legend=c("minVar", "minVar without short", "Equal", "network", "network without short"),
         # col=Colors_Ret[1:5], lty=1:2, cex=0.8)
}
dev.off()
animation <- image_animate(fig, fps = 1)
image_write(animation, paste0(getwd(), "/portfolio_comparison/performance_varying_with_constraint.gif"))



# figures
phi_star<-seq(1,0,-0.05)
for (i in 1:length(phi_star) ) {
  CumRet<-zoo(cbind(data.frame(cumureturn_minVar),
                    data.frame(cumureturn_minVar_noshort),
                    data.frame(cumureturn_equal),
                    data.frame(cumureturn_network_vary_with_phi[[i]]),
                    data.frame(cumureturn_network_noshort)),
              order.by=as.Date(as.character(date), format="%Y-%m-%d"))
  pngname<-paste0(getwd(),"/portfolio_comparison_with_constraint_varying/portfolio_comparison_with_constraint_",phi_star[i],".png")
  png(file = pngname, width=500, height=400, bg = "transparent")
  plot(CumRet, screens=1, col=Colors_Ret[1:5], ylab="", xlab="",title=paste0("phi*=",phi_star[i]))
  legend("topleft",legend=c("minVar", "minVar without short", "Equal", "network", "network without short"),
         col=rainbow(5), lty=1:2, cex=0.8)
  dev.off()
}

cumureturn_network_vary_with_phi_unlist<-unlist(cumureturn_network_vary_with_phi)
# cumelative returns in one figure
CumRet<-zoo(cbind(data.frame(cumureturn_minVar),
                  data.frame(cumureturn_minVar_noshort),
                  data.frame(cumureturn_equal),
                  data.frame(cumureturn_network_vary_with_phi[[3]]),
                  data.frame(cumureturn_network_vary_with_phi[[5]]),
                  data.frame(cumureturn_network_vary_with_phi[[7]]),
                  data.frame(cumureturn_network_vary_with_phi[[9]]),
                  data.frame(cumureturn_network_noshort)),
            order.by=as.Date(as.character(date), format="%Y-%m-%d"))
pngname<-paste0(getwd(),"/portfolio_comparison_with_constraint_varying/portfolio_comparison_with_constraint_",
                phi_star[3],"_",phi_star[5],"_",phi_star[7],"_",phi_star[9],".png")
png(file = pngname, width=500, height=400, bg = "transparent")
plot(CumRet, screens=1, col=rainbow(8), ylab="", xlab="",title=paste0("phi*=",phi_star[i]))
legend("topleft",legend=c("minVar", "minVar without short", "Equal",
                          paste0("network","phi*=",phi_star[3]),paste0("network","phi*=",phi_star[5]),
                          paste0("network","phi*=",phi_star[7]),paste0("network","phi*=",phi_star[9]),
                          "network without short"),
       col=rainbow(8), lty=1, cex=0.8)
dev.off()

