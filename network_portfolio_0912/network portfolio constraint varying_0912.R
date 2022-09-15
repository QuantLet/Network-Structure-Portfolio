# In this file, we construct network by correlation matrix

rm(list = ls())

setwd("~/Documents/Code/Network structure based portfolio/weights of different strategies/network_portfolio_0912")

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
for(t in 1: length(W_in)){
  C_in[[(t)]] =cor(W_in[[(t)]])
  ER_in[[(t)]] = colMeans(W_in[[(t)]])
  COV_in[[(t)]] = cov(W_in[[(t)]])
  # network_port = network.portfolio(W_in[[(t)]])
  network_port = network.correlation(W_in[[(t)]])
  # EC_in[[(t)]] <- eigen_centrality(network_port,directed = FALSE, scale = TRUE)$vector
  # EC_in[[t]]<-eigen(C_in[[(t)]])$vector[,1]
  # network_port<-network.abs(W_in[[(t)]])
  EC_in[[(t)]] <- eigen_centrality(network_port,directed = FALSE, scale = TRUE)$vector
  BetweennessCentrality[[(t)]] <- centr_degree(network_port)
  ClosenessCentrality[[(t)]] <- eigen_centrality(network_port)
  DegreeCentrality[[(t)]] <- eigen_centrality(network_port)
  network_in<-network.posi.naga(W_in[[(t)]])
  EC.posi_in[[(t)]] <- eigen_centrality(network_in$positive,directed = FALSE, scale = TRUE)$vector
  EC.nega_in[[(t)]] <- eigen_centrality(network_in$negative,directed = FALSE, scale = TRUE)$vector
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

#### weights of different portfolios in each rolling window ####
##### minimum variance portfolio  #####
w_minVar<-list()
cumureturn_minVar<-list()
for(t in 1: length(W_in)){
  portf_minVar =globalMin.portfolio(ER_in[[(t)]],COV_in[[(t)]])
  w_minVar[[(t)]] =portf_minVar$weights
  aus<-as.matrix(repmat(w_minVar[[(t)]],7,1)*W_out[[t]])
  cumureturn_minVar[[t]]<-rowSums(aus)
}
return_minVar<-as.matrix(cbind(unlist(cumureturn_minVar)))
cumureturn_minVar<-cumsum(return_minVar)

##### minimum variance portfolio no short #####
w_minVar_noshort<-list()
cumureturn_minVar_noshort<-list()
for(t in 1: length(W_in)){
  portf_minVar =globalMin.portfolio(ER_in[[(t)]],COV_in[[(t)]],FALSE)
  w_minVar_noshort[[(t)]] =portf_minVar$weights
  aus<-as.matrix(repmat(w_minVar_noshort[[(t)]],7,1)*W_out[[t]])
  cumureturn_minVar_noshort[[t]]<-rowSums(aus)
}
return_minVar_noshort<-as.matrix(cbind(unlist(cumureturn_minVar_noshort)))
cumureturn_minVar_noshort<-cumsum(return_minVar_noshort)

##### mean variance portfolio  #####
w_meanVar<-list()
cumureturn_meanVar<-list()
for(t in 1: length(W_in)){
  portf_meanVar =efficient.portfolio(ER_in[[(t)]],COV_in[[(t)]],mean(ER_in[[t]]))
  w_meanVar[[(t)]] =portf_meanVar$weights
  aus<-as.matrix(repmat(w_meanVar[[(t)]],7,1)*W_out[[t]])
  cumureturn_meanVar[[t]]<-rowSums(aus)
}
return_meanVar<-as.matrix(cbind(unlist(cumureturn_meanVar)))
cumureturn_meanVar<-cumsum(return_meanVar)

##### mean variance portfolio no short #####
w_meanVar_noshort<-list()
cumureturn_meanVar_noshort<-list()
for(t in 1: length(W_in)){
  portf_meanVar =efficient.portfolio(ER_in[[(t)]],COV_in[[(t)]],mean(ER_in[[t]]),shorts = FALSE)
  w_meanVar_noshort[[(t)]] =portf_meanVar$weights
  aus<-as.matrix(repmat(w_meanVar_noshort[[(t)]],7,1)*W_out[[t]])
  cumureturn_meanVar_noshort[[t]]<-rowSums(aus)
}
return_meanVar_noshort<-as.matrix(cbind(unlist(cumureturn_meanVar_noshort)))
cumureturn_meanVar_noshort<-cumsum(return_meanVar_noshort)

##### equally portfolio #####
w_equal<-list()
cumureturn_temporal<-list()
centrality_equal_portfolio<-list()
for(t in 1: length(W_in)){
  w_equal[[(t)]] =matrix(1/465,1,465)
  aus<-as.matrix(repmat(w_equal[[(t)]],7,1)*W_out[[t]])
  cumureturn_temporal[[t]]<-rowSums(aus)
  centrality_equal_portfolio[[t]]<-as.double(w_equal[[(t)]]%*%EC_in[[(t)]])
}
return_equal<-as.matrix(cbind(unlist(cumureturn_temporal)))
cumureturn_equal<-cumsum(return_equal)
centrality_equal_portfolio<-as.matrix(cbind(unlist(centrality_equal_portfolio)))

##### network portfolio #####
### default setting centrality constraint 0.8 ###
###### network portfolio 1 constraint ######
####### network portfolio fixed constraint 0.8 #######
w_network<-list()
cumureturn_temporal<-list()
for(t in 1: length(W_in)){
  ## compute global minimum variance portfolio without short ##
  net.gmin.port = network.efficient.portfolio(EC_in[[(t)]], COV_in[[(t)]],0.8)
  w_network[[(t)]] =net.gmin.port$weights
  aus<-as.matrix(repmat(w_network[[(t)]],7,1)*W_out[[t]])
  cumureturn_temporal[[t]]<-rowSums(aus)
}
return_network<-as.matrix(cbind(unlist(cumureturn_temporal)))
cumureturn_network<-cumsum(return_network)

####### network portfolio fixed constraint 0.8 no short #######
w_network_noshort<-list()
cumureturn_temporal<-list()
for(t in 1: length(W_in)){
  ## compute global minimum variance portfolio without short ##
  net.gmin.port = network.efficient.portfolio(EC_in[[(t)]], COV_in[[(t)]],0.8,FALSE)
  w_network_noshort[[(t)]] =net.gmin.port$weights
  aus<-as.matrix(repmat(w_network_noshort[[(t)]],7,1)*W_out[[t]])
  cumureturn_temporal[[t]]<-rowSums(aus)
}
return_network_noshort<-as.matrix(cbind(unlist(cumureturn_temporal)))
cumureturn_network_noshort<-cumsum(return_network_noshort)

####### network portfolio varying constraint #######
cumureturn_network_vary_with_phi<-list()
return_network_vary_with_phi<-list()
phi_star<-seq(1,0,-0.05)
for (i in 1:length(phi_star)) {
  w_network<-list()
  cumureturn_temporal<-list()
  for(t in 1: length(W_in)){
    ## compute global minimum variance portfolio ##
    net.gmin.port = network.efficient.portfolio(EC_in[[(t)]], COV_in[[(t)]],phi_star[i],TRUE)
    # net.gmin.port = network.efficient.portfolio(EC.posi_in[[(t)]], COV_in[[(t)]],phi_star[i],TRUE)
    w_network[[(t)]] =net.gmin.port$weights
    aus<-as.matrix(repmat(w_network[[(t)]],7,1)*W_out[[t]])
    cumureturn_temporal[[t]]<-rowSums(aus)
  }
  return_network_vary_with_phi[[i]]<-as.matrix(cbind(unlist(cumureturn_temporal)))
  cumureturn_network_vary_with_phi[[i]]<-cumsum(return_network_vary_with_phi[[i]])
}


####### network portfolio varying constraint no short #######
cumureturn_network_vary_with_phi_noshort<-list()
return_network_vary_with_phi_noshort<-list()
phi_star<-seq(0.9,0.6,-0.05)
for (i in 1:length(phi_star)) {
  w_network<-list()
  cumureturn_temporal<-list()
  for(t in 1: length(W_in)){
    ## compute global minimum variance portfolio ##
    net.gmin.port = network.efficient.portfolio(EC_in[[(t)]], COV_in[[(t)]],phi_star[i],FALSE)
    w_network[[(t)]] =net.gmin.port$weights
    aus<-as.matrix(repmat(w_network[[(t)]],7,1)*W_out[[t]])
    cumureturn_temporal[[t]]<-rowSums(aus)
  }
  return_network_vary_with_phi_noshort[[i]]<-as.matrix(cbind(unlist(cumureturn_temporal)))
  cumureturn_network_vary_with_phi_noshort[[i]]<-cumsum(return_network_vary_with_phi_noshort[[i]])
}

####### network portfolio data-driven constraint #######
cumureturn_network_datadriven_phistar<-list()
return_network_datadriven_phistar<-list()
phi_star<-centrality_equal_portfolio
w_network<-list()
cumureturn_temporal<-list()
for(t in 1: length(W_in)){
  ## compute global minimum variance portfolio ##
  net.gmin.port = network.efficient.portfolio(EC_in[[(t)]], COV_in[[(t)]],phi_star[t],TRUE)
  w_network[[(t)]] =net.gmin.port$weights
  aus<-as.matrix(repmat(w_network[[(t)]],7,1)*W_out[[t]])
  cumureturn_temporal[[t]]<-rowSums(aus)
}
return_network_datadriven_phistar<-as.matrix(cbind(unlist(cumureturn_temporal)))
cumureturn_network_datadriven_phistar<-cumsum(return_network_datadriven_phistar)

####### network portfolio data-driven constraint no short #######
cumureturn_network_datadriven_phistar_noshort<-list()
return_network_datadriven_phistar_noshort<-list()
cumureturn_temporal<-list()
phi_star<-centrality_equal_portfolio
w_network<-list()
for(t in 1: length(W_in)){
  ## compute global minimum variance portfolio ##
  net.gmin.port = network.efficient.portfolio(EC_in[[(t)]], COV_in[[(t)]],phi_star[t],FALSE)
  w_network[[(t)]] =net.gmin.port$weights
  aus<-as.matrix(repmat(w_network[[(t)]],7,1)*W_out[[t]])
  cumureturn_temporal[[t]]<-rowSums(aus)
}
return_network_datadriven_phistar_noshort<-as.matrix(cbind(unlist(cumureturn_temporal)))
cumureturn_network_datadriven_phistar_noshort<-cumsum(return_network_datadriven_phistar_noshort)

###### network portfolio 2 constraint ######
### default setting centrality constraint 0.8 ###

####### network portfolio fixed constraint 0.8 #######
w_network<-list()
cumureturn_temporal<-list()
for(t in 1: length(W_in)){
  ## compute 2 constraint network portfolio ##
  net.gmin.port = network.2constraint.portfolio(EC_in[[(t)]],ER_in[[(t)]], COV_in[[(t)]],0.8,mean(ER_in[[(t)]]))
  w_network[[(t)]] =net.gmin.port$weights
  aus<-as.matrix(repmat(w_network[[(t)]],7,1)*W_out[[t]])
  cumureturn_temporal[[t]]<-rowSums(aus)
}
return_network_2constraint<-as.matrix(cbind(unlist(cumureturn_temporal)))
cumureturn_network_2constraint<-cumsum(return_network_2constraint)

####### network portfolio fixed constraint 0.8 no short #######
w_network_noshort<-list()
cumureturn_temporal<-list()
for(t in 1: length(W_in)){
  ## compute global minimum variance portfolio without short ##
  net.gmin.port = network.2constraint.portfolio(EC_in[[(t)]],ER_in[[t]], COV_in[[(t)]],0.8,mean(ER_in[[t]]),FALSE)
  w_network_noshort[[(t)]] =net.gmin.port$weights
  aus<-as.matrix(repmat(w_network_noshort[[(t)]],7,1)*W_out[[t]])
  cumureturn_temporal[[t]]<-rowSums(aus)
}
return_network_2constraint_noshort<-as.matrix(cbind(unlist(cumureturn_temporal)))
cumureturn_network_2constraint_noshort<-cumsum(return_network_2constraint_noshort)

####### network portfolio varying constraint #######
cumureturn_network_vary_with_phi_2constraint<-list()
return_network_vary_with_phi_2constraint<-list()
phi_star<-seq(1,0,-0.05)
for (i in 1:length(phi_star)) {
  w_network<-list()
  cumureturn_temporal<-list()
  for(t in 1: length(W_in)){
    ## compute global minimum variance portfolio ##
    net.gmin.port = network.2constraint.portfolio(EC_in[[(t)]],ER[[t]], COV_in[[(t)]],phi_star[i],ER[[t]],TRUE)
    # net.gmin.port = network.efficient.portfolio(EC.posi_in[[(t)]], COV_in[[(t)]],phi_star[i],TRUE)
    w_network[[(t)]] =net.gmin.port$weights
    aus<-as.matrix(repmat(w_network[[(t)]],7,1)*W_out[[t]])
    cumureturn_temporal[[t]]<-rowSums(aus)
  }
  return_network_vary_with_phi_2constraint[[i]]<-as.matrix(cbind(unlist(cumureturn_temporal)))
  cumureturn_network_vary_with_phi_2constraint[[i]]<-cumsum(return_network_vary_with_phi_2constraint[[i]])
}


####### network portfolio varying constraint no short #######
cumureturn_network_vary_with_phi_2constraint_noshort<-list()
return_network_vary_with_phi_2constraint_noshort<-list()
phi_star<-seq(0.9,0.6,-0.05)
for (i in 1:length(phi_star)) {
  w_network<-list()
  cumureturn_temporal<-list()
  for(t in 1: length(W_in)){
    ## compute global minimum variance portfolio ##
    net.gmin.port = network.2constraint.portfolio(EC_in[[(t)]],ER_in[[t]], COV_in[[(t)]],phi_star[i],mean(ER_in[[t]]),FALSE)
    w_network[[(t)]] =net.gmin.port$weights
    aus<-as.matrix(repmat(w_network[[(t)]],7,1)*W_out[[t]])
    cumureturn_temporal[[t]]<-rowSums(aus)
  }
  return_network_vary_with_phi_2constraint_noshort[[i]]<-as.matrix(cbind(unlist(cumureturn_temporal)))
  cumureturn_network_vary_with_phi_2constraint_noshort[[i]]<-cumsum(return_network_vary_with_phi_2constraint_noshort[[i]])
}

####### network portfolio data-driven constraint #######
cumureturn_network_datadriven_phistar_2constraint<-list()
return_network_datadriven_phistar_2constraint<-list()
phi_star<-centrality_equal_portfolio
w_network<-list()
cumureturn_temporal<-list()
for(t in 1: length(W_in)){
  ## compute global minimum variance portfolio ##
  net.gmin.port = network.2constraint.portfolio(EC_in[[(t)]],ER_in[[t]], COV_in[[(t)]],phi_star[t],mean(ER_in[[t]]),TRUE)
  w_network[[(t)]] =net.gmin.port$weights
  aus<-as.matrix(repmat(w_network[[(t)]],7,1)*W_out[[t]])
  cumureturn_temporal[[t]]<-rowSums(aus)
}
return_network_datadriven_phistar_2constraint<-as.matrix(cbind(unlist(cumureturn_temporal)))
cumureturn_network_datadriven_phistar_2constraint<-cumsum(return_network_datadriven_phistar_2constraint)

####### network portfolio data-driven constraint no short #######
cumureturn_network_datadriven_phistar_2constraint_noshort<-list()
return_network_datadriven_phistar_2constraint_noshort<-list()
cumureturn_temporal<-list()
phi_star<-centrality_equal_portfolio
w_network<-list()
for(t in 1: length(W_in)){
  ## compute global minimum variance portfolio ##
  net.gmin.port = network.2constraint.portfolio(EC_in[[(t)]],ER_in[[t]], COV_in[[(t)]],phi_star[t],mean(ER[[t]]),FALSE)
  w_network[[(t)]] =net.gmin.port$weights
  aus<-as.matrix(repmat(w_network[[(t)]],7,1)*W_out[[t]])
  cumureturn_temporal[[t]]<-rowSums(aus)
}
return_network_datadriven_phistar_2constraint_noshort<-as.matrix(cbind(unlist(cumureturn_temporal)))
cumureturn_network_datadriven_phistar_2constraint_noshort<-cumsum(return_network_datadriven_phistar_2constraint_noshort)

#### Figures and animations output ####
##### Figure: cumulative returns of 3 strategies #####
### minVar, minVar no short, equal: (color) 1:3 ###
Colors_Ret<-rainbow(12)
CumRet<-zoo(cbind(data.frame(cumureturn_minVar),
                  data.frame(cumureturn_minVar_noshort),
                  data.frame(cumureturn_equal)
                  ),
            order.by=as.Date(as.character(date), format="%Y-%m-%d"))
pngname<-paste0(getwd(),"/portfolio_comparison/cumulative_return_3_strategies",".png")
png(file = pngname, width=500, height=400, bg = "transparent")
plot(CumRet, screens=1, col=Colors_Ret[1:3], ylab="", xlab="")
# legend("topleft",legend=c("minVar", "minVar no short", "Equal"),
       # col=Colors_Ret[1:3], lty=1:2, cex=0.8)
dev.off()
##### Figure: cumulative returns of 5 strategies #####
### minVar, minVar no short, equal, meanVar, meanVar no short: (color) 1:3,4,7  ###
Colors_Ret<-rainbow(10)
CumRet<-zoo(cbind(data.frame(cumureturn_minVar),
                  data.frame(cumureturn_minVar_noshort),
                  data.frame(cumureturn_equal),
                  data.frame(cumureturn_meanVar),
                  data.frame(cumureturn_meanVar_noshort)
                  ),
order.by=as.Date(as.character(date), format="%Y-%m-%d"))
pngname<-paste0(getwd(),"/portfolio_comparison/cumulative_return_5_strategies",".png")
png(file = pngname, width=500, height=400, bg = "transparent")
plot(CumRet, screens=1, col=Colors_Ret[c(1:3,4,7)], ylab="", xlab="",title=paste0("Cumulate return with network Dantzig"))
legend("topleft",legend=c("minVar", "minVar no short", "Equal", "meanVar", "meanVar noshort"),
       col=Colors_Ret[c(1:3,4,7)], lty=1, cex=0.8)
dev.off()
##### Animation: cumulative returns with varying constraint of 5 strategies #####
###### network varying constraints ######
### minVar, minVar no short, equal: (color) 1:3 ###
### network varying constraint: (color) 9 ###
### network with phistar 0.8 no short: (color) 5  ###
Colors_Ret<-rainbow(12)
phi_star<-seq(1,0,-0.05)
fig = image_graph(width = 800, height = 640, res = 150, bg = "transparent")
for (i in 1:length(phi_star) ) {
  CumRet<-zoo(cbind(data.frame(cumureturn_minVar),
                    data.frame(cumureturn_minVar_noshort),
                    data.frame(cumureturn_equal),
                    data.frame(cumureturn_network_vary_with_phi[[i]]),
                    data.frame(cumureturn_network_noshort)),
              order.by=as.Date(as.character(date), format="%Y-%m-%d"))
  plot(CumRet, screens=1, col=Colors_Ret[c(1:3,9,5)], ylab="", xlab="",title=paste0("phi*=",phi_star[i]))
  # legend("topleft",legend=c("minVar", "minVar without short", "Equal", "network", "network without short"),
  # col=Colors_Ret[1:5], lty=1, cex=0.8)
}
dev.off()
animation <- image_animate(fig, fps = 1)
image_write(animation, paste0(getwd(), "/portfolio_comparison/pcumulative_return_5_strategies_varying_constraint.gif"))
###### network noshort varying constraints ######
### minVar, minVar no short, equal: (color) 1:3 ###
### network with phistat 0.8: (color) 9 ###
### network with varying constraint no short: (color) 5  ###
Colors_Ret<-rainbow(12)
phi_star<-seq(0.9,0.6,-0.05)
fig = image_graph(width = 800, height = 640, res = 150, bg = "transparent")
for (i in 1:length(phi_star) ) {
  CumRet<-zoo(cbind(data.frame(cumureturn_minVar),
                    data.frame(cumureturn_minVar_noshort),
                    data.frame(cumureturn_equal),
                    data.frame(cumureturn_network_vary_with_phi[[5]]),
                    data.frame(cumureturn_network_vary_with_phi_noshort[[i]])),
              order.by=as.Date(as.character(date), format="%Y-%m-%d"))
  plot(CumRet, screens=1, col=Colors_Ret[c(1:3,9,5)], ylab="", xlab="",title=paste0("phi*=",phi_star[i]))
  # legend("topleft",legend=c("minVar", "minVar without short", "Equal", "network", "network without short"),
  # col=Colors_Ret[1:5], lty=1:2, cex=0.8)
}
dev.off()
animation <- image_animate(fig, fps = 1)
image_write(animation, paste0(getwd(), "/portfolio_comparison/cumulative_return_5_strategies_noshort_varying_constraint.gif"))

##### Animation: cumulative returns with varying constraint of 7 strategies #####
###### network varying constraints ######
### minVar, minVar no short, equal, meanVar, meanVar no short: (color) 1:3,4,7 ###
### network varying constraint: (color) 9 ###
### network with phistar 0.8 no short: (color) 5   ###
Colors_Ret<-rainbow(12)
phi_star<-seq(1,0,-0.05)
fig = image_graph(width = 800, height = 640, res = 150, bg = "transparent")
for (i in 1:length(phi_star) ) {
  CumRet<-zoo(cbind(data.frame(cumureturn_minVar),
                    data.frame(cumureturn_minVar_noshort),
                    data.frame(cumureturn_equal),
                    data.frame(cumureturn_meanVar),
                    data.frame(cumureturn_meanVar_noshort),
                    data.frame(cumureturn_network_vary_with_phi[[i]]),
                    data.frame(cumureturn_network_noshort)),
              order.by=as.Date(as.character(date), format="%Y-%m-%d"))
  plot(CumRet, screens=1, col=Colors_Ret[c(1:3,4,7,9,5)], ylab="", xlab="",title=paste0("phi*=",phi_star[i]))
  # legend("topleft",legend=c("minVar", "minVar without short", "Equal", "network", "network without short"),
  # col=Colors_Ret[1:5], lty=1:2, cex=0.8)
}
dev.off()
animation <- image_animate(fig, fps = 1)
image_write(animation, paste0(getwd(), "/portfolio_comparison/cumulative_return_7_strategies_varying_constraint.gif"))
###### network noshort varying constraints ######
### minVar, minVar no short, equal, meanVar, meanVar no short: (color) 1:3,4,7 ###
### network with phistat 0.8: (color) 9  ###
### network with varying constraint no short: (color) 5   ###
Colors_Ret<-rainbow(12)
phi_star<-seq(0.9,0.6,-0.05)
fig = image_graph(width = 800, height = 640, res = 150, bg = "transparent")
for (i in 1:length(phi_star) ) {
  CumRet<-zoo(cbind(data.frame(cumureturn_minVar),
                    data.frame(cumureturn_minVar_noshort),
                    data.frame(cumureturn_equal),
                    data.frame(cumureturn_meanVar),
                    data.frame(cumureturn_meanVar_noshort),
                    data.frame(cumureturn_network_vary_with_phi[[5]]),
                    data.frame(cumureturn_network_vary_with_phi_noshort[[i]])),
              order.by=as.Date(as.character(date), format="%Y-%m-%d"))
  plot(CumRet, screens=1, col=Colors_Ret[c(1:3,4,7,9,5)], ylab="", xlab="",title=paste0("phi*=",phi_star[i]))
  # legend("topleft",legend=c("minVar", "minVar without short", "Equal", "network", "network without short"), col=Colors_Ret[1:5], lty=1, cex=0.8)
}
dev.off()
animation <- image_animate(fig, fps = 1)
image_write(animation, paste0(getwd(), "/portfolio_comparison/cumulative_return_7_strategies_noshort_varying_constraint.gif"))

##### Figures: cumulative returns with varying constraint of 5 strategies #####
###### network varying constraints ######
### minVar, minVar no short, equal: (color) 1:3 ###
### network varying constraint: (color) 9 ###
### network with phistar 0.8 no short: (color) 5   ###
phi_star<-seq(1,0,-0.05)
Colors_Ret<-rainbow(12)
for (i in 1:length(phi_star) ) {
  CumRet<-zoo(cbind(data.frame(cumureturn_minVar),
                    data.frame(cumureturn_minVar_noshort),
                    data.frame(cumureturn_equal),
                    data.frame(cumureturn_network_vary_with_phi[[i]]),
                    data.frame(cumureturn_network_noshort)),
              order.by=as.Date(as.character(date), format="%Y-%m-%d"))
  pngname<-paste0(getwd(),"/portfolio_comparison/5_strategies_varying_constraint/cumulative_return_with_constraint_",phi_star[i],".png")
  png(file = pngname, width=500, height=400, bg = "transparent")
  plot(CumRet, screens=1, col=Colors_Ret[c(1:3,9,5)], ylab="", xlab="")
  legend("topleft",legend=c("minVar", "minVar no short", "Equal", "network", "network no short"),
         col=Colors_Ret[c(1:3,9,5)], lty=1, cex=0.8)
  dev.off()
}

###### network noshort varying constraints ######
### minVar, minVar no short, equal: (color) 1:3 ###
### network with phistar 0.8: (color) 9 ###
### network varying constraint no short: (color) 5 ###
phi_star<-seq(0.9,0.6,-0.05)
Colors_Ret<-rainbow(12)
for (i in 1:length(phi_star) ) {
  CumRet<-zoo(cbind(data.frame(cumureturn_minVar),
                    data.frame(cumureturn_minVar_noshort),
                    data.frame(cumureturn_equal),
                    data.frame(cumureturn_network_vary_with_phi[[5]]),
                    data.frame(cumureturn_network_vary_with_phi_noshort[[i]])),
              order.by=as.Date(as.character(date), format="%Y-%m-%d"))
  pngname<-paste0(getwd(),"/portfolio_comparison/5_strategies_noshort_varying_constraint/cumulative_return_with_constraint_",phi_star[i],".png")
  png(file = pngname, width=500, height=400, bg = "transparent")
  plot(CumRet, screens=1, col=Colors_Ret[c(1:3,9,5)], ylab="", xlab="")
  legend("topleft",legend=c("minVar", "minVar no short", "Equal", "network", "network no short"),
         col=Colors_Ret[c(1:3,9,5)], lty=1, cex=0.8)
  dev.off()
}

##### Figures: cumulative returns with varying constraint of 7 strategies #####
###### network varying constraints ######
### minVar, minVar no short, equal, meanVar, meanVar no short: (color) 1:3,4,7 ###
### network varying constraint: (color) 9  ###
### network with phistar 0.8 no short: (color) 5   ###
Colors_Ret<-rainbow(12)
for (i in 1:length(phi_star) ) {
  CumRet<-zoo(cbind(data.frame(cumureturn_minVar),
                    data.frame(cumureturn_minVar_noshort),
                    data.frame(cumureturn_equal),
                    data.frame(cumureturn_meanVar),
                    data.frame(cumureturn_meanVar_noshort),
                    data.frame(cumureturn_network_vary_with_phi[[i]]),
                    data.frame(cumureturn_network_noshort)),
              order.by=as.Date(as.character(date), format="%Y-%m-%d"))
  pngname<-paste0(getwd(),"/portfolio_comparison/7_strategies_varying_constraint/cumulative_return_with_constraint_",phi_star[i],".png")
  png(file = pngname, width=500, height=400, bg = "transparent")
  plot(CumRet, screens=1, col=Colors_Ret[c(1:3,4,7,9,5)], ylab="", xlab="")
  legend("topleft",legend=c("minVar", "minVar no short", "Equal","meanVar", "meanVar no short",  "network", "network no short"),
         col=Colors_Ret[c(1:3,4,7,9,5)], lty=1, cex=0.8)
  dev.off()
}
###### network noshort varying constraints ######
### minVar, minVar no short, equal: (color) 1:3 ###
### network with phistar 0.8: (color) 9 ###
### network varying constraint no short: (color) 5 ###
phi_star<-seq(0.9,0.6,-0.05)
Colors_Ret<-rainbow(12)
for (i in 1:length(phi_star) ) {
  CumRet<-zoo(cbind(data.frame(cumureturn_minVar),
                    data.frame(cumureturn_minVar_noshort),
                    data.frame(cumureturn_equal),
                    data.frame(cumureturn_meanVar),
                    data.frame(cumureturn_meanVar_noshort),
                    data.frame(cumureturn_network_vary_with_phi[[5]]),
                    data.frame(cumureturn_network_vary_with_phi_noshort[[i]])),
              order.by=as.Date(as.character(date), format="%Y-%m-%d"))
  pngname<-paste0(getwd(),"/portfolio_comparison/7_strategies_noshort_varying_constraint/cumulative_return_with_constraint_",phi_star[i],".png")
  png(file = pngname, width=500, height=400, bg = "transparent")
  plot(CumRet, screens=1, col=Colors_Ret[c(1:3,4,7,9,5)], ylab="", xlab="")
  legend("topleft",legend=c("minVar", "minVar no short", "Equal","meanVar", "meanVar no short", "network", "network no short"),
         col=Colors_Ret[c(1:3,4,7,9,5)], lty=1, cex=0.8)
  dev.off()
}

##### Figure: cumulative returns with 4 network constraints of 5 strategies #####
###### network varying constraints ######
### minVar, minVar no short, equal: (color) 1:3 ###
### network with phistar=0.6:0.1:0.9: (color) 9,8,10,11 ###
### network with phistar 0.8 no short: (color) 5  ###
# cumulative returns in one figure
Colors_Ret<-rainbow(12)
phi_star<-seq(1,0,-0.05)
CumRet<-zoo(cbind(data.frame(cumureturn_minVar),
                  data.frame(cumureturn_minVar_noshort),
                  data.frame(cumureturn_equal),
                  data.frame(cumureturn_network_vary_with_phi[[3]]),
                  data.frame(cumureturn_network_vary_with_phi[[5]]),
                  data.frame(cumureturn_network_vary_with_phi[[7]]),
                  data.frame(cumureturn_network_vary_with_phi[[9]]),
                  data.frame(cumureturn_network_noshort)),
            order.by=as.Date(as.character(date), format="%Y-%m-%d"))
pngname<-paste0(getwd(),"/portfolio_comparison/cumulative_return_5_strategies_with_constraint_",
                phi_star[3],"_",phi_star[5],"_",phi_star[7],"_",phi_star[9],".png")
png(file = pngname, width=500, height=400, bg = "transparent")
plot(CumRet, screens=1, col=Colors_Ret[c(1:3,9,8,10,11,5)], ylab="", xlab="")
legend("topleft",legend=c("minVar", "minVar without short", "Equal",
                          paste("network","phi*=",phi_star[3]),paste("network","phi*=",phi_star[5]),
                          paste("network","phi*=",phi_star[7]),paste("network","phi*=",phi_star[9]),
                          "network no short"),
       col=Colors_Ret[c(1:3,9,8,10,11,5)], lty=1, cex=0.8)
dev.off()
###### network noshort varying constraints ######
### minVar, minVar no short, equal: (color) 1:3 ###
### network with phistar 0.8: (color) 9  ###
### network with phistar=0.6:0.1:0.9 no short: (color) 8,10,11,5   ###
# cumulative returns in one figure
Colors_Ret<-rainbow(12)
phi_star<-seq(0.9,0.6,-0.05)
CumRet<-zoo(cbind(data.frame(cumureturn_minVar),
                  data.frame(cumureturn_minVar_noshort),
                  data.frame(cumureturn_equal),
                  data.frame(cumureturn_network_vary_with_phi[[5]]),
                  data.frame(cumureturn_network_vary_with_phi_noshort[[1]]),
                  data.frame(cumureturn_network_vary_with_phi_noshort[[3]]),
                  data.frame(cumureturn_network_vary_with_phi_noshort[[5]]),
                  data.frame(cumureturn_network_vary_with_phi_noshort[[7]])
                  ),
            order.by=as.Date(as.character(date), format="%Y-%m-%d"))
pngname<-paste0(getwd(),"/portfolio_comparison/cumulative_return_5_strategies_noshort_with_constraint_",
                phi_star[1],"_",phi_star[3],"_",phi_star[5],"_",phi_star[7],".png")
png(file = pngname, width=500, height=400, bg = "transparent")
plot(CumRet, screens=1, col=Colors_Ret[c(1:3,9,8,10,11,5)], ylab="", xlab="")
legend("topleft",legend=c("minVar", "minVar without short", "Equal",paste("network","phi*=",0.8),
                          paste("network","noshort","phi*=",phi_star[1]),paste("network","noshort","phi*=",phi_star[3]),
                          paste("network","noshort","phi*=",phi_star[5]),paste("network","noshort","phi*=",phi_star[7])
                          ),
       col=Colors_Ret[c(1:3,9,8,10,11,5)], lty=1, cex=0.8)
dev.off()

##### Figure: cumulative returns with 4 network constraints of 7 strategies #####
###### network varying constraints ######
### minVar, minVar no short, equal, meanVar, meanVar no short: (color) 1:3,4,7 ###
### network with phistar=0.6:0.1:0.9: (color) 9,8,10,11  ###
### network with phistar 0.8 no short: (color) 5  ###
# cumulative returns in one figure
Colors_Ret<-rainbow(12)
phi_star<-seq(1,0,-0.05)
CumRet<-zoo(cbind(data.frame(cumureturn_minVar),
                  data.frame(cumureturn_minVar_noshort),
                  data.frame(cumureturn_equal),
                  data.frame(cumureturn_meanVar),
                  data.frame(cumureturn_meanVar_noshort),
                  data.frame(cumureturn_network_vary_with_phi[[3]]),
                  data.frame(cumureturn_network_vary_with_phi[[5]]),
                  data.frame(cumureturn_network_vary_with_phi[[7]]),
                  data.frame(cumureturn_network_vary_with_phi[[9]]),
                  data.frame(cumureturn_network_noshort)),
            order.by=as.Date(as.character(date), format="%Y-%m-%d"))
pngname<-paste0(getwd(),"/portfolio_comparison/cumulative_return_7_strategies_with_constraint_",
                phi_star[3],"_",phi_star[5],"_",phi_star[7],"_",phi_star[9],".png")
png(file = pngname, width=500, height=400, bg = "transparent")
plot(CumRet, screens=1, col=Colors_Ret[c(1:3,4,7,9,8,10,11,5)], ylab="", xlab="")
legend("topleft",legend=c("minVar", "minVar without short", "Equal","meanVar", "meanVar without short",
                          paste("network","phi*=",phi_star[3]),paste("network","phi*=",phi_star[5]),
                          paste("network","phi*=",phi_star[7]),paste("network","phi*=",phi_star[9]),
                          "network noshort"),
       col=Colors_Ret[c(1:3,4,7,9,8,10,11,5)], lty=1, cex=0.8)
dev.off()
###### network noshort varying constraints ######
### minVar, minVar no short, equal, meanVar, meanVar no short: (color) 1:3,4,7 ###
### network with phistar 0.8: (color) 9 ###
### network with phistar=0.6:0.1:0.9 no short: (color) 8,10,11,5 ###
# cumulative returns in one figure
Colors_Ret<-rainbow(12)
phi_star<-seq(0.9,0.6,-0.05)
CumRet<-zoo(cbind(data.frame(cumureturn_minVar),
                  data.frame(cumureturn_minVar_noshort),
                  data.frame(cumureturn_equal),
                  data.frame(cumureturn_meanVar),
                  data.frame(cumureturn_meanVar_noshort),
                  data.frame(cumureturn_network_vary_with_phi[[5]]),
                  data.frame(cumureturn_network_vary_with_phi_noshort[[1]]),
                  data.frame(cumureturn_network_vary_with_phi_noshort[[3]]),
                  data.frame(cumureturn_network_vary_with_phi_noshort[[5]]),
                  data.frame(cumureturn_network_vary_with_phi_noshort[[7]])
                  ),
            order.by=as.Date(as.character(date), format="%Y-%m-%d")
            )
pngname<-paste0(getwd(),"/portfolio_comparison/cumulative_return_7_strategies_noshort_with_constraint_",
                phi_star[1],"_",phi_star[3],"_",phi_star[5],"_",phi_star[7],".png")
png(file = pngname, width=500, height=400, bg = "transparent")
plot(CumRet, screens=1, col=Colors_Ret[c(1:3,4,7,9,8,10,11,5)], ylab="", xlab="")
legend("topleft",legend=c("minVar", "minVar without short", "Equal","meanVar", "meanVar without short",
                          "network phi*= 0.8",
                          paste("network noshort","phi*=",phi_star[1]),paste("network noshort","phi*=",phi_star[3]),
                          paste("network noshort","phi*=",phi_star[5]),paste("network noshort","phi*=",phi_star[7])
),
col=Colors_Ret[c(1:3,4,7,9,8,10,11,5)], lty=1, cex=0.8)
dev.off()

# Portfolio performance
n=length(cumureturn_meanVar)
# annual cumulative returns
cumureturn_minVar[n]/n*252
cumureturn_meanVar[n]/n*252
cumureturn_equal[n]/n*252
cumureturn_network[n]/n*252
cumureturn_network_2constraint[n]/n*252
cumureturn_minVar_noshort[n]/n*252
cumureturn_meanVar_noshort[n]/n*252
cumureturn_network_noshort[n]/n*252
cumureturn_network_datadriven_phistar[n]/n*252
cumureturn_network_datadriven_phistar_noshort[n]/n*252
cumureturn_network_vary_with_phi[[1]][n]/n*252
cumureturn_network_vary_with_phi[[2]][n]/n*252
cumureturn_network_vary_with_phi[[3]][n]/n*252
cumureturn_network_vary_with_phi[[4]][n]/n*252
cumureturn_network_vary_with_phi[[5]][n]/n*252
cumureturn_network_vary_with_phi[[7]][n]/n*252
cumureturn_network_vary_with_phi[[10]][n]/n*252
cumureturn_network_vary_with_phi[[13]][n]/n*252
cumureturn_network_vary_with_phi[[17]][n]/n*252
cumureturn_network_vary_with_phi[[20]][n]/n*252
cumureturn_network_vary_with_phi_noshort[[1]][n]/n*252
cumureturn_network_vary_with_phi_noshort[[3]][n]/n*252
cumureturn_network_vary_with_phi_noshort[[7]][n]/n*252
cumureturn_network_2constraint_noshort[n]/n*252
# std
std(return_minVar)*sqrt(252)
std(return_meanVar)*sqrt(252)
std(return_equal)*sqrt(252)
std(return_network)*sqrt(252)
std(return_network_2constraint)*sqrt(252)
std(return_minVar_noshort)*sqrt(252)
std(return_meanVar_noshort)*sqrt(252)
std(return_network_noshort)*sqrt(252)
std(return_network_2constraint_noshort)*sqrt(252)
# sharpe ratio
SharpeRatio(return_minVar)
SharpeRatio(return_meanVar)
SharpeRatio(return_equal)
SharpeRatio(return_network)
SharpeRatio(return_network_2constraint)
SharpeRatio(return_minVar_noshort)
SharpeRatio(return_meanVar_noshort)
SharpeRatio(return_network_noshort)
SharpeRatio(return_network_2constraint_noshort)
# skewness
skewness(return_minVar)
skewness(return_meanVar)
skewness(return_equal)
skewness(return_network)
skewness(return_network_2constraint)
skewness(return_minVar_noshort)
skewness(return_meanVar_noshort)
skewness(return_network_noshort)
skewness(return_network_2constraint_noshort)
# kurtosis
kurtosis(return_minVar)
kurtosis(return_meanVar)
kurtosis(return_equal)
kurtosis(return_network)
kurtosis(return_network_2constraint)
kurtosis(return_minVar_noshort)
kurtosis(return_meanVar_noshort)
kurtosis(return_network_noshort)
kurtosis(return_network_2constraint_noshort)
# drawdown
maxDrawdown(return_minVar)
maxDrawdown(return_meanVar)
maxDrawdown(return_equal)
maxDrawdown(return_network)
maxDrawdown(return_network_2constraint)
maxDrawdown(return_minVar_noshort)
maxDrawdown(return_meanVar_noshort)
maxDrawdown(return_network_noshort)
maxDrawdown(return_network_2constraint_noshort)
# autocorrelation
cor(return_minVar[1:n-1],return_minVar[2:n])
cor(return_meanVar[1:n-1],return_meanVar[2:n])
cor(return_equal[1:n-1],return_equal[2:n])
cor(return_network[1:n-1],return_network[2:n])
cor(return_network_2constraint[1:n-1],return_network_2constraint[2:n])
cor(return_minVar_noshort[1:n-1],return_minVar_noshort[2:n])
cor(return_meanVar_noshort[1:n-1],return_meanVar_noshort[2:n])
cor(return_network_noshort[1:n-1],return_network_noshort[2:n])
cor(return_network_2constraint_noshort[1:n-1],return_network_2constraint_noshort[2:n])
