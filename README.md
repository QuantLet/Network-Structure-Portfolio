# A Network View on a Portfolio Risk Decomposition

[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="880" alt="Visit QuantNet">](http://quantlet.de/index.php?p=info)

```yaml

Name of Quantlet: 'A Network View on a Portfolio Risk Decomposition'

Keywords: 'Network risk', 'Portfolio risk decomposition', 'Dantzig-type estimator', 'Portfolio optimization', 'High dimensions', 'Network portfolio'

Abstract: 'Portfolio performance depends on interactions inside the network of assets. In high dimensions, these interactions are rare and sparse, therefore an inherent and indispensable portfolio risk, network risk, is difficult to quantify. Here a portfolio risk decomposition method is proposed that leads to analytical solutions that provide insights into the accounts for network and idiosyncratic risks. The technical platform is based on Dantzig-type estimator for covariance matrix and eigenvector centrality, which helps reduce estimation error in high-dimensional cases for portfolio optimization. Empirical results show that the network portfolio approach outperforms existing methods out-of-sample on a real dataset and demonstrate the solidity and reliability of our network portfolio and estimation methods from a practical perspective.'

Author: 'Zijin Wang'

Submitted: '15. November 2023, Zijin Wang'

```

Requirements
----
R, Gephi
  
R version 4.1.2

Packages:

magick_2.7.3
glasso_1.11
MST_2.2
MTS_1.1.1
Matrix_1.3-4
pracma_2.3.8
limSolve_1.5.6
quantmod_0.4.18
nlshrink_1.0.1
ROI.plugin.glpk_1.0-0
ROI.plugin.quadprog_1.0-0
fPortfolio_3042.83.1
tidyr_1.2.0
ggpubr_0.4.0
readxl_1.4.0
quadprog_1.5-8
igraph_1.2.11
RiskPortfolios_2.1.7
plotly_4.10.0
ggplot2_3.4.3
timeSeries_3062.100
PerformanceAnalytics_2.0.4
PortfolioAnalytics_1.1.0
xts_0.12.1
zoo_1.8-10
