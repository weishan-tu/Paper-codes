rm(list=ls())
library(bipartite)
library(tidyverse)
library(reshape2)
#load data
site_network <- read_csv("example_site1_JT_site060.csv")
network_matrix <- site_network%>%
  reshape2::acast(formula = prey~species,fun.aggregate = sum,
                  value.var = "RRA")%>%
  as.matrix()

####null model 
##
site_matrix2 <- round(site_matrix*100)
nulls <- nullmodel(site_matrix2, method=1)  ##method: r2dtable
for (i in 1:1000) {
  nulls[[i]] <- nulls[[i]]/100
}
##########################
#####network analysis#####
##########################
###Connectance
net_connect <- networklevel(site_matrix, 
                            index="connectance")
Inulls_connect <- sapply(nulls, function(x) networklevel(x, index="connectance" ))
# compute z-scores (= standardscores) 
z_connect <- (net_connect-mean(Inulls_connect))/sd(Inulls_connect)
p_Q <-  2*pnorm(-abs(z_connect)) 
###Modularity
Modularity_Q <- c(Modularity.Q=computeModules(site_matrix)@likelihood)
Inulls_Q <- sapply(nulls, function(x) computeModules(x)@likelihood)
# compute z-scores (= standardscores) 
z_Modularity_Q <- (Modularity_Q-mean(Inulls_Q))/sd(Inulls_Q)
p_Q <-  2*pnorm(-abs(z_Modularity_Q))

###Nestedness 
WNODF <- nest.smdm(site_matrix, weighted=T)$WNODFmatrix
Inulls_WNODF <- sapply(nulls, function(x) nest.smdm(x, weighted=T)$WNODFmatrix)
z_WNODF <- (WNODF-mean(Inulls_WNODF))/sd(Inulls_WNODF)
p_WNODF <- 2*pnorm(-abs(z_WNODF))


##########################
###centrality analysis####
##########################
##degree
Predator_degree <- specieslevel(site_matrix,index=c("degree"), level = "higher")%>%as.data.frame()

Inulls_degree <- sapply(nulls, function(x) specieslevel(x,index=c("degree"), level = "higher"))
Predator_degree$mean_null_degree <- rowMeans(data.frame(Inulls_degree))
Predator_degree$sd_null_degree <- c(apply(data.frame(Inulls_degree), 1, sd))
# compute z-scores (= standardscores) 
Predator_degree$z_degree <- (Predator_degree$degree-Predator_degree$mean_null_degree)/Predator_degree$sd_null_degree
##closeness
Predator_closeness <- specieslevel(as.matrix(site_matrix),index=c("closeness"), level = "higher")%>%
  as.data.frame()%>%
  dplyr::select(weighted.closeness)
Inulls_closeness <- sapply(nulls, function(x) specieslevel(x,index=c("closeness"), level = "higher"))
Inulls_closeness_list <- lapply(1:1000, function(i) Inulls_closeness[,i]$weighted.closeness)
Predator_closeness$mean_null_closeness <- rowMeans(data.frame(Inulls_closeness_list))
Predator_closeness$sd_null_closeness <- c(apply(data.frame(Inulls_closeness_list), 1, sd))
# compute z-scores (= standardscores) 
Predator_closeness$z_closeness <- (Predator_closeness$weighted.closeness-Predator_closeness$mean_null_closeness)/Predator_closeness$sd_null_closeness
