library(tidyverse)
library(ggpubr)
library(ggpmisc)
library(cowplot)
#load_data
centrality_dat <- read_csv("fig3_centrality_data.csv")
library(hrbrthemes)
shape_list <- c(
  "Fejervarya_multistriata"=16,"Microhyla_fissipes"=0,
  "Pelophylax_nigromaculatus"=1,"Pelophylax_plancyi"=8,
  "Bufo_gargarizans"=3,"Rana_zhenhaiensis"=4 ,
  "Hylarana_latouchii"=5,"Hyla_chinensis"=6,         
  "Quasipaa_spinosa"=7,"Lithobates_catesbeianus"=17
)
# A basic scatterplot with color depending on Species
fig3 <- ggplot(centrality_dat, aes(x=degree, y=closeness, shape=species, color=invasion)) +  #, size=degree
  geom_point(size=3) +
  scale_color_manual(values=c("#3498DB", "#F39C12")) +
  scale_fill_manual(values=c("#3498DB", "#F39C12")) +
  scale_shape_manual(values = shape_list)+
  xlab("Degree Centrality (z-scores)") + ylab("Closeness Centrality (z-scores)")+
  theme_bw()

fig3
ggsave(filename=paste0('figure/fig3.png'),plot=fig3,width=10 ,height=8)
