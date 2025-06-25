library(tidyverse)
library(ggpubr)
library(ggpmisc)
library(cowplot)
#load data
network_dat <- read_csv("network_data.csv")%>%select(pond_code,isolation,invasion)
network_dat$isolation <- log10(network_dat$isolation)
diet_change_dat <- read_csv("fig2b-c_diet_change_data.csv")%>%
  left_join(network_dat)%>%
  filter(invasion=="N")

fig4s4 <- ggplot(data=diet_change_dat,aes(isolation,scaled.breadth ))+
  geom_point(aes(color=invasion,fill=invasion),size=3,shape=21,alpha = 5/10)+
  labs(y="Diet breadth",x="Distance to closest mainland [log10(km)]")+
  scale_fill_manual(values = c("#1D91C0"))+  #,"#810F7C"
  scale_color_manual(values = c("#1D91C0"))+  #,"#810F7C"
  stat_smooth(method="lm",size=1.5,linetype=1,colour="#1D91C0")+
  stat_poly_eq(aes(label=paste(..eq.label..,..rr.label..,..p.value.label..,sep = "~~~~")),
               size = 3)
fig4s4
ggsave(filename=paste0('figure/fig4s4.png'),plot=fig4s4,width=10 ,height=6.5)