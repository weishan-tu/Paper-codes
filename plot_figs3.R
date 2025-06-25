library(tidyverse)
library(ggpubr)
library(ggpmisc)
library(cowplot)
#load data
network_dat <- read_csv("network_data.csv")
network_dat$area <- log10(network_dat$area)
network_control <- network_dat%>%filter(invasion=="N")%>%mutate(type="Control")
network_addinv <- network_dat%>%mutate(type="Invaded")
network_all <- rbind(network_control,network_addinv)
network_all$type <- factor(network_all$type,levels = c("Control","Invaded"))
figs3a <- ggplot()+
    geom_point(data=network_addinv,aes(area,Connectance,color=invasion,fill=invasion),size=3,shape=21,alpha = 5/10)+
  labs(y="Connectance (z-scores)",x="")+
  # labs(y=paste0(ind),x=paste0("log10(KM2)"))+
  scale_fill_manual(values = c("#1D91C0","#FF8000","#1D91C0","#FF8000"))+  #,"#810F7C"
  scale_color_manual(values = c("#1D91C0","#FF8000","#1D91C0","#FF8000"))+  #,"#810F7C"
  stat_smooth(data=network_all,aes(area,Connectance,color=type,fill=type),method="lm",size=1.5,linetype=2)+
  stat_poly_eq(data=network_all,aes(area,Connectance,color=type,fill=type,label=paste(..eq.label..,..rr.label..,..p.value.label..,sep = "~~~~")),
               size = 3) 


figs3b <- ggplot()+
  geom_point(data=network_addinv,aes(area,Modularity ,color=invasion,fill=invasion),size=3,shape=21,alpha = 5/10)+
  labs(y="Modularity (z-scores)",x="Island area [log10(km2)]")+
  # labs(y=paste0(ind),x=paste0("log10(KM2)"))+
  scale_fill_manual(values = c("#1D91C0","#FF8000","#1D91C0","#FF8000"))+  #,"#810F7C"
  scale_color_manual(values = c("#1D91C0","#FF8000","#1D91C0","#FF8000"))+  #,"#810F7C"
  stat_smooth(data=network_all,aes(area,Modularity ,color=type,fill=type),method="lm",size=1.5,linetype=2)+
  stat_poly_eq(data=network_all,aes(area,Modularity,color=type,fill=type,label=paste(..eq.label..,..rr.label..,..p.value.label..,sep = "~~~~")),
               size = 3) 

figs3c <- ggplot()+
  geom_point(data=network_addinv,aes(area,Nestedness  ,color=invasion,fill=invasion),size=3,shape=21,alpha = 5/10)+
  labs(y="Nestedness (z-scores)",x="")+
  # labs(y=paste0(ind),x=paste0("log10(KM2)"))+
  scale_fill_manual(values = c("#1D91C0","#FF8000","#1D91C0","#FF8000"))+  #,"#810F7C"
  scale_color_manual(values = c("#1D91C0","#FF8000","#1D91C0","#FF8000"))+  #,"#810F7C"
  stat_smooth(data=network_all,aes(area,Nestedness  ,color=type,fill=type),method="lm",size=1.5,linetype=2)+
  stat_poly_eq(data=network_all,aes(area,Nestedness,color=type,fill=type,label=paste(..eq.label..,..rr.label..,..p.value.label..,sep = "~~~~")),
               size = 3) 

network_all <- rbind(network_control,network_addinv)
div_isl <- median(unique(network_all$area))
network_all <- network_all %>%
  mutate(group_area = ifelse(area >= div_isl, "Large", "Small"))

network_all$group_area <-  factor(network_all$group_area,levels = c( "Small","Large"))
network_all$invasion <- ifelse(network_all$invasion == "N", "Control", "Invaded")


figs3d <- network_all%>%ggplot(aes(x=invasion, y=Connectance  , color=invasion)) +
  geom_jitter(position=position_jitter(0.25), alpha=0.4) +
  geom_boxplot(width = 0.5, alpha = 0.7) +  # 
  scale_color_manual(values=c("#3498DB", "#F39C12")) +
  xlab("") + ylab("Connectance (z-scores)")+
  stat_compare_means(method = "kruskal.test",label="p.signif", label.x = 1.5)+
  facet_wrap(~ group_area) +         # 
  labs(title = "Connectance (z-scores)") 

figs3e <- network_all%>%ggplot(aes(x=invasion, y=Modularity  , color=invasion)) +
  geom_jitter(position=position_jitter(0.25), alpha=0.4) +
  geom_boxplot(width = 0.5, alpha = 0.7) +  # 
  scale_color_manual(values=c("#3498DB", "#F39C12")) +
  xlab("") + ylab("Modularity (z-scores)")+
  stat_compare_means(method = "kruskal.test",label="p.signif", label.x = 1.5)+
  facet_wrap(~ group_area) +         # 
  labs(title = "Modularity (z-scores)") 

figs3f <- network_all%>%ggplot(aes(x=invasion, y=Nestedness  , color=invasion)) +
  geom_jitter(position=position_jitter(0.25), alpha=0.4) +
  geom_boxplot(width = 0.5, alpha = 0.7) +  # 
  scale_color_manual(values=c("#3498DB", "#F39C12")) +
  xlab("") + ylab("Nestdeness (z-scores)")+
  stat_compare_means(method = "kruskal.test",label="p.signif", label.x = 1.5)+
  facet_wrap(~ group_area) +         # 
  labs(title = "Nestdeness (z-scores)") 



figs3 <- plot_grid(figs3a,figs3b,figs3c,
                  figs3d,figs3e,figs3f,
                  
                  ncol = 3, labels = "AUTO")  ##,'D', 'E', 'F'

figs3
ggsave(filename=paste0('figure/figs3.png'),plot=figs3,width=10 ,height=6.5)
