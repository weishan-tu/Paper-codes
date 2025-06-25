library(tidyverse)
library(ggpubr)
library(ggpmisc)
library(cowplot)
#load data
network_dat <- read_csv("network_data.csv")
network_control <- dat%>%filter(invasion=="N")%>%mutate(type="Control")
network_addinv <- dat%>%mutate(type="Invaded")
network_all <- rbind(network_control,network_addinv)
network_all$type <- factor(network_all$type,levels = c("Control","Invaded"))
div_isl <- median(unique(network_all$isolation))
network_all <- network_all %>%
  mutate(group = ifelse(isolation >= div_isl, "Far", "Near"))

network_all$group <-  factor(network_all$group,levels = c( "Near","Far"))
network_all$invasion <- ifelse(network_all$invasion == "N", "Control", "Invaded")

figs2a <- network_all%>%ggplot(aes(x=invasion, y=Connectance  , color=invasion)) +
  geom_jitter(position=position_jitter(0.25), alpha=0.4) +
  geom_boxplot(width = 0.5, alpha = 0.7) +  # 箱线图透明度
  # stat_summary(fun.data="mean_sdl",  fun.args = list(mult=1), color="red", size=0.8, shape=17) +
  scale_color_manual(values=c("#3498DB", "#F39C12")) +
  xlab("") + ylab("Connectance (z-scores)")+
  stat_compare_means(method = "kruskal.test",label="p.signif", label.x = 1.5)+
  facet_wrap(~ group) +        
  labs(title = "Connectance (z-scores)") 

figs2b <- network_all%>%ggplot(aes(x=invasion, y=Modularity  , color=invasion)) +
  geom_jitter(position=position_jitter(0.25), alpha=0.4) +
  geom_boxplot(width = 0.5, alpha = 0.7) +  # 箱线图透明度
  # stat_summary(fun.data="mean_sdl",  fun.args = list(mult=1), color="red", size=0.8, shape=17) +
  scale_color_manual(values=c("#3498DB", "#F39C12")) +
  xlab("") + ylab("Modularity (z-scores)")+
  stat_compare_means(method = "kruskal.test",label="p.signif", label.x = 1.5)+
  facet_wrap(~ group) +        
  labs(title = "Modularity (z-scores)") 

figs2c <- network_all%>%ggplot(aes(x=invasion, y=Nestedness  , color=invasion)) +
  geom_jitter(position=position_jitter(0.25), alpha=0.4) +
  geom_boxplot(width = 0.5, alpha = 0.7) +  # 箱线图透明度
  # stat_summary(fun.data="mean_sdl",  fun.args = list(mult=1), color="red", size=0.8, shape=17) +
  scale_color_manual(values=c("#3498DB", "#F39C12")) +
  xlab("") + ylab("Nestdeness (z-scores)")+
  stat_compare_means(method = "kruskal.test",label="p.signif", label.x = 1.5)+
  facet_wrap(~ group) +        
  labs(title = "Nestdeness (z-scores)") 


figs2 <- plot_grid(figs2a,figs2b,figs2c,
                  
                  ncol = 3, labels = "AUTO")  ##,'D', 'E', 'F'
figs2 
ggsave(filename=paste0('figure/figs2.png'),plot=pall,width=9 ,height=3)