# load package
library(tidyverse)
library(plot3D)
library(cowplot)
library(bipartite)
#plot fig2 Comparison of the predator‒prey network and diet between the invaded and control ponds
#load data
network_dat <- read_csv("network_data.csv")
#plot fig2a
plot3d_netw <- network_dat%>%filter(!is.na(Connectance))
plot3d_netw$invasion <- as.factor(plot3d_netw$invasion)
Cairo::CairoPNG(
  filename = paste0("figure/fig2a_network_3d.png "), # 文件名称
  width = 10,           # 宽
  height = 10,          # 高
  units = "in",        # 单位
  dpi = 600)           # 分辨率

scatter3D(
  x = plot3d_netw$Connectance,
  y = plot3d_netw$Nestedness,
  z = plot3d_netw$Modularity,
  colvar = as.numeric(dat$invasion), # 颜色映射变量
  col = c("#1D91C0", "#FF8000"), # 自定义颜色
  pch = 16,
  theta = 30,               # 水平旋转角度
  phi = 20,                 # 垂直倾斜角度
  bty = "b2",                # 背景样式（"g"=网格）
  xlab = "Connectance",
  ylab = "Nestedness",
  zlab = "Modularity",
  main = "Network Structure"
)
dev.off()

# example site1
ex_site1 <- read_csv("example_site1_JT_site060.csv")
ex_site1_matrix <- ex_site1%>%
  reshape2::acast(formula = prey~species,fun.aggregate = sum,
                  value.var = "RRA")%>%
  as.data.frame()
seq.high <- colnames(site_matrix)
site_matrix_prec <- ex_site1%>% 
  group_by(species)%>%dplyr::arrange( desc(RRA))%>%
  ungroup()%>%
  dplyr::arrange( match(species,seq.high))%>%
  group_by(prey)%>% filter(RRA == max(RRA))
seq.low <- unique(site_matrix_prec$prey)  
seq_spec=list(seq.high=seq.high,seq.low=seq.low)

high.colors=c("#1D91C0")
low.colors=c("#1D91C0")
int.colors=c("#1D91C0")
Cairo::CairoPNG(
  filename = paste0("figure/fig2a_ex1.png "), width = 8,height = 5,  units = "in",  dpi = 600)

plotweb (ex_site1_matrix, method="normal", arrow="center",
         col.low= low.colors,    col.high= high.colors,   col.interaction= (int.colors),sequence=seq_spec,
         bor.col.low="transparent",  bor.col.high= "transparent",    bor.col.interaction = "transparent" )
dev.off()
# example site2
ex_site2 <- read_csv("example_site2_DS_site021.csv")
ex_site2_matrix <- ex_site2%>%
  reshape2::acast(formula = prey~species,fun.aggregate = sum,
                  value.var = "RRA")%>%
  as.data.frame()
seq.high <- colnames(site_matrix)
site_matrix_prec <- ex_site2%>% 
  group_by(species)%>%dplyr::arrange( desc(RRA))%>%
  ungroup()%>%
  dplyr::arrange( match(species,seq.high))%>%
  group_by(prey)%>% filter(RRA == max(RRA))
seq.low <- unique(site_matrix_prec$prey)  
seq_spec=list(seq.high=seq.high,seq.low=seq.low)

high.colors=c("#FF8000")
low.colors=c("#FF8000")
int.colors=c("#FF8000")
Cairo::CairoPNG(
  filename = paste0("figure/fig2a_ex2.png "), width = 8,height = 5,  units = "in",  dpi = 600)
plotweb (ex_site2_matrix, method="normal", arrow="center",
         col.low= low.colors,    col.high= high.colors,   col.interaction= (int.colors),sequence=seq_spec,
         bor.col.low="transparent",  bor.col.high= "transparent",    bor.col.interaction = "transparent" )
dev.off()
#fig2b 
diet_change_dat <- read_csv("fig2b-c_diet_change_data.csv")
bullfrog_m <- diet_change_dat%>%filter(species=="Lithobates_catesbeianus")
diet_change_nat <- diet_change_dat%>%filter(species%in%c("Fejervarya_multistriata","Pelophylax_nigromaculatus","Microhyla_fissipes","Bufo_gargarizans"))

diet_change_nat$species <- factor(diet_change_nat$species,levels = c("Fejervarya_multistriata","Pelophylax_nigromaculatus","Microhyla_fissipes","Bufo_gargarizans"))
fig2b <- diet_change_nat%>%
  ggplot(aes(x=species, y=log10(prey_no) , color=invasion,shape=species)) +
  geom_jitter(position=position_jitter(0.25), alpha=0.4) +
  geom_boxplot(width = 0.5, alpha = 0.7) +  
  scale_shape_manual(values = shape_list)+
  # stat_summary(fun.data="mean_sdl",  fun.args = list(mult=1), color="red", size=0.8, shape=17) +
  scale_color_manual(values=c("#3498DB", "#F39C12")) +
  xlab("") + ylab("Species Degree")+
  geom_hline(yintercept=mean(log10(bullfrog_m$prey_no)), linetype="dashed", 
             color = "red", size=1)+
  stat_compare_means(method = "kruskal.test",label="p.signif", label.x = 1.5)+
  labs(title = "Species Degree")

#fig2c
fig2c <- diet_change_nat%>%
  ggplot(aes(x=species, y=scaled.breadth , color=invasion,shape=species)) +
  geom_jitter(position=position_jitter(0.25), alpha=0.4) +
  geom_boxplot(width = 0.5, alpha = 0.7) +  
  scale_shape_manual(values = shape_list)+
  # stat_summary(fun.data="mean_sdl",  fun.args = list(mult=1), color="red", size=0.8, shape=17) +
  scale_color_manual(values=c("#3498DB", "#F39C12")) +
  geom_hline(yintercept=mean(bullfrog_m$scaled.breadth), linetype="dashed", 
             color = "red", size=1)+
  xlab("") + ylab("Diet breadth")+
  stat_compare_means(method = "kruskal.test",label="p.signif", label.x = 1.5)+
  labs(title = "Diet breadth")


fig2bc <- plot_grid(fig2b,fig2c,ncol = 1, labels = c('B', 'C'))  
ggsave(filename=paste0('figure/fig2b-c_native_frog_diet_change.png'),plot=fig2bc,width=8 ,height=6)

#fig2d-f
fig2d <- network_dat%>%ggplot(aes(x=invasion, y=Connectance  , color=invasion)) +
  geom_jitter(position=position_jitter(0.25), alpha=0.4) +
  geom_boxplot(width = 0.5, alpha = 0.7) +  # 箱线图透明度
  # stat_summary(fun.data="mean_sdl",  fun.args = list(mult=1), color="red", size=0.8, shape=17) +
  scale_color_manual(values=c("#3498DB", "#F39C12")) +
  xlab("") + ylab("Connectance (z-scores)")+
  stat_compare_means(method = "kruskal.test",label="p.signif", label.x = 1.5)
fig2e <- network_dat%>%ggplot(aes(x=invasion, y=Modularity  , color=invasion)) +
  geom_jitter(position=position_jitter(0.25), alpha=0.4) +
  geom_boxplot(width = 0.5, alpha = 0.7) +  # 箱线图透明度
  # stat_summary(fun.data="mean_sdl",  fun.args = list(mult=1), color="red", size=0.8, shape=17) +
  scale_color_manual(values=c("#3498DB", "#F39C12")) +
  xlab("") + ylab("Modularity (z-scores)")+
  stat_compare_means(method = "kruskal.test",label="p.signif", label.x = 1.5)

fig2f <- network_dat%>%ggplot(aes(x=invasion, y=Nestedness  , color=invasion)) +
  geom_jitter(position=position_jitter(0.25), alpha=0.4) +
  geom_boxplot(width = 0.5, alpha = 0.7) +  # 箱线图透明度
  # stat_summary(fun.data="mean_sdl",  fun.args = list(mult=1), color="red", size=0.8, shape=17) +
  scale_color_manual(values=c("#3498DB", "#F39C12")) +
  xlab("") + ylab("Nestdeness (z-scores)")+
  stat_compare_means(method = "kruskal.test",label="p.signif", label.x = 1.5)
fig2df <- plot_grid(fig2d,fig2e,fig2f,
                  ncol = 1, labels = c('D', 'E', 'F')) 
ggsave(filename=paste0('figure/fig2d-f_network_change.png'),plot=fig2bc,width=3 ,height=8)