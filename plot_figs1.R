library(tidyverse)
library(ggsci)
library(paletteer)
library(cartography)
library(ggpubr)
# # 使用factor函数指定排序顺序
# plot_dat <- left_join(RRA_order_mean_n,order_fly_last)
plot_dat <- read_csv("figs1_data.csv")
plot_dat$prey <- factor(plot_dat$prey,levels =c("Isopoda" ,"Hymenoptera","Diptera","Hemiptera","Coleoptera","Lepidoptera","Araneae", "Sarcoptiformes",   
                                                "Thysanoptera" ,"Trombidiformes","Dermaptera","Polydesmida" ,"Orthoptera", "Podocopida", "Decapoda","Pseudoscorpiones" ,
                                                "Poduromorpha" , "Entomobryomorpha","Mesostigmata" ,"Diplostraca" ,"Odonata" ,"Trichoptera", "Blattodea" ,"Amphipoda" ,       
                                                "Scolopendromorpha","Phthiraptera","Halocyprida","Geophilomorpha", "Symphypleona", "Calanoida" ,"Cyclopoida","Ephemeroptera",    
                                                "Psocoptera"))
plot_dat$species <- factor(plot_dat$species,levels =c( "Fejervarya multistriata","Microhyla fissipes",
                                                       "Pelophylax nigromaculatus","Bufo gargarizans",
                                                       "Pelophylax plancyi","Hyla chinensis", 
                                                       "Rana zhenhaiensis" ,"Polypedates megacephalus",
                                                       "Quasipaa spinosa","Hylarana latouchii" , #"Odorrana schmackeri",
                                                       "Lithobates catesbeianus" ))

p0 <- ggplot(plot_dat,aes(x=species,y=RRA))+
  geom_bar(aes(color=prey,fill=prey),stat="identity")+
  # geom_text(aes(y=lab_pos,label=counts,group=color),size=3)+
  theme_pubr(legend = "right",x.text.angle = 45)+
  scale_color_paletteer_d("ggsci::default_igv",direction=1)+
  scale_fill_paletteer_d("ggsci::default_igv",direction=1)#+ facet_wrap(~fly)
p0

ggsave(paste0("figure/fig_s1A.png"), p0, width = 7, height = 5)



nmds_result <- read_csv("figs1_nmds_result.csv")

nmds_result$species <- factor(nmds_result$species,levels = c( "Fejervarya_multistriata" , 
                                                              "Pelophylax_nigromaculatus" ,"Microhyla_fissipes", 
                                                              "Bufo_gargarizans" ,"Rana_zhenhaiensis" ,"Pelophylax_plancyi" ,
                                                              "Hylarana_latouchii", "Polypedates_megacephalus" , "Hyla_chinensis","Lithobates_catesbeianus"  ))
shape_list <- c(
  "Fejervarya_multistriata"=16,"Microhyla_fissipes"=0,
  "Pelophylax_nigromaculatus"=1,"Pelophylax_plancyi"=8,
  "Bufo_gargarizans"=3,"Rana_zhenhaiensis"=4 ,
  "Hylarana_latouchii"=5,"Hyla_chinensis"=6,         
  "Polypedates_megacephalus"=7,"Lithobates_catesbeianus"=17
)
color_list <- list(
  "Fejervarya_multistriata"=c("#41B7C4"),"Microhyla_fissipes"=c("#5CC0C0"),
  "Pelophylax_nigromaculatus"=c('#7ECDBB'),
  "Bufo_gargarizans"=c('#98D7B7'),"Rana_zhenhaiensis"=c('#B6E4B3') ,"Pelophylax_plancyi"=c('#C7EBB1'),
  "Hylarana_latouchii"=c('#EDF8BC'),"Hyla_chinensis"=c('#F1FABF'),
  "Polypedates_megacephalus"=c('#FCFED4'),"Lithobates_catesbeianus"=c('#DC0000')
)
stress <- "0.149"
p1 <- ggplot(data = nmds_result, aes(x=MDS1, y=MDS2,
                                     color=species,fill=species,
                                     shape=species)) +
  geom_point(size=4)+
  # stat_chull(alpha=0.1,geom="polygon")+
  labs(x=paste("NMDS 1"),
       y=paste("NMDS 2"),
       caption= paste('Stress =', stress))  +# 也可用title、caption
  scale_color_manual(values=color_list) +
  scale_fill_manual(values=color_list) +
  scale_shape_manual(values = shape_list)+
  theme(#legend.position = c(0.9,0.8),
    legend.title = element_blank(),
    panel.grid = element_blank(),plot.title=element_text(hjust=0),panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(color = 'black', fill = 'transparent'),
    axis.text = element_text(color = "black",size=10))+
  geom_hline(aes(yintercept=0), colour="#BEBEBE", linetype="dashed")+
  geom_vline(aes(xintercept=0), colour="#BEBEBE", linetype="dashed")

p1
# # 添加置信椭圆
p2 = p1 + stat_ellipse(data=nmds_result,
                       geom = "polygon",
                       level=0.9,
                       linetype = 2,
                       linewidth=0.5,
                       aes(fill=species),
                       alpha=0.3,
                       show.legend = T) +
  scale_fill_manual(values=color_list)
p2
ggsave(paste0("figure/fig_s1B.png"), p2, width = 7, height = 5)
