library(tidyverse)
library(ggpubr)
library(ggpmisc)
library(cowplot)
library(lme4)
library(lmerTest)
library(performance)
library(rsq)
##load network data
dat <- read_csv("network_data.csv")
dat$residence_time <- bestNormalize::bestNormalize(dat$residence_time)$x.t #orderNorm Transformation
dat$isolation <- log10(dat$isolation)
dat$area <- log10(dat$area)
dat$bullfrog_abundance <- log(dat$bullfrog_abundance+1) 

theme_set(theme_test()+theme(panel.grid=element_blank(),legend.position ="none",
                             legend.title =element_blank(),
                             legend.text = element_text(color="black",size = 12),
                             axis.text.x = element_text(size = 12,  color = "black" ), 
                             axis.text.y = element_text(size = 12, color = "black"),
                             axis.title.y = element_text(size = 12, color = "black",angle = 90),
                             axis.title.x =  element_text(size = 12, color = "black",angle = 0),
                             strip.text.x = element_text(size = 12, color = "black"),
                             strip.text.y = element_text(size = 12, color = "black") , 
                             plot.margin=unit(rep(0.1,4),'cm')))
###Residence time
##LMMs
lme_con <- lmer(Connectance ~ residence_time+ (1|island) + (1|times), data = dat)
R2 <- round(rsq(lme_con)$fixed,3) ##calculate R2
p.value.label <- round(summary(lme_con)$coefficients["residence_time", "Pr(>|t|)"],3)
fig4a <- ggplot(data=dat,aes(residence_time,Connectance ))+
  geom_point(aes(color=invasion,fill=invasion),size=3,shape=21,alpha = 5/10)+
  labs(y="Connectance (z-scores)",x="")+
  scale_fill_manual(values = c("#1D91C0","#FF8000"))+  #,"#810F7C"
  scale_color_manual(values = c("#1D91C0","#FF8000"))+  #,"#810F7C"
  stat_smooth(method="lm",size=1.5,linetype=1,colour="black")+
  annotate(geom="text", x=0.5, y=-25,size=4, label=paste0("R^2==",R2,"~~ italic(p)==",p.value.label ), parse=TRUE)
fig4a
lme_mod <- lmer(Modularity ~ residence_time+ (1|island) + (1|times), data = dat)
R2 <- round(rsq(lme_mod)$fixed,3) ##calculate R2
p.value.label <- round(summary(lme_mod)$coefficients["residence_time", "Pr(>|t|)"],3)
fig4b <- ggplot(data=dat,aes(residence_time,Modularity))+
  geom_point(aes(color=invasion,fill=invasion),size=3,shape=21,alpha = 5/10)+
  labs(y="Modularity (z-scores)",x="Residence time since introduction (orderNorm transformation)")+
  scale_fill_manual(values = c("#1D91C0","#FF8000"))+  #,"#810F7C"
  scale_color_manual(values = c("#1D91C0","#FF8000"))+  #,"#810F7C"
  stat_smooth(method="lm",size=1.5,linetype=5,colour="black")+
  annotate(geom="text", x=0.5, y=40,size=4, label=paste0("R^2==",R2,"~~ italic(p)==",p.value.label ), parse=TRUE)

lme_nest <- lmer(Nestedness ~ residence_time+ (1|island) + (1|times), data = dat)
R2 <- round(rsq(lme_nest)$fixed,3) ##calculate R2
p.value.label <- round(summary(lme_nest)$coefficients["residence_time", "Pr(>|t|)"],3)
fig4c <- ggplot(data=dat,aes(residence_time,Nestedness))+
  geom_point(aes(color=invasion,fill=invasion),size=3,shape=21,alpha = 5/10)+
  labs(y="Nestedness (z-scores)",x="")+
  scale_fill_manual(values = c("#1D91C0","#FF8000"))+  #,"#810F7C"
  scale_color_manual(values = c("#1D91C0","#FF8000"))+  #,"#810F7C"
  stat_smooth(method="lm",size=1.5,linetype=5,colour="black")+
  annotate(geom="text", x=0.5, y=-22,size=4, label=paste0("R^2==",R2,"~~ italic(p)==",p.value.label ), parse=TRUE)

###Bullfrog abundance
lme_con <- lmer(Connectance ~ bullfrog_abundance+ (1|island) + (1|times), data = dat)
R2 <- round(rsq(lme_con)$fixed,3) ##calculate R2
p.value.label <- round(summary(lme_con)$coefficients["bullfrog_abundance", "Pr(>|t|)"],3)
fig4d <- ggplot(data=dat,aes(bullfrog_abundance,Connectance))+
  geom_point(aes(color=invasion,fill=invasion),size=3,shape=21,alpha = 5/10)+
  labs(y="Connectance (z-scores)",x="")+
  scale_fill_manual(values = c("#1D91C0","#FF8000"))+  #,"#810F7C"
  scale_color_manual(values = c("#1D91C0","#FF8000"))+  #,"#810F7C"
  stat_smooth(method="lm",size=1.5,linetype=1,colour="black")+
  annotate(geom="text", x=1.25, y=-22,size=4, label=paste0("R^2==",R2,"~~ italic(p)==",p.value.label ), parse=TRUE)

lme_mod <- lmer(Modularity ~ bullfrog_abundance+ (1|island) + (1|times), data = dat)
R2 <- round(rsq(lme_mod)$fixed,3) ##calculate R2
p.value.label <- round(summary(lme_mod)$coefficients["bullfrog_abundance", "Pr(>|t|)"],3)
fig4e <- ggplot(data=dat,aes(bullfrog_abundance,Modularity))+
  geom_point(aes(color=invasion,fill=invasion),size=3,shape=21,alpha = 5/10)+
  labs(y="Connectance (z-scores)",x="Bullfrog abundance (log[x+1])")+
  scale_fill_manual(values = c("#1D91C0","#FF8000"))+  #,"#810F7C"
  scale_color_manual(values = c("#1D91C0","#FF8000"))+  #,"#810F7C"
  stat_smooth(method="lm",size=1.5,linetype=1,colour="black")+
  annotate(geom="text", x=1.25, y=40,size=4, label=paste0("R^2==",R2,"~~ italic(p)==",p.value.label ), parse=TRUE)

lme_nest <- lmer(Nestedness ~ bullfrog_abundance+ (1|island) + (1|times), data = dat)
R2 <- round(rsq(lme_nest)$fixed,3) ##calculate R2
p.value.label <- round(summary(lme_nest)$coefficients["bullfrog_abundance", "Pr(>|t|)"],3)
fig4f <- ggplot(data=dat,aes(bullfrog_abundance,Nestedness))+
  geom_point(aes(color=invasion,fill=invasion),size=3,shape=21,alpha = 5/10)+
  labs(y="Connectance (z-scores)",x="")+
  scale_fill_manual(values = c("#1D91C0","#FF8000"))+  #,"#810F7C"
  scale_color_manual(values = c("#1D91C0","#FF8000"))+  #,"#810F7C"
  stat_smooth(method="lm",size=1.5,linetype=5,colour="black")+
  annotate(geom="text", x=1.25, y=-22,size=4, label=paste0("R^2==",R2,"~~ italic(p)==",p.value.label ), parse=TRUE)


##Isolation
network_control <- dat%>%filter(invasion=="N")%>%mutate(type="Control")
network_addinv <- dat%>%mutate(type="Invaded")
network_all <- rbind(network_control,network_addinv)
network_all$type <- factor(network_all$type,levels = c("Control","Invaded"))
fig4g <- ggplot()+
  geom_point(data=network_addinv,aes(isolation,Connectance,color=invasion,fill=invasion),size=3,shape=21,alpha = 5/10)+
  labs(y="Connectance (z-scores)",x="")+
  scale_fill_manual(values = c("#1D91C0","#FF8000","#1D91C0","#FF8000"))+  #,"#810F7C"
  scale_color_manual(values = c("#1D91C0","#FF8000","#1D91C0","#FF8000"))+  #,"#810F7C"
  stat_smooth(data=network_all,aes(isolation,Connectance,color=type,fill=type),method="lm",size=1.5,linetype=1)+
  stat_poly_eq(data=network_all,aes(isolation,Connectance,color=type,fill=type,label=paste(..rr.label..,..p.value.label..,sep = "~~~~")),
               size = 4) 

fig4h <- ggplot()+
  geom_point(data=network_addinv,aes(isolation,Modularity ,color=invasion,fill=invasion),size=3,shape=21,alpha = 5/10)+
  labs(y="Modularity (z-scores)",x="Distance to closest mainland [log10(km)]")+
  scale_fill_manual(values = c("#1D91C0","#FF8000","#1D91C0","#FF8000"))+  #,"#810F7C"
  scale_color_manual(values = c("#1D91C0","#FF8000","#1D91C0","#FF8000"))+  #,"#810F7C"
  stat_smooth(data=network_all,aes(isolation,Modularity ,color=type,fill=type),method="lm",size=1.5,linetype=1)+
  stat_poly_eq(data=network_all,aes(isolation,Modularity,color=type,fill=type,label=paste(..rr.label..,..p.value.label..,sep = "~~~~")),
               size = 4) 

fig4i <- ggplot()+
  geom_point(data=network_addinv,aes(isolation,Nestedness  ,color=invasion,fill=invasion),size=3,shape=21,alpha = 5/10)+
  labs(y="Nestedness (z-scores)",x="")+
  scale_fill_manual(values = c("#1D91C0","#FF8000","#1D91C0","#FF8000"))+  #,"#810F7C"
  scale_color_manual(values = c("#1D91C0","#FF8000","#1D91C0","#FF8000"))+  #,"#810F7C"
  stat_smooth(data=network_all,aes(isolation,Nestedness  ,color=type,fill=type),method="lm",size=1.5,linetype=1)+
  stat_poly_eq(data=network_all,aes(isolation,Nestedness,color=type,fill=type,label=paste(..rr.label..,..p.value.label..,sep = "~~~~")),
               size = 4) 
library(cowplot)
fig4 <- plot_grid(
  fig4a,fig4b,fig4c, fig4d,fig4e,fig4f,fig4g,fig4h,fig4i,
  
  ncol = 3, labels = c('A', 'B', 'C','D', 'E', 'F',"G","H","I"))  ##,'D', 'E', 'F'
 
ggsave(filename=paste0('figure/fig4.pdf'),plot=fig4,width=9 ,height=9)


