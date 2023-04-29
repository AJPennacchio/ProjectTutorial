library(DAAG)
library(nlme)
library(ggplot2)
library(ggpubr)
library(viridisLite)
library(ggbeeswarm)

scidat<-na.omit(science)
scidat$like<-scidat$like+rnorm(n = length(scidat$like),mean = mean(scidat$like), sd = sd(scidat$like))

ggplot(data = scidat,aes(x = PrivPub, y = like, fill = sex))+
  geom_boxplot()

ggplot(data = scidat,aes(x = PrivPub, y = like, fill = sex))+
  geom_boxplot()+scale_fill_viridis_d( option = "D")+
  theme_pubr()

ggplot(data = scidat,aes(x = PrivPub, y = like, fill = sex))+
  scale_fill_viridis_d( option = "D")+
  geom_violin(alpha=0.25, position = position_dodge(width = .75),linewidth=1,color="black") +
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=1.2, alpha = 0.7)+
  theme_pubr()

ggplot(data = scidat,aes(x = PrivPub, y = like, fill = sex))+
  scale_fill_viridis_d( option = "D")+
  geom_violin(alpha=0.4, position = position_dodge(width = .75),linewidth=1,color="black") +
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=1.2, alpha = 0.7)+
  geom_point( shape = 21,size=2, position = position_jitterdodge(), color="black",alpha=1)+
  theme_pubr()

ggplot(data = scidat,aes(x = PrivPub, y = like, fill = sex))+
  scale_fill_viridis_d( option = "D")+
  geom_violin(alpha=0.4, position = position_dodge(width = .75),linewidth=1,color="black") +
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=1.2, alpha = 0.7)+
  geom_point( shape = 21,size=2, position = position_jitterdodge(), color="black",alpha=1)+
  theme_pubr()+
  ylab(  c("How much do they like science?")  )  +
  xlab(  c("Type of school")  )  +
  rremove("legend.title")+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2),
        axis.ticks = element_line(size=2,color="black"),
        axis.ticks.length=unit(0.2,"cm"),
        legend.position = c(0.92, 0.85))+
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15) +  
  font("legend.text",size = 15)

ggplot(data = scidat,aes(x = PrivPub, y = like, fill = sex))+
  scale_fill_viridis_d( option = "D")+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),linewidth=1,color=NA) +
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=1, alpha = 0.7,show.legend = F)+
  # geom_point( shape = 21,size=2, position = position_jitterdodge(), color="black",alpha=1)+
  ggbeeswarm::geom_quasirandom(shape = 21,size=2, dodge.width = .75, color="black",alpha=.5,show.legend = F)+
  theme_minimal()+
  ylab(  c("How much do they like science?")  )  +
  xlab(  c("Type of school")  )  +
  rremove("legend.title")+
  theme(#panel.border = element_rect(colour = "black", fill=NA, size=2),
    axis.line = element_line(colour = "black",size=1),
    axis.ticks = element_line(size=1,color="black"),
    axis.text = element_text(color="black"),
    axis.ticks.length=unit(0.2,"cm"),
    legend.position = c(0.95, 0.85))+
  font("xylab",size=15)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15) +  
  font("legend.text",size = 15)+
  guides(fill = guide_legend(override.aes = list(alpha = 1,color="black")))

