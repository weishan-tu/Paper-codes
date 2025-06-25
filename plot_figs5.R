library(tidyverse)
library(iNEXT)

##figs5 Rarefaction curves of the prey data. 
sample_read_seq_mat <- read_csv("sequening_reads_number.csv")%>%
  as.matrix()

rc_reads <- iNEXT(sample_read_seq_mat, q=0, datatype="abundance")

p1 <- ggiNEXT(rc_reads, type=1,color.var ="None")
p11 <- p1 +theme_test()+theme(legend.position = 'none')+ 
  labs(x="Number of sequencing reads",y = "Number of detected Arthropoda OTUs")

ggsave(paste0("figure/figs5_Rarefaction_curves_of_the_prey_data.png"),p11, width = 6, height = 5)
