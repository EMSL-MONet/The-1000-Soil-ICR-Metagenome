## Figure_4_bubble_chart_wo_complete_tbl.R
## By Young C. Song
## This script generates heatmap and bar plots shown
## in Figure 4.

setwd("./Sample_Data/Fig_4")

library(RColorBrewer)
library(ggplot2)
library(reshape)
library(dplyr)

#### Bubble Chart ####

bubble_tbl <- read.table("FTICR_KO_50_pct_top_20.tsv",header=TRUE,sep="\t")

ggplot(bubble_tbl, aes(x=Coordinate, y=PathCode)) + 
  geom_point(aes(color=P_value_code,size=GeneRatio), alpha=0.5) +
  scale_color_manual(values=c("#240D43","#327ECA", "#63D0F0", "#F3EDBF", "#E7A374", "#D6681B")) + 
                     theme_set(theme_bw() + theme(legend.position="bottom"))


#### Bar Plot ####

bar_tbl <- read.csv("FTICR_KO_50_pct_num_KOs.csv")

data <- data.frame(
  name=bar_tbl$Variable,
  value=bar_tbl$Value
)

ggplot(data, aes(x=name, y=value)) + geom_bar(stat="identity", width=0.7) + 
  theme(text=element_text(size=5)) +
  coord_flip()



