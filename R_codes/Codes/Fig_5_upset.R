## Figure_5_upset_plot.R
## By Young C. Song
## This script generates the upset plots shown
## in Figure 5.


setwd("./Sample_Data/Fig_5")

library(ComplexUpset)
library(dbplyr)
library(tidyverse)
#source("./R/normalize_data.R")

# Read the data table, and convert to binary. Anything greater than or equal to 50 is converted to 1 (present)
# and anything less than 50 is converted to 0 (absent)
data_tbl <- read.csv("./Nitroso_50pct_CAZy.csv")
row_names <- data_tbl[, 1]
data <- data_tbl[, -1]
row.names(data) <- row_names

binary_data <- as.data.frame(data >= 50.0) * 1

# read metadata
metadata<- read.csv("./metadata.csv")
row.names(metadata)<-metadata$X
metadata<-metadata[,-1]


# generate data for upset plot
data<- as.data.frame(t(binary_data))
data_upset<- tibble::rownames_to_column(data)
colnames(data_upset)[colnames(data_upset) == "rowname"] <- "sample"
row.names(data_upset)<- data_upset$sample
data_upset<- inner_join(data_upset,metadata)


binary_upset<- function(data, filtervar, starts, location){
  
  binary_df<- filter(data, .data[[filtervar]] == location)
  binary_df<- binary_df[,startsWith(colnames(binary_df), starts)]
  binary_df <- as.numeric(colSums(binary_df) != 0)
  as.vector(binary_df)
}


### This is for combination of layer and resp. level.
create_upset_data_df<- function(data, starts){
  surf_high<- binary_upset(data, "Combine", starts, "Surf_High")
  surf_low<- binary_upset(data, "Combine", starts, "Surf_Low")
  sub_high<- binary_upset(data, "Combine", starts, "Sub_High")
  sub_low<- binary_upset(data, "Combine", starts, "Sub_Low")
  
  
  rbind(surf_high, surf_low, sub_high, sub_low)
}


upset_plot_data<- create_upset_data_df(data_upset, "CAZy")

run_upset<- function(x,y){
  colnames(x)<- colnames(y)
  x<- as.data.frame(t(x))
  x<- x[rowSums(x)>0,]
  
  upset_location<- as.vector(colnames(x))
  upset(x, upset_location)
}

run_upset(upset_plot_data, data)

#ggsave("./Outputs/Plots/Fig.5_ko_upset_plot.pdf", run_upset(upset_plot_data, data),  
#       width=6, height= 4)
