## Figure_6_bar_whisker_plots.R
## By Young C. Song
## This script generates the bar & whisker plots shown
## in Figure 6.

library(ggplot2)

setwd("./Sample_Data/Fig_6")

# Read the aa_std_dev (assuming it's in a CSV file)
# If your aa_std_dev is in a different format, adjust this part accordingly
aa_std_dev <- read.csv("1000_Soil_branched_aa_bar_whisker.csv", header=TRUE)

# Rename columns to match the ones in your file
# (Adjust column names if they're different in your file)
names(aa_std_dev) <- c("Order", "KEGG", "Organism", "Sample", "Average", "Stdev")

# Create the plot
ggplot(aa_std_dev, aes(x = Organism, y = Average, fill = Sample)) +
  geom_bar(stat = "identity", 
           position = position_dodge(width=0.8, preserve = "single"),
           width = 0.7) +
  geom_errorbar(aes(ymin = Average - Stdev, ymax = Average + Stdev), 
                position = position_dodge(width=0.8, preserve="single"),
                width = 0.25) +
  facet_wrap(~ Order, scales = "free_x") +
  labs(y = "Average Abundance", 
       x = "Organism") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_blank(),
    strip.text = element_blank(),  # This completely removes the facet labels
    panel.grid.minor = element_blank()
  )

#ggplot(aa_std_dev, aes(x = Organism, y = Average, fill = Sample)) +
#  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
#  geom_errorbar(aes(ymin = Average - Stdev, ymax = Average + Stdev), 
#                position = position_dodge(width = 0.8), width = 0.25) +
#  facet_wrap(~ Order, scales = "free_x") +
#  labs(title = "Gene Abundance by Order, Organism, and Sample Type",
#       y = "Average Abundance", 
#       x = "Organism") +
#  theme_bw() +
#  theme(axis.text.x = element_text(angle = 45, hjust = 1),
#        strip.background = element_rect(fill = "lightgray"),
#        legend.title = element_text(face = "bold"),
#        panel.grid.minor = element_blank())

# If you want to limit to specific KO numbers for clearer visualization
# For example, showing only one KO gene at a time:
#ko_of_interest <- "K01965"  # Replace with your KO of interest
#filtered_aa_std_dev <- aa_std_dev %>% filter(KEGG == ko_of_interest)

#ggplot(filtered_aa_std_dev, aes(x = Organism, y = Average, fill = Sample)) +
#  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
#  geom_errorbar(aes(ymin = Average - Stdev, ymax = Average + Stdev), 
#                position = position_dodge(width = 0.8), width = 0.25) +
#  labs(title = paste("Gene Abundance for", ko_of_interest),
#       y = "Average Abundance", 
#       x = "Organism") +
#  theme_bw() +
#  theme(axis.text.x = element_text(angle = 45, hjust = 1))
