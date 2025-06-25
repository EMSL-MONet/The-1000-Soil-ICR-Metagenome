## Suppl_Fig_2_box_jitter_plot.R
## By Young C. Song
## This script generates the outout of the flux-balance analysis (FBA) shown
## in Supplementary Figure 2.


library(ggplot2)
library(hrbrthemes)
library(viridis)

setwd("./Sample_Data/Suppl_Fig_2")

##### Rhizobiales
mags_scatterbox <- read.csv("./Rhizo_fba_log10_smpl_fact/Rhizo_top_high_smpl_fact.csv",header=TRUE,sep=",")
mags_scatterbox <- read.csv("./Rhizo_fba_log10_smpl_fact/Rhizo_top_low_smpl_fact.csv",header=TRUE,sep=",")

mags_scatterbox <- read.csv("./Rhizo_fba_log10_smpl_fact/Rhizo_btm_high_smpl_fact.csv",header=TRUE,sep=",")
mags_scatterbox <- read.csv("./Rhizo_fba_log10_smpl_fact/Rhizo_btm_low_smpl_fact.csv",header=TRUE,sep=",")

##### Chthoniobacterales
mags_scatterbox <- read.csv("./Chthonio_fba_log10_smpl_fact/Chthonio_top_high_smpl_fact.csv",header=TRUE,sep=",")
mags_scatterbox <- read.csv("./Chthonio_fba_log10_smpl_fact/Chthonio_top_low_smpl_fact.csv",header=TRUE,sep=",")

mags_scatterbox <- read.csv("./Chthonio_fba_log10_smpl_fact/Chthonio_btm_high_smpl_fact.csv",header=TRUE,sep=",")
mags_scatterbox <- read.csv("./Chthonio_fba_log10_smpl_fact/Chthonio_btm_low_smpl_fact.csv",header=TRUE,sep=",")

#### Nitrososphaerales
mags_scatterbox <- read.csv("./Nitroso_fba_log10_smpl_fact/Nitroso_top_high_smpl_fact.csv",header=TRUE,sep=",")
mags_scatterbox <- read.csv("./Nitroso_fba_log10_smpl_fact/Nitroso_top_low_smpl_fact.csv",header=TRUE,sep=",")

mags_scatterbox <- read.csv("./Nitroso_fba_log10_smpl_fact/Nitroso_btm_high_smpl_fact.csv",header=TRUE,sep=",")
mags_scatterbox <- read.csv("./Nitroso_fba_log10_smpl_fact/Nitroso_btm_low_smpl_fact.csv",header=TRUE,sep=",")
#bpst_scatterbox$Gene <- as.factor(bpst_scatterbox$Gene)

### basic dot plots
#p<-ggplot(bpst_scatterbox,aes(x=Gene, y=NormVal)) + geom_dotplot(binaxis='y', stackdir='center', binwidth=0.5, stackratio=1.5, dotsize=0.5)

# plot it with switched axis
#p + coord_flip()


# choose which modules to display
# p + scale_x_discrete(limits=c("M00009, "M00007))


# dot plot with mean points
#p + stat_summary(fun.y=mean, geom="point", shape=18, size=3, color="red")

# dot plot with median points
#p + stat_summary(fun.y=median, geom="point", shape=18, size=3, color="red")

### Add basic box plot
#b <- ggplot(bpst_scatterbox, aes(x=Gene, y=NormVal)) + geom_boxplot()
#b <- ggplot(bpst_central_carb, aes(x=Module, y=PerComp)) + geom_boxplot() + geom_dotplot(binaxis='y', stackdir='center', binwidth=0.2, stackratio=1.5, dotsize=1.5)


### Add mean and standard deviation
#b <- ggplot(bpst_scatterbox, aes(x=Gene, y=NormVal)) + geom_dotplot(binaxis='y', stackdir='center', binwidth=0.2, stackratio=1.5, dotsize=0.5)
#b + stat_summary(fun.data="mean_sdl",fun.args=list(mult=1),
#                 geom="crossbar",width=0.5)
#b + coord_flip()


#b + stat_summary(fun.data="mean_sdl",fun.args=list(mult=1),
#                 geom="pointrange",color="red")
#b + coord_flip()


### box with jitter plot

data_test <- data.frame(name=c(mags_scatterbox$Factor),value=c(mags_scatterbox$Value))

data_test %>% 
  ggplot( aes(y=name, x=value, fill=name)) + geom_boxplot() +
  scale_fill_viridis(discrete=TRUE, alpha=0.6) +
  geom_jitter(color="black", size=1.5, alpha=0.7, width=0.2, height=0.1) +
  theme_ipsum() +
  theme(
    legend.position="none",
    text=element_text(size=0.5),
    #axis.text.x=element_text(angle=90,hjust=1),
    axis.text.x=element_text(hjust=1),
    plot.title=element_text(size=11)
  ) +
  ggtitle("A boxplot w/ jitter") +
  xlab("")



#b + stat_summary(fun.data="mean_sdl",fun.args=list(mult=1),
#                 geom="crossbar",width=0.5)
#b + coord_flip()


#b + stat_summary(fun.data="mean_sdl",fun.args=list(mult=1),
#                 geom="pointrange",color="red")
#b + coord_flip()

# Change dot plot colours by groups

#c1 <- ggplot(bpst_central_carb, aes(x=Module, y=PerComp)) + geom_dotplot(binaxis='y', stackdir='center', binwidth=0.5, stackratio=1.5, fill="#55BD28")
#c1 + coord_flip()


#c2 <- ggplot(bpst_central_carb, aes(x=Module, y=PerComp, fill=Module)) + geom_dotplot(binaxis='y', stackdir='center', binwidth=0.5, stackratio=1.5, dotsize=1.5)
#c2 + coord_flip()