## LSTD Connectivity paper 
## Step 3: Produce figures

# Load libraries
library(ggplot2)     # For visualization
library(ggcorrplot)  # For visualization
library(ade4)        # For NMDS
library(vegan)       # For NMDS
library(dplyr)
library(tidyr)

# -------------------------------------------------------------------------

all_stats_final <- readRDS("outputs/objects/all_stats_final.rds")

# mapSubsetA <- read.csv("data/mapSetA_exptFullForV.csv")
# 
# all_stats_final <- all_stats_final %>%
#   filter(paID %in% mapSubsetA$paID)

all_stats_wide <- all_stats_final %>%
  select(-c(mean, mean_no_HF)) %>%
  pivot_wider(names_from = sce,values_from = ratio) %>%
  select(-c(1:5))

# all_stats_wide_corrs <- cor(all_stats_wide, 
#                             method="spearman")
# heatmap(all_stats_wide_corrs, symm=TRUE)
# ggcorrplot(all_stats_wide_corrs, hc.order= TRUE)


# -------------------------------------------------------------------------

numClusters <-6
library(corrplot)

# -------------------------------------------------------------------------

all_stats_sub <- all_stats_final %>% 
  #filter(paID %in% read.csv("input/corPlotInput_w0_sFALSE_np1_venterFALSE_samcBTRUE.csv")$paID) %>% 
  #filter(!stringr::str_detect(.data$sce, "I")) %>% 
  #filter(!stringr::str_detect(.data$sce, "MH")) %>% 
  filter(!stringr::str_detect(.data$sce, "NL")) %>% 
  filter(!stringr::str_detect(.data$sce, "C"))
outMatS = subset(all_stats_sub,select=c(paID,sce,mean))

dw <- spread(subset(outMatS,!is.na(mean)), sce, mean)
cm =cor(subset(dw,select=-paID),method="spearman",use="pairwise.complete.obs")

mycolors <- rep(NA,length(rownames(cm)))
names(mycolors) <- rownames(cm)
mycolors[grepl(2,names(mycolors))]   <- grey(0)
mycolors[grepl(5,names(mycolors))]   <- grey(0.2)
mycolors[grepl(10,names(mycolors))]   <- grey(0.4)
mycolors[grepl(20,names(mycolors))]   <- grey(0.6)
mycolors[grepl(40,names(mycolors))]   <- grey(0.8)

ord <- corrMatOrder(cm, order="hclust")
newcolours <- mycolors[ord]

corrplot(cm,tl.col=newcolours, order = "hclust", addrect = numClusters,method="shade")


# -------------------------------------------------------------------------

#normalize data
fau <- decostand(all_stats_wide, method="normalize", 1)
fau <- all_stats_wide

#normality assumptions check
example_NMDS=metaMDS(fau,k=2,trymax=100)

#plot
plot(example_NMDS)

#extract NMDS scores (x and y coordinates)
data_scores = as.data.frame(scores(example_NMDS))

#add Ecozone name
data_scores$nameEco = all_stats_wide$nameEco

#add Ecozone acronym
data_scores$ecoS = all_stats_wide$ecoS

#add paID
data_scores$paArea = all_stats_wide$paArea

#add columns to data frame
data_scores = cbind(data_scores,all_stats_wide[,5:53])
head(data_scores)

#plot WITH NO grouping
ggplot(data_scores, aes(x = NMDS1, y = NMDS2, label=NA)) +
  geom_point(aes(color = factor(nameEco),bg="black",size = paArea))

