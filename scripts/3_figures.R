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
#all_stats_final <- readRDS("outputs/objects/all_stats_final_variant.rds")

# all_stats_final <- all_stats_final %>% filter(!(nameEco %in% c("HudsonPlains",
#                                                              "TaigaPlains",
#                                                              "TaigaCordillera",
#                                                              "TaigaShield", 
#                                                              "NorthernArctic",
#                                                              "SouthernArctic")))

mapSubsetA <- read.csv("data/mapSetA_exptFullForV.csv")

# all_stats_final <- all_stats_final %>%
#   filter(paID %in% mapSubsetA$paID)

all_stats_wide <- all_stats_final %>%
  dplyr::select(-c(mean, mean_no_HF)) %>%
  pivot_wider(names_from = sce,values_from = ratio)

# all_stats_wide_corrs <- cor(all_stats_wide, 
#                             method="spearman")
# heatmap(all_stats_wide_corrs, symm=TRUE)
# ggcorrplot(all_stats_wide_corrs, hc.order= TRUE)


all_stats_wide_copy <- all_stats_wide
all_stats_wide_copy[, 6:40] <- all_stats_wide[,6:40] > 0.99
all_stats_wide_copy$intact <- rowSums(all_stats_wide_copy[, 6:40]) == 35

to_remove <- all_stats_wide_copy[all_stats_wide_copy$intact ==1,] %>% pull(paID) %>% unique()

all_stats_wide_copy[all_stats_wide_copy$intact ==1,] %>% pull(paName) %>% unique()
all_stats_wide_copy[all_stats_wide_copy$intact ==1,] %>% pull(nameEco) %>% table()

# -------------------------------------------------------------------------

numClusters <-8
library(corrplot)
library(stringr)

# -------------------------------------------------------------------------

all_stats_sub <- all_stats_final %>% 
  #filter(paID %in% read.csv("input/corPlotInput_w0_sFALSE_np1_venterFALSE_samcBTRUE.csv")$paID) %>% 
  #filter(!stringr::str_detect(.data$sce, "I")) %>% 
  #filter(!stringr::str_detect(.data$sce, "MH")) %>% 
  filter(!stringr::str_detect(.data$sce, "NL")) %>% 
  filter(!stringr::str_detect(.data$sce, "C"))
outMatS = subset(all_stats_sub,select=c(paID,sce,mean))

str(outMatS)

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

pdf("corPlotNums.pdf",width=15,height=15)
corrplot(cm,tl.col=newcolours, order = "hclust", addrect = numClusters,method="number")
dev.off()

#subsets
dist=10
dist2 = 100
omit=20
omitBH ="BH2"
dws <- spread(subset(outMatS,!is.na(mean)&(grepl(dist,sce)|grepl(dist2,sce))&!grepl(omit,sce)&!grepl(omitBH,sce)), sce, mean)
str(dws)
cs =cor(subset(dws,select=-paID),method="spearman",use="pairwise.complete.obs")
min(cs)

# -------------------------------------------------------------------------

library(vegan)

#normalize data
# all_stats_wide <- all_stats_wide %>% 
#   filter(paID!=6379)
fau <- decostand(all_stats_wide[,6:40], method="normalize", 1)

#normality assumptions check
example_NMDS=metaMDS(fau,k=2,trymax=1000)

#plot
plot(example_NMDS)

#extract NMDS scores (x and y coordinates)
data_scores = as.data.frame(scores(example_NMDS))

#add Ecozone name
data_scores$nameEco = all_stats_wide$nameEco

# #add Ecozone acronym
# data_scores$ecoS = all_stats_wide$

#add paID
data_scores$paArea = all_stats_wide$patchArea

#add columns to data frame
data_scores = cbind(data_scores,all_stats_wide[,6:40])
head(data_scores)

#plot WITH NO grouping
ggplot(data_scores, aes(x = NMDS1, y = NMDS2, label=NA)) +
  geom_point(aes(color = factor(nameEco),bg="black",size = paArea))

