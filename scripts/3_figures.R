## LSTD Connectivity paper 
## Step 3: Produce figures

# Load libraries
library(ggcorrplot)  # For visualization

# -------------------------------------------------------------------------

all_stats <- readRDS("outputs/objects/all_stats.rds")

# mapSubsetA <- read.csv("data/mapSetA_exptFullForV.csv")
# 
# all_stats <- all_stats %>%
#   filter(paID %in% mapSubsetA$paID)

all_stats_wide <- all_stats %>%
  select(-mean, -base_mean) %>% 
  pivot_wider(names_from = sce,values_from = ratio) %>% 
  select(-c(1:5)) %>% 
  drop_na()

all_stats_wide_corrs <- cor(all_stats_wide, 
                            method="spearman")
heatmap(all_stats_wide_corrs, symm=TRUE)
ggcorrplot(all_stats_wide_corrs, hc.order= TRUE)


# -------------------------------------------------------------------------

numClusters <-5
library(corrplot)

# -------------------------------------------------------------------------

all_stats_sub <- all_stats %>% 
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
