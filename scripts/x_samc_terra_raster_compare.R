#devtools::install_github("LandSciTech/LSTDConnect")
#devtools::install_github("LandSciTech/pfocal")

library(raster)
library(terra)
library(pfocal)
library(samc)
library(LSTDConnect)
library(microbenchmark)
library(dplyr)
library(ggplot2)

# ncores used in instance
ncores <- parallel::detectCores()

displacement <- 10#1#  #exponential kernel scale
t <- 1000#40# #SAMC time/scale

# change default theme
new_theme <- theme_classic()+
  theme(legend.text = element_text(size = rel(0.7)),
        legend.title = element_text(size = rel(0.8)),
        legend.key.size = unit(1.4, "lines"))
theme_set(new_theme)

landscape <- raster("data/KennedyGHM100.tif")
landscape[is.na(landscape)] <- 0
plot(landscape)

base_point <- c(-1963628, 915334)
the_res <- res(landscape)[1]
ext_100 <- extent(c(base_point, base_point+100*the_res)[c(1,3,2,4)])
landscape_100 <- crop(landscape, ext_100)
ext_500 <- extent(c(base_point, base_point+500*the_res)[c(1,3,2,4)])
landscape_500 <- crop(landscape, ext_500)
ext_1000 <- extent(c(base_point, base_point+1000*the_res)[c(1,3,2,4)])
landscape_1000 <- crop(landscape, ext_1000)

make_types <- function(land) {
  land_mat <- as.matrix(land)
  land_rast <- terra::rast(land)
  list(land, land_rast, land_mat)
}
lands_100 <- make_types(landscape_100)
lands_500 <- make_types(landscape_500)
lands_1000 <- make_types(landscape_1000)
lands_full <- make_types(landscape)
## Exponential kernel
dist_kernel <- LSTDConnect::exponentialKernel(displacement, negligible = 10^-6)
bm_exp <- bench::mark(min_iterations = 5, check = FALSE,
            focal_100 = raster::focal(lands_100[[1]], dist_kernel),
            terra_100 = terra::focal(lands_100[[2]], dist_kernel),
            pfocal_100 = pfocal::pfocal(lands_100[[1]], dist_kernel),
            focal_500 = raster::focal(lands_500[[1]], dist_kernel),
            terra_500 = terra::focal(lands_500[[2]], dist_kernel),
            pfocal_500 = pfocal::pfocal(lands_500[[1]], dist_kernel),
            focal_1000 = raster::focal(lands_1000[[1]], dist_kernel),
            terra_1000 = terra::focal(lands_1000[[2]], dist_kernel),
            pfocal_1000 = pfocal::pfocal(lands_1000[[1]], dist_kernel),
            focal_full = raster::focal(lands_full[[1]], dist_kernel),
            terra_full = terra::focal(lands_full[[2]], dist_kernel),
            pfocal_full = pfocal::pfocal(lands_full[[1]], dist_kernel))

## SAMC Setup
resistance <- (1-landscape/100)*(20-1)+1
mortality <- (1-landscape/100)*(2/30-0.00001)+0.00001

mort_mat <- as.matrix(mortality)
resist_mat <- as.matrix(resistance)

resist_100 <- crop(resistance, ext_100)
resist_500 <- crop(resistance, ext_500)
resist_1000 <- crop(resistance, ext_1000)

mort_100 <- crop(mortality, ext_100)
mort_500 <- crop(mortality, ext_500)
mort_1000 <- crop(mortality, ext_1000)

resist_100_mat <- as.matrix(resist_100)
resist_500_mat <- as.matrix(resist_500)
resist_1000_mat <- as.matrix(resist_1000)

mort_100_mat <- as.matrix(mort_100)
mort_500_mat <- as.matrix(mort_500)
mort_1000_mat <- as.matrix(mort_1000)

dirs <- 8
tr_list <- list(fun = function(x) 1 / mean(x),
                dir = dirs, 
                sym = TRUE)

samc_bm <- bench::mark(min_iterations = 5, check = FALSE,
  "samc_100" = {
    samc_obj <- samc::samc(data = resist_100_mat,
                           absorption = mort_100_mat,
                           tr_args = tr_list)   
    short_disp <- samc::distribution(samc = samc_obj,
                                     occ = lands_100[[3]],
                                     time = t)
    gc()
  },
  "samc_2_100" = {
    samc_obj_custom <- LSTDConnect::samc(resistance = resist_100_mat,
                                               absorption = mort_100_mat,
                                               directions = dirs)
    short_disp_custom <- LSTDConnect::distribution(time = t,
                                                samc = samc_obj_custom,
                                                occ = lands_100[[3]])
    gc()
  },
  "samc_500" = {
    samc_obj <- samc::samc(data = resist_500_mat,
                           absorption = mort_500_mat,
                           tr_args = tr_list) 
    short_disp <- samc::distribution(samc = samc_obj,
                                     occ = lands_500[[3]],
                                     time = t)
    gc()
  },
  "samc_2_500" = {
    samc_obj_custom <- LSTDConnect::samc(resistance = resist_500_mat,
                                               absorption = mort_500_mat,
                                               directions = dirs)
    short_disp_custom <- LSTDConnect::distribution(time = t,
                                                samc = samc_obj_custom,
                                                occ = lands_500[[3]])
    gc()
  },
  "samc_1000" = {
    samc_obj <- samc::samc(data = resist_1000_mat,
                           absorption = mort_1000_mat,
                           tr_args = tr_list)
    short_disp <- samc::distribution(samc = samc_obj,
                                     occ = lands_1000[[3]],
                                     time = t)
    gc()
  },
  "samc_2_1000" = {
    samc_obj_custom <- LSTDConnect::samc(resistance = resist_1000_mat,
                                               absorption = mort_1000_mat,
                                               directions = dirs)
    short_disp_custom <- LSTDConnect::distribution(time = t,
                                                samc = samc_obj_custom,
                                                occ = lands_1000[[3]])
    gc()
  },
  "samc_2_full" = {
    samc_obj_custom <- LSTDConnect::samc(resistance = resist_mat,
                                               absorption = mort_mat,
                                               directions = dirs)
    short_disp_custom <- LSTDConnect::distribution(time = t,
                                                samc = samc_obj_custom,
                                                occ = lands_full[[3]])
    gc()
  })


# plot memory allocations

# Note this caveat from bench::mark docs:
# Total amount of memory allocated by R while running the expression. Memory
# allocated outside the R heap, e.g. by malloc() or new directly is not tracked,
# take care to avoid misinterpreting the results if running code that may do
# this.
all_bm <- bind_rows(
          bm_exp %>% mutate(ncores = ncores, type = "exponential"),
          samc_bm %>% mutate(ncores = ncores, type = "samc")) 

saveRDS(all_bm, paste0("data/all_benchmarks_", ncores, ".rds"))

# load from saved file
# all_bm <- readRDS( paste0("data/all_benchmarks_", ncores, ".rds"))

cCoreSets<- c(8) # set up for varying number of cores
for(i in 1:length(cCoreSets)){
  ncores = cCoreSets[i]
  all_bm_bit <- readRDS(paste0("data/all_benchmarks_", ncores, ".rds"))
  
  if(i==1){
    all_bm = all_bm_bit
  }else{
    all_bm = bind_rows(all_bm_bit,all_bm)
  }
  
}

pdf(paste0("outputs/figures/focal_compareTime",ncores,".pdf"),width=8,height=4)
plot(all_bm %>% filter(!grepl("samc", expression)),type="boxplot")+
  scale_x_discrete(limits = grep("samc",
                                 unique(names(all_bm$expression)),
                                 invert = TRUE, value = TRUE))+
  facet_wrap(~ncores,labeller=label_both)+xlab("method & landscape width")+ylab("processing time")
dev.off()

pdf(paste0("outputs/figures/focal_compareMemory",ncores,".pdf"),width=8,height=4)
all_bm %>% filter(!grepl("samc", expression)) %>% 
  mutate(x = names(expression), mem = mem_alloc) %>% 
  ggplot(aes(x, mem_alloc))+
  geom_col()+
  #bench::scale_x_bench_expr()+
  bench::scale_y_bench_bytes()+
  coord_flip()+
  scale_x_discrete(limits =grep("samc",
                                unique(names(all_bm$expression)),
                                invert = TRUE, value = TRUE))+
  facet_wrap(~ncores,labeller=label_both)+labs(x = "method & landscape width",y="memory allocation")
dev.off()

pdf(paste0("outputs/figures/samc_compareTime",ncores,".pdf"),width=8,height=4)
plot(all_bm %>% filter(grepl("samc", expression)),type="boxplot")+
  bench::scale_y_bench_time(
    breaks = bench::as_bench_time(c("1s", "5s", "10s","50s", 
                                    "100s", "400s"))
  )+
  scale_x_discrete(limits = grep("samc",
                                 unique(names(all_bm$expression)),
                                 invert = FALSE, value = TRUE))+
  facet_wrap(~ncores,labeller=label_both)+xlab("method & landscape width")+ylab("processing time")
dev.off()

pdf(paste0("outputs/figures/samc_compareMemory",ncores,".pdf"),width=8,height=4)
all_bm %>% filter(grepl("samc", expression)) %>% 
  mutate(x = names(expression), mem = mem_alloc) %>% 
  ggplot(aes(x, mem_alloc))+
  geom_col()+
  #bench::scale_x_bench_expr()+
  bench::scale_y_bench_bytes()+
  coord_flip()+
  scale_x_discrete(limits =grep("samc",
                                unique(names(all_bm$expression)),
                                invert = FALSE, value = TRUE))+
  facet_wrap(~ncores,labeller=label_both)+xlab("method & landscape width")+ylab("processing time")
dev.off()
