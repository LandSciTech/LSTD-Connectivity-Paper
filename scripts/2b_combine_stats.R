# combine stats tables 

# Load libraries
library(LSTDConnect) # For custom samc
library(pfocal)      # For focal test calculations
library(raster)      # For raster casting from matrix values
library(tictoc)      # For benchmarking
library(tidyr)       # For expand_grid
library(dplyr)       # For manipulation
library(tibble)      # For parameter grid making

# Source functions
source('scripts/0_helper_functions.R')

test_mode <- TRUE
# Parameter grid ----------------------------------------------------------

landscape <- raster("outputs/tmp/costmap_can_mask_aggregated.tif")
landscape_no_HF <- raster("outputs/tmp/costmap_can_mask_aggregated_no_HF.tif")
protected_area <- raster("outputs/tmp/paRaster_reproj_masked.tif")

if(test_mode){
  # For testing use small landscape
  landscape <- raster("outputs/tmp/BC_landscape.tif")
  landscape_no_HF <- raster("outputs/tmp/BC_landscape_no_HF.tif")
  protected_area <- raster("outputs/tmp/BC_protected_areas.tif")
  # need to set so behaves like large raster when testing
  raster::rasterOptions(todisk = TRUE) 
}

protected_area_df <- read.csv("data/CanadianPAsLookupSubset.csv")

# save 1-landscape to disk so doesn't get deleted with tmp rasts
landscape_1 <- calc(landscape, fun = function(x){1-x}, 
                    filename = "outputs/tmp/landscape_1.tif", overwrite = TRUE)
landscape_no_HF_1 <- calc(landscape_no_HF, fun = function(x){1-x}, 
                          filename = "outputs/tmp/landscape_no_HF_1.tif", overwrite = TRUE)

# for 300 m raster, t <- c(40,250,1000,4001,16007)
# for 900 m raster, t <- c(4,28,111,444,1778)
# for 1000 m raster, t <- c(4,22,90,360,1440)

t <- c(40,250,1000,4001,16007) # NOTE: t is calibrated for 300m raster
mean_displacement <- c(2, 5, 10, 20, 40)

# For testing
if(test_mode){
  t <- t[1:2]
  mean_displacement <- mean_displacement[1:2]
}

t_df <- tibble(t = t, 
               mean_displacement = (mean_displacement * 1000)/res(landscape)[1]) 
t_df$scale = mean_displacement #label results with whole numbers

scale_factor <- 300

uniform_res <- c(TRUE, FALSE)
uniform_mort <- c(TRUE, FALSE)

background_value_res <- 1
background_value_mort <- 0.00001

ceiling_value_res <- c(2, 20)
ceiling_value_mort <- ceiling_value_res/scale_factor

dirs <- 8
occ_multiplier <- 100

SAMC_params <- tibble(ceiling_value_res = ceiling_value_res, 
                      ceiling_value_mort = ceiling_value_mort) |>
  expand_grid(uniform_res = uniform_res,
              uniform_mort = uniform_mort,
              background_value_res = background_value_res, 
              background_value_mort = background_value_mort, 
              dirs = dirs, 
              occ_multiplier = occ_multiplier) |>
  slice(-which(.data$uniform_res & .data$uniform_mort)[-1]) |>
  mutate(type = "SAMC") %>% 
  relocate(type, 1) 

KERN_params <- expand_grid(type = c("EXPONENTIAL", 'UNIFORM', 'INTACTNESS'), 
                           occ_multiplier = occ_multiplier)

parameters <- bind_rows(SAMC_params, KERN_params)

# Label param combinations

the_labels <- c()
for (row in 1:nrow(parameters)){
  params <- parameters[row,]
  the_labels <- c(the_labels, encode_parameters(params))
}
parameters$sce <- the_labels

gc()

# -------------------------------------------------------------------------


out_df <- list.files("outputs/objects", pattern = "out_df_..?.rds", full.names = TRUE) %>% 
  lapply(readRDS) %>% 
  bind_rows()
out_df_no_HF <- list.files("outputs/objects", pattern = "out_df_no_HF.*.rds", full.names = TRUE) %>% 
  lapply(readRDS) %>% 
  bind_rows()

out_df2 <- data.frame(output_map = list.files("outputs/rasters",
                                             pattern = "..?\\d\\d?.tif",full.names = TRUE)) %>%
  mutate(sce = basename(output_map))

# out_df_no_HF <- data.frame(output_map = 
#                              list.files(full.names = TRUE, 
#                                         "D:/CAN_COST_LSTD_Connectivity_output_rasters/Can_Cost_noH//")) %>% 
#   mutate(sce = stringr::str_replace(tools::file_path_sans_ext(basename(output_map)), "no_HF", ""))

all_stats <- extract_stats(out_df, protected_area, 
                           protected_area_df)
all_stats_no_HF <- extract_stats(out_df_no_HF, protected_area, 
                                 protected_area_df)

saveRDS(all_stats, "outputs/objects/all_stats.rds")
saveRDS(all_stats_no_HF, "outputs/objects/all_stats_no_HF.rds")

# saveRDS(all_stats, "outputs/objects/all_stats_variant.rds")
# saveRDS(all_stats_no_HF, "outputs/objects/all_stats_no_HF_variant.rds")

# -------------------------------------------------------------------------

all_stats <- readRDS("outputs/objects/all_stats.rds")
all_stats_no_HF <- readRDS("outputs/objects/all_stats_no_HF.rds")

# all_stats <- readRDS("outputs/objects/all_stats_variant.rds")
# all_stats_no_HF <- readRDS("outputs/objects/all_stats_no_HF_variant.rds")

all_stats_final <- all_stats %>% 
  left_join(all_stats_no_HF,
            c("zone", "paID", "paName", "nameEco", "patchArea", "sce"),
            suffix = c("", "_no_HF")) %>% 
  mutate(ratio = mean/mean_no_HF)

too_small <- all_stats_final$paName[is.nan(all_stats_final$ratio)] %>% unique()
infinite <- all_stats_final$paName[is.infinite(all_stats_final$ratio)] %>% unique()

all_stats_final <- all_stats_final %>% 
  filter(!(paName %in% c(too_small, infinite)))

# all_stats_final$ratio

saveRDS(all_stats_final, "outputs/objects/all_stats_final.rds")
# saveRDS(all_stats_final, "outputs/objects/all_stats_final_variant.rds")

# -------------------------------------------------------------------------
#  not running on cloud
# all_stats_final_BC <- all_stats_final %>% 
#   filter(paID %in% unique(freq(raster("outputs/tmp/BC_protected_areas.tif"))[,1]))
# saveRDS(all_stats_final_BC, "outputs/objects/all_stats_final_BC.rds")
