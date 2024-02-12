## LSTD Connectivity paper 
## Step 1: Analyse connectivity

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

# Parameter grid ----------------------------------------------------------

# for 300 m raster, t <- c(40,250,1000,4001,16007)
# for 900 m raster, t <- c(4,28,111,444,1778)
# for 1000 m raster, t <- c(4,22,90,360,1440)

t <- c(40,250,1000,4001,16007) # NOTE: t is calibrated for 300m raster
mean_displacement <- c(2, 5, 10, 20, 40)

# For testing
# t <- t[1]
# mean_displacement <- mean_displacement[1]

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

# Remove unwanted combinations 
parameters <- parameters %>% 
  filter(!(sce %in% c("NL", "CH", "CL")))

# Run on grid
out_df <- analyse_connectivity(parameters, 1-landscape, t_df, 
                               ext = NULL)
out_df_no_HF <- analyse_connectivity(parameters, 1-landscape_no_HF, t_df, 
                                     ext = "no_HF")

saveRDS(out_df, "outputs/objects/out_df.rds")
saveRDS(out_df_no_HF, "outputs/objects/out_df_no_HF.rds")

# -------------------------------------------------------------------------

out_df <- readRDS("outputs/objects/out_df.rds")
out_df_no_HF <- readRDS("outputs/objects/out_df_no_HF.rds")

# out_df <- data.frame(output_map = 
#                        list.files(full.names = TRUE, 
#                                   "D:/CAN_COST_LSTD_Connectivity_output_rasters/Can_Cost/")) %>% 
#   mutate(sce = tools::file_path_sans_ext(basename(output_map)))
# 
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

all_stats_final_BC <- all_stats_final %>% 
  filter(paID %in% unique(freq(raster("outputs/tmp/BC_protected_areas.tif"))[,1]))
saveRDS(all_stats_final_BC, "outputs/objects/all_stats_final_BC.rds")
