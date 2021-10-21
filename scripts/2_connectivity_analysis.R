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

t <- c(4, 23, 91, 361, 1455) # NOTE: t is calibrated for 1km raster
mean_displacement <- c(2, 5, 10, 20, 40)

t_df <- tibble(t = t, 
               mean_displacement = (mean_displacement * 1000)/res(landscape)) 

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

# Run on grid
out_list <- vector(mode = "list", length = nrow(parameters))

for (row in (1:nrow(parameters))){
  params <- parameters[row,]
  out_list[[row]] <- run_connectivity(landscape = 1-landscape,
                                      parameters = params, 
                                      t_df = t_df)
  print(out_list[[row]])
  gc()
}
gc()

out_df <- bind_rows(out_list)

saveRDS(out_df, "outputs/objects/out_df.rds")

# -------------------------------------------------------------------------

out_df <- readRDS("outputs/objects/out_df.rds")

out_df_split <- out_df %>% split(.$sce)

zonal_stats <- lapply(FUN = function(row){
  print(row)
  out_zonal <- zonal(x = raster(row$output_map) %>% 
                       crop(protected_area), # Improtant to crop
                     z = protected_area, 
                     fun = "mean", na.rm = TRUE) %>% 
    as_tibble() %>% 
    mutate(paID = zone) %>% 
    left_join(protected_area_df) %>% 
    mutate(sce = row$sce)
}, out_df_split)

all_stats <- bind_rows(zonal_stats)

base_zonal <- zonal(x = landscape*100, 
                    z = protected_area, 
                    fun = "mean", na.rm = TRUE) %>% 
  as_tibble() %>% 
  mutate(paID = zone) %>% 
  left_join(protected_area_df) %>% 
  rename(base_mean = mean)

all_stats <- all_stats %>% 
  left_join(base_zonal, 
            by = c("zone", "paID", "paName", "nameEco", "patchArea")) %>% 
  mutate(ratio = base_mean/mean)

saveRDS(all_stats, "outputs/objects/all_stats.rds")
