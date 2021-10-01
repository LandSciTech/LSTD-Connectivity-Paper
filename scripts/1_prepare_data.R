## LSTD Connectivity paper 
## Step 1: Prepare raster data

# Load libraries
library(raster)      # For raster casting from matrix values

# Source functions
source('scripts/0_helper_functions.R')

# -------------------------------------------------------------------------

# Using GHM
landscape <- scale_raster(raster("data/Kennedy/KennedyGHM100.tif"), 0, 1)

# Using can cost map
landscape <- raster("data/CanCostMap/costmap_can_mask_rescaled_aggregated.tif")
landscape <- raster::aggregate(landscape, 3)
writeRaster(landscape, "outputs/tmp/costmap_can_mask_rescaled_aggregated.tif",
            overwrite = TRUE)

# Load protected area raster
protected_area <- raster("data/ProtectedAreas/paRaster.tif")

# gdalwarp -t_srs '+proj=lcc +lat_0=63.390675 +lon_0=-91.8666666666667 +lat_1=49 
# +lat_2=77 +x_0=6200000 +y_0=3000000 +datum=NAD83 +units=m +no_defs' 
# ProtectedAreas/paRaster.tif ProtectedAreas/protectedareas_new_landscape.tif -overwrite

protected_area <- raster("data/ProtectedAreas/protectedareas_new_landscape.tif")
origin(protected_area) <- origin(landscape)

# Recrop all
protected_area <- crop(protected_area, landscape)
landscape <- crop(landscape, protected_area)
protected_area <- mask(protected_area, landscape)

# Load PA data
protected_area_df <- read.csv("data/ProtectedAreas/CanadianPAsLookupSubset.csv")
protected_area[protected_area <= 10] <- NA

# Load ready-data
writeRaster(protected_area, "outputs/tmp/pa_new_landscape_final.tif")
writeRaster(landscape, "outputs/tmp/new_landscape_final.tif")

protected_area <- raster("outputs/tmp/pa_new_landscape_final.tif")
landscape <- 1-raster("outputs/tmp/new_landscape_final.tif")
