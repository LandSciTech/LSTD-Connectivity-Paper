## LSTD Connectivity paper 
## Step 1: Prepare raster data

# Load libraries
library(raster)      # For raster casting from matrix values
landscape <- raster("data/CombinedCosts/allCostsLayer.tif")

# -------------------------------------------------------------------------

# gdalwarp -overwrite -tr 300 300 -te 3489899.158 463834.515 9214799.158 5442034.515 
# -t_srs "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=63.390675 +lon_0=-91.8666666666667 
# +x_0=6200000 +y_0=3000000 +datum=NAD83 +units=m +no_defs" -r near -co 
# "COMPRESS=LZW" paRaster.tif paRaster_reproj_300.tif

source("scripts/0_helper_functions.R")

# Load the cost map
landscape <- raster("data/CombinedCosts/allCostsLayer.tif")
landscape_no_HF <- raster("data/CombinedCosts/naturalCostsLayer.tif")

# Load protected area raster
protected_area <- raster("data/paRaster_reproj_300.tif")

# Reproject the protected area raster
# protected_area <- projectRaster(protected_area, 
#                                 crs = raster::crs(landscape), 
#                                 res = 300,
#                                 method = "ngb")
# Resample the landscape
# protected_area <- resample(protected_area, landscape,
#                            method = "ngb")
# landscape <- resample(landscape, protected_area, 
#                       method = "ngb")
# landscape_no_HF <- resample(landscape_no_HF, protected_area, 
#                             method = "ngb")

# Mask the PA
# protected_area <- extend(protected_area, landscape)
# protected_area <- crop(protected_area, landscape)
protected_area <- mask(protected_area, landscape)
landscape_no_HF <- mask(landscape_no_HF, landscape)

# Scale between 0 and 1
landscape <- scale_raster(landscape, 0, 1)
landscape_no_HF <- scale_raster(landscape_no_HF, 0, 1)

# Write both files out
writeRaster(landscape, "outputs/tmp/costmap_can_mask_aggregated.tif",
            overwrite = TRUE)
writeRaster(landscape_no_HF, "outputs/tmp/costmap_can_mask_aggregated_no_HF.tif",
            overwrite = TRUE)
writeRaster(protected_area, "outputs/tmp/paRaster_reproj_masked.tif",
            overwrite = TRUE)

# -------------------------------------------------------------------------

landscape <- raster("outputs/tmp/costmap_can_mask_aggregated.tif")
landscape_no_HF <- raster("outputs/tmp/costmap_can_mask_aggregated_no_HF.tif")
protected_area <- raster("outputs/tmp/paRaster_reproj_masked.tif")

# Load PA data
protected_area_df <- read.csv("data/CanadianPAsLookupSubset.csv")
protected_area[protected_area <= 10] <- NA

# base_landscape <- raster("../data/CombinedCosts/naturalCostsLayer.tif")
protected_area[landscape_no_HF==1] <- NA
# plot(protected_area)

# -------------------------------------------------------------------------

# plot(landscape)
# BC_extent <- drawExtent()

BC_landscape <- crop(landscape, BC_extent)
BC_landscape_no_HF <- crop(landscape_no_HF, BC_extent)
BC_protected_areas <- crop(protected_area, BC_extent)

# BC_pa_IDs <- unique(freq(BC_protected_areas)[,1])
# 
# BC_protected_area_df <- protected_area_df %>% 
#   filter(paID %in% BC_pa_IDs)

writeRaster(BC_landscape, "outputs/tmp/BC_landscape.tif",
            overwrite = TRUE)
writeRaster(BC_landscape_no_HF, "outputs/tmp/BC_landscape_no_HF.tif",
            overwrite = TRUE)
writeRaster(BC_protected_areas, "outputs/tmp/BC_protected_areas.tif",
            overwrite = TRUE)
