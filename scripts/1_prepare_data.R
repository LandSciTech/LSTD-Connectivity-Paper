## LSTD Connectivity paper 
## Step 1: Prepare raster data

# Load libraries
library(raster)      # For raster casting from matrix values

# -------------------------------------------------------------------------

# gdalwarp -t_srs '+proj=lcc +lat_0=63.390675 +lon_0=-91.8666666666667 +lat_1=49 
# +lat_2=77 +x_0=6200000 +y_0=3000000 +datum=NAD83 +units=m +no_defs' 
# ProtectedAreas/paRaster.tif ProtectedAreas/protectedareas_new_landscape.tif 
# -overwrite

# Load the cost map
landscape <- raster("data/CanCostMap/costmap_can_mask.tif")
landscape_no_HF <- raster("data/CanCostMap/costmap_can_noHF.tif")

# Load protected area raster
protected_area <- raster("data/ProtectedAreas/paRaster.tif")

# Reproject the protected area raster
protected_area <- projectRaster(protected_area, 
                                crs = raster::crs(landscape), 
                                res = 1000,
                                method = "ngb")

# Resample the landscape
landscape <- resample(landscape, protected_area, 
                      method = "ngb")
landscape_no_HF <- resample(landscape_no_HF, protected_area, 
                            method = "ngb")

# Mask the PA
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
protected_area_df <- read.csv("data/ProtectedAreas/CanadianPAsLookupSubset.csv")
protected_area[protected_area <= 10] <- NA
