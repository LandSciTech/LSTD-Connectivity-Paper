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

# Mask the PA
protected_area <- mask(protected_area, landscape)

# Write both files out
writeRaster(landscape, "outputs/tmp/costmap_can_mask_aggregated.tif",
            overwrite = TRUE)
writeRaster(protected_area, "outputs/tmp/paRaster_reproj_masked.tif",
            overwrite = TRUE)

# -------------------------------------------------------------------------

landscape <- raster("data/CanCostMap/costmap_can_mask_rescaled_aggregated.tif")
protected_area <- raster("data/ProtectedAreas/protectedareas_new_landscape.tif")

# Load PA data
protected_area_df <- read.csv("data/ProtectedAreas/CanadianPAsLookupSubset.csv")
protected_area[protected_area <= 10] <- NA
