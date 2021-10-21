## LSTD Connectivity paper 
## Helper functions

# HELPER FUNCTIONS --------------------------------------------------------
# Prepare inputs ----------------------------------------------------------

scale_raster <- function(to_be_scaled, 
                         the_min, the_max){
  to_be_scaled_min <- cellStats(to_be_scaled, min, na.rm = TRUE)
  to_be_scaled_max <- cellStats(to_be_scaled, max, na.rm = TRUE)
  scaled <- 
    ((to_be_scaled - to_be_scaled_min) * (the_max - the_min) / 
       ((cellStats(to_be_scaled, max, na.rm = TRUE) - to_be_scaled_min)) + the_min)
  return(scaled)
}

make_background_raster <- function(landscape, 
                                   background_value, 
                                   base_value = 0){
  raster_of_ones <- landscape
  values(raster_of_ones) <- background_value
  return(raster_of_ones)
}

produce_mortality <- function(landscape, 
                              ceiling_value,
                              background_value,
                              uniform = FALSE) {
  if(uniform){
    mortality_raster <- make_background_raster(landscape = landscape, 
                                               background_value = background_value)
  } else {
    mortality_raster <- scale_raster((1-landscape), background_value, ceiling_value)
    mortality_raster[is.na(landscape)] <- ceiling_value
  }
  return(mortality_raster)
}

produce_resistance <- function(landscape,
                               ceiling_value,
                               background_value = 1,
                               uniform = FALSE){
  if(uniform){
    resistance_raster <- make_background_raster(landscape = landscape, 
                                                background_value = background_value)
  } else {
    resistance_raster <- scale_raster((1 - landscape), 1, ceiling_value)
    resistance_raster[is.na(landscape)] <- ceiling_value
  }
  return(resistance_raster)
}


# Dispatch method ---------------------------------------------------------

run_connectivity <- function(landscape, parameters, t_df, 
                             out_dir = "outputs/rasters"){
  
  if (parameters$type == 'SAMC') {
    out <- run_samc(landscape, parameters, t_df)
  } else if (parameters$type %in% 
             c('EXPONENTIAL', 'UNIFORM', 'INTACTNESS')) {
    out <- run_kernel(landscape, parameters, t_df, parameters$type)
  } else {
    stop("Analysis type not recognized.")
  }
  
  out <- out %>%
    mutate(sce = paste0(parameters$sce, 
                        t_df$mean_displacement))
  
  paths <- file.path(out_dir, paste0(out$sce, ".tif") )
  writeRaster(stack(out$output_map), filename = paths, bylayer = TRUE, 
              overwrite = TRUE)
  
  out$output_map <- paths
  
  return(out)
  
}

# Analysis functions ------------------------------------------------------

run_samc <- function(landscape, parameters, t_df){
  
  # browser()
  
  output_table <- expand_grid(t_df, 
                              cache_elapsed = NA,
                              step_elapsed = NA, 
                              full_elapsed = NA,
                              output_map = NA)
  
  # Prepare inputs
  res_raster <- 
    produce_resistance(landscape = landscape, 
                       ceiling_value = parameters$ceiling_value_res,
                       background_value = parameters$background_value_res,
                       uniform = parameters$uniform_res)
  mort_raster <- 
    produce_mortality(landscape = landscape, 
                      ceiling_value = parameters$ceiling_value_mort,
                      background_value = parameters$background_value_mort,
                      uniform = parameters$uniform_mort)
  
  # Prepare landscape
  landscape[is.na(landscape)] <- 0
  occ_raster <- landscape * parameters$occ_multiplier
  
  # Calculate steps
  
  tictoc::tic()
  samc_obj_custom <- LSTDConnect::samc_cache(resistance = as.matrix(res_raster),
                                             absorbtion = as.matrix(mort_raster),
                                             kernel = parameters$dirs)
  tictoc::toc() -> clock 
  (clock$toc - clock$tic) -> output_table$cache_elapsed
  
  for(t in t_df$t){
    tictoc::tic()
    short_disp_custom <- LSTDConnect::samc_step(steps = t, 
                                                cache = samc_obj_custom, 
                                                population = as.matrix(occ_raster))
    tictoc::toc() -> clock 
    (clock$toc - clock$tic) -> output_table$step_elapsed[output_table$t == t]
    
    ( raster(as.matrix(short_disp_custom$population[[1]]), 
             template = landscape)) |> list() -> 
      output_table$output_map[output_table$t == t]
    gc()
  }
  
  output_table$full_elapsed <- 
    output_table$cache_elapsed + output_table$step_elapsed
  
  return(output_table)
}

run_kernel <- function(landscape, parameters, t_df, type, negligible = 10^-6){
  
  # browser()
  
  # Get dimension
  cellDim <- raster::res(landscape)[1]
  
  # Prepare landscape
  landscape[is.na(landscape)] <- 0
  occ_raster <- landscape * parameters$occ_multiplier
  
  output_table <- expand_grid(t_df, 
                              cache_elapsed = NA,
                              step_elapsed = NA, 
                              full_elapsed = NA,
                              output_map = NA)
  
  for(t in t_df$t){
    
    displacement <- output_table$mean_displacement[t == t_df$t]
    
    if (type == 'EXPONENTIAL') {
      
      # Exponential kernel with mean dispersal distance of 1. 
      # Note the tail of the kernel (density < 10^-6) is truncated.
      
      k <- exponentialKernel(displacement, 
                             negligible = negligible)
      
      tictoc::tic()
      trMap <- pfocal(as.matrix(occ_raster), 
                      kernel = k)
      tictoc::toc() -> clock
      (clock$toc - clock$tic) -> output_table$full_elapsed[output_table$t == t]
      
      gc()
      
      (raster(trMap, template = landscape)) |> list() -> 
        output_table$output_map[output_table$t == t]
      
    } else if (type == 'UNIFORM'){
      
      k <- uniformKernel(displacement, 
                         useAveDist = TRUE)
      
      tictoc::tic()
      trMap <- pfocal(as.matrix(occ_raster), 
                      kernel = k)
      tictoc::toc() -> clock
      (clock$toc - clock$tic) -> output_table$full_elapsed[output_table$t == t]
      
      gc()
      
      (raster(trMap, template = landscape)) |> list() -> 
        output_table$output_map[output_table$t == t]
      
    } else if (type == 'INTACTNESS'){
      
      k <- exponentialKernel(displacement,
                             negligible = negligible)
      k <- k/sum(k)
      
      occ_raster_z <- as.matrix(occ_raster) ^ 0.5
      
      tictoc::tic()
      trMap <- pfocal(occ_raster_z,
                      na_flag = 0,
                      kernel = k) * occ_raster_z
      tictoc::toc() -> clock
      (clock$toc - clock$tic) -> output_table$full_elapsed[output_table$t == t]
      
      gc()
      
      (raster(trMap, template = landscape)) |> list() -> 
        output_table$output_map[output_table$t == t]
      
    } else {
      stop("Type not recognized")
    }
  }
  
  return(output_table)
}

# Encode params -----------------------------------------------------------

encode_parameters <- function(params){
  
  label <- ""
  
  if (params$type == 'SAMC') {
    
    if(params$uniform_res) {
      
      if(params$uniform_mort) {
        
        label <- paste0(label, "N")
        
      } else {
        
        label <- paste0(label, "M")
        
      }
      
    } else if (params$uniform_mort) {
      
      label <- paste0(label, "C")
      
    } else {
      
      label <- paste0(label, "B")
      
    }
    
    if (params$ceiling_value_res == 2){
      
      label <- paste0(label, "L")
      
    } else if (params$ceiling_value_res == 20){
      
      label <- paste0(label, "H")
      
    }
    
  } else if (params$type == 'EXPONENTIAL') {
    
    label <- paste0(label, "E")
    
  } else if (params$type == 'UNIFORM') {
    
    label <- paste0(label, "U")
    
  } else if (params$type == 'INTACTNESS') {
    
    label <- paste0(label, "I")
    
  } else {
    stop("Analysis type not recognized.")
  }
  
  return(label)
  
}
