## LSTD Connectivity paper 
## Helper functions

# HELPER FUNCTIONS --------------------------------------------------------
# Prepare inputs ----------------------------------------------------------

scale_raster_bis <- function(to_be_scaled, 
                             the_min, the_max, old_min=0, old_max=1){
  scaled <- 
    (to_be_scaled - old_min) * (the_max - the_min) / (old_max - old_min) + the_min
  return(scaled)
}

scale_raster <- function(to_be_scaled, 
                         the_min, the_max){
  to_be_scaled_min <- cellStats(to_be_scaled, min, na.rm = TRUE)
  to_be_scaled_max <- cellStats(to_be_scaled, max, na.rm = TRUE)
  
  # doing it this way caused 4 copies to be written to disk
  # scaled <- 
  #   ((to_be_scaled - to_be_scaled_min) * (the_max - the_min) / 
  #      ((to_be_scaled_max - to_be_scaled_min)) + the_min)
  
  # using calc is faster and takes up less space on disk
  scale_fun <- function(x){
      ((x - to_be_scaled_min) * (the_max - the_min) /
         ((to_be_scaled_max - to_be_scaled_min)) + the_min)
  }
  
  scaled <- calc(to_be_scaled, scale_fun)
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

extract_stats <- function(rasters_df, protected_area_raster, 
                          protected_area_data){
  
  out_df_split <- rasters_df %>% split(.$sce)
  
  zonal_stats <- lapply(FUN = function(row){
    print(row) 
    cropped <- crop(raster(row$output_map), protected_area_raster)
    # writeRaster(cropped, "outputs/tmp/tmp.tif", overwrite = TRUE)
    # cropped <- raster("outputs/tmp/tmp.tif")
    # browser()
    out_zonal <- zonal(x = cropped, # Important to crop
                       z = protected_area_raster, 
                       fun = "mean", na.rm = TRUE) %>% 
      as_tibble() %>% 
      mutate(paID = zone) %>% 
      left_join(protected_area_data) %>% 
      mutate(sce = row$sce)
    # file.remove("outputs/tmp/tmp.tif")
  }, out_df_split)
  
  all_stats_ret <- bind_rows(zonal_stats)
  
  return(all_stats_ret)
  
}

analyse_connectivity <- function(parameters, landscape, t_df, ext){
  
  out_list <- vector(mode = "list", length = nrow(parameters))
  
  for (row in (1:nrow(parameters))){
    params <- parameters[row,]
    out_list[[row]] <- run_connectivity(landscape = landscape,
                                        parameters = params, 
                                        t_df = t_df, 
                                        ext = ext)
    print(out_list[[row]])
    gc()
  }
  gc()
  
  out_df <- bind_rows(out_list)
  
  return(out_df)
}

run_connectivity <- function(landscape, parameters, t_df, ext = NULL,
                             out_dir = "outputs/rasters", sourceMap=NULL){
  
  if (parameters$type == 'SAMC') {
    out <- run_samc(landscape, parameters, t_df,sourceMap=sourceMap)
  } else if (parameters$type %in% 
             c('EXPONENTIAL', 'UNIFORM', 'INTACTNESS')) {
    out <- run_kernel(landscape, parameters, t_df, parameters$type,sourceMap=sourceMap)
  } else {
    stop("Analysis type not recognized.")
  }
  
  out <- out %>%
    mutate(sce = paste0(parameters$sce, 
                        t_df$scale))
  message(out$sce, " complete at ", Sys.time())
  paths <- file.path(out_dir, paste0(out$sce, ext, ".tif") )

  writeRaster(stack(out$output_map), filename = paths, bylayer = TRUE, 
              overwrite = TRUE)
  out$output_map <- paths
  
  return(out)
  
}

# Analysis functions ------------------------------------------------------

run_samc <- function(landscape, parameters, t_df,sourceMap=NULL){
  
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
  
  if(is.null(sourceMap)){
    occ_raster <- landscape * parameters$occ_multiplier
  }else{
    occ_raster <- sourceMap
  }
  
  # Calculate steps
  
  tictoc::tic()
  samc_obj_custom <- LSTDConnect::samc(resistance = as.matrix(res_raster),
                                       absorption = as.matrix(mort_raster),
                                       directions = parameters$dirs)
  tictoc::toc() -> clock 
  (clock$toc - clock$tic) -> output_table$cache_elapsed
  
  for(t in t_df$t){
    tictoc::tic()
    short_disp_custom <- LSTDConnect::distribution(time = t, 
                                                   samc = samc_obj_custom, 
                                                   occ = as.matrix(occ_raster))
    tictoc::toc() -> clock 
    (clock$toc - clock$tic) -> output_table$step_elapsed[output_table$t == t]
    
    ( raster(as.matrix(short_disp_custom$occ[[1]]), 
             template = landscape)) |> list() -> 
      output_table$output_map[output_table$t == t]
    gc()
  }
  
  output_table$full_elapsed <- 
    output_table$cache_elapsed + output_table$step_elapsed
  
  return(output_table)
}

run_kernel <- function(landscape, parameters, t_df, type, negligible = 10^-6,sourceMap=NULL){
  
  # browser()
  
  # Get dimension
  cellDim <- raster::res(landscape)[1]
  
  # Prepare landscape
  landscape[is.na(landscape)] <- 0
  if(is.null(sourceMap)){
    occ_raster <- landscape * parameters$occ_multiplier
  }else{
    occ_raster= sourceMap
  }
  
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
      
      k <- uniformKernel(displacement)
      
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


# Figures -----------------------------------------------------------------

plot_spatial_agreement <- function(scale_param, standardize = TRUE, countryR = "outputs/rasters/countryR.tif",
                                   output_dir = "outputs/figures/",
                                   scenarios = NULL,
                                   resultDir = "D:/CAN_COST_LSTD_Connectivity_output_rasters",
                                   can_cost_dir = file.path(resultDir, "Can_Cost"), 
                                   nat_cost_dir = file.path(resultDir, "Can_Cost_noH")){

  # browser()
  
  # Adapted from Josie's code, not fully cleaned up
  stopifnot(scale_param %in% c(2, 5, 10, 20, 40))
  
  all_files <- list.files(can_cost_dir)
  selectTerm <- as.character(scale_param)
  selectSet <- all_files[grepl(selectTerm, all_files, fixed=T)]
  
  if(scale_param == 2){
    selectSet = selectSet[!grepl("20",selectSet,fixed=T)]
  }
  
  view <- stack(file.path(can_cost_dir, selectSet))

  # TODO this part is costly
  if(standardize){
    denom <- stack(file.path(nat_cost_dir,
                             gsub(".tif", "no_HF.tif", selectSet ,fixed=T)))
    for(i in 1:nlayers(view)){
      view[[i]] <- view[[i]] / denom[[i]]
    }
    rm(denom)
  }  
  
  # Plot whole landscape
  names(view) <- gsub(".tif","",selectSet,fixed=T)
  
  if (!is.null(scenarios)){
    plotStack <- subset(view, paste0(scenarios, scale_param))
    names(plotStack) <- paste0(scenarios, scale_param)
  } else {
    plotStack <- view
    names(plotStack) <- gsub("[[:digit:]]", "", gsub(".tif","",selectSet,fixed=T))
  }
  
  # TODO this line is costly
  # plotStack <- plotStack*raster(countryR)
  # countryR <- raster(countryR)
  # plotStack[is.na(countryR)] <- NA
  print("ready to plot")
  
  pdf(paste0("outputs/figures","/fig5MapsRawBig",selectTerm,"std",standardize,".pdf"))
  par(mar=c(0,0,0,0), oma=c(0,0,0,0))
  print(levelplot(plotStack,xlab=NULL,ylab=NULL,scales=list(draw=FALSE),
                  maxpixels = 2e5,col.regions=rev(terrain.colors(255))))
  dev.off()
  
  raster::removeTmpFiles(0)
  return(NULL)
}
