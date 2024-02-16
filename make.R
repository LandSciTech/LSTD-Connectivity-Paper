
# I copied these from the rocker/r2u instructions. It seems the Rprofile.site
# from the docker image is not being used so need to set here 
options(bspm.version.check=FALSE)
bspm::enable()

Sys.setenv(GITHUB_PAT = "<pat>")

# # install required packages based on list in DESCRIPTION
install.packages("remotes")
remotes::install_deps()

# seems like some R process is continuing to take up memory and is not fixed by
# gc or removeTmpFiles so setting up to run each batch in separate R session/task
sce_use <- commandArgs(trailingOnly = TRUE)[[1]]
cat("running sce", sce_use)

if(as.character(sce_use) == "1"){
  source("scripts/2b_combine_stats.R")
} else {
  source("scripts/2_connectivity_analysis.R")
}

# source("scripts/1_prepare_data.R")


# Used to get list of required packages for just the analysis scripts
# deps1 <- rcompendium:::get_deps_extra("scripts/scripts2") |>
#   stringr::str_extract("(.*)::", group = 1) |> unique()
# 
# deps2 <- rcompendium:::get_deps_extra("scripts/scripts2") |> 
#   stringr::str_subset("::", negate = T) |> unique()
# 
# dplyr::union(deps1, deps2)|> na.omit() |> sort() |> paste0(collapse = ",\n")|> cat()
