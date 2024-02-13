
# I copied these from the rocker/r-bspm Dockerfile because it sets them in the
# RProfile but it wasn't working
# bspm::enable()
# options(pkgType="binary", install.packages.check.source = "no")



# restore required packages with renv
install.packages("remotes")
remotes::install_deps()

source("scripts/1_prepare_data.R")
source("scripts/2_connectivity_analysis.R")

# deps1 <- rcompendium:::get_deps_extra("scripts/scripts2") |>
#   stringr::str_extract("(.*)::", group = 1) |> unique()
# 
# deps2 <- rcompendium:::get_deps_extra("scripts/scripts2") |> 
#   stringr::str_subset("::", negate = T) |> unique()
# 
# dplyr::union(deps1, deps2)|> na.omit() |> sort() |> paste0(collapse = ",\n")|> cat()
