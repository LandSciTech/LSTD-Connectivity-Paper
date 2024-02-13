
# I copied these from the rocker/r-bspm Dockerfile because it sets them in the
# RProfile but it wasn't working
# bspm::enable()
# options(pkgType="binary", install.packages.check.source = "no")

r = getOption("repos")
r["CRAN"] = "https://packagemanager.posit.co/cran/__linux__/jammy/latest"
options(repos = r)

# restore required packages with renv
install.packages("renv")
renv::restore()

source("scripts/1_prepare_data.R")
source("scripts/2_connectivity_analysis.R")

