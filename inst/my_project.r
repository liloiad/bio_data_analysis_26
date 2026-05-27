# Install packages:
install.packages(c("rgbif", "rnaturalearth", "ggplot2", "rinat",
                   "raster", "dplyr", "sf", "rnaturalearthdata",
                   "elevatr", "progress"))

install.packages("remotes")
library(remotes)
install_git("https://gitlabext.wsl.ch/karger/rchelsa.git")
install_github("ropensci/MODIStsp")
install.packages('luna', repos='https://rspatial.r-universe.dev')


source("./src/matrix_full.r")

source("./src/ecosystems.r")

source("./src/elevation.R")

source("./src/sat_manual.r")

source("./src/climate.r")

source("./src/combined_figures.R")
