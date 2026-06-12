# INSTALL PACKAGES

#install.packages(c("rgbif", "rnaturalearth", "ggplot2", "rinat",
#                   "raster", "dplyr", "sf", "rnaturalearthdata",
#                   "elevatr", "progress"))
#
#install.packages("remotes")
#install_git("https://gitlabext.wsl.ch/karger/rchelsa.git")
#install_github("ropensci/MODIStsp")
#install.packages('luna', repos='https://rspatial.r-universe.dev')

# LOAD REQUIRED PACKAGES

library(rgbif)         # access to GBIF data
library(rnaturalearth) # country maps
library(ggplot2)       # graphics
library(rinat)         # access to iNaturalist data
library(raster)        # spatial extent management
library(dplyr)         # table manipulation
library(sf)            # modern spatial objects
library(remotes)
library(elevatr)       # download elevation data
library(rnaturalearthdata)
library(rmarkdown)
library(progress)
library(luna)
library(MODIStsp)
library(appeears)
library(terra)
library(Rchelsa)
library(ggnewscale)
library(fmsb)
library(cowplot)
library(FactoMineR)
library(factoextra)
library(randomForest)
library(caret)
library(viridis)
library(tidyr)

### INTERMEDIATE PROJECT

# 1 - Creating a matrix with data on the two species from GBIF and iNat
source("./src/matrix_full.r")

# 2 - Adding ecosystem data to the matrix
source("./src/ecosystems.r")

# 3 - Adding elevation data to the matrix
source("./src/elevation.R")

# 4 - Adding satellite data to the matrix
source("./src/sat_manual.r")

# 5 - Adding climate data to the matrix
source("./src/climate.r")

### FINAL PROJECT

# 1 - Load the final environmental matrix
source("./src/read_matrix.r")

# 2 - PCA
source("./src/pca.r")

# 3 - Environmental comparison between species
source("./src/environmental_comparison.R")

# 4 - Random forest / discriminating variable analysis
# Feature importance plot and Predicted probability map
source("./src/random_forest.R")

# 5 - Analysis of impact of future climate change
source("./src/climate_change.r")

# 6 - Final summary panel figure
source("./src/combined_figures.R")
