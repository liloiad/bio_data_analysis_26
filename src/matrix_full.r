library(rgbif)         # access to GBIF data
library(rnaturalearth) # country maps
library(ggplot2)       # graphics
library(rinat)         # access to iNaturalist data
library(raster)        # spatial extent management
library(dplyr)         # table manipulation
library(sf)            # modern spatial objects

# Disable spherical geometry for simpler spatial operations
sf_use_s2(FALSE)

# =========================
# 2) DOWNLOAD GBIF DATA 
# =========================

# FOR SALAMANDRA ATRA

# Download occurrences with coordinates
gbif_sal_raw <- occ_data(
  scientificName = "Salamandra atra",
  hasCoordinate = TRUE,
  limit = 1000
)

# Extract the main data table
gbif_sal_occ <- gbif_sal_raw$data

# Select occurrences located in Switzerland
gbif_sal_ch <- gbif_sal_occ %>%
  filter(country == "Switzerland")


# Quick base plot for checking
#windows() 
#plot(
#  gbif_sal_ch$decimalLongitude,
#  gbif_sal_ch$decimalLatitude,
#  pch = 16,
#  col = "darkgreen",
#  xlab = "Longitude",
#  ylab = "Latitude",
#  main = "GBIF occurrences of Salamandra atra in Switzerland"
#)

# FOR CYPROPEDIUM CALCEOLUS

# Download occurrences with coordinates
gbif_cyp_raw <- occ_data(
  scientificName = "Cypripedium calceolus L.",
  hasCoordinate = TRUE,
  limit = 1000
)

# Extract the main data table
gbif_cyp_occ <- gbif_cyp_raw$data

# Select occurrences located in Switzerland
gbif_cyp_ch <- gbif_cyp_occ %>%
  filter(country == "Switzerland")


# Quick base plot for checking
#windows() 
#plot(
#  gbif_cyp_ch$decimalLongitude,
#  gbif_cyp_ch$decimalLatitude,
#  pch = 16,
#  col = "black",
#  xlab = "Longitude",
#  ylab = "Latitude",
#  main = "GBIF occurrences of Cypripedium calceolus in Switzerland"
#)

# =============== 2.1) FORMAT GBIF DATA

# FOR SALAMANDRA ATRA
# creation of new data frame with the name of the column that will be use also for iNat dataframe

# Keep only the useful columns
# eventDate may contain date + time; as.Date() keeps only the date
gbif_sal <- data.frame(
  species   = gbif_sal_ch$species,
  latitude  = gbif_sal_ch$decimalLatitude,
  longitude = gbif_sal_ch$decimalLongitude,
  date_obs  = as.Date(gbif_sal_ch$eventDate),
  source    = "gbif"
)

# FOR CYPROPEDIUM CALCEOLUS
# creation of new data frame with the name of the column that will be use also for iNat dataframe
gbif_cyp <- data.frame(
  species   = gbif_cyp_ch$species,
  latitude  = gbif_cyp_ch$decimalLatitude,
  longitude = gbif_cyp_ch$decimalLongitude,
  date_obs  = as.Date(gbif_cyp_ch$eventDate),
  source    = "gbif"
)


# ================================
# 3) DOWNLOAD iNaturalist DATA
# ================================

# FOR SALAMANDRA ATRA
inat_sal_raw <- get_inat_obs(
  query = "Salamandra atra",
  place_id = "switzerland"
)

# FOR CYPROPEDIUM CALCEOLUS
inat_cyp_raw <- get_inat_obs(
  query = "Cypripedium calceolus",
  place_id = "switzerland"
)

# =============== 3.1) FORMAT iNaturalist DATA

# FOR SALAMANDRA ATRA
# same name column that before for Gbif
inat_sal <- data.frame(
  species   = inat_sal_raw$scientific_name,
  latitude  = inat_sal_raw$latitude,
  longitude = inat_sal_raw$longitude,
  date_obs  = as.Date(inat_sal_raw$observed_on),
  source    = "inat"
)

# FOR CYPROPEDIUM CALCEOLUS
# same name column that before for Gbif
inat_cyp <- data.frame(
  species   = inat_cyp_raw$scientific_name,
  latitude  = inat_cyp_raw$latitude,
  longitude = inat_cyp_raw$longitude,
  date_obs  = as.Date(inat_cyp_raw$observed_on),
  source    = "inat"
)

# ================================
# 4) MERGE ALL DATA
# ================================

cyp_full <- bind_rows(gbif_cyp,inat_cyp)

sal_full <- bind_rows(gbif_sal,inat_sal)

# Collaps all matrix in only one: Gbif (sal + cyp) + iNat (sal + cyp)
matrix_full <- bind_rows(gbif_sal, gbif_cyp,inat_sal,inat_cyp)

# ================================
# 5) MAP OF COMBINED DATA
# ================================

# Download the outline of Switzerland

Switzerland <- ne_countries(
  scale = "medium",
  returnclass = "sf",
  country = "Switzerland"
)

# The matrix contains two different names for the same species 
# => here I change the names to have only one name for each species (Salamandra atra and Cypripedium calceolus)

unique(matrix_full$species)
# In the two database we have two different name for the same species, here i change 
# the name to have only one name for each species in my matrix_full
matrix_full$species[matrix_full$species == "Salamandra atra atra"] <- "Salamandra atra"
matrix_full$species[matrix_full$species == "Cypripedium parviflorum"] <- "Cypripedium calceolus"

p1 <-   ggplot(data = Switzerland) +
        geom_sf(fill = "grey95", color = "black") +
        geom_point(
          data = matrix_full,
          aes(x = longitude, y = latitude, fill = species),
          size = 3,
          shape = 21,
          color = "black",
          alpha = 0.8
        ) +
        labs(
          title = "Combined occurrences",
        ) +
        theme_classic()

print(p1)
View(matrix_full)