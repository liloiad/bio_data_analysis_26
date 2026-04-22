###############################################################################
# COMPLETE COMMENTED SCRIPT
# ADD CLIMATE DATA TO AN EXISTING SPECIES COORDINATE TABLE
###############################################################################
# =========================
# 1) PACKAGES
# =========================

library(Rchelsa)
library(terra)
library(dplyr)
library(ggplot2)

# =========================
# 2) STARTING DATASET
# =========================
# Example: an existing table containing occurrences of one species
# with an ID, species name, longitude and latitude


View(sal_full)

# *sal_full if i want the data avout salamandra
# *cyp_full if i want the data about cypropedium 
# *matrix_full if i want all the data

species_df <- data.frame(
  longitude = sal_full$longitude,     # change there *
  latitude  = sal_full$latitude) %>%  # and there *
  mutate(occurrence_id = row_number())

View(species_df)

# =========================
# 3) CREATE A SPATIAL OBJECT
# =========================
# CHELSA requires coordinates. We therefore create a spatial vector
# from the longitude and latitude columns.

pts_v <- terra::vect(
  species_df,
  geom = c("longitude", "latitude"),
  crs = "EPSG:4326"
)

# Extract simple coordinates as a standard data frame
coords_df <- as.data.frame(terra::geom(pts_v)[, c("x", "y")]) %>%
  rename(
    longitude = x,
    latitude = y
  ) %>%
  mutate(occurrence_id = species_df$occurrence_id)

head(coords_df)

# =========================
# 4) EXTRACT MONTHLY Tmax FOR 2018
# =========================
# CHELSA variable naming:
# - tas    = near-surface air temperature
# - tasmin = minimum near-surface air temperature
# - tasmax = maximum near-surface air temperature
# - pr     = precipitation
#
# Temperature values are often returned in Kelvin.
# Conversion to Celsius: °C = K - 273.15

tmax_r <- getChelsa(
  var       = "tasmax",
  coords    = coords_df %>% dplyr::select(longitude, latitude),
  startdate = as.Date("2018-01-01"),
  enddate   = as.Date("2019-01-01"),
  dataset   = "chelsa-monthly"
)

# Remove the time column and
# calculate the mean across the 12 months for each point
# colMeans() works by column, and here each column corresponds to one point
tmax_mean_k <- colMeans(tmax_r[, -1], na.rm = TRUE)

# Convert Kelvin to Celsius
tmax_mean_c <- tmax_mean_k - 273.15

# Create a table containing the new climate variable
tmax_df <- data.frame(
  occurrence_id = species_df$occurrence_id,
  tmax_mean_c = as.numeric(tmax_mean_c)
)

tmax_df

# =========================
# 5) EXTRACT MONTHLY PRECIPITATION FOR 2018
# =========================

prec_r <- getChelsa(
  var       = "pr",
  coords    = coords_df %>% dplyr::select(longitude, latitude),
  startdate = as.Date("2018-01-01"),
  enddate   = as.Date("2019-01-01"),
  dataset   = "chelsa-monthly"
)

# Remove the time column and
# Calculate the mean across the 12 months for each point
prec_mean <- colMeans(prec_r[, -1], na.rm = TRUE)

# Create a table containing the precipitation variable
prec_df <- data.frame(
  occurrence_id = species_df$occurrence_id,
  prec_mean_annual = as.numeric(prec_mean)
)

prec_df

# =========================
# 6) JOIN THE NEW CLIMATE VARIABLES
#    TO THE ORIGINAL DATASET
# =========================
# This is the key teaching point:
# we start from an existing dataset and add new columns
# extracted from an external source.

species_climate_df <- species_df %>%
  left_join(tmax_df, by = "occurrence_id") %>%
  left_join(prec_df, by = "occurrence_id")

species_climate_df

# =========================
# 7) CHECK THE RESULT
# =========================

dim(species_df)           # original dimensions
dim(species_climate_df)   # enriched dimensions
names(species_climate_df) # column names after enrichment

# =========================
# 8) PLOT THE DISTRIBUTION OF ANNUAL MEAN Tmax
# =========================

tm <- ggplot(species_climate_df, aes(x = tmax_mean_c)) +
  geom_density(color = "darkred", fill = "salmon", adjust = 1.5) +
  theme_classic() +
  labs(
    title = "Annual mean Tmax (2018)",
    x = "Annual mean Tmax (°C)",
    y = "Density"
  )

#print(tm)

# =========================
# 9) PLOT THE DISTRIBUTION OF ANNUAL MEAN PRECIPITATION
# =========================

pm <- ggplot(species_climate_df, aes(x = prec_mean_annual)) +
  geom_density(color = "black", fill = "darkgreen", adjust = 1.5) +
  theme_classic() +
  labs(
    title = "Annual mean precipitation (2018)",
    x = "Annual mean precipitation",
    y = "Density"
  )

#print(pm)

# 10)  CURRENT CLIMATE VS FUTURE CLIMATE
#     SIMPLIFIED EXAMPLE WITH JULY ONLY
# =========================
# Here, instead of averaging all 12 months, we extract climate data
# for one particular month only: July.
#
# This is often easier for teaching because the workflow is simpler:
# one month -> one extraction -> one new column

# ------------------------------------------------------------
# 10A) CURRENT CLIMATE: July temperature
#      climatology over 1981-2010
# ------------------------------------------------------------

tas_cur_july <- getChelsa(
  var     = "tas",
  coords  = coords_df %>% dplyr::select(longitude, latitude),
  date    = c(7, 1981, 2010),   # July climatology
  dataset = "chelsa-climatologies"
)

tas_cur_july_df <- data.frame(
  occurrence_id = species_df$occurrence_id,
  tas_current_july_c = tas_cur_july[, -1] %>%
    unlist() %>%
    as.numeric() - 273.15
)

tas_cur_july_df

# ------------------------------------------------------------
# 10B) FUTURE CLIMATE: July temperature in 2050 under SSP126
# ------------------------------------------------------------

tas_fut_july <- getChelsa(
  var     = "tas",
  coords  = coords_df %>% dplyr::select(longitude, latitude),
  date    = as.Date("2050-07-01"),
  dataset = "chelsa-climatologies",
  ssp     = "ssp126",
  forcing = "MPI-ESM1-2-HR"
)

tas_fut_july_df <- data.frame(
  occurrence_id = species_df$occurrence_id,
  tas_future_july_2050_c = tas_fut_july[, -1] %>%
    unlist() %>%
    as.numeric() - 273.15
)

tas_fut_july_df

# ------------------------------------------------------------
# 10C) ADD CURRENT AND FUTURE JULY TEMPERATURE
#      TO THE ORIGINAL TABLE
# ------------------------------------------------------------

species_climate_future_df <- species_climate_df %>%
  left_join(tas_cur_july_df, by = "occurrence_id") %>%
  left_join(tas_fut_july_df, by = "occurrence_id") %>%
  mutate(
    delta_tas_july_c = tas_future_july_2050_c - tas_current_july_c
  )

species_climate_future_df

# ------------------------------------------------------------
# 10C) ADD CURRENT AND FUTURE CLIMATE TO THE TABLE
# ------------------------------------------------------------

# Ne marche pas -->     question!!!!!!!!!!

#species_climate_future_df <- species_climate_df %>%
#  left_join(tas_cur_df, by = "occurrence_id") %>%
#  left_join(tas_fut_df, by = "occurrence_id") %>%
#  mutate(
#    delta_tas_c = tas_future_2050_c - tas_current_c
#  )

#species_climate_future_df

# =========================
# 11) PLOT CURRENT VS FUTURE TEMPERATURE
# =========================

cf <- ggplot(species_climate_future_df, aes(x = tas_current_july_c, y = tas_future_july_2050_c)) +
  geom_point(size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  theme_classic() +
  labs(
    title = "Current vs future July temperature",
    x = "Current July temperature (°C)",
    y = "Future July temperature in 2050 (°C)"
  )

print(cf)
