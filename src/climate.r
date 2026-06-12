###############################################################################
# COMPLETE COMMENTED SCRIPT
# ADD CLIMATE DATA TO AN EXISTING SPECIES COORDINATE TABLE
###############################################################################
# =========================
# 1) PACKAGES in my_project.r
# =========================

# =========================
# 2) STARTING DATASET
# =========================
# Example: an existing table containing occurrences of one species
# with an ID, species name, longitude and latitude

matrix_full_eco 

# =========================
# 3) PREPARING SPATIAL COORDINATES
# =========================
# The Rchelsa package requires coordinates in a specific format

# 3a) Create a spatial object with terra::vect
pts_v <- terra::vect(
  matrix_full_eco,
  geom = c("longitude", "latitude"),
  crs  = "EPSG:4326"  # WGS84 coordinate system
)

# 3b) Extract coordinates as a data.frame
coords_df <- as.data.frame(terra::geom(pts_v)[, c("x", "y")]) %>%
  rename(
    longitude = x,
    latitude  = y
  ) %>%
  mutate(occurrence_id = matrix_full_eco$occurrence_id)

#print(coords_df)

###############################################################################
# PART 1: EXTRACTING MONTHLY DATA (YEAR 2018)
###############################################################################

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

# =========================
# 5) MONTHLY PRECIPITATION
# =========================

precip_raw <- getChelsa(
  var       = "pr",
  coords    = coords_df %>% dplyr::select(longitude, latitude),
  startdate = as.Date("2018-01-01"),
  enddate   = as.Date("2019-01-01"),
  dataset   = "chelsa-monthly"
)

# Calculate annual mean (or sum depending on your needs)
precip_mean <- colMeans(precip_raw[, -1], na.rm = TRUE)


# =========================
# 6) MERGING CLIMATE VARIABLES WITH THE INITIAL TABLE
# =========================

matrix_full_eco <- data.frame(
  matrix_full_eco,
  precip_mean = precip_mean,
  tmax_mean = tmax_mean_c
)

#View(matrix_full_eco)

# =========================
# 9) VISUALISATIONS OF CURRENT DATA
# =========================

# Plot 1: Distribution of maximum temperature
p1 <- ggplot(matrix_full_eco, aes(x = tmax_mean_c)) +
  geom_density(color = "darkred", fill = "salmon", alpha = 0.6, adjust = 1.5) +
  geom_rug(color = "darkred") +
  theme_classic(base_size = 12) +
  labs(
    title    = "Distribution of mean maximum temperature",
    subtitle = "Alpine species - Year 2018",
    x = "Mean annual maximum temperature (°C)",
    y = "Density"
  ) +
  theme(plot.title = element_text(face = "bold"))

print(p1)

# Plot 2: Distribution of precipitation
p2 <- ggplot(matrix_full_eco, aes(x = precip_mean)) +
  geom_density(color = "black", fill = "darkgreen", alpha = 0.6, adjust = 1.5) +
  geom_rug(color = "darkgreen") +
  theme_classic(base_size = 12) +
  labs(
    title    = "Distribution of mean precipitation",
    subtitle = "Alpine species - Year 2018",
    x = "Mean annual precipitation (mm)",
    y = "Density"
  ) +
  theme(plot.title = element_text(face = "bold"))

print(p2)

# Plot 3: Temperature-precipitation relationship
p3 <- ggplot(matrix_full_eco, aes(x = tmax_mean_c, y = precip_mean)) +
  geom_point(size = 3, color = "steelblue", alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed") +
  theme_classic(base_size = 12) +
  labs(
    title    = "Temperature-precipitation relationship",
    subtitle = "Alpine species  - Year 2018",
    x = "Mean maximum temperature (°C)",
    y = "Mean precipitation (mm)"
  ) +
  theme(plot.title = element_text(face = "bold"))

print(p3)

###############################################################################
# PART 2: CURRENT CLIMATE VS FUTURE CLIMATE
###############################################################################

# To keep the teaching simple, we focus on a single month: JULY
# This allows understanding the logic without the complexity of 12 months

# =========================
# 10) CURRENT CLIMATE: July temperature (climatology 1981-2010)
# =========================

tas_current_july <- getChelsa(
  var     = "tas",
  coords  = coords_df %>% dplyr::select(longitude, latitude),
  date    = c(7, 1981, 2010),   # Month 7 (July), period 1981-2010
  dataset = "chelsa-climatologies"
)

# Create a vector with current temperatures in july
current_july_df <- as.numeric(tas_current_july[1, -1])- 273.15

#View(current_july_df)

# =========================
# 11) FUTURE CLIMATE: July temperature in 2050 (SSP126 scenario)
# =========================

# Available emission scenarios:
# - SSP126: optimistic scenario (low emissions)
# - SSP370: intermediate scenario
# - SSP585: pessimistic scenario (high emissions)

# Available climate models:
# - MPI-ESM1-2-HR: German model (Max Planck Institute)
# - GFDL-ESM4: US model
# - IPSL-CM6A-LR: French model
# - UKESM1-0-LL: British model

tas_future_july <- getChelsa(
  var     = "tas",
  coords  = coords_df %>% dplyr::select(longitude, latitude),
  date    = as.Date("2051-07-01"),
  dataset = "chelsa-climatologies",
  ssp     = "ssp126",           # Emission scenario
  forcing = "MPI-ESM1-2-HR"     # Climate model
)


# Create a vector with future temperatures in july
future_july_df <- as.numeric(tas_future_july[1, -1])- 273.15

#View(future_july_df)

# =========================
# 12) MERGING AND CALCULATING CHANGE
# =========================

matrix_full_eco <- data.frame(
  matrix_full_eco,
  current_t_july = current_july_df,
  future_t_july = future_july_df
)

#View(matrix_full_eco)

# add a column with the difference between current anf future temperatzre in july
matrix_full_eco <- matrix_full_eco %>%
  dplyr::mutate(
    july_temp_change_c = future_t_july - current_t_july
  )

# Statistics on temperature change
print(summary(matrix_full_eco$july_temp_change_c))
cat("\nMean projected change: ", 
    round(mean(matrix_full_eco$july_temp_change_c), 2), "°C\n\n")

# =========================
# 13) VISUALISATIONS OF FUTURE PROJECTIONS
# =========================

# Plot 4: Current vs future comparison
p4 <- ggplot(matrix_full_eco, 
             aes(x = current_t_july, y = future_t_july)) +
  geom_point(size = 4, color = "steelblue", alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", 
              color = "gray40", linewidth = 1) +
  annotate("text", x = min(matrix_full_eco$current_t_july) + 0.5,
           y = max(matrix_full_eco$future_t_july) - 0.5,
           label = "1:1 line\n(no change)", 
           color = "gray40", size = 3.5) +
  theme_classic(base_size = 12) +
  labs(
    title    = "July temperature: Current vs Future (2050)",
    subtitle = "Alpine species- SSP126 scenario",
    x = "Current July temperature (°C) [1981-2010]",
    y = "Future July temperature (°C) [2050]"
  ) +
  theme(plot.title = element_text(face = "bold"))

print(p4)

# Plot 5: Distribution of temperature change
p5 <- ggplot(matrix_full_eco, aes(x = july_temp_change_c)) +
  geom_histogram(bins = 10, fill = "orange", color = "black", alpha = 0.7) +
  geom_vline(xintercept = mean(matrix_full_eco$july_temp_change_c),
             color = "red", linetype = "dashed", linewidth = 1) +
  annotate("text", 
           x = mean(matrix_full_eco$july_temp_change_c) + 0.1,
           y = Inf,
           label = paste0("Mean: ", 
                         round(mean(matrix_full_eco$july_temp_change_c), 2), 
                         "°C"),
           color = "red", vjust = 2, hjust = 0) +
  theme_classic(base_size = 12) +
  labs(
    title    = "Distribution of projected temperature change",
    subtitle = "Difference between 2050 (SSP126) and 1981-2010",
    x = "July temperature change (°C)",
    y = "Number of occurrences"
  ) +
  theme(plot.title = element_text(face = "bold"))

print(p5)

# Plot 6: Map of changes
p6 <- ggplot(matrix_full_eco, 
             aes(x = longitude, y = latitude, color = july_temp_change_c)) +
  geom_point(size = 5, alpha = 0.8) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red",
                        midpoint = mean(matrix_full_eco$july_temp_change_c),
                        name = "Δ Temp (°C)") +
  theme_classic(base_size = 12) +
  labs(
    title    = "Spatial distribution of temperature change",
    subtitle = "July 2050 (SSP126) vs 1981-2010",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "right")

print(p6)
View(matrix_full_eco)

#Export final matrix
#write.table(matrix_full_eco, file="matrix_full.csv")
