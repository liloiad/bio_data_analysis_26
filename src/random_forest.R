# ==============================================================================
# ANALYSIS 4: RANDOM FOREST FOR FEATURE IMPORTANCE 
# Objective: Calculate feature importance for all predictors in the dataset
# to identify the key factors that distinguish the two alpine species
# ==============================================================================

############################################################
# 1) Import occurrence data
############################################################

# This table contains:
# - species names
# - latitude and longitude
# - environmental predictors extracted at each occurrence point

# Basic inspection of the data
head(df)
str(df)
names(df)

# Number of occurrences per species
table(df$species)


############################################################
# 2) Import environmental prediction grid
############################################################

# This table represents a regular environmental grid over Switzerland.
#
# Each row is one geographic point.
# Each point has environmental predictors:
# elevation, precipitation, temperature, NDVI, land cover, etc.
#
# In a real ecological study, this grid should come from real raster layers.
# For this teaching exercise, we use a pre-prepared CSV file.

grid_pred <- read.csv(
  "./data/fake_grid_switzerland.csv"
)

head(grid_pred)
str(grid_pred)


############################################################
# 3) First map of occurrence points
############################################################

# This first plot simply shows where the occurrence points are located.

ggplot(df, aes(x = longitude, y = latitude, color = species)) +
  geom_point(size = 2, alpha = 0.7) +
  coord_equal() +
  theme_classic() +
  labs(
    title = "Occurrence points of the Cardamine species",
    x = "Longitude",
    y = "Latitude",
    color = "Species"
  )


############################################################
# 4) Prepare occurrence data for machine learning
############################################################

# We create a clean table for the Random Forest model.
#
# The response variable is:
# - species
#
# The predictor variables are:
# - elevation
# - precip
# - temp
# - NDVI
# - Red, Green, Blue
# - eco_values
# - categorical environmental variables

ml_df <- df %>%
  select(
    species,
    elevation,
    precip_mean,
    tmax_mean,
    NDVI,
    Red,
    Green,
    Blue,
    eco_values,
    Temperatur,
    Moisture,
    Landcover,
    Landforms,
    Climate_Re,
    W_Ecosystm
  )

# Remove missing values.
# Random Forest cannot use rows with NA values.
ml_df <- na.omit(ml_df)

# Convert the response variable to a factor.
# This tells R that species is a categorical variable.
ml_df$species <- as.factor(ml_df$species)

# Convert categorical predictors to factors.
# Random Forest can use factors as categorical predictors.
ml_df$Temperatur <- as.factor(ml_df$Temperatur)
ml_df$Moisture   <- as.factor(ml_df$Moisture)
ml_df$Landcover  <- as.factor(ml_df$Landcover)
ml_df$Landforms  <- as.factor(ml_df$Landforms)
ml_df$Climate_Re <- as.factor(ml_df$Climate_Re)
ml_df$W_Ecosystm <- as.factor(ml_df$W_Ecosystm)

# Check the final structure
str(ml_df)

# Check the number of samples per species
table(ml_df$species)


############################################################
# 5) Train / test split
############################################################

# We split the data into:
# - 70% training data
# - 30% testing data
#
# The training data are used to build the model.
# The testing data are used to evaluate the model on unseen data.

set.seed(123)

train_index <- createDataPartition(
  y = ml_df$species,
  p = 0.7, # we select only 70% of the dataset and generate a sub dataset with this 70% and the 30% of the other dataset
  list = FALSE
)

train_df <- ml_df[train_index, ]
test_df  <- ml_df[-train_index, ]

# Check that both species are present in both datasets
table(train_df$species)
table(test_df$species)


############################################################
# 6) Train the Random Forest model
############################################################

# The formula species ~ . means:
# predict species using all other columns as predictors.
#
# ntree = 500 means that the forest contains 500 trees.
#
# importance = TRUE allows us to calculate variable importance.

rf_species <- randomForest(
  species ~ ., # in funtion of all the column
  data = train_df,
  ntree = 500,
  importance = TRUE
)

#print(rf_species)


############################################################
# 7) Prediction on test data
############################################################

# We now ask the model to predict the species
# of the test dataset.

pred_species <- predict(
  rf_species,
  newdata = test_df
)

#head(pred_species)


############################################################
# 8) Model evaluation
############################################################

# The confusion matrix compares:
# - predicted species
# - observed species
#
# It gives an estimate of model performance.
# for example to predict wich species i will find in some environnemnt

confusionMatrix(
  data = pred_species,
  reference = test_df$species
)


############################################################
# 9) Feature importance
############################################################

# Random Forest can estimate which variables are most useful
# for discriminating the species.

windows()

importance(rf_species)

# Basic Random Forest importance plot
varImpPlot(rf_species)

# Create a cleaner ggplot version

importance_df <- importance(rf_species) %>%
  as.data.frame()

importance_df$feature <- rownames(importance_df)

importance_df <- importance_df %>%
  arrange(desc(MeanDecreaseGini))

imp <- ggplot(
  importance_df,
  aes(
    x = reorder(feature, MeanDecreaseGini),
    y = MeanDecreaseGini
  )
) +
  geom_col() +
  coord_flip() +
  theme_classic() +
  labs(
    title = "Most important features to discriminate the species",
    x = "Feature",
    y = "Mean decrease in Gini"
  )

print(imp)

# INTERPRETATION 
# The feature importance plots shows that temperature and precipitation are the
# most important variables for the two species and it confirms what we saw with the PCA.
# These are the most important variables to determine the difference in ecological niche 
# between the two species
# The variables NDVI and precipitation are also important 

############################################################
# 10) Prepare the prediction grid
############################################################

# The prediction grid must contain the same predictor columns
# as the training data.
#
# It must NOT contain the response variable species,
# because this is what we want to predict.

grid_ml <- grid_pred %>%
  select(
    longitude,
    latitude,
    elevation,
    precip_mean,
    tmax_mean,
    NDVI,
    Red,
    Green,
    Blue,
    eco_values,
    Temperatur,
    Moisture,
    Landcover,
    Landforms,
    Climate_Re,
    W_Ecosystm
  )

# Convert categorical grid variables to factors.
#
# Important:
# The factor levels must be exactly the same as in the training data.
# Otherwise, R may not be able to use the Random Forest model.

grid_ml$Temperatur <- factor(
  grid_ml$Temperatur,
  levels = levels(train_df$Temperatur)
)

grid_ml$Moisture <- factor(
  grid_ml$Moisture,
  levels = levels(train_df$Moisture)
)

grid_ml$Landcover <- factor(
  grid_ml$Landcover,
  levels = levels(train_df$Landcover)
)

grid_ml$Landforms <- factor(
  grid_ml$Landforms,
  levels = levels(train_df$Landforms)
)

grid_ml$Climate_Re <- factor(
  grid_ml$Climate_Re,
  levels = levels(train_df$Climate_Re)
)

grid_ml$W_Ecosystm <- factor(
  grid_ml$W_Ecosystm,
  levels = levels(train_df$W_Ecosystm)
)

# Remove rows with missing values.
#
# Missing values may appear if some categories in the grid
# are not present in the training data.

grid_ml <- na.omit(grid_ml)

str(grid_ml)


############################################################
# 11) Predict species probabilities on the grid
############################################################

# type = "prob" asks the model to return probabilities
# instead of only the most likely class.
#
# The output contains one column per species.

grid_prob <- predict(
  rf_species,
  newdata = grid_ml,
  type = "prob"
)

head(grid_prob)

# Combine coordinates, predictors and probabilities in one table.

grid_map <- cbind(grid_ml, grid_prob)

head(grid_map)


############################################################
# 12) Map probability for one selected species
############################################################

# First, list all species available in the model.

species_names <- levels(train_df$species)

species_names

# ----------------------------------------------------------
# IMPORTANT 
# ----------------------------------------------------------
# To change the species displayed on the probability map,
# modify only the line below.
#
# Example:
# target_species <- "Cardamine bellidifolia"
#
# or:
# target_species <- "Cardamine resedifolia"
#
# The name must be written exactly as it appears in:
# species_names
# ----------------------------------------------------------

target_species <- "Cypripedium calceolus" #Use the target_species variable to select the correct column for fill.
# Cypripedium calceolus or Salamandra atra


ggplot(grid_map, aes(x = longitude, y = latitude)) +
  geom_tile(aes(fill = .data[[target_species]])) + #  geom_tile creates a colored tile for each grid point, colored according to the predicted probability of the target species.
  geom_point(
    data = df,
    aes(x = longitude, y = latitude),
    inherit.aes = FALSE,
    color = "black",
    size = 0.8,
    alpha = 0.5
  ) +
  scale_fill_viridis_c(limits = c(0, 1)) +
  coord_equal() +
  theme_classic() +
  labs(
    title = paste("Predicted probability map for", target_species),
    subtitle = "Prediction on an environmental grid over Switzerland",
    x = "Longitude",
    y = "Latitude",
    fill = "Probability"
  )


############################################################
# Create the probability map with occurrence points colored by species
############################################################

ggplot(grid_map, aes(x = longitude, y = latitude)) +
  
  # Probability map
  geom_tile(aes(fill = .data[[target_species]])) +
  
  # Add occurrence points
  geom_point( 
    data = df,
    aes(x = longitude, y = latitude, color = species),
    inherit.aes = FALSE,
    size = 1,
    alpha = 0.7
  ) +
  
  # Color scale from 0 to 1
  scale_fill_viridis_c(limits = c(0, 1)) +
  
  # Keep correct geographic proportions
  coord_equal() +
  
  # Simple theme
  theme_classic() +
  
  # Plot labels
  labs(
    title = paste("Predicted probability map"),
    subtitle = "Random Forest prediction on the environmental grid",
    x = "Longitude",
    y = "Latitude",
    fill = "Probability",
    color = "Observed species"
  )
