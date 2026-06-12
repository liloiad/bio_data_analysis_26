# ==============================================================================
# ANALYSIS 1: PRINCIPAL COMPONENT ANALYSIS (PCA)
# Objective: visualize the separation of the ecological niches of the two species
# ==============================================================================

# ============================================================
# 1 - Select continuous quantitative ecological variables 
# ============================================================

pca_vars <- c("elevation", "NDVI", "precip_mean", "tmax_mean")
              #"current_t_july", "future_t_july") To make the pCA graph cleaner 
              # I keep only one of the variables on the tempertaura since they 
              # overlap in the graph

pca_data <- df[, pca_vars]
species_vector <- as.factor(df$species)

# ============================================================
# 2 - Execution of the PCA 
# ============================================================

res_pca <- PCA(pca_data, scale.unit = TRUE, graph = FALSE)

# ============================================================
#  3 - Correlation Circle
# ============================================================

p_var <- fviz_pca_var(res_pca, 
                      col.var = "contrib", 
                      gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                      repel = TRUE) +
         labs(title = "Contribution of Environmental Variables", 
              x = "PC1", y = "PC2") +
         theme_minimal()

print(p_var)

# INTERPRETATION
# Since t_max_mean and elevation are aligned with the horizontal axis
# are the environmental variables that best explain the variability 
# in the dataset. The fact that these two are located one on the opposite side 
# of the other in graph (180°) signifies that there is a negative correlation
# between the two: the higher you go the cooler it is

# If we look the vertical axis (PC2) we have more precipitation in colder and 
# higher areas and a higher NDVI index at lower and warmer altitudes.

# ============================================================
#  4 - Biplot for species
# ============================================================
windows()

p_ind <- fviz_pca_ind(res_pca,
                      label = "none", 
                      habillage = species_vector, # Color by species
                      palette = c("#e63946", "#1d3557"),
                      addEllipses = TRUE,
                      ellipse.type = "confidence",
                      legend.title = "Species") +
         labs(title = "Principal Component Analysis (PCA)", 
              x = "PC1", y = "PC2") +
         theme_minimal()

print(p_ind)

# INTERPRETATION
# The two species have separate niches:
#   1. Altitude-Temperature Gradient: 
#   - The blue dots of Cypripedium calceolus are skewed to the left 
#       (more blue circles on the left) showing that orchids prefer 
#       lower and warmer altitudes
#   - The yellow dots of Salamandra atra are more to the right and central 
#       demonstrating that apline salamadras prefer higher altitudes with 
#       lower temperaures
#   2. Median position of species (ellipses)
#   The two ellipses are separate and are located on opposite sides of the horizontal 
#   axis and vertical demonstrating the clear separation between the two species
#   3. Overlap areas
#   The blue and yellow dots often overlap so the two species share
#   the same macro-habitat (alps) maintaining different preferences in terms of 
#   micro-habit 