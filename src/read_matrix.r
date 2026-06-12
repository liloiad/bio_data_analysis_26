# ==============================================================================
# STEP 1: LOADING THE MATRIX AND DEFINING THE ECOLOGICAL QUESTION
# Course: Biodiversity Data Analysis 
# ==============================================================================

# ECOLOGICAL QUESTION: What are the differences in the ecological and climatic 
# niches of the two alpine species Cypripedium calceolus and Salamandra atra, 
# and how might global climate change affect them?

df  <- read.csv("matrix_full.csv", stringsAsFactors = FALSE, sep=" ")
View(df)