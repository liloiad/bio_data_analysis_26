# ==============================================================================
# ANALYSIS 2: COMPARATIVE BOXPLOTS OF ECOLOGICAL NICHES
# Objective: To validate the trends seen in the PCA, comparing directly the 
# ecological variables between the two species.
# ==============================================================================

my_theme <- theme_minimal() +
  theme(
    plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 9),
    axis.text.x = element_text(angle = 15, hjust = 1), # Inclina i nomi delle specie per leggerli meglio
    legend.position = "none"
  )

# Same colors that before
colors <- c("Cypripedium calceolus" = "#e63946", "Salamandra atra" = "#1d3557")

# 1. Boxplot for Altitudinal Distribution (elevation)
p1 <- ggplot(df, aes(x = species, y = elevation, fill = species)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 16) +
  scale_fill_manual(values = colors) +
  labs(title = "Altitudinal Distribution", x = "", y = "Elevation (m a.s.l.)") +
  my_theme

# 2. Boxplot for Average High Temperature (tmax_mean)
p2 <- ggplot(df, aes(x = species, y = tmax_mean, fill = species)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 16) +
  scale_fill_manual(values = colors) +
  labs(title = "Average High Temperature", x = "", y = "Temperature (°C)") +
  my_theme

# 3. Boxplot for Vegetation Productivity (NDVI)
p3 <- ggplot(df, aes(x = species, y = NDVI, fill = species)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 16) +
  scale_fill_manual(values = colors) +
  labs(title = "Vegetation Productivity", x = "", y = "NDVI") +
  my_theme

# 4. Boxplot for Average Precipitation (precip_mean)
p4 <- ggplot(df, aes(x = species, y = precip_mean, fill = species)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 16) +
  scale_fill_manual(values = colors) +
  labs(title = "Average Precipitation", x = "", y = "Precipitation (mm)") +
  my_theme

# --- UNION OF GRAPH ---
grid <- plot_grid(p1, p2, p3, p4, 
                              ncol = 2, nrow = 2, 
                              labels = c("A", "B", "C", "D"),
                              label_size = 12,
                              align = "hv")

# Add the title
title <- ggdraw() + 
  draw_label("Direct Comparison of Niche Environmental Parameters", 
             fontface = "bold", size = 14, hjust = 0.5)

# Title + graph
final_grid <- plot_grid(title, grid, 
                            ncol = 1, 
                            rel_heights = c(0.1, 1)) # Riserva il 10% dello spazio in alto per il titolo


print(final_grid)

# ============================================================
#  EXPORT
# ============================================================

#ggsave("boxplot_comparison.png", final_grid,
#       width = 13, height = 16, dpi = 300, bg = "white")

#ggsave("boxplot_comparison.pdf", final_grid,
#       width = 13, height = 16, device = cairo_pdf)
