#  WORKSHOP – DATA VISUALIZATION IN R
#  Data: Cardamine observations in Switzerland
#
#  Outline:
#    1. Load the data
#    2. Plot A – Map
#    3. Plot B – Radar chart
#    4. Plot C – Boxplot
#    5. Plot D – Scatter plot
#    6. Combine the plots with cowplot (ggdraw)
# ============================================================


# ---- Packages ----
# Install only once if needed:
# install.packages(c("ggplot2","dplyr","sf","terra","rnaturalearth",
#                    "rnaturalearthdata","ggnewscale","fmsb","cowplot"))

library(ggplot2)
library(dplyr)
library(sf)
library(terra)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggnewscale)
library(fmsb)
library(cowplot)


# ---- Working directory and files ----
#setwd("C:/Users/didil/OneDrive/Desktop/Biodiversity data analysis/my_project_2026")

#df  <- read.csv("matrix_full_eco_elev_clim_sat.csv", stringsAsFactors = FALSE)
df <- matrix_full_eco
# Print a quick summary of the dataset dimensions
cat(nrow(df), "lignes,", ncol(df), "colonnes\n")
# Show the first rows to check that the import worked correctly
head(df)
windows()

# ============================================================
#  PLOT A – MAP OF SWITZERLAND
#  Background colored by ecosystem type (WTE) + points by species
# ============================================================

# -- Switzerland border --
ch_sf <- ne_countries(country = "Switzerland", scale = "medium",
                      returnclass = "sf")

# -- WTE raster: load it, crop it to Switzerland, and convert it to a table --
wte    <- terra::rast("./data/WorldEcosystem.tif")
wte_ch <- terra::crop(wte,    terra::vect(ch_sf))
wte_ch <- terra::mask(wte_ch, terra::vect(ch_sf))

wte_df           <- as.data.frame(wte_ch, xy = TRUE, na.rm = TRUE)
colnames(wte_df) <- c("x", "y", "Value")
wte_df$Value     <- as.numeric(wte_df$Value)

# -- Color palette: extracted directly from the dataset --
# The dataset already contains the hexadecimal color code (column "color")
# and the ecosystem name (column "W_Ecosystm") for each observation.
# We extract unique pairs to build the palette.
palette_wte <- df %>%
  select(Value = eco_values, W_Ecosystm, color) %>%
  distinct()

pal_wte <- setNames(palette_wte$color, palette_wte$W_Ecosystm)

# Join ecosystem names and colors to the raster table
wte_df <- left_join(wte_df, palette_wte, by = "Value")
wte_df$W_Ecosystm <- factor(wte_df$W_Ecosystm, levels = names(pal_wte))

# -- Points: a random seed is set to make the result reproducible --
set.seed(42)
pts_sf <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326)

# -- Build the plot --
graphA <- ggplot() +

  # WTE raster background (one color per ecosystem)
  geom_raster(data = wte_df,
              aes(x = x, y = y, fill = W_Ecosystm)) +
  scale_fill_manual(values = pal_wte, guide = "none") + # Mantiene nascosta la legenda del raster

  # new_scale_fill() is needed to use a second fill variable
  new_scale_fill() +

  # Observation points
  geom_sf(data = pts_sf,
          aes(fill = species, shape = species),
          size = 2.5, color = "white", stroke = 0.3) +
  # Rimosso guide = "none" per mostrare il colore
  scale_fill_manual(values = c("Salamandra atra" = "#1d3557",
                               "Cypripedium calceolus"= "#e63946"),
                    name = "Espèces") + 
  # Rimosso guide = "none" per mostrare la forma (R unisce automaticamente le legende se hanno lo stesso nome)
  scale_shape_manual(values = c(21, 24),
                     name = "Espèces") +

  # Switzerland outline
  geom_sf(data = ch_sf, fill = NA, color = "grey30", linewidth = 0.5) +

  # coord_sf() defines the visible geographic area of the map
  coord_sf(xlim = c(5.9, 10.6), ylim = c(45.8, 47.9), expand = FALSE) +

  labs(title = "A. Localisation des observations (fond World Ecosystem)") +
  theme_void() +  
  theme(
    plot.title = element_text(face = "bold", size = 10),
    legend.position = "right",                    # Posiziona la legenda a destra
    legend.title = element_text(face = "bold"),   # Titolo della legenda in grassetto
    legend.text = element_text(face = "italic")   # Nomi delle specie in corsivo
  )

print(graphA)



# ============================================================
#  PLOT B – RADAR CHART (fmsb)
#  Mean environmental profile by climate regime
# ============================================================

# -- Compute mean values by climate regime --
profils <- df %>%
  group_by(Climate_Re) %>%
  summarise(
    Elevation  = mean(elevation, na.rm = TRUE),
    Precip     = mean(precip_mean,    na.rm = TRUE),
    Temp       = mean(tmax_mean,      na.rm = TRUE),
    NDVI       = mean(NDVI,      na.rm = TRUE),
    .groups = "drop"
  )

profils_mat           <- as.data.frame(profils[, -1])
rownames(profils_mat) <- profils$Climate_Re

# -- Required 0-1 normalization --
# The variables have different units (m, °C, mm, etc.).
# Without normalization, the axes would not be comparable.
profils_norm <- as.data.frame(
  lapply(profils_mat, function(x) (x - min(x)) / (max(x) - min(x)))
)
rownames(profils_norm) <- rownames(profils_mat)

# -- fmsb format: row 1 = max, row 2 = min, then the data --
radar_data <- rbind(rep(1, 4), rep(0, 4), profils_norm)

# -- Colors --
cols_c <- c("#457b9d", "#2d6a4f", "#f4a261", "#c1121f")
cols_f <- adjustcolor(cols_c, alpha.f = 0.25)

# -- Draw the radar chart --
par(mar = c(1, 1, 3, 1))

radarchart(radar_data,
           axistype = 1,
           pcol = cols_c, pfcol = cols_f, plwd = 2,
           cglcol = "grey85", cglty = 1, vlcex = 0.8)

title("B. Profil environnemental par régime climatique",
      cex.main = 0.9, font.main = 2)

legend("topright",
       legend = rownames(profils_norm),
       col = cols_c, lwd = 2, bty = "n", cex = 0.7)


# ============================================================
#  PLOT C – BOXPLOT + OVERLAID POINTS (ggplot2)
#  Elevation by land cover type
# ============================================================

# Order categories by increasing median value to make the plot easier to read
ordre_lc       <- df %>%
  group_by(Landcover) %>%
  summarise(med = median(elevation, na.rm = TRUE), .groups = "drop") %>%
  arrange(med) %>%
  pull(Landcover)

df$Landcover_f <- factor(df$Landcover, levels = ordre_lc)

graphC <- ggplot(df, aes(x = Landcover_f, y = elevation, fill = Landcover_f)) +

  # Boxplots
  # outlier.shape = NA hides outliers because all points are drawn separately
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +

  # Overlaid points with horizontal jitter
  # width controls the amount of horizontal shift; alpha controls transparency
  geom_jitter(width = 0.2, size = 1.2, alpha = 0.3, color = "black") +

  # Reference line = global median
  geom_hline(yintercept = median(df$elevation, na.rm = TRUE),
             linetype = "dashed", color = "grey50") +

  scale_fill_manual(values = hcl.colors(length(ordre_lc), "Zissou 1"),
                    guide  = "none") +

  # coord_flip() swaps the axes: long labels are easier to read
  coord_flip() +

  labs(title = "C. Élévation par couverture du sol",
       x = NULL, y = "Élévation (m)") +

  theme_classic(base_size = 11) +  # simple theme with axes and no grey background  # simple theme with axes and no grey background
  theme(plot.title = element_text(face = "bold", size = 10))

print(graphC)


# ============================================================
#  PLOT D – SCATTER PLOT (ggplot2)
#  Elevation vs temperature, colored by species
# ============================================================

graphD <- ggplot(df, aes(x = elevation, y = tmax_mean, color = species)) +

  # Semi-transparent points
  geom_point(alpha = 0.45, size = 2) +

  # Regression line by species
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +

  scale_color_manual(
    values = c("Salamandra atra" = "#1d3557",
               "Cypripedium calceolus"= "#e63946"),
    name = "Espèce"
  ) +

  labs(
    title = "D. Élévation vs Température par espèce",
    x     = "Élévation (m)",
    y     = "Température (°C)"
  ) +

  theme_classic(base_size = 11) +
  theme(
    plot.title      = element_text(face = "bold", size = 10),
    legend.position = "bottom"
  )

print(graphD)


# ============================================================
#  COMBINE THE PLOTS WITH COWPLOT
#
#  ggdraw() creates an empty 1 × 1 canvas using relative units.
#  draw_plot(plot, x, y, width, height) places each plot on this canvas.
#
#  x, y    = bottom-left corner of the plot (0 = left/bottom edge, 1 = right/top edge)
#  width   = plot width as a proportion of the full figure
#  height  = plot height as a proportion of the full figure
#
#  Layout:
#    ┌──────────────────────────────────────────┐  y = 0.60 → 1.00
#    │           graphA  (map)                  │
#    ├────────────────────┬─────────────────────┤  y = 0.25 → 0.60
#    │   graphB  (radar)  │  graphC  (boxplot)  │
#    ├────────────────────┴─────────────────────┤  y = 0.00 → 0.25
#    │           graphD  (scatter)              │
#    └──────────────────────────────────────────┘
# ============================================================
# Only the radar chart is produced with base R -> as_grob() converts it for cowplot.
# graphC and graphD are already ggplot objects: no conversion is needed.
graphB_grob <- as_grob(~ {
  par(mar = c(1, 1, 3, 1))
  radarchart(radar_data,
             axistype = 1,
             pcol = cols_c, pfcol = cols_f, plwd = 2,
             cglcol = "grey85", cglty = 1, vlcex = 0.8)
  title("B. Profil environnemental", cex.main = 0.9, font.main = 2)
  legend("topright", legend = rownames(profils_norm),
         col = cols_c, lwd = 2, bty = "n", cex = 0.68)
})

# ---- Final figure ----
# x, y   = bottom-left corner of the plot (0 = left/bottom edge, 1 = right/top edge)
# width  = plot width as a proportion of the full figure
# height = plot height as a proportion of the full figure

figure_finale <- ggdraw() +

  draw_plot(graphA,      x = 0.00, y = 0.60, width = 1.00, height = 0.40) +
  draw_plot(graphB_grob, x = 0.00, y = 0.25, width = 0.45, height = 0.35) +
  draw_plot(graphC,      x = 0.45, y = 0.25, width = 0.55, height = 0.35) +
  draw_plot(graphD,      x = 0.00, y = 0.00, width = 1.00, height = 0.25) +

  draw_label("A", x = 0.02, y = 0.99, fontface = "bold", size = 14) +
  draw_label("B", x = 0.02, y = 0.59, fontface = "bold", size = 14) +
  draw_label("C", x = 0.46, y = 0.59, fontface = "bold", size = 14) +
  draw_label("D", x = 0.02, y = 0.24, fontface = "bold", size = 14)

windows()
print(figure_finale)


# ============================================================
#  EXPORT
# ============================================================

ggsave("figure_alpine_species_suisse.png", figure_finale,
       width = 13, height = 16, dpi = 300, bg = "white")

ggsave("figure_alpine_species_suisse.pdf", figure_finale,
       width = 13, height = 16, device = cairo_pdf)

message("Export terminé dans : ", getwd())


