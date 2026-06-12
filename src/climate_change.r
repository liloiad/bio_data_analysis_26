# ==============================================================================
# ANALYSIS 5: IMPACT OF FUTURE CLIMATE CHANGE 
# Objective: Analyze the temperature increase predicted for July at the sites of the
# two species and determine whether the change is significant.
# ==============================================================================

# Calculation of the average temperature change for each species
average_change <- aggregate(july_temp_change_c ~ species, data = df, FUN = mean)
print(average_change)

# Paired statistical test (to determine whether the shift from Today to Future is 
# statistically significant)
test_clima <- wilcox.test(df$future_t_july, df$current_t_july, paired = TRUE, exact = FALSE)
print(test_clima)

# INTERPRETATION 
# The test is statistically significant, so the temperature shift from today to
# the future is significant 

# PLOT
df_long_clima <- df[, c("species", "current_t_july", "future_t_july")] %>%
  pivot_longer(cols = c(current_t_july, future_t_july), 
               names_to = "scenarios", 
               values_to = "temperature_july")

df_long_clima$scenarios <- factor(df_long_clima$scenarios, 
                                 levels = c("current_t_july", "future_t_july"))

ordre_species <- df_long_clima %>%
  group_by(species) %>%
  summarise(med = median(temperature_july, na.rm = TRUE), .groups = "drop") %>%
  arrange(med) %>%
  pull(species)

df_long_clima$species_f <- factor(df_long_clima$species, levels = ordre_species)

median <- median(df_long_clima$temperature_july, na.rm = TRUE)

shift_plot <- ggplot(df_long_clima, aes(x = scenarios, y = temperature_july, fill = scenarios)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7, width = 0.5, color = "black") +
  geom_jitter(width = 0.2, size = 1.2, alpha = 0.3, color = "black") +
  geom_hline(yintercept = median, linetype = "dashed", color = "grey50") +
  facet_wrap(~species_f, scales = "fixed", ncol = 1) + 
  scale_fill_manual(
    values = c("current_t_july" = "#2E9FDF", "future_t_july" = "#FC4E07"),
    guide  = "none"
  ) +
  scale_x_discrete(labels = c("current_t_july" = "Present", "future_t_july" = "Future")) +
  coord_flip() +
  labs(
    title = "Climate Warming Expected in July",
    subtitle = "Comparison of the thermal shift with data distribution points",
    x = "Time period", 
    y = "July Temperature (°C)"
  ) +
  theme_classic(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 11, hjust = 0.5),
    plot.subtitle = element_text(size = 9, face = "italic", hjust = 0.5),
    strip.text = element_text(size = 10, face = "bold"),
    strip.background = element_blank()
  )
windows()
print(shift_plot)

# INTERPRETATION
# We can see from the graph that temperatures increase at both the Salamandra atra and 
# Cypripedium calceolus sites. 
# This finding may be evidence that the two species will face problems as climate change intensifies. 